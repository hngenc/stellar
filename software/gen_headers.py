import os
import sys
import math
import random
from random import randint
from collections import deque, defaultdict
from copy import deepcopy

import numpy as np
import scipy

def flatten(l):
    # https://stackoverflow.com/a/952952
    return [item for sublist in l for item in sublist]

def grouped(l, n):
    result = []
    for i,e in enumerate(l):
        if i % n == 0:
            result.append([e])
        else:
            result[-1].append(e)
    return result

def stringify(array, tohex=False):
    if isinstance(array, np.ndarray):
        array = array.tolist()
    if tohex:
        array = [hex(n) for n in array]
    return str(list(array)).replace("[","{").replace("]","}").replace("'", "")

downloads = [
    'https://suitesparse-collection-website.herokuapp.com/MM/Um/2cubes_sphere.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/amazon0312.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/ca-CondMat.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/vanHeukelum/cage12.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/cit-Patents.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/Williams/cop20k_A.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/email-Enron.tar.gz',
    # TODO Add Facebook matrix as well. For some reason, I can't find it online
    'https://suitesparse-collection-website.herokuapp.com/MM/Oberwolfach/filter3D.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/JGD_Homology/m133-b3.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/GHS_indef/mario002.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/Um/offshore.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/p2p-Gnutella31.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/Pajek/patents_main.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/FEMLAB/poisson3Da.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/roadNet-CA.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/Hamm/scircuit.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/Williams/webbase-1M.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/web-Google.tar.gz',
    'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/wiki-Vote.tar.gz',
]

def download_suite_sparse(link):
    import urllib
    from urllib import request

    if not os.path.exists('matrices'):
        os.makedirs('matrices')

    os.chdir('matrices')

    targz_path = os.path.basename(urllib.parse.urlparse(link).path)

    if not os.path.exists(targz_path):
        request.urlretrieve(link, targz_path)

    name = targz_path[:-len(".tar.gz")]
    mm_path = f"{name}/{name}.mtx"

    if not os.path.exists(mm_path):
        os.system(f"tar -xvkf {targz_path}")

    matrix = scipy.io.mmread(mm_path)

    os.chdir('..')

    return matrix

def matmul_tile_sizes(dim_i, dim_j, dim_k, sramA_elems, sramB_elems, sramC_elems, dim_spatial_array):
    best_intensity = 0

    def get_range(mat_dim):
        result = list(range(dim_spatial_array, mat_dim + 1, dim_spatial_array))
        if len(result) == 0 or result[-1] != mat_dim:
            result.append(mat_dim)
        return result

    for tile_i in get_range(dim_i):
        for tile_j in get_range(dim_j):
            for tile_k in get_range(dim_k):
                macs = tile_i * tile_j * tile_k

                data_A = tile_i * tile_k
                data_B = tile_k * tile_j
                data_C = tile_i * tile_j

                if data_A <= sramA_elems and data_B <= sramB_elems and data_C <= sramC_elems:
                    data_moved = data_A + data_B + data_C + tile_j # "tile_j" is just here to represent "dataD"
                    intensity = macs / data_moved

                    if intensity > best_intensity:
                        best_tiles = (tile_i, tile_j, tile_k)
                        best_intensity = intensity

    return best_tiles

def dense(dim_i, dim_j, dim_k, sramA_elems, sramB_elems, sramC_elems, dim_spatial_array, double_buffered_ab, double_buffered_cd, tile_sizes=None):
    random.seed(12345)
    np.random.seed(12345)

    assert dim_j % dim_spatial_array == 0 and dim_k % dim_spatial_array == 0 # TODO support padding in the hardware

    if double_buffered_ab:
        sramA_elems //= 2
        sramB_elems //= 2

    if double_buffered_cd:
        sramC_elems //= 2

    lo = -1 # -200
    hi = 1 # 200
    matA = np.random.randint(low=lo, high=hi, size=(dim_i, dim_k))
    matB = np.random.randint(low=lo, high=hi, size=(dim_k, dim_j))
    bias = np.random.randint(low=lo, high=hi, size=(dim_j))

    # matA = np.zeros_like(matA)
    # matB = np.zeros_like(matB)
    # matA = np.ones_like(matA)
    # matB = np.ones_like(matB)
    # bias = np.zeros_like(bias)
    # bias = np.ones_like(bias)
    # bias = np.arange(bias.size).reshape(bias.shape)

    # matB = np.arange(dim_k * dim_j).reshape(matB.shape)
    # matB[:,1:] = 0
    # matB[2:,:] = 0

    golden = np.matmul(matA, matB) + bias

    if tile_sizes is None:
        tile_i, tile_j, tile_k = matmul_tile_sizes(dim_i, dim_j, dim_k, sramA_elems, sramB_elems, sramC_elems, dim_spatial_array)
    else:
        tile_i, tile_j, tile_k = tile_sizes

    with open("large/matmul_params.h", "w") as f:
        f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'

#ifndef MATMUL_PARAMS_H
#define MATMUL_PARAMS_H

#define DIM {dim_spatial_array}

#define MAT_DIM_I {dim_i}
#define MAT_DIM_J {dim_j}
#define MAT_DIM_K {dim_k}

#define TILE_I {tile_i}
#define TILE_J {tile_j}
#define TILE_K {tile_k}

#define DOUBLE_BUFFERED_AB {"true" if double_buffered_ab else "false"}
#define DOUBLE_BUFFERED_CD {"true" if double_buffered_cd else "false"}

const elem_t left[{dim_i}][{dim_k}] stellar_align = {stringify(matA)};
const elem_t right[{dim_k}][{dim_j}] stellar_align = {stringify(matB)};
const acc_t bias[{dim_j}] stellar_align = {stringify(bias)};

#ifdef CHECK_RESULT
const elem_t golden[{dim_i}][{dim_j}] stellar_align = {stringify(golden)};
#endif

elem_t result[{dim_i}][{dim_j}] stellar_align;

#endif // MATMUL_PARAMS_H
""")

def resnet50_matmuls(batch_size, sramA_elems, sramB_elems, sramC_elems, dim_spatial_array, double_buffered_ab, double_buffered_cd):
    layers = [
            {"layer_id": 2, "matmul_dims": [(12544//4), 64, 64]},
            {"layer_id": 4, "matmul_dims": [(12544//4), 256, 64]},
            {"layer_id": 6, "matmul_dims": [(12544//4), 64, 256]},
            {"layer_id": 12, "matmul_dims": [(12544//4), 128, 256]},
            {"layer_id": 14, "matmul_dims": [(3136//4), 512, 128]},
            {"layer_id": 16, "matmul_dims": [(3136//4), 128, 512]},
            {"layer_id": 25, "matmul_dims": [(3136//4), 256, 512]},
            {"layer_id": 27, "matmul_dims": [(784//4),1024,256]},
            {"layer_id": 29, "matmul_dims": [(784//4),256,1024]},
            {"layer_id": 44, "matmul_dims": [(784//4),512,1024]},
            {"layer_id": 46, "matmul_dims": [(196//4),2048,512]},
            {"layer_id": 48, "matmul_dims": [(196//4),512,2048]},
            {"layer_id": 54, "matmul_dims": [1,1000,2048]},
    ]

    if double_buffered_ab:
        sramA_elems //= 2
        sramB_elems //= 2

    if double_buffered_cd:
        sramC_elems //= 2

    for l in layers:
        # Scale by batch size
        l['matmul_dims'][0] *= batch_size

        # Pad the layers # TODO the padding should be done silently by the hardware
        l['padded_matmul_dims'] = [l['matmul_dims'][0]] + [d if d % dim_spatial_array == 0 else (d//dim_spatial_array + 1) * dim_spatial_array for d in l['matmul_dims'][1:]]

        # Calculate tile sizes
        dim_i,dim_j,dim_k = l['padded_matmul_dims']

        l['tile_sizes'] = matmul_tile_sizes(dim_i, dim_j, dim_k, sramA_elems, sramB_elems, sramC_elems, dim_spatial_array)

        # Calculate ideal cycle counts
        dim_i,dim_j,dim_k = l['matmul_dims']

        l['ideal_cycles'] = int((dim_i * dim_j * dim_k) / (dim_spatial_array ** 2))

    with open("resnet50/matmul_params.h", "w") as f:
        f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'

#ifndef RESNET50_MATMUL_PARAMS_H
#define RESNET50_MATMUL_PARAMS_H

#define DIM {dim_spatial_array}

#define DOUBLE_BUFFERED_AB {"true" if double_buffered_ab else "false"}
#define DOUBLE_BUFFERED_CD {"true" if double_buffered_cd else "false"}

""")

        for i,layer in enumerate(layers):
            if_clause = "#if" if i == 0 else "#elif"

            f.write(f"""{if_clause} LAYER == {layer['layer_id']}

#define MAT_DIM_I {layer['padded_matmul_dims'][0]}
#define MAT_DIM_J {layer['padded_matmul_dims'][1]}
#define MAT_DIM_K {layer['padded_matmul_dims'][2]}

#define TILE_I {layer['tile_sizes'][0]}
#define TILE_J {layer['tile_sizes'][1]}
#define TILE_K {layer['tile_sizes'][2]}

#define IDEAL_CYCLES {layer['ideal_cycles']}

""")

        f.write("""#else
#error LAYER is not defined
#endif

#endif // RESNET50_MATMUL_PARAMS_H""")


def conv_tile_sizes(batches, orows, ocols, ochs, krows, kcols, kchs, stride, sramA_elems, sramB_elems, sramC_elems, dim_spatial_array):
    best_intensity = 0

    def get_range(mat_dim):
        result = list(range(dim_spatial_array, mat_dim + 1, dim_spatial_array))
        if len(result) == 0 or result[-1] != mat_dim:
            result.append(mat_dim)
        return result

    for tile_batches in range(1, batches+1):
        for tile_orows in range(1, orows+1):
            for tile_ocols in range(1, ocols+1):
                for tile_ochs in get_range(ochs):
                    for tile_krows in range(1, krows+1):
                        for tile_kcols in range(1, kcols+1):
                            for tile_kchs in get_range(kchs):
                                macs = tile_batches * tile_orows * tile_ocols * tile_ochs * tile_krows * tile_kcols * tile_kchs

                                tile_irows = tile_orows * stride + tile_krows - stride
                                tile_icols = tile_ocols * stride + tile_kcols - stride

                                data_image = tile_batches * tile_irows * tile_icols * tile_kchs
                                data_weights = tile_krows * tile_kcols * tile_kchs * tile_ochs
                                data_output = tile_batches * tile_orows * tile_ocols * tile_ochs

                                if data_image <= sramA_elems and data_weights <= sramB_elems and data_output <= sramC_elems:
                                    data_moved = data_image + data_weights + data_output + tile_ochs # "tile_ochs" is just here to represent "data_bias"
                                    intensity = macs / data_moved

                                    if intensity > best_intensity:
                                        best_tiles = tile_batches, tile_orows, tile_ocols, tile_ochs, tile_krows, tile_kcols, tile_kchs
                                        best_intensity = intensity

    return best_tiles


def conv(batches, orows, ocols, ochs, krows, kcols, kchs, stride, sramA_elems, sramB_elems, sramC_elems, dim_spatial_array, double_buffered_ab, double_buffered_cd, bigger_image=False, tile_sizes=None):
    random.seed(12345)
    np.random.seed(12345)

    assert kchs % dim_spatial_array == 0 and ochs % dim_spatial_array == 0 # TODO support padding in the hardware

    if double_buffered_ab:
        sramA_elems //= 2
        sramB_elems //= 2

    if double_buffered_cd:
        sramC_elems //= 2

    irows = orows * stride + krows - stride + bigger_image
    icols = ocols * stride + kcols - stride + bigger_image

    lo = -1 # -200
    hi = 1 # 200
    image = np.random.randint(low=lo, high=hi, size=(batches, irows, icols, kchs))
    weights = np.random.randint(low=lo, high=hi, size=(krows, kcols, kchs, ochs))
    bias = np.random.randint(low=lo, high=hi, size=(ochs,))

    # image = np.arange(0, image.size).reshape(image.shape) % 10
    # weights = np.arange(0, weights.size).reshape(weights.shape) # % 10
    # image = np.ones_like(image)
    # weights = np.ones_like(weights)
    # image = np.ones_like(image)
    # weights = np.zeros_like(weights)
    # bias = np.zeros_like(bias)

    # image[:,:,:,2:] = 0
    # weights[:,:,2:,:] = 0
    # weights[:,:,:,2:] = 0
    # image[:,1::2,:,:] = -1
    # image[:,:,1::2,:] = -1

    # import pdb; pdb.set_trace()

    output = np.empty((batches, orows, ocols, ochs), dtype=int)
    for b in range(batches):
        for orow in range(orows):
            for ocol in range(ocols):
                for och in range(ochs):
                    out = bias[och]

                    for krow in range(krows):
                        irow = orow * stride + krow

                        for kcol in range(kcols):
                            icol = ocol * stride + kcol

                            for kch in range(kchs):
                                im = image[b,irow,icol,kch]
                                w = weights[krow,kcol,kch,och]

                                out += im * w

                    output[b,orow,ocol,och] = out

    if tile_sizes is None:
        tile_batches, tile_orows, tile_ocols, tile_ochs, tile_krows, tile_kcols, tile_kchs = conv_tile_sizes(batches, orows, ocols, ochs, krows, kcols, kchs, stride, sramA_elems, sramB_elems, sramC_elems, dim_spatial_array)
    else:
        tile_batches, tile_orows, tile_ocols, tile_ochs, tile_krows, tile_kcols, tile_kchs = tile_sizes

    with open("large/conv_params.h", "w") as f:
        f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'

#ifndef CONV_PARAMS_H
#define CONV_PARAMS_H

#define DIM {dim_spatial_array}

#define BATCHES {batches}
#define OROWS {orows}
#define OCOLS {ocols}
#define OCHS {ochs}
#define KROWS {krows}
#define KCOLS {kcols}
#define KCHS {kchs}

#define STRIDE {stride}

#define TILE_BATCHES {tile_batches}
#define TILE_OROWS {tile_orows}
#define TILE_OCOLS {tile_ocols}
#define TILE_OCHS {tile_ochs}
#define TILE_KROWS {tile_krows}
#define TILE_KCOLS {tile_kcols}
#define TILE_KCHS {tile_kchs}

#define DOUBLE_BUFFERED_AB {"true" if double_buffered_ab else "false"}
#define DOUBLE_BUFFERED_CD {"true" if double_buffered_cd else "false"}

#define BIGGER_IMAGE {"true" if bigger_image else "false"}

const elem_t image[{batches}][{irows}][{icols}][{kchs}] stellar_align = {stringify(image)};
const elem_t weights[{krows}][{kcols}][{kchs}][{ochs}] stellar_align = {stringify(weights)};
const acc_t bias[{ochs}] stellar_align = {stringify(bias)};

#ifdef CHECK_RESULT
const elem_t golden[{batches}][{orows}][{ocols}][{ochs}] stellar_align = {stringify(output)};
#endif

elem_t result[{batches}][{orows}][{ocols}][{ochs}] stellar_align;

#endif // CONV_PARAMS_H
""")


def sparse_dense_mm(dim_i, dim_j, dim_k, max_coord, download=None):
    random.seed(12345)

    matA = [[randint(-200,200) for _ in range(dim_k)] for _ in range(dim_i)]

    if download is None:
        matB_row_ids = [0]
        for _ in range(dim_k):
            rowlen = randint(0, dim_j)
            matB_row_ids.append(matB_row_ids[-1] + rowlen)

        matB_data = []
        matB_coords = []
        for k in range(dim_k):
            coord = randint(0, 20)
            for j in range(matB_row_ids[k+1] - matB_row_ids[k]):
                matB_data.append(randint(-200, 200))

                matB_coords.append(coord)
                coord += randint(1, 20)

        n_expanded_cols = max(matB_coords, default=-1) + 1

        matB_expanded = [[0 for _ in range(n_expanded_cols)] for _ in range(dim_k)]
        for k in range(dim_k):
            data = matB_data[matB_row_ids[k]:matB_row_ids[k+1]]
            coords = matB_coords[matB_row_ids[k]:matB_row_ids[k+1]]

            for d,j in zip(data, coords):
                matB_expanded[k][j] = d

    else:
        matrix = download_suite_sparse(download)

        matB_expanded = matrix.todense()[:dim_k,:dim_j]
        _, matB_coords = matB_expanded.nonzero()

        n_expanded_cols = matB_expanded.shape[1]

        matB_row_ids = [0]
        for row in matB_expanded:
            rowlen = np.count_nonzero(row)
            matB_row_ids.append(matB_row_ids[-1] + rowlen)

        matB_expanded = matB_expanded.tolist()

        matB_data = np.ones_like(matB_coords)

    golden_expanded = [[0 for _ in range(n_expanded_cols)] for _ in range(dim_i)]
    for i in range(dim_i):
        for j in range(n_expanded_cols):
            for k in range(dim_k):
                golden_expanded[i][j] += matA[i][k] * matB_expanded[k][j]

    golden_coords = set(matB_coords)

    golden_data = [[d for c,d in enumerate(r) if c in golden_coords] for r in golden_expanded]
    golden_data = flatten(golden_data)

    n_golden_cols = len(golden_coords)
    golden_row_ids = [x * n_golden_cols for x in range(0,dim_i+1)]

    golden_coords = sorted(list(golden_coords))
    golden_coords = golden_coords * dim_i

    if max_coord is None:
        max_coord = max(matB_coords)
    assert max_coord <= max(matB_coords)

    with open("large/sparse_dense_matmul_params.h", "w") as f:
        f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'

#ifndef SPARSE_DENSE_MATMUL_PARAMS_H
#define SPARSE_DENSE_MATMUL_PARAMS_H

#define MAT_DIM_I {dim_i}
#define MAT_DIM_J {dim_j}
#define MAT_DIM_K {dim_k}

#define RIGHT_SIZE {len(matB_data)}
#define RESULT_SIZE {len(golden_data)}

#define MAX_COORD {max_coord}

const elem_t left[{dim_i}][{dim_k}] stellar_align = {stringify(matA)};

const elem_t right[RIGHT_SIZE] stellar_align = {stringify(matB_data)};
const elem_t right_col_ids[RIGHT_SIZE] stellar_align = {stringify(matB_coords)};
const elem_t right_row_ids[{dim_k+1}] stellar_align = {stringify(matB_row_ids)};

const elem_t bias_row_ids[{dim_i+1}] stellar_align; // all-zero array

const elem_t golden[RESULT_SIZE] stellar_align = {stringify(golden_data)};
const elem_t golden_col_ids[RESULT_SIZE] stellar_align = {stringify(golden_coords)};
const elem_t golden_row_ids[{dim_i+1}] stellar_align = {stringify(golden_row_ids)};

elem_t result[RESULT_SIZE] stellar_align;
elem_t result_col_ids[RESULT_SIZE] stellar_align;
elem_t result_row_ids[{dim_i+1}] stellar_align;

#endif // SPARSE_DENSE_MATMUL_PARAMS_H
""")


def outerspace(dim, I = None, J = None, K = None, density = 0.1, download = None, write_golden = True, fname = "large/outerspace_params.h", output_c_file = False, align_data_and_coords = False, sramA_maxOuter = 2 * 16, sramA_maxElems = 2 * 16 * 64, sramB_maxOuter = 2 * 16, sramB_maxElems = 2 * 16 * 128, sramScattered_maxOutermost = 2 * 16, sramScattered_maxMiddle = 2 * 16 * 32, sramScattered_maxElems = 2 * 16 * 32 * 32, always_add_nodes = True, interleaved_data_and_coords = True, nsplits = None, is_merge = False):
    random.seed(12345)
    np.random.seed(12345)

    if download is not None:
        A = download_suite_sparse(download).tocsc()

        B = download_suite_sparse(download).tocsr()

        # A = A[:,:256] # TODO don't slice
        # B = B[:256,:] # TODO don't slice

        # A = A[:,:2048] # TODO don't slice
        # B = B[:2048,:] # TODO don't slice
        # A = A[:2048,:] # TODO don't slice

        I = A.shape[0]
        K = A.shape[1]
        J = B.shape[1]

        # TODO when we're downloading a specific SparseSuite matrix, we ideally shouldn't replace its values
        A = A.astype(int)
        A.data = np.random.randn(*A.data.shape).astype(int)

        B = B.astype(int)
        B.data = np.random.randn(*B.data.shape).astype(int)

        max_val = 1 # Just to reduce the size of the header values, we only store nonzeros as ones

    else:
        A = scipy.sparse.random(I, K, density, format='csc', dtype=int)
        B = scipy.sparse.random(K, J, density, format='csr', dtype=int)

        max_val = 50

    # Breaking apart rows
    def break_rows(matA, matB, maxJ):
        ks = []
        b_new_indptrs = [0]
        for k,rowlen in enumerate(np.diff(matB.indptr)):
            rlen = rowlen
            while rlen > maxJ:
                b_new_indptrs.append(b_new_indptrs[-1] + maxJ)
                ks.append(k)
                rlen -= maxJ
            b_new_indptrs.append(b_new_indptrs[-1] + rlen)
            ks.append(k)

        new_B = scipy.sparse.csr_array((B.data, B.indices, b_new_indptrs), shape=(len(ks), matB.shape[1]))
        new_A = matA[:, ks]

        return (new_A, new_B)

    A,B = break_rows(A,B,sramB_maxElems)

    K = A.shape[1]
    assert B.shape[0] == K

    # Sampling
    def sample(mat, isCsc):
        lens = np.diff(mat.indptr)
        step = max(K//(1024*16),1)
        targets = np.argsort(lens)[::step]
        filtered = [index for index in range(K) if index in targets]

        new_data = []
        new_indices = []
        new_indptrs = [0]
        for index in filtered:
            if isCsc:
                sliver = mat[:,index:index+1]
            else:
                sliver = mat[index:index+1,:]

            new_data += sliver.data.tolist()
            new_indices += sliver.indices.tolist()
            new_indptrs.append(new_indptrs[-1] + sliver.indices.size)

        if step == 1:
            assert all(new_indptrs == mat.indptr) and all(new_indices == mat.indices)

        if isCsc:
            return scipy.sparse.csc_array((new_data, new_indices, new_indptrs), shape=(mat.shape[0], len(new_indptrs)-1))
        else:
            return scipy.sparse.csr_array((new_data, new_indices, new_indptrs), shape=(len(new_indptrs)-1, mat.shape[1]))

    K = A.shape[1]
    assert B.shape[0] == K

    # TODO when we're downloading a specific SparseSuite matrix, we ideally shouldn't replace its values
    A.data %= max_val
    B.data %= max_val
    A.data += 1
    B.data += 1

    if K % dim != 0:
        new_K = ((K // dim) + 1) * dim

        new_A_indptr = np.append(A.indptr, np.repeat(A.indptr[-1], dim - (K % dim)))
        new_B_indptr = np.append(B.indptr, np.repeat(B.indptr[-1], dim - (K % dim)))

        A = scipy.sparse.csc_array((A.data, A.indices, new_A_indptr), shape=(I, new_K))
        B = scipy.sparse.csr_array((B.data, B.indices, new_B_indptr), shape=(new_K, J))

        K = new_K

    if I % dim != 0:
        new_I = ((I // dim) + 1) * dim
        A = scipy.sparse.csc_array((A.data, A.indices, A.indptr), shape=(new_I, K))
        I = new_I

    # Calculate tile sizes
    #   Full loop:
    #       for k_dram in 0 until K by size:
    #           mvinB()
    #           for i_dram in 0 until I by tile_i:
    #               mvinA()
    #               matmul()
    #               mvoutC()
    tile_k = dim
    tile_is = []
    small_ks = []
    tiles_fit = []

    A_indptr_diff = np.diff(A.indptr)
    B_indptr_diff = np.diff(B.indptr)

    max_is = [A_indptr_diff[k:k+tile_k].max() for k in range(0, K, tile_k)]

    def _fits_in_sramAB(tile_i, k_to_check, small_k, maxOuter, maxElems, diff):
        if not small_k and tile_k > maxOuter:
            return False

        ks = range(0, K, tile_k) if k_to_check is None else range(k_to_check, k_to_check + 1)
        for k in ks:
            diff_ = diff[k:k+tile_k]
            diff__ = diff_.max() if small_k else diff_.sum()
            if diff__ > maxElems:
                return False

        return True

    def fits_in_sramA(tile_i, k_to_check, small_k):
        return _fits_in_sramAB(tile_i, k_to_check, small_k, sramA_maxOuter, sramA_maxElems, A_indptr_diff.clip(None, tile_i))

    def fits_in_sramB(tile_i, k_to_check, small_k):
        return _fits_in_sramAB(tile_i, k_to_check, small_k, sramB_maxOuter, sramB_maxElems, B_indptr_diff)

    def fits_in_sramScattered(tile_i, k_to_check, small_k):
        if not small_k and tile_k > sramScattered_maxOutermost:
            return False

        ks = range(0, K, tile_k) if k_to_check is None else range(k_to_check, k_to_check + 1)
        for k in ks:
            a_n_elems_ = A_indptr_diff[k:k+tile_k].clip(None, tile_i)
            a_n_elems = a_n_elems_.max() if small_k else a_n_elems_.sum()
            if a_n_elems > sramScattered_maxMiddle:
                return False

            n_elems = 0
            for k_ in range(k, min(k+tile_k, K)):
                n_elems_ = min(tile_i, A_indptr_diff[k_]) * B_indptr_diff[k_]
                if small_k:
                    n_elems = max(n_elems, n_elems_)
                else:
                    n_elems += n_elems_

            if n_elems > sramScattered_maxElems:
                return False

        return True

    for k in range(0, K, tile_k):
        tile_i = I
        small_k = False

        fitsA = False
        fitsB = False
        fitsScattered = False
        while True:
            if not fitsA:
                fitsA = fits_in_sramA(tile_i, k, small_k)

            if not fitsB:
                fitsB = fits_in_sramB(tile_i, k, small_k)
            if not fitsB:
                if small_k:
                    break
                small_k = True
                tile_i = I
                continue

            if not fitsScattered:
                fitsScattered = fits_in_sramScattered(tile_i, k, small_k)

            if fitsA and fitsB and fitsScattered:
                break
            elif tile_i > A_indptr_diff.max() + dim:
                tile_i = A_indptr_diff.max()
                if tile_i % dim != 0:
                    tile_i = ((tile_i // dim) + 1) * dim
            elif tile_i > dim:
                tile_i -= dim
            else:
                tile_i -= 1

            if tile_i == 0 and not small_k:
                small_k = True
                tile_i = I
                continue
            if tile_i <= 0:
                break

        if tile_i <= 0 or not (fits_in_sramA(tile_i, k, small_k) and fits_in_sramB(tile_i, k, small_k) and fits_in_sramScattered(tile_i, k, small_k)):
            print(f"Does not fit: {download=}")
            tiles_fit.append(False)
        else:
            tiles_fit.append(True)

        tile_is.append(tile_i)
        small_ks.append(small_k)

    assert all(tiles_fit)

    # scattered_C_by_k = [(A[:,k:k+1] @ B[k:k+1,:]).tocoo() for k in range(K)] # This line is pretty readable, but it ended up being too slow. The loop below does the same thing, but much faster
    scattered_C_by_k = []
    for k in range(K):
        a = A[:,k:k+1].tocoo()
        b = B[k:k+1,:].tocoo()

        c_data = np.outer(a.data, b.data).flatten()
        c_row = a.row.repeat(b.data.size)
        c_col = np.tile(b.col, a.data.size)

        scattered_C_by_k.append(scipy.sparse.coo_array((c_data, (c_row, c_col)), shape=(I,J)))

    merged_C_row = np.concatenate([x.tocoo().row for x in scattered_C_by_k])
    merged_C_col = np.concatenate([x.tocoo().col for x in scattered_C_by_k])
    merged_C_data = np.concatenate([x.tocoo().data for x in scattered_C_by_k])
    merged_C = scipy.sparse.coo_array((merged_C_data, (merged_C_row, merged_C_col)), shape=(I, J)).tocsr()

    merged_C_indptr_padding = (64 // 4) - 1 # This improves alignment which improves DMA performance

    # Create the ik-linked-list pointer packets
    scatteredC_by_ik_data = [[] for _ in range(I)]
    scatteredC_by_ik_coords = [[] for _ in range(I)]
    for k in range(K):
        scattered_data = dict()
        scattered_coords = dict()

        for i_expanded in A[:, k:k+1].indices:
            scattered_data[i_expanded] = []
            scattered_coords[i_expanded] = []

        mat = scattered_C_by_k[k].tocoo()
        for data,row,col in zip(mat.data, mat.row, mat.col):
            scattered_data[row].append(data)
            scattered_coords[row].append(col)

        for i in scattered_data:
            scatteredC_by_ik_data[i].append(scattered_data[i])
            scatteredC_by_ik_coords[i].append(scattered_coords[i])

    class PtrPacket:
        def __init__(self):
            self.rowlen = -1
            self.ll_len = 0
            self.data_and_coord_offset = 0
            self.next_ptr_rel_offset = -1
            self.last_ptr_rel_offset = -1

            self.print_ll_len = False

        def __str__(self):
            ll_len = self.ll_len if self.print_ll_len else 0
            return f"/* start */ {self.rowlen | (ll_len << 16)}, {self.data_and_coord_offset}, {self.next_ptr_rel_offset}, {self.last_ptr_rel_offset}, "

    # Create function which prints to files
    def print_to_file():
        head_ptrs_with_ll_len = deepcopy(head_ptrs)
        for head in head_ptrs_with_ll_len:
            assert not head.print_ll_len
            head.print_ll_len = True

        # Create the unitialized ptr packets
        uninitialized_ptr_packets = []

        for packet in head_ptrs + non_head_ptrs:
            unitialized_packet = PtrPacket()
            unitialized_packet.data_and_coord_offset = packet.data_and_coord_offset
            uninitialized_ptr_packets.append(unitialized_packet)

        for uninitialized_packet, packet in zip(uninitialized_ptr_packets, last_head_ptrs + last_non_head_ptrs):
            uninitialized_packet.rowlen = packet.rowlen
            uninitialized_packet.next_ptr_rel_offset = packet.next_ptr_rel_offset
            uninitialized_packet.last_ptr_rel_offset = packet.last_ptr_rel_offset

        # When using incoherent memory accesses, we can get errors if the
        #   ptr-packets aren't padded to make sure that the arrays of ptr-packets
        #   fully occupy whichever cache-line they are resident in.
        packets_per_cache_line = 4
        padding_ptr_packets = [PtrPacket()] * (packets_per_cache_line - len(uninitialized_ptr_packets) % packets_per_cache_line if len(uninitialized_ptr_packets) % packets_per_cache_line != 0 else 0)

        # Calculate the ideal number of ops
        # mult_ops = 0
        # for scattered in scattered_C_by_k:
        #     mult_ops += scattered.count_nonzero()
        mult_ops = data_offset - last_data_offset

        add_ops = mult_ops - merged_C.indptr[-1]

        fname = basefname
        if should_split:
            fname = basefname[:-len("_params.h")] + f"_split{split}_params.h"

        K_start = split_start * tile_k
        K_end = split_end * tile_k

        A_split = A[:,K_start:K_end]
        B_split = B[K_start:K_end,:]

        with open(fname, "w") as f:
            f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'
// Matrix name: {download}

#ifndef OUTERSPACE_PARAMS_H
#define OUTERSPACE_PARAMS_H

#include <stdint.h>
#include <stdbool.h>
#include "include/stellar_params.h"

#define DIM {dim}
#define LOG_DIM {int(math.log2(dim))}

#define I {I}
#define J {J}
#define K {K_end - K_start}

#define INTERLEAVED_DATA_AND_COORDS {"true" if interleaved_data_and_coords else "false"}

{"// " if not any(small_ks[split_start:split_end]) else ""}#define SMALL_K_EXISTS

#define MAX_I {np.diff(A.indptr[split_start:split_end]).max()}
{"// " if (np.array(tile_is[split_start:split_end]) < I).any() else ""}#define UNTILED_I
{"// " if not all(tiles_fit[split_start:split_end]) else ""}#define ALL_TILES_FIT

#define PTR_PACKET_SIZE 4
#define PTR_PACKET_ROWLEN 0
#define PTR_PACKET_DATA_OFF 1
#define PTR_PACKET_NEXT_REL_OFF 2
#define PTR_PACKET_LAST_REL_OFF 3

#define FREE_PTR_OFFSET {len(last_non_head_ptrs)}

{"// " if not write_golden else ""}#define WRITE_GOLDEN
""")

        if output_c_file:
            with open(fname, "a") as f:
                f.write(f"""
#ifdef SMALL_K_EXISTS
extern const bool small_ks[{len(small_ks[split_start:split_end])}];
#endif

extern const int max_is[{len(max_is[split_start:split_end])}];

#ifndef UNTILED_I
extern const int tile_is[{len(tile_is[split_start:split_end])}];
#endif

#ifndef ALL_TILES_FIT
extern const bool tiles_fit[{len(tiles_fit[split_start:split_end])}];
#endif

extern const elem_t A_data[{A_split.data.size}];
extern const elem_t A_indices[{A_split.indices.size}];
extern const elem_t A_indptrs[{A_split.indptr.size}];

extern const elem_t B_data[{B_split.data.size}];
extern const elem_t B_indices[{B_split.indices.size}];
extern const elem_t B_indptrs[{B_split.indptr.size}];

{"// " if interleaved_data_and_coords else ""}extern elem_t scattered_data_heap[{data_offset - last_data_offset}];
{"// " if interleaved_data_and_coords else ""}extern elem_t scattered_coord_heap[{data_offset - last_data_offset}];
{"// " if not interleaved_data_and_coords else ""}extern elem_t scattered_data_and_coord_heap[{(data_offset - last_data_offset)*2}];

#ifdef WRITE_GOLDEN
{"// " if interleaved_data_and_coords else ""}extern elem_t golden_scattered_data_heap[{data_offset - last_data_offset}];
{"// " if interleaved_data_and_coords else ""}extern elem_t golden_scattered_coord_heap[{data_offset - last_data_offset}];
{"// " if not interleaved_data_and_coords else ""}extern elem_t golden_scattered_data_and_coord_heap[{(data_offset - last_data_offset)*2}];
#endif

extern elem_t ptr_packets[{len(uninitialized_ptr_packets + padding_ptr_packets)} * PTR_PACKET_SIZE];

#ifdef WRITE_GOLDEN
extern const elem_t golden_ptr_packets[{len(uninitialized_ptr_packets + padding_ptr_packets)} * PTR_PACKET_SIZE];
#endif

extern elem_t merged_C_data[{merged_C.data.size}];
extern elem_t merged_C_indices[{merged_C.indices.size}];
extern elem_t merged_C_indptrs_buffer[{merged_C_indptr_padding + merged_C.indptr.size}];
static elem_t * const merged_C_indptrs = &merged_C_indptrs_buffer[{merged_C_indptr_padding}];

#ifdef WRITE_GOLDEN
extern const elem_t golden_merged_C_data[{merged_C.data.size}];
extern const elem_t golden_merged_C_indices[{merged_C.indices.size}];
extern const elem_t golden_merged_C_indptrs[{merged_C.indptr.size}];
#endif

#endif // OUTERSPACE_PARAMS_H
""")

            cfname = fname[:-2] + ".c"

            with open(cfname, "w") as f:
                f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'
// Matrix name: {download}

#include "{os.path.basename(fname)}"

#ifdef SMALL_K_EXISTS
const bool small_ks[] = {stringify(small_ks[split_start:split_end]).lower()};
#endif

const int max_is[] = {stringify(max_is[split_start:split_end])};

#ifndef UNTILED_I
const int tile_is[] = {stringify(tile_is[split_start:split_end])};
#endif

#ifndef ALL_TILES_FIT
const bool tiles_fit[] = {stringify(tiles_fit[split_start:split_end]).lower()};
#endif

const elem_t A_data[] stellar_align = {stringify(A_split.data)};
const elem_t A_indices[] stellar_align = {stringify(A_split.indices)};
const elem_t A_indptrs[] stellar_align = {stringify(A_split.indptr)};

const elem_t B_data[] stellar_align = {stringify(B_split.data)};
const elem_t B_indices[] stellar_align = {stringify(B_split.indices)};
const elem_t B_indptrs[] stellar_align = {stringify(B_split.indptr)};

{"// " if interleaved_data_and_coords else ""}elem_t scattered_data_heap[{data_offset - last_data_offset}] stellar_align;
{"// " if interleaved_data_and_coords else ""}elem_t scattered_coord_heap[{data_offset - last_data_offset}] stellar_align;
{"// " if not interleaved_data_and_coords else ""}elem_t scattered_data_and_coord_heap[{(data_offset - last_data_offset)*2}] stellar_align;

#ifdef WRITE_GOLDEN""")
                if interleaved_data_and_coords:
                    f.write(f"""
elem_t golden_scattered_data_and_coord_heap[] stellar_align = {stringify([val for pair in zip(golden_scattered_data[last_data_offset:], golden_scattered_coords[last_data_offset:]) for val in pair])};""")
                else:
                    f.write(f"""
elem_t golden_scattered_data_heap[] stellar_align = {stringify(golden_scattered_data[last_data_offset:])};
elem_t golden_scattered_coord_heap[] stellar_align = {stringify(golden_scattered_coords[last_data_offset:])};""")
                f.write(f"""
#endif

elem_t ptr_packets[] stellar_align = {{{"".join([str(x) for x in uninitialized_ptr_packets + padding_ptr_packets])[:-2]}}};

#ifdef WRITE_GOLDEN
const elem_t golden_ptr_packets[] stellar_align = {{{"".join([str(x) for x in head_ptrs_with_ll_len + non_head_ptrs + padding_ptr_packets])[:-2]}}};
#endif

elem_t merged_C_data[{merged_C.data.size}] stellar_align;
elem_t merged_C_indices[{merged_C.indices.size}] stellar_align;
elem_t merged_C_indptrs_buffer[{merged_C_indptr_padding + merged_C.indptr.size}] stellar_align;

#ifdef WRITE_GOLDEN
const elem_t golden_merged_C_data[{merged_C.data.size}] stellar_align = {stringify(merged_C.data)};
const elem_t golden_merged_C_indices[{merged_C.indices.size}] stellar_align = {stringify(merged_C.indices)};
const elem_t golden_merged_C_indptrs[{merged_C.indptr.size}] stellar_align = {stringify(merged_C.indptr)};
#endif
""")

        else:
            with open(fname, "a") as f:
                f.write(f"""

#ifdef SMALL_K_EXISTS
static const bool small_ks[] = {stringify(small_ks[split_start:split_end]).lower()};
#endif

static const int max_is[] = {stringify(max_is[split_start:split_end])};

#ifndef UNTILED_I
static const int tile_is[] = {stringify(tile_is[split_start:split_end])};
#endif

#ifndef ALL_TILES_FIT
static const bool tiles_fit[] = {stringify(tiles_fit[split_start:split_end]).lower()};
#endif

static const elem_t A_data[] stellar_align = {stringify(A_split.data)};
static const elem_t A_indices[] stellar_align = {stringify(A_split.indices)};
static const elem_t A_indptrs[] stellar_align = {stringify(A_split.indptr)};

static const elem_t B_data[] stellar_align = {stringify(B_split.data)};
static const elem_t B_indices[] stellar_align = {stringify(B_split.indices)};
static const elem_t B_indptrs[] stellar_align = {stringify(B_split.indptr)};

{"// " if interleaved_data_and_coords else ""}static elem_t scattered_data_heap[{data_offset - last_data_offset}] stellar_align;
{"// " if interleaved_data_and_coords else ""}static elem_t scattered_coord_heap[{data_offset - last_data_offset}] stellar_align;
{"// " if not interleaved_data_and_coords else ""}static elem_t scattered_data_and_coord_heap[{(data_offset - last_data_offset)*2}] stellar_align;

#ifdef WRITE_GOLDEN
{"// " if interleaved_data_and_coords else ""}static elem_t golden_scattered_data_heap[] stellar_align = {stringify(golden_scattered_data[last_data_offset:])};
{"// " if interleaved_data_and_coords else ""}static elem_t golden_scattered_coord_heap[] stellar_align = {stringify(golden_scattered_coords[last_data_offset:])};
{"// " if not interleaved_data_and_coords else ""}static elem_t golden_scattered_data_and_coord_heap[] stellar_align = {stringify([val for pair in zip(golden_scattered_data[last_data_offset:], golden_scattered_coords[last_data_offset:]) for val in pair])};
#endif

static elem_t ptr_packets[] stellar_align = {{{"".join([str(x) for x in uninitialized_ptr_packets + padding_ptr_packets])[:-2]}}};

#ifdef WRITE_GOLDEN
static const elem_t golden_ptr_packets[] stellar_align = {{{"".join([str(x) for x in head_ptrs_with_ll_len + non_head_ptrs + padding_ptr_packets])[:-2]}}};
#endif

static elem_t merged_C_data[{merged_C.data.size}] stellar_align;
static elem_t merged_C_indices[{merged_C.indices.size}] stellar_align;
static elem_t merged_C_indptrs_buffer[{merged_C_indptr_padding + merged_C.indptr.size}] stellar_align;
static elem_t * const merged_C_indptrs = &merged_C_indptrs_buffer[{merged_C_indptr_padding}];

#ifdef WRITE_GOLDEN
static const elem_t golden_merged_C_data[] stellar_align = {stringify(merged_C.data)};
static const elem_t golden_merged_C_indices[] stellar_align = {stringify(merged_C.indices)};
static const elem_t golden_merged_C_indptrs[] stellar_align = {stringify(merged_C.indptr)};
#endif

#endif // OUTERSPACE_PARAMS_H
""")

    # Create head pointers
    head_ptrs = []
    non_head_ptrs = []

    for i in range(I):
        packet = PtrPacket()

        # valid = len(scatteredC_by_ik_data[i]) > 0
        # if valid:
        #     packet.rowlen = len(scatteredC_by_ik_data[i][0])
        #     packet.last_ptr_rel_offset = 0

        head_ptrs.append(packet)

    # Create non-head pointers
    golden_scattered_data = []
    golden_scattered_coords = []

    data_offset = 0
    tl_req_size = 16

    should_split = nsplits is not None
    if nsplits is None:
        nsplits = 1
    basefname = fname

    n_tiles = len(tile_is)
    assert len(tile_is) == n_tiles and len(small_ks) == n_tiles and len(small_ks) == n_tiles and len(max_is) == n_tiles and len(tiles_fit) == n_tiles

    split = 0
    split_start = 0
    split_end = int((split+1) * (n_tiles / nsplits))
    assert split_start < split_end

    last_data_offset = data_offset
    last_head_ptrs = deepcopy(head_ptrs)
    last_non_head_ptrs = deepcopy(non_head_ptrs)

    A_csr = A.tocsr()
    reached_head = [False] * I
    last_ptr = [None] * I
    for tile_id,(k_outer,tile_i,tile_fits) in enumerate(zip(range(0, K, tile_k), tile_is, tiles_fit, strict=True)):
        if not tile_fits:
            tile_i = I

        for i_outer in range(0, A_indptr_diff[k_outer:k_outer+tile_k].max(), tile_i):
            for k_expanded in range(k_outer, min(k_outer+tile_k, K)):
                for i_compressed,i_expanded in enumerate(A[:, k_expanded:k_expanded+1].indices[i_outer:i_outer+tile_i]):
                    ks = A_csr[i_expanded:i_expanded+1, :].indices.tolist()
                    k_compressed = ks.index(k_expanded)

                    head = head_ptrs[i_expanded]
                    is_head = not reached_head[i_expanded]
                    reached_head[i_expanded] = True

                    if is_head:
                        head.data_and_coord_offset = data_offset
                        head.rowlen = len(scatteredC_by_ik_data[i_expanded][0])
                        head.ll_len = 1
                        head.last_ptr_rel_offset = 0

                        data_offset += head.rowlen

                        golden_scattered_data += scatteredC_by_ik_data[i_expanded][0]
                        golden_scattered_coords += scatteredC_by_ik_coords[i_expanded][0]

                        if always_add_nodes:
                            non_head_ptrs.append(PtrPacket())

                    else:
                        packet = PtrPacket()
                        packet_id = I + len(non_head_ptrs)

                        head.last_ptr_rel_offset = packet_id - i_expanded
                        head.ll_len += 1

                        prev_is_head = last_ptr[i_expanded] is None
                        prev_id = None if prev_is_head else last_ptr[i_expanded]
                        prev = head if prev_is_head else non_head_ptrs[prev_id]
                        prev.next_ptr_rel_offset = head.last_ptr_rel_offset if prev_is_head else packet_id - (prev_id + I)

                        packet.rowlen = len(scatteredC_by_ik_data[i_expanded][k_compressed])
                        packet.data_and_coord_offset = data_offset

                        data_offset += packet.rowlen

                        golden_scattered_data += scatteredC_by_ik_data[i_expanded][k_compressed]
                        golden_scattered_coords += scatteredC_by_ik_coords[i_expanded][k_compressed]

                        non_head_ptrs.append(packet)
                        last_ptr[i_expanded] = packet_id - I

                    if align_data_and_coords and data_offset % tl_req_size != 0:
                        padding = tl_req_size - (data_offset % tl_req_size)
                        golden_scattered_data += [0] * padding
                        golden_scattered_coords += [0] * padding
                        data_offset += padding

        if tile_id == split_end-1:
            print_to_file()

            split += 1
            split_start = split_end
            split_end = int((split+1) * (n_tiles / nsplits))
            assert split_start < split_end
            if split == n_tiles:
                assert split_end == n_tiles

            last_data_offset = data_offset
            last_head_ptrs = deepcopy(head_ptrs)
            last_non_head_ptrs = deepcopy(non_head_ptrs)

    if not align_data_and_coords:
        assert len(golden_scattered_data) == data_offset
        assert len(golden_scattered_coords) == data_offset

    if should_split:
        with open(basefname, "w"):
            pass

def scnn(max_i, max_f, max_wt, max_ht, max_kc, input_channels, output_channels, input_w, input_h, filter_w, filter_h, input_sparsity, weight_sparsity, fname, foperation="w"):
    random.seed(12345)
    np.random.seed(12345)

    # Pad dimensions
    orig_input_w = input_w
    orig_input_h = input_h

    orig_output_channels = output_channels

    if input_w % max_wt != 0:
        input_w += max_wt - (input_w % max_wt)
    if input_h % max_ht != 0:
        input_h += max_ht - (input_h % max_ht)

    if output_channels % max_kc:
        output_channels += max_kc - (output_channels % max_kc)

    # Generate dense tensors
    dense_image = np.ones((input_channels, input_w, input_h))
    dense_weights = np.ones((input_channels, output_channels, filter_w, filter_h))

    # Sparsify image and weights
    def sparsify(tensor, orig_shape, sparsity):
        indices = np.random.choice(np.prod(orig_shape), replace=False, size=int(np.prod(orig_shape) * sparsity))
        unravelled = np.unravel_index(indices, orig_shape)
        tensor[unravelled] = 0

    sparsify(dense_image, (input_channels, orig_input_w, orig_input_h), input_sparsity)
    sparsify(dense_weights, (input_channels, orig_output_channels, filter_w, filter_h), weight_sparsity)

    dense_image[:, orig_input_w:, :] = 0
    dense_image[:, :, orig_input_h:] = 0
    dense_weights[:, orig_output_channels:, :, :] = 0

    dense_image = dense_image.astype(int)
    dense_weights = dense_weights.astype(int)

    # Calculate ideal cycle times
    ideal_macs = 0
    # for c in range(input_channels):
    #     for w in range(input_w):
    #         for h in range(input_h):
    #             for r in range(filter_w):
    #                 for s in range(filter_h):
    #                     if 0 <= w+r-1 < input_w and 0 <= h+s-1 < input_h and dense_image[c,w+r-1,h+s-1] > 0:
    #                         for k in range(output_channels):
    #                             ideal_macs += dense_weights[c,k,r,s] > 0

    for k in range(output_channels):
        for c in range(input_channels):
            ideal_macs += np.count_nonzero(dense_image[c,:,:]) * np.count_nonzero(dense_weights[c,k,:,:])

    ideal_macs_per_cycle = max_i * max_f * max_wt * max_ht
    ideal_cycles = max(ideal_macs // ideal_macs_per_cycle, 1)

    # Partition image
    #   Format: [C, Wt, Wh, I = flatten(TileW * TileH)]
    #   Our format: [C, I/maxI, Wt, Wh, I]
    #               [C, I/maxI, flatten(Wt, Wh), I]
    #               [D, D,      D,               C]

    tile_w = input_w // max_wt; assert input_w % max_wt == 0
    tile_h = input_h // max_ht; assert input_h % max_ht == 0

    transposed_image = dense_image.reshape((input_channels, max_wt, tile_w, max_ht, tile_h)).transpose((0,1,3,2,4))
    i_in_each_tile = np.count_nonzero(transposed_image.reshape((input_channels, max_wt, max_ht, -1)), axis=-1)
    max_actual_i = i_in_each_tile.max()
    assert max_actual_i <= tile_w * tile_h

    i_div_max_i = int(math.ceil(max_actual_i / max_i))

    part_image_v = []
    part_image_h = []
    part_image_w = []

    part_image_row_ids = []
    row_id = 0

    for c in range(input_channels):
        # part_image_v.append([])
        # part_image_h.append([])
        # part_image_w.append([])

        for i_outer in range(i_div_max_i):
            # part_image_v[-1].append([])
            # part_image_h[-1].append([])
            # part_image_w[-1].append([])

            for wt in range(max_wt):
                for ht in range(max_ht):
                    # part_image_v[-1][-1].append([])
                    # part_image_h[-1][-1].append([])
                    # part_image_w[-1][-1].append([])

                    part_image_row_ids.append(row_id)

                    i_inner = 0
                    i_inner_start = i_outer * max_i
                    i_inner_end = (i_outer + 1) * max_i
                    for w in range(tile_w):
                        for h in range(tile_h):
                            value = transposed_image[c, wt, ht, w, h]
                            if value != 0:
                                if i_inner >= i_inner_start:
                                    # part_image_v[-1][-1][-1].append(value)
                                    # part_image_w[-1][-1][-1].append(w)
                                    # part_image_h[-1][-1][-1].append(h)

                                    part_image_v.append(value)
                                    part_image_w.append(w)
                                    part_image_h.append(h)

                                    row_id += 1

                                i_inner += 1

                                if i_inner >= i_inner_end:
                                    break
                        else:
                            continue
                        break

    part_image_row_ids.append(row_id)

    assert len(part_image_v) == np.count_nonzero(dense_image)

    # Partition weights
    #   Format: [C, K/Kc, F = flatten(Kc, R, S)]
    #   Our format: [C, K/Kc, F = flatten(Kc, R, S)]
    #               [D, D,    C]

    k_div_kc = output_channels // max_kc; assert output_channels % max_kc == 0

    part_weight_v = []
    part_weight_w = []
    part_weight_h = []
    part_weight_k = []

    part_weight_row_ids = []
    row_id = 0

    for c in range(input_channels):
        for k_outer in range(k_div_kc):
            part_weight_row_ids.append(row_id)

            for k_inner in range(max_kc):
                k = k_outer * max_kc + k_inner

                for w in range(filter_w):
                    for h in range(filter_h):
                        value = dense_weights[c,k,w,h]
                        if value != 0:
                            part_weight_v.append(value)
                            part_weight_w.append(w)
                            part_weight_h.append(h)
                            part_weight_k.append(k_inner)

                            row_id += 1

    part_weight_row_ids.append(row_id)

    assert len(part_weight_v) == np.count_nonzero(dense_weights)

    # Calculate final stats
    image_sparsity = 1. - len(part_image_v) / (input_channels * orig_input_w * orig_input_h)
    weight_sparsity = 1. - len(part_weight_v) / (input_channels * orig_output_channels * filter_w * filter_h)

    # Final checks
    maxImageRowIds = 1024*16 * max_wt * max_ht
    maxImageNElems = 1024*1024
    assert len(part_image_row_ids) <= maxImageRowIds
    assert len(part_image_v) <= maxImageNElems

    maxFilterRowIds = (1024*16)**2
    maxFilterNElems = 1024*1024
    assert len(part_weight_row_ids) <= maxFilterRowIds
    assert len(part_weight_v) <= maxFilterNElems

    # Write output file
    with open(fname, foperation) as f:
        if foperation == 'w':
            pre = """// Auto-generated, and may be overwritten by 'build.sh'

#ifndef SCNN_CONV_PARAMS_H
#define SCNN_CONV_PARAMS_H

"""

            post = """#endif // SCNN_CONV_PARAMS_H\n"""

        else:
            pre = ""
            post = ""

        f.write(pre + f"""#define MAX_I {max_i}
#define MAX_F {max_f}

#define MAX_WT {max_wt}
#define MAX_HT {max_ht}

#define MAX_KC {max_kc}

#define I_DIV_MAX_I {i_div_max_i}
#define K_DIV_KC {k_div_kc}

#define INPUT_CHANNELS {input_channels}
#define OUTPUT_CHANNELS {output_channels}
#define INPUT_W {input_w}
#define INPUT_H {input_h}
#define FILTER_W {filter_w}
#define FILTER_H {filter_h}

#define ORIG_OUTPUT_CHANNELS {orig_output_channels}
#define ORIG_INPUT_W {orig_input_w}
#define ORIG_INPUT_H {orig_input_h}

#define SRAM_IMAGE_VALUE_AND_H 0
#define SRAM_IMAGE_W 1

#define SRAM_FILTER_VALUE_AND_H 2
#define SRAM_FILTER_W_AND_OUT_CHANNEL 3

#define SRAM_PARAMS 4

#define SRAM_RESULT 5

#define IDEAL_MACS {ideal_macs}
#define IDEAL_CYCLES {ideal_cycles}

#define IMAGE_SPARSITY {image_sparsity}
#define WEIGHT_SPARSITY {weight_sparsity}

#define TILE_K {max_kc}
#define TILE_W {tile_w}
#define TILE_H {tile_h}

#define N_BANKS 32

const elem_t image_row_ids[{len(part_image_row_ids)}] stellar_align = {stringify(part_image_row_ids).replace(" ","")};
const elem_t image_data[{len(part_image_v)}] stellar_align = {stringify(part_image_v).replace(" ","")};
const elem_t image_h[{len(part_image_h)}] stellar_align = {stringify(part_image_h).replace(" ","")};
const elem_t image_w[{len(part_image_w)}] stellar_align = {stringify(part_image_w).replace(" ","")};

const elem_t weight_row_ids[{len(part_weight_row_ids)}] stellar_align = {stringify(part_weight_row_ids).replace(" ","")};
const elem_t weight_data[{len(part_weight_v)}] stellar_align = {stringify(part_weight_v).replace(" ","")};
const elem_t weight_h[{len(part_weight_h)}] stellar_align = {stringify(part_weight_h).replace(" ","")};
const elem_t weight_w[{len(part_weight_w)}] stellar_align = {stringify(part_weight_w).replace(" ","")};
const elem_t weight_out_channel[{len(part_weight_k)}] stellar_align = {stringify(part_weight_k).replace(" ","")};

elem_t result[{input_w * input_h * output_channels}] stellar_align;
""" + post)

def alexnet(max_i, max_f, max_wt, max_ht, max_kc):
    fname = "alexnet/alexnet_params.h"
    with open(fname, "w") as f:
        f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'

#ifndef ALEXNET_PARAMS_H
#define ALEXNET_PARAMS_H

""")

    layers = [
            # {"layer_id": 1, "input_channels": 3, "output_channels": 96, "input_w": 224//4, "input_h": 224//4, "filter_w": 11, "filter_h": 11, "input_sparsity": 0., "weight_sparsity": 1.-0.84},
            # {"layer_id": 2, "input_channels": 96, "output_channels": 256, "input_w": 26, "input_h": 26, "filter_w": 5, "filter_h": 5, "input_sparsity": 1-0.865, "weight_sparsity": 1-0.225},
            # {"layer_id": 3, "input_channels": 256, "output_channels": 384, "input_w": 12, "input_h": 12, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.53, "weight_sparsity": 1-0.34},
            # {"layer_id": 4, "input_channels": 384, "output_channels": 384, "input_w": 12, "input_h": 12, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.39, "weight_sparsity": 1-0.37},
            # {"layer_id": 5, "input_channels": 384, "output_channels": 256, "input_w": 12, "input_h": 12, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.43, "weight_sparsity": 1-0.37},

            {"layer_id": 1, "input_channels": 3, "output_channels": 96, "input_w": 14, "input_h": 14, "filter_w": 3, "filter_h": 3, "input_sparsity": 0., "weight_sparsity": 1.-0.84},
            {"layer_id": 10, "input_channels": 3, "output_channels": 96, "input_w": 14, "input_h": 14, "filter_w": 2, "filter_h": 2, "input_sparsity": 0., "weight_sparsity": 1.-0.84},
            {"layer_id": 2, "input_channels": 96, "output_channels": 256, "input_w": 7, "input_h": 7, "filter_w": 5, "filter_h": 5, "input_sparsity": 1-0.865, "weight_sparsity": 1-0.225},
            {"layer_id": 20, "input_channels": 96, "output_channels": 256, "input_w": 6, "input_h": 6, "filter_w": 5, "filter_h": 5, "input_sparsity": 1-0.865, "weight_sparsity": 1-0.225},
            {"layer_id": 3, "input_channels": 256, "output_channels": 384, "input_w": 4, "input_h": 4, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.53, "weight_sparsity": 1-0.34},
            {"layer_id": 30, "input_channels": 256, "output_channels": 384, "input_w": 1, "input_h": 1, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.53, "weight_sparsity": 1-0.34},
            {"layer_id": 4, "input_channels": 384, "output_channels": 384, "input_w": 4, "input_h": 4, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.39, "weight_sparsity": 1-0.37},
            {"layer_id": 40, "input_channels": 384, "output_channels": 384, "input_w": 1, "input_h": 1, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.39, "weight_sparsity": 1-0.37},
            {"layer_id": 5, "input_channels": 384, "output_channels": 256, "input_w": 4, "input_h": 4, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.43, "weight_sparsity": 1-0.37},
            {"layer_id": 50, "input_channels": 384, "output_channels": 256, "input_w": 1, "input_h": 1, "filter_w": 3, "filter_h": 3, "input_sparsity": 1-0.43, "weight_sparsity": 1-0.37},
    ]

    # Weak scaling
    # for layer in layers:
    #     # TODO To speed up VCS simulations, we're reducing the number of channels. (This won't increase our utilization, and may even slightly reduce it). But when testing on Firesim, we should just run on the full layer
    #     layer['input_channels'] = min(layer['input_channels'], 16)
    #     layer['output_channels'] = min(layer['output_channels'], 64)
    #     layer['input_w'] //= (8 // max_wt)
    #     layer['input_h'] //= (8 // max_ht)

    # Generate layers
    for layer in layers:
        with open(fname, "a") as f:
            f.write(f"""#if LAYER == {layer['layer_id']}\n""")

        scnn(max_i=max_i, max_f=max_f, max_wt=max_wt, max_ht=max_ht, max_kc=max_kc,
                input_channels=layer['input_channels'],
                output_channels=layer['output_channels'],
                input_w=layer['input_w'], input_h=layer['input_h'],
                filter_w=layer['filter_w'], filter_h=layer['filter_h'],
                input_sparsity=layer['input_sparsity'],
                weight_sparsity=layer['weight_sparsity'],
                fname=fname, foperation='a')

        with open(fname, "a") as f:
            f.write(f"""#endif\n\n""")

    with open(fname, "a") as f:
        f.write(f"""#endif // ALEXNET_PARAMS_H""")


def sparch_merge(radix, flat_sorts_per_cycle, grouped_sorters, grouped_sorts_per_cycle, balance, compress, max_sram_elems, max_sram_rows, sram_node_size, download, nsplits, fname):
    max_sram_nodes = max_sram_elems / sram_node_size

    # A = np.ones((5,5))
    # A[:,2:] = 0
    # A = scipy.sparse.csr_matrix(A)

    A = download_suite_sparse(download)
    # A = A.tolil()
    # A[:,40:] = 0
    A = A.tocsr()
    B = A.copy()
    # A = A[:8]
    A = A[:128]

    C = (A @ B).tocsr()

    I = A.shape[0]

    max_k_compressed = np.diff(A.indptr).max()
    max_k_compressed_c = np.diff(C.indptr).max()

    partial_mats = [] # [{"a_weight": a_weight, "js": {i: [j]}}]

    flat_cycles = []
    grouped_cycles = []

    unmerged_mats = []
    merged_mats = []

    elems_fit = True
    rows_fit = True
    no_tiling = True
    max_sram_elems_required = 0
    max_sram_rows_required = 0

    def constructCompressionMap(data):
        result = dict()
        for i,d in enumerate(sorted(list(data))):
            result[d] = i
        return result

    # Initialize partial mats
    for k_compressed in range(max_k_compressed):
        partial_mats.append({"a_weight": 0, "js": dict()})
    for i in range(I):
        for k_compressed,k_expanded in enumerate(A.indices[A.indptr[i]:A.indptr[i+1]]):
            partial_mats[k_compressed]["a_weight"] += 1
            # partial_mats[k_compressed]["js"][i] = set(B[k_expanded].indices)
            partial_mats[k_compressed]["js"][i] = set(B.indices[B.indptr[k_expanded]:B.indptr[k_expanded+1]])

    # Merge all matrices
    def n_elems_in_pmat(pmat):
        result = 0
        for js in pmat['js'].values():
            result += len(js)
        return result

    def n_elems_in_pmat_by_i(pmat):
        result = defaultdict(lambda: 0)
        for i,js in pmat["js"].items():
            result[i] += len(js)
        return result

    def merged_matrix(pmats, is_a_tile):
        result = {
            "a_weight": None if is_a_tile else sum([p["a_weight"] for p in pmats]),
            "js": defaultdict(lambda: set())
        }
        for pmat in pmats:
            for i,js in pmat["js"].items():
                result["js"][i].update(js)
        return result

    tiled_merges = [] # This is a stack/LIFO that is just used when a merge is too large and needs to be split into multiple tiles so it can fit in the scratchpads

    merge_id = 0
    last_merge_id = None
    last_merge_radix = radix

    while len(partial_mats) > 1:
        is_a_tile = len(tiled_merges) > 0

        # Find partial mats to combine
        if not is_a_tile:
            partial_mats.sort(key=lambda x: x['a_weight'])

        if is_a_tile:
            _radix = tiled_merges[0]
            tiled_merges = tiled_merges[1:]
        else:
            _radix = radix

        partial_mats_to_sort = partial_mats[:_radix]
        partial_mats = partial_mats[_radix:]

        # Merge the partial mats
        new_partial_mat = merged_matrix(partial_mats_to_sort, is_a_tile)

        sorted_is = sorted(list(new_partial_mat['js'].keys()))

        total_pmat_elems = sum([n_elems_in_pmat(pmat) for pmat in partial_mats_to_sort])
        total_pmat_elems_by_i = defaultdict(lambda: 0)
        total_pmat_nodes = 0 # We only bother calculating the nodes for grouped mergers, because flat merges will always utilize fewer (or equal) nodes anyways
        for pmat in partial_mats_to_sort:
            pmat_elems_by_i = n_elems_in_pmat_by_i(pmat)
            for i,js in pmat_elems_by_i.items():
                total_pmat_elems_by_i[i] += js
                total_pmat_nodes += (js + sram_node_size - 1) // sram_node_size

        if not is_a_tile:
            partial_mats.append(new_partial_mat)

        # Tile partial mats if necessary
        if total_pmat_elems >= max_sram_elems or total_pmat_nodes >= max_sram_nodes:
            # If the pmats are too big to fit, then we take just half the rows and try again
            middle_row = sorted_is[len(sorted_is)//2]; assert len(sorted_is) > 1

            earlier_half_pmats = [{'a_weight': None, 'js': {row: pmat['js'][row] for row in pmat['js'] if row < middle_row}} for pmat in partial_mats_to_sort]
            later_half_mats = [{'a_weight': None, 'js': {row: pmat['js'][row] for row in pmat['js'] if row >= middle_row}} for pmat in partial_mats_to_sort]

            tiled_merges = [len(earlier_half_pmats), len(later_half_mats)] + tiled_merges
            partial_mats = earlier_half_pmats + later_half_mats + partial_mats

            no_tiling = False
            continue # Go back to the beginning of the while-loop

        # Compress matrices
        if compress:
            i_compression_map = constructCompressionMap(sorted_is)
            j_compression_map = dict()
            for i in sorted_is:
                j_compression_map[i] = constructCompressionMap(new_partial_mat['js'][i])

            compressed_new_partial_mat = {
                "a_weight": new_partial_mat['a_weight'],
                "js": dict()
            }
            for i in sorted_is:
                compressed_js = {j_compression_map[i][j] for j in new_partial_mat['js'][i]}
                compressed_new_partial_mat['js'][i_compression_map[i]] = compressed_js
            new_partial_mat = compressed_new_partial_mat

            for partial_mat_id in range(len(partial_mats_to_sort)):
                pmat = partial_mats_to_sort[partial_mat_id]
                compressed_pmat = {
                    "a_weight": pmat['a_weight'],
                    "js": dict()
                }
                for i in pmat['js'].keys():
                    compressed_js = {j_compression_map[i][j] for j in pmat['js'][i]}
                    compressed_pmat['js'][i_compression_map[i]] = compressed_js
                partial_mats_to_sort[partial_mat_id] = compressed_pmat

        # Append matrices to lists of matrices which are written into the header files
        merged_mats.append(new_partial_mat['js'])
        unmerged_mats += [pmat['js'] for pmat in partial_mats_to_sort]

        # Collect stats on partial-mat-merging
        n_grouped_cycles = 0
        assert balance in [0,1]
        if balance == 0:
            for group in grouped(sorted_is, grouped_sorters):
                most_elems = max([total_pmat_elems_by_i[i] for i in group])
                n_grouped_cycles += most_elems / grouped_sorts_per_cycle
        elif balance == 1:
            total_pmat_elems_by_i_balanced = defaultdict(lambda: 0)
            for i_compressed,i_expanded in enumerate(sorted_is):
                total_pmat_elems_by_i_balanced[i_compressed % grouped_sorters] += total_pmat_elems_by_i[i_expanded]
            n_grouped_cycles += max(total_pmat_elems_by_i_balanced.values()) / grouped_sorts_per_cycle

        grouped_cycles.append(n_grouped_cycles)
        flat_cycles.append(total_pmat_elems / flat_sorts_per_cycle)

        max_sram_elems_required = max(max_sram_elems_required, total_pmat_elems)
        max_sram_rows_required = max(max_sram_rows_required, len(total_pmat_elems_by_i))
        assert total_pmat_elems <= max_sram_elems
        assert len(total_pmat_elems_by_i) <= max_sram_rows

        if last_merge_id is None:
            if len(partial_mats_to_sort) < radix:
                last_merge_id = merge_id
                last_merge_radix = len(partial_mats_to_sort)
        else:
            assert last_merge_radix == len(partial_mats_to_sort)

        merge_id += 1

    if last_merge_id is None:
        last_merge_id = merge_id

    for split in range(nsplits):
        # Split the merged/unmerged matrices
        split_merged_mats = merged_mats[split::nsplits]
        split_unmerged_mats = []

        n_grouped_cycles = 0
        n_flat_cycles = 0

        for merged_mat_id in range(split,len(merged_mats),nsplits):
            partial_mat_id = merged_mat_id*radix if merged_mat_id <= last_merge_id else last_merge_id*radix + (merged_mat_id-last_merge_id)*last_merge_radix
            _radix = radix if merged_mat_id < last_merge_id else last_merge_radix
            split_unmerged_mats += unmerged_mats[partial_mat_id:partial_mat_id+_radix]

            n_flat_cycles += flat_cycles[merged_mat_id]
            n_grouped_cycles += grouped_cycles[merged_mat_id]

        split_last_merge_id = last_merge_id // nsplits + (last_merge_id % nsplits > split)

        # Convert to C
        flat_unmerged_axis_0_coords = []
        flat_unmerged_axis_0_row_ids = [0]

        flat_merged_axis_0_coords = []
        flat_merged_axis_0_row_ids = [0]

        grouped_unmerged_axis_0_coords = []
        grouped_unmerged_axis_0_row_ids = [0]

        grouped_unmerged_axis_1_coords = []
        grouped_unmerged_axis_1_row_ids = [0]

        grouped_merged_axis_0_coords = []
        grouped_merged_axis_0_row_ids = [0]

        grouped_merged_axis_1_coords = []
        grouped_merged_axis_1_row_ids = [0]

        for flat_axis_0_coords, flat_axis_0_row_ids, grouped_axis_0_coords, grouped_axis_0_row_ids, grouped_axis_1_coords, grouped_axis_1_row_ids, mats in [
                (flat_unmerged_axis_0_coords, flat_unmerged_axis_0_row_ids, grouped_unmerged_axis_0_coords, grouped_unmerged_axis_0_row_ids, grouped_unmerged_axis_1_coords, grouped_unmerged_axis_1_row_ids, split_unmerged_mats),
                (flat_merged_axis_0_coords, flat_merged_axis_0_row_ids, grouped_merged_axis_0_coords, grouped_merged_axis_0_row_ids, grouped_merged_axis_1_coords, grouped_merged_axis_1_row_ids, split_merged_mats)]:

            for debug_i,mat in enumerate(mats):
                new_flat_mat = []

                mat_items = [y for y in sorted(mat.items(), key=lambda x: x[0]) if len(y[1]) > 0]

                for i,js in mat_items:
                    js = sorted(list(js)) # "js" can be an unsorted set
                    new_flat_mat += [(i << 32) | j for j in js]
                    grouped_axis_0_coords += js

                    grouped_axis_0_row_ids.append(grouped_axis_0_row_ids[-1] + len(js))
                    grouped_axis_1_coords.append(i)
                grouped_axis_1_row_ids.append(grouped_axis_1_row_ids[-1] + len(mat_items))

                flat_axis_0_coords += new_flat_mat
                flat_axis_0_row_ids.append(flat_axis_0_row_ids[-1] + len(new_flat_mat))

        # Compress the merged matrices
        if compress:
            partial_mat_id = 0
            for merged_mat_id in range(len(flat_merged_axis_0_row_ids)-1):
                merged_mat_start = flat_merged_axis_0_row_ids[merged_mat_id]
                merged_mat_end = flat_merged_axis_0_row_ids[merged_mat_id+1]

                merged_mat = flat_merged_axis_0_coords[merged_mat_start:merged_mat_end]
                compression_map = constructCompressionMap(merged_mat)

                rad = radix if merged_mat_id < split_last_merge_id else last_merge_radix
                partial_mats_start = flat_unmerged_axis_0_row_ids[partial_mat_id]
                partial_mats_end = flat_unmerged_axis_0_row_ids[min(partial_mat_id + rad, len(flat_unmerged_axis_0_row_ids)-1)]

                for index in range(merged_mat_start, merged_mat_end):
                    flat_merged_axis_0_coords[index] = compression_map[flat_merged_axis_0_coords[index]]

                for index in range(partial_mats_start, partial_mats_end):
                    flat_unmerged_axis_0_coords[index] = compression_map[flat_unmerged_axis_0_coords[index]]

                partial_mat_id += radix if merged_mat_id < split_last_merge_id else last_merge_radix

        flat_elem_t = 'int32_t' if compress else 'int64_t'

        # Check that all compressed values are in the right range
        if compress:
            for array in flat_unmerged_axis_0_coords, flat_merged_axis_0_coords, grouped_unmerged_axis_0_coords, grouped_unmerged_axis_1_coords, grouped_merged_axis_0_coords, grouped_merged_axis_1_coords:
                assert len(array) == 0 or max(array) < max(max_sram_elems, max_sram_rows)

        # Write header files
        hfname = fname[:-len("_params.h")] + f"_split{split}_params.h"
        with open(hfname, "w") as f:
            f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'
// Matrix name: {download}

#ifndef MERGE_PARAMS_H
#define MERGE_PARAMS_H

#define RADIX {radix}
#define LAST_MERGE_ID {split_last_merge_id}
#define LAST_MERGE_RADIX {last_merge_radix}

#define N_MERGED_MATS {len(flat_merged_axis_0_row_ids)-1}
#define N_PARTIAL_MATS {len(flat_unmerged_axis_0_row_ids)-1}

#define N_FLAT_CYCLES {int(n_flat_cycles)}
#define N_GROUPED_CYCLES {int(n_grouped_cycles)}

#ifdef FLAT

extern const {flat_elem_t} flat_unmerged_axis_0_coords[{len(flat_unmerged_axis_0_coords)}];
extern const {flat_elem_t} flat_unmerged_axis_0_row_ids[{len(flat_unmerged_axis_0_row_ids)}];

extern {flat_elem_t} golden_flat_merged_axis_0_coords[{len(flat_merged_axis_0_coords)}];
extern {flat_elem_t} golden_flat_merged_axis_0_row_ids[{len(flat_merged_axis_0_row_ids)}];

#ifdef CHECK_RESULT
{flat_elem_t} flat_merged_axis_0_coords[{len(flat_merged_axis_0_coords)}];
{flat_elem_t} flat_merged_axis_0_row_ids[{len(flat_merged_axis_0_row_ids)}];
#endif // CHECK_RESULT

#else // FLAT

extern const int32_t grouped_unmerged_axis_0_coords[{len(grouped_unmerged_axis_0_coords)}];
extern const int32_t grouped_unmerged_axis_0_row_ids[{len(grouped_unmerged_axis_0_row_ids)}];

extern const int32_t grouped_unmerged_axis_1_coords[{len(grouped_unmerged_axis_1_coords)}];
extern const int32_t grouped_unmerged_axis_1_row_ids[{len(grouped_unmerged_axis_1_row_ids)}];

extern int32_t golden_grouped_merged_axis_0_coords[{len(grouped_merged_axis_0_coords)}];
extern int32_t golden_grouped_merged_axis_0_row_ids[{len(grouped_merged_axis_0_row_ids)}];

extern int32_t golden_grouped_merged_axis_1_coords[{len(grouped_merged_axis_1_coords)}];
extern int32_t golden_grouped_merged_axis_1_row_ids[{len(grouped_merged_axis_1_row_ids)}];

#ifdef CHECK_RESULT
int32_t grouped_merged_axis_0_coords[{len(grouped_merged_axis_0_coords)}];
int32_t grouped_merged_axis_0_row_ids[{len(grouped_merged_axis_0_row_ids)}];

int32_t grouped_merged_axis_1_coords[{len(grouped_merged_axis_1_coords)}];
int32_t grouped_merged_axis_1_row_ids[{len(grouped_merged_axis_1_row_ids)}];
#endif // CHECK_RESULT

#endif // FLAT

#endif // MERGE_PARAMS_H
""")

        cfname = fname[:-len("_params.h")] + f"_split{split}_params_flat.c"
        with open(cfname, "w") as f:
            f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'
// Matrix name: {download}

#include <stdint.h>

const {flat_elem_t} flat_unmerged_axis_0_coords[] __attribute__((aligned(64))) = {stringify(flat_unmerged_axis_0_coords, tohex=True)};
const {flat_elem_t} flat_unmerged_axis_0_row_ids[] __attribute__((aligned(64))) = {stringify(flat_unmerged_axis_0_row_ids)};

{flat_elem_t} golden_flat_merged_axis_0_coords[] __attribute__((aligned(64))) = {stringify(flat_merged_axis_0_coords, tohex=True)};
{flat_elem_t} golden_flat_merged_axis_0_row_ids[] __attribute__((aligned(64))) = {stringify(flat_merged_axis_0_row_ids)};
""")

        cfname = fname[:-len("_params.h")] + f"_split{split}_params_grouped.c"
        with open(cfname, "w") as f:
            f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'
// Matrix name: {download}

#include <stdint.h>

const int32_t grouped_unmerged_axis_0_coords[] __attribute__((aligned(64))) = {stringify(grouped_unmerged_axis_0_coords)};
const int32_t grouped_unmerged_axis_0_row_ids[] __attribute__((aligned(64))) = {stringify(grouped_unmerged_axis_0_row_ids)};

const int32_t grouped_unmerged_axis_1_coords[] __attribute__((aligned(64))) = {stringify(grouped_unmerged_axis_1_coords)};
const int32_t grouped_unmerged_axis_1_row_ids[] __attribute__((aligned(64))) = {stringify(grouped_unmerged_axis_1_row_ids)};

int32_t golden_grouped_merged_axis_0_coords[] __attribute__((aligned(64))) = {stringify(grouped_merged_axis_0_coords)};
int32_t golden_grouped_merged_axis_0_row_ids[] __attribute__((aligned(64))) = {stringify(grouped_merged_axis_0_row_ids)};

int32_t golden_grouped_merged_axis_1_coords[] __attribute__((aligned(64))) = {stringify(grouped_merged_axis_1_coords)};
int32_t golden_grouped_merged_axis_1_row_ids[] __attribute__((aligned(64))) = {stringify(grouped_merged_axis_1_row_ids)};
""")

    n_flat_cycles = sum(flat_cycles)
    n_grouped_cycles = sum(grouped_cycles)
    hfname = fname
    with open(hfname, "w") as f:
            f.write(f"""// Auto-generated, and may be overwritten by 'build.sh'
// Matrix name: {download}
// This file is used in a hacky way to order Makefile targets

// {radix=} {I=}
// {len(A.indices)=} {len(set(A.indices))=}
// {len(C.indices)=}

// {len(merged_mats)=}
// {len(unmerged_mats)=}

// {radix=} {flat_sorts_per_cycle=} {grouped_sorters=} {grouped_sorts_per_cycle=} {balance=}

// {max_k_compressed=}
// {max_k_compressed_c=}

// {n_flat_cycles=}
// {n_grouped_cycles=}

// n_flat_cycles_pretty={n_flat_cycles:,}
// n_flat_cycles_ideal_pretty={len(C.indices)/flat_sorts_per_cycle:,}
// n_flat_cycles_scale={len(C.indices)/flat_sorts_per_cycle / n_flat_cycles if n_flat_cycles > 0 else 0:,}

// n_grouped_cycles_pretty={n_grouped_cycles:,}
// n_grouped_cycles_ideal_pretty={len(C.indices)/(grouped_sorters*grouped_sorts_per_cycle):,}
// n_grouped_cycles_scale={len(C.indices)/(grouped_sorters*grouped_sorts_per_cycle) / n_grouped_cycles if n_grouped_cycles > 0 else 0:,}
// n_grouped_cycles_scale_to_flat={n_flat_cycles/n_grouped_cycles if n_grouped_cycles > 0 else 0:,}

// {no_tiling=}
// {elems_fit=} {rows_fit=}
// {max_sram_elems=} {max_sram_rows=}
// max_sram_elems_required={max_sram_elems_required:,} max_sram_rows_required={max_sram_rows_required:,}
""")


if __name__ == "__main__":
    if len(sys.argv) == 1:
        target = 'all'
    elif len(sys.argv) > 1:
        target = sys.argv[1]

    if target == 'all' or target.endswith('large/matmul_params.h'):
        dense(32, 48, 64, 128*1024, 128*1024, 16*1024, 16, True, True)
        # dense(32, 48, 64, 128*1024, 128*1024, 16*1024, 16, True, True)
        # dense(19, 48, 64, 128*1024, 128*1024, 16*1024, 16, True, True)
        # dense(16, 16, 16, 128*1024, 128*1024, 16*1024, 16, True, True)
        # dense(32, 32, 32, 128*1024, 128*1024, 16*1024, 16, True, True)

        # dense(4, 1008, 2048, 128*1024, 128*1024, 16*1024, 16, True, True) # , tile_sizes=(16,8*16,56*16))
        # dense(32, 32, 32, 128*1024, 128*1024, 16*1024, 16, True, True, tile_sizes=(16,16,16))

    if target == 'all' or target.endswith('large/conv_params.h'):
        conv(batches=2, orows=7, ocols=7, ochs=32, krows=3, kcols=3, kchs=48, stride=2, sramA_elems=128*1024, sramB_elems=128*1024, sramC_elems=16*1024, dim_spatial_array=16, double_buffered_ab=True, double_buffered_cd=True) # , tile_sizes=(1, 7, 7, 32, 3, 3, 32))
        # conv(batches=2, orows=7, ocols=7, ochs=32, krows=1, kcols=1, kchs=64, stride=2, sramA_elems=128*1024, sramB_elems=128*1024, sramC_elems=16*1024, dim_spatial_array=16, double_buffered_ab=True, double_buffered_cd=True, bigger_image=True) # , tile_sizes=(1, 7, 7, 32, 3, 3, 32))

        # conv(batches=1, orows=12, ocols=12, ochs=32, krows=3, kcols=3, kchs=256, stride=1, sramA_elems=128*1024, sramB_elems=128*1024, sramC_elems=16*1024, dim_spatial_array=16, double_buffered_ab=True, double_buffered_cd=True) # , tile_sizes=(1, 7, 7, 32, 3, 3, 32))
        # conv(batches=1, orows=12, ocols=12, ochs=384, krows=3, kcols=3, kchs=256, stride=1, sramA_elems=128*1024, sramB_elems=128*1024, sramC_elems=16*1024, dim_spatial_array=16, double_buffered_ab=True, double_buffered_cd=True) # , tile_sizes=(1, 12, 12, 16, 3, 3, 64))

    if target == 'all' or target.endswith('large/sparse_dense_matmul_params.h'):
        sparse_dense_mm(32, 8, 16, None)
        # sparse_dense_mm(16, 8, 8, download='https://suitesparse-collection-website.herokuapp.com/MM/SNAP/wiki-Vote.tar.gz')

    if target == 'all' or target.endswith('resnet50/matmul_params.h'):
        resnet50_matmuls(4, 128*1024, 128*1024, 16*1024, 16, True, True)

    if target == 'all' or target.endswith('large/outerspace_params.h'):
        outerspace(8, 48, 48, 48)

    if target == 'all' or target.endswith('large/scnn_params.h'):
        scnn(max_i=4, max_f=4, max_wt=2, max_ht=2, max_kc=8,
                input_channels=16, output_channels=384,
                input_w=4, input_h=4,
                filter_w=3, filter_h=3,
                input_sparsity=1-0.53, weight_sparsity=1-0.34,
                fname="large/scnn_params.h")

    if target == 'all' or target.endswith('alexnet/alexnet_params.h'):
        alexnet(max_i=4, max_f=4, max_wt=2, max_ht=2, max_kc=8)

    if 'sparsesuite' in target:
        # Example target:
        #   /home/eecs/hngenc/scratch/stellar/chipyard/generators/stellar/software/build/../sparsesuite/2cubes_sphere_params.h'
        target_matrix = os.path.basename(target)[:-len("_params.h")]
        download_url = None
        for download in downloads:
            if target_matrix in os.path.basename(download).replace("-", "_"):
                download_url = download
                break
        assert download_url is not None, f"{target}"

        if len(sys.argv) > 2:
            nsplits = int(sys.argv[2])
        else:
            nsplits = 1

        outerspace(8, download = download_url, fname = target, output_c_file = True, align_data_and_coords = False, write_golden = True # "Patents" not in target
                   , nsplits = nsplits, is_merge = True)

    if 'sparch' in target:
        # Example target:
        #   /home/eecs/hngenc/scratch/stellar/chipyard/generators/stellar/software/build/../sparch/2cubes_sphere_params.h'
        target_matrix = os.path.basename(target)[:-len("_params.h")]
        download_url = None
        for download in downloads:
            if target_matrix in os.path.basename(download).replace("-", "_"):
                download_url = download
                break

        if download_url is None:
            download_url = 'https://suitesparse-collection-website.herokuapp.com/MM/SNAP/ca-CondMat.tar.gz'

        if len(sys.argv) > 2:
            nsplits = int(sys.argv[2])
        else:
            nsplits = 1

        sparch_merge(
                radix=4, flat_sorts_per_cycle=4, grouped_sorters=4,
                # radix=16, flat_sorts_per_cycle=16, grouped_sorters=16,
                # radix=64, flat_sorts_per_cycle=16, grouped_sorters=16,
                grouped_sorts_per_cycle=1, balance=1, compress=True,
                # max_sram_elems=int((512*1024)*0.8), max_sram_rows=int((256*1024)*0.8), sram_node_size=32,
                max_sram_elems=int(256*1024), max_sram_rows=int(256*1024), sram_node_size=32,
                download=download_url, nsplits=nsplits, fname=target)

