#include <erl_nif.h>

/* 传递指针改变值 */
static ERL_NIF_TERM past_ptr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i;
    if (!enif_get_uint(env, argv[0], &i))
        return enif_make_badarg(env);
    if(!past_ptr2(&i))
        return enif_make_atom(env, "false");
    return enif_make_uint(env, i);

}

static int past_ptr2(int* i)
{
    int tmp = *i;
    if(tmp > 10) return 0;
    tmp++;
    *i = tmp;
    return 1;
}

//static ERL_NIF_TERM new_hash_tuple_2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
//{
//    unsigned int incr;
//    unsigned int size;
//
//    if (!enif_get_uint(env, argv[0], &incr))
//        return enif_make_badarg(env);
//    if (!enif_get_uint(env, argv[1], &size))
//        return enif_make_badarg(env);
//
//    ERL_NIF_TERM* arr = enif_alloc(size * sizeof(ERL_NIF_TERM));
//    ERL_NIF_TERM atom_undefined = enif_make_atom(env, "undefined");
//    for(int i = 0; i < size; i++){
//        arr[i] = atom_undefined;
//    }
//
//    return  enif_make_tuple2(env, argv[0], enif_make_tuple_from_array(env, arr, size));
//}
//
//static ERL_NIF_TERM insert_hash_tuple_2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]))
//{
//    unsigned int incr;
//    const ERL_NIF_TERM* hash_tuple;
//    int hash_tuple_size
//    const ERL_NIF_TERM* tuple;
//    int tuple_size;
//
//    // 检查结构
//    if(!enif_get_tuple(env, argv[1], &hash_tuple_size, &hash_tuple) || hash_tuple_size != 2)
//        return enif_make_badarg(env);
//    if (!enif_get_uint(env, hash_tuple[0], &incr) || incr <= 0)
//        return enif_make_badarg(env);
//    if (!enif_get_tuple(env, hash_tuple[1], &tuple_size, &tuple))
//        return enif_make_badarg(env);
//
//    // 检查插入的列表
//    if(!enif_is_list(env, argv[0]))
//        return enif_make_badarg(env);
//
//    // 遍历kv
//    ERL_NIF_TERM list = argv[0];
//    for(;;){
//        ERL_NIF_TERM head;
//        if(enif_get_list_cell(env, list, &head, &list)){
//            ERL_NIF_TERM kv_tuple;
//            int kv_tuple_size;
//            if(!enif_get_tuple(env, head, &kv_tuple_size, &kv_tuple) || kv_tuple_size != 2)
//                return enif_make_badarg(env);
//            if(!try_insert_hash_tuple(env, incr, kv_tuple, tuple_size, tuple))
//                return enif_make_badarg(env);
//        }else{
//            return enif_make_tuple2(env, incr, enif_make_tuple_from_array(env, tuple, tuple_size));
//        }
//    }
//}
//
//static ERL_NIF_TERM try_insert_hash_tuple(ErlNifEnv* env, int incr, ERL_NIF_TERM kv_tuple, int tuple_size, const ERL_NIF_TERM* tuple)
//{
//    // 获取hash值
//    ErlNifUInt64 hash = enif_hash(kv_tuple[0], ERL_NIF_PHASH2, tuple_size);
//    if(enif_is_tuple(env, tuple[hash])){
//        // 扩一下, hash支持最大值134217727
//        // 只能由使用者自己注意, 比较魔幻
//        if(tuple_size + incr > 134217727)
//            return enif_make_badarg(env);
//        // 复制数组, 重新hash, 耗费性能
//        if(!try_extend_hash_tuple(env, incr, tuple_size + incr, tuple_size, tuple))
//            return enif_make_badarg(env);
//        try_insert_hash_tuple(env, kv_tuple, tuple_size, tuple)
//    }else{
//        tuple[hash] = kv_tuple[1];
//    }
//
//   return true;
//}
//
//static ERL_NIF_TERM try_extend_hash_tuple(ErlNifEnv* env, int incr, int new_size, int old_size, const ERL_NIF_TERM* tuple)
//{
//    ERL_NIF_TERM* new_tuple = enif_alloc(new_size * sizeof(ERL_NIF_TERM));
//    ERL_NIF_TERM kv_tuple;
//    int kv_tuple_size;
//    for(int i = 0; i < size; i++){
//        if(!enif_get_tuple(env, tuple[i], &kv_tuple_size, &kv_tuple) || kv_tuple_size != 2)
//        ErlNifUInt64 hash = enif_hash(kv_tuple[0], ERL_NIF_PHASH2, new_size);
//        if(enif_is_tuple(env, tuple[hash])){
//            // 再来一次
//            if(!enif_realloc(new_tuple, new_size  * sizeof(ERL_NIF_TERM)))
//                return false;
//            if(new_size + incr > 134217727)
//                return false;
//            extend_hash_tuple(incr, new_size + inct, old_size, tuple)
//        }else{
//            tuple[hash] = kv_tuple;
//        }
//    }
//    if(!enif_realloc(tuple, old_size * sizeof(ERL_NIF_TERM)))
//        return false;
//    tuple = new_tuple;
//    return true;
//}

static ErlNifFunc nif_funcs[] =
{
    {"past_ptr", 1, past_ptr}
};

ERL_NIF_INIT(example_enif,nif_funcs,NULL,NULL,NULL,NULL)