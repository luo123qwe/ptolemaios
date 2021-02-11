-module(xlsx2erl_battle_unit).

-behaviour(xlsx2erl_callback).


-include("xlsx2erl.hrl").
-include("xlsx2erl_battle_unit.hrl").

%% workbook和sheet的名字
-define(PRIV_WORKBOOK_NAME, "battle").
-define(PRIV_SHEET_NAME, "unit").
%% dets
-define(PRIV_DETS, ?XLSX2ERL_DETS_TABLE1(?PRIV_WORKBOOK_NAME)).
-define(PRIV_DETS_SHEET, list_to_atom(?PRIV_SHEET_NAME)).
-define(PRIV_DETS_INDEX, {index, ?MODULE}).
%% todo 生成的文件名字, hrl复制的mask
-define(PRIV_ERL_FILE, "data_battle_u.erl").
-define(PRIV_HRL_FILE, "ds.hrl").
-define(PRIV_MASK_TAG, "data_battle_u").

%% todo compile_body用的arg
-record(priv_arg, {
    body = [],% 生成内容
    keys = #{}% 全部key, 检查key是否重复
}).

-export([get_index/0]).

-export([update_dets/2, compile/1, clean/1]).

%% 字典数据, 用于报错
init_pd(Sheet) ->
    put(?PD_XLSX2ERL_SHEET, Sheet),
    put(?PD_XLSX2ERL_FIELD_DEF, record_info(fields, data_battle_u)).

clean_pd() ->
    erase(?PD_XLSX2ERL_SHEET),
    erase(?PD_XLSX2ERL_FIELD_DEF),
    erase(?PD_XLSX2ERL_ROW).

update_dets(Sheet, RawData) ->
    init_pd(Sheet),
    
    RowList = xlsx2erl:sheet_data_to_record(RawData, data_battle_u, record_info(fields, data_battle_u)),
    %% todo 可以生成自定义数据索引, 用来优化表交叉验证数据的效率
    %% todo 默认生成record第一个字段为key的map结构
    {RowList1, Index} = update_dets_convert_record(RowList, [], #{}),
    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET), Sheet}),
    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET), RowList1}),
    %% 保存自定义数据
    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET), Index}),
    
    clean_pd().

update_dets_convert_record([], RowList, Index) ->
    {RowList, Index};
update_dets_convert_record([H | T], RowList, Index) ->
    put(?PD_XLSX2ERL_ROW, H),
    %% todo 选择转换类型
    Record1 = #data_battle_u{
        id = ?XLSX2ERL_TO_INT2(#data_battle_u.id, H),
        name = ?XLSX2ERL_TO_BIN2(#data_battle_u.name, H),
        type = ?XLSX2ERL_TO_TERM2(#data_battle_u.type, H),
        radius = ?XLSX2ERL_TO_INT2(#data_battle_u.radius, H),
        skill_arg_1 = ?XLSX2ERL_TO_JSON2(#data_battle_u.skill_arg_1, H),
        skill_arg_2 = ?XLSX2ERL_TO_JSON2(#data_battle_u.skill_arg_2, H)
    },
    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],
    %% todo 构造数据索引
    Index1 = Index#{Record1#data_battle_u.id => Record1},
    update_dets_convert_record(T, RowList1, Index1).

%% todo 对应的获取自定义索引
get_index() ->
    xlsx2erl_util:ensure_dets(?PRIV_DETS),
    K = ?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET),
    [{_, V}] = dets:lookup(?PRIV_DETS, K),
    V.

compile(#xlsx2erl_cb_args{hrl_path = HrlPath, erl_path = ErlPath}) ->
    [{_, Sheet}] = dets:lookup(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET)),
    [{_, RowList}] = dets:lookup(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET)),
    init_pd(Sheet),
    
    %% todo 如果不需要复制hrl定义可以删除这句
    xlsx2erl_util:copy_mask_body(?MODULE, ?PRIV_MASK_TAG, HrlPath ++ "/" ++ ?PRIV_HRL_FILE),
    
    %% todo 构造文件内容, 如果生成多个函数, priv_arg多定义几个参数即可
    Head =
        "-module(" ++ filename:rootname(?PRIV_ERL_FILE) ++ ").\n\n"
    "-include(\"plm_lib.hrl\").\n"
    "-include(\"" ++ ?PRIV_HRL_FILE ++ "\").\n\n"
    "-export([get/1, get/2]).\n\n"
    "get(Key) -> get(Key, true).\n\n",
    #priv_arg{body = Body} = compile_row_list(RowList, #priv_arg{}),
    Tail =
        "get(Key, _) ->\n"
        "    ?LOG_ERROR(\"undefined ~w in " ++ filename:rootname(?PRIV_ERL_FILE) ++ "\", [Key]),\n"
    "    undefined.",
    file:make_dir(ErlPath ++ "/" ++ ?PRIV_WORKBOOK_NAME),
    File = ErlPath ++ "/" ++ ?PRIV_WORKBOOK_NAME ++ "/" ++ ?PRIV_ERL_FILE,
    ok = file:write_file(File, [Head, Body, Tail]),
    
    clean_pd().

compile_row_list([], Arg) -> Arg;
compile_row_list([H | T], Arg) ->
    %% 再套一层, 提示报错
    put(?PD_XLSX2ERL_ROW, H),
    case catch compile_row(H, Arg) of
        {'EXIT', ?XLSX2ERL_ERROR} ->% 已知错误
            exit(?XLSX2ERL_ERROR);
        #priv_arg{} = Arg1 ->
            compile_row_list(T, Arg1);
        Error ->
            ?XLSX2ERL_PD_ERROR2("unknow error ~w", [Error]),
            exit(?XLSX2ERL_ERROR)
    end.

compile_row(#xlsx2erl_row{record = Record}, #priv_arg{body = Body, keys = Keys}) ->
    %% todo 添加数值检查
    %% todo 是否重复key
    Key = Record#data_battle_u.id,
    ?DO_IF(maps:is_key(Key, Keys), ?XLSX2ERL_PD_ERROR2("key ~w 重复了", [Key])),
    %% todo 添加数值转换
    %% [{arg, value}]
    F = fun(K, V, Acc) -> [{bin_to_atom(K), bin_to_atom(V)} | Acc] end,
    Record1 = Record#data_battle_u{
        skill_arg_1 = maps:fold(F, [], Record#data_battle_u.skill_arg_1),
        skill_arg_2 = maps:fold(F, [], Record#data_battle_u.skill_arg_2)
    },
    BodyRow = compile_body(Record1),
    Body1 = [BodyRow | Body],
    Keys1 = Keys#{Key => 1},
    #priv_arg{body = Body1, keys = Keys1}.

%% todo 构造文本
compile_body(Record) ->
    "get(" ++ xlsx2erl_util:to_iolist(Record#data_battle_u.id) ++ ", _) -> \n" ++
        "    #data_battle_u{\n"
        "        id = " ++ xlsx2erl_util:to_iolist(Record#data_battle_u.id) ++ ", \n" ++
        "        name = " ++ xlsx2erl_util:to_iolist(Record#data_battle_u.name) ++ ", \n" ++
        "        type = " ++ xlsx2erl_util:to_iolist(Record#data_battle_u.type) ++ ", \n" ++
        "        radius = " ++ xlsx2erl_util:to_iolist(Record#data_battle_u.radius) ++ ", \n" ++
        "        skill_arg_1 = " ++ xlsx2erl_util:to_iolist(Record#data_battle_u.skill_arg_1) ++ ", \n" ++
        "        skill_arg_2 = " ++ xlsx2erl_util:to_iolist(Record#data_battle_u.skill_arg_2) ++ "\n" ++
        "    };\n".

clean(#xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath}) ->
    %% 先删dets
    xlsx2erl_util:ensure_dets(?PRIV_DETS),
    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET)),
    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET)),
    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET)),
    %% 再删文件
    %% todo 如果不需要复制hrl定义可以删除这句
    xlsx2erl_util:delete_mask_body(?PRIV_MASK_TAG, HrlPath ++ "/" ++ ?PRIV_HRL_FILE),
    file:delete(ErlPath ++ "/" ++ ?PRIV_WORKBOOK_NAME ++ "/" ++ ?PRIV_ERL_FILE).


bin_to_atom(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin);
bin_to_atom(Term) ->
    Term.