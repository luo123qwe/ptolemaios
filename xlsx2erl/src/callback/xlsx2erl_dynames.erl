-module(xlsx2erl_dynames).

-behaviour(xlsx2erl_callback).

-include("xlsx2erl.hrl").

-include("xlsx2erl_dynames.hrl").

-define(DETS_DICT1(Name), {Name, dict}).

-export([get_data_dynames_unit/0]).

-export([update_dets/1, compile/1, clean/1]).

update_dets(FileName) ->
    RecordDef = [
        {data_dynames_unit, record_info(fields, data_dynames_unit)}
    ],
    case xlsx2erl:get_sheet_data(RecordDef, FileName) of
        SheetList when is_list(SheetList) ->
            %% todo 可以生成自定义数据, 可以用来优化表交叉验证数据的效率
            %% todo 默认生成record第一个字段为key的dict结构
            {SheetList1, DictList} = update_dets_convert(SheetList, [], []),
            Now = erlang:localtime(),
            dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_excel{name = ?MODULE, excel_name = filename:basename(FileName), sheet_list = SheetList1}),
            dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_dets{k = ?XLSX2ERL_EXCEL_UPDATE_TIME, v = Now}),
            %% todo 保存自定义数据
            lists:foreach(fun({K, V}) ->
                dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_dets{k = ?DETS_DICT1(K), v = V})
                          end, DictList);
        _ ->
            error
    end.

update_dets_convert([], SheetList, DictList) ->
    {SheetList, DictList};
update_dets_convert([#xlsx2erl_sheet{name = data_dynames_unit, row_list = RowList} = H | T], SheetList, DictList) ->
    {OldRowList, OldDict} = update_dets_convert_sheet_and_dict(data_dynames_unit, SheetList, DictList),
    {RowList1, Dict1} = update_dets_convert_record_data_dynames_unit(RowList, H, record_info(fields, data_dynames_unit), OldRowList, OldDict),
    SheetList1 = lists:keystore(data_dynames_unit, #xlsx2erl_sheet.name, SheetList, H#xlsx2erl_sheet{row_list = RowList1}),
    DictList1 = lists:keystore(data_dynames_unit, 1, DictList, {data_dynames_unit, Dict1}),
    update_dets_convert(T, SheetList1, DictList1).

update_dets_convert_sheet_and_dict(Name, SheetList, DictList) ->
    case lists:keyfind(Name, #xlsx2erl_sheet.name, SheetList) of
        false -> OldRowList = [];
        #xlsx2erl_sheet{row_list = OldRowList} -> ok
    end,
    case lists:keyfind(Name, 1, DictList) of
        false -> OldDict = dict:new();
        {_, OldDict} -> ok
    end,
    {OldRowList, OldDict}.

update_dets_convert_record_data_dynames_unit([], _Sheet, _RecordDef, RowList, Dict) ->
    {RowList, Dict};
update_dets_convert_record_data_dynames_unit([H | T], Sheet, RecordDef, RowList, Dict) ->
    %% todo 选择转换类型
    Record1 = #data_dynames_unit{
        id = xlsx2erl_util:convert_bin(#data_dynames_unit.id, RecordDef, H, Sheet),
        name = xlsx2erl_util:convert_bin(#data_dynames_unit.name, RecordDef, H, Sheet),
        type = xlsx2erl_util:convert_bin(#data_dynames_unit.type, RecordDef, H, Sheet),
        radius = xlsx2erl_util:convert_bin(#data_dynames_unit.radius, RecordDef, H, Sheet),
        skill_arg_1 = xlsx2erl_util:convert_bin(#data_dynames_unit.skill_arg_1, RecordDef, H, Sheet),
        skill_arg_2 = xlsx2erl_util:convert_bin(#data_dynames_unit.skill_arg_2, RecordDef, H, Sheet)
    },
    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],
    %% todo 构造数据索引
    Dict1 = dict:store(element(2, Record1), Record1, Dict),
    update_dets_convert_record_data_dynames_unit(T, Sheet, RecordDef, RowList1, Dict1).

%% todo 对应的获取自定义数据方法, 默认获取dict
get_data_dynames_unit() ->
    xlsx2erl_util:ensure_dets(?DETS_XLSX2ERL1(?MODULE)),
    [#xlsx2erl_dets{v = V}] = dets:lookup(?DETS_XLSX2ERL1(?MODULE), ?DETS_DICT1(data_dynames_unit)),
    V.

compile(#xlsx2erl_cb_args{hrl_path = HrlPath} = Args) ->
    #xlsx2erl_excel{sheet_list = SheetList} = xlsx2erl_util:get_excel(?MODULE),
    %% todo 删除不需要的copy
    xlsx2erl_util:copy_mask_body(?MODULE, "data_dynames_unit", HrlPath ++ "/" ++ ?XLSX2ERL_DEFAULT_HRL),
    do_compile(SheetList, Args).

do_compile([], _Args) ->
    ok;
do_compile([#xlsx2erl_sheet{name = data_dynames_unit, row_list = RowList} | T], #xlsx2erl_cb_args{erl_path = ErlPath} = Args) ->
    Head =
        "-module(data_dynames_unit).\n\n"
    "-include(\"" ++ ?XLSX2ERL_DEFAULT_HRL ++ "\").\n\n"
    "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#xlsx2erl_row{record = Record}) ->
            %% todo 添加数值检查
            "get(" ++ xlsx2erl_util:to_iolist(Record#data_dynames_unit.id) ++ ") -> \n" ++
                "    #data_dynames_unit{"
                "id = " ++ xlsx2erl_util:to_iolist(Record#data_dynames_unit.id) ++ ", " ++
                "name = " ++ xlsx2erl_util:to_iolist(Record#data_dynames_unit.name) ++ ", " ++
                "type = " ++ xlsx2erl_util:to_iolist(Record#data_dynames_unit.type) ++ ", " ++
                "radius = " ++ xlsx2erl_util:to_iolist(Record#data_dynames_unit.radius) ++ ", " ++
                "skill_arg_1 = " ++ xlsx2erl_util:to_iolist(Record#data_dynames_unit.skill_arg_1) ++ ", " ++
                "skill_arg_2 = " ++ xlsx2erl_util:to_iolist(Record#data_dynames_unit.skill_arg_2) ++
                "};\n"
                  end, RowList),
    Tail =
        "get(_) -> undefined.",
    ok = file:write_file(ErlPath ++ "/data_dynames_unit.erl", [Head, Body, Tail]),
    do_compile(T, Args).

clean(#xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath}) ->
    file:delete(HrlPath ++ "/" ++ ?XLSX2ERL_DEFAULT_HRL),
    catch dets:close(?DETS_XLSX2ERL1(?MODULE)),
    file:delete(?DETS_PATH ++ "/" ++ ?MODULE_STRING),
    file:delete(ErlPath ++ "/data_dynames_unit.erl").

