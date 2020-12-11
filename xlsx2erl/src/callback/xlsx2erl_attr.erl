-module(xlsx2erl_attr).

-behaviour(xlsx2erl_callback).

-include("xlsx2erl.hrl").

-include("xlsx2erl_attr.hrl").

-define(DETS_DICT1(Name), {Name, dict}).

-export([get_data_attr/0]).

-export([update_dets/1, compile/1, clean/1]).

update_dets(FileName) ->
    RecordDef = [
        {data_attr, record_info(fields, data_attr)}
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
update_dets_convert([#xlsx2erl_sheet{name = data_attr, row_list = RowList} = H | T], SheetList, DictList) ->
    {OldRowList, OldDict} = update_dets_convert_sheet_and_dict(data_attr, SheetList, DictList),
    {RowList1, Dict1} = update_dets_convert_record_data_attr(RowList, H, record_info(fields, data_attr), OldRowList, OldDict),
    SheetList1 = lists:keystore(data_attr, #xlsx2erl_sheet.name, SheetList, H#xlsx2erl_sheet{row_list = RowList1}),
    DictList1 = lists:keystore(data_attr, 1, DictList, {data_attr, Dict1}),
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

update_dets_convert_record_data_attr([], _Sheet, _RecordDef, RowList, Dict) ->
    {RowList, Dict};
update_dets_convert_record_data_attr([H | T], Sheet, RecordDef, RowList, Dict) ->
    %% todo 选择转换类型
    Record1 = #data_attr{
        id = xlsx2erl_util:convert_int(#data_attr.id, RecordDef, H, Sheet),
        name = xlsx2erl_util:convert_bin(#data_attr.name, RecordDef, H, Sheet),
        macro = xlsx2erl_util:convert_bin(#data_attr.macro, RecordDef, H, Sheet)
    },
    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],
    %% todo 构造数据索引
    Dict1 = dict:store(element(2, Record1), Record1, Dict),
    update_dets_convert_record_data_attr(T, Sheet, RecordDef, RowList1, Dict1).

%% todo 对应的获取自定义数据方法, 默认获取dict
get_data_attr() ->
    xlsx2erl_util:ensure_dets(?DETS_XLSX2ERL1(?MODULE)),
    [#xlsx2erl_dets{v = V}] = dets:lookup(?DETS_XLSX2ERL1(?MODULE), ?DETS_DICT1(data_attr)),
    V.

compile(#xlsx2erl_cb_args{hrl_path = _HrlPath} = Args) ->
    #xlsx2erl_excel{sheet_list = SheetList} = xlsx2erl_util:get_excel(?MODULE),
    do_compile(SheetList, Args).

do_compile([], _Args) ->
    ok;
do_compile([#xlsx2erl_sheet{name = data_attr, row_list = RowList} | T], #xlsx2erl_cb_args{hrl_path = HrlPath} = Args) ->
    Head = "",
    Body =
        lists:map(fun(#xlsx2erl_row{record = Record}) ->
            %% -define(ATTR_X, Id).% 名字
            ["-define(ATTR_", Record#data_attr.macro, ", "
                , xlsx2erl_util:to_iolist(Record#data_attr.id), ").% "
                , Record#data_attr.name, "\n"]
                  end, RowList),
    Tail = "",
    ok = file:write_file(HrlPath ++ "/" ++ ?XLSX2ERL_DEFAULT_HRL, [Head, Body, Tail]),
    do_compile(T, Args).

clean(#xlsx2erl_cb_args{hrl_path = HrlPath}) ->
    file:delete(HrlPath ++ "/" ++ ?XLSX2ERL_DEFAULT_HRL),
    catch dets:close(?DETS_XLSX2ERL1(?MODULE)),
    file:delete(?DETS_PATH ++ "/" ++ ?MODULE_STRING).

