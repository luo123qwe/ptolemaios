-define(EXIA_TREE_ELEMENT(Key, Record), {Key, Record}).

%% 索引元素
-record(exia_i, {% exia_index
    key :: exia:key(),
    alias :: exia:alias(),
    index :: dict:dict()|exia_tree:tree()
}).

%% 总索引
-record(exia, {
    private_key :: exia:key(),
    tree = [] :: [#exia_i{}],
    dict = [] :: [#exia_i{}]
}).

%% 索引存储的值
-record(exia_ie, {% exia_index_element
    private_key,
    key,
    record
}).