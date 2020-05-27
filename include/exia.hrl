-define(EXIA_TREE_ELEMENT(Key, Record), {Key, Record}).

%% 索引元素
-record(exia_element, {
    key :: exia_template:key(),
    alias :: exia_template:alias(),
    index :: dict:dict()|exia_tree:tree()
}).

%% 总索引
-record(exia_index, {
    private_key :: exia_template:key(),
    tree = [] :: [#exia_element{}],
    dict = [] :: [#exia_element{}]
}).