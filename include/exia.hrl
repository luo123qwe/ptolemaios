


%% 逻辑用最大最小值
-define(EXIA_TREE_MAX, exia_tree_max).
-define(EXIA_TREE_MIN, exia_tree_min).
%% 逻辑用key值
-define(EXIA_TREE_KEY, exia_tree_key).

%% 索引元素
-record(exia_i, {% exia_index
    key :: exia:key(),
    alias :: exia:alias(),
    index :: exia_dict:dict()|exia_tree:root()
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