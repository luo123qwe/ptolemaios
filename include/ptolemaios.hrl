-ifndef(PTOLEMAIOS_HRL).
-define(PTOLEMAIOS_HRL, true).

%% mask一下复制了erlang自带库的代码的数据结构的地方
%% 该宏只会出现在注释, 需要使用全局搜索查找
-define(PTOLEMAIOS_MASK_COPY_FROM_ERL, ptolemaios_mask_copy_from_erl).

-endif.