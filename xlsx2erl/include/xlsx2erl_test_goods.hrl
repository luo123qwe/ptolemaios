%% xlsx2erl mask start data_test_goods
%% 物品
-record(data_test_goods, {
    id,% id
    type,% 类型
    name,% 名字
    price,% 价格
    resolve_reward% 分解奖励, [{物品id, 数量}]
}).
%% xlsx2erl mask end data_test_goods
