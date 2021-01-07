%%%%%%%%%%% data_test_goods record define start%%%%%%%%%%%%%%%%%
%% 物品
-record(data_test_goods, {
    id,% id
    type,% 类型
    name,% 名字
    price,% 价格
    resolve_reward% 分解奖励, [{物品id, 数量}]
}).
%%%%%%%%%%% data_test_goods record define end%%%%%%%%%%%%%%%%%%%
