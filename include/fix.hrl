%% tag
-define(FIX_DETS_PATH, "fix_dets").

%% dets
-define(DETS_FIX, fix).

%% dynamic
-define(DYM_FIX_MODULE2(Module, Index), list_to_atom(atom_to_list(Module) ++ "_" ++ integer_to_list(Index))).
