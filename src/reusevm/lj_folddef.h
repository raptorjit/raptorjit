/* This is a generated file. DO NOT EDIT! */

static const FoldFunc fold_func[] = {
  fold_kfold_numarith,
  fold_kfold_numabsneg,
  fold_kfold_ldexp,
  fold_kfold_fpmath,
  fold_kfold_fpcall1,
  fold_kfold_fpcall2,
  fold_kfold_numpow,
  fold_kfold_numcomp,
  fold_kfold_intarith,
  fold_kfold_intovarith,
  fold_kfold_bnot,
  fold_kfold_bswap,
  fold_kfold_intcomp,
  fold_kfold_intcomp0,
  fold_kfold_int64arith,
  fold_kfold_int64arith2,
  fold_kfold_int64shift,
  fold_kfold_bnot64,
  fold_kfold_bswap64,
  fold_kfold_int64comp,
  fold_kfold_int64comp0,
  fold_kfold_snew_kptr,
  fold_kfold_snew_empty,
  fold_kfold_strref,
  fold_kfold_strref_snew,
  fold_kfold_strcmp,
  fold_bufhdr_merge,
  fold_bufput_bufstr,
  fold_bufput_kgc,
  fold_bufstr_kfold_cse,
  fold_bufput_kfold_op,
  fold_bufput_kfold_rep,
  fold_bufput_kfold_fmt,
  fold_kfold_add_kgc,
  fold_kfold_add_kptr,
  fold_kfold_add_kright,
  fold_kfold_tobit,
  fold_kfold_conv_kint_num,
  fold_kfold_conv_kintu32_num,
  fold_kfold_conv_kint_ext,
  fold_kfold_conv_kint_i64,
  fold_kfold_conv_kint64_num_i64,
  fold_kfold_conv_kint64_num_u64,
  fold_kfold_conv_kint64_int_i64,
  fold_kfold_conv_knum_int_num,
  fold_kfold_conv_knum_u32_num,
  fold_kfold_conv_knum_i64_num,
  fold_kfold_conv_knum_u64_num,
  fold_kfold_tostr_knum,
  fold_kfold_tostr_kint,
  fold_kfold_strto,
  lj_opt_cse,
  fold_kfold_kref,
  fold_shortcut_round,
  fold_shortcut_left,
  fold_shortcut_dropleft,
  fold_shortcut_leftleft,
  fold_simplify_numadd_negx,
  fold_simplify_numadd_xneg,
  fold_simplify_numsub_k,
  fold_simplify_numsub_negk,
  fold_simplify_numsub_xneg,
  fold_simplify_nummuldiv_k,
  fold_simplify_nummuldiv_negk,
  fold_simplify_nummuldiv_negneg,
  fold_simplify_numpow_k,
  fold_shortcut_conv_num_int,
  fold_simplify_conv_int_num,
  fold_simplify_conv_i64_num,
  fold_simplify_conv_int_i64,
  fold_simplify_conv_flt_num,
  fold_simplify_tobit_conv,
  fold_simplify_floor_conv,
  fold_simplify_conv_sext,
  fold_simplify_conv_narrow,
  fold_cse_conv,
  fold_narrow_convert,
  fold_simplify_intadd_k,
  fold_simplify_intmul_k,
  fold_simplify_intsub_k,
  fold_simplify_intsub_kleft,
  fold_simplify_intadd_k64,
  fold_simplify_intsub_k64,
  fold_simplify_intmul_k32,
  fold_simplify_intmul_k64,
  fold_simplify_intmod_k,
  fold_simplify_intmod_kleft,
  fold_simplify_intsub,
  fold_simplify_intsubadd_leftcancel,
  fold_simplify_intsubsub_leftcancel,
  fold_simplify_intsubsub_rightcancel,
  fold_simplify_intsubadd_rightcancel,
  fold_simplify_intsubaddadd_cancel,
  fold_simplify_band_k,
  fold_simplify_bor_k,
  fold_simplify_bxor_k,
  fold_simplify_shift_ik,
  fold_simplify_shift_andk,
  fold_simplify_shift1_ki,
  fold_simplify_shift2_ki,
  fold_simplify_shiftk_andk,
  fold_simplify_andk_shiftk,
  fold_simplify_andor_k,
  fold_simplify_andor_k64,
  fold_reassoc_intarith_k,
  fold_reassoc_intarith_k64,
  fold_reassoc_dup,
  fold_reassoc_dup_minmax,
  fold_reassoc_bxor,
  fold_reassoc_shift,
  fold_reassoc_minmax_k,
  fold_abc_fwd,
  fold_abc_k,
  fold_abc_invar,
  fold_comm_swap,
  fold_comm_equal,
  fold_comm_comp,
  fold_comm_dup,
  fold_comm_dup_minmax,
  fold_comm_bxor,
  fold_merge_eqne_snew_kgc,
  lj_opt_fwd_aload,
  fold_kfold_hload_kkptr,
  lj_opt_fwd_hload,
  lj_opt_fwd_uload,
  lj_opt_fwd_alen,
  fold_cse_uref,
  lj_opt_fwd_hrefk,
  fold_fwd_href_tnew,
  fold_fwd_href_tdup,
  fold_fload_tab_tnew_asize,
  fold_fload_tab_tnew_hmask,
  fold_fload_tab_tdup_asize,
  fold_fload_tab_tdup_hmask,
  fold_fload_tab_ah,
  fold_fload_str_len_kgc,
  fold_fload_str_len_snew,
  fold_fload_str_len_tostr,
  fold_fload_sbuf,
  fold_fload_func_ffid_kgc,
  fold_fload_cdata_typeid_kgc,
  fold_fload_cdata_int64_kgc,
  fold_fload_cdata_typeid_cnew,
  fold_fload_cdata_ptr_int64_cnew,
  lj_opt_cse,
  lj_opt_fwd_fload,
  fold_fwd_sload,
  fold_xload_kptr,
  lj_opt_fwd_xload,
  fold_fold_base,
  fold_barrier_tab,
  fold_barrier_tnew_tdup,
  lj_opt_dse_ahstore,
  lj_opt_dse_ustore,
  lj_opt_dse_fstore,
  lj_opt_dse_xstore,
  lj_ir_emit
};

static const uint32_t fold_hash[908] = {
0x634c5bff,
0x00526c1b,
0x8f894c16,
0x45b56a95,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x5b53fc28,
0x48676801,
0x8173441b,
0x22506016,
0xffffffff,
0xffffffff,
0xffffffff,
0x614bfc20,
0xffffffff,
0xffffffff,
0x015c6c44,
0xffffffff,
0xffffffff,
0x07126c1b,
0xffffffff,
0x7745ffff,
0xffffffff,
0xffffffff,
0x3e55fc1b,
0x08625816,
0xffffffff,
0xffffffff,
0x08445816,
0x22506416,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x0e40701c,
0x1fc18c16,
0xffffffff,
0xffffffff,
0xffffffff,
0x5e43fc1c,
0x9089fc00,
0x7405ffff,
0xffffffff,
0x9b9bffff,
0xffffffff,
0xffffffff,
0x6854a816,
0x0c045816,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x43b56a8e,
0x1300701c,
0xffffffff,
0x3f56b81b,
0xffffffff,
0xffffffff,
0x9089fc04,
0xffffffff,
0xffffffff,
0xffffffff,
0x31b85bff,
0xffffffff,
0xffffffff,
0x64468016,
0xffffffff,
0xffffffff,
0x20c18c0b,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x4cb4a2ae,
0xffffffff,
0xffffffff,
0x07066c1b,
0x0f56701c,
0x5c52a028,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x8d885c17,
0x96b1ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x49b5feb3,
0xffffffff,
0x4f53fc16,
0x4cb4a6ae,
0xffffffff,
0x385cbbff,
0x8a89fc10,
0xffffffff,
0xffffffff,
0xffffffff,
0x123e7000,
0x908fffff,
0x7e785fff,
0x27b45a72,
0xffffffff,
0x7f71ffff,
0xffffffff,
0xffffffff,
0x2351fc18,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x6940801c,
0x6a4287ff,
0x9089fc14,
0x634c73ff,
0x83894009,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x8e894814,
0xffffffff,
0x096c5816,
0x6d468c16,
0xffffffff,
0xffffffff,
0x084e5816,
0xffffffff,
0xffffffff,
0xffffffff,
0x3566cc01,
0x6740841c,
0x7a846400,
0xffffffff,
0x85894409,
0xffffffff,
0xffffffff,
0xffffffff,
0x9cc3ffff,
0x740fffff,
0x0a3c5800,
0x8e894c14,
0x9ca5ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x0c0e5816,
0x8689fc09,
0xffffffff,
0x6047fc16,
0xffffffff,
0xffffffff,
0x3950bbff,
0x130a701c,
0xffffffff,
0x634a5bff,
0x00506c1b,
0xffffffff,
0x383e7c00,
0x46b569ae,
0x9511fc0d,
0x30b86fff,
0x7665ffff,
0xffffffff,
0x44b56aae,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x6149fc20,
0x4ab4a276,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x07106c1b,
0x1ec18c14,
0x7543ffff,
0xffffffff,
0xffffffff,
0x3b53fc1b,
0xffffffff,
0x28b45ab4,
0xffffffff,
0x08425816,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x4ab4a676,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x938a67ff,
0xffffffff,
0x5d41fc1c,
0x7c87fc00,
0x7403ffff,
0xffffffff,
0x9a99ffff,
0x49b5fed3,
0xffffffff,
0x5559fc16,
0x0c025816,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x4ab4aa76,
0x1ec18c09,
0xffffffff,
0xffffffff,
0x3f54b81b,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x34125c17,
0xffffffff,
0xffffffff,
0x8d885c15,
0xffffffff,
0xffffffff,
0x375eb844,
0xffffffff,
0xffffffff,
0xffffffff,
0x07046c1b,
0x0e54701c,
0xffffffff,
0x955237ff,
0x8a89fc0e,
0xffffffff,
0xffffffff,
0xffffffff,
0x89897000,
0xffffffff,
0x27b45a70,
0x104e7016,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x4d51fc16,
0x365ebc44,
0x6a4083ff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x065a6c1b,
0x113c7000,
0x928dffff,
0x7e765fff,
0xffffffff,
0x45b56a76,
0x68448816,
0xffffffff,
0xffffffff,
0x2ab471d6,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x634a73ff,
0xffffffff,
0xffffffff,
0x44b56ace,
0xffffffff,
0x32ba5c00,
0x02606c16,
0xffffffff,
0x096a5816,
0x4ab4a296,
0xffffffff,
0xffffffff,
0x084c5816,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x8689fc07,
0x26b459d4,
0x81734417,
0xffffffff,
0xffffffff,
0x28b45ad4,
0xffffffff,
0x383c7800,
0x9cc1ffff,
0x740dffff,
0x20c18c0e,
0xffffffff,
0x4ab4a696,
0xffffffff,
0xffffffff,
0x0c0c5816,
0xffffffff,
0xffffffff,
0x5f45fc16,
0xffffffff,
0xffffffff,
0xffffffff,
0x1308701c,
0xffffffff,
0x62485bff,
0x8b885c03,
0xffffffff,
0x140bfc1c,
0xffffffff,
0xffffffff,
0xffffffff,
0x7663ffff,
0x4ab4aa96,
0x6850a016,
0x644e8016,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x6147fc20,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x070e6c1b,
0xffffffff,
0x7541ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x08405816,
0x4056b82e,
0xffffffff,
0x9089fc17,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x7b85fc00,
0x7401ffff,
0x00646c1b,
0xffffffff,
0x9997ffff,
0xffffffff,
0x42b569d3,
0xffffffff,
0x0c005816,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x3c52b81b,
0x8f894c17,
0x45b56a96,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x5a53fc29,
0x48676802,
0x66428016,
0x34105c17,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x24b66c1b,
0x07026c1b,
0x0e52701c,
0xffffffff,
0xffffffff,
0xffffffff,
0x5455fc1c,
0x9c17ffff,
0xffffffff,
0xffffffff,
0x68428416,
0x1dadffff,
0x104c7016,
0x187f3bff,
0x4e6dfc16,
0xffffffff,
0xffffffff,
0x604ffc16,
0xffffffff,
0xffffffff,
0xffffffff,
0x3412701c,
0x9089fc01,
0x78133817,
0x50525bff,
0x6e64c816,
0x948bffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x726dffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x04bc6fff,
0x624873ff,
0x6b64cbff,
0x81734415,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x415bfc1b,
0x09685816,
0xffffffff,
0x20c18c0c,
0x084a5816,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x3313101a,
0xffffffff,
0x2bb47275,
0x21505c1c,
0xffffffff,
0xffffffff,
0x9cbfffff,
0x740bffff,
0xffffffff,
0xffffffff,
0x9ca1ffff,
0x9cb20000,
0x6d4e9c16,
0xffffffff,
0x0c0a5816,
0xffffffff,
0xffffffff,
0x5e43fc16,
0xffffffff,
0xffffffff,
0x3d53fc2e,
0x8a89fc11,
0x1306701c,
0x62465bff,
0x2250601c,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x644c8016,
0x2351fc19,
0xffffffff,
0x2cb46e6e,
0xffffffff,
0xffffffff,
0xffffffff,
0x9089fc15,
0xffffffff,
0xffffffff,
0x2250641c,
0x070c6c1b,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x085c5816,
0xffffffff,
0xffffffff,
0xffffffff,
0x4054b82e,
0x177e5c16,
0xffffffff,
0xffffffff,
0x3566cc02,
0xffffffff,
0xffffffff,
0xffffffff,
0x6954a81c,
0xffffffff,
0xffffffff,
0x34126bff,
0x7983fc00,
0xffffffff,
0x00626c1b,
0x8f894c15,
0x9895ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x9677ffff,
0xffffffff,
0x48676800,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x505273ff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x7255ffff,
0x159c6416,
0xffffffff,
0x68408016,
0xffffffff,
0xffffffff,
0xffffffff,
0x08545816,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x07006c1b,
0x0e50701c,
0x1ec18c15,
0x6e62c416,
0xffffffff,
0x5253fc1c,
0x7115ffff,
0xffffffff,
0xffffffff,
0x66408416,
0x1cabffff,
0x104a7016,
0x4d6bfc16,
0x0c145816,
0x2bb47295,
0x97af4000,
0x604dfc16,
0xffffffff,
0xffffffff,
0xffffffff,
0x3410701c,
0x6b62c7ff,
0x78113817,
0x00566c1b,
0xffffffff,
0x9189ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x576bffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x97af4400,
0x20c18c0a,
0x614ffc20,
0xffffffff,
0xffffffff,
0x624673ff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x2db46e8e,
0x96affc00,
0x65408c16,
0x6d4c9816,
0x169ffc16,
0x08485816,
0x8d885c16,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x3311101a,
0x0e44701c,
0xffffffff,
0xffffffff,
0x8a89fc0f,
0xffffffff,
0xffffffff,
0x7409ffff,
0xffffffff,
0xffffffff,
0x9c9fffff,
0x65409016,
0x27b45a71,
0xffffffff,
0x0c085816,
0xffffffff,
0xffffffff,
0x5d41fc16,
0x2351fc17,
0xffffffff,
0x3a51fc2e,
0x1304701c,
0xffffffff,
0xffffffff,
0x8a89fc13,
0x82894008,
0x9c7dffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x4cb6a01b,
0x3566cc00,
0xffffffff,
0xffffffff,
0x070a6c1b,
0x0f5a701c,
0x84894408,
0xffffffff,
0xffffffff,
0x9ca3fc00,
0xffffffff,
0xffffffff,
0xffffffff,
0x4bb5ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x8689fc08,
0xffffffff,
0x4cb6a41b,
0xffffffff,
0x87885c00,
0xffffffff,
0x34106bff,
0xffffffff,
0xffffffff,
0x00606c1b,
0x20c18c0f,
0x9893ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x9c75ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x4ab4a275,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x1ec18c13,
0x5753ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x28b45ab3,
0xffffffff,
0x08525816,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x4ab4a675,
0xffffffff,
0x2eb46eae,
0xffffffff,
0xffffffff,
0xffffffff,
0x03666fff,
0x5151fc1c,
0x7313ffff,
0xffffffff,
0xffffffff,
0x3413fc1a,
0x1aa9ffff,
0x10487016,
0x4d69fc16,
0x34125816,
0x19bd8c00,
0xffffffff,
0x604bfc16,
0xffffffff,
0x6944881c,
0x4ab4aa75,
0x130e701c,
0xffffffff,
0x634e5bff,
0x00546c1b,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x7269ffff,
0x6d4a9416,
0x47b7681b,
0x4cb4a26e,
0xffffffff,
0xffffffff,
0xffffffff,
0x0d0bfc16,
0x8c885c14,
0x614dfc20,
0xffffffff,
0x015e6c44,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x3e57fc1b,
0x08645816,
0x6f15fc28,
0x169dfc16,
0x08465816,
0x4cb4a66e,
0x27b45a6f,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x0e42701c,
0x1babfc56,
0x5852a3ff,
0xffffffff,
0xffffffff,
0x5f45fc1c,
0x7407ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x45b56a75,
0x0c065816,
0xffffffff,
0xffffffff,
0x29b471d5,
0xffffffff,
0xffffffff,
0xffffffff,
0x1302701c,
0x6950a01c,
0x5952a7ff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x64488016,
0x4ab4a295,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x05bd8c42,
0x807343ff,
0xffffffff,
0x8689fc06,
0x25b459d3,
0x07086c1b,
0x0f58701c,
0xffffffff,
0x28b45ad3,
0xffffffff,
0x08585816,
0xffffffff,
0xffffffff,
0x20c18c0d,
0xffffffff,
0x4ab4a695,
0xffffffff,
0x2fb46ece,
0xffffffff,
0xffffffff,
0xffffffff,
0x5355fc16,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x56585bff,
0x43b56a6e,
0xffffffff,
0x7d91ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x8673ffff,
0x4ab4aa95,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x8a89fc12,
0x7015fc16,
0x6742801c,
0xffffffff,
0xffffffff,
0x634e73ff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x7251ffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x88893800,
0xffffffff,
0x08505816,
0xffffffff,
0xffffffff,
0x9089fc16,
0xffffffff,
0x6942841c,
0x6c448bff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0xffffffff,
0x9cc5ffff,
0x7311ffff,
0x0b3e5800,
0x9553fc0d,
0x3411fc1a,
0x6d489016,
0x10467016,
0xffffffff,
0x34105816,
0x21505c16,
0xffffffff,
0x6049fc16,
0xffffffff,
0xffffffff,
0xffffffff,
0x130c701c,
0xffffffff,
0xffffffff
};

#define fold_hashkey(k)	(lj_rol(lj_rol((k),14)-(k),28)%907)

