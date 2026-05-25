
/home/khalid/Documents/GitHub/Journey_ECS/main-release.bin:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 c9 6f 00 00 	mov    0x6fc9(%rip),%rax        # 407fd8 <__gmon_start__>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 6f 00 00    	push   0x6fca(%rip)        # 407ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 6f 00 00    	jmp    *0x6fcc(%rip)        # 407ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 6f 00 00    	jmp    *0x6fca(%rip)        # 408000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <memset@plt>:
  401040:	ff 25 c2 6f 00 00    	jmp    *0x6fc2(%rip)        # 408008 <memset@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <calloc@plt>:
  401050:	ff 25 ba 6f 00 00    	jmp    *0x6fba(%rip)        # 408010 <calloc@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <memcpy@plt>:
  401060:	ff 25 b2 6f 00 00    	jmp    *0x6fb2(%rip)        # 408018 <memcpy@GLIBC_2.14>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <malloc@plt>:
  401070:	ff 25 aa 6f 00 00    	jmp    *0x6faa(%rip)        # 408020 <malloc@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <realloc@plt>:
  401080:	ff 25 a2 6f 00 00    	jmp    *0x6fa2(%rip)        # 408028 <realloc@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <memmove@plt>:
  401090:	ff 25 9a 6f 00 00    	jmp    *0x6f9a(%rip)        # 408030 <memmove@GLIBC_2.2.5>
  401096:	68 06 00 00 00       	push   $0x6
  40109b:	e9 80 ff ff ff       	jmp    401020 <_init+0x20>

Disassembly of section .text:

00000000004010a0 <runtime::slice_handle_error>:
  4010a0:	41 57                	push   %r15
  4010a2:	41 56                	push   %r14
  4010a4:	53                   	push   %rbx
  4010a5:	48 83 ec 30          	sub    $0x30,%rsp
  4010a9:	4d 89 ce             	mov    %r9,%r14
  4010ac:	4d 89 c7             	mov    %r8,%r15
  4010af:	48 8b 5c 24 50       	mov    0x50(%rsp),%rbx
  4010b4:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4010b8:	c5 f8 11 44 24 20    	vmovups %xmm0,0x20(%rsp)
  4010be:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4010c3:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4010c8:	89 54 24 18          	mov    %edx,0x18(%rsp)
  4010cc:	89 4c 24 1c          	mov    %ecx,0x1c(%rsp)
  4010d0:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  4010d5:	e8 c6 24 00 00       	call   4035a0 <runtime::print_caller_location>
  4010da:	bf 98 60 40 00       	mov    $0x406098,%edi
  4010df:	be 17 00 00 00       	mov    $0x17,%esi
  4010e4:	e8 87 17 00 00       	call   402870 <runtime::print_string>
  4010e9:	4c 89 ff             	mov    %r15,%rdi
  4010ec:	e8 9f 1c 00 00       	call   402d90 <runtime::print_i64>
  4010f1:	bf b0 60 40 00       	mov    $0x4060b0,%edi
  4010f6:	be 01 00 00 00       	mov    $0x1,%esi
  4010fb:	e8 70 17 00 00       	call   402870 <runtime::print_string>
  401100:	4c 89 f7             	mov    %r14,%rdi
  401103:	e8 88 1c 00 00       	call   402d90 <runtime::print_i64>
  401108:	bf b2 60 40 00       	mov    $0x4060b2,%edi
  40110d:	be 15 00 00 00       	mov    $0x15,%esi
  401112:	e8 59 17 00 00       	call   402870 <runtime::print_string>
  401117:	48 89 df             	mov    %rbx,%rdi
  40111a:	e8 71 1c 00 00       	call   402d90 <runtime::print_i64>
  40111f:	bf 0a 00 00 00       	mov    $0xa,%edi
  401124:	e8 67 1a 00 00       	call   402b90 <runtime::print_byte>
  401129:	0f 0b                	ud2
  40112b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401130 <runtime::multi_pointer_slice_handle_error>:
  401130:	41 56                	push   %r14
  401132:	53                   	push   %rbx
  401133:	48 83 ec 28          	sub    $0x28,%rsp
  401137:	4c 89 cb             	mov    %r9,%rbx
  40113a:	4d 89 c6             	mov    %r8,%r14
  40113d:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401141:	c5 f8 11 44 24 18    	vmovups %xmm0,0x18(%rsp)
  401147:	48 89 3c 24          	mov    %rdi,(%rsp)
  40114b:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  401150:	89 54 24 10          	mov    %edx,0x10(%rsp)
  401154:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  401158:	48 89 e7             	mov    %rsp,%rdi
  40115b:	e8 40 24 00 00       	call   4035a0 <runtime::print_caller_location>
  401160:	bf 98 60 40 00       	mov    $0x406098,%edi
  401165:	be 17 00 00 00       	mov    $0x17,%esi
  40116a:	e8 01 17 00 00       	call   402870 <runtime::print_string>
  40116f:	4c 89 f7             	mov    %r14,%rdi
  401172:	e8 19 1c 00 00       	call   402d90 <runtime::print_i64>
  401177:	bf b0 60 40 00       	mov    $0x4060b0,%edi
  40117c:	be 01 00 00 00       	mov    $0x1,%esi
  401181:	e8 ea 16 00 00       	call   402870 <runtime::print_string>
  401186:	48 89 df             	mov    %rbx,%rdi
  401189:	e8 02 1c 00 00       	call   402d90 <runtime::print_i64>
  40118e:	bf 0a 00 00 00       	mov    $0xa,%edi
  401193:	e8 f8 19 00 00       	call   402b90 <runtime::print_byte>
  401198:	0f 0b                	ud2
  40119a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004011a0 <runtime::default_assertion_failure_proc>:
  4011a0:	50                   	push   %rax
  4011a1:	e8 0a 00 00 00       	call   4011b0 <runtime::default_assertion_contextless_failure_proc>
  4011a6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4011ad:	00 00 00 

00000000004011b0 <runtime::default_assertion_contextless_failure_proc>:
  4011b0:	41 57                	push   %r15
  4011b2:	41 56                	push   %r14
  4011b4:	41 54                	push   %r12
  4011b6:	53                   	push   %rbx
  4011b7:	50                   	push   %rax
  4011b8:	48 89 cb             	mov    %rcx,%rbx
  4011bb:	49 89 d6             	mov    %rdx,%r14
  4011be:	49 89 f7             	mov    %rsi,%r15
  4011c1:	49 89 fc             	mov    %rdi,%r12
  4011c4:	4c 89 c7             	mov    %r8,%rdi
  4011c7:	e8 d4 23 00 00       	call   4035a0 <runtime::print_caller_location>
  4011cc:	bf a8 65 40 00       	mov    $0x4065a8,%edi
  4011d1:	be 01 00 00 00       	mov    $0x1,%esi
  4011d6:	e8 95 16 00 00       	call   402870 <runtime::print_string>
  4011db:	4c 89 e7             	mov    %r12,%rdi
  4011de:	4c 89 fe             	mov    %r15,%rsi
  4011e1:	e8 8a 16 00 00       	call   402870 <runtime::print_string>
  4011e6:	48 85 db             	test   %rbx,%rbx
  4011e9:	7e 1a                	jle    401205 <runtime::default_assertion_contextless_failure_proc+0x55>
  4011eb:	bf d8 64 40 00       	mov    $0x4064d8,%edi
  4011f0:	be 02 00 00 00       	mov    $0x2,%esi
  4011f5:	e8 76 16 00 00       	call   402870 <runtime::print_string>
  4011fa:	4c 89 f7             	mov    %r14,%rdi
  4011fd:	48 89 de             	mov    %rbx,%rsi
  401200:	e8 6b 16 00 00       	call   402870 <runtime::print_string>
  401205:	bf 0a 00 00 00       	mov    $0xa,%edi
  40120a:	e8 81 19 00 00       	call   402b90 <runtime::print_byte>
  40120f:	0f 0b                	ud2
  401211:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  401218:	0f 1f 84 00 00 00 00 
  40121f:	00 

0000000000401220 <runtime::panic_contextless>:
  401220:	50                   	push   %rax
  401221:	49 89 d0             	mov    %rdx,%r8
  401224:	48 89 f1             	mov    %rsi,%rcx
  401227:	48 89 fa             	mov    %rdi,%rdx
  40122a:	bf aa 65 40 00       	mov    $0x4065aa,%edi
  40122f:	be 05 00 00 00       	mov    $0x5,%esi
  401234:	e8 77 ff ff ff       	call   4011b0 <runtime::default_assertion_contextless_failure_proc>
  401239:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401240 <runtime::bounds_check_error.handle_error-0>:
  401240:	41 56                	push   %r14
  401242:	53                   	push   %rbx
  401243:	48 83 ec 28          	sub    $0x28,%rsp
  401247:	4c 89 cb             	mov    %r9,%rbx
  40124a:	4d 89 c6             	mov    %r8,%r14
  40124d:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401251:	c5 f8 11 44 24 18    	vmovups %xmm0,0x18(%rsp)
  401257:	48 89 3c 24          	mov    %rdi,(%rsp)
  40125b:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  401260:	89 54 24 10          	mov    %edx,0x10(%rsp)
  401264:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  401268:	48 89 e7             	mov    %rsp,%rdi
  40126b:	e8 30 23 00 00       	call   4035a0 <runtime::print_caller_location>
  401270:	bf df 65 40 00       	mov    $0x4065df,%edi
  401275:	be 07 00 00 00       	mov    $0x7,%esi
  40127a:	e8 f1 15 00 00       	call   402870 <runtime::print_string>
  40127f:	4c 89 f7             	mov    %r14,%rdi
  401282:	e8 09 1b 00 00       	call   402d90 <runtime::print_i64>
  401287:	bf b2 60 40 00       	mov    $0x4060b2,%edi
  40128c:	be 15 00 00 00       	mov    $0x15,%esi
  401291:	e8 da 15 00 00       	call   402870 <runtime::print_string>
  401296:	48 89 df             	mov    %rbx,%rdi
  401299:	e8 f2 1a 00 00       	call   402d90 <runtime::print_i64>
  40129e:	bf 0a 00 00 00       	mov    $0xa,%edi
  4012a3:	e8 e8 18 00 00       	call   402b90 <runtime::print_byte>
  4012a8:	0f 0b                	ud2
  4012aa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004012b0 <runtime::make_slice_error_loc.handle_error-0>:
  4012b0:	53                   	push   %rbx
  4012b1:	48 89 f3             	mov    %rsi,%rbx
  4012b4:	e8 e7 22 00 00       	call   4035a0 <runtime::print_caller_location>
  4012b9:	bf e7 65 40 00       	mov    $0x4065e7,%edi
  4012be:	be 20 00 00 00       	mov    $0x20,%esi
  4012c3:	e8 a8 15 00 00       	call   402870 <runtime::print_string>
  4012c8:	48 89 df             	mov    %rbx,%rdi
  4012cb:	e8 c0 1a 00 00       	call   402d90 <runtime::print_i64>
  4012d0:	bf 0a 00 00 00       	mov    $0xa,%edi
  4012d5:	e8 b6 18 00 00       	call   402b90 <runtime::print_byte>
  4012da:	0f 0b                	ud2
  4012dc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004012e0 <_start>:
  4012e0:	f3 0f 1e fa          	endbr64
  4012e4:	31 ed                	xor    %ebp,%ebp
  4012e6:	49 89 d1             	mov    %rdx,%r9
  4012e9:	5e                   	pop    %rsi
  4012ea:	48 89 e2             	mov    %rsp,%rdx
  4012ed:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  4012f1:	50                   	push   %rax
  4012f2:	54                   	push   %rsp
  4012f3:	45 31 c0             	xor    %r8d,%r8d
  4012f6:	31 c9                	xor    %ecx,%ecx
  4012f8:	48 c7 c7 70 27 40 00 	mov    $0x402770,%rdi
  4012ff:	ff 15 c3 6c 00 00    	call   *0x6cc3(%rip)        # 407fc8 <__libc_start_main@GLIBC_2.34>
  401305:	f4                   	hlt
  401306:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40130d:	00 00 00 

0000000000401310 <_dl_relocate_static_pie>:
  401310:	f3 0f 1e fa          	endbr64
  401314:	c3                   	ret
  401315:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40131c:	00 00 00 
  40131f:	90                   	nop

0000000000401320 <deregister_tm_clones>:
  401320:	b8 48 80 40 00       	mov    $0x408048,%eax
  401325:	48 3d 48 80 40 00    	cmp    $0x408048,%rax
  40132b:	74 13                	je     401340 <deregister_tm_clones+0x20>
  40132d:	48 8b 05 9c 6c 00 00 	mov    0x6c9c(%rip),%rax        # 407fd0 <_ITM_deregisterTMCloneTable>
  401334:	48 85 c0             	test   %rax,%rax
  401337:	74 07                	je     401340 <deregister_tm_clones+0x20>
  401339:	bf 48 80 40 00       	mov    $0x408048,%edi
  40133e:	ff e0                	jmp    *%rax
  401340:	c3                   	ret
  401341:	0f 1f 40 00          	nopl   0x0(%rax)
  401345:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40134c:	00 00 00 00 

0000000000401350 <register_tm_clones>:
  401350:	be 48 80 40 00       	mov    $0x408048,%esi
  401355:	48 81 ee 48 80 40 00 	sub    $0x408048,%rsi
  40135c:	48 89 f0             	mov    %rsi,%rax
  40135f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401363:	48 c1 f8 03          	sar    $0x3,%rax
  401367:	48 01 c6             	add    %rax,%rsi
  40136a:	48 d1 fe             	sar    $1,%rsi
  40136d:	74 19                	je     401388 <register_tm_clones+0x38>
  40136f:	48 8b 05 6a 6c 00 00 	mov    0x6c6a(%rip),%rax        # 407fe0 <_ITM_registerTMCloneTable>
  401376:	48 85 c0             	test   %rax,%rax
  401379:	74 0d                	je     401388 <register_tm_clones+0x38>
  40137b:	bf 48 80 40 00       	mov    $0x408048,%edi
  401380:	ff e0                	jmp    *%rax
  401382:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401388:	c3                   	ret
  401389:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401390 <__do_global_dtors_aux>:
  401390:	f3 0f 1e fa          	endbr64
  401394:	80 3d ad 6c 00 00 00 	cmpb   $0x0,0x6cad(%rip)        # 408048 <__TMC_END__>
  40139b:	75 13                	jne    4013b0 <__do_global_dtors_aux+0x20>
  40139d:	55                   	push   %rbp
  40139e:	48 89 e5             	mov    %rsp,%rbp
  4013a1:	e8 7a ff ff ff       	call   401320 <deregister_tm_clones>
  4013a6:	c6 05 9b 6c 00 00 01 	movb   $0x1,0x6c9b(%rip)        # 408048 <__TMC_END__>
  4013ad:	5d                   	pop    %rbp
  4013ae:	c3                   	ret
  4013af:	90                   	nop
  4013b0:	c3                   	ret
  4013b1:	0f 1f 40 00          	nopl   0x0(%rax)
  4013b5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4013bc:	00 00 00 00 

00000000004013c0 <frame_dummy>:
  4013c0:	f3 0f 1e fa          	endbr64
  4013c4:	eb 8a                	jmp    401350 <register_tm_clones>
  4013c6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4013cd:	00 00 00 

00000000004013d0 <__$startup_runtime>:
  4013d0:	50                   	push   %rax
  4013d1:	eb 00                	jmp    4013d3 <__$startup_runtime+0x3>
  4013d3:	e8 08 00 00 00       	call   4013e0 <os::[allocators.odin]::init_thread_local_cleaner>
  4013d8:	e8 b3 00 00 00       	call   401490 <os::[file_linux.odin]::_standard_stream_init>
  4013dd:	58                   	pop    %rax
  4013de:	c3                   	ret
  4013df:	90                   	nop

00000000004013e0 <os::[allocators.odin]::init_thread_local_cleaner>:
  4013e0:	48 83 3d 80 6c 00 00 	cmpq   $0x0,0x6c80(%rip)        # 408068 <runtime::[thread_management.odin]::thread_local_cleaners+0x8>
  4013e7:	00 
  4013e8:	74 60                	je     40144a <os::[allocators.odin]::init_thread_local_cleaner+0x6a>
  4013ea:	48 83 3d 86 6c 00 00 	cmpq   $0x0,0x6c86(%rip)        # 408078 <runtime::[thread_management.odin]::thread_local_cleaners+0x18>
  4013f1:	00 
  4013f2:	74 5d                	je     401451 <os::[allocators.odin]::init_thread_local_cleaner+0x71>
  4013f4:	48 83 3d 8c 6c 00 00 	cmpq   $0x0,0x6c8c(%rip)        # 408088 <runtime::[thread_management.odin]::thread_local_cleaners+0x28>
  4013fb:	00 
  4013fc:	74 5a                	je     401458 <os::[allocators.odin]::init_thread_local_cleaner+0x78>
  4013fe:	48 83 3d 92 6c 00 00 	cmpq   $0x0,0x6c92(%rip)        # 408098 <runtime::[thread_management.odin]::thread_local_cleaners+0x38>
  401405:	00 
  401406:	74 57                	je     40145f <os::[allocators.odin]::init_thread_local_cleaner+0x7f>
  401408:	48 83 3d 98 6c 00 00 	cmpq   $0x0,0x6c98(%rip)        # 4080a8 <runtime::[thread_management.odin]::thread_local_cleaners+0x48>
  40140f:	00 
  401410:	74 54                	je     401466 <os::[allocators.odin]::init_thread_local_cleaner+0x86>
  401412:	48 83 3d 9e 6c 00 00 	cmpq   $0x0,0x6c9e(%rip)        # 4080b8 <runtime::[thread_management.odin]::thread_local_cleaners+0x58>
  401419:	00 
  40141a:	74 51                	je     40146d <os::[allocators.odin]::init_thread_local_cleaner+0x8d>
  40141c:	48 83 3d a4 6c 00 00 	cmpq   $0x0,0x6ca4(%rip)        # 4080c8 <runtime::[thread_management.odin]::thread_local_cleaners+0x68>
  401423:	00 
  401424:	74 4e                	je     401474 <os::[allocators.odin]::init_thread_local_cleaner+0x94>
  401426:	b8 d0 80 40 00       	mov    $0x4080d0,%eax
  40142b:	48 83 3d a5 6c 00 00 	cmpq   $0x0,0x6ca5(%rip)        # 4080d8 <runtime::[thread_management.odin]::thread_local_cleaners+0x78>
  401432:	00 
  401433:	74 44                	je     401479 <os::[allocators.odin]::init_thread_local_cleaner+0x99>
  401435:	50                   	push   %rax
  401436:	bf c8 5d 40 00       	mov    $0x405dc8,%edi
  40143b:	be 37 00 00 00       	mov    $0x37,%esi
  401440:	ba 50 5e 40 00       	mov    $0x405e50,%edx
  401445:	e8 d6 fd ff ff       	call   401220 <runtime::panic_contextless>
  40144a:	b8 60 80 40 00       	mov    $0x408060,%eax
  40144f:	eb 28                	jmp    401479 <os::[allocators.odin]::init_thread_local_cleaner+0x99>
  401451:	b8 70 80 40 00       	mov    $0x408070,%eax
  401456:	eb 21                	jmp    401479 <os::[allocators.odin]::init_thread_local_cleaner+0x99>
  401458:	b8 80 80 40 00       	mov    $0x408080,%eax
  40145d:	eb 1a                	jmp    401479 <os::[allocators.odin]::init_thread_local_cleaner+0x99>
  40145f:	b8 90 80 40 00       	mov    $0x408090,%eax
  401464:	eb 13                	jmp    401479 <os::[allocators.odin]::init_thread_local_cleaner+0x99>
  401466:	b8 a0 80 40 00       	mov    $0x4080a0,%eax
  40146b:	eb 0c                	jmp    401479 <os::[allocators.odin]::init_thread_local_cleaner+0x99>
  40146d:	b8 b0 80 40 00       	mov    $0x4080b0,%eax
  401472:	eb 05                	jmp    401479 <os::[allocators.odin]::init_thread_local_cleaner+0x99>
  401474:	b8 c0 80 40 00       	mov    $0x4080c0,%eax
  401479:	48 c7 00 c0 15 40 00 	movq   $0x4015c0,(%rax)
  401480:	48 c7 40 08 02 00 00 	movq   $0x2,0x8(%rax)
  401487:	00 
  401488:	c3                   	ret
  401489:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401490 <os::[file_linux.odin]::_standard_stream_init>:
  401490:	48 c7 05 45 6c 00 00 	movq   $0x4080e0,0x6c45(%rip)        # 4080e0 <os::[file_linux.odin]::_standard_stream_init-.files-41273>
  401497:	e0 80 40 00 
  40149b:	c7 05 63 6c 00 00 00 	movl   $0x0,0x6c63(%rip)        # 408108 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x28>
  4014a2:	00 00 00 
  4014a5:	48 c7 05 60 6c 00 00 	movq   $0x4018a0,0x6c60(%rip)        # 408110 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x30>
  4014ac:	a0 18 40 00 
  4014b0:	48 c7 05 5d 6c 00 00 	movq   $0x0,0x6c5d(%rip)        # 408118 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x38>
  4014b7:	00 00 00 00 
  4014bb:	48 c7 05 32 6c 00 00 	movq   $0x406068,0x6c32(%rip)        # 4080f8 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x18>
  4014c2:	68 60 40 00 
  4014c6:	48 c7 05 2f 6c 00 00 	movq   $0xf,0x6c2f(%rip)        # 408100 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x20>
  4014cd:	0f 00 00 00 
  4014d1:	48 c7 05 0c 6c 00 00 	movq   $0x4035f0,0x6c0c(%rip)        # 4080e8 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x8>
  4014d8:	f0 35 40 00 
  4014dc:	48 c7 05 09 6c 00 00 	movq   $0x4080e0,0x6c09(%rip)        # 4080f0 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x10>
  4014e3:	e0 80 40 00 
  4014e7:	48 c7 05 56 6c 00 00 	movq   $0x408148,0x6c56(%rip)        # 408148 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x68>
  4014ee:	48 81 40 00 
  4014f2:	c7 05 74 6c 00 00 01 	movl   $0x1,0x6c74(%rip)        # 408170 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x90>
  4014f9:	00 00 00 
  4014fc:	48 c7 05 71 6c 00 00 	movq   $0x4018a0,0x6c71(%rip)        # 408178 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x98>
  401503:	a0 18 40 00 
  401507:	48 c7 05 6e 6c 00 00 	movq   $0x0,0x6c6e(%rip)        # 408180 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0xa0>
  40150e:	00 00 00 00 
  401512:	48 c7 05 43 6c 00 00 	movq   $0x406078,0x6c43(%rip)        # 408160 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x80>
  401519:	78 60 40 00 
  40151d:	48 c7 05 40 6c 00 00 	movq   $0xf,0x6c40(%rip)        # 408168 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x88>
  401524:	0f 00 00 00 
  401528:	48 c7 05 1d 6c 00 00 	movq   $0x4035f0,0x6c1d(%rip)        # 408150 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x70>
  40152f:	f0 35 40 00 
  401533:	48 c7 05 1a 6c 00 00 	movq   $0x408148,0x6c1a(%rip)        # 408158 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x78>
  40153a:	48 81 40 00 
  40153e:	48 c7 05 67 6c 00 00 	movq   $0x4081b0,0x6c67(%rip)        # 4081b0 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0xd0>
  401545:	b0 81 40 00 
  401549:	c7 05 85 6c 00 00 02 	movl   $0x2,0x6c85(%rip)        # 4081d8 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0xf8>
  401550:	00 00 00 
  401553:	48 c7 05 82 6c 00 00 	movq   $0x4018a0,0x6c82(%rip)        # 4081e0 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x100>
  40155a:	a0 18 40 00 
  40155e:	48 c7 05 7f 6c 00 00 	movq   $0x0,0x6c7f(%rip)        # 4081e8 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0x108>
  401565:	00 00 00 00 
  401569:	48 c7 05 54 6c 00 00 	movq   $0x406088,0x6c54(%rip)        # 4081c8 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0xe8>
  401570:	88 60 40 00 
  401574:	48 c7 05 51 6c 00 00 	movq   $0xf,0x6c51(%rip)        # 4081d0 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0xf0>
  40157b:	0f 00 00 00 
  40157f:	48 c7 05 2e 6c 00 00 	movq   $0x4035f0,0x6c2e(%rip)        # 4081b8 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0xd8>
  401586:	f0 35 40 00 
  40158a:	48 c7 05 2b 6c 00 00 	movq   $0x4081b0,0x6c2b(%rip)        # 4081c0 <os::[file_linux.odin]::_standard_stream_init-.files-41273+0xe0>
  401591:	b0 81 40 00 
  401595:	c3                   	ret
  401596:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40159d:	00 00 00 

00000000004015a0 <__$cleanup_runtime>:
  4015a0:	50                   	push   %rax
  4015a1:	eb 00                	jmp    4015a3 <__$cleanup_runtime+0x3>
  4015a3:	e8 18 00 00 00       	call   4015c0 <os::[allocators.odin]::temp_allocator_fini>
  4015a8:	e8 f3 01 00 00       	call   4017a0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  4015ad:	58                   	pop    %rax
  4015ae:	c3                   	ret
  4015af:	90                   	nop

00000000004015b0 <os::[process.odin]::delete_args>:
  4015b0:	c3                   	ret
  4015b1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4015b8:	0f 1f 84 00 00 00 00 
  4015bf:	00 

00000000004015c0 <os::[allocators.odin]::temp_allocator_fini>:
  4015c0:	41 56                	push   %r14
  4015c2:	53                   	push   %rbx
  4015c3:	48 83 ec 78          	sub    $0x78,%rsp
  4015c7:	4c 8d 74 24 38       	lea    0x38(%rsp),%r14
  4015cc:	64 48 8b 3c 25 90 fb 	mov    %fs:0xfffffffffffffb90,%rdi
  4015d3:	ff ff 
  4015d5:	48 85 ff             	test   %rdi,%rdi
  4015d8:	0f 84 b3 00 00 00    	je     401691 <os::[allocators.odin]::temp_allocator_fini+0xd1>
  4015de:	48 8d 5c 24 08       	lea    0x8(%rsp),%rbx
  4015e3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4015ea:	84 00 00 00 00 00 
  4015f0:	48 8b 07             	mov    (%rdi),%rax
  4015f3:	64 48 89 04 25 90 fb 	mov    %rax,%fs:0xfffffffffffffb90
  4015fa:	ff ff 
  4015fc:	48 8b 47 28          	mov    0x28(%rdi),%rax
  401600:	64 48 29 04 25 a0 fb 	sub    %rax,%fs:0xfffffffffffffba0
  401607:	ff ff 
  401609:	48 8b 77 08          	mov    0x8(%rdi),%rsi
  40160d:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  401611:	48 c7 44 24 18 10 21 	movq   $0x402110,0x18(%rsp)
  401618:	40 00 
  40161a:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  401621:	00 00 
  401623:	48 8d 80 48 fb ff ff 	lea    -0x4b8(%rax),%rax
  40162a:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40162f:	48 c7 44 24 28 a0 11 	movq   $0x4011a0,0x28(%rsp)
  401636:	40 00 
  401638:	48 c7 44 24 30 20 43 	movq   $0x404320,0x30(%rsp)
  40163f:	40 00 
  401641:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401645:	c4 c1 78 11 06       	vmovups %xmm0,(%r14)
  40164a:	49 c7 46 10 00 00 00 	movq   $0x0,0x10(%r14)
  401651:	00 
  401652:	48 c7 44 24 50 10 24 	movq   $0x402410,0x50(%rsp)
  401659:	40 00 
  40165b:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40165f:	c4 c1 7c 11 46 20    	vmovups %ymm0,0x20(%r14)
  401665:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40166a:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40166f:	b9 a0 5d 40 00       	mov    $0x405da0,%ecx
  401674:	49 89 d8             	mov    %rbx,%r8
  401677:	c5 f8 77             	vzeroupper
  40167a:	e8 71 1e 00 00       	call   4034f0 <runtime::mem_free>
  40167f:	64 48 8b 3c 25 90 fb 	mov    %fs:0xfffffffffffffb90,%rdi
  401686:	ff ff 
  401688:	48 85 ff             	test   %rdi,%rdi
  40168b:	0f 85 5f ff ff ff    	jne    4015f0 <os::[allocators.odin]::temp_allocator_fini+0x30>
  401691:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401695:	64 c5 f8 11 04 25 98 	vmovups %xmm0,%fs:0xfffffffffffffb98
  40169c:	fb ff ff 
  40169f:	64 48 8b 3c 25 c8 fb 	mov    %fs:0xfffffffffffffbc8,%rdi
  4016a6:	ff ff 
  4016a8:	48 85 ff             	test   %rdi,%rdi
  4016ab:	0f 84 b0 00 00 00    	je     401761 <os::[allocators.odin]::temp_allocator_fini+0x1a1>
  4016b1:	48 8d 5c 24 08       	lea    0x8(%rsp),%rbx
  4016b6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4016bd:	00 00 00 
  4016c0:	48 8b 07             	mov    (%rdi),%rax
  4016c3:	64 48 89 04 25 c8 fb 	mov    %rax,%fs:0xfffffffffffffbc8
  4016ca:	ff ff 
  4016cc:	48 8b 47 28          	mov    0x28(%rdi),%rax
  4016d0:	64 48 29 04 25 d8 fb 	sub    %rax,%fs:0xfffffffffffffbd8
  4016d7:	ff ff 
  4016d9:	48 8b 77 08          	mov    0x8(%rdi),%rsi
  4016dd:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  4016e1:	48 c7 44 24 18 10 21 	movq   $0x402110,0x18(%rsp)
  4016e8:	40 00 
  4016ea:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  4016f1:	00 00 
  4016f3:	48 8d 80 48 fb ff ff 	lea    -0x4b8(%rax),%rax
  4016fa:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4016ff:	48 c7 44 24 28 a0 11 	movq   $0x4011a0,0x28(%rsp)
  401706:	40 00 
  401708:	48 c7 44 24 30 20 43 	movq   $0x404320,0x30(%rsp)
  40170f:	40 00 
  401711:	c4 c1 78 11 06       	vmovups %xmm0,(%r14)
  401716:	49 c7 46 10 00 00 00 	movq   $0x0,0x10(%r14)
  40171d:	00 
  40171e:	48 c7 44 24 50 10 24 	movq   $0x402410,0x50(%rsp)
  401725:	40 00 
  401727:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40172b:	c4 c1 7c 11 46 20    	vmovups %ymm0,0x20(%r14)
  401731:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  401736:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40173b:	b9 a0 5d 40 00       	mov    $0x405da0,%ecx
  401740:	49 89 d8             	mov    %rbx,%r8
  401743:	c5 f8 77             	vzeroupper
  401746:	e8 a5 1d 00 00       	call   4034f0 <runtime::mem_free>
  40174b:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40174f:	64 48 8b 3c 25 c8 fb 	mov    %fs:0xfffffffffffffbc8,%rdi
  401756:	ff ff 
  401758:	48 85 ff             	test   %rdi,%rdi
  40175b:	0f 85 5f ff ff ff    	jne    4016c0 <os::[allocators.odin]::temp_allocator_fini+0x100>
  401761:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401765:	64 c5 fc 11 04 25 d0 	vmovups %ymm0,%fs:0xfffffffffffffbd0
  40176c:	fb ff ff 
  40176f:	64 c5 fc 11 04 25 c0 	vmovups %ymm0,%fs:0xfffffffffffffbc0
  401776:	fb ff ff 
  401779:	64 c5 fc 11 04 25 a0 	vmovups %ymm0,%fs:0xfffffffffffffba0
  401780:	fb ff ff 
  401783:	64 c5 fc 11 04 25 80 	vmovups %ymm0,%fs:0xfffffffffffffb80
  40178a:	fb ff ff 
  40178d:	48 83 c4 78          	add    $0x78,%rsp
  401791:	5b                   	pop    %rbx
  401792:	41 5e                	pop    %r14
  401794:	c5 f8 77             	vzeroupper
  401797:	c3                   	ret
  401798:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40179f:	00 

00000000004017a0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  4017a0:	64 48 8b 3c 25 58 fb 	mov    %fs:0xfffffffffffffb58,%rdi
  4017a7:	ff ff 
  4017a9:	48 85 ff             	test   %rdi,%rdi
  4017ac:	0f 84 c6 00 00 00    	je     401878 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0xd8>
  4017b2:	41 56                	push   %r14
  4017b4:	53                   	push   %rbx
  4017b5:	48 83 ec 78          	sub    $0x78,%rsp
  4017b9:	4c 8d 74 24 38       	lea    0x38(%rsp),%r14
  4017be:	48 8d 5c 24 08       	lea    0x8(%rsp),%rbx
  4017c3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4017ca:	84 00 00 00 00 00 
  4017d0:	48 8b 07             	mov    (%rdi),%rax
  4017d3:	64 48 89 04 25 58 fb 	mov    %rax,%fs:0xfffffffffffffb58
  4017da:	ff ff 
  4017dc:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  4017e3:	00 00 
  4017e5:	48 8d 80 48 fb ff ff 	lea    -0x4b8(%rax),%rax
  4017ec:	48 8b 4f 28          	mov    0x28(%rdi),%rcx
  4017f0:	64 48 29 0c 25 68 fb 	sub    %rcx,%fs:0xfffffffffffffb68
  4017f7:	ff ff 
  4017f9:	48 8b 77 08          	mov    0x8(%rdi),%rsi
  4017fd:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  401801:	48 c7 44 24 18 10 21 	movq   $0x402110,0x18(%rsp)
  401808:	40 00 
  40180a:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40180f:	48 c7 44 24 28 a0 11 	movq   $0x4011a0,0x28(%rsp)
  401816:	40 00 
  401818:	48 c7 44 24 30 20 43 	movq   $0x404320,0x30(%rsp)
  40181f:	40 00 
  401821:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401825:	c4 c1 78 11 06       	vmovups %xmm0,(%r14)
  40182a:	49 c7 46 10 00 00 00 	movq   $0x0,0x10(%r14)
  401831:	00 
  401832:	48 c7 44 24 50 10 24 	movq   $0x402410,0x50(%rsp)
  401839:	40 00 
  40183b:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40183f:	c4 c1 7c 11 46 20    	vmovups %ymm0,0x20(%r14)
  401845:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40184a:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40184f:	b9 50 61 40 00       	mov    $0x406150,%ecx
  401854:	49 89 d8             	mov    %rbx,%r8
  401857:	c5 f8 77             	vzeroupper
  40185a:	e8 91 1c 00 00       	call   4034f0 <runtime::mem_free>
  40185f:	64 48 8b 3c 25 58 fb 	mov    %fs:0xfffffffffffffb58,%rdi
  401866:	ff ff 
  401868:	48 85 ff             	test   %rdi,%rdi
  40186b:	0f 85 5f ff ff ff    	jne    4017d0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x30>
  401871:	48 83 c4 78          	add    $0x78,%rsp
  401875:	5b                   	pop    %rbx
  401876:	41 5e                	pop    %r14
  401878:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40187c:	64 c5 fc 11 04 25 60 	vmovups %ymm0,%fs:0xfffffffffffffb60
  401883:	fb ff ff 
  401886:	64 c5 fc 11 04 25 48 	vmovups %ymm0,%fs:0xfffffffffffffb48
  40188d:	fb ff ff 
  401890:	c5 f8 77             	vzeroupper
  401893:	c3                   	ret
  401894:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40189b:	00 00 00 00 00 

00000000004018a0 <runtime::nil_allocator_proc>:
  4018a0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4018a5:	40 80 fe 07          	cmp    $0x7,%sil
  4018a9:	77 30                	ja     4018db <runtime::nil_allocator_proc+0x3b>
  4018ab:	40 0f b6 c6          	movzbl %sil,%eax
  4018af:	ff 24 c5 30 50 40 00 	jmp    *0x405030(,%rax,8)
  4018b6:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4018ba:	c5 f8 11 01          	vmovups %xmm0,(%rcx)
  4018be:	b0 04                	mov    $0x4,%al
  4018c0:	c3                   	ret
  4018c1:	48 85 d2             	test   %rdx,%rdx
  4018c4:	0f 95 c0             	setne  %al
  4018c7:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4018cb:	c5 f8 11 01          	vmovups %xmm0,(%rcx)
  4018cf:	c3                   	ret
  4018d0:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4018d4:	c5 f8 11 01          	vmovups %xmm0,(%rcx)
  4018d8:	b0 01                	mov    $0x1,%al
  4018da:	c3                   	ret
  4018db:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4018df:	c5 f8 11 01          	vmovups %xmm0,(%rcx)
  4018e3:	31 c0                	xor    %eax,%eax
  4018e5:	c3                   	ret
  4018e6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4018ed:	00 00 00 

00000000004018f0 <runtime::udivmod128>:
  4018f0:	41 57                	push   %r15
  4018f2:	41 56                	push   %r14
  4018f4:	41 55                	push   %r13
  4018f6:	41 54                	push   %r12
  4018f8:	53                   	push   %rbx
  4018f9:	48 85 f6             	test   %rsi,%rsi
  4018fc:	74 57                	je     401955 <runtime::udivmod128+0x65>
  4018fe:	48 85 d2             	test   %rdx,%rdx
  401901:	74 74                	je     401977 <runtime::udivmod128+0x87>
  401903:	48 85 c9             	test   %rcx,%rcx
  401906:	0f 84 aa 00 00 00    	je     4019b6 <runtime::udivmod128+0xc6>
  40190c:	f3 4c 0f bd d1       	lzcnt  %rcx,%r10
  401911:	f3 48 0f bd c6       	lzcnt  %rsi,%rax
  401916:	41 29 c2             	sub    %eax,%r10d
  401919:	41 83 fa 40          	cmp    $0x40,%r10d
  40191d:	0f 83 68 01 00 00    	jae    401a8b <runtime::udivmod128+0x19b>
  401923:	41 8d 42 01          	lea    0x1(%r10),%eax
  401927:	83 f8 40             	cmp    $0x40,%eax
  40192a:	0f 84 38 01 00 00    	je     401a68 <runtime::udivmod128+0x178>
  401930:	c4 62 fb f7 ce       	shrx   %rax,%rsi,%r9
  401935:	41 b3 3f             	mov    $0x3f,%r11b
  401938:	45 28 d3             	sub    %r10b,%r11b
  40193b:	c4 62 a1 f7 d6       	shlx   %r11,%rsi,%r10
  401940:	c4 e2 fb f7 f7       	shrx   %rax,%rdi,%rsi
  401945:	4c 09 d6             	or     %r10,%rsi
  401948:	c4 e2 a1 f7 ff       	shlx   %r11,%rdi,%rdi
  40194d:	45 31 d2             	xor    %r10d,%r10d
  401950:	e9 d2 01 00 00       	jmp    401b27 <runtime::udivmod128+0x237>
  401955:	48 85 c9             	test   %rcx,%rcx
  401958:	0f 84 a9 00 00 00    	je     401a07 <runtime::udivmod128+0x117>
  40195e:	4d 85 c0             	test   %r8,%r8
  401961:	0f 84 30 01 00 00    	je     401a97 <runtime::udivmod128+0x1a7>
  401967:	49 89 38             	mov    %rdi,(%r8)
  40196a:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  401971:	00 
  401972:	e9 20 01 00 00       	jmp    401a97 <runtime::udivmod128+0x1a7>
  401977:	48 85 c9             	test   %rcx,%rcx
  40197a:	0f 84 5c 03 00 00    	je     401cdc <runtime::udivmod128+0x3ec>
  401980:	48 85 ff             	test   %rdi,%rdi
  401983:	0f 84 a8 00 00 00    	je     401a31 <runtime::udivmod128+0x141>
  401989:	48 8d 41 ff          	lea    -0x1(%rcx),%rax
  40198d:	48 85 c1             	test   %rax,%rcx
  401990:	0f 85 e2 00 00 00    	jne    401a78 <runtime::udivmod128+0x188>
  401996:	4d 85 c0             	test   %r8,%r8
  401999:	74 0a                	je     4019a5 <runtime::udivmod128+0xb5>
  40199b:	48 21 f0             	and    %rsi,%rax
  40199e:	49 89 38             	mov    %rdi,(%r8)
  4019a1:	49 89 40 08          	mov    %rax,0x8(%r8)
  4019a5:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  4019aa:	c4 e2 fb f7 fe       	shrx   %rax,%rsi,%rdi
  4019af:	31 f6                	xor    %esi,%esi
  4019b1:	e9 16 03 00 00       	jmp    401ccc <runtime::udivmod128+0x3dc>
  4019b6:	48 8d 42 ff          	lea    -0x1(%rdx),%rax
  4019ba:	48 85 c2             	test   %rax,%rdx
  4019bd:	0f 85 90 00 00 00    	jne    401a53 <runtime::udivmod128+0x163>
  4019c3:	4d 85 c0             	test   %r8,%r8
  4019c6:	74 0e                	je     4019d6 <runtime::udivmod128+0xe6>
  4019c8:	48 21 f8             	and    %rdi,%rax
  4019cb:	49 89 00             	mov    %rax,(%r8)
  4019ce:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  4019d5:	00 
  4019d6:	48 83 fa 01          	cmp    $0x1,%rdx
  4019da:	0f 84 ec 02 00 00    	je     401ccc <runtime::udivmod128+0x3dc>
  4019e0:	f3 48 0f bc c2       	tzcnt  %rdx,%rax
  4019e5:	89 c1                	mov    %eax,%ecx
  4019e7:	f6 d9                	neg    %cl
  4019e9:	c4 e2 f1 f7 ce       	shlx   %rcx,%rsi,%rcx
  4019ee:	c4 e2 fb f7 f6       	shrx   %rax,%rsi,%rsi
  4019f3:	48 85 c0             	test   %rax,%rax
  4019f6:	48 0f 44 c8          	cmove  %rax,%rcx
  4019fa:	c4 e2 fb f7 ff       	shrx   %rax,%rdi,%rdi
  4019ff:	48 09 cf             	or     %rcx,%rdi
  401a02:	e9 c5 02 00 00       	jmp    401ccc <runtime::udivmod128+0x3dc>
  401a07:	4d 85 c0             	test   %r8,%r8
  401a0a:	0f 84 90 00 00 00    	je     401aa0 <runtime::udivmod128+0x1b0>
  401a10:	48 85 d2             	test   %rdx,%rdx
  401a13:	0f 84 c3 02 00 00    	je     401cdc <runtime::udivmod128+0x3ec>
  401a19:	48 89 d1             	mov    %rdx,%rcx
  401a1c:	48 89 f8             	mov    %rdi,%rax
  401a1f:	31 d2                	xor    %edx,%edx
  401a21:	48 f7 f1             	div    %rcx
  401a24:	49 89 10             	mov    %rdx,(%r8)
  401a27:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  401a2e:	00 
  401a2f:	eb 7b                	jmp    401aac <runtime::udivmod128+0x1bc>
  401a31:	48 89 f0             	mov    %rsi,%rax
  401a34:	31 d2                	xor    %edx,%edx
  401a36:	48 f7 f1             	div    %rcx
  401a39:	48 89 c7             	mov    %rax,%rdi
  401a3c:	4d 85 c0             	test   %r8,%r8
  401a3f:	74 0b                	je     401a4c <runtime::udivmod128+0x15c>
  401a41:	49 89 50 08          	mov    %rdx,0x8(%r8)
  401a45:	49 c7 00 00 00 00 00 	movq   $0x0,(%r8)
  401a4c:	31 f6                	xor    %esi,%esi
  401a4e:	e9 79 02 00 00       	jmp    401ccc <runtime::udivmod128+0x3dc>
  401a53:	f3 48 0f bd c2       	lzcnt  %rdx,%rax
  401a58:	f3 4c 0f bd ce       	lzcnt  %rsi,%r9
  401a5d:	44 29 c8             	sub    %r9d,%eax
  401a60:	83 c0 41             	add    $0x41,%eax
  401a63:	83 f8 40             	cmp    $0x40,%eax
  401a66:	75 56                	jne    401abe <runtime::udivmod128+0x1ce>
  401a68:	b8 40 00 00 00       	mov    $0x40,%eax
  401a6d:	45 31 d2             	xor    %r10d,%r10d
  401a70:	45 31 c9             	xor    %r9d,%r9d
  401a73:	e9 af 00 00 00       	jmp    401b27 <runtime::udivmod128+0x237>
  401a78:	f3 4c 0f bd c9       	lzcnt  %rcx,%r9
  401a7d:	f3 48 0f bd c6       	lzcnt  %rsi,%rax
  401a82:	41 29 c1             	sub    %eax,%r9d
  401a85:	41 83 f9 3f          	cmp    $0x3f,%r9d
  401a89:	72 59                	jb     401ae4 <runtime::udivmod128+0x1f4>
  401a8b:	4d 85 c0             	test   %r8,%r8
  401a8e:	74 07                	je     401a97 <runtime::udivmod128+0x1a7>
  401a90:	49 89 38             	mov    %rdi,(%r8)
  401a93:	49 89 70 08          	mov    %rsi,0x8(%r8)
  401a97:	31 ff                	xor    %edi,%edi
  401a99:	31 f6                	xor    %esi,%esi
  401a9b:	e9 2c 02 00 00       	jmp    401ccc <runtime::udivmod128+0x3dc>
  401aa0:	48 89 d1             	mov    %rdx,%rcx
  401aa3:	48 85 d2             	test   %rdx,%rdx
  401aa6:	0f 84 30 02 00 00    	je     401cdc <runtime::udivmod128+0x3ec>
  401aac:	31 f6                	xor    %esi,%esi
  401aae:	48 89 f8             	mov    %rdi,%rax
  401ab1:	31 d2                	xor    %edx,%edx
  401ab3:	48 f7 f1             	div    %rcx
  401ab6:	48 89 c7             	mov    %rax,%rdi
  401ab9:	e9 0e 02 00 00       	jmp    401ccc <runtime::udivmod128+0x3dc>
  401abe:	49 89 cb             	mov    %rcx,%r11
  401ac1:	73 49                	jae    401b0c <runtime::udivmod128+0x21c>
  401ac3:	41 89 c1             	mov    %eax,%r9d
  401ac6:	48 89 fb             	mov    %rdi,%rbx
  401ac9:	89 c1                	mov    %eax,%ecx
  401acb:	48 0f ad f3          	shrd   %cl,%rsi,%rbx
  401acf:	41 f6 d9             	neg    %r9b
  401ad2:	c4 e2 b1 f7 ff       	shlx   %r9,%rdi,%rdi
  401ad7:	c4 62 fb f7 ce       	shrx   %rax,%rsi,%r9
  401adc:	45 31 d2             	xor    %r10d,%r10d
  401adf:	48 89 de             	mov    %rbx,%rsi
  401ae2:	eb 40                	jmp    401b24 <runtime::udivmod128+0x234>
  401ae4:	41 8d 41 01          	lea    0x1(%r9),%eax
  401ae8:	49 89 fb             	mov    %rdi,%r11
  401aeb:	49 89 ca             	mov    %rcx,%r10
  401aee:	89 c1                	mov    %eax,%ecx
  401af0:	49 0f ad f3          	shrd   %cl,%rsi,%r11
  401af4:	4c 89 d1             	mov    %r10,%rcx
  401af7:	41 f6 d1             	not    %r9b
  401afa:	c4 e2 b1 f7 ff       	shlx   %r9,%rdi,%rdi
  401aff:	c4 62 fb f7 ce       	shrx   %rax,%rsi,%r9
  401b04:	45 31 d2             	xor    %r10d,%r10d
  401b07:	4c 89 de             	mov    %r11,%rsi
  401b0a:	eb 1b                	jmp    401b27 <runtime::udivmod128+0x237>
  401b0c:	89 c1                	mov    %eax,%ecx
  401b0e:	f6 d9                	neg    %cl
  401b10:	c4 62 f1 f7 d7       	shlx   %rcx,%rdi,%r10
  401b15:	8d 48 c0             	lea    -0x40(%rax),%ecx
  401b18:	48 0f ad f7          	shrd   %cl,%rsi,%rdi
  401b1c:	c4 e2 fb f7 f6       	shrx   %rax,%rsi,%rsi
  401b21:	45 31 c9             	xor    %r9d,%r9d
  401b24:	4c 89 d9             	mov    %r11,%rcx
  401b27:	44 8d 58 ff          	lea    -0x1(%rax),%r11d
  401b2b:	89 c3                	mov    %eax,%ebx
  401b2d:	83 e3 03             	and    $0x3,%ebx
  401b30:	41 83 fb 03          	cmp    $0x3,%r11d
  401b34:	73 10                	jae    401b46 <runtime::udivmod128+0x256>
  401b36:	45 31 f6             	xor    %r14d,%r14d
  401b39:	85 db                	test   %ebx,%ebx
  401b3b:	0f 85 19 01 00 00    	jne    401c5a <runtime::udivmod128+0x36a>
  401b41:	e9 68 01 00 00       	jmp    401cae <runtime::udivmod128+0x3be>
  401b46:	83 e0 fc             	and    $0xfffffffc,%eax
  401b49:	45 31 db             	xor    %r11d,%r11d
  401b4c:	45 31 f6             	xor    %r14d,%r14d
  401b4f:	90                   	nop
  401b50:	49 0f a4 f1 01       	shld   $0x1,%rsi,%r9
  401b55:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  401b5a:	4c 0f a4 d7 01       	shld   $0x1,%r10,%rdi
  401b5f:	4d 01 d2             	add    %r10,%r10
  401b62:	4d 09 f2             	or     %r14,%r10
  401b65:	4d 89 ce             	mov    %r9,%r14
  401b68:	49 f7 d6             	not    %r14
  401b6b:	49 89 f7             	mov    %rsi,%r15
  401b6e:	49 f7 d7             	not    %r15
  401b71:	49 01 d7             	add    %rdx,%r15
  401b74:	49 11 ce             	adc    %rcx,%r14
  401b77:	4d 89 f7             	mov    %r14,%r15
  401b7a:	49 c1 ff 3f          	sar    $0x3f,%r15
  401b7e:	4d 89 fc             	mov    %r15,%r12
  401b81:	49 21 cc             	and    %rcx,%r12
  401b84:	49 21 d7             	and    %rdx,%r15
  401b87:	4c 29 fe             	sub    %r15,%rsi
  401b8a:	4d 19 e1             	sbb    %r12,%r9
  401b8d:	49 0f a4 f1 01       	shld   $0x1,%rsi,%r9
  401b92:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  401b97:	4c 0f a4 d7 01       	shld   $0x1,%r10,%rdi
  401b9c:	4d 0f ac d6 3f       	shrd   $0x3f,%r10,%r14
  401ba1:	4f 8d 3c 12          	lea    (%r10,%r10,1),%r15
  401ba5:	4d 89 ca             	mov    %r9,%r10
  401ba8:	49 f7 d2             	not    %r10
  401bab:	49 89 f4             	mov    %rsi,%r12
  401bae:	49 f7 d4             	not    %r12
  401bb1:	49 01 d4             	add    %rdx,%r12
  401bb4:	49 11 ca             	adc    %rcx,%r10
  401bb7:	4d 89 d4             	mov    %r10,%r12
  401bba:	49 c1 fc 3f          	sar    $0x3f,%r12
  401bbe:	4d 89 e5             	mov    %r12,%r13
  401bc1:	49 21 cd             	and    %rcx,%r13
  401bc4:	49 21 d4             	and    %rdx,%r12
  401bc7:	4c 29 e6             	sub    %r12,%rsi
  401bca:	4d 19 e9             	sbb    %r13,%r9
  401bcd:	49 0f a4 f1 01       	shld   $0x1,%rsi,%r9
  401bd2:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  401bd7:	49 0f ac ff 3f       	shrd   $0x3f,%rdi,%r15
  401bdc:	4d 0f ac f2 3f       	shrd   $0x3f,%r14,%r10
  401be1:	4c 89 cf             	mov    %r9,%rdi
  401be4:	48 f7 d7             	not    %rdi
  401be7:	49 89 f4             	mov    %rsi,%r12
  401bea:	49 f7 d4             	not    %r12
  401bed:	49 01 d4             	add    %rdx,%r12
  401bf0:	48 11 cf             	adc    %rcx,%rdi
  401bf3:	49 0f a4 fa 01       	shld   $0x1,%rdi,%r10
  401bf8:	48 c1 ff 3f          	sar    $0x3f,%rdi
  401bfc:	49 89 fc             	mov    %rdi,%r12
  401bff:	49 21 cc             	and    %rcx,%r12
  401c02:	48 21 d7             	and    %rdx,%rdi
  401c05:	48 29 fe             	sub    %rdi,%rsi
  401c08:	4d 19 e1             	sbb    %r12,%r9
  401c0b:	49 0f a4 f1 01       	shld   $0x1,%rsi,%r9
  401c10:	4c 0f a4 fe 01       	shld   $0x1,%r15,%rsi
  401c15:	4b 8d 3c 36          	lea    (%r14,%r14,1),%rdi
  401c19:	4c 0f ac ff 3f       	shrd   $0x3f,%r15,%rdi
  401c1e:	4d 89 cf             	mov    %r9,%r15
  401c21:	49 f7 d7             	not    %r15
  401c24:	49 89 f6             	mov    %rsi,%r14
  401c27:	49 f7 d6             	not    %r14
  401c2a:	49 01 d6             	add    %rdx,%r14
  401c2d:	49 11 cf             	adc    %rcx,%r15
  401c30:	4d 89 fe             	mov    %r15,%r14
  401c33:	49 c1 ee 3f          	shr    $0x3f,%r14
  401c37:	49 c1 ff 3f          	sar    $0x3f,%r15
  401c3b:	4d 89 fc             	mov    %r15,%r12
  401c3e:	49 21 cc             	and    %rcx,%r12
  401c41:	49 21 d7             	and    %rdx,%r15
  401c44:	4c 29 fe             	sub    %r15,%rsi
  401c47:	4d 19 e1             	sbb    %r12,%r9
  401c4a:	83 c0 fc             	add    $0xfffffffc,%eax
  401c4d:	0f 85 fd fe ff ff    	jne    401b50 <runtime::udivmod128+0x260>
  401c53:	48 89 f0             	mov    %rsi,%rax
  401c56:	85 db                	test   %ebx,%ebx
  401c58:	74 54                	je     401cae <runtime::udivmod128+0x3be>
  401c5a:	45 31 db             	xor    %r11d,%r11d
  401c5d:	0f 1f 00             	nopl   (%rax)
  401c60:	49 0f a4 f1 01       	shld   $0x1,%rsi,%r9
  401c65:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  401c6a:	4c 0f a4 d7 01       	shld   $0x1,%r10,%rdi
  401c6f:	4c 89 d0             	mov    %r10,%rax
  401c72:	4c 01 d0             	add    %r10,%rax
  401c75:	45 89 f2             	mov    %r14d,%r10d
  401c78:	49 09 c2             	or     %rax,%r10
  401c7b:	4c 89 c8             	mov    %r9,%rax
  401c7e:	48 f7 d0             	not    %rax
  401c81:	49 89 f6             	mov    %rsi,%r14
  401c84:	49 f7 d6             	not    %r14
  401c87:	49 01 d6             	add    %rdx,%r14
  401c8a:	48 11 c8             	adc    %rcx,%rax
  401c8d:	49 89 c6             	mov    %rax,%r14
  401c90:	49 c1 ee 3f          	shr    $0x3f,%r14
  401c94:	48 c1 f8 3f          	sar    $0x3f,%rax
  401c98:	49 89 c7             	mov    %rax,%r15
  401c9b:	49 21 cf             	and    %rcx,%r15
  401c9e:	48 21 d0             	and    %rdx,%rax
  401ca1:	48 29 c6             	sub    %rax,%rsi
  401ca4:	4d 19 f9             	sbb    %r15,%r9
  401ca7:	ff cb                	dec    %ebx
  401ca9:	75 b5                	jne    401c60 <runtime::udivmod128+0x370>
  401cab:	48 89 f0             	mov    %rsi,%rax
  401cae:	48 89 fe             	mov    %rdi,%rsi
  401cb1:	4b 8d 3c 12          	lea    (%r10,%r10,1),%rdi
  401cb5:	4c 09 f7             	or     %r14,%rdi
  401cb8:	4c 0f a4 d6 01       	shld   $0x1,%r10,%rsi
  401cbd:	4c 09 de             	or     %r11,%rsi
  401cc0:	4d 85 c0             	test   %r8,%r8
  401cc3:	74 07                	je     401ccc <runtime::udivmod128+0x3dc>
  401cc5:	49 89 00             	mov    %rax,(%r8)
  401cc8:	4d 89 48 08          	mov    %r9,0x8(%r8)
  401ccc:	48 89 f8             	mov    %rdi,%rax
  401ccf:	48 89 f2             	mov    %rsi,%rdx
  401cd2:	5b                   	pop    %rbx
  401cd3:	41 5c                	pop    %r12
  401cd5:	41 5d                	pop    %r13
  401cd7:	41 5e                	pop    %r14
  401cd9:	41 5f                	pop    %r15
  401cdb:	c3                   	ret
  401cdc:	0f 0b                	ud2
  401cde:	66 90                	xchg   %ax,%ax

0000000000401ce0 <runtime::heap_allocator_proc>:
  401ce0:	55                   	push   %rbp
  401ce1:	41 57                	push   %r15
  401ce3:	41 56                	push   %r14
  401ce5:	41 55                	push   %r13
  401ce7:	41 54                	push   %r12
  401ce9:	53                   	push   %rbx
  401cea:	50                   	push   %rax
  401ceb:	4c 8b 7c 24 48       	mov    0x48(%rsp),%r15
  401cf0:	40 80 fe 07          	cmp    $0x7,%sil
  401cf4:	0f 87 e3 00 00 00    	ja     401ddd <runtime::heap_allocator_proc+0xfd>
  401cfa:	40 0f b6 c6          	movzbl %sil,%eax
  401cfe:	ff 24 c5 70 50 40 00 	jmp    *0x405070(,%rax,8)
  401d05:	48 83 f9 09          	cmp    $0x9,%rcx
  401d09:	bb 08 00 00 00       	mov    $0x8,%ebx
  401d0e:	48 0f 4d d9          	cmovge %rcx,%rbx
  401d12:	48 8d 44 1a 07       	lea    0x7(%rdx,%rbx,1),%rax
  401d17:	40 b5 01             	mov    $0x1,%bpl
  401d1a:	48 85 c0             	test   %rax,%rax
  401d1d:	0f 8e 4d 01 00 00    	jle    401e70 <runtime::heap_allocator_proc+0x190>
  401d23:	49 89 d6             	mov    %rdx,%r14
  401d26:	40 84 f6             	test   %sil,%sil
  401d29:	0f 84 2b 01 00 00    	je     401e5a <runtime::heap_allocator_proc+0x17a>
  401d2f:	48 89 c7             	mov    %rax,%rdi
  401d32:	e8 39 f3 ff ff       	call   401070 <malloc@plt>
  401d37:	48 85 c0             	test   %rax,%rax
  401d3a:	0f 84 30 01 00 00    	je     401e70 <runtime::heap_allocator_proc+0x190>
  401d40:	48 8d 4c 18 07       	lea    0x7(%rax,%rbx,1),%rcx
  401d45:	48 f7 db             	neg    %rbx
  401d48:	48 21 cb             	and    %rcx,%rbx
  401d4b:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401d4f:	4c 89 f0             	mov    %r14,%rax
  401d52:	48 c1 f8 3f          	sar    $0x3f,%rax
  401d56:	c4 c2 f8 f2 c6       	andn   %r14,%rax,%rax
  401d5b:	31 ed                	xor    %ebp,%ebp
  401d5d:	e9 12 01 00 00       	jmp    401e74 <runtime::heap_allocator_proc+0x194>
  401d62:	4d 85 c0             	test   %r8,%r8
  401d65:	0f 84 82 00 00 00    	je     401ded <runtime::heap_allocator_proc+0x10d>
  401d6b:	4d 89 ce             	mov    %r9,%r14
  401d6e:	48 83 f9 09          	cmp    $0x9,%rcx
  401d72:	bb 08 00 00 00       	mov    $0x8,%ebx
  401d77:	48 0f 4d d9          	cmovge %rcx,%rbx
  401d7b:	48 8d 44 1a 07       	lea    0x7(%rdx,%rbx,1),%rax
  401d80:	0f 8c 9d 00 00 00    	jl     401e23 <runtime::heap_allocator_proc+0x143>
  401d86:	48 85 c0             	test   %rax,%rax
  401d89:	0f 8e d9 01 00 00    	jle    401f68 <runtime::heap_allocator_proc+0x288>
  401d8f:	4d 89 c5             	mov    %r8,%r13
  401d92:	49 89 d4             	mov    %rdx,%r12
  401d95:	89 f5                	mov    %esi,%ebp
  401d97:	40 80 fe 03          	cmp    $0x3,%sil
  401d9b:	0f 85 10 01 00 00    	jne    401eb1 <runtime::heap_allocator_proc+0x1d1>
  401da1:	bf 01 00 00 00       	mov    $0x1,%edi
  401da6:	48 89 c6             	mov    %rax,%rsi
  401da9:	e8 a2 f2 ff ff       	call   401050 <calloc@plt>
  401dae:	e9 06 01 00 00       	jmp    401eb9 <runtime::heap_allocator_proc+0x1d9>
  401db3:	c5 f9 ef c0          	vpxor  %xmm0,%xmm0,%xmm0
  401db7:	c4 c1 7a 7f 07       	vmovdqu %xmm0,(%r15)
  401dbc:	40 b5 04             	mov    $0x4,%bpl
  401dbf:	e9 32 03 00 00       	jmp    4020f6 <runtime::heap_allocator_proc+0x416>
  401dc4:	4d 85 c0             	test   %r8,%r8
  401dc7:	74 14                	je     401ddd <runtime::heap_allocator_proc+0xfd>
  401dc9:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401dcd:	e8 5e f2 ff ff       	call   401030 <free@plt>
  401dd2:	eb 09                	jmp    401ddd <runtime::heap_allocator_proc+0xfd>
  401dd4:	4d 85 c0             	test   %r8,%r8
  401dd7:	74 04                	je     401ddd <runtime::heap_allocator_proc+0xfd>
  401dd9:	41 c6 00 db          	movb   $0xdb,(%r8)
  401ddd:	c5 f9 ef c0          	vpxor  %xmm0,%xmm0,%xmm0
  401de1:	c4 c1 7a 7f 07       	vmovdqu %xmm0,(%r15)
  401de6:	31 ed                	xor    %ebp,%ebp
  401de8:	e9 09 03 00 00       	jmp    4020f6 <runtime::heap_allocator_proc+0x416>
  401ded:	48 83 f9 09          	cmp    $0x9,%rcx
  401df1:	bb 08 00 00 00       	mov    $0x8,%ebx
  401df6:	48 0f 4d d9          	cmovge %rcx,%rbx
  401dfa:	48 8d 44 1a 07       	lea    0x7(%rdx,%rbx,1),%rax
  401dff:	40 b5 01             	mov    $0x1,%bpl
  401e02:	48 85 c0             	test   %rax,%rax
  401e05:	0f 8e 69 01 00 00    	jle    401f74 <runtime::heap_allocator_proc+0x294>
  401e0b:	49 89 d6             	mov    %rdx,%r14
  401e0e:	40 80 fe 03          	cmp    $0x3,%sil
  401e12:	75 6c                	jne    401e80 <runtime::heap_allocator_proc+0x1a0>
  401e14:	bf 01 00 00 00       	mov    $0x1,%edi
  401e19:	48 89 c6             	mov    %rax,%rsi
  401e1c:	e8 2f f2 ff ff       	call   401050 <calloc@plt>
  401e21:	eb 65                	jmp    401e88 <runtime::heap_allocator_proc+0x1a8>
  401e23:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401e27:	89 f5                	mov    %esi,%ebp
  401e29:	48 89 c6             	mov    %rax,%rsi
  401e2c:	49 89 d4             	mov    %rdx,%r12
  401e2f:	4d 89 c5             	mov    %r8,%r13
  401e32:	e8 49 f2 ff ff       	call   401080 <realloc@plt>
  401e37:	4d 89 e8             	mov    %r13,%r8
  401e3a:	48 85 c0             	test   %rax,%rax
  401e3d:	0f 84 25 01 00 00    	je     401f68 <runtime::heap_allocator_proc+0x288>
  401e43:	4c 89 e1             	mov    %r12,%rcx
  401e46:	48 8d 54 18 07       	lea    0x7(%rax,%rbx,1),%rdx
  401e4b:	48 f7 db             	neg    %rbx
  401e4e:	48 21 d3             	and    %rdx,%rbx
  401e51:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401e55:	e9 b1 00 00 00       	jmp    401f0b <runtime::heap_allocator_proc+0x22b>
  401e5a:	bf 01 00 00 00       	mov    $0x1,%edi
  401e5f:	48 89 c6             	mov    %rax,%rsi
  401e62:	e8 e9 f1 ff ff       	call   401050 <calloc@plt>
  401e67:	48 85 c0             	test   %rax,%rax
  401e6a:	0f 85 d0 fe ff ff    	jne    401d40 <runtime::heap_allocator_proc+0x60>
  401e70:	31 db                	xor    %ebx,%ebx
  401e72:	31 c0                	xor    %eax,%eax
  401e74:	49 89 1f             	mov    %rbx,(%r15)
  401e77:	49 89 47 08          	mov    %rax,0x8(%r15)
  401e7b:	e9 76 02 00 00       	jmp    4020f6 <runtime::heap_allocator_proc+0x416>
  401e80:	48 89 c7             	mov    %rax,%rdi
  401e83:	e8 e8 f1 ff ff       	call   401070 <malloc@plt>
  401e88:	48 85 c0             	test   %rax,%rax
  401e8b:	0f 84 e3 00 00 00    	je     401f74 <runtime::heap_allocator_proc+0x294>
  401e91:	48 8d 4c 18 07       	lea    0x7(%rax,%rbx,1),%rcx
  401e96:	48 f7 db             	neg    %rbx
  401e99:	48 21 cb             	and    %rcx,%rbx
  401e9c:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401ea0:	4c 89 f0             	mov    %r14,%rax
  401ea3:	48 c1 f8 3f          	sar    $0x3f,%rax
  401ea7:	c4 42 f8 f2 ce       	andn   %r14,%rax,%r9
  401eac:	e9 3c 02 00 00       	jmp    4020ed <runtime::heap_allocator_proc+0x40d>
  401eb1:	48 89 c7             	mov    %rax,%rdi
  401eb4:	e8 b7 f1 ff ff       	call   401070 <malloc@plt>
  401eb9:	48 85 c0             	test   %rax,%rax
  401ebc:	4d 89 e8             	mov    %r13,%r8
  401ebf:	0f 84 a3 00 00 00    	je     401f68 <runtime::heap_allocator_proc+0x288>
  401ec5:	4c 89 e2             	mov    %r12,%rdx
  401ec8:	48 8d 4c 18 07       	lea    0x7(%rax,%rbx,1),%rcx
  401ecd:	48 f7 db             	neg    %rbx
  401ed0:	48 21 cb             	and    %rcx,%rbx
  401ed3:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401ed7:	4d 39 e6             	cmp    %r12,%r14
  401eda:	49 0f 4c d6          	cmovl  %r14,%rdx
  401ede:	49 39 d8             	cmp    %rbx,%r8
  401ee1:	0f 95 c0             	setne  %al
  401ee4:	48 85 d2             	test   %rdx,%rdx
  401ee7:	0f 9f c1             	setg   %cl
  401eea:	20 c1                	and    %al,%cl
  401eec:	80 f9 01             	cmp    $0x1,%cl
  401eef:	75 0e                	jne    401eff <runtime::heap_allocator_proc+0x21f>
  401ef1:	48 89 df             	mov    %rbx,%rdi
  401ef4:	4c 89 c6             	mov    %r8,%rsi
  401ef7:	e8 64 f1 ff ff       	call   401060 <memcpy@plt>
  401efc:	4d 89 e8             	mov    %r13,%r8
  401eff:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401f03:	e8 28 f1 ff ff       	call   401030 <free@plt>
  401f08:	4c 89 e1             	mov    %r12,%rcx
  401f0b:	40 80 fd 03          	cmp    $0x3,%bpl
  401f0f:	0f 94 c0             	sete   %al
  401f12:	48 89 ca             	mov    %rcx,%rdx
  401f15:	48 c1 fa 3f          	sar    $0x3f,%rdx
  401f19:	c4 62 e8 f2 c9       	andn   %rcx,%rdx,%r9
  401f1e:	4c 29 f1             	sub    %r14,%rcx
  401f21:	0f 9f c2             	setg   %dl
  401f24:	20 c2                	and    %al,%dl
  401f26:	80 fa 01             	cmp    $0x1,%dl
  401f29:	0f 85 be 01 00 00    	jne    4020ed <runtime::heap_allocator_proc+0x40d>
  401f2f:	4d 85 f6             	test   %r14,%r14
  401f32:	0f 88 fc 00 00 00    	js     402034 <runtime::heap_allocator_proc+0x354>
  401f38:	48 85 c9             	test   %rcx,%rcx
  401f3b:	0f 8e ac 01 00 00    	jle    4020ed <runtime::heap_allocator_proc+0x40d>
  401f41:	4a 8d 04 33          	lea    (%rbx,%r14,1),%rax
  401f45:	89 ca                	mov    %ecx,%edx
  401f47:	83 e2 07             	and    $0x7,%edx
  401f4a:	48 83 f9 08          	cmp    $0x8,%rcx
  401f4e:	0f 82 17 01 00 00    	jb     40206b <runtime::heap_allocator_proc+0x38b>
  401f54:	48 89 ce             	mov    %rcx,%rsi
  401f57:	48 c1 ee 03          	shr    $0x3,%rsi
  401f5b:	48 83 f9 1f          	cmp    $0x1f,%rcx
  401f5f:	77 1d                	ja     401f7e <runtime::heap_allocator_proc+0x29e>
  401f61:	31 ff                	xor    %edi,%edi
  401f63:	e9 ec 00 00 00       	jmp    402054 <runtime::heap_allocator_proc+0x374>
  401f68:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401f6c:	e8 bf f0 ff ff       	call   401030 <free@plt>
  401f71:	40 b5 01             	mov    $0x1,%bpl
  401f74:	31 db                	xor    %ebx,%ebx
  401f76:	45 31 c9             	xor    %r9d,%r9d
  401f79:	e9 71 01 00 00       	jmp    4020ef <runtime::heap_allocator_proc+0x40f>
  401f7e:	49 b8 f0 ff ff ff ff 	movabs $0xffffffffffffff0,%r8
  401f85:	ff ff 0f 
  401f88:	48 81 f9 80 00 00 00 	cmp    $0x80,%rcx
  401f8f:	73 04                	jae    401f95 <runtime::heap_allocator_proc+0x2b5>
  401f91:	31 ff                	xor    %edi,%edi
  401f93:	eb 6f                	jmp    402004 <runtime::heap_allocator_proc+0x324>
  401f95:	48 89 f7             	mov    %rsi,%rdi
  401f98:	4c 21 c7             	and    %r8,%rdi
  401f9b:	4d 8d 54 1e 60       	lea    0x60(%r14,%rbx,1),%r10
  401fa0:	45 31 db             	xor    %r11d,%r11d
  401fa3:	c5 f9 ef c0          	vpxor  %xmm0,%xmm0,%xmm0
  401fa7:	c5 f5 76 c9          	vpcmpeqd %ymm1,%ymm1,%ymm1
  401fab:	c4 82 7d 29 54 da a0 	vpcmpeqq -0x60(%r10,%r11,8),%ymm0,%ymm2
  401fb2:	c5 ed ef d1          	vpxor  %ymm1,%ymm2,%ymm2
  401fb6:	c4 82 7d 29 5c da c0 	vpcmpeqq -0x40(%r10,%r11,8),%ymm0,%ymm3
  401fbd:	c5 e5 ef d9          	vpxor  %ymm1,%ymm3,%ymm3
  401fc1:	c4 82 7d 29 64 da e0 	vpcmpeqq -0x20(%r10,%r11,8),%ymm0,%ymm4
  401fc8:	c5 dd ef e1          	vpxor  %ymm1,%ymm4,%ymm4
  401fcc:	c4 82 7d 29 2c da    	vpcmpeqq (%r10,%r11,8),%ymm0,%ymm5
  401fd2:	c5 d5 ef e9          	vpxor  %ymm1,%ymm5,%ymm5
  401fd6:	c4 82 ed 8e 44 da a0 	vpmaskmovq %ymm0,%ymm2,-0x60(%r10,%r11,8)
  401fdd:	c4 82 e5 8e 44 da c0 	vpmaskmovq %ymm0,%ymm3,-0x40(%r10,%r11,8)
  401fe4:	c4 82 dd 8e 44 da e0 	vpmaskmovq %ymm0,%ymm4,-0x20(%r10,%r11,8)
  401feb:	c4 82 d5 8e 04 da    	vpmaskmovq %ymm0,%ymm5,(%r10,%r11,8)
  401ff1:	49 83 c3 10          	add    $0x10,%r11
  401ff5:	4c 39 df             	cmp    %r11,%rdi
  401ff8:	75 b1                	jne    401fab <runtime::heap_allocator_proc+0x2cb>
  401ffa:	48 39 fe             	cmp    %rdi,%rsi
  401ffd:	74 6c                	je     40206b <runtime::heap_allocator_proc+0x38b>
  401fff:	f6 c1 60             	test   $0x60,%cl
  402002:	74 50                	je     402054 <runtime::heap_allocator_proc+0x374>
  402004:	49 89 fa             	mov    %rdi,%r10
  402007:	49 83 c0 0c          	add    $0xc,%r8
  40200b:	4c 89 c7             	mov    %r8,%rdi
  40200e:	48 21 f7             	and    %rsi,%rdi
  402011:	c5 f9 ef c0          	vpxor  %xmm0,%xmm0,%xmm0
  402015:	c5 f5 76 c9          	vpcmpeqd %ymm1,%ymm1,%ymm1
  402019:	c4 a2 7d 29 14 d0    	vpcmpeqq (%rax,%r10,8),%ymm0,%ymm2
  40201f:	c5 ed ef d1          	vpxor  %ymm1,%ymm2,%ymm2
  402023:	c4 a2 ed 8e 04 d0    	vpmaskmovq %ymm0,%ymm2,(%rax,%r10,8)
  402029:	49 83 c2 04          	add    $0x4,%r10
  40202d:	4c 39 d7             	cmp    %r10,%rdi
  402030:	75 e7                	jne    402019 <runtime::heap_allocator_proc+0x339>
  402032:	eb 32                	jmp    402066 <runtime::heap_allocator_proc+0x386>
  402034:	4c 89 0c 24          	mov    %r9,(%rsp)
  402038:	bf b0 65 40 00       	mov    $0x4065b0,%edi
  40203d:	be 2e 00 00 00       	mov    $0x2e,%esi
  402042:	ba 4d 00 00 00       	mov    $0x4d,%edx
  402047:	b9 26 00 00 00       	mov    $0x26,%ecx
  40204c:	4d 89 f0             	mov    %r14,%r8
  40204f:	e8 4c f0 ff ff       	call   4010a0 <runtime::slice_handle_error>
  402054:	48 83 3c f8 00       	cmpq   $0x0,(%rax,%rdi,8)
  402059:	74 08                	je     402063 <runtime::heap_allocator_proc+0x383>
  40205b:	48 c7 04 f8 00 00 00 	movq   $0x0,(%rax,%rdi,8)
  402062:	00 
  402063:	48 ff c7             	inc    %rdi
  402066:	48 39 fe             	cmp    %rdi,%rsi
  402069:	75 e9                	jne    402054 <runtime::heap_allocator_proc+0x374>
  40206b:	48 85 d2             	test   %rdx,%rdx
  40206e:	74 7d                	je     4020ed <runtime::heap_allocator_proc+0x40d>
  402070:	48 be f8 ff ff ff ff 	movabs $0x7ffffffffffffff8,%rsi
  402077:	ff ff 7f 
  40207a:	48 21 f1             	and    %rsi,%rcx
  40207d:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  402081:	74 04                	je     402087 <runtime::heap_allocator_proc+0x3a7>
  402083:	c6 04 08 00          	movb   $0x0,(%rax,%rcx,1)
  402087:	83 fa 01             	cmp    $0x1,%edx
  40208a:	74 61                	je     4020ed <runtime::heap_allocator_proc+0x40d>
  40208c:	80 7c 08 01 00       	cmpb   $0x0,0x1(%rax,%rcx,1)
  402091:	74 05                	je     402098 <runtime::heap_allocator_proc+0x3b8>
  402093:	c6 44 08 01 00       	movb   $0x0,0x1(%rax,%rcx,1)
  402098:	83 fa 02             	cmp    $0x2,%edx
  40209b:	74 50                	je     4020ed <runtime::heap_allocator_proc+0x40d>
  40209d:	80 7c 08 02 00       	cmpb   $0x0,0x2(%rax,%rcx,1)
  4020a2:	74 05                	je     4020a9 <runtime::heap_allocator_proc+0x3c9>
  4020a4:	c6 44 08 02 00       	movb   $0x0,0x2(%rax,%rcx,1)
  4020a9:	83 fa 03             	cmp    $0x3,%edx
  4020ac:	74 3f                	je     4020ed <runtime::heap_allocator_proc+0x40d>
  4020ae:	80 7c 08 03 00       	cmpb   $0x0,0x3(%rax,%rcx,1)
  4020b3:	74 05                	je     4020ba <runtime::heap_allocator_proc+0x3da>
  4020b5:	c6 44 08 03 00       	movb   $0x0,0x3(%rax,%rcx,1)
  4020ba:	83 fa 04             	cmp    $0x4,%edx
  4020bd:	74 2e                	je     4020ed <runtime::heap_allocator_proc+0x40d>
  4020bf:	80 7c 08 04 00       	cmpb   $0x0,0x4(%rax,%rcx,1)
  4020c4:	74 05                	je     4020cb <runtime::heap_allocator_proc+0x3eb>
  4020c6:	c6 44 08 04 00       	movb   $0x0,0x4(%rax,%rcx,1)
  4020cb:	83 fa 05             	cmp    $0x5,%edx
  4020ce:	74 1d                	je     4020ed <runtime::heap_allocator_proc+0x40d>
  4020d0:	80 7c 08 05 00       	cmpb   $0x0,0x5(%rax,%rcx,1)
  4020d5:	74 05                	je     4020dc <runtime::heap_allocator_proc+0x3fc>
  4020d7:	c6 44 08 05 00       	movb   $0x0,0x5(%rax,%rcx,1)
  4020dc:	83 fa 06             	cmp    $0x6,%edx
  4020df:	74 0c                	je     4020ed <runtime::heap_allocator_proc+0x40d>
  4020e1:	80 7c 08 06 00       	cmpb   $0x0,0x6(%rax,%rcx,1)
  4020e6:	74 05                	je     4020ed <runtime::heap_allocator_proc+0x40d>
  4020e8:	c6 44 08 06 00       	movb   $0x0,0x6(%rax,%rcx,1)
  4020ed:	31 ed                	xor    %ebp,%ebp
  4020ef:	49 89 1f             	mov    %rbx,(%r15)
  4020f2:	4d 89 4f 08          	mov    %r9,0x8(%r15)
  4020f6:	89 e8                	mov    %ebp,%eax
  4020f8:	48 83 c4 08          	add    $0x8,%rsp
  4020fc:	5b                   	pop    %rbx
  4020fd:	41 5c                	pop    %r12
  4020ff:	41 5d                	pop    %r13
  402101:	41 5e                	pop    %r14
  402103:	41 5f                	pop    %r15
  402105:	5d                   	pop    %rbp
  402106:	c5 f8 77             	vzeroupper
  402109:	c3                   	ret
  40210a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402110 <runtime::default_temp_allocator_proc>:
  402110:	41 57                	push   %r15
  402112:	41 56                	push   %r14
  402114:	41 54                	push   %r12
  402116:	53                   	push   %rbx
  402117:	48 83 ec 78          	sub    $0x78,%rsp
  40211b:	40 80 fe 07          	cmp    $0x7,%sil
  40211f:	0f 87 05 02 00 00    	ja     40232a <runtime::default_temp_allocator_proc+0x21a>
  402125:	4c 89 c8             	mov    %r9,%rax
  402128:	48 89 fb             	mov    %rdi,%rbx
  40212b:	4c 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%r9
  402132:	00 
  402133:	4c 8b b4 24 a0 00 00 	mov    0xa0(%rsp),%r14
  40213a:	00 
  40213b:	40 0f b6 f6          	movzbl %sil,%esi
  40213f:	ff 24 f5 b0 50 40 00 	jmp    *0x4050b0(,%rsi,8)
  402146:	b0 04                	mov    $0x4,%al
  402148:	45 31 c0             	xor    %r8d,%r8d
  40214b:	31 d2                	xor    %edx,%edx
  40214d:	e9 df 01 00 00       	jmp    402331 <runtime::default_temp_allocator_proc+0x221>
  402152:	4d 85 c0             	test   %r8,%r8
  402155:	74 31                	je     402188 <runtime::default_temp_allocator_proc+0x78>
  402157:	48 39 c2             	cmp    %rax,%rdx
  40215a:	0f 85 22 01 00 00    	jne    402282 <runtime::default_temp_allocator_proc+0x172>
  402160:	48 85 d2             	test   %rdx,%rdx
  402163:	0f 89 c6 01 00 00    	jns    40232f <runtime::default_temp_allocator_proc+0x21f>
  402169:	bf a1 5f 40 00       	mov    $0x405fa1,%edi
  40216e:	be 3c 00 00 00       	mov    $0x3c,%esi
  402173:	49 89 d1             	mov    %rdx,%r9
  402176:	ba db 00 00 00       	mov    $0xdb,%edx
  40217b:	b9 13 00 00 00       	mov    $0x13,%ecx
  402180:	45 31 c0             	xor    %r8d,%r8d
  402183:	e8 a8 ef ff ff       	call   401130 <runtime::multi_pointer_slice_handle_error>
  402188:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40218c:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  402191:	49 89 e0             	mov    %rsp,%r8
  402194:	48 89 df             	mov    %rbx,%rdi
  402197:	48 89 d6             	mov    %rdx,%rsi
  40219a:	48 89 ca             	mov    %rcx,%rdx
  40219d:	4c 89 f1             	mov    %r14,%rcx
  4021a0:	e8 eb 06 00 00       	call   402890 <runtime::arena_alloc>
  4021a5:	4c 8b 04 24          	mov    (%rsp),%r8
  4021a9:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4021ae:	e9 7e 01 00 00       	jmp    402331 <runtime::default_temp_allocator_proc+0x221>
  4021b3:	48 8b 7b 10          	mov    0x10(%rbx),%rdi
  4021b7:	48 85 ff             	test   %rdi,%rdi
  4021ba:	0f 84 62 01 00 00    	je     402322 <runtime::default_temp_allocator_proc+0x212>
  4021c0:	4c 8d 64 24 30       	lea    0x30(%rsp),%r12
  4021c5:	49 89 e7             	mov    %rsp,%r15
  4021c8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4021cf:	00 
  4021d0:	48 8b 07             	mov    (%rdi),%rax
  4021d3:	48 85 c0             	test   %rax,%rax
  4021d6:	0f 84 28 01 00 00    	je     402304 <runtime::default_temp_allocator_proc+0x1f4>
  4021dc:	48 89 43 10          	mov    %rax,0x10(%rbx)
  4021e0:	48 8b 47 28          	mov    0x28(%rdi),%rax
  4021e4:	48 29 43 20          	sub    %rax,0x20(%rbx)
  4021e8:	48 8b 77 08          	mov    0x8(%rdi),%rsi
  4021ec:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  4021f0:	48 c7 44 24 10 10 21 	movq   $0x402110,0x10(%rsp)
  4021f7:	40 00 
  4021f9:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402200:	00 00 
  402202:	48 8d 80 48 fb ff ff 	lea    -0x4b8(%rax),%rax
  402209:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40220e:	48 c7 44 24 20 a0 11 	movq   $0x4011a0,0x20(%rsp)
  402215:	40 00 
  402217:	48 c7 44 24 28 20 43 	movq   $0x404320,0x28(%rsp)
  40221e:	40 00 
  402220:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402224:	c4 c1 78 11 04 24    	vmovups %xmm0,(%r12)
  40222a:	49 c7 44 24 10 00 00 	movq   $0x0,0x10(%r12)
  402231:	00 00 
  402233:	48 c7 44 24 48 10 24 	movq   $0x402410,0x48(%rsp)
  40223a:	40 00 
  40223c:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402240:	c4 c1 7c 11 44 24 20 	vmovups %ymm0,0x20(%r12)
  402247:	48 89 34 24          	mov    %rsi,(%rsp)
  40224b:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  402250:	4c 89 f1             	mov    %r14,%rcx
  402253:	4d 89 f8             	mov    %r15,%r8
  402256:	c5 f8 77             	vzeroupper
  402259:	e8 92 12 00 00       	call   4034f0 <runtime::mem_free>
  40225e:	48 8b 7b 10          	mov    0x10(%rbx),%rdi
  402262:	48 85 ff             	test   %rdi,%rdi
  402265:	0f 85 65 ff ff ff    	jne    4021d0 <runtime::default_temp_allocator_proc+0xc0>
  40226b:	e9 b2 00 00 00       	jmp    402322 <runtime::default_temp_allocator_proc+0x212>
  402270:	4d 85 c0             	test   %r8,%r8
  402273:	0f 84 b1 00 00 00    	je     40232a <runtime::default_temp_allocator_proc+0x21a>
  402279:	41 c6 00 5d          	movb   $0x5d,(%r8)
  40227d:	e9 a8 00 00 00       	jmp    40232a <runtime::default_temp_allocator_proc+0x21a>
  402282:	48 85 d2             	test   %rdx,%rdx
  402285:	0f 84 bb fe ff ff    	je     402146 <runtime::default_temp_allocator_proc+0x36>
  40228b:	48 8d 71 ff          	lea    -0x1(%rcx),%rsi
  40228f:	4c 85 c6             	test   %r8,%rsi
  402292:	0f 84 b4 00 00 00    	je     40234c <runtime::default_temp_allocator_proc+0x23c>
  402298:	49 89 c4             	mov    %rax,%r12
  40229b:	4d 89 c7             	mov    %r8,%r15
  40229e:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4022a2:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  4022a7:	49 89 e0             	mov    %rsp,%r8
  4022aa:	48 89 df             	mov    %rbx,%rdi
  4022ad:	48 89 d6             	mov    %rdx,%rsi
  4022b0:	48 89 ca             	mov    %rcx,%rdx
  4022b3:	4c 89 f1             	mov    %r14,%rcx
  4022b6:	e8 d5 05 00 00       	call   402890 <runtime::arena_alloc>
  4022bb:	84 c0                	test   %al,%al
  4022bd:	0f 85 85 fe ff ff    	jne    402148 <runtime::default_temp_allocator_proc+0x38>
  4022c3:	48 8b 1c 24          	mov    (%rsp),%rbx
  4022c7:	48 85 db             	test   %rbx,%rbx
  4022ca:	74 5e                	je     40232a <runtime::default_temp_allocator_proc+0x21a>
  4022cc:	4d 89 e1             	mov    %r12,%r9
  4022cf:	4d 85 e4             	test   %r12,%r12
  4022d2:	0f 88 f1 00 00 00    	js     4023c9 <runtime::default_temp_allocator_proc+0x2b9>
  4022d8:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4022dd:	4c 39 ca             	cmp    %r9,%rdx
  4022e0:	4c 0f 4c ca          	cmovl  %rdx,%r9
  4022e4:	4d 85 c9             	test   %r9,%r9
  4022e7:	7e 14                	jle    4022fd <runtime::default_temp_allocator_proc+0x1ed>
  4022e9:	4c 89 fe             	mov    %r15,%rsi
  4022ec:	48 89 df             	mov    %rbx,%rdi
  4022ef:	49 89 d6             	mov    %rdx,%r14
  4022f2:	4c 89 ca             	mov    %r9,%rdx
  4022f5:	e8 96 ed ff ff       	call   401090 <memmove@plt>
  4022fa:	4c 89 f2             	mov    %r14,%rdx
  4022fd:	31 c0                	xor    %eax,%eax
  4022ff:	49 89 d8             	mov    %rbx,%r8
  402302:	eb 2d                	jmp    402331 <runtime::default_temp_allocator_proc+0x221>
  402304:	48 8b 47 18          	mov    0x18(%rdi),%rax
  402308:	48 8b 57 20          	mov    0x20(%rdi),%rdx
  40230c:	48 89 c7             	mov    %rax,%rdi
  40230f:	31 f6                	xor    %esi,%esi
  402311:	e8 2a ed ff ff       	call   401040 <memset@plt>
  402316:	48 8b 43 10          	mov    0x10(%rbx),%rax
  40231a:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  402321:	00 
  402322:	48 c7 43 18 00 00 00 	movq   $0x0,0x18(%rbx)
  402329:	00 
  40232a:	45 31 c0             	xor    %r8d,%r8d
  40232d:	31 d2                	xor    %edx,%edx
  40232f:	31 c0                	xor    %eax,%eax
  402331:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  402338:	00 
  402339:	4c 89 01             	mov    %r8,(%rcx)
  40233c:	48 89 51 08          	mov    %rdx,0x8(%rcx)
  402340:	48 83 c4 78          	add    $0x78,%rsp
  402344:	5b                   	pop    %rbx
  402345:	41 5c                	pop    %r12
  402347:	41 5e                	pop    %r14
  402349:	41 5f                	pop    %r15
  40234b:	c3                   	ret
  40234c:	48 39 c2             	cmp    %rax,%rdx
  40234f:	73 24                	jae    402375 <runtime::default_temp_allocator_proc+0x265>
  402351:	48 85 d2             	test   %rdx,%rdx
  402354:	79 d9                	jns    40232f <runtime::default_temp_allocator_proc+0x21f>
  402356:	bf a1 5f 40 00       	mov    $0x405fa1,%edi
  40235b:	be 3c 00 00 00       	mov    $0x3c,%esi
  402360:	49 89 d1             	mov    %rdx,%r9
  402363:	ba e3 00 00 00       	mov    $0xe3,%edx
  402368:	b9 14 00 00 00       	mov    $0x14,%ecx
  40236d:	45 31 c0             	xor    %r8d,%r8d
  402370:	e8 bb ed ff ff       	call   401130 <runtime::multi_pointer_slice_handle_error>
  402375:	48 8b 73 10          	mov    0x10(%rbx),%rsi
  402379:	48 85 f6             	test   %rsi,%rsi
  40237c:	0f 84 16 ff ff ff    	je     402298 <runtime::default_temp_allocator_proc+0x188>
  402382:	4d 89 c2             	mov    %r8,%r10
  402385:	4c 2b 56 18          	sub    0x18(%rsi),%r10
  402389:	49 8d 3c 02          	lea    (%r10,%rax,1),%rdi
  40238d:	49 39 fa             	cmp    %rdi,%r10
  402390:	0f 83 02 ff ff ff    	jae    402298 <runtime::default_temp_allocator_proc+0x188>
  402396:	48 3b 7e 20          	cmp    0x20(%rsi),%rdi
  40239a:	0f 85 f8 fe ff ff    	jne    402298 <runtime::default_temp_allocator_proc+0x188>
  4023a0:	4d 8d 1c 12          	lea    (%r10,%rdx,1),%r11
  4023a4:	4c 3b 5e 28          	cmp    0x28(%rsi),%r11
  4023a8:	0f 87 ea fe ff ff    	ja     402298 <runtime::default_temp_allocator_proc+0x188>
  4023ae:	4c 89 5e 20          	mov    %r11,0x20(%rsi)
  4023b2:	4c 89 5b 18          	mov    %r11,0x18(%rbx)
  4023b6:	4d 39 da             	cmp    %r11,%r10
  4023b9:	7f 2a                	jg     4023e5 <runtime::default_temp_allocator_proc+0x2d5>
  4023bb:	4c 03 56 18          	add    0x18(%rsi),%r10
  4023bf:	31 c0                	xor    %eax,%eax
  4023c1:	4d 89 d0             	mov    %r10,%r8
  4023c4:	e9 68 ff ff ff       	jmp    402331 <runtime::default_temp_allocator_proc+0x221>
  4023c9:	bf a1 5f 40 00       	mov    $0x405fa1,%edi
  4023ce:	be 3c 00 00 00       	mov    $0x3c,%esi
  4023d3:	ba fa 00 00 00       	mov    $0xfa,%edx
  4023d8:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  4023dd:	45 31 c0             	xor    %r8d,%r8d
  4023e0:	e8 4b ed ff ff       	call   401130 <runtime::multi_pointer_slice_handle_error>
  4023e5:	bf a1 5f 40 00       	mov    $0x405fa1,%edi
  4023ea:	be 3c 00 00 00       	mov    $0x3c,%esi
  4023ef:	ba ef 00 00 00       	mov    $0xef,%edx
  4023f4:	b9 17 00 00 00       	mov    $0x17,%ecx
  4023f9:	4d 89 d0             	mov    %r10,%r8
  4023fc:	4d 89 d9             	mov    %r11,%r9
  4023ff:	e8 2c ed ff ff       	call   401130 <runtime::multi_pointer_slice_handle_error>
  402404:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40240b:	00 00 00 00 00 

0000000000402410 <runtime::default_random_generator_proc>:
  402410:	55                   	push   %rbp
  402411:	41 57                	push   %r15
  402413:	41 56                	push   %r14
  402415:	41 55                	push   %r13
  402417:	41 54                	push   %r12
  402419:	53                   	push   %rbx
  40241a:	48 83 ec 18          	sub    $0x18,%rsp
  40241e:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402423:	49 89 cc             	mov    %rcx,%r12
  402426:	49 89 d7             	mov    %rdx,%r15
  402429:	64 4c 8b 34 25 00 00 	mov    %fs:0x0,%r14
  402430:	00 00 
  402432:	4d 8d b6 f0 fb ff ff 	lea    -0x410(%r14),%r14
  402439:	48 85 ff             	test   %rdi,%rdi
  40243c:	4c 0f 45 f7          	cmovne %rdi,%r14
  402440:	48 83 fe 02          	cmp    $0x2,%rsi
  402444:	0f 84 91 00 00 00    	je     4024db <runtime::default_random_generator_proc+0xcb>
  40244a:	49 8d be e0 03 00 00 	lea    0x3e0(%r14),%rdi
  402451:	48 83 fe 01          	cmp    $0x1,%rsi
  402455:	74 42                	je     402499 <runtime::default_random_generator_proc+0x89>
  402457:	48 85 f6             	test   %rsi,%rsi
  40245a:	0f 85 61 01 00 00    	jne    4025c1 <runtime::default_random_generator_proc+0x1b1>
  402460:	41 80 be 08 04 00 00 	cmpb   $0x0,0x408(%r14)
  402467:	00 
  402468:	0f 84 8d 00 00 00    	je     4024fb <runtime::default_random_generator_proc+0xeb>
  40246e:	49 8b 86 00 04 00 00 	mov    0x400(%r14),%rax
  402475:	48 3d e1 03 00 00    	cmp    $0x3e1,%rax
  40247b:	0f 8d b7 02 00 00    	jge    402738 <runtime::default_random_generator_proc+0x328>
  402481:	4d 8d ae 00 04 00 00 	lea    0x400(%r14),%r13
  402488:	48 3d e0 03 00 00    	cmp    $0x3e0,%rax
  40248e:	0f 84 e8 00 00 00    	je     40257c <runtime::default_random_generator_proc+0x16c>
  402494:	e9 f7 00 00 00       	jmp    402590 <runtime::default_random_generator_proc+0x180>
  402499:	4d 85 e4             	test   %r12,%r12
  40249c:	74 50                	je     4024ee <runtime::default_random_generator_proc+0xde>
  40249e:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4024a2:	c5 fc 11 07          	vmovups %ymm0,(%rdi)
  4024a6:	4d 85 e4             	test   %r12,%r12
  4024a9:	7e 18                	jle    4024c3 <runtime::default_random_generator_proc+0xb3>
  4024ab:	49 83 fc 20          	cmp    $0x20,%r12
  4024af:	ba 20 00 00 00       	mov    $0x20,%edx
  4024b4:	49 0f 42 d4          	cmovb  %r12,%rdx
  4024b8:	4c 89 fe             	mov    %r15,%rsi
  4024bb:	c5 f8 77             	vzeroupper
  4024be:	e8 cd eb ff ff       	call   401090 <memmove@plt>
  4024c3:	41 c6 86 08 04 00 00 	movb   $0x1,0x408(%r14)
  4024ca:	01 
  4024cb:	49 c7 86 00 04 00 00 	movq   $0x3e0,0x400(%r14)
  4024d2:	e0 03 00 00 
  4024d6:	e9 e6 00 00 00       	jmp    4025c1 <runtime::default_random_generator_proc+0x1b1>
  4024db:	49 83 fc 04          	cmp    $0x4,%r12
  4024df:	0f 85 dc 00 00 00    	jne    4025c1 <runtime::default_random_generator_proc+0x1b1>
  4024e5:	41 80 0f 0b          	orb    $0xb,(%r15)
  4024e9:	e9 d3 00 00 00       	jmp    4025c1 <runtime::default_random_generator_proc+0x1b1>
  4024ee:	41 c6 86 08 04 00 00 	movb   $0x0,0x408(%r14)
  4024f5:	00 
  4024f6:	e9 c6 00 00 00       	jmp    4025c1 <runtime::default_random_generator_proc+0x1b1>
  4024fb:	41 b9 20 00 00 00    	mov    $0x20,%r9d
  402501:	31 c9                	xor    %ecx,%ecx
  402503:	41 ba 20 00 00 00    	mov    $0x20,%r10d
  402509:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
  402510:	f6 c1 01             	test   $0x1,%cl
  402513:	0f 85 62 01 00 00    	jne    40267b <runtime::default_random_generator_proc+0x26b>
  402519:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
  402520:	b8 3e 01 00 00       	mov    $0x13e,%eax
  402525:	4c 89 d6             	mov    %r10,%rsi
  402528:	31 d2                	xor    %edx,%edx
  40252a:	0f 05                	syscall
  40252c:	48 83 f8 fc          	cmp    $0xfffffffffffffffc,%rax
  402530:	74 ee                	je     402520 <runtime::default_random_generator_proc+0x110>
  402532:	48 83 f8 da          	cmp    $0xffffffffffffffda,%rax
  402536:	0f 84 2b 01 00 00    	je     402667 <runtime::default_random_generator_proc+0x257>
  40253c:	48 85 c0             	test   %rax,%rax
  40253f:	0f 88 59 01 00 00    	js     40269e <runtime::default_random_generator_proc+0x28e>
  402545:	4c 89 ca             	mov    %r9,%rdx
  402548:	48 29 c2             	sub    %rax,%rdx
  40254b:	0f 82 61 01 00 00    	jb     4026b2 <runtime::default_random_generator_proc+0x2a2>
  402551:	48 01 c7             	add    %rax,%rdi
  402554:	4d 39 ca             	cmp    %r9,%r10
  402557:	0f 97 c1             	seta   %cl
  40255a:	49 29 c2             	sub    %rax,%r10
  40255d:	49 89 d1             	mov    %rdx,%r9
  402560:	7f ae                	jg     402510 <runtime::default_random_generator_proc+0x100>
  402562:	4d 8d ae 00 04 00 00 	lea    0x400(%r14),%r13
  402569:	49 c7 86 00 04 00 00 	movq   $0x3e0,0x400(%r14)
  402570:	e0 03 00 00 
  402574:	41 c6 86 08 04 00 00 	movb   $0x1,0x408(%r14)
  40257b:	01 
  40257c:	4c 89 f7             	mov    %r14,%rdi
  40257f:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402584:	e8 d7 08 00 00       	call   402e60 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>
  402589:	49 8b 86 00 04 00 00 	mov    0x400(%r14),%rax
  402590:	48 3d d9 03 00 00    	cmp    $0x3d9,%rax
  402596:	0f 8d 36 01 00 00    	jge    4026d2 <runtime::default_random_generator_proc+0x2c2>
  40259c:	a8 07                	test   $0x7,%al
  40259e:	0f 85 61 01 00 00    	jne    402705 <runtime::default_random_generator_proc+0x2f5>
  4025a4:	49 83 fc 08          	cmp    $0x8,%r12
  4025a8:	75 49                	jne    4025f3 <runtime::default_random_generator_proc+0x1e3>
  4025aa:	49 8b 0c 06          	mov    (%r14,%rax,1),%rcx
  4025ae:	49 89 0f             	mov    %rcx,(%r15)
  4025b1:	49 c7 04 06 00 00 00 	movq   $0x0,(%r14,%rax,1)
  4025b8:	00 
  4025b9:	49 83 86 00 04 00 00 	addq   $0x8,0x400(%r14)
  4025c0:	08 
  4025c1:	48 83 c4 18          	add    $0x18,%rsp
  4025c5:	5b                   	pop    %rbx
  4025c6:	41 5c                	pop    %r12
  4025c8:	41 5d                	pop    %r13
  4025ca:	41 5e                	pop    %r14
  4025cc:	41 5f                	pop    %r15
  4025ce:	5d                   	pop    %rbp
  4025cf:	c5 f8 77             	vzeroupper
  4025d2:	c3                   	ret
  4025d3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4025da:	84 00 00 00 00 00 
  4025e0:	4c 89 f7             	mov    %r14,%rdi
  4025e3:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4025e8:	e8 73 08 00 00       	call   402e60 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>
  4025ed:	49 01 ef             	add    %rbp,%r15
  4025f0:	49 29 ec             	sub    %rbp,%r12
  4025f3:	4d 85 e4             	test   %r12,%r12
  4025f6:	7e c9                	jle    4025c1 <runtime::default_random_generator_proc+0x1b1>
  4025f8:	49 8b 75 00          	mov    0x0(%r13),%rsi
  4025fc:	bd e0 03 00 00       	mov    $0x3e0,%ebp
  402601:	48 29 f5             	sub    %rsi,%rbp
  402604:	49 39 ec             	cmp    %rbp,%r12
  402607:	49 0f 4c ec          	cmovl  %r12,%rbp
  40260b:	ba 00 04 00 00       	mov    $0x400,%edx
  402610:	48 29 f2             	sub    %rsi,%rdx
  402613:	48 39 d5             	cmp    %rdx,%rbp
  402616:	48 0f 4c d5          	cmovl  %rbp,%rdx
  40261a:	48 85 d2             	test   %rdx,%rdx
  40261d:	7e 12                	jle    402631 <runtime::default_random_generator_proc+0x221>
  40261f:	4c 01 f6             	add    %r14,%rsi
  402622:	4c 89 ff             	mov    %r15,%rdi
  402625:	e8 66 ea ff ff       	call   401090 <memmove@plt>
  40262a:	49 8b b6 00 04 00 00 	mov    0x400(%r14),%rsi
  402631:	48 8d 45 07          	lea    0x7(%rbp),%rax
  402635:	48 8d 55 0e          	lea    0xe(%rbp),%rdx
  402639:	48 85 c0             	test   %rax,%rax
  40263c:	48 0f 49 d0          	cmovns %rax,%rdx
  402640:	48 83 e2 f8          	and    $0xfffffffffffffff8,%rdx
  402644:	48 8d 1c 16          	lea    (%rsi,%rdx,1),%rbx
  402648:	48 81 fb df 03 00 00 	cmp    $0x3df,%rbx
  40264f:	7f 8f                	jg     4025e0 <runtime::default_random_generator_proc+0x1d0>
  402651:	4c 01 f6             	add    %r14,%rsi
  402654:	48 89 f7             	mov    %rsi,%rdi
  402657:	31 f6                	xor    %esi,%esi
  402659:	e8 e2 e9 ff ff       	call   401040 <memset@plt>
  40265e:	49 89 9e 00 04 00 00 	mov    %rbx,0x400(%r14)
  402665:	eb 86                	jmp    4025ed <runtime::default_random_generator_proc+0x1dd>
  402667:	bf aa 5e 40 00       	mov    $0x405eaa,%edi
  40266c:	be 2f 00 00 00       	mov    $0x2f,%esi
  402671:	ba f0 5e 40 00       	mov    $0x405ef0,%edx
  402676:	e8 a5 eb ff ff       	call   401220 <runtime::panic_contextless>
  40267b:	4c 89 0c 24          	mov    %r9,(%rsp)
  40267f:	bf 78 5e 40 00       	mov    $0x405e78,%edi
  402684:	be 31 00 00 00       	mov    $0x31,%esi
  402689:	ba 37 00 00 00       	mov    $0x37,%edx
  40268e:	b9 44 00 00 00       	mov    $0x44,%ecx
  402693:	45 31 c0             	xor    %r8d,%r8d
  402696:	4d 89 d1             	mov    %r10,%r9
  402699:	e8 02 ea ff ff       	call   4010a0 <runtime::slice_handle_error>
  40269e:	bf 18 5f 40 00       	mov    $0x405f18,%edi
  4026a3:	be 1e 00 00 00       	mov    $0x1e,%esi
  4026a8:	ba 40 5f 40 00       	mov    $0x405f40,%edx
  4026ad:	e8 6e eb ff ff       	call   401220 <runtime::panic_contextless>
  4026b2:	4c 89 0c 24          	mov    %r9,(%rsp)
  4026b6:	bf 78 5e 40 00       	mov    $0x405e78,%edi
  4026bb:	be 31 00 00 00       	mov    $0x31,%esi
  4026c0:	ba 4a 00 00 00       	mov    $0x4a,%edx
  4026c5:	b9 0c 00 00 00       	mov    $0xc,%ecx
  4026ca:	49 89 c0             	mov    %rax,%r8
  4026cd:	e8 ce e9 ff ff       	call   4010a0 <runtime::slice_handle_error>
  4026d2:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  4026d7:	49 8b 41 20          	mov    0x20(%r9),%rax
  4026db:	48 85 c0             	test   %rax,%rax
  4026de:	41 ba a0 11 40 00    	mov    $0x4011a0,%r10d
  4026e4:	4c 0f 45 d0          	cmovne %rax,%r10
  4026e8:	bf 08 66 40 00       	mov    $0x406608,%edi
  4026ed:	be 11 00 00 00       	mov    $0x11,%esi
  4026f2:	ba 28 62 40 00       	mov    $0x406228,%edx
  4026f7:	b9 36 00 00 00       	mov    $0x36,%ecx
  4026fc:	41 b8 60 62 40 00    	mov    $0x406260,%r8d
  402702:	41 ff d2             	call   *%r10
  402705:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  40270a:	49 8b 41 20          	mov    0x20(%r9),%rax
  40270e:	48 85 c0             	test   %rax,%rax
  402711:	41 ba a0 11 40 00    	mov    $0x4011a0,%r10d
  402717:	4c 0f 45 d0          	cmovne %rax,%r10
  40271b:	bf 08 66 40 00       	mov    $0x406608,%edi
  402720:	be 11 00 00 00       	mov    $0x11,%esi
  402725:	ba 88 62 40 00       	mov    $0x406288,%edx
  40272a:	b9 3d 00 00 00       	mov    $0x3d,%ecx
  40272f:	41 b8 d0 62 40 00    	mov    $0x4062d0,%r8d
  402735:	41 ff d2             	call   *%r10
  402738:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  40273d:	49 8b 41 20          	mov    0x20(%r9),%rax
  402741:	48 85 c0             	test   %rax,%rax
  402744:	41 ba a0 11 40 00    	mov    $0x4011a0,%r10d
  40274a:	4c 0f 45 d0          	cmovne %rax,%r10
  40274e:	bf 08 66 40 00       	mov    $0x406608,%edi
  402753:	be 11 00 00 00       	mov    $0x11,%esi
  402758:	ba b1 61 40 00       	mov    $0x4061b1,%edx
  40275d:	b9 26 00 00 00       	mov    $0x26,%ecx
  402762:	41 b8 00 62 40 00    	mov    $0x406200,%r8d
  402768:	41 ff d2             	call   *%r10
  40276b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402770 <main>:
  402770:	53                   	push   %rbx
  402771:	4c 63 cf             	movslq %edi,%r9
  402774:	45 85 c9             	test   %r9d,%r9d
  402777:	0f 88 d7 00 00 00    	js     402854 <main+0xe4>
  40277d:	48 89 35 cc 58 00 00 	mov    %rsi,0x58cc(%rip)        # 408050 <runtime::args__>
  402784:	4c 89 0d cd 58 00 00 	mov    %r9,0x58cd(%rip)        # 408058 <runtime::args__+0x8>
  40278b:	e8 40 ec ff ff       	call   4013d0 <__$startup_runtime>
  402790:	b8 09 00 00 00       	mov    $0x9,%eax
  402795:	be 00 e0 01 00       	mov    $0x1e000,%esi
  40279a:	ba 03 00 00 00       	mov    $0x3,%edx
  40279f:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4027a5:	31 ff                	xor    %edi,%edi
  4027a7:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4027ae:	45 31 c9             	xor    %r9d,%r9d
  4027b1:	0f 05                	syscall
  4027b3:	48 89 c3             	mov    %rax,%rbx
  4027b6:	b8 ba 00 00 00       	mov    $0xba,%eax
  4027bb:	0f 05                	syscall
  4027bd:	b8 09 00 00 00       	mov    $0x9,%eax
  4027c2:	be 00 10 02 00       	mov    $0x21000,%esi
  4027c7:	31 ff                	xor    %edi,%edi
  4027c9:	45 31 c9             	xor    %r9d,%r9d
  4027cc:	0f 05                	syscall
  4027ce:	c4 e2 79 23 05 c9 3e 	vpmovsxwd 0x3ec9(%rip),%xmm0        # 4066a0 <_IO_stdin_used+0x16a0>
  4027d5:	00 00 
  4027d7:	c5 fa 7f 43 10       	vmovdqu %xmm0,0x10(%rbx)
  4027dc:	c7 83 02 10 00 00 0f 	movl   $0xfe00ef0f,0x1002(%rbx)
  4027e3:	ef 00 fe 
  4027e6:	c5 fd 76 c0          	vpcmpeqd %ymm0,%ymm0,%ymm0
  4027ea:	c5 fe 7f 83 00 30 00 	vmovdqu %ymm0,0x3000(%rbx)
  4027f1:	00 
  4027f2:	48 b8 ff ff ff ff ff 	movabs $0x3ffffffffff,%rax
  4027f9:	03 00 00 
  4027fc:	48 89 83 20 30 00 00 	mov    %rax,0x3020(%rbx)
  402803:	c5 f8 28 05 85 3e 00 	vmovaps 0x3e85(%rip),%xmm0        # 406690 <_IO_stdin_used+0x1690>
  40280a:	00 
  40280b:	c5 f8 11 43 20       	vmovups %xmm0,0x20(%rbx)
  402810:	48 89 83 00 40 00 00 	mov    %rax,0x4000(%rbx)
  402817:	c4 e2 7d 18 05 e4 27 	vbroadcastss 0x27e4(%rip),%ymm0        # 405004 <_IO_stdin_used+0x4>
  40281e:	00 00 
  402820:	c5 fc 29 83 40 30 00 	vmovaps %ymm0,0x3040(%rbx)
  402827:	00 
  402828:	b8 09 00 00 00       	mov    $0x9,%eax
  40282d:	be 00 00 00 40       	mov    $0x40000000,%esi
  402832:	bf 00 00 00 00       	mov    $0x0,%edi
  402837:	45 31 c9             	xor    %r9d,%r9d
  40283a:	0f 05                	syscall
  40283c:	b8 09 00 00 00       	mov    $0x9,%eax
  402841:	31 ff                	xor    %edi,%edi
  402843:	45 31 c9             	xor    %r9d,%r9d
  402846:	0f 05                	syscall
  402848:	c5 f8 77             	vzeroupper
  40284b:	e8 50 ed ff ff       	call   4015a0 <__$cleanup_runtime>
  402850:	31 c0                	xor    %eax,%eax
  402852:	5b                   	pop    %rbx
  402853:	c3                   	ret
  402854:	bf f8 62 40 00       	mov    $0x4062f8,%edi
  402859:	be 2a 00 00 00       	mov    $0x2a,%esi
  40285e:	ba 36 00 00 00       	mov    $0x36,%edx
  402863:	b9 11 00 00 00       	mov    $0x11,%ecx
  402868:	45 31 c0             	xor    %r8d,%r8d
  40286b:	e8 c0 e8 ff ff       	call   401130 <runtime::multi_pointer_slice_handle_error>

0000000000402870 <runtime::print_string>:
  402870:	48 89 f2             	mov    %rsi,%rdx
  402873:	48 89 fe             	mov    %rdi,%rsi
  402876:	b8 01 00 00 00       	mov    $0x1,%eax
  40287b:	bf 02 00 00 00       	mov    $0x2,%edi
  402880:	0f 05                	syscall
  402882:	c3                   	ret
  402883:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40288a:	84 00 00 00 00 00 

0000000000402890 <runtime::arena_alloc>:
  402890:	55                   	push   %rbp
  402891:	41 57                	push   %r15
  402893:	41 56                	push   %r14
  402895:	41 55                	push   %r13
  402897:	41 54                	push   %r12
  402899:	53                   	push   %rbx
  40289a:	48 83 ec 38          	sub    $0x38,%rsp
  40289e:	49 89 cb             	mov    %rcx,%r11
  4028a1:	c4 e2 f8 f3 ca       	blsr   %rdx,%rax
  4028a6:	0f 85 52 02 00 00    	jne    402afe <runtime::arena_alloc+0x26e>
  4028ac:	49 89 f2             	mov    %rsi,%r10
  4028af:	48 85 f6             	test   %rsi,%rsi
  4028b2:	0f 84 96 00 00 00    	je     40294e <runtime::arena_alloc+0xbe>
  4028b8:	49 89 d4             	mov    %rdx,%r12
  4028bb:	49 89 fe             	mov    %rdi,%r14
  4028be:	48 8b 47 10          	mov    0x10(%rdi),%rax
  4028c2:	48 85 c0             	test   %rax,%rax
  4028c5:	74 37                	je     4028fe <runtime::arena_alloc+0x6e>
  4028c7:	48 8b 48 20          	mov    0x20(%rax),%rcx
  4028cb:	48 8b 70 18          	mov    0x18(%rax),%rsi
  4028cf:	48 01 ce             	add    %rcx,%rsi
  4028d2:	49 8d 7c 24 ff       	lea    -0x1(%r12),%rdi
  4028d7:	48 21 f7             	and    %rsi,%rdi
  4028da:	4c 89 e2             	mov    %r12,%rdx
  4028dd:	48 29 fa             	sub    %rdi,%rdx
  4028e0:	48 85 ff             	test   %rdi,%rdi
  4028e3:	48 0f 44 d7          	cmove  %rdi,%rdx
  4028e7:	4c 89 d7             	mov    %r10,%rdi
  4028ea:	48 01 d7             	add    %rdx,%rdi
  4028ed:	72 0f                	jb     4028fe <runtime::arena_alloc+0x6e>
  4028ef:	48 01 cf             	add    %rcx,%rdi
  4028f2:	72 0a                	jb     4028fe <runtime::arena_alloc+0x6e>
  4028f4:	48 3b 78 28          	cmp    0x28(%rax),%rdi
  4028f8:	0f 86 8f 01 00 00    	jbe    402a8d <runtime::arena_alloc+0x1fd>
  4028fe:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402903:	49 8b 46 28          	mov    0x28(%r14),%rax
  402907:	48 85 c0             	test   %rax,%rax
  40290a:	75 0d                	jne    402919 <runtime::arena_alloc+0x89>
  40290c:	49 c7 46 28 00 00 40 	movq   $0x400000,0x28(%r14)
  402913:	00 
  402914:	b8 00 00 40 00       	mov    $0x400000,%eax
  402919:	49 8d 5c 24 ff       	lea    -0x1(%r12),%rbx
  40291e:	48 89 d9             	mov    %rbx,%rcx
  402921:	4c 21 d1             	and    %r10,%rcx
  402924:	4c 89 e7             	mov    %r12,%rdi
  402927:	48 29 cf             	sub    %rcx,%rdi
  40292a:	48 85 c9             	test   %rcx,%rcx
  40292d:	48 0f 44 f9          	cmove  %rcx,%rdi
  402931:	4c 01 d7             	add    %r10,%rdi
  402934:	48 39 c7             	cmp    %rax,%rdi
  402937:	48 0f 46 f8          	cmovbe %rax,%rdi
  40293b:	4d 8b 2e             	mov    (%r14),%r13
  40293e:	4d 85 ed             	test   %r13,%r13
  402941:	4c 89 54 24 30       	mov    %r10,0x30(%rsp)
  402946:	74 16                	je     40295e <runtime::arena_alloc+0xce>
  402948:	49 8b 6e 08          	mov    0x8(%r14),%rbp
  40294c:	eb 27                	jmp    402975 <runtime::arena_alloc+0xe5>
  40294e:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402952:	c4 c1 78 11 00       	vmovups %xmm0,(%r8)
  402957:	31 c0                	xor    %eax,%eax
  402959:	e9 56 01 00 00       	jmp    402ab4 <runtime::arena_alloc+0x224>
  40295e:	49 c7 06 e0 1c 40 00 	movq   $0x401ce0,(%r14)
  402965:	41 bd e0 1c 40 00    	mov    $0x401ce0,%r13d
  40296b:	49 c7 46 08 00 00 00 	movq   $0x0,0x8(%r14)
  402972:	00 
  402973:	31 ed                	xor    %ebp,%ebp
  402975:	49 83 fc 31          	cmp    $0x31,%r12
  402979:	41 bf 30 00 00 00    	mov    $0x30,%r15d
  40297f:	4d 0f 43 fc          	cmovae %r12,%r15
  402983:	4c 01 ff             	add    %r15,%rdi
  402986:	49 83 fc 11          	cmp    $0x11,%r12
  40298a:	be 10 00 00 00       	mov    $0x10,%esi
  40298f:	49 0f 4d f4          	cmovge %r12,%rsi
  402993:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402997:	c5 f8 29 44 24 20    	vmovaps %xmm0,0x20(%rsp)
  40299d:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  4029a2:	4c 89 0c 24          	mov    %r9,(%rsp)
  4029a6:	4c 8d 4c 24 20       	lea    0x20(%rsp),%r9
  4029ab:	4c 89 ea             	mov    %r13,%rdx
  4029ae:	48 89 e9             	mov    %rbp,%rcx
  4029b1:	4d 89 d8             	mov    %r11,%r8
  4029b4:	e8 77 02 00 00       	call   402c30 <runtime::mem_alloc>
  4029b9:	84 c0                	test   %al,%al
  4029bb:	74 12                	je     4029cf <runtime::arena_alloc+0x13f>
  4029bd:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4029c2:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4029c6:	c5 f8 11 01          	vmovups %xmm0,(%rcx)
  4029ca:	e9 e5 00 00 00       	jmp    402ab4 <runtime::arena_alloc+0x224>
  4029cf:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4029d4:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4029d9:	48 01 d0             	add    %rdx,%rax
  4029dc:	4c 89 6a 08          	mov    %r13,0x8(%rdx)
  4029e0:	48 89 6a 10          	mov    %rbp,0x10(%rdx)
  4029e4:	49 01 d7             	add    %rdx,%r15
  4029e7:	4c 89 7a 18          	mov    %r15,0x18(%rdx)
  4029eb:	4c 29 f8             	sub    %r15,%rax
  4029ee:	48 89 42 28          	mov    %rax,0x28(%rdx)
  4029f2:	48 83 7a 20 00       	cmpq   $0x0,0x20(%rdx)
  4029f7:	0f 85 2c 01 00 00    	jne    402b29 <runtime::arena_alloc+0x299>
  4029fd:	48 83 3a 00          	cmpq   $0x0,(%rdx)
  402a01:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  402a06:	0f 85 50 01 00 00    	jne    402b5c <runtime::arena_alloc+0x2cc>
  402a0c:	49 8b 46 10          	mov    0x10(%r14),%rax
  402a10:	48 89 02             	mov    %rax,(%rdx)
  402a13:	49 89 56 10          	mov    %rdx,0x10(%r14)
  402a17:	48 8b 42 28          	mov    0x28(%rdx),%rax
  402a1b:	49 01 46 20          	add    %rax,0x20(%r14)
  402a1f:	4c 8b 52 20          	mov    0x20(%rdx),%r10
  402a23:	48 8b 7a 18          	mov    0x18(%rdx),%rdi
  402a27:	4c 01 d7             	add    %r10,%rdi
  402a2a:	48 21 fb             	and    %rdi,%rbx
  402a2d:	49 29 dc             	sub    %rbx,%r12
  402a30:	48 85 db             	test   %rbx,%rbx
  402a33:	4c 0f 44 e3          	cmove  %rbx,%r12
  402a37:	31 c9                	xor    %ecx,%ecx
  402a39:	4c 89 ce             	mov    %r9,%rsi
  402a3c:	4c 01 e6             	add    %r12,%rsi
  402a3f:	b0 01                	mov    $0x1,%al
  402a41:	73 0c                	jae    402a4f <runtime::arena_alloc+0x1bf>
  402a43:	45 31 db             	xor    %r11d,%r11d
  402a46:	31 f6                	xor    %esi,%esi
  402a48:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402a4d:	eb 4f                	jmp    402a9e <runtime::arena_alloc+0x20e>
  402a4f:	31 c9                	xor    %ecx,%ecx
  402a51:	49 01 f2             	add    %rsi,%r10
  402a54:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402a59:	73 07                	jae    402a62 <runtime::arena_alloc+0x1d2>
  402a5b:	45 31 db             	xor    %r11d,%r11d
  402a5e:	31 f6                	xor    %esi,%esi
  402a60:	eb 3c                	jmp    402a9e <runtime::arena_alloc+0x20e>
  402a62:	31 c9                	xor    %ecx,%ecx
  402a64:	41 bb 00 00 00 00    	mov    $0x0,%r11d
  402a6a:	be 00 00 00 00       	mov    $0x0,%esi
  402a6f:	4c 3b 52 28          	cmp    0x28(%rdx),%r10
  402a73:	77 29                	ja     402a9e <runtime::arena_alloc+0x20e>
  402a75:	4d 85 c9             	test   %r9,%r9
  402a78:	78 68                	js     402ae2 <runtime::arena_alloc+0x252>
  402a7a:	4c 01 e7             	add    %r12,%rdi
  402a7d:	4c 89 52 20          	mov    %r10,0x20(%rdx)
  402a81:	31 c9                	xor    %ecx,%ecx
  402a83:	31 c0                	xor    %eax,%eax
  402a85:	4d 89 cb             	mov    %r9,%r11
  402a88:	48 89 fe             	mov    %rdi,%rsi
  402a8b:	eb 11                	jmp    402a9e <runtime::arena_alloc+0x20e>
  402a8d:	4d 85 d2             	test   %r10,%r10
  402a90:	78 31                	js     402ac3 <runtime::arena_alloc+0x233>
  402a92:	48 01 d6             	add    %rdx,%rsi
  402a95:	48 89 78 20          	mov    %rdi,0x20(%rax)
  402a99:	31 c0                	xor    %eax,%eax
  402a9b:	4d 89 d3             	mov    %r10,%r11
  402a9e:	49 8b 56 10          	mov    0x10(%r14),%rdx
  402aa2:	48 8b 52 20          	mov    0x20(%rdx),%rdx
  402aa6:	48 29 ca             	sub    %rcx,%rdx
  402aa9:	49 01 56 18          	add    %rdx,0x18(%r14)
  402aad:	49 89 30             	mov    %rsi,(%r8)
  402ab0:	4d 89 58 08          	mov    %r11,0x8(%r8)
  402ab4:	48 83 c4 38          	add    $0x38,%rsp
  402ab8:	5b                   	pop    %rbx
  402ab9:	41 5c                	pop    %r12
  402abb:	41 5d                	pop    %r13
  402abd:	41 5e                	pop    %r14
  402abf:	41 5f                	pop    %r15
  402ac1:	5d                   	pop    %rbp
  402ac2:	c3                   	ret
  402ac3:	bf a1 5f 40 00       	mov    $0x405fa1,%edi
  402ac8:	be 3c 00 00 00       	mov    $0x3c,%esi
  402acd:	ba 5c 00 00 00       	mov    $0x5c,%edx
  402ad2:	b9 31 00 00 00       	mov    $0x31,%ecx
  402ad7:	45 31 c0             	xor    %r8d,%r8d
  402ada:	4d 89 d1             	mov    %r10,%r9
  402add:	e8 4e e6 ff ff       	call   401130 <runtime::multi_pointer_slice_handle_error>
  402ae2:	bf a1 5f 40 00       	mov    $0x405fa1,%edi
  402ae7:	be 3c 00 00 00       	mov    $0x3c,%esi
  402aec:	ba 5c 00 00 00       	mov    $0x5c,%edx
  402af1:	b9 31 00 00 00       	mov    $0x31,%ecx
  402af6:	45 31 c0             	xor    %r8d,%r8d
  402af9:	e8 32 e6 ff ff       	call   401130 <runtime::multi_pointer_slice_handle_error>
  402afe:	49 8b 41 20          	mov    0x20(%r9),%rax
  402b02:	48 85 c0             	test   %rax,%rax
  402b05:	41 ba a0 11 40 00    	mov    $0x4011a0,%r10d
  402b0b:	4c 0f 45 d0          	cmovne %rax,%r10
  402b0f:	bf 08 66 40 00       	mov    $0x406608,%edi
  402b14:	be 11 00 00 00       	mov    $0x11,%esi
  402b19:	ba 23 63 40 00       	mov    $0x406323,%edx
  402b1e:	b9 1a 00 00 00       	mov    $0x1a,%ecx
  402b23:	4d 89 d8             	mov    %r11,%r8
  402b26:	41 ff d2             	call   *%r10
  402b29:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  402b2e:	49 8b 41 20          	mov    0x20(%r9),%rax
  402b32:	48 85 c0             	test   %rax,%rax
  402b35:	41 ba a0 11 40 00    	mov    $0x4011a0,%r10d
  402b3b:	4c 0f 45 d0          	cmovne %rax,%r10
  402b3f:	bf 08 66 40 00       	mov    $0x406608,%edi
  402b44:	be 11 00 00 00       	mov    $0x11,%esi
  402b49:	ba 91 5f 40 00       	mov    $0x405f91,%edx
  402b4e:	b9 0f 00 00 00       	mov    $0xf,%ecx
  402b53:	41 b8 00 60 40 00    	mov    $0x406000,%r8d
  402b59:	41 ff d2             	call   *%r10
  402b5c:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  402b61:	49 8b 41 20          	mov    0x20(%r9),%rax
  402b65:	48 85 c0             	test   %rax,%rax
  402b68:	41 ba a0 11 40 00    	mov    $0x4011a0,%r10d
  402b6e:	4c 0f 45 d0          	cmovne %rax,%r10
  402b72:	bf 08 66 40 00       	mov    $0x406608,%edi
  402b77:	be 11 00 00 00       	mov    $0x11,%esi
  402b7c:	ba 28 60 40 00       	mov    $0x406028,%edx
  402b81:	b9 11 00 00 00       	mov    $0x11,%ecx
  402b86:	41 b8 40 60 40 00    	mov    $0x406040,%r8d
  402b8c:	41 ff d2             	call   *%r10
  402b8f:	90                   	nop

0000000000402b90 <runtime::print_byte>:
  402b90:	40 88 7c 24 f8       	mov    %dil,-0x8(%rsp)
  402b95:	48 8d 74 24 f8       	lea    -0x8(%rsp),%rsi
  402b9a:	b8 01 00 00 00       	mov    $0x1,%eax
  402b9f:	bf 02 00 00 00       	mov    $0x2,%edi
  402ba4:	ba 01 00 00 00       	mov    $0x1,%edx
  402ba9:	0f 05                	syscall
  402bab:	c3                   	ret
  402bac:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402bb0 <runtime::mem_alloc_bytes>:
  402bb0:	41 56                	push   %r14
  402bb2:	53                   	push   %rbx
  402bb3:	48 83 ec 18          	sub    $0x18,%rsp
  402bb7:	4c 89 c3             	mov    %r8,%rbx
  402bba:	49 89 ca             	mov    %rcx,%r10
  402bbd:	48 89 f0             	mov    %rsi,%rax
  402bc0:	48 85 ff             	test   %rdi,%rdi
  402bc3:	0f 94 c1             	sete   %cl
  402bc6:	48 85 f6             	test   %rsi,%rsi
  402bc9:	40 0f 94 c6          	sete   %sil
  402bcd:	40 08 ce             	or     %cl,%sil
  402bd0:	40 80 fe 01          	cmp    $0x1,%sil
  402bd4:	75 06                	jne    402bdc <runtime::mem_alloc_bytes+0x2c>
  402bd6:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402bda:	eb 3c                	jmp    402c18 <runtime::mem_alloc_bytes+0x68>
  402bdc:	4d 89 cb             	mov    %r9,%r11
  402bdf:	49 89 f8             	mov    %rdi,%r8
  402be2:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402be6:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  402beb:	48 83 ec 08          	sub    $0x8,%rsp
  402bef:	4c 8d 74 24 08       	lea    0x8(%rsp),%r14
  402bf4:	b9 01 00 00 00       	mov    $0x1,%ecx
  402bf9:	48 89 d7             	mov    %rdx,%rdi
  402bfc:	31 f6                	xor    %esi,%esi
  402bfe:	4c 89 c2             	mov    %r8,%rdx
  402c01:	45 31 c0             	xor    %r8d,%r8d
  402c04:	45 31 c9             	xor    %r9d,%r9d
  402c07:	41 53                	push   %r11
  402c09:	41 56                	push   %r14
  402c0b:	41 52                	push   %r10
  402c0d:	ff d0                	call   *%rax
  402c0f:	48 83 c4 20          	add    $0x20,%rsp
  402c13:	c5 f8 28 04 24       	vmovaps (%rsp),%xmm0
  402c18:	c5 f8 11 03          	vmovups %xmm0,(%rbx)
  402c1c:	48 83 c4 18          	add    $0x18,%rsp
  402c20:	5b                   	pop    %rbx
  402c21:	41 5e                	pop    %r14
  402c23:	c3                   	ret
  402c24:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402c2b:	00 00 00 00 00 

0000000000402c30 <runtime::mem_alloc>:
  402c30:	41 56                	push   %r14
  402c32:	53                   	push   %rbx
  402c33:	48 83 ec 18          	sub    $0x18,%rsp
  402c37:	4c 89 c0             	mov    %r8,%rax
  402c3a:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  402c3f:	48 85 f6             	test   %rsi,%rsi
  402c42:	7e 6e                	jle    402cb2 <runtime::mem_alloc+0x82>
  402c44:	49 89 f0             	mov    %rsi,%r8
  402c47:	f3 48 0f b8 f6       	popcnt %rsi,%rsi
  402c4c:	83 fe 02             	cmp    $0x2,%esi
  402c4f:	73 61                	jae    402cb2 <runtime::mem_alloc+0x82>
  402c51:	4c 89 cb             	mov    %r9,%rbx
  402c54:	49 89 d3             	mov    %rdx,%r11
  402c57:	48 89 fa             	mov    %rdi,%rdx
  402c5a:	48 85 ff             	test   %rdi,%rdi
  402c5d:	40 0f 94 c6          	sete   %sil
  402c61:	4d 85 db             	test   %r11,%r11
  402c64:	40 0f 94 c7          	sete   %dil
  402c68:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402c6c:	40 08 f7             	or     %sil,%dil
  402c6f:	74 08                	je     402c79 <runtime::mem_alloc+0x49>
  402c71:	c5 f8 11 03          	vmovups %xmm0,(%rbx)
  402c75:	31 c0                	xor    %eax,%eax
  402c77:	eb 31                	jmp    402caa <runtime::mem_alloc+0x7a>
  402c79:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  402c7e:	48 83 ec 08          	sub    $0x8,%rsp
  402c82:	4c 8d 74 24 08       	lea    0x8(%rsp),%r14
  402c87:	48 89 cf             	mov    %rcx,%rdi
  402c8a:	31 f6                	xor    %esi,%esi
  402c8c:	4c 89 c1             	mov    %r8,%rcx
  402c8f:	45 31 c0             	xor    %r8d,%r8d
  402c92:	45 31 c9             	xor    %r9d,%r9d
  402c95:	41 52                	push   %r10
  402c97:	41 56                	push   %r14
  402c99:	50                   	push   %rax
  402c9a:	41 ff d3             	call   *%r11
  402c9d:	48 83 c4 20          	add    $0x20,%rsp
  402ca1:	c5 f8 28 04 24       	vmovaps (%rsp),%xmm0
  402ca6:	c5 f8 11 03          	vmovups %xmm0,(%rbx)
  402caa:	48 83 c4 18          	add    $0x18,%rsp
  402cae:	5b                   	pop    %rbx
  402caf:	41 5e                	pop    %r14
  402cb1:	c3                   	ret
  402cb2:	49 8b 4a 20          	mov    0x20(%r10),%rcx
  402cb6:	48 85 c9             	test   %rcx,%rcx
  402cb9:	41 bb a0 11 40 00    	mov    $0x4011a0,%r11d
  402cbf:	4c 0f 45 d9          	cmovne %rcx,%r11
  402cc3:	bf 08 66 40 00       	mov    $0x406608,%edi
  402cc8:	be 11 00 00 00       	mov    $0x11,%esi
  402ccd:	ba 3e 63 40 00       	mov    $0x40633e,%edx
  402cd2:	b9 20 00 00 00       	mov    $0x20,%ecx
  402cd7:	49 89 c0             	mov    %rax,%r8
  402cda:	4d 89 d1             	mov    %r10,%r9
  402cdd:	41 ff d3             	call   *%r11

0000000000402ce0 <runtime::print_u64>:
  402ce0:	50                   	push   %rax
  402ce1:	48 89 fa             	mov    %rdi,%rdx
  402ce4:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402ce8:	c5 fc 11 44 24 e0    	vmovups %ymm0,-0x20(%rsp)
  402cee:	c5 fc 11 44 24 c0    	vmovups %ymm0,-0x40(%rsp)
  402cf4:	c5 fc 11 44 24 a0    	vmovups %ymm0,-0x60(%rsp)
  402cfa:	c5 fc 11 44 24 80    	vmovups %ymm0,-0x80(%rsp)
  402d00:	c6 04 24 00          	movb   $0x0,(%rsp)
  402d04:	b8 81 00 00 00       	mov    $0x81,%eax
  402d09:	48 83 ff 0a          	cmp    $0xa,%rdi
  402d0d:	72 48                	jb     402d57 <runtime::print_u64+0x77>
  402d0f:	be 81 00 00 00       	mov    $0x81,%esi
  402d14:	48 b9 cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rcx
  402d1b:	cc cc cc 
  402d1e:	66 90                	xchg   %ax,%ax
  402d20:	48 8d 46 ff          	lea    -0x1(%rsi),%rax
  402d24:	c4 e2 c3 f6 f9       	mulx   %rcx,%rdi,%rdi
  402d29:	48 c1 ef 03          	shr    $0x3,%rdi
  402d2d:	4c 8d 04 3f          	lea    (%rdi,%rdi,1),%r8
  402d31:	4f 8d 04 80          	lea    (%r8,%r8,4),%r8
  402d35:	49 f7 d8             	neg    %r8
  402d38:	46 0f b6 84 02 20 67 	movzbl 0x406720(%rdx,%r8,1),%r8d
  402d3f:	40 00 
  402d41:	44 88 84 34 7f ff ff 	mov    %r8b,-0x81(%rsp,%rsi,1)
  402d48:	ff 
  402d49:	48 89 c6             	mov    %rax,%rsi
  402d4c:	48 83 fa 63          	cmp    $0x63,%rdx
  402d50:	48 89 fa             	mov    %rdi,%rdx
  402d53:	77 cb                	ja     402d20 <runtime::print_u64+0x40>
  402d55:	eb 03                	jmp    402d5a <runtime::print_u64+0x7a>
  402d57:	48 89 d7             	mov    %rdx,%rdi
  402d5a:	48 8d 4c 24 80       	lea    -0x80(%rsp),%rcx
  402d5f:	48 8d 74 08 ff       	lea    -0x1(%rax,%rcx,1),%rsi
  402d64:	0f b6 97 20 67 40 00 	movzbl 0x406720(%rdi),%edx
  402d6b:	88 54 08 ff          	mov    %dl,-0x1(%rax,%rcx,1)
  402d6f:	ba 82 00 00 00       	mov    $0x82,%edx
  402d74:	48 29 c2             	sub    %rax,%rdx
  402d77:	b8 01 00 00 00       	mov    $0x1,%eax
  402d7c:	bf 02 00 00 00       	mov    $0x2,%edi
  402d81:	0f 05                	syscall
  402d83:	58                   	pop    %rax
  402d84:	c5 f8 77             	vzeroupper
  402d87:	c3                   	ret
  402d88:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  402d8f:	00 

0000000000402d90 <runtime::print_i64>:
  402d90:	50                   	push   %rax
  402d91:	48 89 fa             	mov    %rdi,%rdx
  402d94:	48 f7 da             	neg    %rdx
  402d97:	48 0f 48 d7          	cmovs  %rdi,%rdx
  402d9b:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402d9f:	c5 fc 11 44 24 e0    	vmovups %ymm0,-0x20(%rsp)
  402da5:	c5 fc 11 44 24 c0    	vmovups %ymm0,-0x40(%rsp)
  402dab:	c5 fc 11 44 24 a0    	vmovups %ymm0,-0x60(%rsp)
  402db1:	c5 fc 11 44 24 80    	vmovups %ymm0,-0x80(%rsp)
  402db7:	c6 04 24 00          	movb   $0x0,(%rsp)
  402dbb:	b8 81 00 00 00       	mov    $0x81,%eax
  402dc0:	48 83 fa 0a          	cmp    $0xa,%rdx
  402dc4:	72 51                	jb     402e17 <runtime::print_i64+0x87>
  402dc6:	be 81 00 00 00       	mov    $0x81,%esi
  402dcb:	48 b9 cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rcx
  402dd2:	cc cc cc 
  402dd5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402ddc:	00 00 00 00 
  402de0:	48 8d 46 ff          	lea    -0x1(%rsi),%rax
  402de4:	c4 62 bb f6 c1       	mulx   %rcx,%r8,%r8
  402de9:	49 c1 e8 03          	shr    $0x3,%r8
  402ded:	4f 8d 0c 00          	lea    (%r8,%r8,1),%r9
  402df1:	4f 8d 0c 89          	lea    (%r9,%r9,4),%r9
  402df5:	49 f7 d9             	neg    %r9
  402df8:	46 0f b6 8c 0a 20 67 	movzbl 0x406720(%rdx,%r9,1),%r9d
  402dff:	40 00 
  402e01:	44 88 8c 34 7f ff ff 	mov    %r9b,-0x81(%rsp,%rsi,1)
  402e08:	ff 
  402e09:	48 89 c6             	mov    %rax,%rsi
  402e0c:	48 83 fa 63          	cmp    $0x63,%rdx
  402e10:	4c 89 c2             	mov    %r8,%rdx
  402e13:	77 cb                	ja     402de0 <runtime::print_i64+0x50>
  402e15:	eb 03                	jmp    402e1a <runtime::print_i64+0x8a>
  402e17:	49 89 d0             	mov    %rdx,%r8
  402e1a:	41 0f b6 88 20 67 40 	movzbl 0x406720(%r8),%ecx
  402e21:	00 
  402e22:	88 8c 04 7f ff ff ff 	mov    %cl,-0x81(%rsp,%rax,1)
  402e29:	48 85 ff             	test   %rdi,%rdi
  402e2c:	78 05                	js     402e33 <runtime::print_i64+0xa3>
  402e2e:	48 ff c8             	dec    %rax
  402e31:	eb 0c                	jmp    402e3f <runtime::print_i64+0xaf>
  402e33:	c6 84 04 7e ff ff ff 	movb   $0x2d,-0x82(%rsp,%rax,1)
  402e3a:	2d 
  402e3b:	48 83 c0 fe          	add    $0xfffffffffffffffe,%rax
  402e3f:	48 8d 74 04 80       	lea    -0x80(%rsp,%rax,1),%rsi
  402e44:	ba 81 00 00 00       	mov    $0x81,%edx
  402e49:	48 29 c2             	sub    %rax,%rdx
  402e4c:	b8 01 00 00 00       	mov    $0x1,%eax
  402e51:	bf 02 00 00 00       	mov    $0x2,%edi
  402e56:	0f 05                	syscall
  402e58:	58                   	pop    %rax
  402e59:	c5 f8 77             	vzeroupper
  402e5c:	c3                   	ret
  402e5d:	0f 1f 00             	nopl   (%rax)

0000000000402e60 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>:
  402e60:	48 81 ec c8 02 00 00 	sub    $0x2c8,%rsp
  402e67:	80 bf 08 04 00 00 01 	cmpb   $0x1,0x408(%rdi)
  402e6e:	0f 85 46 06 00 00    	jne    4034ba <runtime::[random_generator_chacha8.odin]::chacha8rand_refill+0x65a>
  402e74:	c4 62 7d 58 97 e0 03 	vpbroadcastd 0x3e0(%rdi),%ymm10
  402e7b:	00 00 
  402e7d:	c4 e2 7d 58 a7 e4 03 	vpbroadcastd 0x3e4(%rdi),%ymm4
  402e84:	00 00 
  402e86:	c4 62 7d 58 b7 e8 03 	vpbroadcastd 0x3e8(%rdi),%ymm14
  402e8d:	00 00 
  402e8f:	c4 e2 7d 58 af ec 03 	vpbroadcastd 0x3ec(%rdi),%ymm5
  402e96:	00 00 
  402e98:	c4 62 7d 58 8f f0 03 	vpbroadcastd 0x3f0(%rdi),%ymm9
  402e9f:	00 00 
  402ea1:	c4 62 7d 58 af f4 03 	vpbroadcastd 0x3f4(%rdi),%ymm13
  402ea8:	00 00 
  402eaa:	c4 e2 7d 58 b7 f8 03 	vpbroadcastd 0x3f8(%rdi),%ymm6
  402eb1:	00 00 
  402eb3:	c4 e2 7d 58 bf fc 03 	vpbroadcastd 0x3fc(%rdi),%ymm7
  402eba:	00 00 
  402ebc:	b0 01                	mov    $0x1,%al
  402ebe:	c5 7d 6f 05 fa 37 00 	vmovdqa 0x37fa(%rip),%ymm8        # 4066c0 <_IO_stdin_used+0x16c0>
  402ec5:	00 
  402ec6:	c4 e2 7d 18 05 39 21 	vbroadcastss 0x2139(%rip),%ymm0        # 405008 <_IO_stdin_used+0x8>
  402ecd:	00 00 
  402ecf:	c5 fc 11 84 24 00 02 	vmovups %ymm0,0x200(%rsp)
  402ed6:	00 00 
  402ed8:	c4 e2 7d 18 05 2b 21 	vbroadcastss 0x212b(%rip),%ymm0        # 40500c <_IO_stdin_used+0xc>
  402edf:	00 00 
  402ee1:	c5 fc 11 84 24 e0 01 	vmovups %ymm0,0x1e0(%rsp)
  402ee8:	00 00 
  402eea:	c4 e2 7d 18 05 1d 21 	vbroadcastss 0x211d(%rip),%ymm0        # 405010 <_IO_stdin_used+0x10>
  402ef1:	00 00 
  402ef3:	c5 fc 11 84 24 c0 01 	vmovups %ymm0,0x1c0(%rsp)
  402efa:	00 00 
  402efc:	c4 e2 7d 18 05 0f 21 	vbroadcastss 0x210f(%rip),%ymm0        # 405014 <_IO_stdin_used+0x14>
  402f03:	00 00 
  402f05:	c5 fc 11 84 24 a0 01 	vmovups %ymm0,0x1a0(%rsp)
  402f0c:	00 00 
  402f0e:	c4 e2 7d 58 05 01 21 	vpbroadcastd 0x2101(%rip),%ymm0        # 405018 <_IO_stdin_used+0x18>
  402f15:	00 00 
  402f17:	c5 fe 7f 84 24 80 01 	vmovdqu %ymm0,0x180(%rsp)
  402f1e:	00 00 
  402f20:	48 89 f9             	mov    %rdi,%rcx
  402f23:	c5 7e 7f 94 24 60 01 	vmovdqu %ymm10,0x160(%rsp)
  402f2a:	00 00 
  402f2c:	c5 fe 7f a4 24 40 01 	vmovdqu %ymm4,0x140(%rsp)
  402f33:	00 00 
  402f35:	c5 7e 7f b4 24 20 01 	vmovdqu %ymm14,0x120(%rsp)
  402f3c:	00 00 
  402f3e:	c5 fe 7f ac 24 00 01 	vmovdqu %ymm5,0x100(%rsp)
  402f45:	00 00 
  402f47:	c5 7e 7f 8c 24 e0 00 	vmovdqu %ymm9,0xe0(%rsp)
  402f4e:	00 00 
  402f50:	c5 7e 7f ac 24 c0 00 	vmovdqu %ymm13,0xc0(%rsp)
  402f57:	00 00 
  402f59:	c5 fe 7f b4 24 40 02 	vmovdqu %ymm6,0x240(%rsp)
  402f60:	00 00 
  402f62:	c5 fe 7f bc 24 20 02 	vmovdqu %ymm7,0x220(%rsp)
  402f69:	00 00 
  402f6b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  402f70:	c5 e9 ef d2          	vpxor  %xmm2,%xmm2,%xmm2
  402f74:	ba 0a 00 00 00       	mov    $0xa,%edx
  402f79:	c5 7e 6f a4 24 a0 01 	vmovdqu 0x1a0(%rsp),%ymm12
  402f80:	00 00 
  402f82:	c5 7e 6f bc 24 c0 01 	vmovdqu 0x1c0(%rsp),%ymm15
  402f89:	00 00 
  402f8b:	c5 fe 6f 84 24 e0 01 	vmovdqu 0x1e0(%rsp),%ymm0
  402f92:	00 00 
  402f94:	c5 fc 10 8c 24 00 02 	vmovups 0x200(%rsp),%ymm1
  402f9b:	00 00 
  402f9d:	c5 7d 7f f3          	vmovdqa %ymm14,%ymm3
  402fa1:	c5 7d 6f f6          	vmovdqa %ymm6,%ymm14
  402fa5:	c5 fe 7f bc 24 80 00 	vmovdqu %ymm7,0x80(%rsp)
  402fac:	00 00 
  402fae:	c5 7e 7f 84 24 60 02 	vmovdqu %ymm8,0x260(%rsp)
  402fb5:	00 00 
  402fb7:	c4 41 21 ef db       	vpxor  %xmm11,%xmm11,%xmm11
  402fbc:	c5 c9 ef f6          	vpxor  %xmm6,%xmm6,%xmm6
  402fc0:	c5 fe 7f b4 24 a0 00 	vmovdqu %ymm6,0xa0(%rsp)
  402fc7:	00 00 
  402fc9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
  402fd0:	c5 fc 11 0c 24       	vmovups %ymm1,(%rsp)
  402fd5:	c4 41 2d fe e4       	vpaddd %ymm12,%ymm10,%ymm12
  402fda:	c4 41 3d ef c4       	vpxor  %ymm12,%ymm8,%ymm8
  402fdf:	c5 fd 6f 35 f9 36 00 	vmovdqa 0x36f9(%rip),%ymm6        # 4066e0 <_IO_stdin_used+0x16e0>
  402fe6:	00 
  402fe7:	c4 62 3d 00 c6       	vpshufb %ymm6,%ymm8,%ymm8
  402fec:	c5 fd 6f c8          	vmovdqa %ymm0,%ymm1
  402ff0:	c5 fd 6f c6          	vmovdqa %ymm6,%ymm0
  402ff4:	c4 41 3d fe c9       	vpaddd %ymm9,%ymm8,%ymm9
  402ff9:	c4 41 35 ef d2       	vpxor  %ymm10,%ymm9,%ymm10
  402ffe:	c4 c1 4d 72 d2 14    	vpsrld $0x14,%ymm10,%ymm6
  403004:	c4 c1 2d 72 f2 0c    	vpslld $0xc,%ymm10,%ymm10
  40300a:	c5 ad eb f6          	vpor   %ymm6,%ymm10,%ymm6
  40300e:	c5 1d fe e6          	vpaddd %ymm6,%ymm12,%ymm12
  403012:	c5 fd 6f 3d e6 36 00 	vmovdqa 0x36e6(%rip),%ymm7        # 406700 <_IO_stdin_used+0x1700>
  403019:	00 
  40301a:	c4 62 3d 00 c7       	vpshufb %ymm7,%ymm8,%ymm8
  40301f:	c4 62 1d 00 d7       	vpshufb %ymm7,%ymm12,%ymm10
  403024:	c4 41 2d ef c0       	vpxor  %ymm8,%ymm10,%ymm8
  403029:	c4 41 3d fe c9       	vpaddd %ymm9,%ymm8,%ymm9
  40302e:	c5 7e 7f 4c 24 40    	vmovdqu %ymm9,0x40(%rsp)
  403034:	c5 b5 ef f6          	vpxor  %ymm6,%ymm9,%ymm6
  403038:	c5 ad 72 d6 19       	vpsrld $0x19,%ymm6,%ymm10
  40303d:	c5 cd 72 f6 07       	vpslld $0x7,%ymm6,%ymm6
  403042:	c5 ad eb f6          	vpor   %ymm6,%ymm10,%ymm6
  403046:	c5 fe 7f 74 24 60    	vmovdqu %ymm6,0x60(%rsp)
  40304c:	c5 85 fe f4          	vpaddd %ymm4,%ymm15,%ymm6
  403050:	c5 ed ef d6          	vpxor  %ymm6,%ymm2,%ymm2
  403054:	c4 e2 6d 00 d0       	vpshufb %ymm0,%ymm2,%ymm2
  403059:	c5 7d 6f f8          	vmovdqa %ymm0,%ymm15
  40305d:	c5 15 fe ea          	vpaddd %ymm2,%ymm13,%ymm13
  403061:	c5 95 ef e4          	vpxor  %ymm4,%ymm13,%ymm4
  403065:	c5 ad 72 d4 14       	vpsrld $0x14,%ymm4,%ymm10
  40306a:	c5 dd 72 f4 0c       	vpslld $0xc,%ymm4,%ymm4
  40306f:	c5 ad eb e4          	vpor   %ymm4,%ymm10,%ymm4
  403073:	c5 5d fe d6          	vpaddd %ymm6,%ymm4,%ymm10
  403077:	c5 7d 6f cf          	vmovdqa %ymm7,%ymm9
  40307b:	c4 e2 6d 00 d7       	vpshufb %ymm7,%ymm2,%ymm2
  403080:	c4 e2 2d 00 f7       	vpshufb %ymm7,%ymm10,%ymm6
  403085:	c5 cd ef d2          	vpxor  %ymm2,%ymm6,%ymm2
  403089:	c5 95 fe f2          	vpaddd %ymm2,%ymm13,%ymm6
  40308d:	c5 fe 7f 74 24 20    	vmovdqu %ymm6,0x20(%rsp)
  403093:	c5 cd ef e4          	vpxor  %ymm4,%ymm6,%ymm4
  403097:	c5 cd 72 d4 19       	vpsrld $0x19,%ymm4,%ymm6
  40309c:	c5 dd 72 f4 07       	vpslld $0x7,%ymm4,%ymm4
  4030a1:	c5 dd eb e6          	vpor   %ymm6,%ymm4,%ymm4
  4030a5:	c5 e5 fe c1          	vpaddd %ymm1,%ymm3,%ymm0
  4030a9:	c5 a5 ef f0          	vpxor  %ymm0,%ymm11,%ymm6
  4030ad:	c4 41 7d 6f ef       	vmovdqa %ymm15,%ymm13
  4030b2:	c4 c2 4d 00 f7       	vpshufb %ymm15,%ymm6,%ymm6
  4030b7:	c5 0d fe fe          	vpaddd %ymm6,%ymm14,%ymm15
  4030bb:	c5 85 ef db          	vpxor  %ymm3,%ymm15,%ymm3
  4030bf:	c5 a5 72 d3 14       	vpsrld $0x14,%ymm3,%ymm11
  4030c4:	c5 e5 72 f3 0c       	vpslld $0xc,%ymm3,%ymm3
  4030c9:	c5 a5 eb db          	vpor   %ymm3,%ymm11,%ymm3
  4030cd:	c5 e5 fe c0          	vpaddd %ymm0,%ymm3,%ymm0
  4030d1:	c4 e2 4d 00 f7       	vpshufb %ymm7,%ymm6,%ymm6
  4030d6:	c4 62 7d 00 df       	vpshufb %ymm7,%ymm0,%ymm11
  4030db:	c5 25 ef de          	vpxor  %ymm6,%ymm11,%ymm11
  4030df:	c4 c1 25 fe f7       	vpaddd %ymm15,%ymm11,%ymm6
  4030e4:	c5 cd ef db          	vpxor  %ymm3,%ymm6,%ymm3
  4030e8:	c5 85 72 d3 19       	vpsrld $0x19,%ymm3,%ymm15
  4030ed:	c5 e5 72 f3 07       	vpslld $0x7,%ymm3,%ymm3
  4030f2:	c5 85 eb db          	vpor   %ymm3,%ymm15,%ymm3
  4030f6:	c5 d5 fe 0c 24       	vpaddd (%rsp),%ymm5,%ymm1
  4030fb:	c5 75 ef bc 24 a0 00 	vpxor  0xa0(%rsp),%ymm1,%ymm15
  403102:	00 00 
  403104:	c4 42 05 00 fd       	vpshufb %ymm13,%ymm15,%ymm15
  403109:	c5 85 fe bc 24 80 00 	vpaddd 0x80(%rsp),%ymm15,%ymm7
  403110:	00 00 
  403112:	c5 c5 ef ed          	vpxor  %ymm5,%ymm7,%ymm5
  403116:	c5 8d 72 d5 14       	vpsrld $0x14,%ymm5,%ymm14
  40311b:	c5 d5 72 f5 0c       	vpslld $0xc,%ymm5,%ymm5
  403120:	c5 8d eb ed          	vpor   %ymm5,%ymm14,%ymm5
  403124:	c5 d5 fe c9          	vpaddd %ymm1,%ymm5,%ymm1
  403128:	c4 42 05 00 f1       	vpshufb %ymm9,%ymm15,%ymm14
  40312d:	c4 42 75 00 f9       	vpshufb %ymm9,%ymm1,%ymm15
  403132:	c4 41 05 ef f6       	vpxor  %ymm14,%ymm15,%ymm14
  403137:	c5 8d fe ff          	vpaddd %ymm7,%ymm14,%ymm7
  40313b:	c5 c5 ef ed          	vpxor  %ymm5,%ymm7,%ymm5
  40313f:	c5 85 72 d5 19       	vpsrld $0x19,%ymm5,%ymm15
  403144:	c5 d5 72 f5 07       	vpslld $0x7,%ymm5,%ymm5
  403149:	c5 85 eb ed          	vpor   %ymm5,%ymm15,%ymm5
  40314d:	c5 1d fe e4          	vpaddd %ymm4,%ymm12,%ymm12
  403151:	c4 42 0d 00 f5       	vpshufb %ymm13,%ymm14,%ymm14
  403156:	c4 42 1d 00 fd       	vpshufb %ymm13,%ymm12,%ymm15
  40315b:	c4 41 05 ef f6       	vpxor  %ymm14,%ymm15,%ymm14
  403160:	c5 8d fe f6          	vpaddd %ymm6,%ymm14,%ymm6
  403164:	c5 cd ef e4          	vpxor  %ymm4,%ymm6,%ymm4
  403168:	c5 85 72 d4 14       	vpsrld $0x14,%ymm4,%ymm15
  40316d:	c5 dd 72 f4 0c       	vpslld $0xc,%ymm4,%ymm4
  403172:	c5 85 eb e4          	vpor   %ymm4,%ymm15,%ymm4
  403176:	c5 1d fe e4          	vpaddd %ymm4,%ymm12,%ymm12
  40317a:	c5 7e 7f 24 24       	vmovdqu %ymm12,(%rsp)
  40317f:	c4 42 0d 00 f1       	vpshufb %ymm9,%ymm14,%ymm14
  403184:	c4 42 1d 00 f9       	vpshufb %ymm9,%ymm12,%ymm15
  403189:	c4 41 05 ef f6       	vpxor  %ymm14,%ymm15,%ymm14
  40318e:	c5 7e 7f b4 24 a0 00 	vmovdqu %ymm14,0xa0(%rsp)
  403195:	00 00 
  403197:	c5 8d fe f6          	vpaddd %ymm6,%ymm14,%ymm6
  40319b:	c5 fe 7f b4 24 a0 02 	vmovdqu %ymm6,0x2a0(%rsp)
  4031a2:	00 00 
  4031a4:	c5 cd ef e4          	vpxor  %ymm4,%ymm6,%ymm4
  4031a8:	c5 cd 72 d4 19       	vpsrld $0x19,%ymm4,%ymm6
  4031ad:	c5 dd 72 f4 07       	vpslld $0x7,%ymm4,%ymm4
  4031b2:	c5 dd eb e6          	vpor   %ymm6,%ymm4,%ymm4
  4031b6:	c5 fe 7f a4 24 80 02 	vmovdqu %ymm4,0x280(%rsp)
  4031bd:	00 00 
  4031bf:	c5 ad fe f3          	vpaddd %ymm3,%ymm10,%ymm6
  4031c3:	c4 42 3d 00 c5       	vpshufb %ymm13,%ymm8,%ymm8
  4031c8:	c4 42 4d 00 d5       	vpshufb %ymm13,%ymm6,%ymm10
  4031cd:	c4 41 2d ef c0       	vpxor  %ymm8,%ymm10,%ymm8
  4031d2:	c5 bd fe ff          	vpaddd %ymm7,%ymm8,%ymm7
  4031d6:	c5 c5 ef db          	vpxor  %ymm3,%ymm7,%ymm3
  4031da:	c5 ad 72 d3 14       	vpsrld $0x14,%ymm3,%ymm10
  4031df:	c5 e5 72 f3 0c       	vpslld $0xc,%ymm3,%ymm3
  4031e4:	c5 ad eb db          	vpor   %ymm3,%ymm10,%ymm3
  4031e8:	c5 65 fe e6          	vpaddd %ymm6,%ymm3,%ymm12
  4031ec:	c4 c2 3d 00 f1       	vpshufb %ymm9,%ymm8,%ymm6
  4031f1:	c4 42 1d 00 c1       	vpshufb %ymm9,%ymm12,%ymm8
  4031f6:	c4 41 7d 6f f9       	vmovdqa %ymm9,%ymm15
  4031fb:	c5 3d ef c6          	vpxor  %ymm6,%ymm8,%ymm8
  4031ff:	c5 bd fe f7          	vpaddd %ymm7,%ymm8,%ymm6
  403203:	c5 fe 7f b4 24 80 00 	vmovdqu %ymm6,0x80(%rsp)
  40320a:	00 00 
  40320c:	c5 cd ef db          	vpxor  %ymm3,%ymm6,%ymm3
  403210:	c5 cd 72 d3 19       	vpsrld $0x19,%ymm3,%ymm6
  403215:	c5 e5 72 f3 07       	vpslld $0x7,%ymm3,%ymm3
  40321a:	c5 e5 eb de          	vpor   %ymm6,%ymm3,%ymm3
  40321e:	c5 d5 fe c0          	vpaddd %ymm0,%ymm5,%ymm0
  403222:	c4 c2 6d 00 d5       	vpshufb %ymm13,%ymm2,%ymm2
  403227:	c4 c2 7d 00 f5       	vpshufb %ymm13,%ymm0,%ymm6
  40322c:	c5 cd ef d2          	vpxor  %ymm2,%ymm6,%ymm2
  403230:	c5 ed fe 74 24 40    	vpaddd 0x40(%rsp),%ymm2,%ymm6
  403236:	c5 cd ef ed          	vpxor  %ymm5,%ymm6,%ymm5
  40323a:	c5 b5 72 d5 14       	vpsrld $0x14,%ymm5,%ymm9
  40323f:	c5 d5 72 f5 0c       	vpslld $0xc,%ymm5,%ymm5
  403244:	c5 b5 eb ed          	vpor   %ymm5,%ymm9,%ymm5
  403248:	c5 d5 fe c0          	vpaddd %ymm0,%ymm5,%ymm0
  40324c:	c4 c2 6d 00 d7       	vpshufb %ymm15,%ymm2,%ymm2
  403251:	c4 42 7d 00 cf       	vpshufb %ymm15,%ymm0,%ymm9
  403256:	c5 b5 ef d2          	vpxor  %ymm2,%ymm9,%ymm2
  40325a:	c5 6d fe ce          	vpaddd %ymm6,%ymm2,%ymm9
  40325e:	c5 b5 ef ed          	vpxor  %ymm5,%ymm9,%ymm5
  403262:	c5 cd 72 d5 19       	vpsrld $0x19,%ymm5,%ymm6
  403267:	c5 d5 72 f5 07       	vpslld $0x7,%ymm5,%ymm5
  40326c:	c5 d5 eb ee          	vpor   %ymm6,%ymm5,%ymm5
  403270:	c5 fe 6f 7c 24 60    	vmovdqu 0x60(%rsp),%ymm7
  403276:	c5 c5 fe c9          	vpaddd %ymm1,%ymm7,%ymm1
  40327a:	c4 c2 25 00 f5       	vpshufb %ymm13,%ymm11,%ymm6
  40327f:	c4 42 75 00 d5       	vpshufb %ymm13,%ymm1,%ymm10
  403284:	c5 ad ef f6          	vpxor  %ymm6,%ymm10,%ymm6
  403288:	c5 4d fe 54 24 20    	vpaddd 0x20(%rsp),%ymm6,%ymm10
  40328e:	c5 2d ef df          	vpxor  %ymm7,%ymm10,%ymm11
  403292:	c4 c1 15 72 d3 14    	vpsrld $0x14,%ymm11,%ymm13
  403298:	c4 c1 25 72 f3 0c    	vpslld $0xc,%ymm11,%ymm11
  40329e:	c4 41 25 eb f5       	vpor   %ymm13,%ymm11,%ymm14
  4032a3:	c5 8d fe c9          	vpaddd %ymm1,%ymm14,%ymm1
  4032a7:	c4 c2 4d 00 f7       	vpshufb %ymm15,%ymm6,%ymm6
  4032ac:	c4 42 75 00 df       	vpshufb %ymm15,%ymm1,%ymm11
  4032b1:	c4 41 7d 6f fc       	vmovdqa %ymm12,%ymm15
  4032b6:	c5 fe 6f a4 24 80 02 	vmovdqu 0x280(%rsp),%ymm4
  4032bd:	00 00 
  4032bf:	c5 25 ef de          	vpxor  %ymm6,%ymm11,%ymm11
  4032c3:	c4 41 25 fe ea       	vpaddd %ymm10,%ymm11,%ymm13
  4032c8:	c4 c1 15 ef f6       	vpxor  %ymm14,%ymm13,%ymm6
  4032cd:	c5 7e 6f b4 24 a0 02 	vmovdqu 0x2a0(%rsp),%ymm14
  4032d4:	00 00 
  4032d6:	c5 7e 6f 24 24       	vmovdqu (%rsp),%ymm12
  4032db:	c5 ad 72 d6 19       	vpsrld $0x19,%ymm6,%ymm10
  4032e0:	c5 cd 72 f6 07       	vpslld $0x7,%ymm6,%ymm6
  4032e5:	c5 2d eb d6          	vpor   %ymm6,%ymm10,%ymm10
  4032e9:	48 83 c2 fe          	add    $0xfffffffffffffffe,%rdx
  4032ed:	48 83 fa 02          	cmp    $0x2,%rdx
  4032f1:	0f 87 d9 fc ff ff    	ja     402fd0 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill+0x170>
  4032f7:	c4 c3 1d 38 f7 01    	vinserti128 $0x1,%xmm15,%ymm12,%ymm6
  4032fd:	c5 fe 7f 31          	vmovdqu %ymm6,(%rcx)
  403301:	c4 e3 7d 38 f1 01    	vinserti128 $0x1,%xmm1,%ymm0,%ymm6
  403307:	c5 fe 7f 71 20       	vmovdqu %ymm6,0x20(%rcx)
  40330c:	c5 ad fe b4 24 60 01 	vpaddd 0x160(%rsp),%ymm10,%ymm6
  403313:	00 00 
  403315:	c5 fe 7f 74 24 40    	vmovdqu %ymm6,0x40(%rsp)
  40331b:	c5 dd fe a4 24 40 01 	vpaddd 0x140(%rsp),%ymm4,%ymm4
  403322:	00 00 
  403324:	c5 fe 7f 64 24 60    	vmovdqu %ymm4,0x60(%rsp)
  40332a:	c4 63 4d 38 d4 01    	vinserti128 $0x1,%xmm4,%ymm6,%ymm10
  403330:	c5 7e 7f 51 40       	vmovdqu %ymm10,0x40(%rcx)
  403335:	c5 e5 fe 9c 24 20 01 	vpaddd 0x120(%rsp),%ymm3,%ymm3
  40333c:	00 00 
  40333e:	c5 fe 7f 5c 24 20    	vmovdqu %ymm3,0x20(%rsp)
  403344:	c5 d5 fe ac 24 00 01 	vpaddd 0x100(%rsp),%ymm5,%ymm5
  40334b:	00 00 
  40334d:	c4 63 65 38 d5 01    	vinserti128 $0x1,%xmm5,%ymm3,%ymm10
  403353:	c5 7e 7f 51 60       	vmovdqu %ymm10,0x60(%rcx)
  403358:	c5 35 fe 8c 24 e0 00 	vpaddd 0xe0(%rsp),%ymm9,%ymm9
  40335f:	00 00 
  403361:	c5 15 fe 94 24 c0 00 	vpaddd 0xc0(%rsp),%ymm13,%ymm10
  403368:	00 00 
  40336a:	c4 43 35 38 ea 01    	vinserti128 $0x1,%xmm10,%ymm9,%ymm13
  403370:	c5 7e 7f a9 80 00 00 	vmovdqu %ymm13,0x80(%rcx)
  403377:	00 
  403378:	c5 fe 6f b4 24 40 02 	vmovdqu 0x240(%rsp),%ymm6
  40337f:	00 00 
  403381:	c5 0d fe ee          	vpaddd %ymm6,%ymm14,%ymm13
  403385:	c5 fe 6f a4 24 20 02 	vmovdqu 0x220(%rsp),%ymm4
  40338c:	00 00 
  40338e:	c5 dd fe bc 24 80 00 	vpaddd 0x80(%rsp),%ymm4,%ymm7
  403395:	00 00 
  403397:	c4 63 15 38 f7 01    	vinserti128 $0x1,%xmm7,%ymm13,%ymm14
  40339d:	c5 7e 7f b1 a0 00 00 	vmovdqu %ymm14,0xa0(%rcx)
  4033a4:	00 
  4033a5:	c4 63 3d 38 f2 01    	vinserti128 $0x1,%xmm2,%ymm8,%ymm14
  4033ab:	c5 7e 7f b1 c0 00 00 	vmovdqu %ymm14,0xc0(%rcx)
  4033b2:	00 
  4033b3:	c5 fe 6f 9c 24 a0 00 	vmovdqu 0xa0(%rsp),%ymm3
  4033ba:	00 00 
  4033bc:	c4 63 25 38 f3 01    	vinserti128 $0x1,%xmm3,%ymm11,%ymm14
  4033c2:	c5 7e 7f b1 e0 00 00 	vmovdqu %ymm14,0xe0(%rcx)
  4033c9:	00 
  4033ca:	c4 43 1d 46 e7 31    	vperm2i128 $0x31,%ymm15,%ymm12,%ymm12
  4033d0:	c5 7e 7f a1 00 01 00 	vmovdqu %ymm12,0x100(%rcx)
  4033d7:	00 
  4033d8:	c4 e3 7d 46 c1 31    	vperm2i128 $0x31,%ymm1,%ymm0,%ymm0
  4033de:	c5 fe 7f 81 20 01 00 	vmovdqu %ymm0,0x120(%rcx)
  4033e5:	00 
  4033e6:	c5 fc 10 44 24 40    	vmovups 0x40(%rsp),%ymm0
  4033ec:	c4 e3 7d 06 44 24 60 	vperm2f128 $0x31,0x60(%rsp),%ymm0,%ymm0
  4033f3:	31 
  4033f4:	c5 fc 11 81 40 01 00 	vmovups %ymm0,0x140(%rcx)
  4033fb:	00 
  4033fc:	c4 e3 55 46 44 24 20 	vperm2i128 $0x13,0x20(%rsp),%ymm5,%ymm0
  403403:	13 
  403404:	c5 fe 6f ac 24 00 01 	vmovdqu 0x100(%rsp),%ymm5
  40340b:	00 00 
  40340d:	c5 7e 6f b4 24 20 01 	vmovdqu 0x120(%rsp),%ymm14
  403414:	00 00 
  403416:	c5 fe 7f 81 60 01 00 	vmovdqu %ymm0,0x160(%rcx)
  40341d:	00 
  40341e:	c4 c3 35 46 c2 31    	vperm2i128 $0x31,%ymm10,%ymm9,%ymm0
  403424:	c5 7e 6f 8c 24 e0 00 	vmovdqu 0xe0(%rsp),%ymm9
  40342b:	00 00 
  40342d:	c5 7e 6f 94 24 60 01 	vmovdqu 0x160(%rsp),%ymm10
  403434:	00 00 
  403436:	c5 fe 7f 81 80 01 00 	vmovdqu %ymm0,0x180(%rcx)
  40343d:	00 
  40343e:	c4 e3 15 46 c7 31    	vperm2i128 $0x31,%ymm7,%ymm13,%ymm0
  403444:	c5 fd 6f fc          	vmovdqa %ymm4,%ymm7
  403448:	c5 fe 6f a4 24 40 01 	vmovdqu 0x140(%rsp),%ymm4
  40344f:	00 00 
  403451:	c5 7e 6f ac 24 c0 00 	vmovdqu 0xc0(%rsp),%ymm13
  403458:	00 00 
  40345a:	c5 fe 7f 81 a0 01 00 	vmovdqu %ymm0,0x1a0(%rcx)
  403461:	00 
  403462:	c4 e3 3d 46 c2 31    	vperm2i128 $0x31,%ymm2,%ymm8,%ymm0
  403468:	c5 fe 7f 81 c0 01 00 	vmovdqu %ymm0,0x1c0(%rcx)
  40346f:	00 
  403470:	c4 e3 25 46 c3 31    	vperm2i128 $0x31,%ymm3,%ymm11,%ymm0
  403476:	c5 fe 7f 81 e0 01 00 	vmovdqu %ymm0,0x1e0(%rcx)
  40347d:	00 
  40347e:	c5 7e 6f 84 24 60 02 	vmovdqu 0x260(%rsp),%ymm8
  403485:	00 00 
  403487:	c5 3d fe 84 24 80 01 	vpaddd 0x180(%rsp),%ymm8,%ymm8
  40348e:	00 00 
  403490:	48 81 c1 00 02 00 00 	add    $0x200,%rcx
  403497:	a8 01                	test   $0x1,%al
  403499:	b8 00 00 00 00       	mov    $0x0,%eax
  40349e:	0f 85 cc fa ff ff    	jne    402f70 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill+0x110>
  4034a4:	48 c7 87 00 04 00 00 	movq   $0x0,0x400(%rdi)
  4034ab:	00 00 00 00 
  4034af:	48 81 c4 c8 02 00 00 	add    $0x2c8,%rsp
  4034b6:	c5 f8 77             	vzeroupper
  4034b9:	c3                   	ret
  4034ba:	49 89 f1             	mov    %rsi,%r9
  4034bd:	48 8b 46 20          	mov    0x20(%rsi),%rax
  4034c1:	48 85 c0             	test   %rax,%rax
  4034c4:	41 ba a0 11 40 00    	mov    $0x4011a0,%r10d
  4034ca:	4c 0f 45 d0          	cmovne %rax,%r10
  4034ce:	bf 08 66 40 00       	mov    $0x406608,%edi
  4034d3:	be 11 00 00 00       	mov    $0x11,%esi
  4034d8:	ba 18 64 40 00       	mov    $0x406418,%edx
  4034dd:	b9 20 00 00 00       	mov    $0x20,%ecx
  4034e2:	41 b8 50 64 40 00    	mov    $0x406450,%r8d
  4034e8:	41 ff d2             	call   *%r10
  4034eb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004034f0 <runtime::mem_free>:
  4034f0:	48 85 f6             	test   %rsi,%rsi
  4034f3:	74 3e                	je     403533 <runtime::mem_free+0x43>
  4034f5:	53                   	push   %rbx
  4034f6:	48 83 ec 10          	sub    $0x10,%rsp
  4034fa:	4d 89 c3             	mov    %r8,%r11
  4034fd:	49 89 ca             	mov    %rcx,%r10
  403500:	48 89 f0             	mov    %rsi,%rax
  403503:	49 89 f8             	mov    %rdi,%r8
  403506:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40350a:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  40350f:	48 83 ec 08          	sub    $0x8,%rsp
  403513:	48 8d 5c 24 08       	lea    0x8(%rsp),%rbx
  403518:	48 89 d7             	mov    %rdx,%rdi
  40351b:	be 01 00 00 00       	mov    $0x1,%esi
  403520:	31 d2                	xor    %edx,%edx
  403522:	31 c9                	xor    %ecx,%ecx
  403524:	45 31 c9             	xor    %r9d,%r9d
  403527:	41 53                	push   %r11
  403529:	53                   	push   %rbx
  40352a:	41 52                	push   %r10
  40352c:	ff d0                	call   *%rax
  40352e:	48 83 c4 30          	add    $0x30,%rsp
  403532:	5b                   	pop    %rbx
  403533:	c3                   	ret
  403534:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40353b:	00 00 00 00 00 

0000000000403540 <runtime::mem_free_with_size>:
  403540:	4d 89 cb             	mov    %r9,%r11
  403543:	48 89 d0             	mov    %rdx,%rax
  403546:	49 89 f1             	mov    %rsi,%r9
  403549:	48 85 ff             	test   %rdi,%rdi
  40354c:	0f 94 c2             	sete   %dl
  40354f:	48 85 c0             	test   %rax,%rax
  403552:	40 0f 94 c6          	sete   %sil
  403556:	40 08 d6             	or     %dl,%sil
  403559:	40 80 fe 01          	cmp    $0x1,%sil
  40355d:	74 35                	je     403594 <runtime::mem_free_with_size+0x54>
  40355f:	53                   	push   %rbx
  403560:	48 83 ec 10          	sub    $0x10,%rsp
  403564:	4d 89 c2             	mov    %r8,%r10
  403567:	49 89 f8             	mov    %rdi,%r8
  40356a:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40356e:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  403573:	48 83 ec 08          	sub    $0x8,%rsp
  403577:	48 8d 5c 24 08       	lea    0x8(%rsp),%rbx
  40357c:	48 89 cf             	mov    %rcx,%rdi
  40357f:	be 01 00 00 00       	mov    $0x1,%esi
  403584:	31 d2                	xor    %edx,%edx
  403586:	31 c9                	xor    %ecx,%ecx
  403588:	41 53                	push   %r11
  40358a:	53                   	push   %rbx
  40358b:	41 52                	push   %r10
  40358d:	ff d0                	call   *%rax
  40358f:	48 83 c4 30          	add    $0x30,%rsp
  403593:	5b                   	pop    %rbx
  403594:	c3                   	ret
  403595:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40359c:	00 00 00 00 

00000000004035a0 <runtime::print_caller_location>:
  4035a0:	53                   	push   %rbx
  4035a1:	48 89 fb             	mov    %rdi,%rbx
  4035a4:	48 8b 3f             	mov    (%rdi),%rdi
  4035a7:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  4035ab:	e8 c0 f2 ff ff       	call   402870 <runtime::print_string>
  4035b0:	bf 28 00 00 00       	mov    $0x28,%edi
  4035b5:	e8 d6 f5 ff ff       	call   402b90 <runtime::print_byte>
  4035ba:	48 63 7b 10          	movslq 0x10(%rbx),%rdi
  4035be:	e8 1d f7 ff ff       	call   402ce0 <runtime::print_u64>
  4035c3:	83 7b 14 00          	cmpl   $0x0,0x14(%rbx)
  4035c7:	74 13                	je     4035dc <runtime::print_caller_location+0x3c>
  4035c9:	bf 3a 00 00 00       	mov    $0x3a,%edi
  4035ce:	e8 bd f5 ff ff       	call   402b90 <runtime::print_byte>
  4035d3:	48 63 7b 14          	movslq 0x14(%rbx),%rdi
  4035d7:	e8 04 f7 ff ff       	call   402ce0 <runtime::print_u64>
  4035dc:	bf 29 00 00 00       	mov    $0x29,%edi
  4035e1:	5b                   	pop    %rbx
  4035e2:	e9 a9 f5 ff ff       	jmp    402b90 <runtime::print_byte>
  4035e7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4035ee:	00 00 

00000000004035f0 <os::[file_linux.odin]::_file_stream_proc>:
  4035f0:	55                   	push   %rbp
  4035f1:	41 57                	push   %r15
  4035f3:	41 56                	push   %r14
  4035f5:	41 55                	push   %r13
  4035f7:	41 54                	push   %r12
  4035f9:	53                   	push   %rbx
  4035fa:	48 81 ec 68 01 00 00 	sub    $0x168,%rsp
  403601:	48 83 fe 0a          	cmp    $0xa,%rsi
  403605:	0f 87 07 04 00 00    	ja     403a12 <os::[file_linux.odin]::_file_stream_proc+0x422>
  40360b:	4c 8b bc 24 b8 01 00 	mov    0x1b8(%rsp),%r15
  403612:	00 
  403613:	ff 24 f5 f0 50 40 00 	jmp    *0x4050f0(,%rsi,8)
  40361a:	48 85 ff             	test   %rdi,%rdi
  40361d:	0f 84 26 03 00 00    	je     403949 <os::[file_linux.odin]::_file_stream_proc+0x359>
  403623:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  40362a:	00 00 00 
  40362d:	48 89 fd             	mov    %rdi,%rbp
  403630:	48 63 7f 28          	movslq 0x28(%rdi),%rdi
  403634:	b8 03 00 00 00       	mov    $0x3,%eax
  403639:	0f 05                	syscall
  40363b:	49 89 c6             	mov    %rax,%r14
  40363e:	41 83 fe f7          	cmp    $0xfffffff7,%r14d
  403642:	0f 84 76 04 00 00    	je     403abe <os::[file_linux.odin]::_file_stream_proc+0x4ce>
  403648:	4c 8b 65 30          	mov    0x30(%rbp),%r12
  40364c:	4c 8b 6d 38          	mov    0x38(%rbp),%r13
  403650:	48 8b 7d 18          	mov    0x18(%rbp),%rdi
  403654:	48 8b 75 20          	mov    0x20(%rbp),%rsi
  403658:	41 b8 90 63 40 00    	mov    $0x406390,%r8d
  40365e:	4c 89 e2             	mov    %r12,%rdx
  403661:	4c 89 e9             	mov    %r13,%rcx
  403664:	4d 89 f9             	mov    %r15,%r9
  403667:	e8 d4 fe ff ff       	call   403540 <runtime::mem_free_with_size>
  40366c:	48 8b 7d 40          	mov    0x40(%rbp),%rdi
  403670:	48 8b 75 48          	mov    0x48(%rbp),%rsi
  403674:	41 b8 c0 63 40 00    	mov    $0x4063c0,%r8d
  40367a:	4c 89 e2             	mov    %r12,%rdx
  40367d:	4c 89 e9             	mov    %r13,%rcx
  403680:	4d 89 f9             	mov    %r15,%r9
  403683:	e8 b8 fe ff ff       	call   403540 <runtime::mem_free_with_size>
  403688:	b9 f0 63 40 00       	mov    $0x4063f0,%ecx
  40368d:	48 89 ef             	mov    %rbp,%rdi
  403690:	4c 89 e6             	mov    %r12,%rsi
  403693:	4c 89 ea             	mov    %r13,%rdx
  403696:	4d 89 f8             	mov    %r15,%r8
  403699:	e8 52 fe ff ff       	call   4034f0 <runtime::mem_free>
  40369e:	41 8d 46 26          	lea    0x26(%r14),%eax
  4036a2:	83 f8 26             	cmp    $0x26,%eax
  4036a5:	0f 87 57 04 00 00    	ja     403b02 <os::[file_linux.odin]::_file_stream_proc+0x512>
  4036ab:	ff 24 c5 b0 53 40 00 	jmp    *0x4053b0(,%rax,8)
  4036b2:	45 89 f6             	mov    %r14d,%r14d
  4036b5:	4c 89 f3             	mov    %r14,%rbx
  4036b8:	e9 28 07 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  4036bd:	48 85 c9             	test   %rcx,%rcx
  4036c0:	0f 8e 83 02 00 00    	jle    403949 <os::[file_linux.odin]::_file_stream_proc+0x359>
  4036c6:	48 bb 10 00 00 00 02 	movabs $0x200000010,%rbx
  4036cd:	00 00 00 
  4036d0:	4d 85 c0             	test   %r8,%r8
  4036d3:	0f 88 00 04 00 00    	js     403ad9 <os::[file_linux.odin]::_file_stream_proc+0x4e9>
  4036d9:	48 63 7f 28          	movslq 0x28(%rdi),%rdi
  4036dd:	48 81 f9 00 00 00 40 	cmp    $0x40000000,%rcx
  4036e4:	41 b9 00 00 00 40    	mov    $0x40000000,%r9d
  4036ea:	4c 0f 42 c9          	cmovb  %rcx,%r9
  4036ee:	b8 11 00 00 00       	mov    $0x11,%eax
  4036f3:	48 89 d6             	mov    %rdx,%rsi
  4036f6:	4c 89 ca             	mov    %r9,%rdx
  4036f9:	4d 89 c2             	mov    %r8,%r10
  4036fc:	0f 05                	syscall
  4036fe:	49 89 c6             	mov    %rax,%r14
  403701:	44 89 f1             	mov    %r14d,%ecx
  403704:	f7 d9                	neg    %ecx
  403706:	48 c1 f8 3f          	sar    $0x3f,%rax
  40370a:	21 c8                	and    %ecx,%eax
  40370c:	83 f8 26             	cmp    $0x26,%eax
  40370f:	0f 87 dc 04 00 00    	ja     403bf1 <os::[file_linux.odin]::_file_stream_proc+0x601>
  403715:	ff 24 c5 f0 5a 40 00 	jmp    *0x405af0(,%rax,8)
  40371c:	48 83 c3 f1          	add    $0xfffffffffffffff1,%rbx
  403720:	31 c0                	xor    %eax,%eax
  403722:	4d 85 f6             	test   %r14,%r14
  403725:	4c 0f 4e f0          	cmovle %rax,%r14
  403729:	48 0f 4f d8          	cmovg  %rax,%rbx
  40372d:	e9 b3 06 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403732:	41 be ff 03 00 00    	mov    $0x3ff,%r14d
  403738:	e9 0f 02 00 00       	jmp    40394c <os::[file_linux.odin]::_file_stream_proc+0x35c>
  40373d:	48 63 7f 28          	movslq 0x28(%rdi),%rdi
  403741:	b8 4a 00 00 00       	mov    $0x4a,%eax
  403746:	0f 05                	syscall
  403748:	8d 48 26             	lea    0x26(%rax),%ecx
  40374b:	83 f9 26             	cmp    $0x26,%ecx
  40374e:	0f 87 8e 03 00 00    	ja     403ae2 <os::[file_linux.odin]::_file_stream_proc+0x4f2>
  403754:	ff 24 cd e8 54 40 00 	jmp    *0x4054e8(,%rcx,8)
  40375b:	48 bb 10 00 00 00 02 	movabs $0x200000010,%rbx
  403762:	00 00 00 
  403765:	e9 78 06 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  40376a:	48 85 c9             	test   %rcx,%rcx
  40376d:	0f 8e d6 01 00 00    	jle    403949 <os::[file_linux.odin]::_file_stream_proc+0x359>
  403773:	48 63 7f 28          	movslq 0x28(%rdi),%rdi
  403777:	48 81 f9 00 00 00 40 	cmp    $0x40000000,%rcx
  40377e:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  403784:	4c 0f 42 c1          	cmovb  %rcx,%r8
  403788:	45 31 f6             	xor    %r14d,%r14d
  40378b:	31 c0                	xor    %eax,%eax
  40378d:	48 89 d6             	mov    %rdx,%rsi
  403790:	4c 89 c2             	mov    %r8,%rdx
  403793:	0f 05                	syscall
  403795:	89 c2                	mov    %eax,%edx
  403797:	f7 da                	neg    %edx
  403799:	48 89 c1             	mov    %rax,%rcx
  40379c:	48 c1 f9 3f          	sar    $0x3f,%rcx
  4037a0:	21 d1                	and    %edx,%ecx
  4037a2:	83 f9 26             	cmp    $0x26,%ecx
  4037a5:	0f 87 47 03 00 00    	ja     403af2 <os::[file_linux.odin]::_file_stream_proc+0x502>
  4037ab:	48 bb 10 00 00 00 02 	movabs $0x200000010,%rbx
  4037b2:	00 00 00 
  4037b5:	ff 24 cd 28 5c 40 00 	jmp    *0x405c28(,%rcx,8)
  4037bc:	48 83 c3 f0          	add    $0xfffffffffffffff0,%rbx
  4037c0:	31 c9                	xor    %ecx,%ecx
  4037c2:	31 d2                	xor    %edx,%edx
  4037c4:	48 85 c0             	test   %rax,%rax
  4037c7:	48 0f 4e c1          	cmovle %rcx,%rax
  4037cb:	0f 9e c2             	setle  %dl
  4037ce:	48 0f 4f d9          	cmovg  %rcx,%rbx
  4037d2:	48 09 d3             	or     %rdx,%rbx
  4037d5:	49 89 c6             	mov    %rax,%r14
  4037d8:	e9 08 06 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  4037dd:	48 bb 10 00 00 00 02 	movabs $0x200000010,%rbx
  4037e4:	00 00 00 
  4037e7:	49 83 f9 02          	cmp    $0x2,%r9
  4037eb:	0f 87 52 02 00 00    	ja     403a43 <os::[file_linux.odin]::_file_stream_proc+0x453>
  4037f1:	48 63 7f 28          	movslq 0x28(%rdi),%rdi
  4037f5:	b8 08 00 00 00       	mov    $0x8,%eax
  4037fa:	4c 89 c6             	mov    %r8,%rsi
  4037fd:	4c 89 ca             	mov    %r9,%rdx
  403800:	0f 05                	syscall
  403802:	89 c1                	mov    %eax,%ecx
  403804:	f7 d9                	neg    %ecx
  403806:	48 89 c2             	mov    %rax,%rdx
  403809:	48 c1 fa 3f          	sar    $0x3f,%rdx
  40380d:	21 d1                	and    %edx,%ecx
  40380f:	83 f9 26             	cmp    $0x26,%ecx
  403812:	0f 87 da 02 00 00    	ja     403af2 <os::[file_linux.odin]::_file_stream_proc+0x502>
  403818:	ff 24 cd 58 57 40 00 	jmp    *0x405758(,%rcx,8)
  40381f:	c4 62 e8 f2 f0       	andn   %rax,%rdx,%r14
  403824:	e9 23 01 00 00       	jmp    40394c <os::[file_linux.odin]::_file_stream_proc+0x35c>
  403829:	48 85 c9             	test   %rcx,%rcx
  40382c:	0f 8e 17 01 00 00    	jle    403949 <os::[file_linux.odin]::_file_stream_proc+0x359>
  403832:	49 b9 01 00 00 00 01 	movabs $0x100000001,%r9
  403839:	00 00 00 
  40383c:	31 db                	xor    %ebx,%ebx
  40383e:	4d 8d 51 02          	lea    0x2(%r9),%r10
  403842:	45 31 f6             	xor    %r14d,%r14d
  403845:	49 89 ff             	mov    %rdi,%r15
  403848:	48 63 7f 28          	movslq 0x28(%rdi),%rdi
  40384c:	48 81 f9 00 00 00 40 	cmp    $0x40000000,%rcx
  403853:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  403859:	49 89 cd             	mov    %rcx,%r13
  40385c:	4c 0f 42 c1          	cmovb  %rcx,%r8
  403860:	b8 01 00 00 00       	mov    $0x1,%eax
  403865:	49 89 d4             	mov    %rdx,%r12
  403868:	48 89 d6             	mov    %rdx,%rsi
  40386b:	4c 89 c2             	mov    %r8,%rdx
  40386e:	0f 05                	syscall
  403870:	89 c1                	mov    %eax,%ecx
  403872:	f7 d9                	neg    %ecx
  403874:	48 89 c2             	mov    %rax,%rdx
  403877:	48 c1 fa 3f          	sar    $0x3f,%rdx
  40387b:	21 d1                	and    %edx,%ecx
  40387d:	0f 85 9e 01 00 00    	jne    403a21 <os::[file_linux.odin]::_file_stream_proc+0x431>
  403883:	c4 62 e8 f2 c0       	andn   %rax,%rdx,%r8
  403888:	4c 89 e9             	mov    %r13,%rcx
  40388b:	4c 39 e8             	cmp    %r13,%rax
  40388e:	0f 8f 1a 09 00 00    	jg     4041ae <os::[file_linux.odin]::_file_stream_proc+0xbbe>
  403894:	4c 89 e2             	mov    %r12,%rdx
  403897:	4c 01 c2             	add    %r8,%rdx
  40389a:	4d 01 c6             	add    %r8,%r14
  40389d:	4c 29 c1             	sub    %r8,%rcx
  4038a0:	4c 89 ff             	mov    %r15,%rdi
  4038a3:	7f a0                	jg     403845 <os::[file_linux.odin]::_file_stream_proc+0x255>
  4038a5:	e9 3b 05 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  4038aa:	49 bf 10 00 00 00 02 	movabs $0x200000010,%r15
  4038b1:	00 00 00 
  4038b4:	4d 85 c0             	test   %r8,%r8
  4038b7:	0f 88 ab 01 00 00    	js     403a68 <os::[file_linux.odin]::_file_stream_proc+0x478>
  4038bd:	48 85 c9             	test   %rcx,%rcx
  4038c0:	0f 8e 83 00 00 00    	jle    403949 <os::[file_linux.odin]::_file_stream_proc+0x359>
  4038c6:	48 b8 ff ff ff ff 02 	movabs $0x2ffffffff,%rax
  4038cd:	00 00 00 
  4038d0:	31 db                	xor    %ebx,%ebx
  4038d2:	48 83 c0 02          	add    $0x2,%rax
  4038d6:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4038db:	45 31 f6             	xor    %r14d,%r14d
  4038de:	48 89 fd             	mov    %rdi,%rbp
  4038e1:	48 63 7f 28          	movslq 0x28(%rdi),%rdi
  4038e5:	48 81 f9 00 00 00 40 	cmp    $0x40000000,%rcx
  4038ec:	41 b9 00 00 00 40    	mov    $0x40000000,%r9d
  4038f2:	49 89 cd             	mov    %rcx,%r13
  4038f5:	4c 0f 42 c9          	cmovb  %rcx,%r9
  4038f9:	b8 12 00 00 00       	mov    $0x12,%eax
  4038fe:	49 89 d4             	mov    %rdx,%r12
  403901:	48 89 d6             	mov    %rdx,%rsi
  403904:	4c 89 ca             	mov    %r9,%rdx
  403907:	4d 89 c2             	mov    %r8,%r10
  40390a:	0f 05                	syscall
  40390c:	89 c1                	mov    %eax,%ecx
  40390e:	f7 d9                	neg    %ecx
  403910:	48 89 c2             	mov    %rax,%rdx
  403913:	48 c1 fa 3f          	sar    $0x3f,%rdx
  403917:	21 d1                	and    %edx,%ecx
  403919:	0f 85 58 01 00 00    	jne    403a77 <os::[file_linux.odin]::_file_stream_proc+0x487>
  40391f:	c4 62 e8 f2 d0       	andn   %rax,%rdx,%r10
  403924:	4c 89 e9             	mov    %r13,%rcx
  403927:	4c 39 e8             	cmp    %r13,%rax
  40392a:	0f 8f 9e 08 00 00    	jg     4041ce <os::[file_linux.odin]::_file_stream_proc+0xbde>
  403930:	4c 89 e2             	mov    %r12,%rdx
  403933:	4c 01 d2             	add    %r10,%rdx
  403936:	4d 01 d6             	add    %r10,%r14
  403939:	4d 01 d0             	add    %r10,%r8
  40393c:	4c 29 d1             	sub    %r10,%rcx
  40393f:	48 89 ef             	mov    %rbp,%rdi
  403942:	7f 9a                	jg     4038de <os::[file_linux.odin]::_file_stream_proc+0x2ee>
  403944:	e9 9c 04 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403949:	45 31 f6             	xor    %r14d,%r14d
  40394c:	31 db                	xor    %ebx,%ebx
  40394e:	e9 92 04 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403953:	48 bb 10 00 00 00 02 	movabs $0x200000010,%rbx
  40395a:	00 00 00 
  40395d:	45 31 f6             	xor    %r14d,%r14d
  403960:	48 83 f9 60          	cmp    $0x60,%rcx
  403964:	0f 8c e2 00 00 00    	jl     403a4c <os::[file_linux.odin]::_file_stream_proc+0x45c>
  40396a:	48 8d 84 24 a0 01 00 	lea    0x1a0(%rsp),%rax
  403971:	00 
  403972:	48 8b 08             	mov    (%rax),%rcx
  403975:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40397a:	48 8b 68 08          	mov    0x8(%rax),%rbp
  40397e:	48 8b 07             	mov    (%rdi),%rax
  403981:	44 8b 48 28          	mov    0x28(%rax),%r9d
  403985:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  403989:	c5 fc 11 84 24 c0 00 	vmovups %ymm0,0xc0(%rsp)
  403990:	00 00 
  403992:	c5 fc 11 84 24 b0 00 	vmovups %ymm0,0xb0(%rsp)
  403999:	00 00 
  40399b:	c5 fc 11 84 24 90 00 	vmovups %ymm0,0x90(%rsp)
  4039a2:	00 00 
  4039a4:	c5 fc 11 44 24 70    	vmovups %ymm0,0x70(%rsp)
  4039aa:	c5 fc 11 44 24 50    	vmovups %ymm0,0x50(%rsp)
  4039b0:	49 63 f9             	movslq %r9d,%rdi
  4039b3:	48 8d 74 24 50       	lea    0x50(%rsp),%rsi
  4039b8:	b8 05 00 00 00       	mov    $0x5,%eax
  4039bd:	0f 05                	syscall
  4039bf:	8d 48 26             	lea    0x26(%rax),%ecx
  4039c2:	83 f9 26             	cmp    $0x26,%ecx
  4039c5:	0f 87 98 01 00 00    	ja     403b63 <os::[file_linux.odin]::_file_stream_proc+0x573>
  4039cb:	ff 24 cd 48 51 40 00 	jmp    *0x405148(,%rcx,8)
  4039d2:	45 31 e4             	xor    %r12d,%r12d
  4039d5:	31 f6                	xor    %esi,%esi
  4039d7:	45 31 c0             	xor    %r8d,%r8d
  4039da:	31 ff                	xor    %edi,%edi
  4039dc:	31 c0                	xor    %eax,%eax
  4039de:	31 c9                	xor    %ecx,%ecx
  4039e0:	e9 a9 03 00 00       	jmp    403d8e <os::[file_linux.odin]::_file_stream_proc+0x79e>
  4039e5:	48 63 7f 28          	movslq 0x28(%rdi),%rdi
  4039e9:	48 8d 74 24 50       	lea    0x50(%rsp),%rsi
  4039ee:	b8 05 00 00 00       	mov    $0x5,%eax
  4039f3:	0f 05                	syscall
  4039f5:	8d 48 26             	lea    0x26(%rax),%ecx
  4039f8:	83 f9 26             	cmp    $0x26,%ecx
  4039fb:	0f 87 e1 00 00 00    	ja     403ae2 <os::[file_linux.odin]::_file_stream_proc+0x4f2>
  403a01:	48 bb 10 00 00 00 02 	movabs $0x200000010,%rbx
  403a08:	00 00 00 
  403a0b:	ff 24 cd 20 56 40 00 	jmp    *0x405620(,%rcx,8)
  403a12:	48 bb ff ff ff ff 02 	movabs $0x2ffffffff,%rbx
  403a19:	00 00 00 
  403a1c:	e9 c1 03 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403a21:	8d 41 ff             	lea    -0x1(%rcx),%eax
  403a24:	83 f8 25             	cmp    $0x25,%eax
  403a27:	0f 87 25 01 00 00    	ja     403b52 <os::[file_linux.odin]::_file_stream_proc+0x562>
  403a2d:	ff 24 c5 c0 59 40 00 	jmp    *0x4059c0(,%rax,8)
  403a34:	48 bb 10 00 00 00 02 	movabs $0x200000010,%rbx
  403a3b:	00 00 00 
  403a3e:	e9 a2 03 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403a43:	48 83 c3 f7          	add    $0xfffffffffffffff7,%rbx
  403a47:	e9 96 03 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403a4c:	48 83 c3 f5          	add    $0xfffffffffffffff5,%rbx
  403a50:	e9 90 03 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403a55:	48 bb ff ff ff ff 02 	movabs $0x2ffffffff,%rbx
  403a5c:	00 00 00 
  403a5f:	48 83 c3 02          	add    $0x2,%rbx
  403a63:	e9 7a 03 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403a68:	49 83 c7 f8          	add    $0xfffffffffffffff8,%r15
  403a6c:	45 31 f6             	xor    %r14d,%r14d
  403a6f:	4c 89 fb             	mov    %r15,%rbx
  403a72:	e9 6e 03 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403a77:	8d 41 ff             	lea    -0x1(%rcx),%eax
  403a7a:	83 f8 25             	cmp    $0x25,%eax
  403a7d:	0f 87 42 01 00 00    	ja     403bc5 <os::[file_linux.odin]::_file_stream_proc+0x5d5>
  403a83:	ff 24 c5 90 58 40 00 	jmp    *0x405890(,%rax,8)
  403a8a:	4c 89 fb             	mov    %r15,%rbx
  403a8d:	e9 53 03 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403a92:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403a99:	00 00 00 
  403a9c:	e9 41 03 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403aa1:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403aa8:	00 00 00 
  403aab:	48 83 c3 03          	add    $0x3,%rbx
  403aaf:	e9 2e 03 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403ab4:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403abb:	00 00 00 
  403abe:	48 83 cb 04          	or     $0x4,%rbx
  403ac2:	e9 1b 03 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403ac7:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403ace:	00 00 00 
  403ad1:	48 ff c3             	inc    %rbx
  403ad4:	e9 09 03 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403ad9:	48 83 c3 f8          	add    $0xfffffffffffffff8,%rbx
  403add:	e9 00 03 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403ae2:	83 f8 92             	cmp    $0xffffff92,%eax
  403ae5:	0f 84 0f 01 00 00    	je     403bfa <os::[file_linux.odin]::_file_stream_proc+0x60a>
  403aeb:	f7 d8                	neg    %eax
  403aed:	e9 e3 02 00 00       	jmp    403dd5 <os::[file_linux.odin]::_file_stream_proc+0x7e5>
  403af2:	83 f9 6e             	cmp    $0x6e,%ecx
  403af5:	0f 84 ff 00 00 00    	je     403bfa <os::[file_linux.odin]::_file_stream_proc+0x60a>
  403afb:	89 c8                	mov    %ecx,%eax
  403afd:	e9 d3 02 00 00       	jmp    403dd5 <os::[file_linux.odin]::_file_stream_proc+0x7e5>
  403b02:	41 83 fe 92          	cmp    $0xffffff92,%r14d
  403b06:	0f 84 53 ff ff ff    	je     403a5f <os::[file_linux.odin]::_file_stream_proc+0x46f>
  403b0c:	49 f7 de             	neg    %r14
  403b0f:	44 89 f0             	mov    %r14d,%eax
  403b12:	e9 be 02 00 00       	jmp    403dd5 <os::[file_linux.odin]::_file_stream_proc+0x7e5>
  403b17:	48 bb ff ff ff ff 02 	movabs $0x2ffffffff,%rbx
  403b1e:	00 00 00 
  403b21:	e9 bf 02 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403b26:	41 89 c6             	mov    %eax,%r14d
  403b29:	4c 89 f3             	mov    %r14,%rbx
  403b2c:	e9 b4 02 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403b31:	b8 00 f0 00 00       	mov    $0xf000,%eax
  403b36:	23 44 24 68          	and    0x68(%rsp),%eax
  403b3a:	3d 00 80 00 00       	cmp    $0x8000,%eax
  403b3f:	0f 85 e5 01 00 00    	jne    403d2a <os::[file_linux.odin]::_file_stream_proc+0x73a>
  403b45:	4c 8b b4 24 80 00 00 	mov    0x80(%rsp),%r14
  403b4c:	00 
  403b4d:	e9 fa fd ff ff       	jmp    40394c <os::[file_linux.odin]::_file_stream_proc+0x35c>
  403b52:	83 f9 6e             	cmp    $0x6e,%ecx
  403b55:	0f 85 82 00 00 00    	jne    403bdd <os::[file_linux.odin]::_file_stream_proc+0x5ed>
  403b5b:	4c 89 d3             	mov    %r10,%rbx
  403b5e:	e9 82 02 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403b63:	83 f8 92             	cmp    $0xffffff92,%eax
  403b66:	75 49                	jne    403bb1 <os::[file_linux.odin]::_file_stream_proc+0x5c1>
  403b68:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403b6f:	00 00 00 
  403b72:	48 83 cb 02          	or     $0x2,%rbx
  403b76:	e9 02 02 00 00       	jmp    403d7d <os::[file_linux.odin]::_file_stream_proc+0x78d>
  403b7b:	48 bb ff ff ff ff 02 	movabs $0x2ffffffff,%rbx
  403b82:	00 00 00 
  403b85:	48 83 c3 02          	add    $0x2,%rbx
  403b89:	e9 57 02 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403b8e:	49 83 c1 03          	add    $0x3,%r9
  403b92:	4c 89 cb             	mov    %r9,%rbx
  403b95:	e9 4b 02 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403b9a:	49 83 c1 04          	add    $0x4,%r9
  403b9e:	4c 89 cb             	mov    %r9,%rbx
  403ba1:	e9 3f 02 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403ba6:	49 ff c1             	inc    %r9
  403ba9:	4c 89 cb             	mov    %r9,%rbx
  403bac:	e9 34 02 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403bb1:	f7 d8                	neg    %eax
  403bb3:	48 bb 00 00 00 00 04 	movabs $0x400000000,%rbx
  403bba:	00 00 00 
  403bbd:	48 09 c3             	or     %rax,%rbx
  403bc0:	e9 b8 01 00 00       	jmp    403d7d <os::[file_linux.odin]::_file_stream_proc+0x78d>
  403bc5:	83 f9 6e             	cmp    $0x6e,%ecx
  403bc8:	75 13                	jne    403bdd <os::[file_linux.odin]::_file_stream_proc+0x5ed>
  403bca:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403bd1:	00 00 00 
  403bd4:	48 83 cb 02          	or     $0x2,%rbx
  403bd8:	e9 08 02 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403bdd:	89 c8                	mov    %ecx,%eax
  403bdf:	48 bb 00 00 00 00 04 	movabs $0x400000000,%rbx
  403be6:	00 00 00 
  403be9:	48 09 c3             	or     %rax,%rbx
  403bec:	e9 f4 01 00 00       	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403bf1:	83 f8 6e             	cmp    $0x6e,%eax
  403bf4:	0f 85 d9 01 00 00    	jne    403dd3 <os::[file_linux.odin]::_file_stream_proc+0x7e3>
  403bfa:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403c01:	00 00 00 
  403c04:	48 83 cb 02          	or     $0x2,%rbx
  403c08:	e9 d5 01 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403c0d:	48 bb ff ff ff ff 02 	movabs $0x2ffffffff,%rbx
  403c14:	00 00 00 
  403c17:	48 83 c3 02          	add    $0x2,%rbx
  403c1b:	e9 5d 01 00 00       	jmp    403d7d <os::[file_linux.odin]::_file_stream_proc+0x78d>
  403c20:	44 8b 74 24 68       	mov    0x68(%rsp),%r14d
  403c25:	44 89 f0             	mov    %r14d,%eax
  403c28:	25 00 f0 00 00       	and    $0xf000,%eax
  403c2d:	05 00 f0 ff ff       	add    $0xfffff000,%eax
  403c32:	41 ba 01 00 00 00    	mov    $0x1,%r10d
  403c38:	3d ff bf 00 00       	cmp    $0xbfff,%eax
  403c3d:	77 0a                	ja     403c49 <os::[file_linux.odin]::_file_stream_proc+0x659>
  403c3f:	c1 e8 09             	shr    $0x9,%eax
  403c42:	4c 8b 90 20 66 40 00 	mov    0x406620(%rax),%r10
  403c49:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  403c4d:	c5 f8 11 44 24 3e    	vmovups %xmm0,0x3e(%rsp)
  403c53:	66 c7 44 24 4e 00 00 	movw   $0x0,0x4e(%rsp)
  403c5a:	48 b8 2f 70 72 6f 63 	movabs $0x65732f636f72702f,%rax
  403c61:	2f 73 65 
  403c64:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403c69:	48 b8 73 65 6c 66 2f 	movabs $0x2f64662f666c6573,%rax
  403c70:	66 64 2f 
  403c73:	48 89 44 24 36       	mov    %rax,0x36(%rsp)
  403c78:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  403c7c:	c5 fc 11 84 24 40 01 	vmovups %ymm0,0x140(%rsp)
  403c83:	00 00 
  403c85:	c5 fc 11 84 24 20 01 	vmovups %ymm0,0x120(%rsp)
  403c8c:	00 00 
  403c8e:	c5 fc 11 84 24 00 01 	vmovups %ymm0,0x100(%rsp)
  403c95:	00 00 
  403c97:	c5 fc 11 84 24 e0 00 	vmovups %ymm0,0xe0(%rsp)
  403c9e:	00 00 
  403ca0:	c6 84 24 60 01 00 00 	movb   $0x0,0x160(%rsp)
  403ca7:	00 
  403ca8:	44 89 c8             	mov    %r9d,%eax
  403cab:	f7 d8                	neg    %eax
  403cad:	41 0f 48 c1          	cmovs  %r9d,%eax
  403cb1:	83 f8 0a             	cmp    $0xa,%eax
  403cb4:	0f 82 90 01 00 00    	jb     403e4a <os::[file_linux.odin]::_file_stream_proc+0x85a>
  403cba:	48 89 d1             	mov    %rdx,%rcx
  403cbd:	41 bc 80 00 00 00    	mov    $0x80,%r12d
  403cc3:	48 be cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rsi
  403cca:	cc cc cc 
  403ccd:	49 81 fc 81 00 00 00 	cmp    $0x81,%r12
  403cd4:	0f 83 17 05 00 00    	jae    4041f1 <os::[file_linux.odin]::_file_stream_proc+0xc01>
  403cda:	48 89 c2             	mov    %rax,%rdx
  403cdd:	c4 e2 eb f6 d6       	mulx   %rsi,%rdx,%rdx
  403ce2:	48 c1 ea 03          	shr    $0x3,%rdx
  403ce6:	48 8d 3c 12          	lea    (%rdx,%rdx,1),%rdi
  403cea:	48 8d 3c bf          	lea    (%rdi,%rdi,4),%rdi
  403cee:	48 f7 df             	neg    %rdi
  403cf1:	0f b6 bc 38 20 67 40 	movzbl 0x406720(%rax,%rdi,1),%edi
  403cf8:	00 
  403cf9:	42 88 bc 24 e0 00 00 	mov    %dil,0xe0(%rsp,%r12,1)
  403d00:	00 
  403d01:	49 ff cc             	dec    %r12
  403d04:	48 83 f8 63          	cmp    $0x63,%rax
  403d08:	48 89 d0             	mov    %rdx,%rax
  403d0b:	77 c0                	ja     403ccd <os::[file_linux.odin]::_file_stream_proc+0x6dd>
  403d0d:	49 81 fc 81 00 00 00 	cmp    $0x81,%r12
  403d14:	0f 83 30 05 00 00    	jae    40424a <os::[file_linux.odin]::_file_stream_proc+0xc5a>
  403d1a:	4d 8d 44 24 01       	lea    0x1(%r12),%r8
  403d1f:	48 89 d0             	mov    %rdx,%rax
  403d22:	48 89 ca             	mov    %rcx,%rdx
  403d25:	e9 2c 01 00 00       	jmp    403e56 <os::[file_linux.odin]::_file_stream_proc+0x866>
  403d2a:	48 ff cb             	dec    %rbx
  403d2d:	e9 b0 00 00 00       	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403d32:	45 31 e4             	xor    %r12d,%r12d
  403d35:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403d3c:	00 00 00 
  403d3f:	eb 3f                	jmp    403d80 <os::[file_linux.odin]::_file_stream_proc+0x790>
  403d41:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403d48:	00 00 00 
  403d4b:	48 83 cb 04          	or     $0x4,%rbx
  403d4f:	eb 2c                	jmp    403d7d <os::[file_linux.odin]::_file_stream_proc+0x78d>
  403d51:	45 31 e4             	xor    %r12d,%r12d
  403d54:	48 bb ff ff ff ff 02 	movabs $0x2ffffffff,%rbx
  403d5b:	00 00 00 
  403d5e:	eb 20                	jmp    403d80 <os::[file_linux.odin]::_file_stream_proc+0x790>
  403d60:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403d67:	00 00 00 
  403d6a:	48 83 c3 03          	add    $0x3,%rbx
  403d6e:	eb 0d                	jmp    403d7d <os::[file_linux.odin]::_file_stream_proc+0x78d>
  403d70:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403d77:	00 00 00 
  403d7a:	48 ff c3             	inc    %rbx
  403d7d:	45 31 e4             	xor    %r12d,%r12d
  403d80:	31 f6                	xor    %esi,%esi
  403d82:	45 31 c0             	xor    %r8d,%r8d
  403d85:	31 ff                	xor    %edi,%edi
  403d87:	31 c0                	xor    %eax,%eax
  403d89:	31 c9                	xor    %ecx,%ecx
  403d8b:	45 31 f6             	xor    %r14d,%r14d
  403d8e:	45 31 c9             	xor    %r9d,%r9d
  403d91:	45 31 d2             	xor    %r10d,%r10d
  403d94:	45 31 db             	xor    %r11d,%r11d
  403d97:	4c 89 22             	mov    %r12,(%rdx)
  403d9a:	48 89 72 08          	mov    %rsi,0x8(%rdx)
  403d9e:	4c 89 42 10          	mov    %r8,0x10(%rdx)
  403da2:	48 89 7a 18          	mov    %rdi,0x18(%rdx)
  403da6:	48 89 42 20          	mov    %rax,0x20(%rdx)
  403daa:	48 c7 42 28 00 00 00 	movq   $0x0,0x28(%rdx)
  403db1:	00 
  403db2:	48 89 4a 30          	mov    %rcx,0x30(%rdx)
  403db6:	44 89 72 38          	mov    %r14d,0x38(%rdx)
  403dba:	c7 42 3c 00 00 00 00 	movl   $0x0,0x3c(%rdx)
  403dc1:	4c 89 4a 40          	mov    %r9,0x40(%rdx)
  403dc5:	4c 89 52 48          	mov    %r10,0x48(%rdx)
  403dc9:	4c 89 52 50          	mov    %r10,0x50(%rdx)
  403dcd:	4c 89 5a 58          	mov    %r11,0x58(%rdx)
  403dd1:	eb 0f                	jmp    403de2 <os::[file_linux.odin]::_file_stream_proc+0x7f2>
  403dd3:	89 c0                	mov    %eax,%eax
  403dd5:	48 bb 00 00 00 00 04 	movabs $0x400000000,%rbx
  403ddc:	00 00 00 
  403ddf:	48 09 c3             	or     %rax,%rbx
  403de2:	45 31 f6             	xor    %r14d,%r14d
  403de5:	48 8b 84 24 b0 01 00 	mov    0x1b0(%rsp),%rax
  403dec:	00 
  403ded:	4c 89 30             	mov    %r14,(%rax)
  403df0:	48 89 d8             	mov    %rbx,%rax
  403df3:	48 81 c4 68 01 00 00 	add    $0x168,%rsp
  403dfa:	5b                   	pop    %rbx
  403dfb:	41 5c                	pop    %r12
  403dfd:	41 5d                	pop    %r13
  403dff:	41 5e                	pop    %r14
  403e01:	41 5f                	pop    %r15
  403e03:	5d                   	pop    %rbp
  403e04:	c5 f8 77             	vzeroupper
  403e07:	c3                   	ret
  403e08:	48 8b 5c 24 10       	mov    0x10(%rsp),%rbx
  403e0d:	eb d6                	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403e0f:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403e16:	00 00 00 
  403e19:	48 83 c3 03          	add    $0x3,%rbx
  403e1d:	eb c6                	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403e1f:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403e26:	00 00 00 
  403e29:	eb ba                	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403e2b:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403e32:	00 00 00 
  403e35:	48 83 cb 04          	or     $0x4,%rbx
  403e39:	eb aa                	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403e3b:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  403e42:	00 00 00 
  403e45:	48 ff c3             	inc    %rbx
  403e48:	eb 9b                	jmp    403de5 <os::[file_linux.odin]::_file_stream_proc+0x7f5>
  403e4a:	41 bc 80 00 00 00    	mov    $0x80,%r12d
  403e50:	41 b8 81 00 00 00    	mov    $0x81,%r8d
  403e56:	0f b6 80 20 67 40 00 	movzbl 0x406720(%rax),%eax
  403e5d:	42 88 84 24 e0 00 00 	mov    %al,0xe0(%rsp,%r12,1)
  403e64:	00 
  403e65:	45 85 c9             	test   %r9d,%r9d
  403e68:	79 1d                	jns    403e87 <os::[file_linux.odin]::_file_stream_proc+0x897>
  403e6a:	49 83 c0 fe          	add    $0xfffffffffffffffe,%r8
  403e6e:	49 81 f8 81 00 00 00 	cmp    $0x81,%r8
  403e75:	0f 83 f4 03 00 00    	jae    40426f <os::[file_linux.odin]::_file_stream_proc+0xc7f>
  403e7b:	42 c6 84 04 e0 00 00 	movb   $0x2d,0xe0(%rsp,%r8,1)
  403e82:	00 2d 
  403e84:	4d 89 c4             	mov    %r8,%r12
  403e87:	4c 89 54 24 20       	mov    %r10,0x20(%rsp)
  403e8c:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  403e91:	48 8d 7c 24 3e       	lea    0x3e(%rsp),%rdi
  403e96:	41 bd 81 00 00 00    	mov    $0x81,%r13d
  403e9c:	4d 29 e5             	sub    %r12,%r13
  403e9f:	49 83 fd 12          	cmp    $0x12,%r13
  403ea3:	ba 12 00 00 00       	mov    $0x12,%edx
  403ea8:	49 0f 42 d5          	cmovb  %r13,%rdx
  403eac:	4a 8d b4 24 e0 00 00 	lea    0xe0(%rsp,%r12,1),%rsi
  403eb3:	00 
  403eb4:	c5 f8 77             	vzeroupper
  403eb7:	e8 a4 d1 ff ff       	call   401060 <memcpy@plt>
  403ebc:	49 83 fc 6e          	cmp    $0x6e,%r12
  403ec0:	0f 86 5d 03 00 00    	jbe    404223 <os::[file_linux.odin]::_file_stream_proc+0xc33>
  403ec6:	41 bd 00 01 00 00    	mov    $0x100,%r13d
  403ecc:	4c 8d 84 24 e0 00 00 	lea    0xe0(%rsp),%r8
  403ed3:	00 
  403ed4:	bf 00 01 00 00       	mov    $0x100,%edi
  403ed9:	b9 f0 64 40 00       	mov    $0x4064f0,%ecx
  403ede:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403ee3:	48 89 6c 24 18       	mov    %rbp,0x18(%rsp)
  403ee8:	48 89 ea             	mov    %rbp,%rdx
  403eeb:	4d 89 f9             	mov    %r15,%r9
  403eee:	e8 bd ec ff ff       	call   402bb0 <runtime::mem_alloc_bytes>
  403ef3:	4c 8b a4 24 e0 00 00 	mov    0xe0(%rsp),%r12
  403efa:	00 
  403efb:	31 d2                	xor    %edx,%edx
  403efd:	4d 85 e4             	test   %r12,%r12
  403f00:	0f 95 c2             	setne  %dl
  403f03:	c1 e2 08             	shl    $0x8,%edx
  403f06:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  403f0b:	b8 59 00 00 00       	mov    $0x59,%eax
  403f10:	4c 89 e6             	mov    %r12,%rsi
  403f13:	0f 05                	syscall
  403f15:	48 89 c5             	mov    %rax,%rbp
  403f18:	48 85 c0             	test   %rax,%rax
  403f1b:	0f 98 c0             	sets   %al
  403f1e:	85 ed                	test   %ebp,%ebp
  403f20:	0f 95 c1             	setne  %cl
  403f23:	84 c8                	test   %cl,%al
  403f25:	0f 85 91 00 00 00    	jne    403fbc <os::[file_linux.odin]::_file_stream_proc+0x9cc>
  403f2b:	4c 39 ed             	cmp    %r13,%rbp
  403f2e:	0f 85 c8 00 00 00    	jne    403ffc <os::[file_linux.odin]::_file_stream_proc+0xa0c>
  403f34:	49 89 ed             	mov    %rbp,%r13
  403f37:	49 01 ed             	add    %rbp,%r13
  403f3a:	41 b8 50 65 40 00    	mov    $0x406550,%r8d
  403f40:	4c 89 e7             	mov    %r12,%rdi
  403f43:	48 89 d6             	mov    %rdx,%rsi
  403f46:	48 8b 6c 24 10       	mov    0x10(%rsp),%rbp
  403f4b:	48 89 ea             	mov    %rbp,%rdx
  403f4e:	4c 8b 64 24 18       	mov    0x18(%rsp),%r12
  403f53:	4c 89 e1             	mov    %r12,%rcx
  403f56:	4d 89 f9             	mov    %r15,%r9
  403f59:	e8 e2 f5 ff ff       	call   403540 <runtime::mem_free_with_size>
  403f5e:	4d 85 ed             	test   %r13,%r13
  403f61:	0f 88 af 02 00 00    	js     404216 <os::[file_linux.odin]::_file_stream_proc+0xc26>
  403f67:	b9 80 65 40 00       	mov    $0x406580,%ecx
  403f6c:	4c 89 ef             	mov    %r13,%rdi
  403f6f:	48 89 ee             	mov    %rbp,%rsi
  403f72:	4c 89 e2             	mov    %r12,%rdx
  403f75:	4c 8d 84 24 e0 00 00 	lea    0xe0(%rsp),%r8
  403f7c:	00 
  403f7d:	4d 89 f9             	mov    %r15,%r9
  403f80:	e8 2b ec ff ff       	call   402bb0 <runtime::mem_alloc_bytes>
  403f85:	4c 8b a4 24 e0 00 00 	mov    0xe0(%rsp),%r12
  403f8c:	00 
  403f8d:	4d 85 e4             	test   %r12,%r12
  403f90:	4c 89 ea             	mov    %r13,%rdx
  403f93:	49 0f 44 d4          	cmove  %r12,%rdx
  403f97:	b8 59 00 00 00       	mov    $0x59,%eax
  403f9c:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  403fa1:	4c 89 e6             	mov    %r12,%rsi
  403fa4:	0f 05                	syscall
  403fa6:	48 89 c5             	mov    %rax,%rbp
  403fa9:	48 85 c0             	test   %rax,%rax
  403fac:	0f 99 c0             	setns  %al
  403faf:	85 ed                	test   %ebp,%ebp
  403fb1:	0f 94 c1             	sete   %cl
  403fb4:	08 c1                	or     %al,%cl
  403fb6:	0f 85 6f ff ff ff    	jne    403f2b <os::[file_linux.odin]::_file_stream_proc+0x93b>
  403fbc:	41 b8 20 65 40 00    	mov    $0x406520,%r8d
  403fc2:	4c 89 e7             	mov    %r12,%rdi
  403fc5:	48 89 d6             	mov    %rdx,%rsi
  403fc8:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  403fcd:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403fd2:	4d 89 f9             	mov    %r15,%r9
  403fd5:	e8 66 f5 ff ff       	call   403540 <runtime::mem_free_with_size>
  403fda:	8d 45 26             	lea    0x26(%rbp),%eax
  403fdd:	83 f8 25             	cmp    $0x25,%eax
  403fe0:	0f 87 08 01 00 00    	ja     4040ee <os::[file_linux.odin]::_file_stream_proc+0xafe>
  403fe6:	ff 24 c5 80 52 40 00 	jmp    *0x405280(,%rax,8)
  403fed:	48 bb ff ff ff ff 02 	movabs $0x2ffffffff,%rbx
  403ff4:	00 00 00 
  403ff7:	e9 61 01 00 00       	jmp    40415d <os::[file_linux.odin]::_file_stream_proc+0xb6d>
  403ffc:	48 89 e8             	mov    %rbp,%rax
  403fff:	48 c1 f8 3f          	sar    $0x3f,%rax
  404003:	c4 e2 f8 f2 f5       	andn   %rbp,%rax,%rsi
  404008:	48 39 d6             	cmp    %rdx,%rsi
  40400b:	0f 87 80 02 00 00    	ja     404291 <os::[file_linux.odin]::_file_stream_proc+0xca1>
  404011:	48 85 ed             	test   %rbp,%rbp
  404014:	0f 8e 9a 02 00 00    	jle    4042b4 <os::[file_linux.odin]::_file_stream_proc+0xcc4>
  40401a:	41 80 3c 24 2f       	cmpb   $0x2f,(%r12)
  40401f:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404024:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  404029:	74 16                	je     404041 <os::[file_linux.odin]::_file_stream_proc+0xa51>
  40402b:	41 b8 b0 64 40 00    	mov    $0x4064b0,%r8d
  404031:	4c 89 e7             	mov    %r12,%rdi
  404034:	4d 89 f9             	mov    %r15,%r9
  404037:	e8 04 f5 ff ff       	call   403540 <runtime::mem_free_with_size>
  40403c:	45 31 e4             	xor    %r12d,%r12d
  40403f:	31 f6                	xor    %esi,%esi
  404041:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  404046:	41 81 e6 ff 0f 00 00 	and    $0xfff,%r14d
  40404d:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  404052:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  404059:	00 
  40405a:	4c 69 94 24 a8 00 00 	imul   $0x3b9aca00,0xa8(%rsp),%r10
  404061:	00 00 ca 9a 3b 
  404066:	4c 03 94 24 b0 00 00 	add    0xb0(%rsp),%r10
  40406d:	00 
  40406e:	4c 69 9c 24 98 00 00 	imul   $0x3b9aca00,0x98(%rsp),%r11
  404075:	00 00 ca 9a 3b 
  40407a:	4c 03 9c 24 a0 00 00 	add    0xa0(%rsp),%r11
  404081:	00 
  404082:	48 8d 7e ff          	lea    -0x1(%rsi),%rdi
  404086:	49 c7 c7 ff ff ff ff 	mov    $0xffffffffffffffff,%r15
  40408d:	31 db                	xor    %ebx,%ebx
  40408f:	45 31 c9             	xor    %r9d,%r9d
  404092:	4d 89 e0             	mov    %r12,%r8
  404095:	4e 8d 2c 0e          	lea    (%rsi,%r9,1),%r13
  404099:	4d 85 ed             	test   %r13,%r13
  40409c:	7e 45                	jle    4040e3 <os::[file_linux.odin]::_file_stream_proc+0xaf3>
  40409e:	49 ff c7             	inc    %r15
  4040a1:	4d 8d 68 ff          	lea    -0x1(%r8),%r13
  4040a5:	49 ff c9             	dec    %r9
  4040a8:	41 80 7c 30 ff 2f    	cmpb   $0x2f,-0x1(%r8,%rsi,1)
  4040ae:	4d 89 e8             	mov    %r13,%r8
  4040b1:	75 e2                	jne    404095 <os::[file_linux.odin]::_file_stream_proc+0xaa5>
  4040b3:	49 01 f1             	add    %rsi,%r9
  4040b6:	0f 84 e1 00 00 00    	je     40419d <os::[file_linux.odin]::_file_stream_proc+0xbad>
  4040bc:	49 39 f1             	cmp    %rsi,%r9
  4040bf:	0f 8f 0e 02 00 00    	jg     4042d3 <os::[file_linux.odin]::_file_stream_proc+0xce3>
  4040c5:	49 89 f0             	mov    %rsi,%r8
  4040c8:	4d 29 f8             	sub    %r15,%r8
  4040cb:	49 39 f0             	cmp    %rsi,%r8
  4040ce:	0f 87 1f 02 00 00    	ja     4042f3 <os::[file_linux.odin]::_file_stream_proc+0xd03>
  4040d4:	4d 8d 44 35 01       	lea    0x1(%r13,%rsi,1),%r8
  4040d9:	31 db                	xor    %ebx,%ebx
  4040db:	4c 89 ff             	mov    %r15,%rdi
  4040de:	e9 c1 00 00 00       	jmp    4041a4 <os::[file_linux.odin]::_file_stream_proc+0xbb4>
  4040e3:	4d 89 e0             	mov    %r12,%r8
  4040e6:	48 89 f7             	mov    %rsi,%rdi
  4040e9:	e9 b6 00 00 00       	jmp    4041a4 <os::[file_linux.odin]::_file_stream_proc+0xbb4>
  4040ee:	83 fd 92             	cmp    $0xffffff92,%ebp
  4040f1:	75 10                	jne    404103 <os::[file_linux.odin]::_file_stream_proc+0xb13>
  4040f3:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  4040fa:	00 00 00 
  4040fd:	48 83 cb 02          	or     $0x2,%rbx
  404101:	eb 5a                	jmp    40415d <os::[file_linux.odin]::_file_stream_proc+0xb6d>
  404103:	f7 dd                	neg    %ebp
  404105:	48 bb 00 00 00 00 04 	movabs $0x400000000,%rbx
  40410c:	00 00 00 
  40410f:	48 09 eb             	or     %rbp,%rbx
  404112:	eb 49                	jmp    40415d <os::[file_linux.odin]::_file_stream_proc+0xb6d>
  404114:	48 bb ff ff ff ff 02 	movabs $0x2ffffffff,%rbx
  40411b:	00 00 00 
  40411e:	48 83 c3 02          	add    $0x2,%rbx
  404122:	eb 39                	jmp    40415d <os::[file_linux.odin]::_file_stream_proc+0xb6d>
  404124:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  40412b:	00 00 00 
  40412e:	48 ff c3             	inc    %rbx
  404131:	eb 2a                	jmp    40415d <os::[file_linux.odin]::_file_stream_proc+0xb6d>
  404133:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  40413a:	00 00 00 
  40413d:	48 83 c3 03          	add    $0x3,%rbx
  404141:	eb 1a                	jmp    40415d <os::[file_linux.odin]::_file_stream_proc+0xb6d>
  404143:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  40414a:	00 00 00 
  40414d:	48 83 cb 04          	or     $0x4,%rbx
  404151:	eb 0a                	jmp    40415d <os::[file_linux.odin]::_file_stream_proc+0xb6d>
  404153:	48 bb 01 00 00 00 01 	movabs $0x100000001,%rbx
  40415a:	00 00 00 
  40415d:	45 31 e4             	xor    %r12d,%r12d
  404160:	41 b8 b0 64 40 00    	mov    $0x4064b0,%r8d
  404166:	31 ff                	xor    %edi,%edi
  404168:	31 f6                	xor    %esi,%esi
  40416a:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40416f:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404174:	4d 89 f9             	mov    %r15,%r9
  404177:	e8 c4 f3 ff ff       	call   403540 <runtime::mem_free_with_size>
  40417c:	31 f6                	xor    %esi,%esi
  40417e:	45 31 c0             	xor    %r8d,%r8d
  404181:	31 ff                	xor    %edi,%edi
  404183:	31 c0                	xor    %eax,%eax
  404185:	31 c9                	xor    %ecx,%ecx
  404187:	45 31 f6             	xor    %r14d,%r14d
  40418a:	45 31 c9             	xor    %r9d,%r9d
  40418d:	45 31 d2             	xor    %r10d,%r10d
  404190:	45 31 db             	xor    %r11d,%r11d
  404193:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  404198:	e9 fa fb ff ff       	jmp    403d97 <os::[file_linux.odin]::_file_stream_proc+0x7a7>
  40419d:	4d 8d 44 24 01       	lea    0x1(%r12),%r8
  4041a2:	31 db                	xor    %ebx,%ebx
  4041a4:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  4041a9:	e9 e9 fb ff ff       	jmp    403d97 <os::[file_linux.odin]::_file_stream_proc+0x7a7>
  4041ae:	48 89 0c 24          	mov    %rcx,(%rsp)
  4041b2:	bf 5f 63 40 00       	mov    $0x40635f,%edi
  4041b7:	be 25 00 00 00       	mov    $0x25,%esi
  4041bc:	ba fb 00 00 00       	mov    $0xfb,%edx
  4041c1:	49 89 c9             	mov    %rcx,%r9
  4041c4:	b9 08 00 00 00       	mov    $0x8,%ecx
  4041c9:	e8 d2 ce ff ff       	call   4010a0 <runtime::slice_handle_error>
  4041ce:	48 89 0c 24          	mov    %rcx,(%rsp)
  4041d2:	bf 5f 63 40 00       	mov    $0x40635f,%edi
  4041d7:	be 25 00 00 00       	mov    $0x25,%esi
  4041dc:	ba 10 01 00 00       	mov    $0x110,%edx
  4041e1:	49 89 c9             	mov    %rcx,%r9
  4041e4:	b9 08 00 00 00       	mov    $0x8,%ecx
  4041e9:	4d 89 d0             	mov    %r10,%r8
  4041ec:	e8 af ce ff ff       	call   4010a0 <runtime::slice_handle_error>
  4041f1:	bf 68 5f 40 00       	mov    $0x405f68,%edi
  4041f6:	be 28 00 00 00       	mov    $0x28,%esi
  4041fb:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  404201:	ba 4b 00 00 00       	mov    $0x4b,%edx
  404206:	b9 0b 00 00 00       	mov    $0xb,%ecx
  40420b:	4d 89 e0             	mov    %r12,%r8
  40420e:	c5 f8 77             	vzeroupper
  404211:	e8 2a d0 ff ff       	call   401240 <runtime::bounds_check_error.handle_error-0>
  404216:	bf 80 65 40 00       	mov    $0x406580,%edi
  40421b:	4c 89 ee             	mov    %r13,%rsi
  40421e:	e8 8d d0 ff ff       	call   4012b0 <runtime::make_slice_error_loc.handle_error-0>
  404223:	48 c7 04 24 12 00 00 	movq   $0x12,(%rsp)
  40422a:	00 
  40422b:	bf 68 5f 40 00       	mov    $0x405f68,%edi
  404230:	be 28 00 00 00       	mov    $0x28,%esi
  404235:	ba 68 00 00 00       	mov    $0x68,%edx
  40423a:	b9 13 00 00 00       	mov    $0x13,%ecx
  40423f:	45 31 c0             	xor    %r8d,%r8d
  404242:	4d 89 e9             	mov    %r13,%r9
  404245:	e8 56 ce ff ff       	call   4010a0 <runtime::slice_handle_error>
  40424a:	bf 68 5f 40 00       	mov    $0x405f68,%edi
  40424f:	be 28 00 00 00       	mov    $0x28,%esi
  404254:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  40425a:	ba 4e 00 00 00       	mov    $0x4e,%edx
  40425f:	b9 0a 00 00 00       	mov    $0xa,%ecx
  404264:	4d 89 e0             	mov    %r12,%r8
  404267:	c5 f8 77             	vzeroupper
  40426a:	e8 d1 cf ff ff       	call   401240 <runtime::bounds_check_error.handle_error-0>
  40426f:	bf 68 5f 40 00       	mov    $0x405f68,%edi
  404274:	be 28 00 00 00       	mov    $0x28,%esi
  404279:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  40427f:	ba 61 00 00 00       	mov    $0x61,%edx
  404284:	b9 0b 00 00 00       	mov    $0xb,%ecx
  404289:	c5 f8 77             	vzeroupper
  40428c:	e8 af cf ff ff       	call   401240 <runtime::bounds_check_error.handle_error-0>
  404291:	48 89 14 24          	mov    %rdx,(%rsp)
  404295:	bf 5f 63 40 00       	mov    $0x40635f,%edi
  40429a:	49 89 f1             	mov    %rsi,%r9
  40429d:	be 25 00 00 00       	mov    $0x25,%esi
  4042a2:	ba 66 01 00 00       	mov    $0x166,%edx
  4042a7:	b9 15 00 00 00       	mov    $0x15,%ecx
  4042ac:	45 31 c0             	xor    %r8d,%r8d
  4042af:	e8 ec cd ff ff       	call   4010a0 <runtime::slice_handle_error>
  4042b4:	bf 78 64 40 00       	mov    $0x406478,%edi
  4042b9:	49 89 f1             	mov    %rsi,%r9
  4042bc:	be 25 00 00 00       	mov    $0x25,%esi
  4042c1:	ba cc 00 00 00       	mov    $0xcc,%edx
  4042c6:	b9 5a 00 00 00       	mov    $0x5a,%ecx
  4042cb:	45 31 c0             	xor    %r8d,%r8d
  4042ce:	e8 6d cf ff ff       	call   401240 <runtime::bounds_check_error.handle_error-0>
  4042d3:	48 89 34 24          	mov    %rsi,(%rsp)
  4042d7:	bf c8 60 40 00       	mov    $0x4060c8,%edi
  4042dc:	be 27 00 00 00       	mov    $0x27,%esi
  4042e1:	ba 36 00 00 00       	mov    $0x36,%edx
  4042e6:	b9 0e 00 00 00       	mov    $0xe,%ecx
  4042eb:	45 31 c0             	xor    %r8d,%r8d
  4042ee:	e8 ad cd ff ff       	call   4010a0 <runtime::slice_handle_error>
  4042f3:	48 89 34 24          	mov    %rsi,(%rsp)
  4042f7:	bf c8 60 40 00       	mov    $0x4060c8,%edi
  4042fc:	49 89 f1             	mov    %rsi,%r9
  4042ff:	be 27 00 00 00       	mov    $0x27,%esi
  404304:	ba 36 00 00 00       	mov    $0x36,%edx
  404309:	b9 18 00 00 00       	mov    $0x18,%ecx
  40430e:	e8 8d cd ff ff       	call   4010a0 <runtime::slice_handle_error>
  404313:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40431a:	84 00 00 00 00 00 

0000000000404320 <runtime::default_logger_proc>:
  404320:	c3                   	ret
  404321:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404328:	0f 1f 84 00 00 00 00 
  40432f:	00 

0000000000404330 <__truncsfhf2>:
  404330:	c5 f9 7e c7          	vmovd  %xmm0,%edi
  404334:	89 f8                	mov    %edi,%eax
  404336:	c1 e8 10             	shr    $0x10,%eax
  404339:	25 00 80 00 00       	and    $0x8000,%eax
  40433e:	b9 17 08 00 00       	mov    $0x817,%ecx
  404343:	c4 e2 70 f7 d7       	bextr  %ecx,%edi,%edx
  404348:	89 f9                	mov    %edi,%ecx
  40434a:	81 e1 ff ff 7f 00    	and    $0x7fffff,%ecx
  404350:	83 fa 70             	cmp    $0x70,%edx
  404353:	77 32                	ja     404387 <__truncsfhf2+0x57>
  404355:	83 fa 66             	cmp    $0x66,%edx
  404358:	0f 82 40 01 00 00    	jb     40449e <__truncsfhf2+0x16e>
  40435e:	81 c9 00 00 80 00    	or     $0x800000,%ecx
  404364:	40 b6 71             	mov    $0x71,%sil
  404367:	40 28 d6             	sub    %dl,%sil
  40436a:	c4 e2 4b f7 c9       	shrx   %esi,%ecx,%ecx
  40436f:	89 ca                	mov    %ecx,%edx
  404371:	81 e2 00 10 00 00    	and    $0x1000,%edx
  404377:	8d 0c 51             	lea    (%rcx,%rdx,2),%ecx
  40437a:	c1 e9 0d             	shr    $0xd,%ecx
  40437d:	09 c1                	or     %eax,%ecx
  40437f:	89 c8                	mov    %ecx,%eax
  404381:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  404386:	c3                   	ret
  404387:	8d 72 90             	lea    -0x70(%rdx),%esi
  40438a:	81 fe 8f 00 00 00    	cmp    $0x8f,%esi
  404390:	75 2b                	jne    4043bd <__truncsfhf2+0x8d>
  404392:	85 c9                	test   %ecx,%ecx
  404394:	0f 84 ed 00 00 00    	je     404487 <__truncsfhf2+0x157>
  40439a:	89 ca                	mov    %ecx,%edx
  40439c:	c1 ea 0d             	shr    $0xd,%edx
  40439f:	31 f6                	xor    %esi,%esi
  4043a1:	81 f9 00 20 00 00    	cmp    $0x2000,%ecx
  4043a7:	40 0f 92 c6          	setb   %sil
  4043ab:	09 c2                	or     %eax,%edx
  4043ad:	09 f2                	or     %esi,%edx
  4043af:	81 ca 00 7c 00 00    	or     $0x7c00,%edx
  4043b5:	89 d0                	mov    %edx,%eax
  4043b7:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  4043bc:	c3                   	ret
  4043bd:	f7 c7 00 10 00 00    	test   $0x1000,%edi
  4043c3:	74 1e                	je     4043e3 <__truncsfhf2+0xb3>
  4043c5:	8d b9 00 20 00 00    	lea    0x2000(%rcx),%edi
  4043cb:	83 c2 91             	add    $0xffffff91,%edx
  4043ce:	45 31 c0             	xor    %r8d,%r8d
  4043d1:	81 f9 00 e0 7f 00    	cmp    $0x7fe000,%ecx
  4043d7:	44 0f 42 c7          	cmovb  %edi,%r8d
  4043db:	0f 42 d6             	cmovb  %esi,%edx
  4043de:	44 89 c1             	mov    %r8d,%ecx
  4043e1:	89 d6                	mov    %edx,%esi
  4043e3:	83 fe 1f             	cmp    $0x1f,%esi
  4043e6:	0f 82 a6 00 00 00    	jb     404492 <__truncsfhf2+0x162>
  4043ec:	48 b9 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rcx
  4043f3:	00 00 00 
  4043f6:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4043fb:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  404400:	48 0f af c9          	imul   %rcx,%rcx
  404404:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404409:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40440e:	48 0f af c9          	imul   %rcx,%rcx
  404412:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404417:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40441c:	48 0f af c9          	imul   %rcx,%rcx
  404420:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404425:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40442a:	48 0f af c9          	imul   %rcx,%rcx
  40442e:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404433:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  404438:	48 0f af c9          	imul   %rcx,%rcx
  40443c:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404441:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  404446:	48 0f af c9          	imul   %rcx,%rcx
  40444a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40444f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  404454:	48 0f af c9          	imul   %rcx,%rcx
  404458:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40445d:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  404462:	48 0f af c9          	imul   %rcx,%rcx
  404466:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40446b:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  404470:	48 0f af c9          	imul   %rcx,%rcx
  404474:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404479:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40447e:	48 0f af c9          	imul   %rcx,%rcx
  404482:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404487:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40448c:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  404491:	c3                   	ret
  404492:	c1 e6 0a             	shl    $0xa,%esi
  404495:	c1 e9 0d             	shr    $0xd,%ecx
  404498:	09 c1                	or     %eax,%ecx
  40449a:	09 f1                	or     %esi,%ecx
  40449c:	89 c8                	mov    %ecx,%eax
  40449e:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  4044a3:	c3                   	ret
  4044a4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4044ab:	00 00 00 00 00 

00000000004044b0 <__truncdfhf2>:
  4044b0:	c5 fb 5a c0          	vcvtsd2ss %xmm0,%xmm0,%xmm0
  4044b4:	e9 77 fe ff ff       	jmp    404330 <__truncsfhf2>
  4044b9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004044c0 <__gnu_h2f_ieee>:
  4044c0:	c5 f9 c5 c0 00       	vpextrw $0x0,%xmm0,%eax
  4044c5:	89 c1                	mov    %eax,%ecx
  4044c7:	81 e1 ff 7f 00 00    	and    $0x7fff,%ecx
  4044cd:	c1 e1 0d             	shl    $0xd,%ecx
  4044d0:	c5 f9 6e c1          	vmovd  %ecx,%xmm0
  4044d4:	c5 fa 59 05 40 0b 00 	vmulss 0xb40(%rip),%xmm0,%xmm0        # 40501c <_IO_stdin_used+0x1c>
  4044db:	00 
  4044dc:	c5 f9 7e c1          	vmovd  %xmm0,%ecx
  4044e0:	89 ca                	mov    %ecx,%edx
  4044e2:	81 ca 00 00 80 7f    	or     $0x7f800000,%edx
  4044e8:	c5 f8 2e 05 30 0b 00 	vucomiss 0xb30(%rip),%xmm0        # 405020 <_IO_stdin_used+0x20>
  4044ef:	00 
  4044f0:	0f 42 d1             	cmovb  %ecx,%edx
  4044f3:	25 00 80 00 00       	and    $0x8000,%eax
  4044f8:	c1 e0 10             	shl    $0x10,%eax
  4044fb:	09 d0                	or     %edx,%eax
  4044fd:	c5 f9 6e c0          	vmovd  %eax,%xmm0
  404501:	c3                   	ret
  404502:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404509:	1f 84 00 00 00 00 00 

0000000000404510 <__gnu_f2h_ieee>:
  404510:	e9 1b fe ff ff       	jmp    404330 <__truncsfhf2>
  404515:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40451c:	00 00 00 00 

0000000000404520 <__extendhfsf2>:
  404520:	c5 f9 c5 c0 00       	vpextrw $0x0,%xmm0,%eax
  404525:	89 c1                	mov    %eax,%ecx
  404527:	81 e1 ff 7f 00 00    	and    $0x7fff,%ecx
  40452d:	c1 e1 0d             	shl    $0xd,%ecx
  404530:	c5 f9 6e c1          	vmovd  %ecx,%xmm0
  404534:	c5 fa 59 05 e0 0a 00 	vmulss 0xae0(%rip),%xmm0,%xmm0        # 40501c <_IO_stdin_used+0x1c>
  40453b:	00 
  40453c:	c5 f9 7e c1          	vmovd  %xmm0,%ecx
  404540:	89 ca                	mov    %ecx,%edx
  404542:	81 ca 00 00 80 7f    	or     $0x7f800000,%edx
  404548:	c5 f8 2e 05 d0 0a 00 	vucomiss 0xad0(%rip),%xmm0        # 405020 <_IO_stdin_used+0x20>
  40454f:	00 
  404550:	0f 42 d1             	cmovb  %ecx,%edx
  404553:	25 00 80 00 00       	and    $0x8000,%eax
  404558:	c1 e0 10             	shl    $0x10,%eax
  40455b:	09 d0                	or     %edx,%eax
  40455d:	c5 f9 6e c0          	vmovd  %eax,%xmm0
  404561:	c3                   	ret
  404562:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404569:	1f 84 00 00 00 00 00 

0000000000404570 <__floattidf>:
  404570:	48 89 f8             	mov    %rdi,%rax
  404573:	48 09 f0             	or     %rsi,%rax
  404576:	74 5b                	je     4045d3 <__floattidf+0x63>
  404578:	48 89 f2             	mov    %rsi,%rdx
  40457b:	48 c1 fa 3f          	sar    $0x3f,%rdx
  40457f:	48 31 d6             	xor    %rdx,%rsi
  404582:	48 31 d7             	xor    %rdx,%rdi
  404585:	48 29 d7             	sub    %rdx,%rdi
  404588:	48 19 d6             	sbb    %rdx,%rsi
  40458b:	f3 48 0f bd ce       	lzcnt  %rsi,%rcx
  404590:	f3 48 0f bd c7       	lzcnt  %rdi,%rax
  404595:	48 83 c0 40          	add    $0x40,%rax
  404599:	48 85 f6             	test   %rsi,%rsi
  40459c:	48 0f 45 c1          	cmovne %rcx,%rax
  4045a0:	41 89 c0             	mov    %eax,%r8d
  4045a3:	41 83 f0 7f          	xor    $0x7f,%r8d
  4045a7:	48 89 f9             	mov    %rdi,%rcx
  4045aa:	48 c1 e9 35          	shr    $0x35,%rcx
  4045ae:	48 09 f1             	or     %rsi,%rcx
  4045b1:	74 25                	je     4045d8 <__floattidf+0x68>
  4045b3:	89 c1                	mov    %eax,%ecx
  4045b5:	80 e1 7f             	and    $0x7f,%cl
  4045b8:	80 f9 49             	cmp    $0x49,%cl
  4045bb:	0f 84 b5 00 00 00    	je     404676 <__floattidf+0x106>
  4045c1:	83 f8 4a             	cmp    $0x4a,%eax
  4045c4:	75 26                	jne    4045ec <__floattidf+0x7c>
  4045c6:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  4045cb:	48 01 ff             	add    %rdi,%rdi
  4045ce:	e9 a3 00 00 00       	jmp    404676 <__floattidf+0x106>
  4045d3:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4045d7:	c3                   	ret
  4045d8:	04 b5                	add    $0xb5,%al
  4045da:	c4 e2 f9 f7 cf       	shlx   %rax,%rdi,%rcx
  4045df:	31 f6                	xor    %esi,%esi
  4045e1:	a8 40                	test   $0x40,%al
  4045e3:	48 0f 44 f1          	cmove  %rcx,%rsi
  4045e7:	e9 bb 00 00 00       	jmp    4046a7 <__floattidf+0x137>
  4045ec:	41 56                	push   %r14
  4045ee:	53                   	push   %rbx
  4045ef:	45 31 db             	xor    %r11d,%r11d
  4045f2:	b9 49 00 00 00       	mov    $0x49,%ecx
  4045f7:	48 29 c1             	sub    %rax,%rcx
  4045fa:	bb 00 00 00 00       	mov    $0x0,%ebx
  4045ff:	48 19 db             	sbb    %rbx,%rbx
  404602:	49 89 f9             	mov    %rdi,%r9
  404605:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  404609:	c4 62 f3 f7 d6       	shrx   %rcx,%rsi,%r10
  40460e:	f6 c1 40             	test   $0x40,%cl
  404611:	4d 0f 45 ca          	cmovne %r10,%r9
  404615:	4d 0f 45 d3          	cmovne %r11,%r10
  404619:	48 81 f9 80 00 00 00 	cmp    $0x80,%rcx
  404620:	48 83 db 00          	sbb    $0x0,%rbx
  404624:	4d 0f 43 d3          	cmovae %r11,%r10
  404628:	4d 0f 43 cb          	cmovae %r11,%r9
  40462c:	bb ff 01 00 00       	mov    $0x1ff,%ebx
  404631:	49 c7 c6 ff ff ff ff 	mov    $0xffffffffffffffff,%r14
  404638:	89 c1                	mov    %eax,%ecx
  40463a:	49 0f ad de          	shrd   %cl,%rbx,%r14
  40463e:	48 89 f9             	mov    %rdi,%rcx
  404641:	48 c1 e9 37          	shr    $0x37,%rcx
  404645:	c4 e2 fb f7 db       	shrx   %rax,%rbx,%rbx
  40464a:	a8 40                	test   $0x40,%al
  40464c:	4c 0f 45 f3          	cmovne %rbx,%r14
  404650:	49 0f 45 db          	cmovne %r11,%rbx
  404654:	48 09 f1             	or     %rsi,%rcx
  404657:	4d 0f 44 f3          	cmove  %r11,%r14
  40465b:	41 0f 44 db          	cmove  %r11d,%ebx
  40465f:	49 21 fe             	and    %rdi,%r14
  404662:	21 de                	and    %ebx,%esi
  404664:	31 ff                	xor    %edi,%edi
  404666:	4c 09 f6             	or     %r14,%rsi
  404669:	40 0f 95 c7          	setne  %dil
  40466d:	4c 09 cf             	or     %r9,%rdi
  404670:	4c 89 d6             	mov    %r10,%rsi
  404673:	5b                   	pop    %rbx
  404674:	41 5e                	pop    %r14
  404676:	b9 02 01 00 00       	mov    $0x102,%ecx
  40467b:	c4 e2 70 f7 cf       	bextr  %ecx,%edi,%ecx
  404680:	48 09 f9             	or     %rdi,%rcx
  404683:	48 83 c1 01          	add    $0x1,%rcx
  404687:	48 83 d6 00          	adc    $0x0,%rsi
  40468b:	48 0f ba e1 37       	bt     $0x37,%rcx
  404690:	72 07                	jb     404699 <__floattidf+0x129>
  404692:	48 0f a4 ce 3e       	shld   $0x3e,%rcx,%rsi
  404697:	eb 0e                	jmp    4046a7 <__floattidf+0x137>
  404699:	48 0f a4 ce 3d       	shld   $0x3d,%rcx,%rsi
  40469e:	41 b8 80 00 00 00    	mov    $0x80,%r8d
  4046a4:	41 29 c0             	sub    %eax,%r8d
  4046a7:	81 e2 00 00 00 80    	and    $0x80000000,%edx
  4046ad:	41 c1 e0 14          	shl    $0x14,%r8d
  4046b1:	41 09 d0             	or     %edx,%r8d
  4046b4:	48 89 f0             	mov    %rsi,%rax
  4046b7:	48 c1 e8 20          	shr    $0x20,%rax
  4046bb:	25 ff ff 0f 00       	and    $0xfffff,%eax
  4046c0:	42 8d 84 00 00 00 f0 	lea    0x3ff00000(%rax,%r8,1),%eax
  4046c7:	3f 
  4046c8:	48 c1 e0 20          	shl    $0x20,%rax
  4046cc:	89 f1                	mov    %esi,%ecx
  4046ce:	48 09 c1             	or     %rax,%rcx
  4046d1:	c4 e1 f9 6e c1       	vmovq  %rcx,%xmm0
  4046d6:	c3                   	ret
  4046d7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4046de:	00 00 

00000000004046e0 <__floattidf_unsigned>:
  4046e0:	48 89 f8             	mov    %rdi,%rax
  4046e3:	48 09 f0             	or     %rsi,%rax
  4046e6:	74 46                	je     40472e <__floattidf_unsigned+0x4e>
  4046e8:	f3 48 0f bd ce       	lzcnt  %rsi,%rcx
  4046ed:	f3 48 0f bd c7       	lzcnt  %rdi,%rax
  4046f2:	48 83 c0 40          	add    $0x40,%rax
  4046f6:	48 85 f6             	test   %rsi,%rsi
  4046f9:	48 0f 45 c1          	cmovne %rcx,%rax
  4046fd:	89 c2                	mov    %eax,%edx
  4046ff:	83 f2 7f             	xor    $0x7f,%edx
  404702:	48 89 f9             	mov    %rdi,%rcx
  404705:	48 c1 e9 35          	shr    $0x35,%rcx
  404709:	48 09 f1             	or     %rsi,%rcx
  40470c:	74 25                	je     404733 <__floattidf_unsigned+0x53>
  40470e:	89 c1                	mov    %eax,%ecx
  404710:	80 e1 7f             	and    $0x7f,%cl
  404713:	80 f9 49             	cmp    $0x49,%cl
  404716:	0f 84 b4 00 00 00    	je     4047d0 <__floattidf_unsigned+0xf0>
  40471c:	83 f8 4a             	cmp    $0x4a,%eax
  40471f:	75 26                	jne    404747 <__floattidf_unsigned+0x67>
  404721:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  404726:	48 01 ff             	add    %rdi,%rdi
  404729:	e9 a2 00 00 00       	jmp    4047d0 <__floattidf_unsigned+0xf0>
  40472e:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  404732:	c3                   	ret
  404733:	04 b5                	add    $0xb5,%al
  404735:	c4 e2 f9 f7 cf       	shlx   %rax,%rdi,%rcx
  40473a:	31 f6                	xor    %esi,%esi
  40473c:	a8 40                	test   $0x40,%al
  40473e:	48 0f 44 f1          	cmove  %rcx,%rsi
  404742:	e9 b8 00 00 00       	jmp    4047ff <__floattidf_unsigned+0x11f>
  404747:	53                   	push   %rbx
  404748:	45 31 d2             	xor    %r10d,%r10d
  40474b:	b9 49 00 00 00       	mov    $0x49,%ecx
  404750:	48 29 c1             	sub    %rax,%rcx
  404753:	41 bb 00 00 00 00    	mov    $0x0,%r11d
  404759:	4d 19 db             	sbb    %r11,%r11
  40475c:	49 89 f8             	mov    %rdi,%r8
  40475f:	49 0f ad f0          	shrd   %cl,%rsi,%r8
  404763:	c4 62 f3 f7 ce       	shrx   %rcx,%rsi,%r9
  404768:	f6 c1 40             	test   $0x40,%cl
  40476b:	4d 0f 45 c1          	cmovne %r9,%r8
  40476f:	4d 0f 45 ca          	cmovne %r10,%r9
  404773:	48 81 f9 80 00 00 00 	cmp    $0x80,%rcx
  40477a:	49 83 db 00          	sbb    $0x0,%r11
  40477e:	4d 0f 43 ca          	cmovae %r10,%r9
  404782:	4d 0f 43 c2          	cmovae %r10,%r8
  404786:	41 bb ff 01 00 00    	mov    $0x1ff,%r11d
  40478c:	48 c7 c3 ff ff ff ff 	mov    $0xffffffffffffffff,%rbx
  404793:	89 c1                	mov    %eax,%ecx
  404795:	4c 0f ad db          	shrd   %cl,%r11,%rbx
  404799:	48 89 f9             	mov    %rdi,%rcx
  40479c:	48 c1 e9 37          	shr    $0x37,%rcx
  4047a0:	c4 42 fb f7 db       	shrx   %rax,%r11,%r11
  4047a5:	a8 40                	test   $0x40,%al
  4047a7:	49 0f 45 db          	cmovne %r11,%rbx
  4047ab:	4d 0f 45 da          	cmovne %r10,%r11
  4047af:	48 09 f1             	or     %rsi,%rcx
  4047b2:	49 0f 44 da          	cmove  %r10,%rbx
  4047b6:	45 0f 44 da          	cmove  %r10d,%r11d
  4047ba:	48 21 fb             	and    %rdi,%rbx
  4047bd:	44 21 de             	and    %r11d,%esi
  4047c0:	31 ff                	xor    %edi,%edi
  4047c2:	48 09 de             	or     %rbx,%rsi
  4047c5:	40 0f 95 c7          	setne  %dil
  4047c9:	4c 09 c7             	or     %r8,%rdi
  4047cc:	4c 89 ce             	mov    %r9,%rsi
  4047cf:	5b                   	pop    %rbx
  4047d0:	b9 02 01 00 00       	mov    $0x102,%ecx
  4047d5:	c4 e2 70 f7 cf       	bextr  %ecx,%edi,%ecx
  4047da:	48 09 f9             	or     %rdi,%rcx
  4047dd:	48 83 c1 01          	add    $0x1,%rcx
  4047e1:	48 83 d6 00          	adc    $0x0,%rsi
  4047e5:	48 0f ba e1 37       	bt     $0x37,%rcx
  4047ea:	72 07                	jb     4047f3 <__floattidf_unsigned+0x113>
  4047ec:	48 0f a4 ce 3e       	shld   $0x3e,%rcx,%rsi
  4047f1:	eb 0c                	jmp    4047ff <__floattidf_unsigned+0x11f>
  4047f3:	48 0f a4 ce 3d       	shld   $0x3d,%rcx,%rsi
  4047f8:	ba 80 00 00 00       	mov    $0x80,%edx
  4047fd:	29 c2                	sub    %eax,%edx
  4047ff:	c1 e2 14             	shl    $0x14,%edx
  404802:	48 89 f0             	mov    %rsi,%rax
  404805:	48 c1 e8 20          	shr    $0x20,%rax
  404809:	25 ff ff 0f 00       	and    $0xfffff,%eax
  40480e:	8d 84 02 00 00 f0 3f 	lea    0x3ff00000(%rdx,%rax,1),%eax
  404815:	48 c1 e0 20          	shl    $0x20,%rax
  404819:	89 f1                	mov    %esi,%ecx
  40481b:	48 09 c1             	or     %rax,%rcx
  40481e:	c4 e1 f9 6e c1       	vmovq  %rcx,%xmm0
  404823:	c3                   	ret
  404824:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40482b:	00 00 00 00 00 

0000000000404830 <__umodti3>:
  404830:	48 83 ec 18          	sub    $0x18,%rsp
  404834:	49 89 e0             	mov    %rsp,%r8
  404837:	e8 b4 d0 ff ff       	call   4018f0 <runtime::udivmod128>
  40483c:	48 8b 04 24          	mov    (%rsp),%rax
  404840:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  404845:	48 83 c4 18          	add    $0x18,%rsp
  404849:	c3                   	ret
  40484a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000404850 <__udivmodti4>:
  404850:	e9 9b d0 ff ff       	jmp    4018f0 <runtime::udivmod128>
  404855:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40485c:	00 00 00 00 

0000000000404860 <__udivti3>:
  404860:	45 31 c0             	xor    %r8d,%r8d
  404863:	e9 88 d0 ff ff       	jmp    4018f0 <runtime::udivmod128>
  404868:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40486f:	00 

0000000000404870 <__modti3>:
  404870:	53                   	push   %rbx
  404871:	48 83 ec 10          	sub    $0x10,%rsp
  404875:	48 89 f3             	mov    %rsi,%rbx
  404878:	48 c1 fb 3f          	sar    $0x3f,%rbx
  40487c:	48 31 de             	xor    %rbx,%rsi
  40487f:	48 31 df             	xor    %rbx,%rdi
  404882:	48 29 df             	sub    %rbx,%rdi
  404885:	48 19 de             	sbb    %rbx,%rsi
  404888:	48 89 c8             	mov    %rcx,%rax
  40488b:	48 c1 f8 3f          	sar    $0x3f,%rax
  40488f:	48 31 c1             	xor    %rax,%rcx
  404892:	48 31 c2             	xor    %rax,%rdx
  404895:	48 29 c2             	sub    %rax,%rdx
  404898:	48 19 c1             	sbb    %rax,%rcx
  40489b:	49 89 e0             	mov    %rsp,%r8
  40489e:	e8 4d d0 ff ff       	call   4018f0 <runtime::udivmod128>
  4048a3:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4048a8:	48 31 da             	xor    %rbx,%rdx
  4048ab:	48 8b 04 24          	mov    (%rsp),%rax
  4048af:	48 31 d8             	xor    %rbx,%rax
  4048b2:	48 29 d8             	sub    %rbx,%rax
  4048b5:	48 19 da             	sbb    %rbx,%rdx
  4048b8:	48 83 c4 10          	add    $0x10,%rsp
  4048bc:	5b                   	pop    %rbx
  4048bd:	c3                   	ret

Disassembly of section .fini:

00000000004048c0 <_fini>:
  4048c0:	f3 0f 1e fa          	endbr64
  4048c4:	48 83 ec 08          	sub    $0x8,%rsp
  4048c8:	48 83 c4 08          	add    $0x8,%rsp
  4048cc:	c3                   	ret
