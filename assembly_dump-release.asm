
/home/khalidd/Desktop/Github/Journey_ECS/main-release.bin:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 c9 4f 00 00 	mov    0x4fc9(%rip),%rax        # 405fd8 <__gmon_start__@Base>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 4f 00 00    	push   0x4fca(%rip)        # 405ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 4f 00 00    	jmp    *0x4fcc(%rip)        # 405ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 4f 00 00    	jmp    *0x4fca(%rip)        # 406000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <memset@plt>:
  401040:	ff 25 c2 4f 00 00    	jmp    *0x4fc2(%rip)        # 406008 <memset@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <calloc@plt>:
  401050:	ff 25 ba 4f 00 00    	jmp    *0x4fba(%rip)        # 406010 <calloc@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <memcpy@plt>:
  401060:	ff 25 b2 4f 00 00    	jmp    *0x4fb2(%rip)        # 406018 <memcpy@GLIBC_2.14>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <malloc@plt>:
  401070:	ff 25 aa 4f 00 00    	jmp    *0x4faa(%rip)        # 406020 <malloc@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <realloc@plt>:
  401080:	ff 25 a2 4f 00 00    	jmp    *0x4fa2(%rip)        # 406028 <realloc@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <memmove@plt>:
  401090:	ff 25 9a 4f 00 00    	jmp    *0x4f9a(%rip)        # 406030 <memmove@GLIBC_2.2.5>
  401096:	68 06 00 00 00       	push   $0x6
  40109b:	e9 80 ff ff ff       	jmp    401020 <_init+0x20>

Disassembly of section .text:

00000000004010a0 <runtime::slice_handle_error>:
  4010a0:	41 57                	push   %r15
  4010a2:	41 56                	push   %r14
  4010a4:	53                   	push   %rbx
  4010a5:	48 83 ec 30          	sub    $0x30,%rsp
  4010a9:	48 89 d3             	mov    %rdx,%rbx
  4010ac:	49 89 f6             	mov    %rsi,%r14
  4010af:	49 89 ff             	mov    %rdi,%r15
  4010b2:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4010b6:	c5 f8 11 44 24 20    	vmovups %xmm0,0x20(%rsp)
  4010bc:	48 c7 44 24 08 48 43 	movq   $0x404348,0x8(%rsp)
  4010c3:	40 00 
  4010c5:	48 c7 44 24 10 2e 00 	movq   $0x2e,0x10(%rsp)
  4010cc:	00 00 
  4010ce:	48 b8 4d 00 00 00 26 	movabs $0x260000004d,%rax
  4010d5:	00 00 00 
  4010d8:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4010dd:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  4010e2:	e8 a9 1b 00 00       	call   402c90 <runtime::print_caller_location>
  4010e7:	bf 58 41 40 00       	mov    $0x404158,%edi
  4010ec:	be 17 00 00 00       	mov    $0x17,%esi
  4010f1:	e8 ba 14 00 00       	call   4025b0 <runtime::print_string>
  4010f6:	4c 89 ff             	mov    %r15,%rdi
  4010f9:	e8 32 1a 00 00       	call   402b30 <runtime::print_i64>
  4010fe:	bf 70 41 40 00       	mov    $0x404170,%edi
  401103:	be 01 00 00 00       	mov    $0x1,%esi
  401108:	e8 a3 14 00 00       	call   4025b0 <runtime::print_string>
  40110d:	4c 89 f7             	mov    %r14,%rdi
  401110:	e8 1b 1a 00 00       	call   402b30 <runtime::print_i64>
  401115:	bf 72 41 40 00       	mov    $0x404172,%edi
  40111a:	be 15 00 00 00       	mov    $0x15,%esi
  40111f:	e8 8c 14 00 00       	call   4025b0 <runtime::print_string>
  401124:	48 89 df             	mov    %rbx,%rdi
  401127:	e8 04 1a 00 00       	call   402b30 <runtime::print_i64>
  40112c:	bf 0a 00 00 00       	mov    $0xa,%edi
  401131:	e8 1a 18 00 00       	call   402950 <runtime::print_byte>
  401136:	0f 0b                	ud2
  401138:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40113f:	00 

0000000000401140 <runtime::multi_pointer_slice_handle_error>:
  401140:	41 56                	push   %r14
  401142:	53                   	push   %rbx
  401143:	48 83 ec 28          	sub    $0x28,%rsp
  401147:	4c 89 cb             	mov    %r9,%rbx
  40114a:	4d 89 c6             	mov    %r8,%r14
  40114d:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401151:	c5 f8 11 44 24 18    	vmovups %xmm0,0x18(%rsp)
  401157:	48 89 3c 24          	mov    %rdi,(%rsp)
  40115b:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  401160:	89 54 24 10          	mov    %edx,0x10(%rsp)
  401164:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  401168:	48 89 e7             	mov    %rsp,%rdi
  40116b:	e8 20 1b 00 00       	call   402c90 <runtime::print_caller_location>
  401170:	bf 58 41 40 00       	mov    $0x404158,%edi
  401175:	be 17 00 00 00       	mov    $0x17,%esi
  40117a:	e8 31 14 00 00       	call   4025b0 <runtime::print_string>
  40117f:	4c 89 f7             	mov    %r14,%rdi
  401182:	e8 a9 19 00 00       	call   402b30 <runtime::print_i64>
  401187:	bf 70 41 40 00       	mov    $0x404170,%edi
  40118c:	be 01 00 00 00       	mov    $0x1,%esi
  401191:	e8 1a 14 00 00       	call   4025b0 <runtime::print_string>
  401196:	48 89 df             	mov    %rbx,%rdi
  401199:	e8 92 19 00 00       	call   402b30 <runtime::print_i64>
  40119e:	bf 0a 00 00 00       	mov    $0xa,%edi
  4011a3:	e8 a8 17 00 00       	call   402950 <runtime::print_byte>
  4011a8:	0f 0b                	ud2
  4011aa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004011b0 <runtime::default_assertion_failure_proc>:
  4011b0:	50                   	push   %rax
  4011b1:	e8 0a 00 00 00       	call   4011c0 <runtime::default_assertion_contextless_failure_proc>
  4011b6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4011bd:	00 00 00 

00000000004011c0 <runtime::default_assertion_contextless_failure_proc>:
  4011c0:	41 57                	push   %r15
  4011c2:	41 56                	push   %r14
  4011c4:	41 54                	push   %r12
  4011c6:	53                   	push   %rbx
  4011c7:	50                   	push   %rax
  4011c8:	48 89 cb             	mov    %rcx,%rbx
  4011cb:	49 89 d6             	mov    %rdx,%r14
  4011ce:	49 89 f7             	mov    %rsi,%r15
  4011d1:	49 89 fc             	mov    %rdi,%r12
  4011d4:	4c 89 c7             	mov    %r8,%rdi
  4011d7:	e8 b4 1a 00 00       	call   402c90 <runtime::print_caller_location>
  4011dc:	bf 7f 42 40 00       	mov    $0x40427f,%edi
  4011e1:	be 01 00 00 00       	mov    $0x1,%esi
  4011e6:	e8 c5 13 00 00       	call   4025b0 <runtime::print_string>
  4011eb:	4c 89 e7             	mov    %r12,%rdi
  4011ee:	4c 89 fe             	mov    %r15,%rsi
  4011f1:	e8 ba 13 00 00       	call   4025b0 <runtime::print_string>
  4011f6:	48 85 db             	test   %rbx,%rbx
  4011f9:	7e 1a                	jle    401215 <runtime::default_assertion_contextless_failure_proc+0x55>
  4011fb:	bf 81 42 40 00       	mov    $0x404281,%edi
  401200:	be 02 00 00 00       	mov    $0x2,%esi
  401205:	e8 a6 13 00 00       	call   4025b0 <runtime::print_string>
  40120a:	4c 89 f7             	mov    %r14,%rdi
  40120d:	48 89 de             	mov    %rbx,%rsi
  401210:	e8 9b 13 00 00       	call   4025b0 <runtime::print_string>
  401215:	bf 0a 00 00 00       	mov    $0xa,%edi
  40121a:	e8 31 17 00 00       	call   402950 <runtime::print_byte>
  40121f:	0f 0b                	ud2
  401221:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  401228:	0f 1f 84 00 00 00 00 
  40122f:	00 

0000000000401230 <runtime::bounds_check_error.handle_error-0>:
  401230:	41 56                	push   %r14
  401232:	53                   	push   %rbx
  401233:	48 83 ec 28          	sub    $0x28,%rsp
  401237:	48 89 d3             	mov    %rdx,%rbx
  40123a:	49 89 f6             	mov    %rsi,%r14
  40123d:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401241:	c5 f8 11 44 24 18    	vmovups %xmm0,0x18(%rsp)
  401247:	48 c7 04 24 84 42 40 	movq   $0x404284,(%rsp)
  40124e:	00 
  40124f:	48 c7 44 24 08 23 00 	movq   $0x23,0x8(%rsp)
  401256:	00 00 
  401258:	c7 44 24 10 51 04 00 	movl   $0x451,0x10(%rsp)
  40125f:	00 
  401260:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  401264:	48 89 e7             	mov    %rsp,%rdi
  401267:	e8 24 1a 00 00       	call   402c90 <runtime::print_caller_location>
  40126c:	bf 77 43 40 00       	mov    $0x404377,%edi
  401271:	be 07 00 00 00       	mov    $0x7,%esi
  401276:	e8 35 13 00 00       	call   4025b0 <runtime::print_string>
  40127b:	4c 89 f7             	mov    %r14,%rdi
  40127e:	e8 ad 18 00 00       	call   402b30 <runtime::print_i64>
  401283:	bf 72 41 40 00       	mov    $0x404172,%edi
  401288:	be 15 00 00 00       	mov    $0x15,%esi
  40128d:	e8 1e 13 00 00       	call   4025b0 <runtime::print_string>
  401292:	48 89 df             	mov    %rbx,%rdi
  401295:	e8 96 18 00 00       	call   402b30 <runtime::print_i64>
  40129a:	bf 0a 00 00 00       	mov    $0xa,%edi
  40129f:	e8 ac 16 00 00       	call   402950 <runtime::print_byte>
  4012a4:	0f 0b                	ud2
  4012a6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4012ad:	00 00 00 

00000000004012b0 <runtime::make_slice_error_loc.handle_error-0>:
  4012b0:	53                   	push   %rbx
  4012b1:	48 89 fb             	mov    %rdi,%rbx
  4012b4:	bf d0 42 40 00       	mov    $0x4042d0,%edi
  4012b9:	e8 d2 19 00 00       	call   402c90 <runtime::print_caller_location>
  4012be:	bf 7f 43 40 00       	mov    $0x40437f,%edi
  4012c3:	be 20 00 00 00       	mov    $0x20,%esi
  4012c8:	e8 e3 12 00 00       	call   4025b0 <runtime::print_string>
  4012cd:	48 89 df             	mov    %rbx,%rdi
  4012d0:	e8 5b 18 00 00       	call   402b30 <runtime::print_i64>
  4012d5:	bf 0a 00 00 00       	mov    $0xa,%edi
  4012da:	e8 71 16 00 00       	call   402950 <runtime::print_byte>
  4012df:	0f 0b                	ud2
  4012e1:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4012e8:	00 00 00 
  4012eb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004012f0 <_start>:
  4012f0:	f3 0f 1e fa          	endbr64
  4012f4:	31 ed                	xor    %ebp,%ebp
  4012f6:	49 89 d1             	mov    %rdx,%r9
  4012f9:	5e                   	pop    %rsi
  4012fa:	48 89 e2             	mov    %rsp,%rdx
  4012fd:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  401301:	50                   	push   %rax
  401302:	54                   	push   %rsp
  401303:	45 31 c0             	xor    %r8d,%r8d
  401306:	31 c9                	xor    %ecx,%ecx
  401308:	48 c7 c7 40 24 40 00 	mov    $0x402440,%rdi
  40130f:	ff 15 b3 4c 00 00    	call   *0x4cb3(%rip)        # 405fc8 <__libc_start_main@GLIBC_2.34>
  401315:	f4                   	hlt
  401316:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40131d:	00 00 00 

0000000000401320 <_dl_relocate_static_pie>:
  401320:	f3 0f 1e fa          	endbr64
  401324:	c3                   	ret
  401325:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40132c:	00 00 00 
  40132f:	90                   	nop

0000000000401330 <deregister_tm_clones>:
  401330:	b8 48 60 40 00       	mov    $0x406048,%eax
  401335:	48 3d 48 60 40 00    	cmp    $0x406048,%rax
  40133b:	74 13                	je     401350 <deregister_tm_clones+0x20>
  40133d:	48 8b 05 8c 4c 00 00 	mov    0x4c8c(%rip),%rax        # 405fd0 <_ITM_deregisterTMCloneTable@Base>
  401344:	48 85 c0             	test   %rax,%rax
  401347:	74 07                	je     401350 <deregister_tm_clones+0x20>
  401349:	bf 48 60 40 00       	mov    $0x406048,%edi
  40134e:	ff e0                	jmp    *%rax
  401350:	c3                   	ret
  401351:	0f 1f 40 00          	nopl   0x0(%rax)
  401355:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40135c:	00 00 00 00 

0000000000401360 <register_tm_clones>:
  401360:	be 48 60 40 00       	mov    $0x406048,%esi
  401365:	48 81 ee 48 60 40 00 	sub    $0x406048,%rsi
  40136c:	48 89 f0             	mov    %rsi,%rax
  40136f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401373:	48 c1 f8 03          	sar    $0x3,%rax
  401377:	48 01 c6             	add    %rax,%rsi
  40137a:	48 d1 fe             	sar    $1,%rsi
  40137d:	74 19                	je     401398 <register_tm_clones+0x38>
  40137f:	48 8b 05 5a 4c 00 00 	mov    0x4c5a(%rip),%rax        # 405fe0 <_ITM_registerTMCloneTable@Base>
  401386:	48 85 c0             	test   %rax,%rax
  401389:	74 0d                	je     401398 <register_tm_clones+0x38>
  40138b:	bf 48 60 40 00       	mov    $0x406048,%edi
  401390:	ff e0                	jmp    *%rax
  401392:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401398:	c3                   	ret
  401399:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004013a0 <__do_global_dtors_aux>:
  4013a0:	f3 0f 1e fa          	endbr64
  4013a4:	80 3d 9d 4c 00 00 00 	cmpb   $0x0,0x4c9d(%rip)        # 406048 <__TMC_END__>
  4013ab:	75 13                	jne    4013c0 <__do_global_dtors_aux+0x20>
  4013ad:	55                   	push   %rbp
  4013ae:	48 89 e5             	mov    %rsp,%rbp
  4013b1:	e8 7a ff ff ff       	call   401330 <deregister_tm_clones>
  4013b6:	c6 05 8b 4c 00 00 01 	movb   $0x1,0x4c8b(%rip)        # 406048 <__TMC_END__>
  4013bd:	5d                   	pop    %rbp
  4013be:	c3                   	ret
  4013bf:	90                   	nop
  4013c0:	c3                   	ret
  4013c1:	0f 1f 40 00          	nopl   0x0(%rax)
  4013c5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4013cc:	00 00 00 00 

00000000004013d0 <frame_dummy>:
  4013d0:	f3 0f 1e fa          	endbr64
  4013d4:	eb 8a                	jmp    401360 <register_tm_clones>
  4013d6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4013dd:	00 00 00 

00000000004013e0 <__$startup_runtime>:
  4013e0:	50                   	push   %rax
  4013e1:	eb 00                	jmp    4013e3 <__$startup_runtime+0x3>
  4013e3:	e8 08 00 00 00       	call   4013f0 <__$startup$os::args>
  4013e8:	58                   	pop    %rax
  4013e9:	c3                   	ret
  4013ea:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004013f0 <__$startup$os::args>:
  4013f0:	53                   	push   %rbx
  4013f1:	48 81 ec 80 00 00 00 	sub    $0x80,%rsp
  4013f8:	48 c7 44 24 10 60 1a 	movq   $0x401a60,0x10(%rsp)
  4013ff:	40 00 
  401401:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  401408:	00 00 
  40140a:	48 c7 44 24 20 40 21 	movq   $0x402140,0x20(%rsp)
  401411:	40 00 
  401413:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  40141a:	00 00 
  40141c:	48 8d 80 b8 ff ff ff 	lea    -0x48(%rax),%rax
  401423:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401428:	48 c7 44 24 30 b0 11 	movq   $0x4011b0,0x30(%rsp)
  40142f:	40 00 
  401431:	48 c7 44 24 38 e0 2c 	movq   $0x402ce0,0x38(%rsp)
  401438:	40 00 
  40143a:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40143e:	c5 f8 11 44 24 40    	vmovups %xmm0,0x40(%rsp)
  401444:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40144b:	00 00 
  40144d:	48 c7 44 24 58 a0 1e 	movq   $0x401ea0,0x58(%rsp)
  401454:	40 00 
  401456:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40145a:	c5 fc 11 44 24 60    	vmovups %ymm0,0x60(%rsp)
  401460:	48 8b 1d f1 4b 00 00 	mov    0x4bf1(%rip),%rbx        # 406058 <runtime::args__.1>
  401467:	48 85 db             	test   %rbx,%rbx
  40146a:	0f 88 b2 00 00 00    	js     401522 <__$startup$os::args+0x132>
  401470:	48 89 df             	mov    %rbx,%rdi
  401473:	48 c1 e7 04          	shl    $0x4,%rdi
  401477:	48 89 e1             	mov    %rsp,%rcx
  40147a:	4c 8d 44 24 10       	lea    0x10(%rsp),%r8
  40147f:	be 60 1a 40 00       	mov    $0x401a60,%esi
  401484:	31 d2                	xor    %edx,%edx
  401486:	c5 f8 77             	vzeroupper
  401489:	e8 42 11 00 00       	call   4025d0 <runtime::mem_alloc_bytes>
  40148e:	48 8b 04 24          	mov    (%rsp),%rax
  401492:	48 85 c0             	test   %rax,%rax
  401495:	48 0f 44 d8          	cmove  %rax,%rbx
  401499:	48 85 db             	test   %rbx,%rbx
  40149c:	74 63                	je     401501 <__$startup$os::args+0x111>
  40149e:	31 f6                	xor    %esi,%esi
  4014a0:	eb 2b                	jmp    4014cd <__$startup$os::args+0xdd>
  4014a2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4014a9:	1f 84 00 00 00 00 00 
  4014b0:	31 c9                	xor    %ecx,%ecx
  4014b2:	31 d2                	xor    %edx,%edx
  4014b4:	48 89 f7             	mov    %rsi,%rdi
  4014b7:	48 c1 e7 04          	shl    $0x4,%rdi
  4014bb:	48 01 c7             	add    %rax,%rdi
  4014be:	48 89 0f             	mov    %rcx,(%rdi)
  4014c1:	48 89 57 08          	mov    %rdx,0x8(%rdi)
  4014c5:	48 ff c6             	inc    %rsi
  4014c8:	48 39 de             	cmp    %rbx,%rsi
  4014cb:	74 34                	je     401501 <__$startup$os::args+0x111>
  4014cd:	48 8b 15 84 4b 00 00 	mov    0x4b84(%rip),%rdx        # 406058 <runtime::args__.1>
  4014d4:	48 39 d6             	cmp    %rdx,%rsi
  4014d7:	73 3f                	jae    401518 <__$startup$os::args+0x128>
  4014d9:	48 8b 0d 70 4b 00 00 	mov    0x4b70(%rip),%rcx        # 406050 <runtime::args__.0>
  4014e0:	48 8b 0c f1          	mov    (%rcx,%rsi,8),%rcx
  4014e4:	48 85 c9             	test   %rcx,%rcx
  4014e7:	74 c7                	je     4014b0 <__$startup$os::args+0xc0>
  4014e9:	48 89 ca             	mov    %rcx,%rdx
  4014ec:	0f 1f 40 00          	nopl   0x0(%rax)
  4014f0:	80 3a 00             	cmpb   $0x0,(%rdx)
  4014f3:	74 07                	je     4014fc <__$startup$os::args+0x10c>
  4014f5:	48 ff c2             	inc    %rdx
  4014f8:	75 f6                	jne    4014f0 <__$startup$os::args+0x100>
  4014fa:	31 d2                	xor    %edx,%edx
  4014fc:	48 29 ca             	sub    %rcx,%rdx
  4014ff:	eb b3                	jmp    4014b4 <__$startup$os::args+0xc4>
  401501:	48 89 05 58 4b 00 00 	mov    %rax,0x4b58(%rip)        # 406060 <os::args.0>
  401508:	48 89 1d 59 4b 00 00 	mov    %rbx,0x4b59(%rip)        # 406068 <os::args.1>
  40150f:	48 81 c4 80 00 00 00 	add    $0x80,%rsp
  401516:	5b                   	pop    %rbx
  401517:	c3                   	ret
  401518:	bf 22 00 00 00       	mov    $0x22,%edi
  40151d:	e8 0e fd ff ff       	call   401230 <runtime::bounds_check_error.handle_error-0>
  401522:	48 89 df             	mov    %rbx,%rdi
  401525:	c5 f8 77             	vzeroupper
  401528:	e8 83 fd ff ff       	call   4012b0 <runtime::make_slice_error_loc.handle_error-0>
  40152d:	0f 1f 00             	nopl   (%rax)

0000000000401530 <__$cleanup_runtime>:
  401530:	50                   	push   %rax
  401531:	eb 00                	jmp    401533 <__$cleanup_runtime+0x3>
  401533:	e8 08 00 00 00       	call   401540 <os::[os_linux.odin]::_delete_command_line_arguments>
  401538:	e8 a3 00 00 00       	call   4015e0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  40153d:	58                   	pop    %rax
  40153e:	c3                   	ret
  40153f:	90                   	nop

0000000000401540 <os::[os_linux.odin]::_delete_command_line_arguments>:
  401540:	48 83 ec 78          	sub    $0x78,%rsp
  401544:	48 c7 44 24 08 60 1a 	movq   $0x401a60,0x8(%rsp)
  40154b:	40 00 
  40154d:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  401554:	00 00 
  401556:	48 c7 44 24 18 40 21 	movq   $0x402140,0x18(%rsp)
  40155d:	40 00 
  40155f:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  401566:	00 00 
  401568:	48 8d 80 b8 ff ff ff 	lea    -0x48(%rax),%rax
  40156f:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401574:	48 c7 44 24 28 b0 11 	movq   $0x4011b0,0x28(%rsp)
  40157b:	40 00 
  40157d:	48 c7 44 24 30 e0 2c 	movq   $0x402ce0,0x30(%rsp)
  401584:	40 00 
  401586:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40158a:	c5 f8 11 44 24 38    	vmovups %xmm0,0x38(%rsp)
  401590:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  401597:	00 00 
  401599:	48 c7 44 24 50 a0 1e 	movq   $0x401ea0,0x50(%rsp)
  4015a0:	40 00 
  4015a2:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4015a6:	c5 fc 11 44 24 58    	vmovups %ymm0,0x58(%rsp)
  4015ac:	48 8b 3d ad 4a 00 00 	mov    0x4aad(%rip),%rdi        # 406060 <os::args.0>
  4015b3:	48 8b 35 ae 4a 00 00 	mov    0x4aae(%rip),%rsi        # 406068 <os::args.1>
  4015ba:	48 c1 e6 04          	shl    $0x4,%rsi
  4015be:	4c 8d 44 24 08       	lea    0x8(%rsp),%r8
  4015c3:	ba 60 1a 40 00       	mov    $0x401a60,%edx
  4015c8:	31 c9                	xor    %ecx,%ecx
  4015ca:	c5 f8 77             	vzeroupper
  4015cd:	e8 5e 16 00 00       	call   402c30 <runtime::mem_free_with_size>
  4015d2:	48 83 c4 78          	add    $0x78,%rsp
  4015d6:	c3                   	ret
  4015d7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4015de:	00 00 

00000000004015e0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  4015e0:	64 48 8b 3c 25 c8 ff 	mov    %fs:0xffffffffffffffc8,%rdi
  4015e7:	ff ff 
  4015e9:	48 85 ff             	test   %rdi,%rdi
  4015ec:	0f 84 c6 00 00 00    	je     4016b8 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0xd8>
  4015f2:	41 56                	push   %r14
  4015f4:	53                   	push   %rbx
  4015f5:	48 83 ec 78          	sub    $0x78,%rsp
  4015f9:	4c 8d 74 24 38       	lea    0x38(%rsp),%r14
  4015fe:	48 8d 5c 24 08       	lea    0x8(%rsp),%rbx
  401603:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40160a:	84 00 00 00 00 00 
  401610:	48 8b 07             	mov    (%rdi),%rax
  401613:	64 48 89 04 25 c8 ff 	mov    %rax,%fs:0xffffffffffffffc8
  40161a:	ff ff 
  40161c:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  401623:	00 00 
  401625:	48 8d 80 b8 ff ff ff 	lea    -0x48(%rax),%rax
  40162c:	48 8b 4f 28          	mov    0x28(%rdi),%rcx
  401630:	64 48 29 0c 25 d8 ff 	sub    %rcx,%fs:0xffffffffffffffd8
  401637:	ff ff 
  401639:	48 8b 77 08          	mov    0x8(%rdi),%rsi
  40163d:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  401641:	48 c7 44 24 18 40 21 	movq   $0x402140,0x18(%rsp)
  401648:	40 00 
  40164a:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40164f:	48 c7 44 24 28 b0 11 	movq   $0x4011b0,0x28(%rsp)
  401656:	40 00 
  401658:	48 c7 44 24 30 e0 2c 	movq   $0x402ce0,0x30(%rsp)
  40165f:	40 00 
  401661:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401665:	c4 c1 78 11 06       	vmovups %xmm0,(%r14)
  40166a:	49 c7 46 10 00 00 00 	movq   $0x0,0x10(%r14)
  401671:	00 
  401672:	48 c7 44 24 50 a0 1e 	movq   $0x401ea0,0x50(%rsp)
  401679:	40 00 
  40167b:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40167f:	c4 c1 7c 11 46 20    	vmovups %ymm0,0x20(%r14)
  401685:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40168a:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40168f:	b9 f0 41 40 00       	mov    $0x4041f0,%ecx
  401694:	49 89 d8             	mov    %rbx,%r8
  401697:	c5 f8 77             	vzeroupper
  40169a:	e8 41 14 00 00       	call   402ae0 <runtime::mem_free>
  40169f:	64 48 8b 3c 25 c8 ff 	mov    %fs:0xffffffffffffffc8,%rdi
  4016a6:	ff ff 
  4016a8:	48 85 ff             	test   %rdi,%rdi
  4016ab:	0f 85 5f ff ff ff    	jne    401610 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x30>
  4016b1:	48 83 c4 78          	add    $0x78,%rsp
  4016b5:	5b                   	pop    %rbx
  4016b6:	41 5e                	pop    %r14
  4016b8:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4016bc:	64 c5 fc 11 04 25 d0 	vmovups %ymm0,%fs:0xffffffffffffffd0
  4016c3:	ff ff ff 
  4016c6:	64 c5 fc 11 04 25 b8 	vmovups %ymm0,%fs:0xffffffffffffffb8
  4016cd:	ff ff ff 
  4016d0:	c5 f8 77             	vzeroupper
  4016d3:	c3                   	ret
  4016d4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4016db:	00 00 00 00 00 

00000000004016e0 <runtime::udivmod128>:
  4016e0:	55                   	push   %rbp
  4016e1:	41 57                	push   %r15
  4016e3:	41 56                	push   %r14
  4016e5:	53                   	push   %rbx
  4016e6:	48 85 f6             	test   %rsi,%rsi
  4016e9:	0f 84 6a 01 00 00    	je     401859 <runtime::udivmod128+0x179>
  4016ef:	48 85 d2             	test   %rdx,%rdx
  4016f2:	0f 84 83 01 00 00    	je     40187b <runtime::udivmod128+0x19b>
  4016f8:	48 85 c9             	test   %rcx,%rcx
  4016fb:	0f 84 b9 01 00 00    	je     4018ba <runtime::udivmod128+0x1da>
  401701:	f3 4c 0f bd d1       	lzcnt  %rcx,%r10
  401706:	f3 48 0f bd c6       	lzcnt  %rsi,%rax
  40170b:	41 29 c2             	sub    %eax,%r10d
  40170e:	41 83 fa 40          	cmp    $0x40,%r10d
  401712:	0f 83 8a 02 00 00    	jae    4019a2 <runtime::udivmod128+0x2c2>
  401718:	41 8d 42 01          	lea    0x1(%r10),%eax
  40171c:	83 f8 40             	cmp    $0x40,%eax
  40171f:	0f 84 47 02 00 00    	je     40196c <runtime::udivmod128+0x28c>
  401725:	c4 62 fb f7 ce       	shrx   %rax,%rsi,%r9
  40172a:	41 b3 3f             	mov    $0x3f,%r11b
  40172d:	45 28 d3             	sub    %r10b,%r11b
  401730:	c4 62 a1 f7 d6       	shlx   %r11,%rsi,%r10
  401735:	c4 e2 fb f7 f7       	shrx   %rax,%rdi,%rsi
  40173a:	4c 09 d6             	or     %r10,%rsi
  40173d:	c4 e2 a1 f7 ff       	shlx   %r11,%rdi,%rdi
  401742:	45 31 d2             	xor    %r10d,%r10d
  401745:	83 f8 01             	cmp    $0x1,%eax
  401748:	0f 84 32 02 00 00    	je     401980 <runtime::udivmod128+0x2a0>
  40174e:	89 c5                	mov    %eax,%ebp
  401750:	83 e5 fe             	and    $0xfffffffe,%ebp
  401753:	45 31 db             	xor    %r11d,%r11d
  401756:	31 db                	xor    %ebx,%ebx
  401758:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40175f:	00 
  401760:	49 0f a4 f1 01       	shld   $0x1,%rsi,%r9
  401765:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  40176a:	4c 0f a4 d7 01       	shld   $0x1,%r10,%rdi
  40176f:	4d 01 d2             	add    %r10,%r10
  401772:	4d 89 d6             	mov    %r10,%r14
  401775:	49 09 de             	or     %rbx,%r14
  401778:	4d 89 ca             	mov    %r9,%r10
  40177b:	49 f7 d2             	not    %r10
  40177e:	48 89 f3             	mov    %rsi,%rbx
  401781:	48 f7 d3             	not    %rbx
  401784:	48 01 d3             	add    %rdx,%rbx
  401787:	49 11 ca             	adc    %rcx,%r10
  40178a:	4c 89 d3             	mov    %r10,%rbx
  40178d:	48 c1 fb 3f          	sar    $0x3f,%rbx
  401791:	49 89 df             	mov    %rbx,%r15
  401794:	49 21 cf             	and    %rcx,%r15
  401797:	48 21 d3             	and    %rdx,%rbx
  40179a:	48 29 de             	sub    %rbx,%rsi
  40179d:	4d 19 f9             	sbb    %r15,%r9
  4017a0:	49 0f a4 f1 01       	shld   $0x1,%rsi,%r9
  4017a5:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  4017aa:	4c 0f a4 f7 01       	shld   $0x1,%r14,%rdi
  4017af:	4d 0f ac f2 3f       	shrd   $0x3f,%r14,%r10
  4017b4:	4d 89 ce             	mov    %r9,%r14
  4017b7:	49 f7 d6             	not    %r14
  4017ba:	48 89 f3             	mov    %rsi,%rbx
  4017bd:	48 f7 d3             	not    %rbx
  4017c0:	48 01 d3             	add    %rdx,%rbx
  4017c3:	49 11 ce             	adc    %rcx,%r14
  4017c6:	4c 89 f3             	mov    %r14,%rbx
  4017c9:	48 c1 eb 3f          	shr    $0x3f,%rbx
  4017cd:	49 c1 fe 3f          	sar    $0x3f,%r14
  4017d1:	4d 89 f7             	mov    %r14,%r15
  4017d4:	49 21 cf             	and    %rcx,%r15
  4017d7:	49 21 d6             	and    %rdx,%r14
  4017da:	4c 29 f6             	sub    %r14,%rsi
  4017dd:	4d 19 f9             	sbb    %r15,%r9
  4017e0:	83 c5 fe             	add    $0xfffffffe,%ebp
  4017e3:	0f 85 77 ff ff ff    	jne    401760 <runtime::udivmod128+0x80>
  4017e9:	49 89 f6             	mov    %rsi,%r14
  4017ec:	a8 01                	test   $0x1,%al
  4017ee:	74 42                	je     401832 <runtime::udivmod128+0x152>
  4017f0:	49 0f a4 f1 01       	shld   $0x1,%rsi,%r9
  4017f5:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  4017fa:	4c 0f a4 d7 01       	shld   $0x1,%r10,%rdi
  4017ff:	4e 8d 14 53          	lea    (%rbx,%r10,2),%r10
  401803:	4c 89 c8             	mov    %r9,%rax
  401806:	48 f7 d0             	not    %rax
  401809:	49 89 f3             	mov    %rsi,%r11
  40180c:	49 f7 d3             	not    %r11
  40180f:	49 01 d3             	add    %rdx,%r11
  401812:	48 11 c8             	adc    %rcx,%rax
  401815:	48 89 c3             	mov    %rax,%rbx
  401818:	48 c1 eb 3f          	shr    $0x3f,%rbx
  40181c:	45 31 db             	xor    %r11d,%r11d
  40181f:	48 c1 f8 3f          	sar    $0x3f,%rax
  401823:	48 21 c1             	and    %rax,%rcx
  401826:	48 21 d0             	and    %rdx,%rax
  401829:	48 29 c6             	sub    %rax,%rsi
  40182c:	49 19 c9             	sbb    %rcx,%r9
  40182f:	49 89 f6             	mov    %rsi,%r14
  401832:	48 89 fe             	mov    %rdi,%rsi
  401835:	4b 8d 3c 12          	lea    (%r10,%r10,1),%rdi
  401839:	48 09 df             	or     %rbx,%rdi
  40183c:	4c 0f a4 d6 01       	shld   $0x1,%r10,%rsi
  401841:	4c 09 de             	or     %r11,%rsi
  401844:	4d 85 c0             	test   %r8,%r8
  401847:	0f 84 80 01 00 00    	je     4019cd <runtime::udivmod128+0x2ed>
  40184d:	4d 89 30             	mov    %r14,(%r8)
  401850:	4d 89 48 08          	mov    %r9,0x8(%r8)
  401854:	e9 74 01 00 00       	jmp    4019cd <runtime::udivmod128+0x2ed>
  401859:	48 85 c9             	test   %rcx,%rcx
  40185c:	0f 84 a9 00 00 00    	je     40190b <runtime::udivmod128+0x22b>
  401862:	4d 85 c0             	test   %r8,%r8
  401865:	0f 84 43 01 00 00    	je     4019ae <runtime::udivmod128+0x2ce>
  40186b:	49 89 38             	mov    %rdi,(%r8)
  40186e:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  401875:	00 
  401876:	e9 33 01 00 00       	jmp    4019ae <runtime::udivmod128+0x2ce>
  40187b:	48 85 c9             	test   %rcx,%rcx
  40187e:	0f 84 d8 01 00 00    	je     401a5c <runtime::udivmod128+0x37c>
  401884:	48 85 ff             	test   %rdi,%rdi
  401887:	0f 84 ab 00 00 00    	je     401938 <runtime::udivmod128+0x258>
  40188d:	48 8d 41 ff          	lea    -0x1(%rcx),%rax
  401891:	48 85 c1             	test   %rax,%rcx
  401894:	0f 85 f5 00 00 00    	jne    40198f <runtime::udivmod128+0x2af>
  40189a:	4d 85 c0             	test   %r8,%r8
  40189d:	74 0a                	je     4018a9 <runtime::udivmod128+0x1c9>
  40189f:	48 21 f0             	and    %rsi,%rax
  4018a2:	49 89 38             	mov    %rdi,(%r8)
  4018a5:	49 89 40 08          	mov    %rax,0x8(%r8)
  4018a9:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  4018ae:	c4 e2 fb f7 fe       	shrx   %rax,%rsi,%rdi
  4018b3:	31 f6                	xor    %esi,%esi
  4018b5:	e9 13 01 00 00       	jmp    4019cd <runtime::udivmod128+0x2ed>
  4018ba:	48 8d 42 ff          	lea    -0x1(%rdx),%rax
  4018be:	48 85 c2             	test   %rax,%rdx
  4018c1:	0f 85 90 00 00 00    	jne    401957 <runtime::udivmod128+0x277>
  4018c7:	4d 85 c0             	test   %r8,%r8
  4018ca:	74 0e                	je     4018da <runtime::udivmod128+0x1fa>
  4018cc:	48 21 f8             	and    %rdi,%rax
  4018cf:	49 89 00             	mov    %rax,(%r8)
  4018d2:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  4018d9:	00 
  4018da:	48 83 fa 01          	cmp    $0x1,%rdx
  4018de:	0f 84 e9 00 00 00    	je     4019cd <runtime::udivmod128+0x2ed>
  4018e4:	f3 48 0f bc c2       	tzcnt  %rdx,%rax
  4018e9:	89 c1                	mov    %eax,%ecx
  4018eb:	f6 d9                	neg    %cl
  4018ed:	c4 e2 f1 f7 ce       	shlx   %rcx,%rsi,%rcx
  4018f2:	c4 e2 fb f7 f6       	shrx   %rax,%rsi,%rsi
  4018f7:	48 85 c0             	test   %rax,%rax
  4018fa:	48 0f 44 c8          	cmove  %rax,%rcx
  4018fe:	c4 e2 fb f7 ff       	shrx   %rax,%rdi,%rdi
  401903:	48 09 cf             	or     %rcx,%rdi
  401906:	e9 c2 00 00 00       	jmp    4019cd <runtime::udivmod128+0x2ed>
  40190b:	4d 85 c0             	test   %r8,%r8
  40190e:	0f 84 a0 00 00 00    	je     4019b4 <runtime::udivmod128+0x2d4>
  401914:	48 85 d2             	test   %rdx,%rdx
  401917:	0f 84 3f 01 00 00    	je     401a5c <runtime::udivmod128+0x37c>
  40191d:	48 89 d1             	mov    %rdx,%rcx
  401920:	48 89 f8             	mov    %rdi,%rax
  401923:	31 d2                	xor    %edx,%edx
  401925:	48 f7 f1             	div    %rcx
  401928:	49 89 10             	mov    %rdx,(%r8)
  40192b:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  401932:	00 
  401933:	e9 88 00 00 00       	jmp    4019c0 <runtime::udivmod128+0x2e0>
  401938:	48 89 f0             	mov    %rsi,%rax
  40193b:	31 d2                	xor    %edx,%edx
  40193d:	48 f7 f1             	div    %rcx
  401940:	48 89 c7             	mov    %rax,%rdi
  401943:	4d 85 c0             	test   %r8,%r8
  401946:	74 0b                	je     401953 <runtime::udivmod128+0x273>
  401948:	49 89 50 08          	mov    %rdx,0x8(%r8)
  40194c:	49 c7 00 00 00 00 00 	movq   $0x0,(%r8)
  401953:	31 f6                	xor    %esi,%esi
  401955:	eb 76                	jmp    4019cd <runtime::udivmod128+0x2ed>
  401957:	f3 48 0f bd c2       	lzcnt  %rdx,%rax
  40195c:	f3 4c 0f bd ce       	lzcnt  %rsi,%r9
  401961:	44 29 c8             	sub    %r9d,%eax
  401964:	83 c0 41             	add    $0x41,%eax
  401967:	83 f8 40             	cmp    $0x40,%eax
  40196a:	75 6e                	jne    4019da <runtime::udivmod128+0x2fa>
  40196c:	b8 40 00 00 00       	mov    $0x40,%eax
  401971:	45 31 d2             	xor    %r10d,%r10d
  401974:	45 31 c9             	xor    %r9d,%r9d
  401977:	83 f8 01             	cmp    $0x1,%eax
  40197a:	0f 85 ce fd ff ff    	jne    40174e <runtime::udivmod128+0x6e>
  401980:	31 db                	xor    %ebx,%ebx
  401982:	a8 01                	test   $0x1,%al
  401984:	0f 85 66 fe ff ff    	jne    4017f0 <runtime::udivmod128+0x110>
  40198a:	e9 a3 fe ff ff       	jmp    401832 <runtime::udivmod128+0x152>
  40198f:	f3 4c 0f bd c9       	lzcnt  %rcx,%r9
  401994:	f3 48 0f bd c6       	lzcnt  %rsi,%rax
  401999:	41 29 c1             	sub    %eax,%r9d
  40199c:	41 83 f9 3f          	cmp    $0x3f,%r9d
  4019a0:	72 5c                	jb     4019fe <runtime::udivmod128+0x31e>
  4019a2:	4d 85 c0             	test   %r8,%r8
  4019a5:	74 07                	je     4019ae <runtime::udivmod128+0x2ce>
  4019a7:	49 89 38             	mov    %rdi,(%r8)
  4019aa:	49 89 70 08          	mov    %rsi,0x8(%r8)
  4019ae:	31 ff                	xor    %edi,%edi
  4019b0:	31 f6                	xor    %esi,%esi
  4019b2:	eb 19                	jmp    4019cd <runtime::udivmod128+0x2ed>
  4019b4:	48 89 d1             	mov    %rdx,%rcx
  4019b7:	48 85 d2             	test   %rdx,%rdx
  4019ba:	0f 84 9c 00 00 00    	je     401a5c <runtime::udivmod128+0x37c>
  4019c0:	31 f6                	xor    %esi,%esi
  4019c2:	48 89 f8             	mov    %rdi,%rax
  4019c5:	31 d2                	xor    %edx,%edx
  4019c7:	48 f7 f1             	div    %rcx
  4019ca:	48 89 c7             	mov    %rax,%rdi
  4019cd:	48 89 f8             	mov    %rdi,%rax
  4019d0:	48 89 f2             	mov    %rsi,%rdx
  4019d3:	5b                   	pop    %rbx
  4019d4:	41 5e                	pop    %r14
  4019d6:	41 5f                	pop    %r15
  4019d8:	5d                   	pop    %rbp
  4019d9:	c3                   	ret
  4019da:	49 89 cb             	mov    %rcx,%r11
  4019dd:	89 c1                	mov    %eax,%ecx
  4019df:	73 54                	jae    401a35 <runtime::udivmod128+0x355>
  4019e1:	f6 d9                	neg    %cl
  4019e3:	c4 e2 f1 f7 df       	shlx   %rcx,%rdi,%rbx
  4019e8:	c4 62 fb f7 ce       	shrx   %rax,%rsi,%r9
  4019ed:	89 c1                	mov    %eax,%ecx
  4019ef:	48 0f ad f7          	shrd   %cl,%rsi,%rdi
  4019f3:	45 31 d2             	xor    %r10d,%r10d
  4019f6:	48 89 fe             	mov    %rdi,%rsi
  4019f9:	48 89 df             	mov    %rbx,%rdi
  4019fc:	eb 4d                	jmp    401a4b <runtime::udivmod128+0x36b>
  4019fe:	41 8d 41 01          	lea    0x1(%r9),%eax
  401a02:	41 f6 d1             	not    %r9b
  401a05:	c4 62 b1 f7 df       	shlx   %r9,%rdi,%r11
  401a0a:	c4 62 fb f7 ce       	shrx   %rax,%rsi,%r9
  401a0f:	48 89 fb             	mov    %rdi,%rbx
  401a12:	48 89 cf             	mov    %rcx,%rdi
  401a15:	89 c1                	mov    %eax,%ecx
  401a17:	48 0f ad f3          	shrd   %cl,%rsi,%rbx
  401a1b:	48 89 f9             	mov    %rdi,%rcx
  401a1e:	45 31 d2             	xor    %r10d,%r10d
  401a21:	48 89 de             	mov    %rbx,%rsi
  401a24:	4c 89 df             	mov    %r11,%rdi
  401a27:	83 f8 01             	cmp    $0x1,%eax
  401a2a:	0f 85 1e fd ff ff    	jne    40174e <runtime::udivmod128+0x6e>
  401a30:	e9 4b ff ff ff       	jmp    401980 <runtime::udivmod128+0x2a0>
  401a35:	f6 d9                	neg    %cl
  401a37:	c4 62 f1 f7 d7       	shlx   %rcx,%rdi,%r10
  401a3c:	8d 48 c0             	lea    -0x40(%rax),%ecx
  401a3f:	48 0f ad f7          	shrd   %cl,%rsi,%rdi
  401a43:	c4 e2 fb f7 f6       	shrx   %rax,%rsi,%rsi
  401a48:	45 31 c9             	xor    %r9d,%r9d
  401a4b:	4c 89 d9             	mov    %r11,%rcx
  401a4e:	83 f8 01             	cmp    $0x1,%eax
  401a51:	0f 84 29 ff ff ff    	je     401980 <runtime::udivmod128+0x2a0>
  401a57:	e9 f2 fc ff ff       	jmp    40174e <runtime::udivmod128+0x6e>
  401a5c:	0f 0b                	ud2
  401a5e:	66 90                	xchg   %ax,%ax

0000000000401a60 <runtime::heap_allocator_proc>:
  401a60:	55                   	push   %rbp
  401a61:	41 57                	push   %r15
  401a63:	41 56                	push   %r14
  401a65:	41 55                	push   %r13
  401a67:	41 54                	push   %r12
  401a69:	53                   	push   %rbx
  401a6a:	50                   	push   %rax
  401a6b:	4c 8b 74 24 48       	mov    0x48(%rsp),%r14
  401a70:	40 80 fe 07          	cmp    $0x7,%sil
  401a74:	0f 87 e9 00 00 00    	ja     401b63 <runtime::heap_allocator_proc+0x103>
  401a7a:	40 0f b6 c6          	movzbl %sil,%eax
  401a7e:	ff 24 c5 10 40 40 00 	jmp    *0x404010(,%rax,8)
  401a85:	48 83 f9 09          	cmp    $0x9,%rcx
  401a89:	bb 08 00 00 00       	mov    $0x8,%ebx
  401a8e:	48 0f 4d d9          	cmovge %rcx,%rbx
  401a92:	48 8d 04 1a          	lea    (%rdx,%rbx,1),%rax
  401a96:	48 83 c0 07          	add    $0x7,%rax
  401a9a:	40 b5 01             	mov    $0x1,%bpl
  401a9d:	48 85 c0             	test   %rax,%rax
  401aa0:	0f 8e 5c 01 00 00    	jle    401c02 <runtime::heap_allocator_proc+0x1a2>
  401aa6:	49 89 d7             	mov    %rdx,%r15
  401aa9:	40 84 f6             	test   %sil,%sil
  401aac:	0f 84 3a 01 00 00    	je     401bec <runtime::heap_allocator_proc+0x18c>
  401ab2:	48 89 c7             	mov    %rax,%rdi
  401ab5:	e8 b6 f5 ff ff       	call   401070 <malloc@plt>
  401aba:	48 85 c0             	test   %rax,%rax
  401abd:	0f 84 3f 01 00 00    	je     401c02 <runtime::heap_allocator_proc+0x1a2>
  401ac3:	48 8d 0c 18          	lea    (%rax,%rbx,1),%rcx
  401ac7:	48 83 c1 07          	add    $0x7,%rcx
  401acb:	48 f7 db             	neg    %rbx
  401ace:	48 21 cb             	and    %rcx,%rbx
  401ad1:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401ad5:	4c 89 f8             	mov    %r15,%rax
  401ad8:	48 c1 f8 3f          	sar    $0x3f,%rax
  401adc:	c4 c2 f8 f2 c7       	andn   %r15,%rax,%rax
  401ae1:	31 ed                	xor    %ebp,%ebp
  401ae3:	e9 1e 01 00 00       	jmp    401c06 <runtime::heap_allocator_proc+0x1a6>
  401ae8:	4d 85 c0             	test   %r8,%r8
  401aeb:	0f 84 82 00 00 00    	je     401b73 <runtime::heap_allocator_proc+0x113>
  401af1:	48 83 f9 09          	cmp    $0x9,%rcx
  401af5:	bb 08 00 00 00       	mov    $0x8,%ebx
  401afa:	48 0f 4d d9          	cmovge %rcx,%rbx
  401afe:	48 8d 44 1a 07       	lea    0x7(%rdx,%rbx,1),%rax
  401b03:	0f 8c a3 00 00 00    	jl     401bac <runtime::heap_allocator_proc+0x14c>
  401b09:	48 85 c0             	test   %rax,%rax
  401b0c:	0f 8e f5 01 00 00    	jle    401d07 <runtime::heap_allocator_proc+0x2a7>
  401b12:	4d 89 c5             	mov    %r8,%r13
  401b15:	4d 89 cf             	mov    %r9,%r15
  401b18:	49 89 d4             	mov    %rdx,%r12
  401b1b:	89 f5                	mov    %esi,%ebp
  401b1d:	40 80 fe 03          	cmp    $0x3,%sil
  401b21:	0f 85 1f 01 00 00    	jne    401c46 <runtime::heap_allocator_proc+0x1e6>
  401b27:	bf 01 00 00 00       	mov    $0x1,%edi
  401b2c:	48 89 c6             	mov    %rax,%rsi
  401b2f:	e8 1c f5 ff ff       	call   401050 <calloc@plt>
  401b34:	e9 15 01 00 00       	jmp    401c4e <runtime::heap_allocator_proc+0x1ee>
  401b39:	c5 f9 ef c0          	vpxor  %xmm0,%xmm0,%xmm0
  401b3d:	c4 c1 7a 7f 06       	vmovdqu %xmm0,(%r14)
  401b42:	40 b5 04             	mov    $0x4,%bpl
  401b45:	e9 35 03 00 00       	jmp    401e7f <runtime::heap_allocator_proc+0x41f>
  401b4a:	4d 85 c0             	test   %r8,%r8
  401b4d:	74 14                	je     401b63 <runtime::heap_allocator_proc+0x103>
  401b4f:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401b53:	e8 d8 f4 ff ff       	call   401030 <free@plt>
  401b58:	eb 09                	jmp    401b63 <runtime::heap_allocator_proc+0x103>
  401b5a:	4d 85 c0             	test   %r8,%r8
  401b5d:	74 04                	je     401b63 <runtime::heap_allocator_proc+0x103>
  401b5f:	41 c6 00 db          	movb   $0xdb,(%r8)
  401b63:	c5 f9 ef c0          	vpxor  %xmm0,%xmm0,%xmm0
  401b67:	c4 c1 7a 7f 06       	vmovdqu %xmm0,(%r14)
  401b6c:	31 ed                	xor    %ebp,%ebp
  401b6e:	e9 0c 03 00 00       	jmp    401e7f <runtime::heap_allocator_proc+0x41f>
  401b73:	48 83 f9 09          	cmp    $0x9,%rcx
  401b77:	bb 08 00 00 00       	mov    $0x8,%ebx
  401b7c:	48 0f 4d d9          	cmovge %rcx,%rbx
  401b80:	48 8d 04 1a          	lea    (%rdx,%rbx,1),%rax
  401b84:	48 83 c0 07          	add    $0x7,%rax
  401b88:	40 b5 01             	mov    $0x1,%bpl
  401b8b:	48 85 c0             	test   %rax,%rax
  401b8e:	0f 8e 7f 01 00 00    	jle    401d13 <runtime::heap_allocator_proc+0x2b3>
  401b94:	49 89 d7             	mov    %rdx,%r15
  401b97:	40 80 fe 03          	cmp    $0x3,%sil
  401b9b:	75 75                	jne    401c12 <runtime::heap_allocator_proc+0x1b2>
  401b9d:	bf 01 00 00 00       	mov    $0x1,%edi
  401ba2:	48 89 c6             	mov    %rax,%rsi
  401ba5:	e8 a6 f4 ff ff       	call   401050 <calloc@plt>
  401baa:	eb 6e                	jmp    401c1a <runtime::heap_allocator_proc+0x1ba>
  401bac:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401bb0:	89 f5                	mov    %esi,%ebp
  401bb2:	48 89 c6             	mov    %rax,%rsi
  401bb5:	49 89 d7             	mov    %rdx,%r15
  401bb8:	4d 89 cd             	mov    %r9,%r13
  401bbb:	4d 89 c4             	mov    %r8,%r12
  401bbe:	e8 bd f4 ff ff       	call   401080 <realloc@plt>
  401bc3:	4d 89 e0             	mov    %r12,%r8
  401bc6:	48 85 c0             	test   %rax,%rax
  401bc9:	0f 84 38 01 00 00    	je     401d07 <runtime::heap_allocator_proc+0x2a7>
  401bcf:	4c 89 ef             	mov    %r13,%rdi
  401bd2:	4c 89 f9             	mov    %r15,%rcx
  401bd5:	48 8d 14 18          	lea    (%rax,%rbx,1),%rdx
  401bd9:	48 83 c2 07          	add    $0x7,%rdx
  401bdd:	48 f7 db             	neg    %rbx
  401be0:	48 21 d3             	and    %rdx,%rbx
  401be3:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401be7:	e9 ba 00 00 00       	jmp    401ca6 <runtime::heap_allocator_proc+0x246>
  401bec:	bf 01 00 00 00       	mov    $0x1,%edi
  401bf1:	48 89 c6             	mov    %rax,%rsi
  401bf4:	e8 57 f4 ff ff       	call   401050 <calloc@plt>
  401bf9:	48 85 c0             	test   %rax,%rax
  401bfc:	0f 85 c1 fe ff ff    	jne    401ac3 <runtime::heap_allocator_proc+0x63>
  401c02:	31 db                	xor    %ebx,%ebx
  401c04:	31 c0                	xor    %eax,%eax
  401c06:	49 89 1e             	mov    %rbx,(%r14)
  401c09:	49 89 46 08          	mov    %rax,0x8(%r14)
  401c0d:	e9 6d 02 00 00       	jmp    401e7f <runtime::heap_allocator_proc+0x41f>
  401c12:	48 89 c7             	mov    %rax,%rdi
  401c15:	e8 56 f4 ff ff       	call   401070 <malloc@plt>
  401c1a:	48 85 c0             	test   %rax,%rax
  401c1d:	0f 84 f0 00 00 00    	je     401d13 <runtime::heap_allocator_proc+0x2b3>
  401c23:	48 8d 0c 18          	lea    (%rax,%rbx,1),%rcx
  401c27:	48 83 c1 07          	add    $0x7,%rcx
  401c2b:	48 f7 db             	neg    %rbx
  401c2e:	48 21 cb             	and    %rcx,%rbx
  401c31:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401c35:	4c 89 f8             	mov    %r15,%rax
  401c38:	48 c1 f8 3f          	sar    $0x3f,%rax
  401c3c:	c4 c2 f8 f2 d7       	andn   %r15,%rax,%rdx
  401c41:	e9 30 02 00 00       	jmp    401e76 <runtime::heap_allocator_proc+0x416>
  401c46:	48 89 c7             	mov    %rax,%rdi
  401c49:	e8 22 f4 ff ff       	call   401070 <malloc@plt>
  401c4e:	48 85 c0             	test   %rax,%rax
  401c51:	4d 89 e8             	mov    %r13,%r8
  401c54:	0f 84 ad 00 00 00    	je     401d07 <runtime::heap_allocator_proc+0x2a7>
  401c5a:	4c 89 e2             	mov    %r12,%rdx
  401c5d:	48 8d 34 18          	lea    (%rax,%rbx,1),%rsi
  401c61:	48 83 c6 07          	add    $0x7,%rsi
  401c65:	48 f7 db             	neg    %rbx
  401c68:	48 21 f3             	and    %rsi,%rbx
  401c6b:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401c6f:	4d 39 e7             	cmp    %r12,%r15
  401c72:	49 0f 4c d7          	cmovl  %r15,%rdx
  401c76:	49 39 d8             	cmp    %rbx,%r8
  401c79:	0f 95 c0             	setne  %al
  401c7c:	48 85 d2             	test   %rdx,%rdx
  401c7f:	0f 9f c1             	setg   %cl
  401c82:	20 c1                	and    %al,%cl
  401c84:	80 f9 01             	cmp    $0x1,%cl
  401c87:	75 0e                	jne    401c97 <runtime::heap_allocator_proc+0x237>
  401c89:	48 89 df             	mov    %rbx,%rdi
  401c8c:	4c 89 c6             	mov    %r8,%rsi
  401c8f:	e8 cc f3 ff ff       	call   401060 <memcpy@plt>
  401c94:	4d 89 e8             	mov    %r13,%r8
  401c97:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401c9b:	e8 90 f3 ff ff       	call   401030 <free@plt>
  401ca0:	4c 89 e1             	mov    %r12,%rcx
  401ca3:	4c 89 ff             	mov    %r15,%rdi
  401ca6:	40 80 fd 03          	cmp    $0x3,%bpl
  401caa:	0f 94 c0             	sete   %al
  401cad:	48 89 ca             	mov    %rcx,%rdx
  401cb0:	48 c1 fa 3f          	sar    $0x3f,%rdx
  401cb4:	c4 e2 e8 f2 d1       	andn   %rcx,%rdx,%rdx
  401cb9:	48 29 f9             	sub    %rdi,%rcx
  401cbc:	40 0f 9f c6          	setg   %sil
  401cc0:	40 20 c6             	and    %al,%sil
  401cc3:	40 80 fe 01          	cmp    $0x1,%sil
  401cc7:	0f 85 a9 01 00 00    	jne    401e76 <runtime::heap_allocator_proc+0x416>
  401ccd:	48 85 ff             	test   %rdi,%rdi
  401cd0:	0f 88 ff 00 00 00    	js     401dd5 <runtime::heap_allocator_proc+0x375>
  401cd6:	48 85 c9             	test   %rcx,%rcx
  401cd9:	0f 8e 97 01 00 00    	jle    401e76 <runtime::heap_allocator_proc+0x416>
  401cdf:	48 8d 04 3b          	lea    (%rbx,%rdi,1),%rax
  401ce3:	89 ce                	mov    %ecx,%esi
  401ce5:	83 e6 07             	and    $0x7,%esi
  401ce8:	48 83 f9 08          	cmp    $0x8,%rcx
  401cec:	0f 82 02 01 00 00    	jb     401df4 <runtime::heap_allocator_proc+0x394>
  401cf2:	49 89 c8             	mov    %rcx,%r8
  401cf5:	49 c1 e8 03          	shr    $0x3,%r8
  401cf9:	48 83 f9 1f          	cmp    $0x1f,%rcx
  401cfd:	77 1d                	ja     401d1c <runtime::heap_allocator_proc+0x2bc>
  401cff:	45 31 c9             	xor    %r9d,%r9d
  401d02:	e9 d6 00 00 00       	jmp    401ddd <runtime::heap_allocator_proc+0x37d>
  401d07:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401d0b:	e8 20 f3 ff ff       	call   401030 <free@plt>
  401d10:	40 b5 01             	mov    $0x1,%bpl
  401d13:	31 db                	xor    %ebx,%ebx
  401d15:	31 d2                	xor    %edx,%edx
  401d17:	e9 5c 01 00 00       	jmp    401e78 <runtime::heap_allocator_proc+0x418>
  401d1c:	49 ba f0 ff ff ff ff 	movabs $0xffffffffffffff0,%r10
  401d23:	ff ff 0f 
  401d26:	48 81 f9 80 00 00 00 	cmp    $0x80,%rcx
  401d2d:	73 05                	jae    401d34 <runtime::heap_allocator_proc+0x2d4>
  401d2f:	45 31 c9             	xor    %r9d,%r9d
  401d32:	eb 71                	jmp    401da5 <runtime::heap_allocator_proc+0x345>
  401d34:	4d 89 c1             	mov    %r8,%r9
  401d37:	4d 21 d1             	and    %r10,%r9
  401d3a:	48 01 df             	add    %rbx,%rdi
  401d3d:	48 83 c7 60          	add    $0x60,%rdi
  401d41:	45 31 db             	xor    %r11d,%r11d
  401d44:	c5 f9 ef c0          	vpxor  %xmm0,%xmm0,%xmm0
  401d48:	c5 f5 76 c9          	vpcmpeqd %ymm1,%ymm1,%ymm1
  401d4c:	c4 a2 7d 29 54 df a0 	vpcmpeqq -0x60(%rdi,%r11,8),%ymm0,%ymm2
  401d53:	c5 ed ef d1          	vpxor  %ymm1,%ymm2,%ymm2
  401d57:	c4 a2 7d 29 5c df c0 	vpcmpeqq -0x40(%rdi,%r11,8),%ymm0,%ymm3
  401d5e:	c5 e5 ef d9          	vpxor  %ymm1,%ymm3,%ymm3
  401d62:	c4 a2 7d 29 64 df e0 	vpcmpeqq -0x20(%rdi,%r11,8),%ymm0,%ymm4
  401d69:	c5 dd ef e1          	vpxor  %ymm1,%ymm4,%ymm4
  401d6d:	c4 a2 7d 29 2c df    	vpcmpeqq (%rdi,%r11,8),%ymm0,%ymm5
  401d73:	c4 a2 ed 8e 44 df a0 	vpmaskmovq %ymm0,%ymm2,-0x60(%rdi,%r11,8)
  401d7a:	c4 a2 e5 8e 44 df c0 	vpmaskmovq %ymm0,%ymm3,-0x40(%rdi,%r11,8)
  401d81:	c4 a2 dd 8e 44 df e0 	vpmaskmovq %ymm0,%ymm4,-0x20(%rdi,%r11,8)
  401d88:	c5 d5 ef d1          	vpxor  %ymm1,%ymm5,%ymm2
  401d8c:	c4 a2 ed 8e 04 df    	vpmaskmovq %ymm0,%ymm2,(%rdi,%r11,8)
  401d92:	49 83 c3 10          	add    $0x10,%r11
  401d96:	4d 39 d9             	cmp    %r11,%r9
  401d99:	75 b1                	jne    401d4c <runtime::heap_allocator_proc+0x2ec>
  401d9b:	4d 39 c8             	cmp    %r9,%r8
  401d9e:	74 54                	je     401df4 <runtime::heap_allocator_proc+0x394>
  401da0:	f6 c1 60             	test   $0x60,%cl
  401da3:	74 38                	je     401ddd <runtime::heap_allocator_proc+0x37d>
  401da5:	4c 89 cf             	mov    %r9,%rdi
  401da8:	49 83 c2 0c          	add    $0xc,%r10
  401dac:	4d 89 d1             	mov    %r10,%r9
  401daf:	4d 21 c1             	and    %r8,%r9
  401db2:	c5 f9 ef c0          	vpxor  %xmm0,%xmm0,%xmm0
  401db6:	c5 f5 76 c9          	vpcmpeqd %ymm1,%ymm1,%ymm1
  401dba:	c4 e2 7d 29 14 f8    	vpcmpeqq (%rax,%rdi,8),%ymm0,%ymm2
  401dc0:	c5 ed ef d1          	vpxor  %ymm1,%ymm2,%ymm2
  401dc4:	c4 e2 ed 8e 04 f8    	vpmaskmovq %ymm0,%ymm2,(%rax,%rdi,8)
  401dca:	48 83 c7 04          	add    $0x4,%rdi
  401dce:	49 39 f9             	cmp    %rdi,%r9
  401dd1:	75 e7                	jne    401dba <runtime::heap_allocator_proc+0x35a>
  401dd3:	eb 1a                	jmp    401def <runtime::heap_allocator_proc+0x38f>
  401dd5:	48 89 d6             	mov    %rdx,%rsi
  401dd8:	e8 c3 f2 ff ff       	call   4010a0 <runtime::slice_handle_error>
  401ddd:	4a 83 3c c8 00       	cmpq   $0x0,(%rax,%r9,8)
  401de2:	74 08                	je     401dec <runtime::heap_allocator_proc+0x38c>
  401de4:	4a c7 04 c8 00 00 00 	movq   $0x0,(%rax,%r9,8)
  401deb:	00 
  401dec:	49 ff c1             	inc    %r9
  401def:	4d 39 c8             	cmp    %r9,%r8
  401df2:	75 e9                	jne    401ddd <runtime::heap_allocator_proc+0x37d>
  401df4:	48 85 f6             	test   %rsi,%rsi
  401df7:	74 7d                	je     401e76 <runtime::heap_allocator_proc+0x416>
  401df9:	48 bf f8 ff ff ff ff 	movabs $0x7ffffffffffffff8,%rdi
  401e00:	ff ff 7f 
  401e03:	48 21 f9             	and    %rdi,%rcx
  401e06:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  401e0a:	74 04                	je     401e10 <runtime::heap_allocator_proc+0x3b0>
  401e0c:	c6 04 08 00          	movb   $0x0,(%rax,%rcx,1)
  401e10:	83 fe 01             	cmp    $0x1,%esi
  401e13:	74 61                	je     401e76 <runtime::heap_allocator_proc+0x416>
  401e15:	80 7c 08 01 00       	cmpb   $0x0,0x1(%rax,%rcx,1)
  401e1a:	74 05                	je     401e21 <runtime::heap_allocator_proc+0x3c1>
  401e1c:	c6 44 08 01 00       	movb   $0x0,0x1(%rax,%rcx,1)
  401e21:	83 fe 02             	cmp    $0x2,%esi
  401e24:	74 50                	je     401e76 <runtime::heap_allocator_proc+0x416>
  401e26:	80 7c 08 02 00       	cmpb   $0x0,0x2(%rax,%rcx,1)
  401e2b:	74 05                	je     401e32 <runtime::heap_allocator_proc+0x3d2>
  401e2d:	c6 44 08 02 00       	movb   $0x0,0x2(%rax,%rcx,1)
  401e32:	83 fe 03             	cmp    $0x3,%esi
  401e35:	74 3f                	je     401e76 <runtime::heap_allocator_proc+0x416>
  401e37:	80 7c 08 03 00       	cmpb   $0x0,0x3(%rax,%rcx,1)
  401e3c:	74 05                	je     401e43 <runtime::heap_allocator_proc+0x3e3>
  401e3e:	c6 44 08 03 00       	movb   $0x0,0x3(%rax,%rcx,1)
  401e43:	83 fe 04             	cmp    $0x4,%esi
  401e46:	74 2e                	je     401e76 <runtime::heap_allocator_proc+0x416>
  401e48:	80 7c 08 04 00       	cmpb   $0x0,0x4(%rax,%rcx,1)
  401e4d:	74 05                	je     401e54 <runtime::heap_allocator_proc+0x3f4>
  401e4f:	c6 44 08 04 00       	movb   $0x0,0x4(%rax,%rcx,1)
  401e54:	83 fe 05             	cmp    $0x5,%esi
  401e57:	74 1d                	je     401e76 <runtime::heap_allocator_proc+0x416>
  401e59:	80 7c 08 05 00       	cmpb   $0x0,0x5(%rax,%rcx,1)
  401e5e:	74 05                	je     401e65 <runtime::heap_allocator_proc+0x405>
  401e60:	c6 44 08 05 00       	movb   $0x0,0x5(%rax,%rcx,1)
  401e65:	83 fe 06             	cmp    $0x6,%esi
  401e68:	74 0c                	je     401e76 <runtime::heap_allocator_proc+0x416>
  401e6a:	80 7c 08 06 00       	cmpb   $0x0,0x6(%rax,%rcx,1)
  401e6f:	74 05                	je     401e76 <runtime::heap_allocator_proc+0x416>
  401e71:	c6 44 08 06 00       	movb   $0x0,0x6(%rax,%rcx,1)
  401e76:	31 ed                	xor    %ebp,%ebp
  401e78:	49 89 1e             	mov    %rbx,(%r14)
  401e7b:	49 89 56 08          	mov    %rdx,0x8(%r14)
  401e7f:	89 e8                	mov    %ebp,%eax
  401e81:	48 83 c4 08          	add    $0x8,%rsp
  401e85:	5b                   	pop    %rbx
  401e86:	41 5c                	pop    %r12
  401e88:	41 5d                	pop    %r13
  401e8a:	41 5e                	pop    %r14
  401e8c:	41 5f                	pop    %r15
  401e8e:	5d                   	pop    %rbp
  401e8f:	c5 f8 77             	vzeroupper
  401e92:	c3                   	ret
  401e93:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  401e9a:	84 00 00 00 00 00 

0000000000401ea0 <runtime::default_random_generator_proc>:
  401ea0:	41 56                	push   %r14
  401ea2:	53                   	push   %rbx
  401ea3:	50                   	push   %rax
  401ea4:	49 89 c8             	mov    %rcx,%r8
  401ea7:	64 48 8b 1c 25 00 00 	mov    %fs:0x0,%rbx
  401eae:	00 00 
  401eb0:	48 8d 9b f0 ff ff ff 	lea    -0x10(%rbx),%rbx
  401eb7:	48 85 ff             	test   %rdi,%rdi
  401eba:	48 0f 45 df          	cmovne %rdi,%rbx
  401ebe:	48 83 fe 02          	cmp    $0x2,%rsi
  401ec2:	0f 84 ec 00 00 00    	je     401fb4 <runtime::default_random_generator_proc+0x114>
  401ec8:	48 83 fe 01          	cmp    $0x1,%rsi
  401ecc:	74 6e                	je     401f3c <runtime::default_random_generator_proc+0x9c>
  401ece:	48 85 f6             	test   %rsi,%rsi
  401ed1:	0f 85 61 02 00 00    	jne    402138 <runtime::default_random_generator_proc+0x298>
  401ed7:	48 8b 03             	mov    (%rbx),%rax
  401eda:	48 85 c0             	test   %rax,%rax
  401edd:	75 0d                	jne    401eec <runtime::default_random_generator_proc+0x4c>
  401edf:	48 83 7b 08 00       	cmpq   $0x0,0x8(%rbx)
  401ee4:	0f 84 dc 00 00 00    	je     401fc6 <runtime::default_random_generator_proc+0x126>
  401eea:	31 c0                	xor    %eax,%eax
  401eec:	49 83 f8 08          	cmp    $0x8,%r8
  401ef0:	0f 85 17 01 00 00    	jne    40200d <runtime::default_random_generator_proc+0x16d>
  401ef6:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  401efd:	f4 51 58 
  401f00:	48 0f af c8          	imul   %rax,%rcx
  401f04:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  401f08:	48 83 ce 01          	or     $0x1,%rsi
  401f0c:	48 01 ce             	add    %rcx,%rsi
  401f0f:	48 89 33             	mov    %rsi,(%rbx)
  401f12:	48 89 c1             	mov    %rax,%rcx
  401f15:	48 c1 e9 3b          	shr    $0x3b,%rcx
  401f19:	48 89 ce             	mov    %rcx,%rsi
  401f1c:	48 83 c6 05          	add    $0x5,%rsi
  401f20:	48 31 c6             	xor    %rax,%rsi
  401f23:	48 b8 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rax
  401f2a:	75 f1 ae 
  401f2d:	48 0f af c6          	imul   %rsi,%rax
  401f31:	48 d3 c8             	ror    %cl,%rax
  401f34:	48 89 02             	mov    %rax,(%rdx)
  401f37:	e9 fc 01 00 00       	jmp    402138 <runtime::default_random_generator_proc+0x298>
  401f3c:	48 c7 04 24 00 00 00 	movq   $0x0,(%rsp)
  401f43:	00 
  401f44:	48 85 d2             	test   %rdx,%rdx
  401f47:	0f 95 c0             	setne  %al
  401f4a:	4d 85 c0             	test   %r8,%r8
  401f4d:	0f 9f c1             	setg   %cl
  401f50:	20 c1                	and    %al,%cl
  401f52:	80 f9 01             	cmp    $0x1,%cl
  401f55:	75 24                	jne    401f7b <runtime::default_random_generator_proc+0xdb>
  401f57:	49 83 f8 08          	cmp    $0x8,%r8
  401f5b:	b8 08 00 00 00       	mov    $0x8,%eax
  401f60:	49 0f 42 c0          	cmovb  %r8,%rax
  401f64:	48 89 e7             	mov    %rsp,%rdi
  401f67:	48 89 d6             	mov    %rdx,%rsi
  401f6a:	48 89 c2             	mov    %rax,%rdx
  401f6d:	e8 ee f0 ff ff       	call   401060 <memcpy@plt>
  401f72:	48 8b 14 24          	mov    (%rsp),%rdx
  401f76:	48 85 d2             	test   %rdx,%rdx
  401f79:	75 09                	jne    401f84 <runtime::default_random_generator_proc+0xe4>
  401f7b:	0f 31                	rdtsc
  401f7d:	48 c1 e2 20          	shl    $0x20,%rdx
  401f81:	48 09 c2             	or     %rax,%rdx
  401f84:	48 8d 04 55 01 00 00 	lea    0x1(,%rdx,2),%rax
  401f8b:	00 
  401f8c:	48 89 43 08          	mov    %rax,0x8(%rbx)
  401f90:	48 8d 04 52          	lea    (%rdx,%rdx,2),%rax
  401f94:	48 ff c0             	inc    %rax
  401f97:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  401f9e:	f4 51 58 
  401fa1:	48 0f af c8          	imul   %rax,%rcx
  401fa5:	48 8d 04 51          	lea    (%rcx,%rdx,2),%rax
  401fa9:	48 ff c0             	inc    %rax
  401fac:	48 89 03             	mov    %rax,(%rbx)
  401faf:	e9 84 01 00 00       	jmp    402138 <runtime::default_random_generator_proc+0x298>
  401fb4:	49 83 f8 04          	cmp    $0x4,%r8
  401fb8:	0f 85 7a 01 00 00    	jne    402138 <runtime::default_random_generator_proc+0x298>
  401fbe:	80 0a 0a             	orb    $0xa,(%rdx)
  401fc1:	e9 72 01 00 00       	jmp    402138 <runtime::default_random_generator_proc+0x298>
  401fc6:	48 89 d6             	mov    %rdx,%rsi
  401fc9:	0f 31                	rdtsc
  401fcb:	48 89 d1             	mov    %rdx,%rcx
  401fce:	48 89 f2             	mov    %rsi,%rdx
  401fd1:	48 c1 e1 20          	shl    $0x20,%rcx
  401fd5:	48 09 c1             	or     %rax,%rcx
  401fd8:	48 8d 04 4d 01 00 00 	lea    0x1(,%rcx,2),%rax
  401fdf:	00 
  401fe0:	48 89 43 08          	mov    %rax,0x8(%rbx)
  401fe4:	48 8d 04 49          	lea    (%rcx,%rcx,2),%rax
  401fe8:	48 ff c0             	inc    %rax
  401feb:	48 be 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rsi
  401ff2:	f4 51 58 
  401ff5:	48 0f af f0          	imul   %rax,%rsi
  401ff9:	48 8d 04 4e          	lea    (%rsi,%rcx,2),%rax
  401ffd:	48 ff c0             	inc    %rax
  402000:	48 89 03             	mov    %rax,(%rbx)
  402003:	49 83 f8 08          	cmp    $0x8,%r8
  402007:	0f 84 e9 fe ff ff    	je     401ef6 <runtime::default_random_generator_proc+0x56>
  40200d:	4d 85 c0             	test   %r8,%r8
  402010:	0f 8e 22 01 00 00    	jle    402138 <runtime::default_random_generator_proc+0x298>
  402016:	49 83 f8 01          	cmp    $0x1,%r8
  40201a:	75 0c                	jne    402028 <runtime::default_random_generator_proc+0x188>
  40201c:	b1 01                	mov    $0x1,%cl
  40201e:	31 c0                	xor    %eax,%eax
  402020:	45 31 d2             	xor    %r10d,%r10d
  402023:	e9 c1 00 00 00       	jmp    4020e9 <runtime::default_random_generator_proc+0x249>
  402028:	48 be d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rsi
  40202f:	75 f1 ae 
  402032:	48 bf 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rdi
  402039:	f4 51 58 
  40203c:	49 b9 fe ff ff ff ff 	movabs $0x7ffffffffffffffe,%r9
  402043:	ff ff 7f 
  402046:	4d 21 c1             	and    %r8,%r9
  402049:	31 c0                	xor    %eax,%eax
  40204b:	31 c9                	xor    %ecx,%ecx
  40204d:	45 31 d2             	xor    %r10d,%r10d
  402050:	eb 51                	jmp    4020a3 <runtime::default_random_generator_proc+0x203>
  402052:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402059:	1f 84 00 00 00 00 00 
  402060:	4c 8b 1b             	mov    (%rbx),%r11
  402063:	4c 8b 73 08          	mov    0x8(%rbx),%r14
  402067:	4c 89 d9             	mov    %r11,%rcx
  40206a:	48 c1 e9 3b          	shr    $0x3b,%rcx
  40206e:	49 89 ca             	mov    %rcx,%r10
  402071:	49 83 c2 05          	add    $0x5,%r10
  402075:	4d 31 da             	xor    %r11,%r10
  402078:	4c 0f af df          	imul   %rdi,%r11
  40207c:	49 83 ce 01          	or     $0x1,%r14
  402080:	4d 01 de             	add    %r11,%r14
  402083:	4c 89 33             	mov    %r14,(%rbx)
  402086:	4c 0f af d6          	imul   %rsi,%r10
  40208a:	49 d3 ca             	ror    %cl,%r10
  40208d:	b1 08                	mov    $0x8,%cl
  40208f:	44 88 54 02 01       	mov    %r10b,0x1(%rdx,%rax,1)
  402094:	49 c1 ea 08          	shr    $0x8,%r10
  402098:	fe c9                	dec    %cl
  40209a:	48 83 c0 02          	add    $0x2,%rax
  40209e:	49 39 c1             	cmp    %rax,%r9
  4020a1:	74 41                	je     4020e4 <runtime::default_random_generator_proc+0x244>
  4020a3:	84 c9                	test   %cl,%cl
  4020a5:	75 2f                	jne    4020d6 <runtime::default_random_generator_proc+0x236>
  4020a7:	4c 8b 1b             	mov    (%rbx),%r11
  4020aa:	4c 8b 73 08          	mov    0x8(%rbx),%r14
  4020ae:	4c 89 d9             	mov    %r11,%rcx
  4020b1:	48 c1 e9 3b          	shr    $0x3b,%rcx
  4020b5:	49 89 ca             	mov    %rcx,%r10
  4020b8:	49 83 c2 05          	add    $0x5,%r10
  4020bc:	4d 31 da             	xor    %r11,%r10
  4020bf:	4c 0f af df          	imul   %rdi,%r11
  4020c3:	49 83 ce 01          	or     $0x1,%r14
  4020c7:	4d 01 de             	add    %r11,%r14
  4020ca:	4c 89 33             	mov    %r14,(%rbx)
  4020cd:	4c 0f af d6          	imul   %rsi,%r10
  4020d1:	49 d3 ca             	ror    %cl,%r10
  4020d4:	b1 08                	mov    $0x8,%cl
  4020d6:	44 88 14 02          	mov    %r10b,(%rdx,%rax,1)
  4020da:	fe c9                	dec    %cl
  4020dc:	74 82                	je     402060 <runtime::default_random_generator_proc+0x1c0>
  4020de:	49 c1 ea 08          	shr    $0x8,%r10
  4020e2:	eb ab                	jmp    40208f <runtime::default_random_generator_proc+0x1ef>
  4020e4:	84 c9                	test   %cl,%cl
  4020e6:	0f 94 c1             	sete   %cl
  4020e9:	41 f6 c0 01          	test   $0x1,%r8b
  4020ed:	74 49                	je     402138 <runtime::default_random_generator_proc+0x298>
  4020ef:	84 c9                	test   %cl,%cl
  4020f1:	74 41                	je     402134 <runtime::default_random_generator_proc+0x294>
  4020f3:	48 8b 33             	mov    (%rbx),%rsi
  4020f6:	48 8b 4b 08          	mov    0x8(%rbx),%rcx
  4020fa:	48 bf 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rdi
  402101:	f4 51 58 
  402104:	48 0f af fe          	imul   %rsi,%rdi
  402108:	48 83 c9 01          	or     $0x1,%rcx
  40210c:	48 01 f9             	add    %rdi,%rcx
  40210f:	48 89 0b             	mov    %rcx,(%rbx)
  402112:	48 89 f1             	mov    %rsi,%rcx
  402115:	48 c1 e9 3b          	shr    $0x3b,%rcx
  402119:	48 89 cf             	mov    %rcx,%rdi
  40211c:	48 83 c7 05          	add    $0x5,%rdi
  402120:	48 31 f7             	xor    %rsi,%rdi
  402123:	49 ba d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%r10
  40212a:	75 f1 ae 
  40212d:	4c 0f af d7          	imul   %rdi,%r10
  402131:	49 d3 ca             	ror    %cl,%r10
  402134:	44 88 14 02          	mov    %r10b,(%rdx,%rax,1)
  402138:	48 83 c4 08          	add    $0x8,%rsp
  40213c:	5b                   	pop    %rbx
  40213d:	41 5e                	pop    %r14
  40213f:	c3                   	ret

0000000000402140 <runtime::default_temp_allocator_proc>:
  402140:	41 57                	push   %r15
  402142:	41 56                	push   %r14
  402144:	41 54                	push   %r12
  402146:	53                   	push   %rbx
  402147:	48 83 ec 78          	sub    $0x78,%rsp
  40214b:	40 80 fe 07          	cmp    $0x7,%sil
  40214f:	0f 87 05 02 00 00    	ja     40235a <runtime::default_temp_allocator_proc+0x21a>
  402155:	4c 89 c8             	mov    %r9,%rax
  402158:	48 89 fb             	mov    %rdi,%rbx
  40215b:	4c 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%r9
  402162:	00 
  402163:	4c 8b b4 24 a0 00 00 	mov    0xa0(%rsp),%r14
  40216a:	00 
  40216b:	40 0f b6 f6          	movzbl %sil,%esi
  40216f:	ff 24 f5 50 40 40 00 	jmp    *0x404050(,%rsi,8)
  402176:	b0 04                	mov    $0x4,%al
  402178:	45 31 c0             	xor    %r8d,%r8d
  40217b:	31 d2                	xor    %edx,%edx
  40217d:	e9 df 01 00 00       	jmp    402361 <runtime::default_temp_allocator_proc+0x221>
  402182:	4d 85 c0             	test   %r8,%r8
  402185:	74 31                	je     4021b8 <runtime::default_temp_allocator_proc+0x78>
  402187:	48 39 c2             	cmp    %rax,%rdx
  40218a:	0f 85 22 01 00 00    	jne    4022b2 <runtime::default_temp_allocator_proc+0x172>
  402190:	48 85 d2             	test   %rdx,%rdx
  402193:	0f 89 c6 01 00 00    	jns    40235f <runtime::default_temp_allocator_proc+0x21f>
  402199:	bf a0 40 40 00       	mov    $0x4040a0,%edi
  40219e:	be 3c 00 00 00       	mov    $0x3c,%esi
  4021a3:	49 89 d1             	mov    %rdx,%r9
  4021a6:	ba db 00 00 00       	mov    $0xdb,%edx
  4021ab:	b9 13 00 00 00       	mov    $0x13,%ecx
  4021b0:	45 31 c0             	xor    %r8d,%r8d
  4021b3:	e8 88 ef ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  4021b8:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4021bc:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  4021c1:	49 89 e0             	mov    %rsp,%r8
  4021c4:	48 89 df             	mov    %rbx,%rdi
  4021c7:	48 89 d6             	mov    %rdx,%rsi
  4021ca:	48 89 ca             	mov    %rcx,%rdx
  4021cd:	4c 89 f1             	mov    %r14,%rcx
  4021d0:	e8 7b 04 00 00       	call   402650 <runtime::arena_alloc>
  4021d5:	4c 8b 04 24          	mov    (%rsp),%r8
  4021d9:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4021de:	e9 7e 01 00 00       	jmp    402361 <runtime::default_temp_allocator_proc+0x221>
  4021e3:	48 8b 7b 10          	mov    0x10(%rbx),%rdi
  4021e7:	48 85 ff             	test   %rdi,%rdi
  4021ea:	0f 84 62 01 00 00    	je     402352 <runtime::default_temp_allocator_proc+0x212>
  4021f0:	4c 8d 64 24 30       	lea    0x30(%rsp),%r12
  4021f5:	49 89 e7             	mov    %rsp,%r15
  4021f8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4021ff:	00 
  402200:	48 8b 07             	mov    (%rdi),%rax
  402203:	48 85 c0             	test   %rax,%rax
  402206:	0f 84 28 01 00 00    	je     402334 <runtime::default_temp_allocator_proc+0x1f4>
  40220c:	48 89 43 10          	mov    %rax,0x10(%rbx)
  402210:	48 8b 47 28          	mov    0x28(%rdi),%rax
  402214:	48 29 43 20          	sub    %rax,0x20(%rbx)
  402218:	48 8b 77 08          	mov    0x8(%rdi),%rsi
  40221c:	48 8b 57 10          	mov    0x10(%rdi),%rdx
  402220:	48 c7 44 24 10 40 21 	movq   $0x402140,0x10(%rsp)
  402227:	40 00 
  402229:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402230:	00 00 
  402232:	48 8d 80 b8 ff ff ff 	lea    -0x48(%rax),%rax
  402239:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40223e:	48 c7 44 24 20 b0 11 	movq   $0x4011b0,0x20(%rsp)
  402245:	40 00 
  402247:	48 c7 44 24 28 e0 2c 	movq   $0x402ce0,0x28(%rsp)
  40224e:	40 00 
  402250:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402254:	c4 c1 78 11 04 24    	vmovups %xmm0,(%r12)
  40225a:	49 c7 44 24 10 00 00 	movq   $0x0,0x10(%r12)
  402261:	00 00 
  402263:	48 c7 44 24 48 a0 1e 	movq   $0x401ea0,0x48(%rsp)
  40226a:	40 00 
  40226c:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402270:	c4 c1 7c 11 44 24 20 	vmovups %ymm0,0x20(%r12)
  402277:	48 89 34 24          	mov    %rsi,(%rsp)
  40227b:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  402280:	4c 89 f1             	mov    %r14,%rcx
  402283:	4d 89 f8             	mov    %r15,%r8
  402286:	c5 f8 77             	vzeroupper
  402289:	e8 52 08 00 00       	call   402ae0 <runtime::mem_free>
  40228e:	48 8b 7b 10          	mov    0x10(%rbx),%rdi
  402292:	48 85 ff             	test   %rdi,%rdi
  402295:	0f 85 65 ff ff ff    	jne    402200 <runtime::default_temp_allocator_proc+0xc0>
  40229b:	e9 b2 00 00 00       	jmp    402352 <runtime::default_temp_allocator_proc+0x212>
  4022a0:	4d 85 c0             	test   %r8,%r8
  4022a3:	0f 84 b1 00 00 00    	je     40235a <runtime::default_temp_allocator_proc+0x21a>
  4022a9:	41 c6 00 5d          	movb   $0x5d,(%r8)
  4022ad:	e9 a8 00 00 00       	jmp    40235a <runtime::default_temp_allocator_proc+0x21a>
  4022b2:	48 85 d2             	test   %rdx,%rdx
  4022b5:	0f 84 bb fe ff ff    	je     402176 <runtime::default_temp_allocator_proc+0x36>
  4022bb:	48 8d 71 ff          	lea    -0x1(%rcx),%rsi
  4022bf:	4c 85 c6             	test   %r8,%rsi
  4022c2:	0f 84 b4 00 00 00    	je     40237c <runtime::default_temp_allocator_proc+0x23c>
  4022c8:	49 89 c4             	mov    %rax,%r12
  4022cb:	4d 89 c7             	mov    %r8,%r15
  4022ce:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4022d2:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  4022d7:	49 89 e0             	mov    %rsp,%r8
  4022da:	48 89 df             	mov    %rbx,%rdi
  4022dd:	48 89 d6             	mov    %rdx,%rsi
  4022e0:	48 89 ca             	mov    %rcx,%rdx
  4022e3:	4c 89 f1             	mov    %r14,%rcx
  4022e6:	e8 65 03 00 00       	call   402650 <runtime::arena_alloc>
  4022eb:	84 c0                	test   %al,%al
  4022ed:	0f 85 85 fe ff ff    	jne    402178 <runtime::default_temp_allocator_proc+0x38>
  4022f3:	48 8b 1c 24          	mov    (%rsp),%rbx
  4022f7:	48 85 db             	test   %rbx,%rbx
  4022fa:	74 5e                	je     40235a <runtime::default_temp_allocator_proc+0x21a>
  4022fc:	4d 89 e1             	mov    %r12,%r9
  4022ff:	4d 85 e4             	test   %r12,%r12
  402302:	0f 88 ef 00 00 00    	js     4023f7 <runtime::default_temp_allocator_proc+0x2b7>
  402308:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40230d:	4c 39 ca             	cmp    %r9,%rdx
  402310:	4c 0f 4c ca          	cmovl  %rdx,%r9
  402314:	4d 85 c9             	test   %r9,%r9
  402317:	7e 14                	jle    40232d <runtime::default_temp_allocator_proc+0x1ed>
  402319:	4c 89 fe             	mov    %r15,%rsi
  40231c:	48 89 df             	mov    %rbx,%rdi
  40231f:	49 89 d6             	mov    %rdx,%r14
  402322:	4c 89 ca             	mov    %r9,%rdx
  402325:	e8 66 ed ff ff       	call   401090 <memmove@plt>
  40232a:	4c 89 f2             	mov    %r14,%rdx
  40232d:	31 c0                	xor    %eax,%eax
  40232f:	49 89 d8             	mov    %rbx,%r8
  402332:	eb 2d                	jmp    402361 <runtime::default_temp_allocator_proc+0x221>
  402334:	48 8b 47 18          	mov    0x18(%rdi),%rax
  402338:	48 8b 57 20          	mov    0x20(%rdi),%rdx
  40233c:	48 89 c7             	mov    %rax,%rdi
  40233f:	31 f6                	xor    %esi,%esi
  402341:	e8 fa ec ff ff       	call   401040 <memset@plt>
  402346:	48 8b 43 10          	mov    0x10(%rbx),%rax
  40234a:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  402351:	00 
  402352:	48 c7 43 18 00 00 00 	movq   $0x0,0x18(%rbx)
  402359:	00 
  40235a:	45 31 c0             	xor    %r8d,%r8d
  40235d:	31 d2                	xor    %edx,%edx
  40235f:	31 c0                	xor    %eax,%eax
  402361:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  402368:	00 
  402369:	4c 89 01             	mov    %r8,(%rcx)
  40236c:	48 89 51 08          	mov    %rdx,0x8(%rcx)
  402370:	48 83 c4 78          	add    $0x78,%rsp
  402374:	5b                   	pop    %rbx
  402375:	41 5c                	pop    %r12
  402377:	41 5e                	pop    %r14
  402379:	41 5f                	pop    %r15
  40237b:	c3                   	ret
  40237c:	48 39 c2             	cmp    %rax,%rdx
  40237f:	73 24                	jae    4023a5 <runtime::default_temp_allocator_proc+0x265>
  402381:	48 85 d2             	test   %rdx,%rdx
  402384:	79 d9                	jns    40235f <runtime::default_temp_allocator_proc+0x21f>
  402386:	bf a0 40 40 00       	mov    $0x4040a0,%edi
  40238b:	be 3c 00 00 00       	mov    $0x3c,%esi
  402390:	49 89 d1             	mov    %rdx,%r9
  402393:	ba e3 00 00 00       	mov    $0xe3,%edx
  402398:	b9 14 00 00 00       	mov    $0x14,%ecx
  40239d:	45 31 c0             	xor    %r8d,%r8d
  4023a0:	e8 9b ed ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  4023a5:	48 8b 7b 10          	mov    0x10(%rbx),%rdi
  4023a9:	48 85 ff             	test   %rdi,%rdi
  4023ac:	0f 84 16 ff ff ff    	je     4022c8 <runtime::default_temp_allocator_proc+0x188>
  4023b2:	48 8b 77 18          	mov    0x18(%rdi),%rsi
  4023b6:	4d 89 c2             	mov    %r8,%r10
  4023b9:	49 29 f2             	sub    %rsi,%r10
  4023bc:	4d 8d 1c 02          	lea    (%r10,%rax,1),%r11
  4023c0:	4d 39 da             	cmp    %r11,%r10
  4023c3:	0f 83 ff fe ff ff    	jae    4022c8 <runtime::default_temp_allocator_proc+0x188>
  4023c9:	4c 3b 5f 20          	cmp    0x20(%rdi),%r11
  4023cd:	0f 85 f5 fe ff ff    	jne    4022c8 <runtime::default_temp_allocator_proc+0x188>
  4023d3:	4d 8d 1c 12          	lea    (%r10,%rdx,1),%r11
  4023d7:	4c 3b 5f 28          	cmp    0x28(%rdi),%r11
  4023db:	0f 87 e7 fe ff ff    	ja     4022c8 <runtime::default_temp_allocator_proc+0x188>
  4023e1:	4c 89 5f 20          	mov    %r11,0x20(%rdi)
  4023e5:	4d 39 da             	cmp    %r11,%r10
  4023e8:	7f 29                	jg     402413 <runtime::default_temp_allocator_proc+0x2d3>
  4023ea:	4c 01 d6             	add    %r10,%rsi
  4023ed:	31 c0                	xor    %eax,%eax
  4023ef:	49 89 f0             	mov    %rsi,%r8
  4023f2:	e9 6a ff ff ff       	jmp    402361 <runtime::default_temp_allocator_proc+0x221>
  4023f7:	bf a0 40 40 00       	mov    $0x4040a0,%edi
  4023fc:	be 3c 00 00 00       	mov    $0x3c,%esi
  402401:	ba f9 00 00 00       	mov    $0xf9,%edx
  402406:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  40240b:	45 31 c0             	xor    %r8d,%r8d
  40240e:	e8 2d ed ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  402413:	bf a0 40 40 00       	mov    $0x4040a0,%edi
  402418:	be 3c 00 00 00       	mov    $0x3c,%esi
  40241d:	ba ee 00 00 00       	mov    $0xee,%edx
  402422:	b9 17 00 00 00       	mov    $0x17,%ecx
  402427:	4d 89 d0             	mov    %r10,%r8
  40242a:	4d 89 d9             	mov    %r11,%r9
  40242d:	e8 0e ed ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  402432:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402439:	1f 84 00 00 00 00 00 

0000000000402440 <main>:
  402440:	41 56                	push   %r14
  402442:	53                   	push   %rbx
  402443:	48 83 ec 78          	sub    $0x78,%rsp
  402447:	4c 63 cf             	movslq %edi,%r9
  40244a:	45 85 c9             	test   %r9d,%r9d
  40244d:	0f 88 3b 01 00 00    	js     40258e <main+0x14e>
  402453:	48 89 35 f6 3b 00 00 	mov    %rsi,0x3bf6(%rip)        # 406050 <runtime::args__.0>
  40245a:	4c 89 0d f7 3b 00 00 	mov    %r9,0x3bf7(%rip)        # 406058 <runtime::args__.1>
  402461:	48 c7 44 24 08 60 1a 	movq   $0x401a60,0x8(%rsp)
  402468:	40 00 
  40246a:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  402471:	00 00 
  402473:	48 c7 44 24 18 40 21 	movq   $0x402140,0x18(%rsp)
  40247a:	40 00 
  40247c:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402483:	00 00 
  402485:	48 8d 80 b8 ff ff ff 	lea    -0x48(%rax),%rax
  40248c:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402491:	48 c7 44 24 28 b0 11 	movq   $0x4011b0,0x28(%rsp)
  402498:	40 00 
  40249a:	48 c7 44 24 30 e0 2c 	movq   $0x402ce0,0x30(%rsp)
  4024a1:	40 00 
  4024a3:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4024a7:	c5 f8 11 44 24 38    	vmovups %xmm0,0x38(%rsp)
  4024ad:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  4024b4:	00 00 
  4024b6:	48 c7 44 24 50 a0 1e 	movq   $0x401ea0,0x50(%rsp)
  4024bd:	40 00 
  4024bf:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4024c3:	c5 fc 11 44 24 58    	vmovups %ymm0,0x58(%rsp)
  4024c9:	48 8d 5c 24 08       	lea    0x8(%rsp),%rbx
  4024ce:	48 89 df             	mov    %rbx,%rdi
  4024d1:	c5 f8 77             	vzeroupper
  4024d4:	e8 07 ef ff ff       	call   4013e0 <__$startup_runtime>
  4024d9:	b8 09 00 00 00       	mov    $0x9,%eax
  4024de:	be 00 e0 01 00       	mov    $0x1e000,%esi
  4024e3:	ba 03 00 00 00       	mov    $0x3,%edx
  4024e8:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4024ee:	31 ff                	xor    %edi,%edi
  4024f0:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4024f7:	45 31 c9             	xor    %r9d,%r9d
  4024fa:	0f 05                	syscall
  4024fc:	49 89 c6             	mov    %rax,%r14
  4024ff:	b8 ba 00 00 00       	mov    $0xba,%eax
  402504:	0f 05                	syscall
  402506:	b8 09 00 00 00       	mov    $0x9,%eax
  40250b:	be 00 10 02 00       	mov    $0x21000,%esi
  402510:	31 ff                	xor    %edi,%edi
  402512:	45 31 c9             	xor    %r9d,%r9d
  402515:	0f 05                	syscall
  402517:	c5 f9 6f 05 a1 1e 00 	vmovdqa 0x1ea1(%rip),%xmm0        # 4043c0 <_IO_stdin_used+0x3c0>
  40251e:	00 
  40251f:	c4 c1 7a 7f 46 10    	vmovdqu %xmm0,0x10(%r14)
  402525:	41 c7 86 02 10 00 00 	movl   $0xfe00ef0f,0x1002(%r14)
  40252c:	0f ef 00 fe 
  402530:	c5 fd 76 c0          	vpcmpeqd %ymm0,%ymm0,%ymm0
  402534:	c4 c1 7e 7f 86 00 30 	vmovdqu %ymm0,0x3000(%r14)
  40253b:	00 00 
  40253d:	48 b8 ff ff ff ff ff 	movabs $0x3ffffffffff,%rax
  402544:	03 00 00 
  402547:	49 89 86 20 30 00 00 	mov    %rax,0x3020(%r14)
  40254e:	c5 f8 28 05 7a 1e 00 	vmovaps 0x1e7a(%rip),%xmm0        # 4043d0 <_IO_stdin_used+0x3d0>
  402555:	00 
  402556:	c4 c1 78 11 46 20    	vmovups %xmm0,0x20(%r14)
  40255c:	49 c7 86 00 40 00 00 	movq   $0x1fffff,0x4000(%r14)
  402563:	ff ff 1f 00 
  402567:	c4 e2 7d 18 05 94 1a 	vbroadcastss 0x1a94(%rip),%ymm0        # 404004 <_IO_stdin_used+0x4>
  40256e:	00 00 
  402570:	c4 c1 7c 29 86 40 30 	vmovaps %ymm0,0x3040(%r14)
  402577:	00 00 
  402579:	48 89 df             	mov    %rbx,%rdi
  40257c:	c5 f8 77             	vzeroupper
  40257f:	e8 ac ef ff ff       	call   401530 <__$cleanup_runtime>
  402584:	31 c0                	xor    %eax,%eax
  402586:	48 83 c4 78          	add    $0x78,%rsp
  40258a:	5b                   	pop    %rbx
  40258b:	41 5e                	pop    %r14
  40258d:	c3                   	ret
  40258e:	bf 18 42 40 00       	mov    $0x404218,%edi
  402593:	be 2a 00 00 00       	mov    $0x2a,%esi
  402598:	ba 36 00 00 00       	mov    $0x36,%edx
  40259d:	b9 11 00 00 00       	mov    $0x11,%ecx
  4025a2:	45 31 c0             	xor    %r8d,%r8d
  4025a5:	e8 96 eb ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  4025aa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004025b0 <runtime::print_string>:
  4025b0:	48 89 f2             	mov    %rsi,%rdx
  4025b3:	48 89 fe             	mov    %rdi,%rsi
  4025b6:	b8 01 00 00 00       	mov    $0x1,%eax
  4025bb:	bf 02 00 00 00       	mov    $0x2,%edi
  4025c0:	0f 05                	syscall
  4025c2:	c3                   	ret
  4025c3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4025ca:	84 00 00 00 00 00 

00000000004025d0 <runtime::mem_alloc_bytes>:
  4025d0:	53                   	push   %rbx
  4025d1:	48 83 ec 10          	sub    $0x10,%rsp
  4025d5:	4d 89 c2             	mov    %r8,%r10
  4025d8:	48 89 cb             	mov    %rcx,%rbx
  4025db:	48 89 f0             	mov    %rsi,%rax
  4025de:	49 89 f8             	mov    %rdi,%r8
  4025e1:	48 85 ff             	test   %rdi,%rdi
  4025e4:	0f 94 c1             	sete   %cl
  4025e7:	48 85 f6             	test   %rsi,%rsi
  4025ea:	40 0f 94 c7          	sete   %dil
  4025ee:	40 08 cf             	or     %cl,%dil
  4025f1:	31 c9                	xor    %ecx,%ecx
  4025f3:	be 00 00 00 00       	mov    $0x0,%esi
  4025f8:	40 80 ff 01          	cmp    $0x1,%dil
  4025fc:	74 3d                	je     40263b <runtime::mem_alloc_bytes+0x6b>
  4025fe:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402602:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  402607:	48 83 ec 08          	sub    $0x8,%rsp
  40260b:	4c 8d 5c 24 08       	lea    0x8(%rsp),%r11
  402610:	b9 08 00 00 00       	mov    $0x8,%ecx
  402615:	48 89 d7             	mov    %rdx,%rdi
  402618:	31 f6                	xor    %esi,%esi
  40261a:	4c 89 c2             	mov    %r8,%rdx
  40261d:	45 31 c0             	xor    %r8d,%r8d
  402620:	45 31 c9             	xor    %r9d,%r9d
  402623:	41 52                	push   %r10
  402625:	41 53                	push   %r11
  402627:	68 d0 42 40 00       	push   $0x4042d0
  40262c:	ff d0                	call   *%rax
  40262e:	48 83 c4 20          	add    $0x20,%rsp
  402632:	48 8b 0c 24          	mov    (%rsp),%rcx
  402636:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40263b:	48 89 0b             	mov    %rcx,(%rbx)
  40263e:	48 89 73 08          	mov    %rsi,0x8(%rbx)
  402642:	48 83 c4 10          	add    $0x10,%rsp
  402646:	5b                   	pop    %rbx
  402647:	c3                   	ret
  402648:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40264f:	00 

0000000000402650 <runtime::arena_alloc>:
  402650:	55                   	push   %rbp
  402651:	41 57                	push   %r15
  402653:	41 56                	push   %r14
  402655:	41 55                	push   %r13
  402657:	41 54                	push   %r12
  402659:	53                   	push   %rbx
  40265a:	48 83 ec 38          	sub    $0x38,%rsp
  40265e:	49 89 cb             	mov    %rcx,%r11
  402661:	c4 e2 f8 f3 ca       	blsr   %rdx,%rax
  402666:	0f 85 52 02 00 00    	jne    4028be <runtime::arena_alloc+0x26e>
  40266c:	49 89 f2             	mov    %rsi,%r10
  40266f:	48 85 f6             	test   %rsi,%rsi
  402672:	0f 84 96 00 00 00    	je     40270e <runtime::arena_alloc+0xbe>
  402678:	49 89 d4             	mov    %rdx,%r12
  40267b:	49 89 fe             	mov    %rdi,%r14
  40267e:	48 8b 47 10          	mov    0x10(%rdi),%rax
  402682:	48 85 c0             	test   %rax,%rax
  402685:	74 37                	je     4026be <runtime::arena_alloc+0x6e>
  402687:	48 8b 48 20          	mov    0x20(%rax),%rcx
  40268b:	48 8b 70 18          	mov    0x18(%rax),%rsi
  40268f:	48 01 ce             	add    %rcx,%rsi
  402692:	49 8d 7c 24 ff       	lea    -0x1(%r12),%rdi
  402697:	48 21 f7             	and    %rsi,%rdi
  40269a:	4c 89 e2             	mov    %r12,%rdx
  40269d:	48 29 fa             	sub    %rdi,%rdx
  4026a0:	48 85 ff             	test   %rdi,%rdi
  4026a3:	48 0f 44 d7          	cmove  %rdi,%rdx
  4026a7:	4c 89 d7             	mov    %r10,%rdi
  4026aa:	48 01 d7             	add    %rdx,%rdi
  4026ad:	72 0f                	jb     4026be <runtime::arena_alloc+0x6e>
  4026af:	48 01 cf             	add    %rcx,%rdi
  4026b2:	72 0a                	jb     4026be <runtime::arena_alloc+0x6e>
  4026b4:	48 3b 78 28          	cmp    0x28(%rax),%rdi
  4026b8:	0f 86 8f 01 00 00    	jbe    40284d <runtime::arena_alloc+0x1fd>
  4026be:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4026c3:	49 8b 46 28          	mov    0x28(%r14),%rax
  4026c7:	48 85 c0             	test   %rax,%rax
  4026ca:	75 0d                	jne    4026d9 <runtime::arena_alloc+0x89>
  4026cc:	49 c7 46 28 00 00 40 	movq   $0x400000,0x28(%r14)
  4026d3:	00 
  4026d4:	b8 00 00 40 00       	mov    $0x400000,%eax
  4026d9:	49 8d 5c 24 ff       	lea    -0x1(%r12),%rbx
  4026de:	48 89 d9             	mov    %rbx,%rcx
  4026e1:	4c 21 d1             	and    %r10,%rcx
  4026e4:	4c 89 e7             	mov    %r12,%rdi
  4026e7:	48 29 cf             	sub    %rcx,%rdi
  4026ea:	48 85 c9             	test   %rcx,%rcx
  4026ed:	48 0f 44 f9          	cmove  %rcx,%rdi
  4026f1:	4c 01 d7             	add    %r10,%rdi
  4026f4:	48 39 c7             	cmp    %rax,%rdi
  4026f7:	48 0f 46 f8          	cmovbe %rax,%rdi
  4026fb:	4d 8b 2e             	mov    (%r14),%r13
  4026fe:	4d 85 ed             	test   %r13,%r13
  402701:	4c 89 54 24 30       	mov    %r10,0x30(%rsp)
  402706:	74 16                	je     40271e <runtime::arena_alloc+0xce>
  402708:	49 8b 6e 08          	mov    0x8(%r14),%rbp
  40270c:	eb 27                	jmp    402735 <runtime::arena_alloc+0xe5>
  40270e:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402712:	c4 c1 78 11 00       	vmovups %xmm0,(%r8)
  402717:	31 c0                	xor    %eax,%eax
  402719:	e9 56 01 00 00       	jmp    402874 <runtime::arena_alloc+0x224>
  40271e:	49 c7 06 60 1a 40 00 	movq   $0x401a60,(%r14)
  402725:	41 bd 60 1a 40 00    	mov    $0x401a60,%r13d
  40272b:	49 c7 46 08 00 00 00 	movq   $0x0,0x8(%r14)
  402732:	00 
  402733:	31 ed                	xor    %ebp,%ebp
  402735:	49 83 fc 31          	cmp    $0x31,%r12
  402739:	41 bf 30 00 00 00    	mov    $0x30,%r15d
  40273f:	4d 0f 43 fc          	cmovae %r12,%r15
  402743:	4c 01 ff             	add    %r15,%rdi
  402746:	49 83 fc 11          	cmp    $0x11,%r12
  40274a:	be 10 00 00 00       	mov    $0x10,%esi
  40274f:	49 0f 4d f4          	cmovge %r12,%rsi
  402753:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402757:	c5 f8 29 44 24 20    	vmovaps %xmm0,0x20(%rsp)
  40275d:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  402762:	4c 89 0c 24          	mov    %r9,(%rsp)
  402766:	4c 8d 4c 24 20       	lea    0x20(%rsp),%r9
  40276b:	4c 89 ea             	mov    %r13,%rdx
  40276e:	48 89 e9             	mov    %rbp,%rcx
  402771:	4d 89 d8             	mov    %r11,%r8
  402774:	e8 f7 01 00 00       	call   402970 <runtime::mem_alloc>
  402779:	84 c0                	test   %al,%al
  40277b:	74 12                	je     40278f <runtime::arena_alloc+0x13f>
  40277d:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402782:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402786:	c5 f8 11 01          	vmovups %xmm0,(%rcx)
  40278a:	e9 e5 00 00 00       	jmp    402874 <runtime::arena_alloc+0x224>
  40278f:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402794:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402799:	48 01 d0             	add    %rdx,%rax
  40279c:	4c 89 6a 08          	mov    %r13,0x8(%rdx)
  4027a0:	48 89 6a 10          	mov    %rbp,0x10(%rdx)
  4027a4:	49 01 d7             	add    %rdx,%r15
  4027a7:	4c 89 7a 18          	mov    %r15,0x18(%rdx)
  4027ab:	4c 29 f8             	sub    %r15,%rax
  4027ae:	48 89 42 28          	mov    %rax,0x28(%rdx)
  4027b2:	48 83 7a 20 00       	cmpq   $0x0,0x20(%rdx)
  4027b7:	0f 85 2c 01 00 00    	jne    4028e9 <runtime::arena_alloc+0x299>
  4027bd:	48 83 3a 00          	cmpq   $0x0,(%rdx)
  4027c1:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  4027c6:	0f 85 50 01 00 00    	jne    40291c <runtime::arena_alloc+0x2cc>
  4027cc:	49 8b 46 10          	mov    0x10(%r14),%rax
  4027d0:	48 89 02             	mov    %rax,(%rdx)
  4027d3:	49 89 56 10          	mov    %rdx,0x10(%r14)
  4027d7:	48 8b 42 28          	mov    0x28(%rdx),%rax
  4027db:	49 01 46 20          	add    %rax,0x20(%r14)
  4027df:	4c 8b 52 20          	mov    0x20(%rdx),%r10
  4027e3:	48 8b 7a 18          	mov    0x18(%rdx),%rdi
  4027e7:	4c 01 d7             	add    %r10,%rdi
  4027ea:	48 21 fb             	and    %rdi,%rbx
  4027ed:	49 29 dc             	sub    %rbx,%r12
  4027f0:	48 85 db             	test   %rbx,%rbx
  4027f3:	4c 0f 44 e3          	cmove  %rbx,%r12
  4027f7:	31 c9                	xor    %ecx,%ecx
  4027f9:	4c 89 ce             	mov    %r9,%rsi
  4027fc:	b0 01                	mov    $0x1,%al
  4027fe:	4c 01 e6             	add    %r12,%rsi
  402801:	73 0c                	jae    40280f <runtime::arena_alloc+0x1bf>
  402803:	45 31 db             	xor    %r11d,%r11d
  402806:	31 f6                	xor    %esi,%esi
  402808:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40280d:	eb 4f                	jmp    40285e <runtime::arena_alloc+0x20e>
  40280f:	31 c9                	xor    %ecx,%ecx
  402811:	49 01 f2             	add    %rsi,%r10
  402814:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402819:	73 07                	jae    402822 <runtime::arena_alloc+0x1d2>
  40281b:	45 31 db             	xor    %r11d,%r11d
  40281e:	31 f6                	xor    %esi,%esi
  402820:	eb 3c                	jmp    40285e <runtime::arena_alloc+0x20e>
  402822:	31 c9                	xor    %ecx,%ecx
  402824:	41 bb 00 00 00 00    	mov    $0x0,%r11d
  40282a:	be 00 00 00 00       	mov    $0x0,%esi
  40282f:	4c 3b 52 28          	cmp    0x28(%rdx),%r10
  402833:	77 29                	ja     40285e <runtime::arena_alloc+0x20e>
  402835:	4d 85 c9             	test   %r9,%r9
  402838:	78 68                	js     4028a2 <runtime::arena_alloc+0x252>
  40283a:	4c 01 e7             	add    %r12,%rdi
  40283d:	4c 89 52 20          	mov    %r10,0x20(%rdx)
  402841:	31 c9                	xor    %ecx,%ecx
  402843:	31 c0                	xor    %eax,%eax
  402845:	4d 89 cb             	mov    %r9,%r11
  402848:	48 89 fe             	mov    %rdi,%rsi
  40284b:	eb 11                	jmp    40285e <runtime::arena_alloc+0x20e>
  40284d:	4d 85 d2             	test   %r10,%r10
  402850:	78 31                	js     402883 <runtime::arena_alloc+0x233>
  402852:	48 01 d6             	add    %rdx,%rsi
  402855:	48 89 78 20          	mov    %rdi,0x20(%rax)
  402859:	31 c0                	xor    %eax,%eax
  40285b:	4d 89 d3             	mov    %r10,%r11
  40285e:	49 8b 56 10          	mov    0x10(%r14),%rdx
  402862:	48 8b 52 20          	mov    0x20(%rdx),%rdx
  402866:	48 29 ca             	sub    %rcx,%rdx
  402869:	49 01 56 18          	add    %rdx,0x18(%r14)
  40286d:	49 89 30             	mov    %rsi,(%r8)
  402870:	4d 89 58 08          	mov    %r11,0x8(%r8)
  402874:	48 83 c4 38          	add    $0x38,%rsp
  402878:	5b                   	pop    %rbx
  402879:	41 5c                	pop    %r12
  40287b:	41 5d                	pop    %r13
  40287d:	41 5e                	pop    %r14
  40287f:	41 5f                	pop    %r15
  402881:	5d                   	pop    %rbp
  402882:	c3                   	ret
  402883:	bf a0 40 40 00       	mov    $0x4040a0,%edi
  402888:	be 3c 00 00 00       	mov    $0x3c,%esi
  40288d:	ba 5c 00 00 00       	mov    $0x5c,%edx
  402892:	b9 31 00 00 00       	mov    $0x31,%ecx
  402897:	45 31 c0             	xor    %r8d,%r8d
  40289a:	4d 89 d1             	mov    %r10,%r9
  40289d:	e8 9e e8 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  4028a2:	bf a0 40 40 00       	mov    $0x4040a0,%edi
  4028a7:	be 3c 00 00 00       	mov    $0x3c,%esi
  4028ac:	ba 5c 00 00 00       	mov    $0x5c,%edx
  4028b1:	b9 31 00 00 00       	mov    $0x31,%ecx
  4028b6:	45 31 c0             	xor    %r8d,%r8d
  4028b9:	e8 82 e8 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  4028be:	49 8b 41 20          	mov    0x20(%r9),%rax
  4028c2:	48 85 c0             	test   %rax,%rax
  4028c5:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  4028cb:	4c 0f 45 d0          	cmovne %rax,%r10
  4028cf:	bf a0 43 40 00       	mov    $0x4043a0,%edi
  4028d4:	be 11 00 00 00       	mov    $0x11,%esi
  4028d9:	ba 64 42 40 00       	mov    $0x404264,%edx
  4028de:	b9 1a 00 00 00       	mov    $0x1a,%ecx
  4028e3:	4d 89 d8             	mov    %r11,%r8
  4028e6:	41 ff d2             	call   *%r10
  4028e9:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  4028ee:	49 8b 41 20          	mov    0x20(%r9),%rax
  4028f2:	48 85 c0             	test   %rax,%rax
  4028f5:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  4028fb:	4c 0f 45 d0          	cmovne %rax,%r10
  4028ff:	bf a0 43 40 00       	mov    $0x4043a0,%edi
  402904:	be 11 00 00 00       	mov    $0x11,%esi
  402909:	ba 90 40 40 00       	mov    $0x404090,%edx
  40290e:	b9 0f 00 00 00       	mov    $0xf,%ecx
  402913:	41 b8 f0 40 40 00    	mov    $0x4040f0,%r8d
  402919:	41 ff d2             	call   *%r10
  40291c:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  402921:	49 8b 41 20          	mov    0x20(%r9),%rax
  402925:	48 85 c0             	test   %rax,%rax
  402928:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  40292e:	4c 0f 45 d0          	cmovne %rax,%r10
  402932:	bf a0 43 40 00       	mov    $0x4043a0,%edi
  402937:	be 11 00 00 00       	mov    $0x11,%esi
  40293c:	ba 18 41 40 00       	mov    $0x404118,%edx
  402941:	b9 11 00 00 00       	mov    $0x11,%ecx
  402946:	41 b8 30 41 40 00    	mov    $0x404130,%r8d
  40294c:	41 ff d2             	call   *%r10
  40294f:	90                   	nop

0000000000402950 <runtime::print_byte>:
  402950:	40 88 7c 24 f8       	mov    %dil,-0x8(%rsp)
  402955:	48 8d 74 24 f8       	lea    -0x8(%rsp),%rsi
  40295a:	b8 01 00 00 00       	mov    $0x1,%eax
  40295f:	bf 02 00 00 00       	mov    $0x2,%edi
  402964:	ba 01 00 00 00       	mov    $0x1,%edx
  402969:	0f 05                	syscall
  40296b:	c3                   	ret
  40296c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402970 <runtime::mem_alloc>:
  402970:	41 56                	push   %r14
  402972:	53                   	push   %rbx
  402973:	48 83 ec 18          	sub    $0x18,%rsp
  402977:	4c 89 c0             	mov    %r8,%rax
  40297a:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  40297f:	48 85 f6             	test   %rsi,%rsi
  402982:	7e 79                	jle    4029fd <runtime::mem_alloc+0x8d>
  402984:	49 89 f0             	mov    %rsi,%r8
  402987:	f3 48 0f b8 f6       	popcnt %rsi,%rsi
  40298c:	83 fe 02             	cmp    $0x2,%esi
  40298f:	73 6c                	jae    4029fd <runtime::mem_alloc+0x8d>
  402991:	4c 89 cb             	mov    %r9,%rbx
  402994:	49 89 d3             	mov    %rdx,%r11
  402997:	48 89 fa             	mov    %rdi,%rdx
  40299a:	48 85 ff             	test   %rdi,%rdi
  40299d:	40 0f 94 c6          	sete   %sil
  4029a1:	4d 85 db             	test   %r11,%r11
  4029a4:	40 0f 94 c7          	sete   %dil
  4029a8:	40 08 f7             	or     %sil,%dil
  4029ab:	40 80 ff 01          	cmp    $0x1,%dil
  4029af:	75 08                	jne    4029b9 <runtime::mem_alloc+0x49>
  4029b1:	31 c9                	xor    %ecx,%ecx
  4029b3:	31 d2                	xor    %edx,%edx
  4029b5:	31 c0                	xor    %eax,%eax
  4029b7:	eb 35                	jmp    4029ee <runtime::mem_alloc+0x7e>
  4029b9:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4029bd:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  4029c2:	48 83 ec 08          	sub    $0x8,%rsp
  4029c6:	4c 8d 74 24 08       	lea    0x8(%rsp),%r14
  4029cb:	48 89 cf             	mov    %rcx,%rdi
  4029ce:	31 f6                	xor    %esi,%esi
  4029d0:	4c 89 c1             	mov    %r8,%rcx
  4029d3:	45 31 c0             	xor    %r8d,%r8d
  4029d6:	45 31 c9             	xor    %r9d,%r9d
  4029d9:	41 52                	push   %r10
  4029db:	41 56                	push   %r14
  4029dd:	50                   	push   %rax
  4029de:	41 ff d3             	call   *%r11
  4029e1:	48 83 c4 20          	add    $0x20,%rsp
  4029e5:	48 8b 0c 24          	mov    (%rsp),%rcx
  4029e9:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4029ee:	48 89 0b             	mov    %rcx,(%rbx)
  4029f1:	48 89 53 08          	mov    %rdx,0x8(%rbx)
  4029f5:	48 83 c4 18          	add    $0x18,%rsp
  4029f9:	5b                   	pop    %rbx
  4029fa:	41 5e                	pop    %r14
  4029fc:	c3                   	ret
  4029fd:	49 8b 4a 20          	mov    0x20(%r10),%rcx
  402a01:	48 85 c9             	test   %rcx,%rcx
  402a04:	41 bb b0 11 40 00    	mov    $0x4011b0,%r11d
  402a0a:	4c 0f 45 d9          	cmovne %rcx,%r11
  402a0e:	bf a0 43 40 00       	mov    $0x4043a0,%edi
  402a13:	be 11 00 00 00       	mov    $0x11,%esi
  402a18:	ba 43 42 40 00       	mov    $0x404243,%edx
  402a1d:	b9 20 00 00 00       	mov    $0x20,%ecx
  402a22:	49 89 c0             	mov    %rax,%r8
  402a25:	4d 89 d1             	mov    %r10,%r9
  402a28:	41 ff d3             	call   *%r11
  402a2b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402a30 <runtime::print_u64>:
  402a30:	50                   	push   %rax
  402a31:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402a35:	c5 fc 11 44 24 e0    	vmovups %ymm0,-0x20(%rsp)
  402a3b:	c5 fc 11 44 24 c0    	vmovups %ymm0,-0x40(%rsp)
  402a41:	c5 fc 11 44 24 a0    	vmovups %ymm0,-0x60(%rsp)
  402a47:	48 89 fa             	mov    %rdi,%rdx
  402a4a:	c5 fc 11 44 24 80    	vmovups %ymm0,-0x80(%rsp)
  402a50:	c6 04 24 00          	movb   $0x0,(%rsp)
  402a54:	b8 81 00 00 00       	mov    $0x81,%eax
  402a59:	48 83 ff 0a          	cmp    $0xa,%rdi
  402a5d:	72 48                	jb     402aa7 <runtime::print_u64+0x77>
  402a5f:	be 81 00 00 00       	mov    $0x81,%esi
  402a64:	48 b9 cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rcx
  402a6b:	cc cc cc 
  402a6e:	66 90                	xchg   %ax,%ax
  402a70:	48 8d 46 ff          	lea    -0x1(%rsi),%rax
  402a74:	c4 e2 c3 f6 f9       	mulx   %rcx,%rdi,%rdi
  402a79:	48 c1 ef 03          	shr    $0x3,%rdi
  402a7d:	4c 8d 04 3f          	lea    (%rdi,%rdi,1),%r8
  402a81:	4f 8d 04 80          	lea    (%r8,%r8,4),%r8
  402a85:	49 f7 d8             	neg    %r8
  402a88:	46 0f b6 84 02 e0 43 	movzbl 0x4043e0(%rdx,%r8,1),%r8d
  402a8f:	40 00 
  402a91:	44 88 84 34 7f ff ff 	mov    %r8b,-0x81(%rsp,%rsi,1)
  402a98:	ff 
  402a99:	48 89 c6             	mov    %rax,%rsi
  402a9c:	48 83 fa 63          	cmp    $0x63,%rdx
  402aa0:	48 89 fa             	mov    %rdi,%rdx
  402aa3:	77 cb                	ja     402a70 <runtime::print_u64+0x40>
  402aa5:	eb 03                	jmp    402aaa <runtime::print_u64+0x7a>
  402aa7:	48 89 d7             	mov    %rdx,%rdi
  402aaa:	48 8d 4c 24 80       	lea    -0x80(%rsp),%rcx
  402aaf:	48 8d 34 08          	lea    (%rax,%rcx,1),%rsi
  402ab3:	48 ff ce             	dec    %rsi
  402ab6:	0f b6 97 e0 43 40 00 	movzbl 0x4043e0(%rdi),%edx
  402abd:	88 54 08 ff          	mov    %dl,-0x1(%rax,%rcx,1)
  402ac1:	ba 82 00 00 00       	mov    $0x82,%edx
  402ac6:	48 29 c2             	sub    %rax,%rdx
  402ac9:	b8 01 00 00 00       	mov    $0x1,%eax
  402ace:	bf 02 00 00 00       	mov    $0x2,%edi
  402ad3:	0f 05                	syscall
  402ad5:	58                   	pop    %rax
  402ad6:	c5 f8 77             	vzeroupper
  402ad9:	c3                   	ret
  402ada:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402ae0 <runtime::mem_free>:
  402ae0:	48 85 f6             	test   %rsi,%rsi
  402ae3:	74 3e                	je     402b23 <runtime::mem_free+0x43>
  402ae5:	53                   	push   %rbx
  402ae6:	48 83 ec 10          	sub    $0x10,%rsp
  402aea:	4d 89 c3             	mov    %r8,%r11
  402aed:	49 89 ca             	mov    %rcx,%r10
  402af0:	48 89 f0             	mov    %rsi,%rax
  402af3:	49 89 f8             	mov    %rdi,%r8
  402af6:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402afa:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  402aff:	48 83 ec 08          	sub    $0x8,%rsp
  402b03:	48 8d 5c 24 08       	lea    0x8(%rsp),%rbx
  402b08:	48 89 d7             	mov    %rdx,%rdi
  402b0b:	be 01 00 00 00       	mov    $0x1,%esi
  402b10:	31 d2                	xor    %edx,%edx
  402b12:	31 c9                	xor    %ecx,%ecx
  402b14:	45 31 c9             	xor    %r9d,%r9d
  402b17:	41 53                	push   %r11
  402b19:	53                   	push   %rbx
  402b1a:	41 52                	push   %r10
  402b1c:	ff d0                	call   *%rax
  402b1e:	48 83 c4 30          	add    $0x30,%rsp
  402b22:	5b                   	pop    %rbx
  402b23:	c3                   	ret
  402b24:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402b2b:	00 00 00 00 00 

0000000000402b30 <runtime::print_i64>:
  402b30:	50                   	push   %rax
  402b31:	48 89 fa             	mov    %rdi,%rdx
  402b34:	48 f7 da             	neg    %rdx
  402b37:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402b3b:	c5 fc 11 44 24 e0    	vmovups %ymm0,-0x20(%rsp)
  402b41:	c5 fc 11 44 24 c0    	vmovups %ymm0,-0x40(%rsp)
  402b47:	c5 fc 11 44 24 a0    	vmovups %ymm0,-0x60(%rsp)
  402b4d:	48 0f 48 d7          	cmovs  %rdi,%rdx
  402b51:	c5 fc 11 44 24 80    	vmovups %ymm0,-0x80(%rsp)
  402b57:	c6 04 24 00          	movb   $0x0,(%rsp)
  402b5b:	41 b8 81 00 00 00    	mov    $0x81,%r8d
  402b61:	48 83 fa 0a          	cmp    $0xa,%rdx
  402b65:	7c 50                	jl     402bb7 <runtime::print_i64+0x87>
  402b67:	be 81 00 00 00       	mov    $0x81,%esi
  402b6c:	48 b8 cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rax
  402b73:	cc cc cc 
  402b76:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  402b7d:	00 00 00 
  402b80:	4c 8d 46 ff          	lea    -0x1(%rsi),%r8
  402b84:	c4 e2 f3 f6 c8       	mulx   %rax,%rcx,%rcx
  402b89:	48 c1 e9 03          	shr    $0x3,%rcx
  402b8d:	4c 8d 0c 09          	lea    (%rcx,%rcx,1),%r9
  402b91:	4f 8d 0c 89          	lea    (%r9,%r9,4),%r9
  402b95:	49 f7 d9             	neg    %r9
  402b98:	46 0f b6 8c 0a e0 43 	movzbl 0x4043e0(%rdx,%r9,1),%r9d
  402b9f:	40 00 
  402ba1:	44 88 8c 34 7f ff ff 	mov    %r9b,-0x81(%rsp,%rsi,1)
  402ba8:	ff 
  402ba9:	4c 89 c6             	mov    %r8,%rsi
  402bac:	48 83 fa 63          	cmp    $0x63,%rdx
  402bb0:	48 89 ca             	mov    %rcx,%rdx
  402bb3:	77 cb                	ja     402b80 <runtime::print_i64+0x50>
  402bb5:	eb 03                	jmp    402bba <runtime::print_i64+0x8a>
  402bb7:	48 89 d1             	mov    %rdx,%rcx
  402bba:	48 ba 67 66 66 66 66 	movabs $0x6666666666666667,%rdx
  402bc1:	66 66 66 
  402bc4:	48 89 c8             	mov    %rcx,%rax
  402bc7:	48 f7 ea             	imul   %rdx
  402bca:	48 89 d0             	mov    %rdx,%rax
  402bcd:	48 c1 e8 3f          	shr    $0x3f,%rax
  402bd1:	48 c1 fa 02          	sar    $0x2,%rdx
  402bd5:	48 01 c2             	add    %rax,%rdx
  402bd8:	48 01 d2             	add    %rdx,%rdx
  402bdb:	48 8d 04 92          	lea    (%rdx,%rdx,4),%rax
  402bdf:	48 f7 d8             	neg    %rax
  402be2:	0f b6 84 01 e0 43 40 	movzbl 0x4043e0(%rcx,%rax,1),%eax
  402be9:	00 
  402bea:	42 88 84 04 7f ff ff 	mov    %al,-0x81(%rsp,%r8,1)
  402bf1:	ff 
  402bf2:	48 85 ff             	test   %rdi,%rdi
  402bf5:	78 05                	js     402bfc <runtime::print_i64+0xcc>
  402bf7:	49 ff c8             	dec    %r8
  402bfa:	eb 0d                	jmp    402c09 <runtime::print_i64+0xd9>
  402bfc:	42 c6 84 04 7e ff ff 	movb   $0x2d,-0x82(%rsp,%r8,1)
  402c03:	ff 2d 
  402c05:	49 83 c0 fe          	add    $0xfffffffffffffffe,%r8
  402c09:	4a 8d 34 04          	lea    (%rsp,%r8,1),%rsi
  402c0d:	48 83 c6 80          	add    $0xffffffffffffff80,%rsi
  402c11:	ba 81 00 00 00       	mov    $0x81,%edx
  402c16:	4c 29 c2             	sub    %r8,%rdx
  402c19:	b8 01 00 00 00       	mov    $0x1,%eax
  402c1e:	bf 02 00 00 00       	mov    $0x2,%edi
  402c23:	0f 05                	syscall
  402c25:	58                   	pop    %rax
  402c26:	c5 f8 77             	vzeroupper
  402c29:	c3                   	ret
  402c2a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402c30 <runtime::mem_free_with_size>:
  402c30:	48 89 d0             	mov    %rdx,%rax
  402c33:	49 89 f1             	mov    %rsi,%r9
  402c36:	48 85 ff             	test   %rdi,%rdi
  402c39:	0f 94 c2             	sete   %dl
  402c3c:	48 85 c0             	test   %rax,%rax
  402c3f:	40 0f 94 c6          	sete   %sil
  402c43:	40 08 d6             	or     %dl,%sil
  402c46:	40 80 fe 01          	cmp    $0x1,%sil
  402c4a:	74 37                	je     402c83 <runtime::mem_free_with_size+0x53>
  402c4c:	48 83 ec 18          	sub    $0x18,%rsp
  402c50:	4d 89 c2             	mov    %r8,%r10
  402c53:	49 89 f8             	mov    %rdi,%r8
  402c56:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402c5a:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  402c5f:	48 83 ec 08          	sub    $0x8,%rsp
  402c63:	4c 8d 5c 24 08       	lea    0x8(%rsp),%r11
  402c68:	48 89 cf             	mov    %rcx,%rdi
  402c6b:	be 01 00 00 00       	mov    $0x1,%esi
  402c70:	31 d2                	xor    %edx,%edx
  402c72:	31 c9                	xor    %ecx,%ecx
  402c74:	41 52                	push   %r10
  402c76:	41 53                	push   %r11
  402c78:	68 20 43 40 00       	push   $0x404320
  402c7d:	ff d0                	call   *%rax
  402c7f:	48 83 c4 38          	add    $0x38,%rsp
  402c83:	c3                   	ret
  402c84:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402c8b:	00 00 00 00 00 

0000000000402c90 <runtime::print_caller_location>:
  402c90:	53                   	push   %rbx
  402c91:	48 89 fb             	mov    %rdi,%rbx
  402c94:	48 8b 3f             	mov    (%rdi),%rdi
  402c97:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  402c9b:	e8 10 f9 ff ff       	call   4025b0 <runtime::print_string>
  402ca0:	bf 28 00 00 00       	mov    $0x28,%edi
  402ca5:	e8 a6 fc ff ff       	call   402950 <runtime::print_byte>
  402caa:	48 63 7b 10          	movslq 0x10(%rbx),%rdi
  402cae:	e8 7d fd ff ff       	call   402a30 <runtime::print_u64>
  402cb3:	83 7b 14 00          	cmpl   $0x0,0x14(%rbx)
  402cb7:	74 13                	je     402ccc <runtime::print_caller_location+0x3c>
  402cb9:	bf 3a 00 00 00       	mov    $0x3a,%edi
  402cbe:	e8 8d fc ff ff       	call   402950 <runtime::print_byte>
  402cc3:	48 63 7b 14          	movslq 0x14(%rbx),%rdi
  402cc7:	e8 64 fd ff ff       	call   402a30 <runtime::print_u64>
  402ccc:	bf 29 00 00 00       	mov    $0x29,%edi
  402cd1:	5b                   	pop    %rbx
  402cd2:	e9 79 fc ff ff       	jmp    402950 <runtime::print_byte>
  402cd7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  402cde:	00 00 

0000000000402ce0 <runtime::default_logger_proc>:
  402ce0:	c3                   	ret
  402ce1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402ce8:	0f 1f 84 00 00 00 00 
  402cef:	00 

0000000000402cf0 <__truncsfhf2>:
  402cf0:	c5 f9 7e c7          	vmovd  %xmm0,%edi
  402cf4:	89 f8                	mov    %edi,%eax
  402cf6:	c1 e8 10             	shr    $0x10,%eax
  402cf9:	25 00 80 00 00       	and    $0x8000,%eax
  402cfe:	89 fa                	mov    %edi,%edx
  402d00:	c1 ea 17             	shr    $0x17,%edx
  402d03:	0f b6 f2             	movzbl %dl,%esi
  402d06:	89 f9                	mov    %edi,%ecx
  402d08:	81 e1 ff ff 7f 00    	and    $0x7fffff,%ecx
  402d0e:	83 fe 70             	cmp    $0x70,%esi
  402d11:	77 32                	ja     402d45 <__truncsfhf2+0x55>
  402d13:	83 fe 66             	cmp    $0x66,%esi
  402d16:	0f 82 40 01 00 00    	jb     402e5c <__truncsfhf2+0x16c>
  402d1c:	81 c9 00 00 80 00    	or     $0x800000,%ecx
  402d22:	40 b6 71             	mov    $0x71,%sil
  402d25:	40 28 d6             	sub    %dl,%sil
  402d28:	c4 e2 4b f7 c9       	shrx   %esi,%ecx,%ecx
  402d2d:	89 ca                	mov    %ecx,%edx
  402d2f:	81 e2 00 10 00 00    	and    $0x1000,%edx
  402d35:	8d 0c 51             	lea    (%rcx,%rdx,2),%ecx
  402d38:	c1 e9 0d             	shr    $0xd,%ecx
  402d3b:	09 c1                	or     %eax,%ecx
  402d3d:	89 c8                	mov    %ecx,%eax
  402d3f:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  402d44:	c3                   	ret
  402d45:	8d 56 90             	lea    -0x70(%rsi),%edx
  402d48:	81 fa 8f 00 00 00    	cmp    $0x8f,%edx
  402d4e:	75 2b                	jne    402d7b <__truncsfhf2+0x8b>
  402d50:	85 c9                	test   %ecx,%ecx
  402d52:	0f 84 ed 00 00 00    	je     402e45 <__truncsfhf2+0x155>
  402d58:	89 ca                	mov    %ecx,%edx
  402d5a:	c1 ea 0d             	shr    $0xd,%edx
  402d5d:	31 f6                	xor    %esi,%esi
  402d5f:	81 f9 00 20 00 00    	cmp    $0x2000,%ecx
  402d65:	40 0f 92 c6          	setb   %sil
  402d69:	09 c2                	or     %eax,%edx
  402d6b:	09 f2                	or     %esi,%edx
  402d6d:	81 ca 00 7c 00 00    	or     $0x7c00,%edx
  402d73:	89 d0                	mov    %edx,%eax
  402d75:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  402d7a:	c3                   	ret
  402d7b:	f7 c7 00 10 00 00    	test   $0x1000,%edi
  402d81:	74 1e                	je     402da1 <__truncsfhf2+0xb1>
  402d83:	8d b9 00 20 00 00    	lea    0x2000(%rcx),%edi
  402d89:	83 c6 91             	add    $0xffffff91,%esi
  402d8c:	45 31 c0             	xor    %r8d,%r8d
  402d8f:	81 f9 00 e0 7f 00    	cmp    $0x7fe000,%ecx
  402d95:	44 0f 42 c7          	cmovb  %edi,%r8d
  402d99:	0f 42 f2             	cmovb  %edx,%esi
  402d9c:	44 89 c1             	mov    %r8d,%ecx
  402d9f:	89 f2                	mov    %esi,%edx
  402da1:	83 fa 1f             	cmp    $0x1f,%edx
  402da4:	0f 82 a6 00 00 00    	jb     402e50 <__truncsfhf2+0x160>
  402daa:	48 b9 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rcx
  402db1:	00 00 00 
  402db4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402db9:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402dbe:	48 0f af c9          	imul   %rcx,%rcx
  402dc2:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402dc7:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402dcc:	48 0f af c9          	imul   %rcx,%rcx
  402dd0:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402dd5:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402dda:	48 0f af c9          	imul   %rcx,%rcx
  402dde:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402de3:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402de8:	48 0f af c9          	imul   %rcx,%rcx
  402dec:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402df1:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402df6:	48 0f af c9          	imul   %rcx,%rcx
  402dfa:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402dff:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402e04:	48 0f af c9          	imul   %rcx,%rcx
  402e08:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402e0d:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402e12:	48 0f af c9          	imul   %rcx,%rcx
  402e16:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402e1b:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402e20:	48 0f af c9          	imul   %rcx,%rcx
  402e24:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402e29:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402e2e:	48 0f af c9          	imul   %rcx,%rcx
  402e32:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402e37:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402e3c:	48 0f af c9          	imul   %rcx,%rcx
  402e40:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402e45:	0d 00 7c 00 00       	or     $0x7c00,%eax
  402e4a:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  402e4f:	c3                   	ret
  402e50:	c1 e2 0a             	shl    $0xa,%edx
  402e53:	c1 e9 0d             	shr    $0xd,%ecx
  402e56:	09 c1                	or     %eax,%ecx
  402e58:	09 d1                	or     %edx,%ecx
  402e5a:	89 c8                	mov    %ecx,%eax
  402e5c:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  402e61:	c3                   	ret
  402e62:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402e69:	1f 84 00 00 00 00 00 

0000000000402e70 <__truncdfhf2>:
  402e70:	c5 fb 5a c0          	vcvtsd2ss %xmm0,%xmm0,%xmm0
  402e74:	e9 77 fe ff ff       	jmp    402cf0 <__truncsfhf2>
  402e79:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000402e80 <__gnu_h2f_ieee>:
  402e80:	c5 f9 c5 c0 00       	vpextrw $0x0,%xmm0,%eax
  402e85:	89 c1                	mov    %eax,%ecx
  402e87:	81 e1 ff 7f 00 00    	and    $0x7fff,%ecx
  402e8d:	c1 e1 0d             	shl    $0xd,%ecx
  402e90:	c5 f9 6e c1          	vmovd  %ecx,%xmm0
  402e94:	c5 fa 59 05 6c 11 00 	vmulss 0x116c(%rip),%xmm0,%xmm0        # 404008 <_IO_stdin_used+0x8>
  402e9b:	00 
  402e9c:	c5 f9 7e c1          	vmovd  %xmm0,%ecx
  402ea0:	89 ca                	mov    %ecx,%edx
  402ea2:	81 ca 00 00 80 7f    	or     $0x7f800000,%edx
  402ea8:	c5 f8 2e 05 5c 11 00 	vucomiss 0x115c(%rip),%xmm0        # 40400c <_IO_stdin_used+0xc>
  402eaf:	00 
  402eb0:	0f 42 d1             	cmovb  %ecx,%edx
  402eb3:	25 00 80 00 00       	and    $0x8000,%eax
  402eb8:	c1 e0 10             	shl    $0x10,%eax
  402ebb:	09 d0                	or     %edx,%eax
  402ebd:	c5 f9 6e c0          	vmovd  %eax,%xmm0
  402ec1:	c3                   	ret
  402ec2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402ec9:	1f 84 00 00 00 00 00 

0000000000402ed0 <__gnu_f2h_ieee>:
  402ed0:	e9 1b fe ff ff       	jmp    402cf0 <__truncsfhf2>
  402ed5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402edc:	00 00 00 00 

0000000000402ee0 <__extendhfsf2>:
  402ee0:	c5 f9 c5 c0 00       	vpextrw $0x0,%xmm0,%eax
  402ee5:	89 c1                	mov    %eax,%ecx
  402ee7:	81 e1 ff 7f 00 00    	and    $0x7fff,%ecx
  402eed:	c1 e1 0d             	shl    $0xd,%ecx
  402ef0:	c5 f9 6e c1          	vmovd  %ecx,%xmm0
  402ef4:	c5 fa 59 05 0c 11 00 	vmulss 0x110c(%rip),%xmm0,%xmm0        # 404008 <_IO_stdin_used+0x8>
  402efb:	00 
  402efc:	c5 f9 7e c1          	vmovd  %xmm0,%ecx
  402f00:	89 ca                	mov    %ecx,%edx
  402f02:	81 ca 00 00 80 7f    	or     $0x7f800000,%edx
  402f08:	c5 f8 2e 05 fc 10 00 	vucomiss 0x10fc(%rip),%xmm0        # 40400c <_IO_stdin_used+0xc>
  402f0f:	00 
  402f10:	0f 42 d1             	cmovb  %ecx,%edx
  402f13:	25 00 80 00 00       	and    $0x8000,%eax
  402f18:	c1 e0 10             	shl    $0x10,%eax
  402f1b:	09 d0                	or     %edx,%eax
  402f1d:	c5 f9 6e c0          	vmovd  %eax,%xmm0
  402f21:	c3                   	ret
  402f22:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402f29:	1f 84 00 00 00 00 00 

0000000000402f30 <__floattidf>:
  402f30:	48 89 f8             	mov    %rdi,%rax
  402f33:	48 09 f0             	or     %rsi,%rax
  402f36:	74 5b                	je     402f93 <__floattidf+0x63>
  402f38:	48 89 f2             	mov    %rsi,%rdx
  402f3b:	48 c1 fa 3f          	sar    $0x3f,%rdx
  402f3f:	48 31 d6             	xor    %rdx,%rsi
  402f42:	48 31 d7             	xor    %rdx,%rdi
  402f45:	48 29 d7             	sub    %rdx,%rdi
  402f48:	48 19 d6             	sbb    %rdx,%rsi
  402f4b:	f3 48 0f bd ce       	lzcnt  %rsi,%rcx
  402f50:	f3 48 0f bd c7       	lzcnt  %rdi,%rax
  402f55:	48 83 c0 40          	add    $0x40,%rax
  402f59:	48 85 f6             	test   %rsi,%rsi
  402f5c:	48 0f 45 c1          	cmovne %rcx,%rax
  402f60:	41 89 c0             	mov    %eax,%r8d
  402f63:	41 83 f0 7f          	xor    $0x7f,%r8d
  402f67:	48 89 f9             	mov    %rdi,%rcx
  402f6a:	48 c1 e9 35          	shr    $0x35,%rcx
  402f6e:	48 09 f1             	or     %rsi,%rcx
  402f71:	74 25                	je     402f98 <__floattidf+0x68>
  402f73:	89 c1                	mov    %eax,%ecx
  402f75:	80 e1 7f             	and    $0x7f,%cl
  402f78:	80 f9 49             	cmp    $0x49,%cl
  402f7b:	0f 84 ba 00 00 00    	je     40303b <__floattidf+0x10b>
  402f81:	83 f8 4a             	cmp    $0x4a,%eax
  402f84:	75 26                	jne    402fac <__floattidf+0x7c>
  402f86:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  402f8b:	48 01 ff             	add    %rdi,%rdi
  402f8e:	e9 a8 00 00 00       	jmp    40303b <__floattidf+0x10b>
  402f93:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402f97:	c3                   	ret
  402f98:	04 b5                	add    $0xb5,%al
  402f9a:	c4 e2 f9 f7 cf       	shlx   %rax,%rdi,%rcx
  402f9f:	31 f6                	xor    %esi,%esi
  402fa1:	a8 40                	test   $0x40,%al
  402fa3:	48 0f 44 f1          	cmove  %rcx,%rsi
  402fa7:	e9 be 00 00 00       	jmp    40306a <__floattidf+0x13a>
  402fac:	41 57                	push   %r15
  402fae:	41 56                	push   %r14
  402fb0:	53                   	push   %rbx
  402fb1:	45 31 db             	xor    %r11d,%r11d
  402fb4:	b9 49 00 00 00       	mov    $0x49,%ecx
  402fb9:	48 29 c1             	sub    %rax,%rcx
  402fbc:	bb 00 00 00 00       	mov    $0x0,%ebx
  402fc1:	48 19 db             	sbb    %rbx,%rbx
  402fc4:	49 89 f9             	mov    %rdi,%r9
  402fc7:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  402fcb:	c4 62 f3 f7 d6       	shrx   %rcx,%rsi,%r10
  402fd0:	f6 c1 40             	test   $0x40,%cl
  402fd3:	4d 0f 45 ca          	cmovne %r10,%r9
  402fd7:	4d 0f 45 d3          	cmovne %r11,%r10
  402fdb:	48 81 f9 80 00 00 00 	cmp    $0x80,%rcx
  402fe2:	48 83 db 00          	sbb    $0x0,%rbx
  402fe6:	4d 0f 43 d3          	cmovae %r11,%r10
  402fea:	4d 0f 43 cb          	cmovae %r11,%r9
  402fee:	48 89 fb             	mov    %rdi,%rbx
  402ff1:	48 c1 eb 37          	shr    $0x37,%rbx
  402ff5:	41 be ff 01 00 00    	mov    $0x1ff,%r14d
  402ffb:	49 c7 c7 ff ff ff ff 	mov    $0xffffffffffffffff,%r15
  403002:	89 c1                	mov    %eax,%ecx
  403004:	4d 0f ad f7          	shrd   %cl,%r14,%r15
  403008:	c4 c2 fb f7 ce       	shrx   %rax,%r14,%rcx
  40300d:	a8 40                	test   $0x40,%al
  40300f:	4c 0f 45 f9          	cmovne %rcx,%r15
  403013:	49 0f 45 cb          	cmovne %r11,%rcx
  403017:	48 09 f3             	or     %rsi,%rbx
  40301a:	4d 0f 44 fb          	cmove  %r11,%r15
  40301e:	41 0f 44 cb          	cmove  %r11d,%ecx
  403022:	49 21 ff             	and    %rdi,%r15
  403025:	21 ce                	and    %ecx,%esi
  403027:	31 ff                	xor    %edi,%edi
  403029:	4c 09 fe             	or     %r15,%rsi
  40302c:	40 0f 95 c7          	setne  %dil
  403030:	4c 09 cf             	or     %r9,%rdi
  403033:	4c 89 d6             	mov    %r10,%rsi
  403036:	5b                   	pop    %rbx
  403037:	41 5e                	pop    %r14
  403039:	41 5f                	pop    %r15
  40303b:	89 f9                	mov    %edi,%ecx
  40303d:	c1 e9 02             	shr    $0x2,%ecx
  403040:	83 e1 01             	and    $0x1,%ecx
  403043:	48 09 f9             	or     %rdi,%rcx
  403046:	48 83 c1 01          	add    $0x1,%rcx
  40304a:	48 83 d6 00          	adc    $0x0,%rsi
  40304e:	48 0f ba e1 37       	bt     $0x37,%rcx
  403053:	72 07                	jb     40305c <__floattidf+0x12c>
  403055:	48 0f a4 ce 3e       	shld   $0x3e,%rcx,%rsi
  40305a:	eb 0e                	jmp    40306a <__floattidf+0x13a>
  40305c:	48 0f a4 ce 3d       	shld   $0x3d,%rcx,%rsi
  403061:	41 b8 80 00 00 00    	mov    $0x80,%r8d
  403067:	41 29 c0             	sub    %eax,%r8d
  40306a:	81 e2 00 00 00 80    	and    $0x80000000,%edx
  403070:	41 c1 e0 14          	shl    $0x14,%r8d
  403074:	41 09 d0             	or     %edx,%r8d
  403077:	48 89 f0             	mov    %rsi,%rax
  40307a:	48 c1 e8 20          	shr    $0x20,%rax
  40307e:	25 ff ff 0f 00       	and    $0xfffff,%eax
  403083:	44 01 c0             	add    %r8d,%eax
  403086:	05 00 00 f0 3f       	add    $0x3ff00000,%eax
  40308b:	48 c1 e0 20          	shl    $0x20,%rax
  40308f:	89 f1                	mov    %esi,%ecx
  403091:	48 09 c1             	or     %rax,%rcx
  403094:	c4 e1 f9 6e c1       	vmovq  %rcx,%xmm0
  403099:	c3                   	ret
  40309a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004030a0 <__floattidf_unsigned>:
  4030a0:	48 89 f8             	mov    %rdi,%rax
  4030a3:	48 09 f0             	or     %rsi,%rax
  4030a6:	74 46                	je     4030ee <__floattidf_unsigned+0x4e>
  4030a8:	f3 48 0f bd ce       	lzcnt  %rsi,%rcx
  4030ad:	f3 48 0f bd c7       	lzcnt  %rdi,%rax
  4030b2:	48 83 c0 40          	add    $0x40,%rax
  4030b6:	48 85 f6             	test   %rsi,%rsi
  4030b9:	48 0f 45 c1          	cmovne %rcx,%rax
  4030bd:	89 c2                	mov    %eax,%edx
  4030bf:	83 f2 7f             	xor    $0x7f,%edx
  4030c2:	48 89 f9             	mov    %rdi,%rcx
  4030c5:	48 c1 e9 35          	shr    $0x35,%rcx
  4030c9:	48 09 f1             	or     %rsi,%rcx
  4030cc:	74 25                	je     4030f3 <__floattidf_unsigned+0x53>
  4030ce:	89 c1                	mov    %eax,%ecx
  4030d0:	80 e1 7f             	and    $0x7f,%cl
  4030d3:	80 f9 49             	cmp    $0x49,%cl
  4030d6:	0f 84 b6 00 00 00    	je     403192 <__floattidf_unsigned+0xf2>
  4030dc:	83 f8 4a             	cmp    $0x4a,%eax
  4030df:	75 26                	jne    403107 <__floattidf_unsigned+0x67>
  4030e1:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  4030e6:	48 01 ff             	add    %rdi,%rdi
  4030e9:	e9 a4 00 00 00       	jmp    403192 <__floattidf_unsigned+0xf2>
  4030ee:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4030f2:	c3                   	ret
  4030f3:	04 b5                	add    $0xb5,%al
  4030f5:	c4 e2 f9 f7 cf       	shlx   %rax,%rdi,%rcx
  4030fa:	31 f6                	xor    %esi,%esi
  4030fc:	a8 40                	test   $0x40,%al
  4030fe:	48 0f 44 f1          	cmove  %rcx,%rsi
  403102:	e9 b8 00 00 00       	jmp    4031bf <__floattidf_unsigned+0x11f>
  403107:	41 56                	push   %r14
  403109:	53                   	push   %rbx
  40310a:	45 31 d2             	xor    %r10d,%r10d
  40310d:	b9 49 00 00 00       	mov    $0x49,%ecx
  403112:	48 29 c1             	sub    %rax,%rcx
  403115:	41 bb 00 00 00 00    	mov    $0x0,%r11d
  40311b:	4d 19 db             	sbb    %r11,%r11
  40311e:	49 89 f8             	mov    %rdi,%r8
  403121:	49 0f ad f0          	shrd   %cl,%rsi,%r8
  403125:	c4 62 f3 f7 ce       	shrx   %rcx,%rsi,%r9
  40312a:	f6 c1 40             	test   $0x40,%cl
  40312d:	4d 0f 45 c1          	cmovne %r9,%r8
  403131:	4d 0f 45 ca          	cmovne %r10,%r9
  403135:	48 81 f9 80 00 00 00 	cmp    $0x80,%rcx
  40313c:	49 83 db 00          	sbb    $0x0,%r11
  403140:	4d 0f 43 ca          	cmovae %r10,%r9
  403144:	4d 0f 43 c2          	cmovae %r10,%r8
  403148:	49 89 fb             	mov    %rdi,%r11
  40314b:	49 c1 eb 37          	shr    $0x37,%r11
  40314f:	bb ff 01 00 00       	mov    $0x1ff,%ebx
  403154:	49 c7 c6 ff ff ff ff 	mov    $0xffffffffffffffff,%r14
  40315b:	89 c1                	mov    %eax,%ecx
  40315d:	49 0f ad de          	shrd   %cl,%rbx,%r14
  403161:	c4 e2 fb f7 cb       	shrx   %rax,%rbx,%rcx
  403166:	a8 40                	test   $0x40,%al
  403168:	4c 0f 45 f1          	cmovne %rcx,%r14
  40316c:	49 0f 45 ca          	cmovne %r10,%rcx
  403170:	49 09 f3             	or     %rsi,%r11
  403173:	4d 0f 44 f2          	cmove  %r10,%r14
  403177:	41 0f 44 ca          	cmove  %r10d,%ecx
  40317b:	49 21 fe             	and    %rdi,%r14
  40317e:	21 ce                	and    %ecx,%esi
  403180:	31 ff                	xor    %edi,%edi
  403182:	4c 09 f6             	or     %r14,%rsi
  403185:	40 0f 95 c7          	setne  %dil
  403189:	4c 09 c7             	or     %r8,%rdi
  40318c:	4c 89 ce             	mov    %r9,%rsi
  40318f:	5b                   	pop    %rbx
  403190:	41 5e                	pop    %r14
  403192:	89 f9                	mov    %edi,%ecx
  403194:	c1 e9 02             	shr    $0x2,%ecx
  403197:	83 e1 01             	and    $0x1,%ecx
  40319a:	48 09 f9             	or     %rdi,%rcx
  40319d:	48 83 c1 01          	add    $0x1,%rcx
  4031a1:	48 83 d6 00          	adc    $0x0,%rsi
  4031a5:	48 0f ba e1 37       	bt     $0x37,%rcx
  4031aa:	72 07                	jb     4031b3 <__floattidf_unsigned+0x113>
  4031ac:	48 0f a4 ce 3e       	shld   $0x3e,%rcx,%rsi
  4031b1:	eb 0c                	jmp    4031bf <__floattidf_unsigned+0x11f>
  4031b3:	48 0f a4 ce 3d       	shld   $0x3d,%rcx,%rsi
  4031b8:	ba 80 00 00 00       	mov    $0x80,%edx
  4031bd:	29 c2                	sub    %eax,%edx
  4031bf:	c1 e2 14             	shl    $0x14,%edx
  4031c2:	48 89 f0             	mov    %rsi,%rax
  4031c5:	48 c1 e8 20          	shr    $0x20,%rax
  4031c9:	25 ff ff 0f 00       	and    $0xfffff,%eax
  4031ce:	01 d0                	add    %edx,%eax
  4031d0:	05 00 00 f0 3f       	add    $0x3ff00000,%eax
  4031d5:	48 c1 e0 20          	shl    $0x20,%rax
  4031d9:	89 f1                	mov    %esi,%ecx
  4031db:	48 09 c1             	or     %rax,%rcx
  4031de:	c4 e1 f9 6e c1       	vmovq  %rcx,%xmm0
  4031e3:	c3                   	ret
  4031e4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4031eb:	00 00 00 00 00 

00000000004031f0 <__umodti3>:
  4031f0:	48 83 ec 18          	sub    $0x18,%rsp
  4031f4:	49 89 e0             	mov    %rsp,%r8
  4031f7:	e8 e4 e4 ff ff       	call   4016e0 <runtime::udivmod128>
  4031fc:	48 8b 04 24          	mov    (%rsp),%rax
  403200:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403205:	48 83 c4 18          	add    $0x18,%rsp
  403209:	c3                   	ret
  40320a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000403210 <__udivmodti4>:
  403210:	e9 cb e4 ff ff       	jmp    4016e0 <runtime::udivmod128>
  403215:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40321c:	00 00 00 00 

0000000000403220 <__udivti3>:
  403220:	45 31 c0             	xor    %r8d,%r8d
  403223:	e9 b8 e4 ff ff       	jmp    4016e0 <runtime::udivmod128>

Disassembly of section .fini:

0000000000403228 <_fini>:
  403228:	f3 0f 1e fa          	endbr64
  40322c:	48 83 ec 08          	sub    $0x8,%rsp
  403230:	48 83 c4 08          	add    $0x8,%rsp
  403234:	c3                   	ret
