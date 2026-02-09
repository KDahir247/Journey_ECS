
/home/khalid/Documents/GitHub/Journey_ECS/main-release.bin:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 c1 3f 00 00 	mov    0x3fc1(%rip),%rax        # 404fd0 <__gmon_start__@Base>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 3f 00 00    	push   0x3fca(%rip)        # 404ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 3f 00 00    	jmp    *0x3fcc(%rip)        # 404ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 3f 00 00    	jmp    *0x3fca(%rip)        # 405000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <memset@plt>:
  401040:	ff 25 c2 3f 00 00    	jmp    *0x3fc2(%rip)        # 405008 <memset@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <calloc@plt>:
  401050:	ff 25 ba 3f 00 00    	jmp    *0x3fba(%rip)        # 405010 <calloc@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <memcpy@plt>:
  401060:	ff 25 b2 3f 00 00    	jmp    *0x3fb2(%rip)        # 405018 <memcpy@GLIBC_2.14>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <malloc@plt>:
  401070:	ff 25 aa 3f 00 00    	jmp    *0x3faa(%rip)        # 405020 <malloc@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <realloc@plt>:
  401080:	ff 25 a2 3f 00 00    	jmp    *0x3fa2(%rip)        # 405028 <realloc@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <memmove@plt>:
  401090:	ff 25 9a 3f 00 00    	jmp    *0x3f9a(%rip)        # 405030 <memmove@GLIBC_2.2.5>
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
  4010bc:	48 c7 44 24 08 a6 32 	movq   $0x4032a6,0x8(%rsp)
  4010c3:	40 00 
  4010c5:	48 c7 44 24 10 30 00 	movq   $0x30,0x10(%rsp)
  4010cc:	00 00 
  4010ce:	48 b8 4b 00 00 00 25 	movabs $0x250000004b,%rax
  4010d5:	00 00 00 
  4010d8:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4010dd:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  4010e2:	e8 b9 15 00 00       	call   4026a0 <runtime::print_caller_location>
  4010e7:	bf 08 32 40 00       	mov    $0x403208,%edi
  4010ec:	be 17 00 00 00       	mov    $0x17,%esi
  4010f1:	e8 ba 13 00 00       	call   4024b0 <runtime::print_string>
  4010f6:	4c 89 ff             	mov    %r15,%rdi
  4010f9:	e8 a2 14 00 00       	call   4025a0 <runtime::print_i64>
  4010fe:	bf 20 32 40 00       	mov    $0x403220,%edi
  401103:	be 01 00 00 00       	mov    $0x1,%esi
  401108:	e8 a3 13 00 00       	call   4024b0 <runtime::print_string>
  40110d:	4c 89 f7             	mov    %r14,%rdi
  401110:	e8 8b 14 00 00       	call   4025a0 <runtime::print_i64>
  401115:	bf 22 32 40 00       	mov    $0x403222,%edi
  40111a:	be 15 00 00 00       	mov    $0x15,%esi
  40111f:	e8 8c 13 00 00       	call   4024b0 <runtime::print_string>
  401124:	48 89 df             	mov    %rbx,%rdi
  401127:	e8 74 14 00 00       	call   4025a0 <runtime::print_i64>
  40112c:	bf 0a 00 00 00       	mov    $0xa,%edi
  401131:	e8 9a 13 00 00       	call   4024d0 <runtime::print_byte>
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
  40116b:	e8 30 15 00 00       	call   4026a0 <runtime::print_caller_location>
  401170:	bf 08 32 40 00       	mov    $0x403208,%edi
  401175:	be 17 00 00 00       	mov    $0x17,%esi
  40117a:	e8 31 13 00 00       	call   4024b0 <runtime::print_string>
  40117f:	4c 89 f7             	mov    %r14,%rdi
  401182:	e8 19 14 00 00       	call   4025a0 <runtime::print_i64>
  401187:	bf 20 32 40 00       	mov    $0x403220,%edi
  40118c:	be 01 00 00 00       	mov    $0x1,%esi
  401191:	e8 1a 13 00 00       	call   4024b0 <runtime::print_string>
  401196:	48 89 df             	mov    %rbx,%rdi
  401199:	e8 02 14 00 00       	call   4025a0 <runtime::print_i64>
  40119e:	bf 0a 00 00 00       	mov    $0xa,%edi
  4011a3:	e8 28 13 00 00       	call   4024d0 <runtime::print_byte>
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
  4011d7:	e8 c4 14 00 00       	call   4026a0 <runtime::print_caller_location>
  4011dc:	bf a1 32 40 00       	mov    $0x4032a1,%edi
  4011e1:	be 01 00 00 00       	mov    $0x1,%esi
  4011e6:	e8 c5 12 00 00       	call   4024b0 <runtime::print_string>
  4011eb:	4c 89 e7             	mov    %r12,%rdi
  4011ee:	4c 89 fe             	mov    %r15,%rsi
  4011f1:	e8 ba 12 00 00       	call   4024b0 <runtime::print_string>
  4011f6:	48 85 db             	test   %rbx,%rbx
  4011f9:	7e 1a                	jle    401215 <runtime::default_assertion_contextless_failure_proc+0x55>
  4011fb:	bf a3 32 40 00       	mov    $0x4032a3,%edi
  401200:	be 02 00 00 00       	mov    $0x2,%esi
  401205:	e8 a6 12 00 00       	call   4024b0 <runtime::print_string>
  40120a:	4c 89 f7             	mov    %r14,%rdi
  40120d:	48 89 de             	mov    %rbx,%rsi
  401210:	e8 9b 12 00 00       	call   4024b0 <runtime::print_string>
  401215:	bf 0a 00 00 00       	mov    $0xa,%edi
  40121a:	e8 b1 12 00 00       	call   4024d0 <runtime::print_byte>
  40121f:	0f 0b                	ud2
  401221:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401228:	00 00 00 
  40122b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401230 <_start>:
  401230:	f3 0f 1e fa          	endbr64
  401234:	31 ed                	xor    %ebp,%ebp
  401236:	49 89 d1             	mov    %rdx,%r9
  401239:	5e                   	pop    %rsi
  40123a:	48 89 e2             	mov    %rsp,%rdx
  40123d:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  401241:	50                   	push   %rax
  401242:	54                   	push   %rsp
  401243:	45 31 c0             	xor    %r8d,%r8d
  401246:	31 c9                	xor    %ecx,%ecx
  401248:	48 c7 c7 80 20 40 00 	mov    $0x402080,%rdi
  40124f:	ff 15 6b 3d 00 00    	call   *0x3d6b(%rip)        # 404fc0 <__libc_start_main@GLIBC_2.34>
  401255:	f4                   	hlt
  401256:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40125d:	00 00 00 

0000000000401260 <_dl_relocate_static_pie>:
  401260:	f3 0f 1e fa          	endbr64
  401264:	c3                   	ret
  401265:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40126c:	00 00 00 
  40126f:	90                   	nop

0000000000401270 <deregister_tm_clones>:
  401270:	b8 48 50 40 00       	mov    $0x405048,%eax
  401275:	48 3d 48 50 40 00    	cmp    $0x405048,%rax
  40127b:	74 13                	je     401290 <deregister_tm_clones+0x20>
  40127d:	48 8b 05 44 3d 00 00 	mov    0x3d44(%rip),%rax        # 404fc8 <_ITM_deregisterTMCloneTable@Base>
  401284:	48 85 c0             	test   %rax,%rax
  401287:	74 07                	je     401290 <deregister_tm_clones+0x20>
  401289:	bf 48 50 40 00       	mov    $0x405048,%edi
  40128e:	ff e0                	jmp    *%rax
  401290:	c3                   	ret
  401291:	0f 1f 40 00          	nopl   0x0(%rax)
  401295:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40129c:	00 00 00 00 

00000000004012a0 <register_tm_clones>:
  4012a0:	be 48 50 40 00       	mov    $0x405048,%esi
  4012a5:	48 81 ee 48 50 40 00 	sub    $0x405048,%rsi
  4012ac:	48 89 f0             	mov    %rsi,%rax
  4012af:	48 c1 ee 3f          	shr    $0x3f,%rsi
  4012b3:	48 c1 f8 03          	sar    $0x3,%rax
  4012b7:	48 01 c6             	add    %rax,%rsi
  4012ba:	48 d1 fe             	sar    $1,%rsi
  4012bd:	74 19                	je     4012d8 <register_tm_clones+0x38>
  4012bf:	48 8b 05 12 3d 00 00 	mov    0x3d12(%rip),%rax        # 404fd8 <_ITM_registerTMCloneTable@Base>
  4012c6:	48 85 c0             	test   %rax,%rax
  4012c9:	74 0d                	je     4012d8 <register_tm_clones+0x38>
  4012cb:	bf 48 50 40 00       	mov    $0x405048,%edi
  4012d0:	ff e0                	jmp    *%rax
  4012d2:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  4012d8:	c3                   	ret
  4012d9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004012e0 <__do_global_dtors_aux>:
  4012e0:	f3 0f 1e fa          	endbr64
  4012e4:	80 3d 5d 3d 00 00 00 	cmpb   $0x0,0x3d5d(%rip)        # 405048 <__TMC_END__>
  4012eb:	75 13                	jne    401300 <__do_global_dtors_aux+0x20>
  4012ed:	55                   	push   %rbp
  4012ee:	48 89 e5             	mov    %rsp,%rbp
  4012f1:	e8 7a ff ff ff       	call   401270 <deregister_tm_clones>
  4012f6:	c6 05 4b 3d 00 00 01 	movb   $0x1,0x3d4b(%rip)        # 405048 <__TMC_END__>
  4012fd:	5d                   	pop    %rbp
  4012fe:	c3                   	ret
  4012ff:	90                   	nop
  401300:	c3                   	ret
  401301:	0f 1f 40 00          	nopl   0x0(%rax)
  401305:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40130c:	00 00 00 00 

0000000000401310 <frame_dummy>:
  401310:	f3 0f 1e fa          	endbr64
  401314:	eb 8a                	jmp    4012a0 <register_tm_clones>
  401316:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40131d:	00 00 00 

0000000000401320 <__$startup_runtime>:
  401320:	eb 00                	jmp    401322 <__$startup_runtime+0x2>
  401322:	c3                   	ret
  401323:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40132a:	84 00 00 00 00 00 

0000000000401330 <__$cleanup_runtime>:
  401330:	50                   	push   %rax
  401331:	eb 00                	jmp    401333 <__$cleanup_runtime+0x3>
  401333:	e8 08 00 00 00       	call   401340 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  401338:	58                   	pop    %rax
  401339:	c3                   	ret
  40133a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401340 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  401340:	64 4c 8b 04 25 c0 ff 	mov    %fs:0xffffffffffffffc0,%r8
  401347:	ff ff 
  401349:	4d 85 c0             	test   %r8,%r8
  40134c:	0f 84 8f 00 00 00    	je     4013e1 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0xa1>
  401352:	41 56                	push   %r14
  401354:	53                   	push   %rbx
  401355:	48 83 ec 18          	sub    $0x18,%rsp
  401359:	48 89 fb             	mov    %rdi,%rbx
  40135c:	64 48 8b 04 25 d0 ff 	mov    %fs:0xffffffffffffffd0,%rax
  401363:	ff ff 
  401365:	49 89 e6             	mov    %rsp,%r14
  401368:	eb 0e                	jmp    401378 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x38>
  40136a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401370:	49 89 c8             	mov    %rcx,%r8
  401373:	48 85 c9             	test   %rcx,%rcx
  401376:	74 62                	je     4013da <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x9a>
  401378:	49 8b 08             	mov    (%r8),%rcx
  40137b:	64 48 89 0c 25 c0 ff 	mov    %rcx,%fs:0xffffffffffffffc0
  401382:	ff ff 
  401384:	49 2b 40 28          	sub    0x28(%r8),%rax
  401388:	64 48 89 04 25 d0 ff 	mov    %rax,%fs:0xffffffffffffffd0
  40138f:	ff ff 
  401391:	4d 8b 50 08          	mov    0x8(%r8),%r10
  401395:	4d 85 d2             	test   %r10,%r10
  401398:	74 d6                	je     401370 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x30>
  40139a:	49 8b 78 10          	mov    0x10(%r8),%rdi
  40139e:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4013a2:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  4013a7:	48 83 ec 08          	sub    $0x8,%rsp
  4013ab:	be 01 00 00 00       	mov    $0x1,%esi
  4013b0:	31 d2                	xor    %edx,%edx
  4013b2:	31 c9                	xor    %ecx,%ecx
  4013b4:	45 31 c9             	xor    %r9d,%r9d
  4013b7:	53                   	push   %rbx
  4013b8:	41 56                	push   %r14
  4013ba:	68 e0 31 40 00       	push   $0x4031e0
  4013bf:	41 ff d2             	call   *%r10
  4013c2:	48 83 c4 20          	add    $0x20,%rsp
  4013c6:	64 48 8b 0c 25 c0 ff 	mov    %fs:0xffffffffffffffc0,%rcx
  4013cd:	ff ff 
  4013cf:	64 48 8b 04 25 d0 ff 	mov    %fs:0xffffffffffffffd0,%rax
  4013d6:	ff ff 
  4013d8:	eb 96                	jmp    401370 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x30>
  4013da:	48 83 c4 18          	add    $0x18,%rsp
  4013de:	5b                   	pop    %rbx
  4013df:	41 5e                	pop    %r14
  4013e1:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4013e5:	64 c5 fc 11 04 25 c8 	vmovups %ymm0,%fs:0xffffffffffffffc8
  4013ec:	ff ff ff 
  4013ef:	64 c5 fc 11 04 25 b0 	vmovups %ymm0,%fs:0xffffffffffffffb0
  4013f6:	ff ff ff 
  4013f9:	c5 f8 77             	vzeroupper
  4013fc:	c3                   	ret
  4013fd:	0f 1f 00             	nopl   (%rax)

0000000000401400 <runtime::udivmod128>:
  401400:	55                   	push   %rbp
  401401:	41 57                	push   %r15
  401403:	41 56                	push   %r14
  401405:	41 55                	push   %r13
  401407:	41 54                	push   %r12
  401409:	53                   	push   %rbx
  40140a:	48 89 f8             	mov    %rdi,%rax
  40140d:	48 85 f6             	test   %rsi,%rsi
  401410:	74 32                	je     401444 <runtime::udivmod128+0x44>
  401412:	48 85 d2             	test   %rdx,%rdx
  401415:	74 4f                	je     401466 <runtime::udivmod128+0x66>
  401417:	48 85 c9             	test   %rcx,%rcx
  40141a:	0f 84 85 00 00 00    	je     4014a5 <runtime::udivmod128+0xa5>
  401420:	f3 48 0f bd f9       	lzcnt  %rcx,%rdi
  401425:	f3 4c 0f bd ce       	lzcnt  %rsi,%r9
  40142a:	44 29 cf             	sub    %r9d,%edi
  40142d:	83 ff 40             	cmp    $0x40,%edi
  401430:	0f 82 da 00 00 00    	jb     401510 <runtime::udivmod128+0x110>
  401436:	4d 85 c0             	test   %r8,%r8
  401439:	74 22                	je     40145d <runtime::udivmod128+0x5d>
  40143b:	49 89 00             	mov    %rax,(%r8)
  40143e:	49 89 70 08          	mov    %rsi,0x8(%r8)
  401442:	eb 19                	jmp    40145d <runtime::udivmod128+0x5d>
  401444:	48 85 c9             	test   %rcx,%rcx
  401447:	0f 84 a9 00 00 00    	je     4014f6 <runtime::udivmod128+0xf6>
  40144d:	4d 85 c0             	test   %r8,%r8
  401450:	74 0b                	je     40145d <runtime::udivmod128+0x5d>
  401452:	49 89 00             	mov    %rax,(%r8)
  401455:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  40145c:	00 
  40145d:	31 c0                	xor    %eax,%eax
  40145f:	31 f6                	xor    %esi,%esi
  401461:	e9 69 03 00 00       	jmp    4017cf <runtime::udivmod128+0x3cf>
  401466:	48 85 c9             	test   %rcx,%rcx
  401469:	0f 84 cd 00 00 00    	je     40153c <runtime::udivmod128+0x13c>
  40146f:	48 85 c0             	test   %rax,%rax
  401472:	0f 84 ec 00 00 00    	je     401564 <runtime::udivmod128+0x164>
  401478:	48 8d 79 ff          	lea    -0x1(%rcx),%rdi
  40147c:	48 85 f9             	test   %rdi,%rcx
  40147f:	0f 85 fe 00 00 00    	jne    401583 <runtime::udivmod128+0x183>
  401485:	4d 85 c0             	test   %r8,%r8
  401488:	74 0a                	je     401494 <runtime::udivmod128+0x94>
  40148a:	48 21 f7             	and    %rsi,%rdi
  40148d:	49 89 00             	mov    %rax,(%r8)
  401490:	49 89 78 08          	mov    %rdi,0x8(%r8)
  401494:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  401499:	c4 e2 fb f7 c6       	shrx   %rax,%rsi,%rax
  40149e:	31 f6                	xor    %esi,%esi
  4014a0:	e9 2a 03 00 00       	jmp    4017cf <runtime::udivmod128+0x3cf>
  4014a5:	48 8d 7a ff          	lea    -0x1(%rdx),%rdi
  4014a9:	48 85 fa             	test   %rdi,%rdx
  4014ac:	0f 85 8f 00 00 00    	jne    401541 <runtime::udivmod128+0x141>
  4014b2:	4d 85 c0             	test   %r8,%r8
  4014b5:	74 0e                	je     4014c5 <runtime::udivmod128+0xc5>
  4014b7:	48 21 c7             	and    %rax,%rdi
  4014ba:	49 89 38             	mov    %rdi,(%r8)
  4014bd:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  4014c4:	00 
  4014c5:	48 83 fa 01          	cmp    $0x1,%rdx
  4014c9:	0f 84 00 03 00 00    	je     4017cf <runtime::udivmod128+0x3cf>
  4014cf:	f3 48 0f bc ca       	tzcnt  %rdx,%rcx
  4014d4:	89 ca                	mov    %ecx,%edx
  4014d6:	f6 da                	neg    %dl
  4014d8:	c4 e2 e9 f7 d6       	shlx   %rdx,%rsi,%rdx
  4014dd:	c4 e2 f3 f7 f6       	shrx   %rcx,%rsi,%rsi
  4014e2:	48 85 c9             	test   %rcx,%rcx
  4014e5:	48 0f 44 d1          	cmove  %rcx,%rdx
  4014e9:	c4 e2 f3 f7 c0       	shrx   %rcx,%rax,%rax
  4014ee:	48 09 d0             	or     %rdx,%rax
  4014f1:	e9 d9 02 00 00       	jmp    4017cf <runtime::udivmod128+0x3cf>
  4014f6:	48 89 d1             	mov    %rdx,%rcx
  4014f9:	31 d2                	xor    %edx,%edx
  4014fb:	48 f7 f1             	div    %rcx
  4014fe:	4d 85 c0             	test   %r8,%r8
  401501:	74 79                	je     40157c <runtime::udivmod128+0x17c>
  401503:	49 89 10             	mov    %rdx,(%r8)
  401506:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  40150d:	00 
  40150e:	eb 6c                	jmp    40157c <runtime::udivmod128+0x17c>
  401510:	44 8d 4f 01          	lea    0x1(%rdi),%r9d
  401514:	41 83 f9 40          	cmp    $0x40,%r9d
  401518:	74 3e                	je     401558 <runtime::udivmod128+0x158>
  40151a:	c4 e2 b3 f7 de       	shrx   %r9,%rsi,%rbx
  40151f:	41 b2 3f             	mov    $0x3f,%r10b
  401522:	41 28 fa             	sub    %dil,%r10b
  401525:	c4 e2 a9 f7 fe       	shlx   %r10,%rsi,%rdi
  40152a:	c4 e2 b3 f7 f0       	shrx   %r9,%rax,%rsi
  40152f:	48 09 fe             	or     %rdi,%rsi
  401532:	c4 e2 a9 f7 c0       	shlx   %r10,%rax,%rax
  401537:	e9 92 00 00 00       	jmp    4015ce <runtime::udivmod128+0x1ce>
  40153c:	e9 8e 02 00 00       	jmp    4017cf <runtime::udivmod128+0x3cf>
  401541:	f3 4c 0f bd ca       	lzcnt  %rdx,%r9
  401546:	f3 48 0f bd fe       	lzcnt  %rsi,%rdi
  40154b:	41 29 f9             	sub    %edi,%r9d
  40154e:	41 83 c1 41          	add    $0x41,%r9d
  401552:	41 83 f9 40          	cmp    $0x40,%r9d
  401556:	75 4a                	jne    4015a2 <runtime::udivmod128+0x1a2>
  401558:	41 b9 40 00 00 00    	mov    $0x40,%r9d
  40155e:	31 ff                	xor    %edi,%edi
  401560:	31 db                	xor    %ebx,%ebx
  401562:	eb 6c                	jmp    4015d0 <runtime::udivmod128+0x1d0>
  401564:	48 89 f0             	mov    %rsi,%rax
  401567:	31 d2                	xor    %edx,%edx
  401569:	48 f7 f1             	div    %rcx
  40156c:	4d 85 c0             	test   %r8,%r8
  40156f:	74 0b                	je     40157c <runtime::udivmod128+0x17c>
  401571:	49 89 50 08          	mov    %rdx,0x8(%r8)
  401575:	49 c7 00 00 00 00 00 	movq   $0x0,(%r8)
  40157c:	31 f6                	xor    %esi,%esi
  40157e:	e9 4c 02 00 00       	jmp    4017cf <runtime::udivmod128+0x3cf>
  401583:	f3 48 0f bd f9       	lzcnt  %rcx,%rdi
  401588:	f3 4c 0f bd ce       	lzcnt  %rsi,%r9
  40158d:	44 29 cf             	sub    %r9d,%edi
  401590:	83 ff 3f             	cmp    $0x3f,%edi
  401593:	0f 83 9d fe ff ff    	jae    401436 <runtime::udivmod128+0x36>
  401599:	44 8d 4f 01          	lea    0x1(%rdi),%r9d
  40159d:	40 f6 d7             	not    %dil
  4015a0:	eb 0c                	jmp    4015ae <runtime::udivmod128+0x1ae>
  4015a2:	44 89 cf             	mov    %r9d,%edi
  4015a5:	0f 83 32 02 00 00    	jae    4017dd <runtime::udivmod128+0x3dd>
  4015ab:	40 f6 df             	neg    %dil
  4015ae:	c4 62 b3 f7 d0       	shrx   %r9,%rax,%r10
  4015b3:	c4 e2 c1 f7 c0       	shlx   %rdi,%rax,%rax
  4015b8:	c4 e2 b3 f7 de       	shrx   %r9,%rsi,%rbx
  4015bd:	44 89 cf             	mov    %r9d,%edi
  4015c0:	40 f6 d7             	not    %dil
  4015c3:	48 01 f6             	add    %rsi,%rsi
  4015c6:	c4 e2 c1 f7 f6       	shlx   %rdi,%rsi,%rsi
  4015cb:	4c 09 d6             	or     %r10,%rsi
  4015ce:	31 ff                	xor    %edi,%edi
  4015d0:	45 8d 51 ff          	lea    -0x1(%r9),%r10d
  4015d4:	45 89 cb             	mov    %r9d,%r11d
  4015d7:	41 83 e3 03          	and    $0x3,%r11d
  4015db:	41 83 fa 03          	cmp    $0x3,%r10d
  4015df:	73 11                	jae    4015f2 <runtime::udivmod128+0x1f2>
  4015e1:	45 31 f6             	xor    %r14d,%r14d
  4015e4:	45 85 db             	test   %r11d,%r11d
  4015e7:	0f 85 5f 01 00 00    	jne    40174c <runtime::udivmod128+0x34c>
  4015ed:	e9 bc 01 00 00       	jmp    4017ae <runtime::udivmod128+0x3ae>
  4015f2:	41 83 e1 fc          	and    $0xfffffffc,%r9d
  4015f6:	45 31 d2             	xor    %r10d,%r10d
  4015f9:	45 31 f6             	xor    %r14d,%r14d
  4015fc:	0f 1f 40 00          	nopl   0x0(%rax)
  401600:	49 89 c7             	mov    %rax,%r15
  401603:	49 c1 ef 3f          	shr    $0x3f,%r15
  401607:	4d 8d 3c 77          	lea    (%r15,%rsi,2),%r15
  40160b:	48 c1 ee 3f          	shr    $0x3f,%rsi
  40160f:	48 8d 34 5e          	lea    (%rsi,%rbx,2),%rsi
  401613:	48 89 fb             	mov    %rdi,%rbx
  401616:	48 c1 eb 3f          	shr    $0x3f,%rbx
  40161a:	48 8d 1c 43          	lea    (%rbx,%rax,2),%rbx
  40161e:	48 01 c0             	add    %rax,%rax
  401621:	48 01 ff             	add    %rdi,%rdi
  401624:	4c 09 f7             	or     %r14,%rdi
  401627:	49 89 f4             	mov    %rsi,%r12
  40162a:	49 f7 d4             	not    %r12
  40162d:	4d 89 fe             	mov    %r15,%r14
  401630:	49 f7 d6             	not    %r14
  401633:	49 01 d6             	add    %rdx,%r14
  401636:	49 11 cc             	adc    %rcx,%r12
  401639:	4d 89 e6             	mov    %r12,%r14
  40163c:	49 c1 fe 3f          	sar    $0x3f,%r14
  401640:	4d 89 f5             	mov    %r14,%r13
  401643:	49 21 cd             	and    %rcx,%r13
  401646:	49 21 d6             	and    %rdx,%r14
  401649:	4d 29 f7             	sub    %r14,%r15
  40164c:	4c 19 ee             	sbb    %r13,%rsi
  40164f:	48 c1 e8 3f          	shr    $0x3f,%rax
  401653:	4e 8d 34 78          	lea    (%rax,%r15,2),%r14
  401657:	49 c1 ef 3f          	shr    $0x3f,%r15
  40165b:	4d 8d 3c 77          	lea    (%r15,%rsi,2),%r15
  40165f:	48 89 f8             	mov    %rdi,%rax
  401662:	48 c1 e8 3f          	shr    $0x3f,%rax
  401666:	48 8d 34 58          	lea    (%rax,%rbx,2),%rsi
  40166a:	49 c1 ec 3f          	shr    $0x3f,%r12
  40166e:	49 8d 04 7c          	lea    (%r12,%rdi,2),%rax
  401672:	48 01 db             	add    %rbx,%rbx
  401675:	48 01 ff             	add    %rdi,%rdi
  401678:	4d 89 fc             	mov    %r15,%r12
  40167b:	49 f7 d4             	not    %r12
  40167e:	4d 89 f5             	mov    %r14,%r13
  401681:	49 f7 d5             	not    %r13
  401684:	49 01 d5             	add    %rdx,%r13
  401687:	49 11 cc             	adc    %rcx,%r12
  40168a:	4d 89 e5             	mov    %r12,%r13
  40168d:	49 c1 fd 3f          	sar    $0x3f,%r13
  401691:	4c 89 ed             	mov    %r13,%rbp
  401694:	48 21 cd             	and    %rcx,%rbp
  401697:	49 21 d5             	and    %rdx,%r13
  40169a:	4d 29 ee             	sub    %r13,%r14
  40169d:	49 19 ef             	sbb    %rbp,%r15
  4016a0:	48 c1 eb 3f          	shr    $0x3f,%rbx
  4016a4:	4a 8d 1c 73          	lea    (%rbx,%r14,2),%rbx
  4016a8:	49 c1 ee 3f          	shr    $0x3f,%r14
  4016ac:	4f 8d 34 7e          	lea    (%r14,%r15,2),%r14
  4016b0:	48 c1 ef 3f          	shr    $0x3f,%rdi
  4016b4:	48 8d 3c 77          	lea    (%rdi,%rsi,2),%rdi
  4016b8:	48 01 f6             	add    %rsi,%rsi
  4016bb:	49 c1 ec 3f          	shr    $0x3f,%r12
  4016bf:	4d 8d 3c 44          	lea    (%r12,%rax,2),%r15
  4016c3:	48 01 c0             	add    %rax,%rax
  4016c6:	4d 89 f4             	mov    %r14,%r12
  4016c9:	49 f7 d4             	not    %r12
  4016cc:	49 89 dd             	mov    %rbx,%r13
  4016cf:	49 f7 d5             	not    %r13
  4016d2:	49 01 d5             	add    %rdx,%r13
  4016d5:	49 11 cc             	adc    %rcx,%r12
  4016d8:	4d 89 e5             	mov    %r12,%r13
  4016db:	49 c1 fd 3f          	sar    $0x3f,%r13
  4016df:	4c 89 ed             	mov    %r13,%rbp
  4016e2:	48 21 cd             	and    %rcx,%rbp
  4016e5:	49 21 d5             	and    %rdx,%r13
  4016e8:	4c 29 eb             	sub    %r13,%rbx
  4016eb:	49 19 ee             	sbb    %rbp,%r14
  4016ee:	48 c1 ee 3f          	shr    $0x3f,%rsi
  4016f2:	48 8d 34 5e          	lea    (%rsi,%rbx,2),%rsi
  4016f6:	48 c1 eb 3f          	shr    $0x3f,%rbx
  4016fa:	4a 8d 1c 73          	lea    (%rbx,%r14,2),%rbx
  4016fe:	48 c1 e8 3f          	shr    $0x3f,%rax
  401702:	48 8d 04 78          	lea    (%rax,%rdi,2),%rax
  401706:	49 c1 ec 3f          	shr    $0x3f,%r12
  40170a:	4b 8d 3c 7c          	lea    (%r12,%r15,2),%rdi
  40170e:	49 89 df             	mov    %rbx,%r15
  401711:	49 f7 d7             	not    %r15
  401714:	49 89 f6             	mov    %rsi,%r14
  401717:	49 f7 d6             	not    %r14
  40171a:	49 01 d6             	add    %rdx,%r14
  40171d:	49 11 cf             	adc    %rcx,%r15
  401720:	4d 89 fe             	mov    %r15,%r14
  401723:	49 c1 ee 3f          	shr    $0x3f,%r14
  401727:	49 c1 ff 3f          	sar    $0x3f,%r15
  40172b:	4d 89 fc             	mov    %r15,%r12
  40172e:	49 21 cc             	and    %rcx,%r12
  401731:	49 21 d7             	and    %rdx,%r15
  401734:	4c 29 fe             	sub    %r15,%rsi
  401737:	4c 19 e3             	sbb    %r12,%rbx
  40173a:	41 83 c1 fc          	add    $0xfffffffc,%r9d
  40173e:	0f 85 bc fe ff ff    	jne    401600 <runtime::udivmod128+0x200>
  401744:	49 89 f1             	mov    %rsi,%r9
  401747:	45 85 db             	test   %r11d,%r11d
  40174a:	74 62                	je     4017ae <runtime::udivmod128+0x3ae>
  40174c:	45 31 d2             	xor    %r10d,%r10d
  40174f:	90                   	nop
  401750:	49 89 f1             	mov    %rsi,%r9
  401753:	49 c1 e9 3f          	shr    $0x3f,%r9
  401757:	49 8d 1c 59          	lea    (%r9,%rbx,2),%rbx
  40175b:	49 89 c1             	mov    %rax,%r9
  40175e:	49 c1 e9 3f          	shr    $0x3f,%r9
  401762:	49 8d 34 71          	lea    (%r9,%rsi,2),%rsi
  401766:	49 89 f9             	mov    %rdi,%r9
  401769:	48 c1 ef 3f          	shr    $0x3f,%rdi
  40176d:	48 8d 04 47          	lea    (%rdi,%rax,2),%rax
  401771:	4d 01 c9             	add    %r9,%r9
  401774:	44 89 f7             	mov    %r14d,%edi
  401777:	4c 09 cf             	or     %r9,%rdi
  40177a:	49 89 d9             	mov    %rbx,%r9
  40177d:	49 f7 d1             	not    %r9
  401780:	49 89 f6             	mov    %rsi,%r14
  401783:	49 f7 d6             	not    %r14
  401786:	49 01 d6             	add    %rdx,%r14
  401789:	49 11 c9             	adc    %rcx,%r9
  40178c:	4d 89 ce             	mov    %r9,%r14
  40178f:	49 c1 ee 3f          	shr    $0x3f,%r14
  401793:	49 c1 f9 3f          	sar    $0x3f,%r9
  401797:	4d 89 cf             	mov    %r9,%r15
  40179a:	49 21 cf             	and    %rcx,%r15
  40179d:	49 21 d1             	and    %rdx,%r9
  4017a0:	4c 29 ce             	sub    %r9,%rsi
  4017a3:	4c 19 fb             	sbb    %r15,%rbx
  4017a6:	41 ff cb             	dec    %r11d
  4017a9:	75 a5                	jne    401750 <runtime::udivmod128+0x350>
  4017ab:	49 89 f1             	mov    %rsi,%r9
  4017ae:	48 89 c1             	mov    %rax,%rcx
  4017b1:	48 8d 04 3f          	lea    (%rdi,%rdi,1),%rax
  4017b5:	4c 09 f0             	or     %r14,%rax
  4017b8:	48 c1 ef 3f          	shr    $0x3f,%rdi
  4017bc:	48 8d 34 4f          	lea    (%rdi,%rcx,2),%rsi
  4017c0:	4c 09 d6             	or     %r10,%rsi
  4017c3:	4d 85 c0             	test   %r8,%r8
  4017c6:	74 07                	je     4017cf <runtime::udivmod128+0x3cf>
  4017c8:	4d 89 08             	mov    %r9,(%r8)
  4017cb:	49 89 58 08          	mov    %rbx,0x8(%r8)
  4017cf:	48 89 f2             	mov    %rsi,%rdx
  4017d2:	5b                   	pop    %rbx
  4017d3:	41 5c                	pop    %r12
  4017d5:	41 5d                	pop    %r13
  4017d7:	41 5e                	pop    %r14
  4017d9:	41 5f                	pop    %r15
  4017db:	5d                   	pop    %rbp
  4017dc:	c3                   	ret
  4017dd:	40 f6 df             	neg    %dil
  4017e0:	c4 e2 c1 f7 f8       	shlx   %rdi,%rax,%rdi
  4017e5:	45 8d 51 c0          	lea    -0x40(%r9),%r10d
  4017e9:	c4 62 ab f7 d8       	shrx   %r10,%rax,%r11
  4017ee:	41 f6 d2             	not    %r10b
  4017f1:	48 8d 04 36          	lea    (%rsi,%rsi,1),%rax
  4017f5:	c4 e2 a9 f7 c0       	shlx   %r10,%rax,%rax
  4017fa:	4c 09 d8             	or     %r11,%rax
  4017fd:	c4 e2 b3 f7 f6       	shrx   %r9,%rsi,%rsi
  401802:	31 db                	xor    %ebx,%ebx
  401804:	e9 c7 fd ff ff       	jmp    4015d0 <runtime::udivmod128+0x1d0>
  401809:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401810 <runtime::heap_allocator_proc>:
  401810:	55                   	push   %rbp
  401811:	41 57                	push   %r15
  401813:	41 56                	push   %r14
  401815:	41 55                	push   %r13
  401817:	41 54                	push   %r12
  401819:	53                   	push   %rbx
  40181a:	50                   	push   %rax
  40181b:	4c 8b 7c 24 48       	mov    0x48(%rsp),%r15
  401820:	40 80 fe 07          	cmp    $0x7,%sil
  401824:	0f 87 e3 00 00 00    	ja     40190d <runtime::heap_allocator_proc+0xfd>
  40182a:	40 0f b6 c6          	movzbl %sil,%eax
  40182e:	ff 24 c5 20 30 40 00 	jmp    *0x403020(,%rax,8)
  401835:	48 83 f9 09          	cmp    $0x9,%rcx
  401839:	bb 08 00 00 00       	mov    $0x8,%ebx
  40183e:	48 0f 4d d9          	cmovge %rcx,%rbx
  401842:	48 8d 44 1a 07       	lea    0x7(%rdx,%rbx,1),%rax
  401847:	40 b5 01             	mov    $0x1,%bpl
  40184a:	48 85 c0             	test   %rax,%rax
  40184d:	0f 8e 53 01 00 00    	jle    4019a6 <runtime::heap_allocator_proc+0x196>
  401853:	49 89 d6             	mov    %rdx,%r14
  401856:	40 84 f6             	test   %sil,%sil
  401859:	0f 84 31 01 00 00    	je     401990 <runtime::heap_allocator_proc+0x180>
  40185f:	48 89 c7             	mov    %rax,%rdi
  401862:	e8 09 f8 ff ff       	call   401070 <malloc@plt>
  401867:	48 85 c0             	test   %rax,%rax
  40186a:	0f 84 36 01 00 00    	je     4019a6 <runtime::heap_allocator_proc+0x196>
  401870:	48 8d 4c 18 07       	lea    0x7(%rax,%rbx,1),%rcx
  401875:	48 f7 db             	neg    %rbx
  401878:	48 21 cb             	and    %rcx,%rbx
  40187b:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  40187f:	4c 89 f0             	mov    %r14,%rax
  401882:	48 c1 f8 3f          	sar    $0x3f,%rax
  401886:	c4 c2 f8 f2 c6       	andn   %r14,%rax,%rax
  40188b:	31 ed                	xor    %ebp,%ebp
  40188d:	e9 18 01 00 00       	jmp    4019aa <runtime::heap_allocator_proc+0x19a>
  401892:	4d 85 c0             	test   %r8,%r8
  401895:	0f 84 82 00 00 00    	je     40191d <runtime::heap_allocator_proc+0x10d>
  40189b:	bb 08 00 00 00       	mov    $0x8,%ebx
  4018a0:	48 83 f9 09          	cmp    $0x9,%rcx
  4018a4:	48 0f 4d d9          	cmovge %rcx,%rbx
  4018a8:	48 8d 44 1a 07       	lea    0x7(%rdx,%rbx,1),%rax
  4018ad:	0f 8c a0 00 00 00    	jl     401953 <runtime::heap_allocator_proc+0x143>
  4018b3:	48 85 c0             	test   %rax,%rax
  4018b6:	0f 8e b7 01 00 00    	jle    401a73 <runtime::heap_allocator_proc+0x263>
  4018bc:	4d 89 c5             	mov    %r8,%r13
  4018bf:	4d 89 ce             	mov    %r9,%r14
  4018c2:	49 89 d4             	mov    %rdx,%r12
  4018c5:	89 f5                	mov    %esi,%ebp
  4018c7:	40 80 fe 03          	cmp    $0x3,%sil
  4018cb:	0f 85 16 01 00 00    	jne    4019e7 <runtime::heap_allocator_proc+0x1d7>
  4018d1:	bf 01 00 00 00       	mov    $0x1,%edi
  4018d6:	48 89 c6             	mov    %rax,%rsi
  4018d9:	e8 72 f7 ff ff       	call   401050 <calloc@plt>
  4018de:	e9 0c 01 00 00       	jmp    4019ef <runtime::heap_allocator_proc+0x1df>
  4018e3:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4018e7:	c4 c1 78 11 07       	vmovups %xmm0,(%r15)
  4018ec:	40 b5 04             	mov    $0x4,%bpl
  4018ef:	e9 9b 01 00 00       	jmp    401a8f <runtime::heap_allocator_proc+0x27f>
  4018f4:	4d 85 c0             	test   %r8,%r8
  4018f7:	74 14                	je     40190d <runtime::heap_allocator_proc+0xfd>
  4018f9:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  4018fd:	e8 2e f7 ff ff       	call   401030 <free@plt>
  401902:	eb 09                	jmp    40190d <runtime::heap_allocator_proc+0xfd>
  401904:	4d 85 c0             	test   %r8,%r8
  401907:	74 04                	je     40190d <runtime::heap_allocator_proc+0xfd>
  401909:	41 c6 00 db          	movb   $0xdb,(%r8)
  40190d:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401911:	c4 c1 78 11 07       	vmovups %xmm0,(%r15)
  401916:	31 ed                	xor    %ebp,%ebp
  401918:	e9 72 01 00 00       	jmp    401a8f <runtime::heap_allocator_proc+0x27f>
  40191d:	48 83 f9 09          	cmp    $0x9,%rcx
  401921:	bb 08 00 00 00       	mov    $0x8,%ebx
  401926:	48 0f 4d d9          	cmovge %rcx,%rbx
  40192a:	48 8d 44 1a 07       	lea    0x7(%rdx,%rbx,1),%rax
  40192f:	40 b5 01             	mov    $0x1,%bpl
  401932:	48 85 c0             	test   %rax,%rax
  401935:	0f 8e 44 01 00 00    	jle    401a7f <runtime::heap_allocator_proc+0x26f>
  40193b:	49 89 d6             	mov    %rdx,%r14
  40193e:	40 80 fe 03          	cmp    $0x3,%sil
  401942:	75 72                	jne    4019b6 <runtime::heap_allocator_proc+0x1a6>
  401944:	bf 01 00 00 00       	mov    $0x1,%edi
  401949:	48 89 c6             	mov    %rax,%rsi
  40194c:	e8 ff f6 ff ff       	call   401050 <calloc@plt>
  401951:	eb 6b                	jmp    4019be <runtime::heap_allocator_proc+0x1ae>
  401953:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401957:	89 f5                	mov    %esi,%ebp
  401959:	48 89 c6             	mov    %rax,%rsi
  40195c:	49 89 d6             	mov    %rdx,%r14
  40195f:	4d 89 cd             	mov    %r9,%r13
  401962:	4d 89 c4             	mov    %r8,%r12
  401965:	e8 16 f7 ff ff       	call   401080 <realloc@plt>
  40196a:	4d 89 e0             	mov    %r12,%r8
  40196d:	48 85 c0             	test   %rax,%rax
  401970:	0f 84 fd 00 00 00    	je     401a73 <runtime::heap_allocator_proc+0x263>
  401976:	4c 89 ef             	mov    %r13,%rdi
  401979:	4c 89 f2             	mov    %r14,%rdx
  40197c:	48 8d 4c 18 07       	lea    0x7(%rax,%rbx,1),%rcx
  401981:	48 f7 db             	neg    %rbx
  401984:	48 21 cb             	and    %rcx,%rbx
  401987:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  40198b:	e9 b0 00 00 00       	jmp    401a40 <runtime::heap_allocator_proc+0x230>
  401990:	bf 01 00 00 00       	mov    $0x1,%edi
  401995:	48 89 c6             	mov    %rax,%rsi
  401998:	e8 b3 f6 ff ff       	call   401050 <calloc@plt>
  40199d:	48 85 c0             	test   %rax,%rax
  4019a0:	0f 85 ca fe ff ff    	jne    401870 <runtime::heap_allocator_proc+0x60>
  4019a6:	31 db                	xor    %ebx,%ebx
  4019a8:	31 c0                	xor    %eax,%eax
  4019aa:	49 89 1f             	mov    %rbx,(%r15)
  4019ad:	49 89 47 08          	mov    %rax,0x8(%r15)
  4019b1:	e9 d9 00 00 00       	jmp    401a8f <runtime::heap_allocator_proc+0x27f>
  4019b6:	48 89 c7             	mov    %rax,%rdi
  4019b9:	e8 b2 f6 ff ff       	call   401070 <malloc@plt>
  4019be:	48 85 c0             	test   %rax,%rax
  4019c1:	0f 84 b8 00 00 00    	je     401a7f <runtime::heap_allocator_proc+0x26f>
  4019c7:	48 8d 4c 18 07       	lea    0x7(%rax,%rbx,1),%rcx
  4019cc:	48 f7 db             	neg    %rbx
  4019cf:	48 21 cb             	and    %rcx,%rbx
  4019d2:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  4019d6:	4c 89 f0             	mov    %r14,%rax
  4019d9:	48 c1 f8 3f          	sar    $0x3f,%rax
  4019dd:	c4 42 f8 f2 f6       	andn   %r14,%rax,%r14
  4019e2:	e9 9f 00 00 00       	jmp    401a86 <runtime::heap_allocator_proc+0x276>
  4019e7:	48 89 c7             	mov    %rax,%rdi
  4019ea:	e8 81 f6 ff ff       	call   401070 <malloc@plt>
  4019ef:	48 85 c0             	test   %rax,%rax
  4019f2:	4d 89 e8             	mov    %r13,%r8
  4019f5:	74 7c                	je     401a73 <runtime::heap_allocator_proc+0x263>
  4019f7:	4c 89 e2             	mov    %r12,%rdx
  4019fa:	48 8d 74 18 07       	lea    0x7(%rax,%rbx,1),%rsi
  4019ff:	48 f7 db             	neg    %rbx
  401a02:	48 21 f3             	and    %rsi,%rbx
  401a05:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401a09:	4d 39 e6             	cmp    %r12,%r14
  401a0c:	49 0f 4c d6          	cmovl  %r14,%rdx
  401a10:	49 39 d8             	cmp    %rbx,%r8
  401a13:	0f 95 c0             	setne  %al
  401a16:	48 85 d2             	test   %rdx,%rdx
  401a19:	0f 9f c1             	setg   %cl
  401a1c:	20 c1                	and    %al,%cl
  401a1e:	80 f9 01             	cmp    $0x1,%cl
  401a21:	75 0e                	jne    401a31 <runtime::heap_allocator_proc+0x221>
  401a23:	48 89 df             	mov    %rbx,%rdi
  401a26:	4c 89 c6             	mov    %r8,%rsi
  401a29:	e8 32 f6 ff ff       	call   401060 <memcpy@plt>
  401a2e:	4d 89 e8             	mov    %r13,%r8
  401a31:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401a35:	e8 f6 f5 ff ff       	call   401030 <free@plt>
  401a3a:	4c 89 e2             	mov    %r12,%rdx
  401a3d:	4c 89 f7             	mov    %r14,%rdi
  401a40:	40 80 fd 03          	cmp    $0x3,%bpl
  401a44:	0f 94 c0             	sete   %al
  401a47:	48 89 d1             	mov    %rdx,%rcx
  401a4a:	48 c1 f9 3f          	sar    $0x3f,%rcx
  401a4e:	c4 62 f0 f2 f2       	andn   %rdx,%rcx,%r14
  401a53:	48 29 fa             	sub    %rdi,%rdx
  401a56:	0f 9f c1             	setg   %cl
  401a59:	20 c1                	and    %al,%cl
  401a5b:	80 f9 01             	cmp    $0x1,%cl
  401a5e:	75 26                	jne    401a86 <runtime::heap_allocator_proc+0x276>
  401a60:	48 85 ff             	test   %rdi,%rdi
  401a63:	78 3b                	js     401aa0 <runtime::heap_allocator_proc+0x290>
  401a65:	48 01 df             	add    %rbx,%rdi
  401a68:	31 ed                	xor    %ebp,%ebp
  401a6a:	31 f6                	xor    %esi,%esi
  401a6c:	e8 cf f5 ff ff       	call   401040 <memset@plt>
  401a71:	eb 15                	jmp    401a88 <runtime::heap_allocator_proc+0x278>
  401a73:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401a77:	e8 b4 f5 ff ff       	call   401030 <free@plt>
  401a7c:	40 b5 01             	mov    $0x1,%bpl
  401a7f:	31 db                	xor    %ebx,%ebx
  401a81:	45 31 f6             	xor    %r14d,%r14d
  401a84:	eb 02                	jmp    401a88 <runtime::heap_allocator_proc+0x278>
  401a86:	31 ed                	xor    %ebp,%ebp
  401a88:	49 89 1f             	mov    %rbx,(%r15)
  401a8b:	4d 89 77 08          	mov    %r14,0x8(%r15)
  401a8f:	89 e8                	mov    %ebp,%eax
  401a91:	48 83 c4 08          	add    $0x8,%rsp
  401a95:	5b                   	pop    %rbx
  401a96:	41 5c                	pop    %r12
  401a98:	41 5d                	pop    %r13
  401a9a:	41 5e                	pop    %r14
  401a9c:	41 5f                	pop    %r15
  401a9e:	5d                   	pop    %rbp
  401a9f:	c3                   	ret
  401aa0:	4c 89 f6             	mov    %r14,%rsi
  401aa3:	4c 89 f2             	mov    %r14,%rdx
  401aa6:	e8 f5 f5 ff ff       	call   4010a0 <runtime::slice_handle_error>
  401aab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401ab0 <runtime::default_random_generator_proc>:
  401ab0:	41 56                	push   %r14
  401ab2:	53                   	push   %rbx
  401ab3:	50                   	push   %rax
  401ab4:	64 48 8b 1c 25 00 00 	mov    %fs:0x0,%rbx
  401abb:	00 00 
  401abd:	48 8d 9b e8 ff ff ff 	lea    -0x18(%rbx),%rbx
  401ac4:	48 85 ff             	test   %rdi,%rdi
  401ac7:	48 0f 45 df          	cmovne %rdi,%rbx
  401acb:	48 83 fe 02          	cmp    $0x2,%rsi
  401acf:	0f 84 e8 00 00 00    	je     401bbd <runtime::default_random_generator_proc+0x10d>
  401ad5:	48 83 fe 01          	cmp    $0x1,%rsi
  401ad9:	74 6e                	je     401b49 <runtime::default_random_generator_proc+0x99>
  401adb:	48 85 f6             	test   %rsi,%rsi
  401ade:	0f 85 b3 01 00 00    	jne    401c97 <runtime::default_random_generator_proc+0x1e7>
  401ae4:	48 8b 03             	mov    (%rbx),%rax
  401ae7:	48 85 c0             	test   %rax,%rax
  401aea:	75 0d                	jne    401af9 <runtime::default_random_generator_proc+0x49>
  401aec:	48 83 7b 08 00       	cmpq   $0x0,0x8(%rbx)
  401af1:	0f 84 d8 00 00 00    	je     401bcf <runtime::default_random_generator_proc+0x11f>
  401af7:	31 c0                	xor    %eax,%eax
  401af9:	48 83 f9 08          	cmp    $0x8,%rcx
  401afd:	0f 85 0c 01 00 00    	jne    401c0f <runtime::default_random_generator_proc+0x15f>
  401b03:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  401b0a:	f4 51 58 
  401b0d:	48 0f af c8          	imul   %rax,%rcx
  401b11:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  401b15:	48 83 ce 01          	or     $0x1,%rsi
  401b19:	48 01 ce             	add    %rcx,%rsi
  401b1c:	48 89 33             	mov    %rsi,(%rbx)
  401b1f:	48 89 c1             	mov    %rax,%rcx
  401b22:	48 c1 e9 3b          	shr    $0x3b,%rcx
  401b26:	48 89 ce             	mov    %rcx,%rsi
  401b29:	48 83 c6 05          	add    $0x5,%rsi
  401b2d:	48 31 c6             	xor    %rax,%rsi
  401b30:	48 b8 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rax
  401b37:	75 f1 ae 
  401b3a:	48 0f af c6          	imul   %rsi,%rax
  401b3e:	48 d3 c8             	ror    %cl,%rax
  401b41:	48 89 02             	mov    %rax,(%rdx)
  401b44:	e9 4e 01 00 00       	jmp    401c97 <runtime::default_random_generator_proc+0x1e7>
  401b49:	48 c7 04 24 00 00 00 	movq   $0x0,(%rsp)
  401b50:	00 
  401b51:	48 85 d2             	test   %rdx,%rdx
  401b54:	0f 95 c0             	setne  %al
  401b57:	48 85 c9             	test   %rcx,%rcx
  401b5a:	40 0f 9f c6          	setg   %sil
  401b5e:	40 20 c6             	and    %al,%sil
  401b61:	40 80 fe 01          	cmp    $0x1,%sil
  401b65:	75 24                	jne    401b8b <runtime::default_random_generator_proc+0xdb>
  401b67:	48 83 f9 08          	cmp    $0x8,%rcx
  401b6b:	b8 08 00 00 00       	mov    $0x8,%eax
  401b70:	48 0f 42 c1          	cmovb  %rcx,%rax
  401b74:	48 89 e7             	mov    %rsp,%rdi
  401b77:	48 89 d6             	mov    %rdx,%rsi
  401b7a:	48 89 c2             	mov    %rax,%rdx
  401b7d:	e8 de f4 ff ff       	call   401060 <memcpy@plt>
  401b82:	48 8b 14 24          	mov    (%rsp),%rdx
  401b86:	48 85 d2             	test   %rdx,%rdx
  401b89:	75 09                	jne    401b94 <runtime::default_random_generator_proc+0xe4>
  401b8b:	0f 31                	rdtsc
  401b8d:	48 c1 e2 20          	shl    $0x20,%rdx
  401b91:	48 09 c2             	or     %rax,%rdx
  401b94:	48 8d 44 12 01       	lea    0x1(%rdx,%rdx,1),%rax
  401b99:	48 8d 4c 52 01       	lea    0x1(%rdx,%rdx,2),%rcx
  401b9e:	48 89 43 08          	mov    %rax,0x8(%rbx)
  401ba2:	48 b8 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rax
  401ba9:	f4 51 58 
  401bac:	48 0f af c1          	imul   %rcx,%rax
  401bb0:	48 8d 44 50 01       	lea    0x1(%rax,%rdx,2),%rax
  401bb5:	48 89 03             	mov    %rax,(%rbx)
  401bb8:	e9 da 00 00 00       	jmp    401c97 <runtime::default_random_generator_proc+0x1e7>
  401bbd:	48 83 f9 04          	cmp    $0x4,%rcx
  401bc1:	0f 85 d0 00 00 00    	jne    401c97 <runtime::default_random_generator_proc+0x1e7>
  401bc7:	80 0a 0a             	orb    $0xa,(%rdx)
  401bca:	e9 c8 00 00 00       	jmp    401c97 <runtime::default_random_generator_proc+0x1e7>
  401bcf:	48 89 d7             	mov    %rdx,%rdi
  401bd2:	0f 31                	rdtsc
  401bd4:	48 89 d6             	mov    %rdx,%rsi
  401bd7:	48 89 fa             	mov    %rdi,%rdx
  401bda:	48 c1 e6 20          	shl    $0x20,%rsi
  401bde:	48 09 c6             	or     %rax,%rsi
  401be1:	48 8d 44 36 01       	lea    0x1(%rsi,%rsi,1),%rax
  401be6:	48 8d 7c 76 01       	lea    0x1(%rsi,%rsi,2),%rdi
  401beb:	48 89 43 08          	mov    %rax,0x8(%rbx)
  401bef:	48 b8 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rax
  401bf6:	f4 51 58 
  401bf9:	48 0f af c7          	imul   %rdi,%rax
  401bfd:	48 8d 44 70 01       	lea    0x1(%rax,%rsi,2),%rax
  401c02:	48 89 03             	mov    %rax,(%rbx)
  401c05:	48 83 f9 08          	cmp    $0x8,%rcx
  401c09:	0f 84 f4 fe ff ff    	je     401b03 <runtime::default_random_generator_proc+0x53>
  401c0f:	48 85 c9             	test   %rcx,%rcx
  401c12:	0f 8e 7f 00 00 00    	jle    401c97 <runtime::default_random_generator_proc+0x1e7>
  401c18:	48 b8 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rax
  401c1f:	75 f1 ae 
  401c22:	48 be 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rsi
  401c29:	f4 51 58 
  401c2c:	89 cf                	mov    %ecx,%edi
  401c2e:	83 e7 03             	and    $0x3,%edi
  401c31:	48 83 f9 04          	cmp    $0x4,%rcx
  401c35:	73 68                	jae    401c9f <runtime::default_random_generator_proc+0x1ef>
  401c37:	45 31 c0             	xor    %r8d,%r8d
  401c3a:	31 c9                	xor    %ecx,%ecx
  401c3c:	45 31 d2             	xor    %r10d,%r10d
  401c3f:	48 85 ff             	test   %rdi,%rdi
  401c42:	74 53                	je     401c97 <runtime::default_random_generator_proc+0x1e7>
  401c44:	49 01 d0             	add    %rdx,%r8
  401c47:	31 d2                	xor    %edx,%edx
  401c49:	eb 17                	jmp    401c62 <runtime::default_random_generator_proc+0x1b2>
  401c4b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  401c50:	45 88 14 10          	mov    %r10b,(%r8,%rdx,1)
  401c54:	49 c1 ea 08          	shr    $0x8,%r10
  401c58:	fe c9                	dec    %cl
  401c5a:	48 ff c2             	inc    %rdx
  401c5d:	48 39 d7             	cmp    %rdx,%rdi
  401c60:	74 35                	je     401c97 <runtime::default_random_generator_proc+0x1e7>
  401c62:	84 c9                	test   %cl,%cl
  401c64:	75 ea                	jne    401c50 <runtime::default_random_generator_proc+0x1a0>
  401c66:	4c 8b 0b             	mov    (%rbx),%r9
  401c69:	4c 8b 5b 08          	mov    0x8(%rbx),%r11
  401c6d:	4c 89 c9             	mov    %r9,%rcx
  401c70:	48 c1 e9 3b          	shr    $0x3b,%rcx
  401c74:	49 89 ca             	mov    %rcx,%r10
  401c77:	49 83 c2 05          	add    $0x5,%r10
  401c7b:	4d 31 ca             	xor    %r9,%r10
  401c7e:	4c 0f af ce          	imul   %rsi,%r9
  401c82:	49 83 cb 01          	or     $0x1,%r11
  401c86:	4d 01 cb             	add    %r9,%r11
  401c89:	4c 89 1b             	mov    %r11,(%rbx)
  401c8c:	4c 0f af d0          	imul   %rax,%r10
  401c90:	49 d3 ca             	ror    %cl,%r10
  401c93:	b1 07                	mov    $0x7,%cl
  401c95:	eb b9                	jmp    401c50 <runtime::default_random_generator_proc+0x1a0>
  401c97:	48 83 c4 08          	add    $0x8,%rsp
  401c9b:	5b                   	pop    %rbx
  401c9c:	41 5e                	pop    %r14
  401c9e:	c3                   	ret
  401c9f:	49 b9 fc ff ff ff ff 	movabs $0x7ffffffffffffffc,%r9
  401ca6:	ff ff 7f 
  401ca9:	49 21 c9             	and    %rcx,%r9
  401cac:	45 31 c0             	xor    %r8d,%r8d
  401caf:	31 c9                	xor    %ecx,%ecx
  401cb1:	45 31 d2             	xor    %r10d,%r10d
  401cb4:	eb 26                	jmp    401cdc <runtime::default_random_generator_proc+0x22c>
  401cb6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401cbd:	00 00 00 
  401cc0:	49 c1 ea 08          	shr    $0x8,%r10
  401cc4:	46 88 54 02 03       	mov    %r10b,0x3(%rdx,%r8,1)
  401cc9:	49 c1 ea 08          	shr    $0x8,%r10
  401ccd:	fe c9                	dec    %cl
  401ccf:	49 83 c0 04          	add    $0x4,%r8
  401cd3:	4d 39 c1             	cmp    %r8,%r9
  401cd6:	0f 84 63 ff ff ff    	je     401c3f <runtime::default_random_generator_proc+0x18f>
  401cdc:	84 c9                	test   %cl,%cl
  401cde:	75 2f                	jne    401d0f <runtime::default_random_generator_proc+0x25f>
  401ce0:	4c 8b 1b             	mov    (%rbx),%r11
  401ce3:	4c 8b 73 08          	mov    0x8(%rbx),%r14
  401ce7:	4c 89 d9             	mov    %r11,%rcx
  401cea:	48 c1 e9 3b          	shr    $0x3b,%rcx
  401cee:	49 89 ca             	mov    %rcx,%r10
  401cf1:	49 83 c2 05          	add    $0x5,%r10
  401cf5:	4d 31 da             	xor    %r11,%r10
  401cf8:	4c 0f af de          	imul   %rsi,%r11
  401cfc:	49 83 ce 01          	or     $0x1,%r14
  401d00:	4d 01 de             	add    %r11,%r14
  401d03:	4c 89 33             	mov    %r14,(%rbx)
  401d06:	4c 0f af d0          	imul   %rax,%r10
  401d0a:	49 d3 ca             	ror    %cl,%r10
  401d0d:	b1 07                	mov    $0x7,%cl
  401d0f:	46 88 14 02          	mov    %r10b,(%rdx,%r8,1)
  401d13:	fe c9                	dec    %cl
  401d15:	74 09                	je     401d20 <runtime::default_random_generator_proc+0x270>
  401d17:	49 c1 ea 08          	shr    $0x8,%r10
  401d1b:	eb 32                	jmp    401d4f <runtime::default_random_generator_proc+0x29f>
  401d1d:	0f 1f 00             	nopl   (%rax)
  401d20:	4c 8b 1b             	mov    (%rbx),%r11
  401d23:	4c 8b 73 08          	mov    0x8(%rbx),%r14
  401d27:	4c 89 d9             	mov    %r11,%rcx
  401d2a:	48 c1 e9 3b          	shr    $0x3b,%rcx
  401d2e:	49 89 ca             	mov    %rcx,%r10
  401d31:	49 83 c2 05          	add    $0x5,%r10
  401d35:	4d 31 da             	xor    %r11,%r10
  401d38:	4c 0f af de          	imul   %rsi,%r11
  401d3c:	49 83 ce 01          	or     $0x1,%r14
  401d40:	4d 01 de             	add    %r11,%r14
  401d43:	4c 89 33             	mov    %r14,(%rbx)
  401d46:	4c 0f af d0          	imul   %rax,%r10
  401d4a:	49 d3 ca             	ror    %cl,%r10
  401d4d:	b1 07                	mov    $0x7,%cl
  401d4f:	46 88 54 02 01       	mov    %r10b,0x1(%rdx,%r8,1)
  401d54:	fe c9                	dec    %cl
  401d56:	74 08                	je     401d60 <runtime::default_random_generator_proc+0x2b0>
  401d58:	49 c1 ea 08          	shr    $0x8,%r10
  401d5c:	eb 31                	jmp    401d8f <runtime::default_random_generator_proc+0x2df>
  401d5e:	66 90                	xchg   %ax,%ax
  401d60:	4c 8b 1b             	mov    (%rbx),%r11
  401d63:	4c 8b 73 08          	mov    0x8(%rbx),%r14
  401d67:	4c 89 d9             	mov    %r11,%rcx
  401d6a:	48 c1 e9 3b          	shr    $0x3b,%rcx
  401d6e:	49 89 ca             	mov    %rcx,%r10
  401d71:	49 83 c2 05          	add    $0x5,%r10
  401d75:	4d 31 da             	xor    %r11,%r10
  401d78:	4c 0f af de          	imul   %rsi,%r11
  401d7c:	49 83 ce 01          	or     $0x1,%r14
  401d80:	4d 01 de             	add    %r11,%r14
  401d83:	4c 89 33             	mov    %r14,(%rbx)
  401d86:	4c 0f af d0          	imul   %rax,%r10
  401d8a:	49 d3 ca             	ror    %cl,%r10
  401d8d:	b1 07                	mov    $0x7,%cl
  401d8f:	46 88 54 02 02       	mov    %r10b,0x2(%rdx,%r8,1)
  401d94:	fe c9                	dec    %cl
  401d96:	0f 85 24 ff ff ff    	jne    401cc0 <runtime::default_random_generator_proc+0x210>
  401d9c:	4c 8b 1b             	mov    (%rbx),%r11
  401d9f:	4c 8b 73 08          	mov    0x8(%rbx),%r14
  401da3:	4c 89 d9             	mov    %r11,%rcx
  401da6:	48 c1 e9 3b          	shr    $0x3b,%rcx
  401daa:	49 89 ca             	mov    %rcx,%r10
  401dad:	49 83 c2 05          	add    $0x5,%r10
  401db1:	4d 31 da             	xor    %r11,%r10
  401db4:	4c 0f af de          	imul   %rsi,%r11
  401db8:	49 83 ce 01          	or     $0x1,%r14
  401dbc:	4d 01 de             	add    %r11,%r14
  401dbf:	4c 89 33             	mov    %r14,(%rbx)
  401dc2:	4c 0f af d0          	imul   %rax,%r10
  401dc6:	49 d3 ca             	ror    %cl,%r10
  401dc9:	b1 07                	mov    $0x7,%cl
  401dcb:	e9 f4 fe ff ff       	jmp    401cc4 <runtime::default_random_generator_proc+0x214>

0000000000401dd0 <runtime::default_temp_allocator_proc>:
  401dd0:	41 57                	push   %r15
  401dd2:	41 56                	push   %r14
  401dd4:	41 55                	push   %r13
  401dd6:	41 54                	push   %r12
  401dd8:	53                   	push   %rbx
  401dd9:	48 83 ec 10          	sub    $0x10,%rsp
  401ddd:	40 80 fe 07          	cmp    $0x7,%sil
  401de1:	0f 87 bf 01 00 00    	ja     401fa6 <runtime::default_temp_allocator_proc+0x1d6>
  401de7:	48 89 fb             	mov    %rdi,%rbx
  401dea:	4c 8b 74 24 50       	mov    0x50(%rsp),%r14
  401def:	4c 8b 7c 24 40       	mov    0x40(%rsp),%r15
  401df4:	40 0f b6 c6          	movzbl %sil,%eax
  401df8:	ff 24 c5 60 30 40 00 	jmp    *0x403060(,%rax,8)
  401dff:	b0 04                	mov    $0x4,%al
  401e01:	45 31 c0             	xor    %r8d,%r8d
  401e04:	31 d2                	xor    %edx,%edx
  401e06:	e9 a2 01 00 00       	jmp    401fad <runtime::default_temp_allocator_proc+0x1dd>
  401e0b:	4d 85 c0             	test   %r8,%r8
  401e0e:	74 31                	je     401e41 <runtime::default_temp_allocator_proc+0x71>
  401e10:	4c 39 ca             	cmp    %r9,%rdx
  401e13:	0f 85 e5 00 00 00    	jne    401efe <runtime::default_temp_allocator_proc+0x12e>
  401e19:	48 85 d2             	test   %rdx,%rdx
  401e1c:	0f 89 89 01 00 00    	jns    401fab <runtime::default_temp_allocator_proc+0x1db>
  401e22:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  401e27:	be 3e 00 00 00       	mov    $0x3e,%esi
  401e2c:	49 89 d1             	mov    %rdx,%r9
  401e2f:	ba d1 00 00 00       	mov    $0xd1,%edx
  401e34:	b9 13 00 00 00       	mov    $0x13,%ecx
  401e39:	45 31 c0             	xor    %r8d,%r8d
  401e3c:	e8 ff f2 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  401e41:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401e45:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  401e4a:	49 89 e0             	mov    %rsp,%r8
  401e4d:	48 89 df             	mov    %rbx,%rdi
  401e50:	48 89 d6             	mov    %rdx,%rsi
  401e53:	48 89 ca             	mov    %rcx,%rdx
  401e56:	4c 89 f9             	mov    %r15,%rcx
  401e59:	4d 89 f1             	mov    %r14,%r9
  401e5c:	e8 2f 03 00 00       	call   402190 <runtime::arena_alloc>
  401e61:	4c 8b 04 24          	mov    (%rsp),%r8
  401e65:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401e6a:	e9 3e 01 00 00       	jmp    401fad <runtime::default_temp_allocator_proc+0x1dd>
  401e6f:	4c 8b 43 10          	mov    0x10(%rbx),%r8
  401e73:	4d 85 c0             	test   %r8,%r8
  401e76:	0f 84 22 01 00 00    	je     401f9e <runtime::default_temp_allocator_proc+0x1ce>
  401e7c:	49 89 e4             	mov    %rsp,%r12
  401e7f:	eb 1b                	jmp    401e9c <runtime::default_temp_allocator_proc+0xcc>
  401e81:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  401e88:	0f 1f 84 00 00 00 00 
  401e8f:	00 
  401e90:	49 89 c8             	mov    %rcx,%r8
  401e93:	48 85 c9             	test   %rcx,%rcx
  401e96:	0f 84 02 01 00 00    	je     401f9e <runtime::default_temp_allocator_proc+0x1ce>
  401e9c:	49 8b 08             	mov    (%r8),%rcx
  401e9f:	48 85 c9             	test   %rcx,%rcx
  401ea2:	0f 84 db 00 00 00    	je     401f83 <runtime::default_temp_allocator_proc+0x1b3>
  401ea8:	48 89 4b 10          	mov    %rcx,0x10(%rbx)
  401eac:	49 8b 40 28          	mov    0x28(%r8),%rax
  401eb0:	48 29 43 20          	sub    %rax,0x20(%rbx)
  401eb4:	49 8b 40 08          	mov    0x8(%r8),%rax
  401eb8:	48 85 c0             	test   %rax,%rax
  401ebb:	74 d3                	je     401e90 <runtime::default_temp_allocator_proc+0xc0>
  401ebd:	49 8b 78 10          	mov    0x10(%r8),%rdi
  401ec1:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401ec5:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  401eca:	48 83 ec 08          	sub    $0x8,%rsp
  401ece:	be 01 00 00 00       	mov    $0x1,%esi
  401ed3:	31 d2                	xor    %edx,%edx
  401ed5:	31 c9                	xor    %ecx,%ecx
  401ed7:	45 31 c9             	xor    %r9d,%r9d
  401eda:	41 56                	push   %r14
  401edc:	41 54                	push   %r12
  401ede:	41 57                	push   %r15
  401ee0:	ff d0                	call   *%rax
  401ee2:	48 83 c4 20          	add    $0x20,%rsp
  401ee6:	48 8b 4b 10          	mov    0x10(%rbx),%rcx
  401eea:	eb a4                	jmp    401e90 <runtime::default_temp_allocator_proc+0xc0>
  401eec:	4d 85 c0             	test   %r8,%r8
  401eef:	0f 84 b1 00 00 00    	je     401fa6 <runtime::default_temp_allocator_proc+0x1d6>
  401ef5:	41 c6 00 5d          	movb   $0x5d,(%r8)
  401ef9:	e9 a8 00 00 00       	jmp    401fa6 <runtime::default_temp_allocator_proc+0x1d6>
  401efe:	48 85 d2             	test   %rdx,%rdx
  401f01:	0f 84 f8 fe ff ff    	je     401dff <runtime::default_temp_allocator_proc+0x2f>
  401f07:	48 8d 41 ff          	lea    -0x1(%rcx),%rax
  401f0b:	4c 85 c0             	test   %r8,%rax
  401f0e:	0f 84 b3 00 00 00    	je     401fc7 <runtime::default_temp_allocator_proc+0x1f7>
  401f14:	4d 89 cd             	mov    %r9,%r13
  401f17:	4d 89 c4             	mov    %r8,%r12
  401f1a:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  401f1e:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  401f23:	49 89 e0             	mov    %rsp,%r8
  401f26:	48 89 df             	mov    %rbx,%rdi
  401f29:	48 89 d6             	mov    %rdx,%rsi
  401f2c:	48 89 ca             	mov    %rcx,%rdx
  401f2f:	4c 89 f9             	mov    %r15,%rcx
  401f32:	4d 89 f1             	mov    %r14,%r9
  401f35:	e8 56 02 00 00       	call   402190 <runtime::arena_alloc>
  401f3a:	84 c0                	test   %al,%al
  401f3c:	0f 85 bf fe ff ff    	jne    401e01 <runtime::default_temp_allocator_proc+0x31>
  401f42:	48 8b 1c 24          	mov    (%rsp),%rbx
  401f46:	48 85 db             	test   %rbx,%rbx
  401f49:	74 5b                	je     401fa6 <runtime::default_temp_allocator_proc+0x1d6>
  401f4b:	4d 89 e9             	mov    %r13,%r9
  401f4e:	4d 85 ed             	test   %r13,%r13
  401f51:	0f 88 eb 00 00 00    	js     402042 <runtime::default_temp_allocator_proc+0x272>
  401f57:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401f5c:	4c 39 ca             	cmp    %r9,%rdx
  401f5f:	4c 0f 4c ca          	cmovl  %rdx,%r9
  401f63:	4d 85 c9             	test   %r9,%r9
  401f66:	7e 14                	jle    401f7c <runtime::default_temp_allocator_proc+0x1ac>
  401f68:	4c 89 e6             	mov    %r12,%rsi
  401f6b:	48 89 df             	mov    %rbx,%rdi
  401f6e:	49 89 d6             	mov    %rdx,%r14
  401f71:	4c 89 ca             	mov    %r9,%rdx
  401f74:	e8 17 f1 ff ff       	call   401090 <memmove@plt>
  401f79:	4c 89 f2             	mov    %r14,%rdx
  401f7c:	31 c0                	xor    %eax,%eax
  401f7e:	49 89 d8             	mov    %rbx,%r8
  401f81:	eb 2a                	jmp    401fad <runtime::default_temp_allocator_proc+0x1dd>
  401f83:	49 8b 78 18          	mov    0x18(%r8),%rdi
  401f87:	49 8b 50 20          	mov    0x20(%r8),%rdx
  401f8b:	31 f6                	xor    %esi,%esi
  401f8d:	e8 ae f0 ff ff       	call   401040 <memset@plt>
  401f92:	48 8b 43 10          	mov    0x10(%rbx),%rax
  401f96:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  401f9d:	00 
  401f9e:	48 c7 43 18 00 00 00 	movq   $0x0,0x18(%rbx)
  401fa5:	00 
  401fa6:	45 31 c0             	xor    %r8d,%r8d
  401fa9:	31 d2                	xor    %edx,%edx
  401fab:	31 c0                	xor    %eax,%eax
  401fad:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401fb2:	4c 89 01             	mov    %r8,(%rcx)
  401fb5:	48 89 51 08          	mov    %rdx,0x8(%rcx)
  401fb9:	48 83 c4 10          	add    $0x10,%rsp
  401fbd:	5b                   	pop    %rbx
  401fbe:	41 5c                	pop    %r12
  401fc0:	41 5d                	pop    %r13
  401fc2:	41 5e                	pop    %r14
  401fc4:	41 5f                	pop    %r15
  401fc6:	c3                   	ret
  401fc7:	4c 39 ca             	cmp    %r9,%rdx
  401fca:	73 24                	jae    401ff0 <runtime::default_temp_allocator_proc+0x220>
  401fcc:	48 85 d2             	test   %rdx,%rdx
  401fcf:	79 da                	jns    401fab <runtime::default_temp_allocator_proc+0x1db>
  401fd1:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  401fd6:	be 3e 00 00 00       	mov    $0x3e,%esi
  401fdb:	49 89 d1             	mov    %rdx,%r9
  401fde:	ba d9 00 00 00       	mov    $0xd9,%edx
  401fe3:	b9 14 00 00 00       	mov    $0x14,%ecx
  401fe8:	45 31 c0             	xor    %r8d,%r8d
  401feb:	e8 50 f1 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  401ff0:	48 8b 7b 10          	mov    0x10(%rbx),%rdi
  401ff4:	48 85 ff             	test   %rdi,%rdi
  401ff7:	0f 84 17 ff ff ff    	je     401f14 <runtime::default_temp_allocator_proc+0x144>
  401ffd:	48 8b 77 18          	mov    0x18(%rdi),%rsi
  402001:	4c 89 c0             	mov    %r8,%rax
  402004:	48 29 f0             	sub    %rsi,%rax
  402007:	4e 8d 14 08          	lea    (%rax,%r9,1),%r10
  40200b:	4c 39 d0             	cmp    %r10,%rax
  40200e:	0f 83 00 ff ff ff    	jae    401f14 <runtime::default_temp_allocator_proc+0x144>
  402014:	4c 3b 57 20          	cmp    0x20(%rdi),%r10
  402018:	0f 85 f6 fe ff ff    	jne    401f14 <runtime::default_temp_allocator_proc+0x144>
  40201e:	4c 8d 14 10          	lea    (%rax,%rdx,1),%r10
  402022:	4c 3b 57 28          	cmp    0x28(%rdi),%r10
  402026:	0f 87 e8 fe ff ff    	ja     401f14 <runtime::default_temp_allocator_proc+0x144>
  40202c:	4c 89 57 20          	mov    %r10,0x20(%rdi)
  402030:	4c 39 d0             	cmp    %r10,%rax
  402033:	7f 29                	jg     40205e <runtime::default_temp_allocator_proc+0x28e>
  402035:	48 01 c6             	add    %rax,%rsi
  402038:	31 c0                	xor    %eax,%eax
  40203a:	49 89 f0             	mov    %rsi,%r8
  40203d:	e9 6b ff ff ff       	jmp    401fad <runtime::default_temp_allocator_proc+0x1dd>
  402042:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  402047:	be 3e 00 00 00       	mov    $0x3e,%esi
  40204c:	ba ee 00 00 00       	mov    $0xee,%edx
  402051:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  402056:	45 31 c0             	xor    %r8d,%r8d
  402059:	e8 e2 f0 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  40205e:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  402063:	be 3e 00 00 00       	mov    $0x3e,%esi
  402068:	ba e4 00 00 00       	mov    $0xe4,%edx
  40206d:	b9 17 00 00 00       	mov    $0x17,%ecx
  402072:	49 89 c0             	mov    %rax,%r8
  402075:	4d 89 d1             	mov    %r10,%r9
  402078:	e8 c3 f0 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  40207d:	0f 1f 00             	nopl   (%rax)

0000000000402080 <main>:
  402080:	48 83 ec 78          	sub    $0x78,%rsp
  402084:	4c 63 cf             	movslq %edi,%r9
  402087:	45 85 c9             	test   %r9d,%r9d
  40208a:	0f 88 db 00 00 00    	js     40216b <main+0xeb>
  402090:	48 89 35 b9 2f 00 00 	mov    %rsi,0x2fb9(%rip)        # 405050 <runtime::args__>
  402097:	4c 89 0d ba 2f 00 00 	mov    %r9,0x2fba(%rip)        # 405058 <runtime::args__+0x8>
  40209e:	48 c7 44 24 08 10 18 	movq   $0x401810,0x8(%rsp)
  4020a5:	40 00 
  4020a7:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  4020ae:	00 00 
  4020b0:	48 c7 44 24 18 d0 1d 	movq   $0x401dd0,0x18(%rsp)
  4020b7:	40 00 
  4020b9:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  4020c0:	00 00 
  4020c2:	48 8d 80 b0 ff ff ff 	lea    -0x50(%rax),%rax
  4020c9:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4020ce:	48 c7 44 24 28 b0 11 	movq   $0x4011b0,0x28(%rsp)
  4020d5:	40 00 
  4020d7:	48 c7 44 24 30 f0 26 	movq   $0x4026f0,0x30(%rsp)
  4020de:	40 00 
  4020e0:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4020e4:	c5 f8 11 44 24 38    	vmovups %xmm0,0x38(%rsp)
  4020ea:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  4020f1:	00 00 
  4020f3:	48 c7 44 24 50 b0 1a 	movq   $0x401ab0,0x50(%rsp)
  4020fa:	40 00 
  4020fc:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402100:	c5 fc 11 44 24 58    	vmovups %ymm0,0x58(%rsp)
  402106:	c5 f8 77             	vzeroupper
  402109:	e8 12 f2 ff ff       	call   401320 <__$startup_runtime>
  40210e:	b8 09 00 00 00       	mov    $0x9,%eax
  402113:	be 00 90 01 00       	mov    $0x19000,%esi
  402118:	ba 03 00 00 00       	mov    $0x3,%edx
  40211d:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  402123:	31 ff                	xor    %edi,%edi
  402125:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  40212c:	45 31 c9             	xor    %r9d,%r9d
  40212f:	0f 05                	syscall
  402131:	b8 ba 00 00 00       	mov    $0xba,%eax
  402136:	0f 05                	syscall
  402138:	b8 09 00 00 00       	mov    $0x9,%eax
  40213d:	be 00 00 02 00       	mov    $0x20000,%esi
  402142:	31 ff                	xor    %edi,%edi
  402144:	45 31 c9             	xor    %r9d,%r9d
  402147:	0f 05                	syscall
  402149:	b8 09 00 00 00       	mov    $0x9,%eax
  40214e:	be 00 00 04 00       	mov    $0x40000,%esi
  402153:	31 ff                	xor    %edi,%edi
  402155:	45 31 c9             	xor    %r9d,%r9d
  402158:	0f 05                	syscall
  40215a:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  40215f:	e8 cc f1 ff ff       	call   401330 <__$cleanup_runtime>
  402164:	31 c0                	xor    %eax,%eax
  402166:	48 83 c4 78          	add    $0x78,%rsp
  40216a:	c3                   	ret
  40216b:	bf 38 32 40 00       	mov    $0x403238,%edi
  402170:	be 2c 00 00 00       	mov    $0x2c,%esi
  402175:	ba 36 00 00 00       	mov    $0x36,%edx
  40217a:	b9 11 00 00 00       	mov    $0x11,%ecx
  40217f:	45 31 c0             	xor    %r8d,%r8d
  402182:	e8 b9 ef ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  402187:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40218e:	00 00 

0000000000402190 <runtime::arena_alloc>:
  402190:	55                   	push   %rbp
  402191:	41 57                	push   %r15
  402193:	41 56                	push   %r14
  402195:	41 55                	push   %r13
  402197:	41 54                	push   %r12
  402199:	53                   	push   %rbx
  40219a:	48 83 ec 28          	sub    $0x28,%rsp
  40219e:	c4 e2 f8 f3 ca       	blsr   %rdx,%rax
  4021a3:	0f 85 54 02 00 00    	jne    4023fd <runtime::arena_alloc+0x26d>
  4021a9:	48 85 f6             	test   %rsi,%rsi
  4021ac:	0f 84 85 00 00 00    	je     402237 <runtime::arena_alloc+0xa7>
  4021b2:	48 8b 47 10          	mov    0x10(%rdi),%rax
  4021b6:	48 85 c0             	test   %rax,%rax
  4021b9:	74 36                	je     4021f1 <runtime::arena_alloc+0x61>
  4021bb:	4c 8b 70 20          	mov    0x20(%rax),%r14
  4021bf:	4c 8b 78 18          	mov    0x18(%rax),%r15
  4021c3:	4d 01 f7             	add    %r14,%r15
  4021c6:	4c 8d 5a ff          	lea    -0x1(%rdx),%r11
  4021ca:	4d 21 fb             	and    %r15,%r11
  4021cd:	49 89 d2             	mov    %rdx,%r10
  4021d0:	4d 29 da             	sub    %r11,%r10
  4021d3:	4d 85 db             	test   %r11,%r11
  4021d6:	4d 0f 44 d3          	cmove  %r11,%r10
  4021da:	49 89 f3             	mov    %rsi,%r11
  4021dd:	4d 01 d3             	add    %r10,%r11
  4021e0:	72 0f                	jb     4021f1 <runtime::arena_alloc+0x61>
  4021e2:	4d 01 f3             	add    %r14,%r11
  4021e5:	72 0a                	jb     4021f1 <runtime::arena_alloc+0x61>
  4021e7:	4c 3b 58 28          	cmp    0x28(%rax),%r11
  4021eb:	0f 86 da 01 00 00    	jbe    4023cb <runtime::arena_alloc+0x23b>
  4021f1:	4c 8b 57 28          	mov    0x28(%rdi),%r10
  4021f5:	4d 85 d2             	test   %r10,%r10
  4021f8:	75 0e                	jne    402208 <runtime::arena_alloc+0x78>
  4021fa:	48 c7 47 28 00 00 40 	movq   $0x400000,0x28(%rdi)
  402201:	00 
  402202:	41 ba 00 00 40 00    	mov    $0x400000,%r10d
  402208:	4c 8d 62 ff          	lea    -0x1(%rdx),%r12
  40220c:	4d 89 e3             	mov    %r12,%r11
  40220f:	49 21 f3             	and    %rsi,%r11
  402212:	48 89 d0             	mov    %rdx,%rax
  402215:	4c 29 d8             	sub    %r11,%rax
  402218:	4d 85 db             	test   %r11,%r11
  40221b:	49 0f 44 c3          	cmove  %r11,%rax
  40221f:	48 01 f0             	add    %rsi,%rax
  402222:	4c 39 d0             	cmp    %r10,%rax
  402225:	49 0f 46 c2          	cmovbe %r10,%rax
  402229:	4c 8b 2f             	mov    (%rdi),%r13
  40222c:	4d 85 ed             	test   %r13,%r13
  40222f:	74 16                	je     402247 <runtime::arena_alloc+0xb7>
  402231:	4c 8b 7f 08          	mov    0x8(%rdi),%r15
  402235:	eb 28                	jmp    40225f <runtime::arena_alloc+0xcf>
  402237:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  40223b:	c4 c1 78 11 00       	vmovups %xmm0,(%r8)
  402240:	31 c0                	xor    %eax,%eax
  402242:	e9 48 01 00 00       	jmp    40238f <runtime::arena_alloc+0x1ff>
  402247:	48 c7 07 10 18 40 00 	movq   $0x401810,(%rdi)
  40224e:	41 bd 10 18 40 00    	mov    $0x401810,%r13d
  402254:	48 c7 47 08 00 00 00 	movq   $0x0,0x8(%rdi)
  40225b:	00 
  40225c:	45 31 ff             	xor    %r15d,%r15d
  40225f:	48 83 fa 31          	cmp    $0x31,%rdx
  402263:	bd 30 00 00 00       	mov    $0x30,%ebp
  402268:	48 0f 43 ea          	cmovae %rdx,%rbp
  40226c:	48 83 fa 11          	cmp    $0x11,%rdx
  402270:	41 bb 10 00 00 00    	mov    $0x10,%r11d
  402276:	4c 0f 4d da          	cmovge %rdx,%r11
  40227a:	c4 c2 a8 f3 cb       	blsr   %r11,%r10
  40227f:	0f 85 a3 01 00 00    	jne    402428 <runtime::arena_alloc+0x298>
  402285:	48 01 e8             	add    %rbp,%rax
  402288:	74 58                	je     4022e2 <runtime::arena_alloc+0x152>
  40228a:	4d 89 ce             	mov    %r9,%r14
  40228d:	4c 89 c3             	mov    %r8,%rbx
  402290:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  402295:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40229a:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40229f:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4022a3:	c5 f8 29 04 24       	vmovaps %xmm0,(%rsp)
  4022a8:	48 83 ec 08          	sub    $0x8,%rsp
  4022ac:	4c 8d 54 24 08       	lea    0x8(%rsp),%r10
  4022b1:	4c 89 ff             	mov    %r15,%rdi
  4022b4:	31 f6                	xor    %esi,%esi
  4022b6:	48 89 c2             	mov    %rax,%rdx
  4022b9:	48 89 c8             	mov    %rcx,%rax
  4022bc:	4c 89 d9             	mov    %r11,%rcx
  4022bf:	45 31 c0             	xor    %r8d,%r8d
  4022c2:	45 31 c9             	xor    %r9d,%r9d
  4022c5:	41 56                	push   %r14
  4022c7:	41 52                	push   %r10
  4022c9:	50                   	push   %rax
  4022ca:	41 ff d5             	call   *%r13
  4022cd:	48 83 c4 20          	add    $0x20,%rsp
  4022d1:	84 c0                	test   %al,%al
  4022d3:	74 13                	je     4022e8 <runtime::arena_alloc+0x158>
  4022d5:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4022d9:	c5 f8 11 03          	vmovups %xmm0,(%rbx)
  4022dd:	e9 ad 00 00 00       	jmp    40238f <runtime::arena_alloc+0x1ff>
  4022e2:	31 c0                	xor    %eax,%eax
  4022e4:	31 c9                	xor    %ecx,%ecx
  4022e6:	eb 1e                	jmp    402306 <runtime::arena_alloc+0x176>
  4022e8:	48 8b 0c 24          	mov    (%rsp),%rcx
  4022ec:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4022f1:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4022f6:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4022fb:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402300:	49 89 d8             	mov    %rbx,%r8
  402303:	4d 89 f1             	mov    %r14,%r9
  402306:	48 01 c8             	add    %rcx,%rax
  402309:	4c 89 69 08          	mov    %r13,0x8(%rcx)
  40230d:	4c 89 79 10          	mov    %r15,0x10(%rcx)
  402311:	48 01 cd             	add    %rcx,%rbp
  402314:	48 89 69 18          	mov    %rbp,0x18(%rcx)
  402318:	48 29 e8             	sub    %rbp,%rax
  40231b:	48 89 41 28          	mov    %rax,0x28(%rcx)
  40231f:	48 83 79 20 00       	cmpq   $0x0,0x20(%rcx)
  402324:	0f 85 29 01 00 00    	jne    402453 <runtime::arena_alloc+0x2c3>
  40232a:	48 83 39 00          	cmpq   $0x0,(%rcx)
  40232e:	0f 85 4d 01 00 00    	jne    402481 <runtime::arena_alloc+0x2f1>
  402334:	48 8b 47 10          	mov    0x10(%rdi),%rax
  402338:	48 89 01             	mov    %rax,(%rcx)
  40233b:	48 89 4f 10          	mov    %rcx,0x10(%rdi)
  40233f:	48 8b 41 28          	mov    0x28(%rcx),%rax
  402343:	48 01 47 20          	add    %rax,0x20(%rdi)
  402347:	4c 8b 59 20          	mov    0x20(%rcx),%r11
  40234b:	4c 8b 51 18          	mov    0x18(%rcx),%r10
  40234f:	4d 01 da             	add    %r11,%r10
  402352:	4d 21 d4             	and    %r10,%r12
  402355:	4c 29 e2             	sub    %r12,%rdx
  402358:	4d 85 e4             	test   %r12,%r12
  40235b:	49 0f 44 d4          	cmove  %r12,%rdx
  40235f:	45 31 f6             	xor    %r14d,%r14d
  402362:	49 89 f1             	mov    %rsi,%r9
  402365:	49 01 d1             	add    %rdx,%r9
  402368:	b0 01                	mov    $0x1,%al
  40236a:	72 08                	jb     402374 <runtime::arena_alloc+0x1e4>
  40236c:	45 31 f6             	xor    %r14d,%r14d
  40236f:	4d 01 cb             	add    %r9,%r11
  402372:	73 2a                	jae    40239e <runtime::arena_alloc+0x20e>
  402374:	31 db                	xor    %ebx,%ebx
  402376:	45 31 ff             	xor    %r15d,%r15d
  402379:	48 8b 4f 10          	mov    0x10(%rdi),%rcx
  40237d:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  402381:	4c 29 f1             	sub    %r14,%rcx
  402384:	48 01 4f 18          	add    %rcx,0x18(%rdi)
  402388:	4d 89 38             	mov    %r15,(%r8)
  40238b:	49 89 58 08          	mov    %rbx,0x8(%r8)
  40238f:	48 83 c4 28          	add    $0x28,%rsp
  402393:	5b                   	pop    %rbx
  402394:	41 5c                	pop    %r12
  402396:	41 5d                	pop    %r13
  402398:	41 5e                	pop    %r14
  40239a:	41 5f                	pop    %r15
  40239c:	5d                   	pop    %rbp
  40239d:	c3                   	ret
  40239e:	45 31 f6             	xor    %r14d,%r14d
  4023a1:	bb 00 00 00 00       	mov    $0x0,%ebx
  4023a6:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4023ac:	4c 3b 59 28          	cmp    0x28(%rcx),%r11
  4023b0:	77 c7                	ja     402379 <runtime::arena_alloc+0x1e9>
  4023b2:	48 85 f6             	test   %rsi,%rsi
  4023b5:	78 27                	js     4023de <runtime::arena_alloc+0x24e>
  4023b7:	49 01 d2             	add    %rdx,%r10
  4023ba:	4c 89 59 20          	mov    %r11,0x20(%rcx)
  4023be:	45 31 f6             	xor    %r14d,%r14d
  4023c1:	31 c0                	xor    %eax,%eax
  4023c3:	48 89 f3             	mov    %rsi,%rbx
  4023c6:	4d 89 d7             	mov    %r10,%r15
  4023c9:	eb ae                	jmp    402379 <runtime::arena_alloc+0x1e9>
  4023cb:	48 85 f6             	test   %rsi,%rsi
  4023ce:	78 0e                	js     4023de <runtime::arena_alloc+0x24e>
  4023d0:	4d 01 d7             	add    %r10,%r15
  4023d3:	4c 89 58 20          	mov    %r11,0x20(%rax)
  4023d7:	31 c0                	xor    %eax,%eax
  4023d9:	48 89 f3             	mov    %rsi,%rbx
  4023dc:	eb 9b                	jmp    402379 <runtime::arena_alloc+0x1e9>
  4023de:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  4023e3:	49 89 f1             	mov    %rsi,%r9
  4023e6:	be 3e 00 00 00       	mov    $0x3e,%esi
  4023eb:	ba 55 00 00 00       	mov    $0x55,%edx
  4023f0:	b9 31 00 00 00       	mov    $0x31,%ecx
  4023f5:	45 31 c0             	xor    %r8d,%r8d
  4023f8:	e8 43 ed ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  4023fd:	49 8b 41 20          	mov    0x20(%r9),%rax
  402401:	48 85 c0             	test   %rax,%rax
  402404:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  40240a:	4c 0f 45 d0          	cmovne %rax,%r10
  40240e:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  402413:	be 11 00 00 00       	mov    $0x11,%esi
  402418:	ba 65 32 40 00       	mov    $0x403265,%edx
  40241d:	49 89 c8             	mov    %rcx,%r8
  402420:	b9 1a 00 00 00       	mov    $0x1a,%ecx
  402425:	41 ff d2             	call   *%r10
  402428:	49 8b 41 20          	mov    0x20(%r9),%rax
  40242c:	48 85 c0             	test   %rax,%rax
  40242f:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  402435:	4c 0f 45 d0          	cmovne %rax,%r10
  402439:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  40243e:	be 11 00 00 00       	mov    $0x11,%esi
  402443:	ba 80 32 40 00       	mov    $0x403280,%edx
  402448:	49 89 c8             	mov    %rcx,%r8
  40244b:	b9 20 00 00 00       	mov    $0x20,%ecx
  402450:	41 ff d2             	call   *%r10
  402453:	49 8b 41 20          	mov    0x20(%r9),%rax
  402457:	48 85 c0             	test   %rax,%rax
  40245a:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  402460:	4c 0f 45 d0          	cmovne %rax,%r10
  402464:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  402469:	be 11 00 00 00       	mov    $0x11,%esi
  40246e:	ba a0 30 40 00       	mov    $0x4030a0,%edx
  402473:	b9 0f 00 00 00       	mov    $0xf,%ecx
  402478:	41 b8 10 31 40 00    	mov    $0x403110,%r8d
  40247e:	41 ff d2             	call   *%r10
  402481:	49 8b 41 20          	mov    0x20(%r9),%rax
  402485:	48 85 c0             	test   %rax,%rax
  402488:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  40248e:	4c 0f 45 d0          	cmovne %rax,%r10
  402492:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  402497:	be 11 00 00 00       	mov    $0x11,%esi
  40249c:	ba 38 31 40 00       	mov    $0x403138,%edx
  4024a1:	b9 11 00 00 00       	mov    $0x11,%ecx
  4024a6:	41 b8 50 31 40 00    	mov    $0x403150,%r8d
  4024ac:	41 ff d2             	call   *%r10
  4024af:	90                   	nop

00000000004024b0 <runtime::print_string>:
  4024b0:	48 89 f2             	mov    %rsi,%rdx
  4024b3:	48 89 fe             	mov    %rdi,%rsi
  4024b6:	b8 01 00 00 00       	mov    $0x1,%eax
  4024bb:	bf 02 00 00 00       	mov    $0x2,%edi
  4024c0:	0f 05                	syscall
  4024c2:	c3                   	ret
  4024c3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4024ca:	84 00 00 00 00 00 

00000000004024d0 <runtime::print_byte>:
  4024d0:	40 88 7c 24 f8       	mov    %dil,-0x8(%rsp)
  4024d5:	48 8d 74 24 f8       	lea    -0x8(%rsp),%rsi
  4024da:	b8 01 00 00 00       	mov    $0x1,%eax
  4024df:	bf 02 00 00 00       	mov    $0x2,%edi
  4024e4:	ba 01 00 00 00       	mov    $0x1,%edx
  4024e9:	0f 05                	syscall
  4024eb:	c3                   	ret
  4024ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004024f0 <runtime::print_u64>:
  4024f0:	50                   	push   %rax
  4024f1:	48 89 fa             	mov    %rdi,%rdx
  4024f4:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4024f8:	c5 fc 11 44 24 e0    	vmovups %ymm0,-0x20(%rsp)
  4024fe:	c5 fc 11 44 24 c0    	vmovups %ymm0,-0x40(%rsp)
  402504:	c5 fc 11 44 24 a0    	vmovups %ymm0,-0x60(%rsp)
  40250a:	c5 fc 11 44 24 80    	vmovups %ymm0,-0x80(%rsp)
  402510:	c6 04 24 00          	movb   $0x0,(%rsp)
  402514:	b8 81 00 00 00       	mov    $0x81,%eax
  402519:	48 83 ff 0a          	cmp    $0xa,%rdi
  40251d:	72 48                	jb     402567 <runtime::print_u64+0x77>
  40251f:	be 81 00 00 00       	mov    $0x81,%esi
  402524:	48 b9 cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rcx
  40252b:	cc cc cc 
  40252e:	66 90                	xchg   %ax,%ax
  402530:	48 8d 46 ff          	lea    -0x1(%rsi),%rax
  402534:	c4 e2 c3 f6 f9       	mulx   %rcx,%rdi,%rdi
  402539:	48 c1 ef 03          	shr    $0x3,%rdi
  40253d:	4c 8d 04 3f          	lea    (%rdi,%rdi,1),%r8
  402541:	4f 8d 04 80          	lea    (%r8,%r8,4),%r8
  402545:	49 f7 d8             	neg    %r8
  402548:	46 0f b6 84 02 e9 32 	movzbl 0x4032e9(%rdx,%r8,1),%r8d
  40254f:	40 00 
  402551:	44 88 84 34 7f ff ff 	mov    %r8b,-0x81(%rsp,%rsi,1)
  402558:	ff 
  402559:	48 89 c6             	mov    %rax,%rsi
  40255c:	48 83 fa 63          	cmp    $0x63,%rdx
  402560:	48 89 fa             	mov    %rdi,%rdx
  402563:	77 cb                	ja     402530 <runtime::print_u64+0x40>
  402565:	eb 03                	jmp    40256a <runtime::print_u64+0x7a>
  402567:	48 89 d7             	mov    %rdx,%rdi
  40256a:	48 8d 4c 24 80       	lea    -0x80(%rsp),%rcx
  40256f:	48 8d 74 08 ff       	lea    -0x1(%rax,%rcx,1),%rsi
  402574:	0f b6 97 e9 32 40 00 	movzbl 0x4032e9(%rdi),%edx
  40257b:	88 54 08 ff          	mov    %dl,-0x1(%rax,%rcx,1)
  40257f:	ba 82 00 00 00       	mov    $0x82,%edx
  402584:	48 29 c2             	sub    %rax,%rdx
  402587:	b8 01 00 00 00       	mov    $0x1,%eax
  40258c:	bf 02 00 00 00       	mov    $0x2,%edi
  402591:	0f 05                	syscall
  402593:	58                   	pop    %rax
  402594:	c5 f8 77             	vzeroupper
  402597:	c3                   	ret
  402598:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40259f:	00 

00000000004025a0 <runtime::print_i64>:
  4025a0:	50                   	push   %rax
  4025a1:	48 89 fa             	mov    %rdi,%rdx
  4025a4:	48 f7 da             	neg    %rdx
  4025a7:	48 0f 48 d7          	cmovs  %rdi,%rdx
  4025ab:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4025af:	c5 fc 11 44 24 e0    	vmovups %ymm0,-0x20(%rsp)
  4025b5:	c5 fc 11 44 24 c0    	vmovups %ymm0,-0x40(%rsp)
  4025bb:	c5 fc 11 44 24 a0    	vmovups %ymm0,-0x60(%rsp)
  4025c1:	c5 fc 11 44 24 80    	vmovups %ymm0,-0x80(%rsp)
  4025c7:	c6 04 24 00          	movb   $0x0,(%rsp)
  4025cb:	41 b8 81 00 00 00    	mov    $0x81,%r8d
  4025d1:	48 83 fa 0a          	cmp    $0xa,%rdx
  4025d5:	7c 50                	jl     402627 <runtime::print_i64+0x87>
  4025d7:	be 81 00 00 00       	mov    $0x81,%esi
  4025dc:	48 b8 cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rax
  4025e3:	cc cc cc 
  4025e6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4025ed:	00 00 00 
  4025f0:	4c 8d 46 ff          	lea    -0x1(%rsi),%r8
  4025f4:	c4 e2 f3 f6 c8       	mulx   %rax,%rcx,%rcx
  4025f9:	48 c1 e9 03          	shr    $0x3,%rcx
  4025fd:	4c 8d 0c 09          	lea    (%rcx,%rcx,1),%r9
  402601:	4f 8d 0c 89          	lea    (%r9,%r9,4),%r9
  402605:	49 f7 d9             	neg    %r9
  402608:	46 0f b6 8c 0a e9 32 	movzbl 0x4032e9(%rdx,%r9,1),%r9d
  40260f:	40 00 
  402611:	44 88 8c 34 7f ff ff 	mov    %r9b,-0x81(%rsp,%rsi,1)
  402618:	ff 
  402619:	4c 89 c6             	mov    %r8,%rsi
  40261c:	48 83 fa 63          	cmp    $0x63,%rdx
  402620:	48 89 ca             	mov    %rcx,%rdx
  402623:	77 cb                	ja     4025f0 <runtime::print_i64+0x50>
  402625:	eb 03                	jmp    40262a <runtime::print_i64+0x8a>
  402627:	48 89 d1             	mov    %rdx,%rcx
  40262a:	48 ba 67 66 66 66 66 	movabs $0x6666666666666667,%rdx
  402631:	66 66 66 
  402634:	48 89 c8             	mov    %rcx,%rax
  402637:	48 f7 ea             	imul   %rdx
  40263a:	48 89 d0             	mov    %rdx,%rax
  40263d:	48 c1 e8 3f          	shr    $0x3f,%rax
  402641:	48 c1 fa 02          	sar    $0x2,%rdx
  402645:	48 01 c2             	add    %rax,%rdx
  402648:	48 01 d2             	add    %rdx,%rdx
  40264b:	48 8d 04 92          	lea    (%rdx,%rdx,4),%rax
  40264f:	48 f7 d8             	neg    %rax
  402652:	0f b6 84 01 e9 32 40 	movzbl 0x4032e9(%rcx,%rax,1),%eax
  402659:	00 
  40265a:	42 88 84 04 7f ff ff 	mov    %al,-0x81(%rsp,%r8,1)
  402661:	ff 
  402662:	48 85 ff             	test   %rdi,%rdi
  402665:	78 05                	js     40266c <runtime::print_i64+0xcc>
  402667:	49 ff c8             	dec    %r8
  40266a:	eb 0d                	jmp    402679 <runtime::print_i64+0xd9>
  40266c:	42 c6 84 04 7e ff ff 	movb   $0x2d,-0x82(%rsp,%r8,1)
  402673:	ff 2d 
  402675:	49 83 c0 fe          	add    $0xfffffffffffffffe,%r8
  402679:	4a 8d 74 04 80       	lea    -0x80(%rsp,%r8,1),%rsi
  40267e:	ba 81 00 00 00       	mov    $0x81,%edx
  402683:	4c 29 c2             	sub    %r8,%rdx
  402686:	b8 01 00 00 00       	mov    $0x1,%eax
  40268b:	bf 02 00 00 00       	mov    $0x2,%edi
  402690:	0f 05                	syscall
  402692:	58                   	pop    %rax
  402693:	c5 f8 77             	vzeroupper
  402696:	c3                   	ret
  402697:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40269e:	00 00 

00000000004026a0 <runtime::print_caller_location>:
  4026a0:	53                   	push   %rbx
  4026a1:	48 89 fb             	mov    %rdi,%rbx
  4026a4:	48 8b 3f             	mov    (%rdi),%rdi
  4026a7:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  4026ab:	e8 00 fe ff ff       	call   4024b0 <runtime::print_string>
  4026b0:	bf 28 00 00 00       	mov    $0x28,%edi
  4026b5:	e8 16 fe ff ff       	call   4024d0 <runtime::print_byte>
  4026ba:	48 63 7b 10          	movslq 0x10(%rbx),%rdi
  4026be:	e8 2d fe ff ff       	call   4024f0 <runtime::print_u64>
  4026c3:	83 7b 14 00          	cmpl   $0x0,0x14(%rbx)
  4026c7:	74 13                	je     4026dc <runtime::print_caller_location+0x3c>
  4026c9:	bf 3a 00 00 00       	mov    $0x3a,%edi
  4026ce:	e8 fd fd ff ff       	call   4024d0 <runtime::print_byte>
  4026d3:	48 63 7b 14          	movslq 0x14(%rbx),%rdi
  4026d7:	e8 14 fe ff ff       	call   4024f0 <runtime::print_u64>
  4026dc:	bf 29 00 00 00       	mov    $0x29,%edi
  4026e1:	5b                   	pop    %rbx
  4026e2:	e9 e9 fd ff ff       	jmp    4024d0 <runtime::print_byte>
  4026e7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4026ee:	00 00 

00000000004026f0 <runtime::default_logger_proc>:
  4026f0:	c3                   	ret
  4026f1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4026f8:	0f 1f 84 00 00 00 00 
  4026ff:	00 

0000000000402700 <__truncsfhf2>:
  402700:	c5 f9 7e c7          	vmovd  %xmm0,%edi
  402704:	89 f8                	mov    %edi,%eax
  402706:	c1 e8 10             	shr    $0x10,%eax
  402709:	25 00 80 00 00       	and    $0x8000,%eax
  40270e:	b9 17 08 00 00       	mov    $0x817,%ecx
  402713:	c4 e2 70 f7 d7       	bextr  %ecx,%edi,%edx
  402718:	89 f9                	mov    %edi,%ecx
  40271a:	81 e1 ff ff 7f 00    	and    $0x7fffff,%ecx
  402720:	83 fa 70             	cmp    $0x70,%edx
  402723:	77 32                	ja     402757 <__truncsfhf2+0x57>
  402725:	83 fa 66             	cmp    $0x66,%edx
  402728:	0f 82 40 01 00 00    	jb     40286e <__truncsfhf2+0x16e>
  40272e:	81 c9 00 00 80 00    	or     $0x800000,%ecx
  402734:	40 b6 71             	mov    $0x71,%sil
  402737:	40 28 d6             	sub    %dl,%sil
  40273a:	c4 e2 4b f7 c9       	shrx   %esi,%ecx,%ecx
  40273f:	89 ca                	mov    %ecx,%edx
  402741:	81 e2 00 10 00 00    	and    $0x1000,%edx
  402747:	8d 0c 51             	lea    (%rcx,%rdx,2),%ecx
  40274a:	c1 e9 0d             	shr    $0xd,%ecx
  40274d:	09 c1                	or     %eax,%ecx
  40274f:	89 c8                	mov    %ecx,%eax
  402751:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  402756:	c3                   	ret
  402757:	8d 72 90             	lea    -0x70(%rdx),%esi
  40275a:	81 fe 8f 00 00 00    	cmp    $0x8f,%esi
  402760:	75 2b                	jne    40278d <__truncsfhf2+0x8d>
  402762:	85 c9                	test   %ecx,%ecx
  402764:	0f 84 ed 00 00 00    	je     402857 <__truncsfhf2+0x157>
  40276a:	89 ca                	mov    %ecx,%edx
  40276c:	c1 ea 0d             	shr    $0xd,%edx
  40276f:	31 f6                	xor    %esi,%esi
  402771:	81 f9 00 20 00 00    	cmp    $0x2000,%ecx
  402777:	40 0f 92 c6          	setb   %sil
  40277b:	09 c2                	or     %eax,%edx
  40277d:	09 f2                	or     %esi,%edx
  40277f:	81 ca 00 7c 00 00    	or     $0x7c00,%edx
  402785:	89 d0                	mov    %edx,%eax
  402787:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  40278c:	c3                   	ret
  40278d:	f7 c7 00 10 00 00    	test   $0x1000,%edi
  402793:	74 1e                	je     4027b3 <__truncsfhf2+0xb3>
  402795:	8d b9 00 20 00 00    	lea    0x2000(%rcx),%edi
  40279b:	83 c2 91             	add    $0xffffff91,%edx
  40279e:	45 31 c0             	xor    %r8d,%r8d
  4027a1:	81 f9 00 e0 7f 00    	cmp    $0x7fe000,%ecx
  4027a7:	44 0f 42 c7          	cmovb  %edi,%r8d
  4027ab:	0f 42 d6             	cmovb  %esi,%edx
  4027ae:	44 89 c1             	mov    %r8d,%ecx
  4027b1:	89 d6                	mov    %edx,%esi
  4027b3:	83 fe 1f             	cmp    $0x1f,%esi
  4027b6:	0f 82 a6 00 00 00    	jb     402862 <__truncsfhf2+0x162>
  4027bc:	48 b9 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rcx
  4027c3:	00 00 00 
  4027c6:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4027cb:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4027d0:	48 0f af c9          	imul   %rcx,%rcx
  4027d4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4027d9:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4027de:	48 0f af c9          	imul   %rcx,%rcx
  4027e2:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4027e7:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4027ec:	48 0f af c9          	imul   %rcx,%rcx
  4027f0:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4027f5:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4027fa:	48 0f af c9          	imul   %rcx,%rcx
  4027fe:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402803:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402808:	48 0f af c9          	imul   %rcx,%rcx
  40280c:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402811:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402816:	48 0f af c9          	imul   %rcx,%rcx
  40281a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40281f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402824:	48 0f af c9          	imul   %rcx,%rcx
  402828:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40282d:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402832:	48 0f af c9          	imul   %rcx,%rcx
  402836:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40283b:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402840:	48 0f af c9          	imul   %rcx,%rcx
  402844:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402849:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40284e:	48 0f af c9          	imul   %rcx,%rcx
  402852:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402857:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40285c:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  402861:	c3                   	ret
  402862:	c1 e6 0a             	shl    $0xa,%esi
  402865:	c1 e9 0d             	shr    $0xd,%ecx
  402868:	09 c1                	or     %eax,%ecx
  40286a:	09 f1                	or     %esi,%ecx
  40286c:	89 c8                	mov    %ecx,%eax
  40286e:	c5 f9 c4 c0 00       	vpinsrw $0x0,%eax,%xmm0,%xmm0
  402873:	c3                   	ret
  402874:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40287b:	00 00 00 00 00 

0000000000402880 <__truncdfhf2>:
  402880:	c5 fb 5a c0          	vcvtsd2ss %xmm0,%xmm0,%xmm0
  402884:	e9 77 fe ff ff       	jmp    402700 <__truncsfhf2>
  402889:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000402890 <__gnu_h2f_ieee>:
  402890:	c5 f9 c5 c0 00       	vpextrw $0x0,%xmm0,%eax
  402895:	89 c1                	mov    %eax,%ecx
  402897:	81 e1 ff 7f 00 00    	and    $0x7fff,%ecx
  40289d:	c1 e1 0d             	shl    $0xd,%ecx
  4028a0:	c5 f9 6e c1          	vmovd  %ecx,%xmm0
  4028a4:	c5 fa 59 05 58 07 00 	vmulss 0x758(%rip),%xmm0,%xmm0        # 403004 <_IO_stdin_used+0x4>
  4028ab:	00 
  4028ac:	c5 f9 7e c1          	vmovd  %xmm0,%ecx
  4028b0:	89 ca                	mov    %ecx,%edx
  4028b2:	81 ca 00 00 80 7f    	or     $0x7f800000,%edx
  4028b8:	c5 f8 2e 05 48 07 00 	vucomiss 0x748(%rip),%xmm0        # 403008 <_IO_stdin_used+0x8>
  4028bf:	00 
  4028c0:	0f 42 d1             	cmovb  %ecx,%edx
  4028c3:	25 00 80 00 00       	and    $0x8000,%eax
  4028c8:	c1 e0 10             	shl    $0x10,%eax
  4028cb:	09 d0                	or     %edx,%eax
  4028cd:	c5 f9 6e c0          	vmovd  %eax,%xmm0
  4028d1:	c3                   	ret
  4028d2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4028d9:	1f 84 00 00 00 00 00 

00000000004028e0 <__gnu_f2h_ieee>:
  4028e0:	e9 1b fe ff ff       	jmp    402700 <__truncsfhf2>
  4028e5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4028ec:	00 00 00 00 

00000000004028f0 <__extendhfsf2>:
  4028f0:	c5 f9 c5 c0 00       	vpextrw $0x0,%xmm0,%eax
  4028f5:	89 c1                	mov    %eax,%ecx
  4028f7:	81 e1 ff 7f 00 00    	and    $0x7fff,%ecx
  4028fd:	c1 e1 0d             	shl    $0xd,%ecx
  402900:	c5 f9 6e c1          	vmovd  %ecx,%xmm0
  402904:	c5 fa 59 05 f8 06 00 	vmulss 0x6f8(%rip),%xmm0,%xmm0        # 403004 <_IO_stdin_used+0x4>
  40290b:	00 
  40290c:	c5 f9 7e c1          	vmovd  %xmm0,%ecx
  402910:	89 ca                	mov    %ecx,%edx
  402912:	81 ca 00 00 80 7f    	or     $0x7f800000,%edx
  402918:	c5 f8 2e 05 e8 06 00 	vucomiss 0x6e8(%rip),%xmm0        # 403008 <_IO_stdin_used+0x8>
  40291f:	00 
  402920:	0f 42 d1             	cmovb  %ecx,%edx
  402923:	25 00 80 00 00       	and    $0x8000,%eax
  402928:	c1 e0 10             	shl    $0x10,%eax
  40292b:	09 d0                	or     %edx,%eax
  40292d:	c5 f9 6e c0          	vmovd  %eax,%xmm0
  402931:	c3                   	ret
  402932:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402939:	1f 84 00 00 00 00 00 

0000000000402940 <__floattidf>:
  402940:	48 89 f8             	mov    %rdi,%rax
  402943:	48 09 f0             	or     %rsi,%rax
  402946:	74 62                	je     4029aa <__floattidf+0x6a>
  402948:	48 89 f0             	mov    %rsi,%rax
  40294b:	48 c1 f8 3f          	sar    $0x3f,%rax
  40294f:	48 31 c6             	xor    %rax,%rsi
  402952:	48 31 c7             	xor    %rax,%rdi
  402955:	48 29 c7             	sub    %rax,%rdi
  402958:	48 19 c6             	sbb    %rax,%rsi
  40295b:	f3 48 0f bd ce       	lzcnt  %rsi,%rcx
  402960:	f3 48 0f bd d7       	lzcnt  %rdi,%rdx
  402965:	48 83 c2 40          	add    $0x40,%rdx
  402969:	48 85 f6             	test   %rsi,%rsi
  40296c:	48 0f 45 d1          	cmovne %rcx,%rdx
  402970:	89 d1                	mov    %edx,%ecx
  402972:	83 f1 7f             	xor    $0x7f,%ecx
  402975:	49 89 f8             	mov    %rdi,%r8
  402978:	49 c1 e8 35          	shr    $0x35,%r8
  40297c:	49 09 f0             	or     %rsi,%r8
  40297f:	74 2e                	je     4029af <__floattidf+0x6f>
  402981:	41 89 d0             	mov    %edx,%r8d
  402984:	41 80 e0 7f          	and    $0x7f,%r8b
  402988:	41 80 f8 49          	cmp    $0x49,%r8b
  40298c:	0f 84 e7 00 00 00    	je     402a79 <__floattidf+0x139>
  402992:	83 fa 4a             	cmp    $0x4a,%edx
  402995:	75 2e                	jne    4029c5 <__floattidf+0x85>
  402997:	49 89 f8             	mov    %rdi,%r8
  40299a:	49 c1 e8 3f          	shr    $0x3f,%r8
  40299e:	49 8d 34 70          	lea    (%r8,%rsi,2),%rsi
  4029a2:	48 01 ff             	add    %rdi,%rdi
  4029a5:	e9 cf 00 00 00       	jmp    402a79 <__floattidf+0x139>
  4029aa:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  4029ae:	c3                   	ret
  4029af:	80 c2 35             	add    $0x35,%dl
  4029b2:	c4 e2 e9 f7 ff       	shlx   %rdx,%rdi,%rdi
  4029b7:	31 f6                	xor    %esi,%esi
  4029b9:	f6 c2 40             	test   $0x40,%dl
  4029bc:	48 0f 44 f7          	cmove  %rdi,%rsi
  4029c0:	e9 f0 00 00 00       	jmp    402ab5 <__floattidf+0x175>
  4029c5:	41 57                	push   %r15
  4029c7:	41 56                	push   %r14
  4029c9:	53                   	push   %rbx
  4029ca:	45 31 d2             	xor    %r10d,%r10d
  4029cd:	41 bb 49 00 00 00    	mov    $0x49,%r11d
  4029d3:	49 29 d3             	sub    %rdx,%r11
  4029d6:	bb 00 00 00 00       	mov    $0x0,%ebx
  4029db:	48 19 db             	sbb    %rbx,%rbx
  4029de:	c4 62 a3 f7 cf       	shrx   %r11,%rdi,%r9
  4029e3:	45 89 d8             	mov    %r11d,%r8d
  4029e6:	41 f6 d0             	not    %r8b
  4029e9:	4c 8d 34 36          	lea    (%rsi,%rsi,1),%r14
  4029ed:	c4 42 b9 f7 c6       	shlx   %r8,%r14,%r8
  4029f2:	4d 09 c8             	or     %r9,%r8
  4029f5:	c4 62 a3 f7 ce       	shrx   %r11,%rsi,%r9
  4029fa:	41 f6 c3 40          	test   $0x40,%r11b
  4029fe:	4d 0f 45 c1          	cmovne %r9,%r8
  402a02:	4d 0f 45 ca          	cmovne %r10,%r9
  402a06:	49 81 fb 80 00 00 00 	cmp    $0x80,%r11
  402a0d:	48 83 db 00          	sbb    $0x0,%rbx
  402a11:	4d 0f 43 ca          	cmovae %r10,%r9
  402a15:	4d 0f 43 c2          	cmovae %r10,%r8
  402a19:	49 89 fb             	mov    %rdi,%r11
  402a1c:	49 c1 eb 37          	shr    $0x37,%r11
  402a20:	48 c7 c3 ff ff ff ff 	mov    $0xffffffffffffffff,%rbx
  402a27:	c4 e2 eb f7 db       	shrx   %rdx,%rbx,%rbx
  402a2c:	41 89 d6             	mov    %edx,%r14d
  402a2f:	41 f6 d6             	not    %r14b
  402a32:	41 bf fe 03 00 00    	mov    $0x3fe,%r15d
  402a38:	c4 42 89 f7 f7       	shlx   %r14,%r15,%r14
  402a3d:	49 09 de             	or     %rbx,%r14
  402a40:	bb ff 01 00 00       	mov    $0x1ff,%ebx
  402a45:	c4 e2 eb f7 db       	shrx   %rdx,%rbx,%rbx
  402a4a:	f6 c2 40             	test   $0x40,%dl
  402a4d:	4c 0f 45 f3          	cmovne %rbx,%r14
  402a51:	49 0f 45 da          	cmovne %r10,%rbx
  402a55:	49 09 f3             	or     %rsi,%r11
  402a58:	4d 0f 44 f2          	cmove  %r10,%r14
  402a5c:	41 0f 44 da          	cmove  %r10d,%ebx
  402a60:	49 21 fe             	and    %rdi,%r14
  402a63:	21 de                	and    %ebx,%esi
  402a65:	31 ff                	xor    %edi,%edi
  402a67:	4c 09 f6             	or     %r14,%rsi
  402a6a:	40 0f 95 c7          	setne  %dil
  402a6e:	4c 09 c7             	or     %r8,%rdi
  402a71:	4c 89 ce             	mov    %r9,%rsi
  402a74:	5b                   	pop    %rbx
  402a75:	41 5e                	pop    %r14
  402a77:	41 5f                	pop    %r15
  402a79:	41 b8 02 01 00 00    	mov    $0x102,%r8d
  402a7f:	c4 62 38 f7 c7       	bextr  %r8d,%edi,%r8d
  402a84:	49 09 f8             	or     %rdi,%r8
  402a87:	49 83 c0 01          	add    $0x1,%r8
  402a8b:	48 83 d6 00          	adc    $0x0,%rsi
  402a8f:	49 0f ba e0 37       	bt     $0x37,%r8
  402a94:	72 0d                	jb     402aa3 <__floattidf+0x163>
  402a96:	49 c1 e8 02          	shr    $0x2,%r8
  402a9a:	48 c1 e6 3e          	shl    $0x3e,%rsi
  402a9e:	4c 09 c6             	or     %r8,%rsi
  402aa1:	eb 12                	jmp    402ab5 <__floattidf+0x175>
  402aa3:	49 c1 e8 03          	shr    $0x3,%r8
  402aa7:	48 c1 e6 3d          	shl    $0x3d,%rsi
  402aab:	4c 09 c6             	or     %r8,%rsi
  402aae:	b9 80 00 00 00       	mov    $0x80,%ecx
  402ab3:	29 d1                	sub    %edx,%ecx
  402ab5:	25 00 00 00 80       	and    $0x80000000,%eax
  402aba:	c1 e1 14             	shl    $0x14,%ecx
  402abd:	09 c1                	or     %eax,%ecx
  402abf:	48 89 f0             	mov    %rsi,%rax
  402ac2:	48 c1 e8 20          	shr    $0x20,%rax
  402ac6:	25 ff ff 0f 00       	and    $0xfffff,%eax
  402acb:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  402ad2:	48 c1 e0 20          	shl    $0x20,%rax
  402ad6:	89 f1                	mov    %esi,%ecx
  402ad8:	48 09 c1             	or     %rax,%rcx
  402adb:	c4 e1 f9 6e c1       	vmovq  %rcx,%xmm0
  402ae0:	c3                   	ret
  402ae1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402ae8:	0f 1f 84 00 00 00 00 
  402aef:	00 

0000000000402af0 <__floattidf_unsigned>:
  402af0:	48 89 f8             	mov    %rdi,%rax
  402af3:	48 09 f0             	or     %rsi,%rax
  402af6:	74 4c                	je     402b44 <__floattidf_unsigned+0x54>
  402af8:	f3 48 0f bd c6       	lzcnt  %rsi,%rax
  402afd:	f3 48 0f bd cf       	lzcnt  %rdi,%rcx
  402b02:	48 83 c1 40          	add    $0x40,%rcx
  402b06:	48 85 f6             	test   %rsi,%rsi
  402b09:	48 0f 45 c8          	cmovne %rax,%rcx
  402b0d:	89 c8                	mov    %ecx,%eax
  402b0f:	83 f0 7f             	xor    $0x7f,%eax
  402b12:	48 89 fa             	mov    %rdi,%rdx
  402b15:	48 c1 ea 35          	shr    $0x35,%rdx
  402b19:	48 09 f2             	or     %rsi,%rdx
  402b1c:	74 2b                	je     402b49 <__floattidf_unsigned+0x59>
  402b1e:	89 ca                	mov    %ecx,%edx
  402b20:	80 e2 7f             	and    $0x7f,%dl
  402b23:	80 fa 49             	cmp    $0x49,%dl
  402b26:	0f 84 e3 00 00 00    	je     402c0f <__floattidf_unsigned+0x11f>
  402b2c:	83 f9 4a             	cmp    $0x4a,%ecx
  402b2f:	75 2e                	jne    402b5f <__floattidf_unsigned+0x6f>
  402b31:	48 89 fa             	mov    %rdi,%rdx
  402b34:	48 c1 ea 3f          	shr    $0x3f,%rdx
  402b38:	48 8d 34 72          	lea    (%rdx,%rsi,2),%rsi
  402b3c:	48 01 ff             	add    %rdi,%rdi
  402b3f:	e9 cb 00 00 00       	jmp    402c0f <__floattidf_unsigned+0x11f>
  402b44:	c5 f8 57 c0          	vxorps %xmm0,%xmm0,%xmm0
  402b48:	c3                   	ret
  402b49:	80 c1 b5             	add    $0xb5,%cl
  402b4c:	c4 e2 f1 f7 d7       	shlx   %rcx,%rdi,%rdx
  402b51:	31 f6                	xor    %esi,%esi
  402b53:	f6 c1 40             	test   $0x40,%cl
  402b56:	48 0f 44 f2          	cmove  %rdx,%rsi
  402b5a:	e9 eb 00 00 00       	jmp    402c4a <__floattidf_unsigned+0x15a>
  402b5f:	41 56                	push   %r14
  402b61:	53                   	push   %rbx
  402b62:	45 31 c9             	xor    %r9d,%r9d
  402b65:	41 ba 49 00 00 00    	mov    $0x49,%r10d
  402b6b:	49 29 ca             	sub    %rcx,%r10
  402b6e:	41 bb 00 00 00 00    	mov    $0x0,%r11d
  402b74:	4d 19 db             	sbb    %r11,%r11
  402b77:	c4 62 ab f7 c7       	shrx   %r10,%rdi,%r8
  402b7c:	44 89 d2             	mov    %r10d,%edx
  402b7f:	f6 d2                	not    %dl
  402b81:	48 8d 1c 36          	lea    (%rsi,%rsi,1),%rbx
  402b85:	c4 e2 e9 f7 d3       	shlx   %rdx,%rbx,%rdx
  402b8a:	4c 09 c2             	or     %r8,%rdx
  402b8d:	c4 62 ab f7 c6       	shrx   %r10,%rsi,%r8
  402b92:	41 f6 c2 40          	test   $0x40,%r10b
  402b96:	49 0f 45 d0          	cmovne %r8,%rdx
  402b9a:	4d 0f 45 c1          	cmovne %r9,%r8
  402b9e:	49 81 fa 80 00 00 00 	cmp    $0x80,%r10
  402ba5:	49 83 db 00          	sbb    $0x0,%r11
  402ba9:	4d 0f 43 c1          	cmovae %r9,%r8
  402bad:	49 0f 43 d1          	cmovae %r9,%rdx
  402bb1:	49 89 fa             	mov    %rdi,%r10
  402bb4:	49 c1 ea 37          	shr    $0x37,%r10
  402bb8:	49 c7 c3 ff ff ff ff 	mov    $0xffffffffffffffff,%r11
  402bbf:	c4 42 f3 f7 db       	shrx   %rcx,%r11,%r11
  402bc4:	89 cb                	mov    %ecx,%ebx
  402bc6:	f6 d3                	not    %bl
  402bc8:	41 be fe 03 00 00    	mov    $0x3fe,%r14d
  402bce:	c4 c2 e1 f7 de       	shlx   %rbx,%r14,%rbx
  402bd3:	4c 09 db             	or     %r11,%rbx
  402bd6:	41 bb ff 01 00 00    	mov    $0x1ff,%r11d
  402bdc:	c4 42 f3 f7 db       	shrx   %rcx,%r11,%r11
  402be1:	f6 c1 40             	test   $0x40,%cl
  402be4:	49 0f 45 db          	cmovne %r11,%rbx
  402be8:	4d 0f 45 d9          	cmovne %r9,%r11
  402bec:	49 09 f2             	or     %rsi,%r10
  402bef:	49 0f 44 d9          	cmove  %r9,%rbx
  402bf3:	45 0f 44 d9          	cmove  %r9d,%r11d
  402bf7:	48 21 fb             	and    %rdi,%rbx
  402bfa:	44 21 de             	and    %r11d,%esi
  402bfd:	31 ff                	xor    %edi,%edi
  402bff:	48 09 de             	or     %rbx,%rsi
  402c02:	40 0f 95 c7          	setne  %dil
  402c06:	48 09 d7             	or     %rdx,%rdi
  402c09:	4c 89 c6             	mov    %r8,%rsi
  402c0c:	5b                   	pop    %rbx
  402c0d:	41 5e                	pop    %r14
  402c0f:	ba 02 01 00 00       	mov    $0x102,%edx
  402c14:	c4 e2 68 f7 d7       	bextr  %edx,%edi,%edx
  402c19:	48 09 fa             	or     %rdi,%rdx
  402c1c:	48 83 c2 01          	add    $0x1,%rdx
  402c20:	48 83 d6 00          	adc    $0x0,%rsi
  402c24:	48 0f ba e2 37       	bt     $0x37,%rdx
  402c29:	72 0d                	jb     402c38 <__floattidf_unsigned+0x148>
  402c2b:	48 c1 ea 02          	shr    $0x2,%rdx
  402c2f:	48 c1 e6 3e          	shl    $0x3e,%rsi
  402c33:	48 09 d6             	or     %rdx,%rsi
  402c36:	eb 12                	jmp    402c4a <__floattidf_unsigned+0x15a>
  402c38:	48 c1 ea 03          	shr    $0x3,%rdx
  402c3c:	48 c1 e6 3d          	shl    $0x3d,%rsi
  402c40:	48 09 d6             	or     %rdx,%rsi
  402c43:	b8 80 00 00 00       	mov    $0x80,%eax
  402c48:	29 c8                	sub    %ecx,%eax
  402c4a:	c1 e0 14             	shl    $0x14,%eax
  402c4d:	48 89 f1             	mov    %rsi,%rcx
  402c50:	48 c1 e9 20          	shr    $0x20,%rcx
  402c54:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  402c5a:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  402c61:	48 c1 e0 20          	shl    $0x20,%rax
  402c65:	89 f1                	mov    %esi,%ecx
  402c67:	48 09 c1             	or     %rax,%rcx
  402c6a:	c4 e1 f9 6e c1       	vmovq  %rcx,%xmm0
  402c6f:	c3                   	ret

0000000000402c70 <__umodti3>:
  402c70:	48 83 ec 18          	sub    $0x18,%rsp
  402c74:	49 89 e0             	mov    %rsp,%r8
  402c77:	e8 84 e7 ff ff       	call   401400 <runtime::udivmod128>
  402c7c:	48 8b 04 24          	mov    (%rsp),%rax
  402c80:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402c85:	48 83 c4 18          	add    $0x18,%rsp
  402c89:	c3                   	ret
  402c8a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402c90 <__udivmodti4>:
  402c90:	e9 6b e7 ff ff       	jmp    401400 <runtime::udivmod128>
  402c95:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402c9c:	00 00 00 00 

0000000000402ca0 <__udivti3>:
  402ca0:	45 31 c0             	xor    %r8d,%r8d
  402ca3:	e9 58 e7 ff ff       	jmp    401400 <runtime::udivmod128>

Disassembly of section .fini:

0000000000402ca8 <_fini>:
  402ca8:	f3 0f 1e fa          	endbr64
  402cac:	48 83 ec 08          	sub    $0x8,%rsp
  402cb0:	48 83 c4 08          	add    $0x8,%rsp
  402cb4:	c3                   	ret
