
/home/khalid/Documents/GitHub/Journey_ECS/main.bin:     file format elf64-x86-64


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
  4010b2:	0f 57 c0             	xorps  %xmm0,%xmm0
  4010b5:	0f 11 44 24 20       	movups %xmm0,0x20(%rsp)
  4010ba:	48 c7 44 24 08 a6 32 	movq   $0x4032a6,0x8(%rsp)
  4010c1:	40 00 
  4010c3:	48 c7 44 24 10 30 00 	movq   $0x30,0x10(%rsp)
  4010ca:	00 00 
  4010cc:	48 b8 4b 00 00 00 25 	movabs $0x250000004b,%rax
  4010d3:	00 00 00 
  4010d6:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4010db:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  4010e0:	e8 ab 13 00 00       	call   402490 <runtime::print_caller_location>
  4010e5:	bf 08 32 40 00       	mov    $0x403208,%edi
  4010ea:	be 17 00 00 00       	mov    $0x17,%esi
  4010ef:	e8 9c 11 00 00       	call   402290 <runtime::print_string>
  4010f4:	4c 89 ff             	mov    %r15,%rdi
  4010f7:	e8 94 12 00 00       	call   402390 <runtime::print_i64>
  4010fc:	bf 20 32 40 00       	mov    $0x403220,%edi
  401101:	be 01 00 00 00       	mov    $0x1,%esi
  401106:	e8 85 11 00 00       	call   402290 <runtime::print_string>
  40110b:	4c 89 f7             	mov    %r14,%rdi
  40110e:	e8 7d 12 00 00       	call   402390 <runtime::print_i64>
  401113:	bf 22 32 40 00       	mov    $0x403222,%edi
  401118:	be 15 00 00 00       	mov    $0x15,%esi
  40111d:	e8 6e 11 00 00       	call   402290 <runtime::print_string>
  401122:	48 89 df             	mov    %rbx,%rdi
  401125:	e8 66 12 00 00       	call   402390 <runtime::print_i64>
  40112a:	bf 0a 00 00 00       	mov    $0xa,%edi
  40112f:	e8 7c 11 00 00       	call   4022b0 <runtime::print_byte>
  401134:	0f 0b                	ud2
  401136:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40113d:	00 00 00 

0000000000401140 <runtime::multi_pointer_slice_handle_error>:
  401140:	41 56                	push   %r14
  401142:	53                   	push   %rbx
  401143:	48 83 ec 28          	sub    $0x28,%rsp
  401147:	4c 89 cb             	mov    %r9,%rbx
  40114a:	4d 89 c6             	mov    %r8,%r14
  40114d:	0f 57 c0             	xorps  %xmm0,%xmm0
  401150:	0f 11 44 24 18       	movups %xmm0,0x18(%rsp)
  401155:	48 89 3c 24          	mov    %rdi,(%rsp)
  401159:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40115e:	89 54 24 10          	mov    %edx,0x10(%rsp)
  401162:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  401166:	48 89 e7             	mov    %rsp,%rdi
  401169:	e8 22 13 00 00       	call   402490 <runtime::print_caller_location>
  40116e:	bf 08 32 40 00       	mov    $0x403208,%edi
  401173:	be 17 00 00 00       	mov    $0x17,%esi
  401178:	e8 13 11 00 00       	call   402290 <runtime::print_string>
  40117d:	4c 89 f7             	mov    %r14,%rdi
  401180:	e8 0b 12 00 00       	call   402390 <runtime::print_i64>
  401185:	bf 20 32 40 00       	mov    $0x403220,%edi
  40118a:	be 01 00 00 00       	mov    $0x1,%esi
  40118f:	e8 fc 10 00 00       	call   402290 <runtime::print_string>
  401194:	48 89 df             	mov    %rbx,%rdi
  401197:	e8 f4 11 00 00       	call   402390 <runtime::print_i64>
  40119c:	bf 0a 00 00 00       	mov    $0xa,%edi
  4011a1:	e8 0a 11 00 00       	call   4022b0 <runtime::print_byte>
  4011a6:	0f 0b                	ud2
  4011a8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4011af:	00 

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
  4011d7:	e8 b4 12 00 00       	call   402490 <runtime::print_caller_location>
  4011dc:	bf a1 32 40 00       	mov    $0x4032a1,%edi
  4011e1:	be 01 00 00 00       	mov    $0x1,%esi
  4011e6:	e8 a5 10 00 00       	call   402290 <runtime::print_string>
  4011eb:	4c 89 e7             	mov    %r12,%rdi
  4011ee:	4c 89 fe             	mov    %r15,%rsi
  4011f1:	e8 9a 10 00 00       	call   402290 <runtime::print_string>
  4011f6:	48 85 db             	test   %rbx,%rbx
  4011f9:	7e 1a                	jle    401215 <runtime::default_assertion_contextless_failure_proc+0x55>
  4011fb:	bf a3 32 40 00       	mov    $0x4032a3,%edi
  401200:	be 02 00 00 00       	mov    $0x2,%esi
  401205:	e8 86 10 00 00       	call   402290 <runtime::print_string>
  40120a:	4c 89 f7             	mov    %r14,%rdi
  40120d:	48 89 de             	mov    %rbx,%rsi
  401210:	e8 7b 10 00 00       	call   402290 <runtime::print_string>
  401215:	bf 0a 00 00 00       	mov    $0xa,%edi
  40121a:	e8 91 10 00 00       	call   4022b0 <runtime::print_byte>
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
  401248:	48 c7 c7 00 1e 40 00 	mov    $0x401e00,%rdi
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
  401270:	b8 48 50 40 00       	mov    $0x405048,%eax
  401275:	48 3d 48 50 40 00    	cmp    $0x405048,%rax
  40127b:	74 13                	je     401290 <_dl_relocate_static_pie+0x30>
  40127d:	48 8b 05 44 3d 00 00 	mov    0x3d44(%rip),%rax        # 404fc8 <_ITM_deregisterTMCloneTable@Base>
  401284:	48 85 c0             	test   %rax,%rax
  401287:	74 07                	je     401290 <_dl_relocate_static_pie+0x30>
  401289:	bf 48 50 40 00       	mov    $0x405048,%edi
  40128e:	ff e0                	jmp    *%rax
  401290:	c3                   	ret
  401291:	0f 1f 40 00          	nopl   0x0(%rax)
  401295:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40129c:	00 00 00 00 
  4012a0:	be 48 50 40 00       	mov    $0x405048,%esi
  4012a5:	48 81 ee 48 50 40 00 	sub    $0x405048,%rsi
  4012ac:	48 89 f0             	mov    %rsi,%rax
  4012af:	48 c1 ee 3f          	shr    $0x3f,%rsi
  4012b3:	48 c1 f8 03          	sar    $0x3,%rax
  4012b7:	48 01 c6             	add    %rax,%rsi
  4012ba:	48 d1 fe             	sar    $1,%rsi
  4012bd:	74 19                	je     4012d8 <_dl_relocate_static_pie+0x78>
  4012bf:	48 8b 05 12 3d 00 00 	mov    0x3d12(%rip),%rax        # 404fd8 <_ITM_registerTMCloneTable@Base>
  4012c6:	48 85 c0             	test   %rax,%rax
  4012c9:	74 0d                	je     4012d8 <_dl_relocate_static_pie+0x78>
  4012cb:	bf 48 50 40 00       	mov    $0x405048,%edi
  4012d0:	ff e0                	jmp    *%rax
  4012d2:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  4012d8:	c3                   	ret
  4012d9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
  4012e0:	f3 0f 1e fa          	endbr64
  4012e4:	80 3d 5d 3d 00 00 00 	cmpb   $0x0,0x3d5d(%rip)        # 405048 <__TMC_END__>
  4012eb:	75 13                	jne    401300 <_dl_relocate_static_pie+0xa0>
  4012ed:	55                   	push   %rbp
  4012ee:	48 89 e5             	mov    %rsp,%rbp
  4012f1:	e8 7a ff ff ff       	call   401270 <_dl_relocate_static_pie+0x10>
  4012f6:	c6 05 4b 3d 00 00 01 	movb   $0x1,0x3d4b(%rip)        # 405048 <__TMC_END__>
  4012fd:	5d                   	pop    %rbp
  4012fe:	c3                   	ret
  4012ff:	90                   	nop
  401300:	c3                   	ret
  401301:	0f 1f 40 00          	nopl   0x0(%rax)
  401305:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40130c:	00 00 00 00 
  401310:	f3 0f 1e fa          	endbr64
  401314:	eb 8a                	jmp    4012a0 <_dl_relocate_static_pie+0x40>
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
  40134c:	0f 84 8d 00 00 00    	je     4013df <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x9f>
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
  401376:	74 60                	je     4013d8 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x98>
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
  40139e:	0f 57 c0             	xorps  %xmm0,%xmm0
  4013a1:	0f 29 04 24          	movaps %xmm0,(%rsp)
  4013a5:	48 83 ec 08          	sub    $0x8,%rsp
  4013a9:	be 01 00 00 00       	mov    $0x1,%esi
  4013ae:	31 d2                	xor    %edx,%edx
  4013b0:	31 c9                	xor    %ecx,%ecx
  4013b2:	45 31 c9             	xor    %r9d,%r9d
  4013b5:	53                   	push   %rbx
  4013b6:	41 56                	push   %r14
  4013b8:	68 e0 31 40 00       	push   $0x4031e0
  4013bd:	41 ff d2             	call   *%r10
  4013c0:	48 83 c4 20          	add    $0x20,%rsp
  4013c4:	64 48 8b 0c 25 c0 ff 	mov    %fs:0xffffffffffffffc0,%rcx
  4013cb:	ff ff 
  4013cd:	64 48 8b 04 25 d0 ff 	mov    %fs:0xffffffffffffffd0,%rax
  4013d4:	ff ff 
  4013d6:	eb 98                	jmp    401370 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x30>
  4013d8:	48 83 c4 18          	add    $0x18,%rsp
  4013dc:	5b                   	pop    %rbx
  4013dd:	41 5e                	pop    %r14
  4013df:	0f 57 c0             	xorps  %xmm0,%xmm0
  4013e2:	64 0f 29 04 25 d0 ff 	movaps %xmm0,%fs:0xffffffffffffffd0
  4013e9:	ff ff 
  4013eb:	64 0f 29 04 25 c0 ff 	movaps %xmm0,%fs:0xffffffffffffffc0
  4013f2:	ff ff 
  4013f4:	64 0f 29 04 25 b0 ff 	movaps %xmm0,%fs:0xffffffffffffffb0
  4013fb:	ff ff 
  4013fd:	64 48 c7 04 25 e0 ff 	movq   $0x0,%fs:0xffffffffffffffe0
  401404:	ff ff 00 00 00 00 
  40140a:	c3                   	ret
  40140b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401410 <runtime::udivmod128>:
  401410:	41 56                	push   %r14
  401412:	53                   	push   %rbx
  401413:	49 89 d1             	mov    %rdx,%r9
  401416:	48 89 f8             	mov    %rdi,%rax
  401419:	48 85 f6             	test   %rsi,%rsi
  40141c:	74 3b                	je     401459 <runtime::udivmod128+0x49>
  40141e:	48 89 f2             	mov    %rsi,%rdx
  401421:	4d 85 c9             	test   %r9,%r9
  401424:	74 54                	je     40147a <runtime::udivmod128+0x6a>
  401426:	48 85 c9             	test   %rcx,%rcx
  401429:	0f 84 86 00 00 00    	je     4014b5 <runtime::udivmod128+0xa5>
  40142f:	4c 0f bd d9          	bsr    %rcx,%r11
  401433:	41 83 f3 3f          	xor    $0x3f,%r11d
  401437:	48 0f bd f2          	bsr    %rdx,%rsi
  40143b:	83 f6 3f             	xor    $0x3f,%esi
  40143e:	41 29 f3             	sub    %esi,%r11d
  401441:	41 83 fb 40          	cmp    $0x40,%r11d
  401445:	0f 82 d7 00 00 00    	jb     401522 <runtime::udivmod128+0x112>
  40144b:	4d 85 c0             	test   %r8,%r8
  40144e:	74 22                	je     401472 <runtime::udivmod128+0x62>
  401450:	49 89 00             	mov    %rax,(%r8)
  401453:	49 89 50 08          	mov    %rdx,0x8(%r8)
  401457:	eb 19                	jmp    401472 <runtime::udivmod128+0x62>
  401459:	48 85 c9             	test   %rcx,%rcx
  40145c:	0f 84 a2 00 00 00    	je     401504 <runtime::udivmod128+0xf4>
  401462:	4d 85 c0             	test   %r8,%r8
  401465:	74 0b                	je     401472 <runtime::udivmod128+0x62>
  401467:	49 89 00             	mov    %rax,(%r8)
  40146a:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  401471:	00 
  401472:	31 c0                	xor    %eax,%eax
  401474:	31 d2                	xor    %edx,%edx
  401476:	5b                   	pop    %rbx
  401477:	41 5e                	pop    %r14
  401479:	c3                   	ret
  40147a:	48 85 c9             	test   %rcx,%rcx
  40147d:	0f 84 d5 00 00 00    	je     401558 <runtime::udivmod128+0x148>
  401483:	48 85 c0             	test   %rax,%rax
  401486:	0f 84 6c 01 00 00    	je     4015f8 <runtime::udivmod128+0x1e8>
  40148c:	48 8d 71 ff          	lea    -0x1(%rcx),%rsi
  401490:	48 85 f1             	test   %rsi,%rcx
  401493:	0f 85 80 01 00 00    	jne    401619 <runtime::udivmod128+0x209>
  401499:	4d 85 c0             	test   %r8,%r8
  40149c:	74 0a                	je     4014a8 <runtime::udivmod128+0x98>
  40149e:	48 21 d6             	and    %rdx,%rsi
  4014a1:	49 89 00             	mov    %rax,(%r8)
  4014a4:	49 89 70 08          	mov    %rsi,0x8(%r8)
  4014a8:	f3 48 0f bc c9       	tzcnt  %rcx,%rcx
  4014ad:	48 d3 ea             	shr    %cl,%rdx
  4014b0:	48 89 d0             	mov    %rdx,%rax
  4014b3:	eb bf                	jmp    401474 <runtime::udivmod128+0x64>
  4014b5:	49 8d 71 ff          	lea    -0x1(%r9),%rsi
  4014b9:	49 85 f1             	test   %rsi,%r9
  4014bc:	0f 85 9b 00 00 00    	jne    40155d <runtime::udivmod128+0x14d>
  4014c2:	4d 85 c0             	test   %r8,%r8
  4014c5:	74 0e                	je     4014d5 <runtime::udivmod128+0xc5>
  4014c7:	48 21 c6             	and    %rax,%rsi
  4014ca:	49 89 30             	mov    %rsi,(%r8)
  4014cd:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  4014d4:	00 
  4014d5:	49 83 f9 01          	cmp    $0x1,%r9
  4014d9:	74 9b                	je     401476 <runtime::udivmod128+0x66>
  4014db:	f3 49 0f bc f1       	tzcnt  %r9,%rsi
  4014e0:	48 89 d7             	mov    %rdx,%rdi
  4014e3:	89 f1                	mov    %esi,%ecx
  4014e5:	48 d3 ef             	shr    %cl,%rdi
  4014e8:	f6 d9                	neg    %cl
  4014ea:	48 d3 e2             	shl    %cl,%rdx
  4014ed:	48 85 f6             	test   %rsi,%rsi
  4014f0:	48 0f 44 d6          	cmove  %rsi,%rdx
  4014f4:	89 f1                	mov    %esi,%ecx
  4014f6:	48 d3 e8             	shr    %cl,%rax
  4014f9:	48 09 d0             	or     %rdx,%rax
  4014fc:	48 89 fa             	mov    %rdi,%rdx
  4014ff:	e9 72 ff ff ff       	jmp    401476 <runtime::udivmod128+0x66>
  401504:	31 d2                	xor    %edx,%edx
  401506:	49 f7 f1             	div    %r9
  401509:	4d 85 c0             	test   %r8,%r8
  40150c:	0f 84 62 ff ff ff    	je     401474 <runtime::udivmod128+0x64>
  401512:	49 89 10             	mov    %rdx,(%r8)
  401515:	49 c7 40 08 00 00 00 	movq   $0x0,0x8(%r8)
  40151c:	00 
  40151d:	e9 52 ff ff ff       	jmp    401474 <runtime::udivmod128+0x64>
  401522:	41 8d 73 01          	lea    0x1(%r11),%esi
  401526:	83 fe 40             	cmp    $0x40,%esi
  401529:	74 4e                	je     401579 <runtime::udivmod128+0x169>
  40152b:	49 89 d2             	mov    %rdx,%r10
  40152e:	48 89 cb             	mov    %rcx,%rbx
  401531:	89 f1                	mov    %esi,%ecx
  401533:	49 d3 ea             	shr    %cl,%r10
  401536:	40 b7 3f             	mov    $0x3f,%dil
  401539:	44 28 df             	sub    %r11b,%dil
  40153c:	89 f9                	mov    %edi,%ecx
  40153e:	48 d3 e2             	shl    %cl,%rdx
  401541:	49 89 c3             	mov    %rax,%r11
  401544:	89 f1                	mov    %esi,%ecx
  401546:	49 d3 eb             	shr    %cl,%r11
  401549:	4c 09 da             	or     %r11,%rdx
  40154c:	89 f9                	mov    %edi,%ecx
  40154e:	48 d3 e0             	shl    %cl,%rax
  401551:	48 89 d9             	mov    %rbx,%rcx
  401554:	31 ff                	xor    %edi,%edi
  401556:	eb 2b                	jmp    401583 <runtime::udivmod128+0x173>
  401558:	e9 19 ff ff ff       	jmp    401476 <runtime::udivmod128+0x66>
  40155d:	49 0f bd f1          	bsr    %r9,%rsi
  401561:	83 f6 3f             	xor    $0x3f,%esi
  401564:	48 0f bd fa          	bsr    %rdx,%rdi
  401568:	83 f7 3f             	xor    $0x3f,%edi
  40156b:	29 fe                	sub    %edi,%esi
  40156d:	83 c6 41             	add    $0x41,%esi
  401570:	83 fe 40             	cmp    $0x40,%esi
  401573:	0f 85 e9 00 00 00    	jne    401662 <runtime::udivmod128+0x252>
  401579:	be 40 00 00 00       	mov    $0x40,%esi
  40157e:	31 ff                	xor    %edi,%edi
  401580:	45 31 d2             	xor    %r10d,%r10d
  401583:	49 89 d3             	mov    %rdx,%r11
  401586:	31 db                	xor    %ebx,%ebx
  401588:	48 89 c2             	mov    %rax,%rdx
  40158b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  401590:	4d 0f a4 da 01       	shld   $0x1,%r11,%r10
  401595:	49 0f a4 d3 01       	shld   $0x1,%rdx,%r11
  40159a:	48 0f a4 fa 01       	shld   $0x1,%rdi,%rdx
  40159f:	48 89 f8             	mov    %rdi,%rax
  4015a2:	48 01 f8             	add    %rdi,%rax
  4015a5:	89 df                	mov    %ebx,%edi
  4015a7:	48 09 c7             	or     %rax,%rdi
  4015aa:	4c 89 d0             	mov    %r10,%rax
  4015ad:	48 f7 d0             	not    %rax
  4015b0:	4c 89 db             	mov    %r11,%rbx
  4015b3:	48 f7 d3             	not    %rbx
  4015b6:	4c 01 cb             	add    %r9,%rbx
  4015b9:	48 11 c8             	adc    %rcx,%rax
  4015bc:	48 89 c3             	mov    %rax,%rbx
  4015bf:	48 c1 eb 3f          	shr    $0x3f,%rbx
  4015c3:	48 c1 f8 3f          	sar    $0x3f,%rax
  4015c7:	49 89 c6             	mov    %rax,%r14
  4015ca:	49 21 ce             	and    %rcx,%r14
  4015cd:	4c 21 c8             	and    %r9,%rax
  4015d0:	49 29 c3             	sub    %rax,%r11
  4015d3:	4d 19 f2             	sbb    %r14,%r10
  4015d6:	ff ce                	dec    %esi
  4015d8:	75 b6                	jne    401590 <runtime::udivmod128+0x180>
  4015da:	48 0f a4 fa 01       	shld   $0x1,%rdi,%rdx
  4015df:	48 8d 04 7b          	lea    (%rbx,%rdi,2),%rax
  4015e3:	4d 85 c0             	test   %r8,%r8
  4015e6:	0f 84 8a fe ff ff    	je     401476 <runtime::udivmod128+0x66>
  4015ec:	4d 89 18             	mov    %r11,(%r8)
  4015ef:	4d 89 50 08          	mov    %r10,0x8(%r8)
  4015f3:	e9 7e fe ff ff       	jmp    401476 <runtime::udivmod128+0x66>
  4015f8:	48 89 d0             	mov    %rdx,%rax
  4015fb:	31 d2                	xor    %edx,%edx
  4015fd:	48 f7 f1             	div    %rcx
  401600:	4d 85 c0             	test   %r8,%r8
  401603:	0f 84 6b fe ff ff    	je     401474 <runtime::udivmod128+0x64>
  401609:	49 89 50 08          	mov    %rdx,0x8(%r8)
  40160d:	49 c7 00 00 00 00 00 	movq   $0x0,(%r8)
  401614:	e9 5b fe ff ff       	jmp    401474 <runtime::udivmod128+0x64>
  401619:	48 0f bd f9          	bsr    %rcx,%rdi
  40161d:	83 f7 3f             	xor    $0x3f,%edi
  401620:	48 0f bd f2          	bsr    %rdx,%rsi
  401624:	83 f6 3f             	xor    $0x3f,%esi
  401627:	29 f7                	sub    %esi,%edi
  401629:	83 ff 3f             	cmp    $0x3f,%edi
  40162c:	0f 83 19 fe ff ff    	jae    40144b <runtime::udivmod128+0x3b>
  401632:	8d 77 01             	lea    0x1(%rdi),%esi
  401635:	40 f6 d7             	not    %dil
  401638:	48 89 c3             	mov    %rax,%rbx
  40163b:	49 89 cb             	mov    %rcx,%r11
  40163e:	89 f9                	mov    %edi,%ecx
  401640:	48 d3 e3             	shl    %cl,%rbx
  401643:	49 89 c6             	mov    %rax,%r14
  401646:	49 89 d2             	mov    %rdx,%r10
  401649:	89 f1                	mov    %esi,%ecx
  40164b:	49 d3 ea             	shr    %cl,%r10
  40164e:	49 0f ad d6          	shrd   %cl,%rdx,%r14
  401652:	4c 89 d9             	mov    %r11,%rcx
  401655:	31 ff                	xor    %edi,%edi
  401657:	48 89 d8             	mov    %rbx,%rax
  40165a:	4c 89 f2             	mov    %r14,%rdx
  40165d:	e9 21 ff ff ff       	jmp    401583 <runtime::udivmod128+0x173>
  401662:	49 89 cb             	mov    %rcx,%r11
  401665:	89 f1                	mov    %esi,%ecx
  401667:	73 27                	jae    401690 <runtime::udivmod128+0x280>
  401669:	f6 d9                	neg    %cl
  40166b:	48 89 c3             	mov    %rax,%rbx
  40166e:	48 d3 e3             	shl    %cl,%rbx
  401671:	49 89 c6             	mov    %rax,%r14
  401674:	49 89 d2             	mov    %rdx,%r10
  401677:	89 f1                	mov    %esi,%ecx
  401679:	49 d3 ea             	shr    %cl,%r10
  40167c:	49 0f ad d6          	shrd   %cl,%rdx,%r14
  401680:	31 ff                	xor    %edi,%edi
  401682:	48 89 d8             	mov    %rbx,%rax
  401685:	4c 89 f2             	mov    %r14,%rdx
  401688:	4c 89 d9             	mov    %r11,%rcx
  40168b:	e9 f3 fe ff ff       	jmp    401583 <runtime::udivmod128+0x173>
  401690:	f6 d9                	neg    %cl
  401692:	48 89 c7             	mov    %rax,%rdi
  401695:	48 d3 e7             	shl    %cl,%rdi
  401698:	8d 4e c0             	lea    -0x40(%rsi),%ecx
  40169b:	48 0f ad d0          	shrd   %cl,%rdx,%rax
  40169f:	89 f1                	mov    %esi,%ecx
  4016a1:	48 d3 ea             	shr    %cl,%rdx
  4016a4:	45 31 d2             	xor    %r10d,%r10d
  4016a7:	4c 89 d9             	mov    %r11,%rcx
  4016aa:	e9 d4 fe ff ff       	jmp    401583 <runtime::udivmod128+0x173>
  4016af:	90                   	nop

00000000004016b0 <runtime::heap_allocator_proc>:
  4016b0:	55                   	push   %rbp
  4016b1:	41 57                	push   %r15
  4016b3:	41 56                	push   %r14
  4016b5:	41 55                	push   %r13
  4016b7:	41 54                	push   %r12
  4016b9:	53                   	push   %rbx
  4016ba:	50                   	push   %rax
  4016bb:	4c 8b 64 24 48       	mov    0x48(%rsp),%r12
  4016c0:	40 80 fe 07          	cmp    $0x7,%sil
  4016c4:	0f 87 e7 00 00 00    	ja     4017b1 <runtime::heap_allocator_proc+0x101>
  4016ca:	40 0f b6 c6          	movzbl %sil,%eax
  4016ce:	ff 24 c5 20 30 40 00 	jmp    *0x403020(,%rax,8)
  4016d5:	48 83 f9 09          	cmp    $0x9,%rcx
  4016d9:	bb 08 00 00 00       	mov    $0x8,%ebx
  4016de:	48 0f 4d d9          	cmovge %rcx,%rbx
  4016e2:	48 8d 04 1a          	lea    (%rdx,%rbx,1),%rax
  4016e6:	48 83 c0 07          	add    $0x7,%rax
  4016ea:	41 b6 01             	mov    $0x1,%r14b
  4016ed:	48 85 c0             	test   %rax,%rax
  4016f0:	0f 8e 5a 01 00 00    	jle    401850 <runtime::heap_allocator_proc+0x1a0>
  4016f6:	49 89 d7             	mov    %rdx,%r15
  4016f9:	40 84 f6             	test   %sil,%sil
  4016fc:	0f 84 38 01 00 00    	je     40183a <runtime::heap_allocator_proc+0x18a>
  401702:	48 89 c7             	mov    %rax,%rdi
  401705:	e8 66 f9 ff ff       	call   401070 <malloc@plt>
  40170a:	48 85 c0             	test   %rax,%rax
  40170d:	0f 84 3d 01 00 00    	je     401850 <runtime::heap_allocator_proc+0x1a0>
  401713:	4c 89 f9             	mov    %r15,%rcx
  401716:	48 8d 14 18          	lea    (%rax,%rbx,1),%rdx
  40171a:	48 83 c2 07          	add    $0x7,%rdx
  40171e:	48 f7 db             	neg    %rbx
  401721:	48 21 d3             	and    %rdx,%rbx
  401724:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401728:	45 31 f6             	xor    %r14d,%r14d
  40172b:	4d 85 ff             	test   %r15,%r15
  40172e:	49 0f 4e ce          	cmovle %r14,%rcx
  401732:	e9 1d 01 00 00       	jmp    401854 <runtime::heap_allocator_proc+0x1a4>
  401737:	4d 85 c0             	test   %r8,%r8
  40173a:	0f 84 81 00 00 00    	je     4017c1 <runtime::heap_allocator_proc+0x111>
  401740:	bb 08 00 00 00       	mov    $0x8,%ebx
  401745:	48 83 f9 09          	cmp    $0x9,%rcx
  401749:	48 0f 4d d9          	cmovge %rcx,%rbx
  40174d:	48 8d 44 1a 07       	lea    0x7(%rdx,%rbx,1),%rax
  401752:	0f 8c a2 00 00 00    	jl     4017fa <runtime::heap_allocator_proc+0x14a>
  401758:	48 85 c0             	test   %rax,%rax
  40175b:	0f 8e cb 01 00 00    	jle    40192c <runtime::heap_allocator_proc+0x27c>
  401761:	4d 89 c5             	mov    %r8,%r13
  401764:	4d 89 ce             	mov    %r9,%r14
  401767:	49 89 d7             	mov    %rdx,%r15
  40176a:	89 f5                	mov    %esi,%ebp
  40176c:	40 80 fe 03          	cmp    $0x3,%sil
  401770:	0f 85 1e 01 00 00    	jne    401894 <runtime::heap_allocator_proc+0x1e4>
  401776:	bf 01 00 00 00       	mov    $0x1,%edi
  40177b:	48 89 c6             	mov    %rax,%rsi
  40177e:	e8 cd f8 ff ff       	call   401050 <calloc@plt>
  401783:	e9 14 01 00 00       	jmp    40189c <runtime::heap_allocator_proc+0x1ec>
  401788:	0f 57 c0             	xorps  %xmm0,%xmm0
  40178b:	41 0f 11 04 24       	movups %xmm0,(%r12)
  401790:	41 b6 04             	mov    $0x4,%r14b
  401793:	e9 ae 01 00 00       	jmp    401946 <runtime::heap_allocator_proc+0x296>
  401798:	4d 85 c0             	test   %r8,%r8
  40179b:	74 14                	je     4017b1 <runtime::heap_allocator_proc+0x101>
  40179d:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  4017a1:	e8 8a f8 ff ff       	call   401030 <free@plt>
  4017a6:	eb 09                	jmp    4017b1 <runtime::heap_allocator_proc+0x101>
  4017a8:	4d 85 c0             	test   %r8,%r8
  4017ab:	74 04                	je     4017b1 <runtime::heap_allocator_proc+0x101>
  4017ad:	41 c6 00 db          	movb   $0xdb,(%r8)
  4017b1:	0f 57 c0             	xorps  %xmm0,%xmm0
  4017b4:	41 0f 11 04 24       	movups %xmm0,(%r12)
  4017b9:	45 31 f6             	xor    %r14d,%r14d
  4017bc:	e9 85 01 00 00       	jmp    401946 <runtime::heap_allocator_proc+0x296>
  4017c1:	48 83 f9 09          	cmp    $0x9,%rcx
  4017c5:	bb 08 00 00 00       	mov    $0x8,%ebx
  4017ca:	48 0f 4d d9          	cmovge %rcx,%rbx
  4017ce:	48 8d 04 1a          	lea    (%rdx,%rbx,1),%rax
  4017d2:	48 83 c0 07          	add    $0x7,%rax
  4017d6:	41 b6 01             	mov    $0x1,%r14b
  4017d9:	48 85 c0             	test   %rax,%rax
  4017dc:	0f 8e 56 01 00 00    	jle    401938 <runtime::heap_allocator_proc+0x288>
  4017e2:	49 89 d7             	mov    %rdx,%r15
  4017e5:	40 80 fe 03          	cmp    $0x3,%sil
  4017e9:	75 77                	jne    401862 <runtime::heap_allocator_proc+0x1b2>
  4017eb:	bf 01 00 00 00       	mov    $0x1,%edi
  4017f0:	48 89 c6             	mov    %rax,%rsi
  4017f3:	e8 58 f8 ff ff       	call   401050 <calloc@plt>
  4017f8:	eb 70                	jmp    40186a <runtime::heap_allocator_proc+0x1ba>
  4017fa:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  4017fe:	89 f5                	mov    %esi,%ebp
  401800:	48 89 c6             	mov    %rax,%rsi
  401803:	49 89 d6             	mov    %rdx,%r14
  401806:	4d 89 cd             	mov    %r9,%r13
  401809:	4d 89 c7             	mov    %r8,%r15
  40180c:	e8 6f f8 ff ff       	call   401080 <realloc@plt>
  401811:	4d 89 f8             	mov    %r15,%r8
  401814:	48 85 c0             	test   %rax,%rax
  401817:	0f 84 0f 01 00 00    	je     40192c <runtime::heap_allocator_proc+0x27c>
  40181d:	4c 89 ef             	mov    %r13,%rdi
  401820:	4c 89 f2             	mov    %r14,%rdx
  401823:	48 8d 0c 18          	lea    (%rax,%rbx,1),%rcx
  401827:	48 83 c1 07          	add    $0x7,%rcx
  40182b:	48 f7 db             	neg    %rbx
  40182e:	48 21 cb             	and    %rcx,%rbx
  401831:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401835:	e9 ba 00 00 00       	jmp    4018f4 <runtime::heap_allocator_proc+0x244>
  40183a:	bf 01 00 00 00       	mov    $0x1,%edi
  40183f:	48 89 c6             	mov    %rax,%rsi
  401842:	e8 09 f8 ff ff       	call   401050 <calloc@plt>
  401847:	48 85 c0             	test   %rax,%rax
  40184a:	0f 85 c3 fe ff ff    	jne    401713 <runtime::heap_allocator_proc+0x63>
  401850:	31 db                	xor    %ebx,%ebx
  401852:	31 c9                	xor    %ecx,%ecx
  401854:	49 89 1c 24          	mov    %rbx,(%r12)
  401858:	49 89 4c 24 08       	mov    %rcx,0x8(%r12)
  40185d:	e9 e4 00 00 00       	jmp    401946 <runtime::heap_allocator_proc+0x296>
  401862:	48 89 c7             	mov    %rax,%rdi
  401865:	e8 06 f8 ff ff       	call   401070 <malloc@plt>
  40186a:	48 85 c0             	test   %rax,%rax
  40186d:	0f 84 c5 00 00 00    	je     401938 <runtime::heap_allocator_proc+0x288>
  401873:	48 8d 0c 18          	lea    (%rax,%rbx,1),%rcx
  401877:	48 83 c1 07          	add    $0x7,%rcx
  40187b:	48 f7 db             	neg    %rbx
  40187e:	48 21 cb             	and    %rcx,%rbx
  401881:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  401885:	45 31 f6             	xor    %r14d,%r14d
  401888:	4d 85 ff             	test   %r15,%r15
  40188b:	4d 0f 4e fe          	cmovle %r14,%r15
  40188f:	e9 a9 00 00 00       	jmp    40193d <runtime::heap_allocator_proc+0x28d>
  401894:	48 89 c7             	mov    %rax,%rdi
  401897:	e8 d4 f7 ff ff       	call   401070 <malloc@plt>
  40189c:	48 85 c0             	test   %rax,%rax
  40189f:	4d 89 e8             	mov    %r13,%r8
  4018a2:	0f 84 84 00 00 00    	je     40192c <runtime::heap_allocator_proc+0x27c>
  4018a8:	4c 89 fa             	mov    %r15,%rdx
  4018ab:	48 8d 34 18          	lea    (%rax,%rbx,1),%rsi
  4018af:	48 83 c6 07          	add    $0x7,%rsi
  4018b3:	48 f7 db             	neg    %rbx
  4018b6:	48 21 f3             	and    %rsi,%rbx
  4018b9:	48 89 43 f8          	mov    %rax,-0x8(%rbx)
  4018bd:	4d 39 fe             	cmp    %r15,%r14
  4018c0:	49 0f 4c d6          	cmovl  %r14,%rdx
  4018c4:	49 39 d8             	cmp    %rbx,%r8
  4018c7:	0f 95 c0             	setne  %al
  4018ca:	48 85 d2             	test   %rdx,%rdx
  4018cd:	0f 9f c1             	setg   %cl
  4018d0:	20 c1                	and    %al,%cl
  4018d2:	80 f9 01             	cmp    $0x1,%cl
  4018d5:	75 0e                	jne    4018e5 <runtime::heap_allocator_proc+0x235>
  4018d7:	48 89 df             	mov    %rbx,%rdi
  4018da:	4c 89 c6             	mov    %r8,%rsi
  4018dd:	e8 7e f7 ff ff       	call   401060 <memcpy@plt>
  4018e2:	4d 89 e8             	mov    %r13,%r8
  4018e5:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  4018e9:	e8 42 f7 ff ff       	call   401030 <free@plt>
  4018ee:	4c 89 fa             	mov    %r15,%rdx
  4018f1:	4c 89 f7             	mov    %r14,%rdi
  4018f4:	40 80 fd 03          	cmp    $0x3,%bpl
  4018f8:	0f 94 c0             	sete   %al
  4018fb:	45 31 f6             	xor    %r14d,%r14d
  4018fe:	48 85 d2             	test   %rdx,%rdx
  401901:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  401907:	4c 0f 4f fa          	cmovg  %rdx,%r15
  40190b:	48 29 fa             	sub    %rdi,%rdx
  40190e:	0f 9f c1             	setg   %cl
  401911:	20 c1                	and    %al,%cl
  401913:	80 f9 01             	cmp    $0x1,%cl
  401916:	75 25                	jne    40193d <runtime::heap_allocator_proc+0x28d>
  401918:	48 85 ff             	test   %rdi,%rdi
  40191b:	78 3b                	js     401958 <runtime::heap_allocator_proc+0x2a8>
  40191d:	48 01 df             	add    %rbx,%rdi
  401920:	45 31 f6             	xor    %r14d,%r14d
  401923:	31 f6                	xor    %esi,%esi
  401925:	e8 16 f7 ff ff       	call   401040 <memset@plt>
  40192a:	eb 11                	jmp    40193d <runtime::heap_allocator_proc+0x28d>
  40192c:	49 8b 78 f8          	mov    -0x8(%r8),%rdi
  401930:	e8 fb f6 ff ff       	call   401030 <free@plt>
  401935:	41 b6 01             	mov    $0x1,%r14b
  401938:	31 db                	xor    %ebx,%ebx
  40193a:	45 31 ff             	xor    %r15d,%r15d
  40193d:	49 89 1c 24          	mov    %rbx,(%r12)
  401941:	4d 89 7c 24 08       	mov    %r15,0x8(%r12)
  401946:	44 89 f0             	mov    %r14d,%eax
  401949:	48 83 c4 08          	add    $0x8,%rsp
  40194d:	5b                   	pop    %rbx
  40194e:	41 5c                	pop    %r12
  401950:	41 5d                	pop    %r13
  401952:	41 5e                	pop    %r14
  401954:	41 5f                	pop    %r15
  401956:	5d                   	pop    %rbp
  401957:	c3                   	ret
  401958:	4c 89 fe             	mov    %r15,%rsi
  40195b:	4c 89 fa             	mov    %r15,%rdx
  40195e:	e8 3d f7 ff ff       	call   4010a0 <runtime::slice_handle_error>
  401963:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40196a:	84 00 00 00 00 00 

0000000000401970 <runtime::default_random_generator_proc>:
  401970:	53                   	push   %rbx
  401971:	48 83 ec 10          	sub    $0x10,%rsp
  401975:	49 89 c8             	mov    %rcx,%r8
  401978:	64 48 8b 1c 25 00 00 	mov    %fs:0x0,%rbx
  40197f:	00 00 
  401981:	48 8d 9b e8 ff ff ff 	lea    -0x18(%rbx),%rbx
  401988:	48 85 ff             	test   %rdi,%rdi
  40198b:	48 0f 45 df          	cmovne %rdi,%rbx
  40198f:	48 83 fe 02          	cmp    $0x2,%rsi
  401993:	0f 84 f0 00 00 00    	je     401a89 <runtime::default_random_generator_proc+0x119>
  401999:	48 83 fe 01          	cmp    $0x1,%rsi
  40199d:	74 6e                	je     401a0d <runtime::default_random_generator_proc+0x9d>
  40199f:	48 85 f6             	test   %rsi,%rsi
  4019a2:	0f 85 af 01 00 00    	jne    401b57 <runtime::default_random_generator_proc+0x1e7>
  4019a8:	48 8b 03             	mov    (%rbx),%rax
  4019ab:	48 85 c0             	test   %rax,%rax
  4019ae:	75 0d                	jne    4019bd <runtime::default_random_generator_proc+0x4d>
  4019b0:	48 83 7b 08 00       	cmpq   $0x0,0x8(%rbx)
  4019b5:	0f 84 e0 00 00 00    	je     401a9b <runtime::default_random_generator_proc+0x12b>
  4019bb:	31 c0                	xor    %eax,%eax
  4019bd:	49 83 f8 08          	cmp    $0x8,%r8
  4019c1:	0f 85 1b 01 00 00    	jne    401ae2 <runtime::default_random_generator_proc+0x172>
  4019c7:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  4019ce:	f4 51 58 
  4019d1:	48 0f af c8          	imul   %rax,%rcx
  4019d5:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  4019d9:	48 83 ce 01          	or     $0x1,%rsi
  4019dd:	48 01 ce             	add    %rcx,%rsi
  4019e0:	48 89 33             	mov    %rsi,(%rbx)
  4019e3:	48 89 c1             	mov    %rax,%rcx
  4019e6:	48 c1 e9 3b          	shr    $0x3b,%rcx
  4019ea:	48 89 ce             	mov    %rcx,%rsi
  4019ed:	48 83 c6 05          	add    $0x5,%rsi
  4019f1:	48 31 c6             	xor    %rax,%rsi
  4019f4:	48 b8 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rax
  4019fb:	75 f1 ae 
  4019fe:	48 0f af c6          	imul   %rsi,%rax
  401a02:	48 d3 c8             	ror    %cl,%rax
  401a05:	48 89 02             	mov    %rax,(%rdx)
  401a08:	e9 4a 01 00 00       	jmp    401b57 <runtime::default_random_generator_proc+0x1e7>
  401a0d:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  401a14:	00 00 
  401a16:	48 85 d2             	test   %rdx,%rdx
  401a19:	0f 95 c0             	setne  %al
  401a1c:	4d 85 c0             	test   %r8,%r8
  401a1f:	0f 9f c1             	setg   %cl
  401a22:	20 c1                	and    %al,%cl
  401a24:	80 f9 01             	cmp    $0x1,%cl
  401a27:	75 27                	jne    401a50 <runtime::default_random_generator_proc+0xe0>
  401a29:	49 83 f8 08          	cmp    $0x8,%r8
  401a2d:	b8 08 00 00 00       	mov    $0x8,%eax
  401a32:	49 0f 42 c0          	cmovb  %r8,%rax
  401a36:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  401a3b:	48 89 d6             	mov    %rdx,%rsi
  401a3e:	48 89 c2             	mov    %rax,%rdx
  401a41:	e8 1a f6 ff ff       	call   401060 <memcpy@plt>
  401a46:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401a4b:	48 85 d2             	test   %rdx,%rdx
  401a4e:	75 09                	jne    401a59 <runtime::default_random_generator_proc+0xe9>
  401a50:	0f 31                	rdtsc
  401a52:	48 c1 e2 20          	shl    $0x20,%rdx
  401a56:	48 09 c2             	or     %rax,%rdx
  401a59:	48 8d 04 55 01 00 00 	lea    0x1(,%rdx,2),%rax
  401a60:	00 
  401a61:	48 89 43 08          	mov    %rax,0x8(%rbx)
  401a65:	48 8d 04 52          	lea    (%rdx,%rdx,2),%rax
  401a69:	48 ff c0             	inc    %rax
  401a6c:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  401a73:	f4 51 58 
  401a76:	48 0f af c8          	imul   %rax,%rcx
  401a7a:	48 8d 04 51          	lea    (%rcx,%rdx,2),%rax
  401a7e:	48 ff c0             	inc    %rax
  401a81:	48 89 03             	mov    %rax,(%rbx)
  401a84:	e9 ce 00 00 00       	jmp    401b57 <runtime::default_random_generator_proc+0x1e7>
  401a89:	49 83 f8 04          	cmp    $0x4,%r8
  401a8d:	0f 85 c4 00 00 00    	jne    401b57 <runtime::default_random_generator_proc+0x1e7>
  401a93:	80 0a 0a             	orb    $0xa,(%rdx)
  401a96:	e9 bc 00 00 00       	jmp    401b57 <runtime::default_random_generator_proc+0x1e7>
  401a9b:	48 89 d6             	mov    %rdx,%rsi
  401a9e:	0f 31                	rdtsc
  401aa0:	48 89 d1             	mov    %rdx,%rcx
  401aa3:	48 89 f2             	mov    %rsi,%rdx
  401aa6:	48 c1 e1 20          	shl    $0x20,%rcx
  401aaa:	48 09 c1             	or     %rax,%rcx
  401aad:	48 8d 04 4d 01 00 00 	lea    0x1(,%rcx,2),%rax
  401ab4:	00 
  401ab5:	48 89 43 08          	mov    %rax,0x8(%rbx)
  401ab9:	48 8d 04 49          	lea    (%rcx,%rcx,2),%rax
  401abd:	48 ff c0             	inc    %rax
  401ac0:	48 be 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rsi
  401ac7:	f4 51 58 
  401aca:	48 0f af f0          	imul   %rax,%rsi
  401ace:	48 8d 04 4e          	lea    (%rsi,%rcx,2),%rax
  401ad2:	48 ff c0             	inc    %rax
  401ad5:	48 89 03             	mov    %rax,(%rbx)
  401ad8:	49 83 f8 08          	cmp    $0x8,%r8
  401adc:	0f 84 e5 fe ff ff    	je     4019c7 <runtime::default_random_generator_proc+0x57>
  401ae2:	4d 85 c0             	test   %r8,%r8
  401ae5:	7e 70                	jle    401b57 <runtime::default_random_generator_proc+0x1e7>
  401ae7:	48 b8 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rax
  401aee:	75 f1 ae 
  401af1:	48 be 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rsi
  401af8:	f4 51 58 
  401afb:	31 ff                	xor    %edi,%edi
  401afd:	31 c9                	xor    %ecx,%ecx
  401aff:	45 31 c9             	xor    %r9d,%r9d
  401b02:	eb 1e                	jmp    401b22 <runtime::default_random_generator_proc+0x1b2>
  401b04:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  401b0b:	00 00 00 00 00 
  401b10:	44 88 0c 3a          	mov    %r9b,(%rdx,%rdi,1)
  401b14:	49 c1 e9 08          	shr    $0x8,%r9
  401b18:	fe c9                	dec    %cl
  401b1a:	48 ff c7             	inc    %rdi
  401b1d:	49 39 f8             	cmp    %rdi,%r8
  401b20:	74 35                	je     401b57 <runtime::default_random_generator_proc+0x1e7>
  401b22:	84 c9                	test   %cl,%cl
  401b24:	75 ea                	jne    401b10 <runtime::default_random_generator_proc+0x1a0>
  401b26:	4c 8b 13             	mov    (%rbx),%r10
  401b29:	4c 8b 5b 08          	mov    0x8(%rbx),%r11
  401b2d:	4c 89 d1             	mov    %r10,%rcx
  401b30:	48 c1 e9 3b          	shr    $0x3b,%rcx
  401b34:	49 89 c9             	mov    %rcx,%r9
  401b37:	49 83 c1 05          	add    $0x5,%r9
  401b3b:	4d 31 d1             	xor    %r10,%r9
  401b3e:	4c 0f af d6          	imul   %rsi,%r10
  401b42:	49 83 cb 01          	or     $0x1,%r11
  401b46:	4d 01 d3             	add    %r10,%r11
  401b49:	4c 89 1b             	mov    %r11,(%rbx)
  401b4c:	4c 0f af c8          	imul   %rax,%r9
  401b50:	49 d3 c9             	ror    %cl,%r9
  401b53:	b1 07                	mov    $0x7,%cl
  401b55:	eb b9                	jmp    401b10 <runtime::default_random_generator_proc+0x1a0>
  401b57:	48 83 c4 10          	add    $0x10,%rsp
  401b5b:	5b                   	pop    %rbx
  401b5c:	c3                   	ret
  401b5d:	0f 1f 00             	nopl   (%rax)

0000000000401b60 <runtime::default_temp_allocator_proc>:
  401b60:	41 57                	push   %r15
  401b62:	41 56                	push   %r14
  401b64:	41 55                	push   %r13
  401b66:	41 54                	push   %r12
  401b68:	53                   	push   %rbx
  401b69:	48 83 ec 10          	sub    $0x10,%rsp
  401b6d:	40 80 fe 07          	cmp    $0x7,%sil
  401b71:	0f 87 ab 01 00 00    	ja     401d22 <runtime::default_temp_allocator_proc+0x1c2>
  401b77:	48 89 fb             	mov    %rdi,%rbx
  401b7a:	4c 8b 74 24 50       	mov    0x50(%rsp),%r14
  401b7f:	4c 8b 7c 24 40       	mov    0x40(%rsp),%r15
  401b84:	40 0f b6 c6          	movzbl %sil,%eax
  401b88:	ff 24 c5 60 30 40 00 	jmp    *0x403060(,%rax,8)
  401b8f:	b0 04                	mov    $0x4,%al
  401b91:	45 31 c0             	xor    %r8d,%r8d
  401b94:	31 d2                	xor    %edx,%edx
  401b96:	e9 8e 01 00 00       	jmp    401d29 <runtime::default_temp_allocator_proc+0x1c9>
  401b9b:	4d 85 c0             	test   %r8,%r8
  401b9e:	74 31                	je     401bd1 <runtime::default_temp_allocator_proc+0x71>
  401ba0:	4c 39 ca             	cmp    %r9,%rdx
  401ba3:	0f 85 d3 00 00 00    	jne    401c7c <runtime::default_temp_allocator_proc+0x11c>
  401ba9:	48 85 d2             	test   %rdx,%rdx
  401bac:	0f 89 75 01 00 00    	jns    401d27 <runtime::default_temp_allocator_proc+0x1c7>
  401bb2:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  401bb7:	be 3e 00 00 00       	mov    $0x3e,%esi
  401bbc:	49 89 d1             	mov    %rdx,%r9
  401bbf:	ba d1 00 00 00       	mov    $0xd1,%edx
  401bc4:	b9 13 00 00 00       	mov    $0x13,%ecx
  401bc9:	45 31 c0             	xor    %r8d,%r8d
  401bcc:	e8 6f f5 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  401bd1:	0f 57 c0             	xorps  %xmm0,%xmm0
  401bd4:	0f 29 04 24          	movaps %xmm0,(%rsp)
  401bd8:	49 89 e0             	mov    %rsp,%r8
  401bdb:	48 89 df             	mov    %rbx,%rdi
  401bde:	48 89 d6             	mov    %rdx,%rsi
  401be1:	48 89 ca             	mov    %rcx,%rdx
  401be4:	4c 89 f9             	mov    %r15,%rcx
  401be7:	4d 89 f1             	mov    %r14,%r9
  401bea:	e8 81 03 00 00       	call   401f70 <runtime::arena_alloc>
  401bef:	4c 8b 04 24          	mov    (%rsp),%r8
  401bf3:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401bf8:	e9 2c 01 00 00       	jmp    401d29 <runtime::default_temp_allocator_proc+0x1c9>
  401bfd:	4c 8b 43 10          	mov    0x10(%rbx),%r8
  401c01:	4d 85 c0             	test   %r8,%r8
  401c04:	0f 84 10 01 00 00    	je     401d1a <runtime::default_temp_allocator_proc+0x1ba>
  401c0a:	49 89 e4             	mov    %rsp,%r12
  401c0d:	eb 0d                	jmp    401c1c <runtime::default_temp_allocator_proc+0xbc>
  401c0f:	90                   	nop
  401c10:	49 89 c8             	mov    %rcx,%r8
  401c13:	48 85 c9             	test   %rcx,%rcx
  401c16:	0f 84 fe 00 00 00    	je     401d1a <runtime::default_temp_allocator_proc+0x1ba>
  401c1c:	49 8b 08             	mov    (%r8),%rcx
  401c1f:	48 85 c9             	test   %rcx,%rcx
  401c22:	0f 84 d7 00 00 00    	je     401cff <runtime::default_temp_allocator_proc+0x19f>
  401c28:	48 89 4b 10          	mov    %rcx,0x10(%rbx)
  401c2c:	49 8b 40 28          	mov    0x28(%r8),%rax
  401c30:	48 29 43 20          	sub    %rax,0x20(%rbx)
  401c34:	49 8b 40 08          	mov    0x8(%r8),%rax
  401c38:	48 85 c0             	test   %rax,%rax
  401c3b:	74 d3                	je     401c10 <runtime::default_temp_allocator_proc+0xb0>
  401c3d:	49 8b 78 10          	mov    0x10(%r8),%rdi
  401c41:	0f 57 c0             	xorps  %xmm0,%xmm0
  401c44:	0f 29 04 24          	movaps %xmm0,(%rsp)
  401c48:	48 83 ec 08          	sub    $0x8,%rsp
  401c4c:	be 01 00 00 00       	mov    $0x1,%esi
  401c51:	31 d2                	xor    %edx,%edx
  401c53:	31 c9                	xor    %ecx,%ecx
  401c55:	45 31 c9             	xor    %r9d,%r9d
  401c58:	41 56                	push   %r14
  401c5a:	41 54                	push   %r12
  401c5c:	41 57                	push   %r15
  401c5e:	ff d0                	call   *%rax
  401c60:	48 83 c4 20          	add    $0x20,%rsp
  401c64:	48 8b 4b 10          	mov    0x10(%rbx),%rcx
  401c68:	eb a6                	jmp    401c10 <runtime::default_temp_allocator_proc+0xb0>
  401c6a:	4d 85 c0             	test   %r8,%r8
  401c6d:	0f 84 af 00 00 00    	je     401d22 <runtime::default_temp_allocator_proc+0x1c2>
  401c73:	41 c6 00 5d          	movb   $0x5d,(%r8)
  401c77:	e9 a6 00 00 00       	jmp    401d22 <runtime::default_temp_allocator_proc+0x1c2>
  401c7c:	48 85 d2             	test   %rdx,%rdx
  401c7f:	0f 84 0a ff ff ff    	je     401b8f <runtime::default_temp_allocator_proc+0x2f>
  401c85:	48 8d 41 ff          	lea    -0x1(%rcx),%rax
  401c89:	4c 85 c0             	test   %r8,%rax
  401c8c:	0f 84 b1 00 00 00    	je     401d43 <runtime::default_temp_allocator_proc+0x1e3>
  401c92:	4d 89 cd             	mov    %r9,%r13
  401c95:	4d 89 c4             	mov    %r8,%r12
  401c98:	0f 57 c0             	xorps  %xmm0,%xmm0
  401c9b:	0f 29 04 24          	movaps %xmm0,(%rsp)
  401c9f:	49 89 e0             	mov    %rsp,%r8
  401ca2:	48 89 df             	mov    %rbx,%rdi
  401ca5:	48 89 d6             	mov    %rdx,%rsi
  401ca8:	48 89 ca             	mov    %rcx,%rdx
  401cab:	4c 89 f9             	mov    %r15,%rcx
  401cae:	4d 89 f1             	mov    %r14,%r9
  401cb1:	e8 ba 02 00 00       	call   401f70 <runtime::arena_alloc>
  401cb6:	84 c0                	test   %al,%al
  401cb8:	0f 85 d3 fe ff ff    	jne    401b91 <runtime::default_temp_allocator_proc+0x31>
  401cbe:	48 8b 1c 24          	mov    (%rsp),%rbx
  401cc2:	48 85 db             	test   %rbx,%rbx
  401cc5:	74 5b                	je     401d22 <runtime::default_temp_allocator_proc+0x1c2>
  401cc7:	4d 89 e9             	mov    %r13,%r9
  401cca:	4d 85 ed             	test   %r13,%r13
  401ccd:	0f 88 eb 00 00 00    	js     401dbe <runtime::default_temp_allocator_proc+0x25e>
  401cd3:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401cd8:	4c 39 ca             	cmp    %r9,%rdx
  401cdb:	4c 0f 4c ca          	cmovl  %rdx,%r9
  401cdf:	4d 85 c9             	test   %r9,%r9
  401ce2:	7e 14                	jle    401cf8 <runtime::default_temp_allocator_proc+0x198>
  401ce4:	4c 89 e6             	mov    %r12,%rsi
  401ce7:	48 89 df             	mov    %rbx,%rdi
  401cea:	49 89 d6             	mov    %rdx,%r14
  401ced:	4c 89 ca             	mov    %r9,%rdx
  401cf0:	e8 9b f3 ff ff       	call   401090 <memmove@plt>
  401cf5:	4c 89 f2             	mov    %r14,%rdx
  401cf8:	31 c0                	xor    %eax,%eax
  401cfa:	49 89 d8             	mov    %rbx,%r8
  401cfd:	eb 2a                	jmp    401d29 <runtime::default_temp_allocator_proc+0x1c9>
  401cff:	49 8b 78 18          	mov    0x18(%r8),%rdi
  401d03:	49 8b 50 20          	mov    0x20(%r8),%rdx
  401d07:	31 f6                	xor    %esi,%esi
  401d09:	e8 32 f3 ff ff       	call   401040 <memset@plt>
  401d0e:	48 8b 43 10          	mov    0x10(%rbx),%rax
  401d12:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  401d19:	00 
  401d1a:	48 c7 43 18 00 00 00 	movq   $0x0,0x18(%rbx)
  401d21:	00 
  401d22:	45 31 c0             	xor    %r8d,%r8d
  401d25:	31 d2                	xor    %edx,%edx
  401d27:	31 c0                	xor    %eax,%eax
  401d29:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401d2e:	4c 89 01             	mov    %r8,(%rcx)
  401d31:	48 89 51 08          	mov    %rdx,0x8(%rcx)
  401d35:	48 83 c4 10          	add    $0x10,%rsp
  401d39:	5b                   	pop    %rbx
  401d3a:	41 5c                	pop    %r12
  401d3c:	41 5d                	pop    %r13
  401d3e:	41 5e                	pop    %r14
  401d40:	41 5f                	pop    %r15
  401d42:	c3                   	ret
  401d43:	4c 39 ca             	cmp    %r9,%rdx
  401d46:	73 24                	jae    401d6c <runtime::default_temp_allocator_proc+0x20c>
  401d48:	48 85 d2             	test   %rdx,%rdx
  401d4b:	79 da                	jns    401d27 <runtime::default_temp_allocator_proc+0x1c7>
  401d4d:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  401d52:	be 3e 00 00 00       	mov    $0x3e,%esi
  401d57:	49 89 d1             	mov    %rdx,%r9
  401d5a:	ba d9 00 00 00       	mov    $0xd9,%edx
  401d5f:	b9 14 00 00 00       	mov    $0x14,%ecx
  401d64:	45 31 c0             	xor    %r8d,%r8d
  401d67:	e8 d4 f3 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  401d6c:	48 8b 7b 10          	mov    0x10(%rbx),%rdi
  401d70:	48 85 ff             	test   %rdi,%rdi
  401d73:	0f 84 19 ff ff ff    	je     401c92 <runtime::default_temp_allocator_proc+0x132>
  401d79:	48 8b 77 18          	mov    0x18(%rdi),%rsi
  401d7d:	4c 89 c0             	mov    %r8,%rax
  401d80:	48 29 f0             	sub    %rsi,%rax
  401d83:	4e 8d 14 08          	lea    (%rax,%r9,1),%r10
  401d87:	4c 39 d0             	cmp    %r10,%rax
  401d8a:	0f 83 02 ff ff ff    	jae    401c92 <runtime::default_temp_allocator_proc+0x132>
  401d90:	4c 3b 57 20          	cmp    0x20(%rdi),%r10
  401d94:	0f 85 f8 fe ff ff    	jne    401c92 <runtime::default_temp_allocator_proc+0x132>
  401d9a:	4c 8d 14 10          	lea    (%rax,%rdx,1),%r10
  401d9e:	4c 3b 57 28          	cmp    0x28(%rdi),%r10
  401da2:	0f 87 ea fe ff ff    	ja     401c92 <runtime::default_temp_allocator_proc+0x132>
  401da8:	4c 89 57 20          	mov    %r10,0x20(%rdi)
  401dac:	4c 39 d0             	cmp    %r10,%rax
  401daf:	7f 29                	jg     401dda <runtime::default_temp_allocator_proc+0x27a>
  401db1:	48 01 c6             	add    %rax,%rsi
  401db4:	31 c0                	xor    %eax,%eax
  401db6:	49 89 f0             	mov    %rsi,%r8
  401db9:	e9 6b ff ff ff       	jmp    401d29 <runtime::default_temp_allocator_proc+0x1c9>
  401dbe:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  401dc3:	be 3e 00 00 00       	mov    $0x3e,%esi
  401dc8:	ba ee 00 00 00       	mov    $0xee,%edx
  401dcd:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  401dd2:	45 31 c0             	xor    %r8d,%r8d
  401dd5:	e8 66 f3 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  401dda:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  401ddf:	be 3e 00 00 00       	mov    $0x3e,%esi
  401de4:	ba e4 00 00 00       	mov    $0xe4,%edx
  401de9:	b9 17 00 00 00       	mov    $0x17,%ecx
  401dee:	49 89 c0             	mov    %rax,%r8
  401df1:	4d 89 d1             	mov    %r10,%r9
  401df4:	e8 47 f3 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  401df9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401e00 <main>:
  401e00:	41 57                	push   %r15
  401e02:	41 56                	push   %r14
  401e04:	53                   	push   %rbx
  401e05:	48 81 ec 80 00 00 00 	sub    $0x80,%rsp
  401e0c:	4c 63 cf             	movslq %edi,%r9
  401e0f:	45 85 c9             	test   %r9d,%r9d
  401e12:	0f 88 38 01 00 00    	js     401f50 <main+0x150>
  401e18:	48 89 35 31 32 00 00 	mov    %rsi,0x3231(%rip)        # 405050 <runtime::args__>
  401e1f:	4c 89 0d 32 32 00 00 	mov    %r9,0x3232(%rip)        # 405058 <runtime::args__+0x8>
  401e26:	48 c7 44 24 10 b0 16 	movq   $0x4016b0,0x10(%rsp)
  401e2d:	40 00 
  401e2f:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  401e36:	00 00 
  401e38:	48 c7 44 24 20 60 1b 	movq   $0x401b60,0x20(%rsp)
  401e3f:	40 00 
  401e41:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  401e48:	00 00 
  401e4a:	48 8d 80 b0 ff ff ff 	lea    -0x50(%rax),%rax
  401e51:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401e56:	48 c7 44 24 30 b0 11 	movq   $0x4011b0,0x30(%rsp)
  401e5d:	40 00 
  401e5f:	48 c7 44 24 38 80 25 	movq   $0x402580,0x38(%rsp)
  401e66:	40 00 
  401e68:	0f 57 c0             	xorps  %xmm0,%xmm0
  401e6b:	0f 11 44 24 40       	movups %xmm0,0x40(%rsp)
  401e70:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  401e77:	00 00 
  401e79:	48 c7 44 24 58 70 19 	movq   $0x401970,0x58(%rsp)
  401e80:	40 00 
  401e82:	0f 11 44 24 60       	movups %xmm0,0x60(%rsp)
  401e87:	0f 11 44 24 70       	movups %xmm0,0x70(%rsp)
  401e8c:	e8 8f f4 ff ff       	call   401320 <__$startup_runtime>
  401e91:	b8 09 00 00 00       	mov    $0x9,%eax
  401e96:	be 00 10 00 00       	mov    $0x1000,%esi
  401e9b:	ba 03 00 00 00       	mov    $0x3,%edx
  401ea0:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401ea6:	31 ff                	xor    %edi,%edi
  401ea8:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401eaf:	45 31 c9             	xor    %r9d,%r9d
  401eb2:	0f 05                	syscall
  401eb4:	48 89 c3             	mov    %rax,%rbx
  401eb7:	b8 09 00 00 00       	mov    $0x9,%eax
  401ebc:	31 ff                	xor    %edi,%edi
  401ebe:	45 31 c9             	xor    %r9d,%r9d
  401ec1:	0f 05                	syscall
  401ec3:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  401eca:	48 89 1c 24          	mov    %rbx,(%rsp)
  401ece:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  401ed3:	b8 09 00 00 00       	mov    $0x9,%eax
  401ed8:	31 ff                	xor    %edi,%edi
  401eda:	45 31 c9             	xor    %r9d,%r9d
  401edd:	0f 05                	syscall
  401edf:	48 89 03             	mov    %rax,(%rbx)
  401ee2:	48 c7 43 08 00 00 00 	movq   $0x0,0x8(%rbx)
  401ee9:	00 
  401eea:	48 c7 43 10 c8 00 00 	movq   $0xc8,0x10(%rbx)
  401ef1:	00 
  401ef2:	48 c7 43 18 08 00 00 	movq   $0x8,0x18(%rbx)
  401ef9:	00 
  401efa:	49 89 e6             	mov    %rsp,%r14
  401efd:	4c 89 f7             	mov    %r14,%rdi
  401f00:	e8 d2 05 00 00       	call   4024d7 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)>
  401f05:	49 89 c7             	mov    %rax,%r15
  401f08:	4c 89 f7             	mov    %r14,%rdi
  401f0b:	e8 1a 06 00 00       	call   40252a <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)>
  401f10:	48 8b 0b             	mov    (%rbx),%rcx
  401f13:	48 8b 53 08          	mov    0x8(%rbx),%rdx
  401f17:	48 c1 e2 04          	shl    $0x4,%rdx
  401f1b:	44 89 fe             	mov    %r15d,%esi
  401f1e:	48 89 34 0a          	mov    %rsi,(%rdx,%rcx,1)
  401f22:	89 c6                	mov    %eax,%esi
  401f24:	48 89 74 0a 08       	mov    %rsi,0x8(%rdx,%rcx,1)
  401f29:	48 c1 e8 20          	shr    $0x20,%rax
  401f2d:	48 89 44 0a 10       	mov    %rax,0x10(%rdx,%rcx,1)
  401f32:	48 83 43 08 02       	addq   $0x2,0x8(%rbx)
  401f37:	48 8d 7c 24 10       	lea    0x10(%rsp),%rdi
  401f3c:	e8 ef f3 ff ff       	call   401330 <__$cleanup_runtime>
  401f41:	31 c0                	xor    %eax,%eax
  401f43:	48 81 c4 80 00 00 00 	add    $0x80,%rsp
  401f4a:	5b                   	pop    %rbx
  401f4b:	41 5e                	pop    %r14
  401f4d:	41 5f                	pop    %r15
  401f4f:	c3                   	ret
  401f50:	bf 38 32 40 00       	mov    $0x403238,%edi
  401f55:	be 2c 00 00 00       	mov    $0x2c,%esi
  401f5a:	ba 36 00 00 00       	mov    $0x36,%edx
  401f5f:	b9 11 00 00 00       	mov    $0x11,%ecx
  401f64:	45 31 c0             	xor    %r8d,%r8d
  401f67:	e8 d4 f1 ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  401f6c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401f70 <runtime::arena_alloc>:
  401f70:	55                   	push   %rbp
  401f71:	41 57                	push   %r15
  401f73:	41 56                	push   %r14
  401f75:	41 55                	push   %r13
  401f77:	41 54                	push   %r12
  401f79:	53                   	push   %rbx
  401f7a:	48 83 ec 28          	sub    $0x28,%rsp
  401f7e:	4c 8d 62 ff          	lea    -0x1(%rdx),%r12
  401f82:	4c 85 e2             	test   %r12,%rdx
  401f85:	0f 85 4b 02 00 00    	jne    4021d6 <runtime::arena_alloc+0x266>
  401f8b:	48 85 f6             	test   %rsi,%rsi
  401f8e:	0f 84 80 00 00 00    	je     402014 <runtime::arena_alloc+0xa4>
  401f94:	48 8b 47 10          	mov    0x10(%rdi),%rax
  401f98:	48 85 c0             	test   %rax,%rax
  401f9b:	74 35                	je     401fd2 <runtime::arena_alloc+0x62>
  401f9d:	4c 8b 70 20          	mov    0x20(%rax),%r14
  401fa1:	4c 8b 78 18          	mov    0x18(%rax),%r15
  401fa5:	4d 01 f7             	add    %r14,%r15
  401fa8:	4d 89 e3             	mov    %r12,%r11
  401fab:	4d 21 fb             	and    %r15,%r11
  401fae:	49 89 d2             	mov    %rdx,%r10
  401fb1:	4d 29 da             	sub    %r11,%r10
  401fb4:	4d 85 db             	test   %r11,%r11
  401fb7:	4d 0f 44 d3          	cmove  %r11,%r10
  401fbb:	49 89 f3             	mov    %rsi,%r11
  401fbe:	4d 01 d3             	add    %r10,%r11
  401fc1:	72 0f                	jb     401fd2 <runtime::arena_alloc+0x62>
  401fc3:	4d 01 f3             	add    %r14,%r11
  401fc6:	72 0a                	jb     401fd2 <runtime::arena_alloc+0x62>
  401fc8:	4c 3b 58 28          	cmp    0x28(%rax),%r11
  401fcc:	0f 86 d2 01 00 00    	jbe    4021a4 <runtime::arena_alloc+0x234>
  401fd2:	4c 8b 57 28          	mov    0x28(%rdi),%r10
  401fd6:	4d 85 d2             	test   %r10,%r10
  401fd9:	75 0e                	jne    401fe9 <runtime::arena_alloc+0x79>
  401fdb:	48 c7 47 28 00 00 40 	movq   $0x400000,0x28(%rdi)
  401fe2:	00 
  401fe3:	41 ba 00 00 40 00    	mov    $0x400000,%r10d
  401fe9:	4d 89 e3             	mov    %r12,%r11
  401fec:	49 21 f3             	and    %rsi,%r11
  401fef:	48 89 d0             	mov    %rdx,%rax
  401ff2:	4c 29 d8             	sub    %r11,%rax
  401ff5:	4d 85 db             	test   %r11,%r11
  401ff8:	49 0f 44 c3          	cmove  %r11,%rax
  401ffc:	48 01 f0             	add    %rsi,%rax
  401fff:	4c 39 d0             	cmp    %r10,%rax
  402002:	49 0f 46 c2          	cmovbe %r10,%rax
  402006:	4c 8b 2f             	mov    (%rdi),%r13
  402009:	4d 85 ed             	test   %r13,%r13
  40200c:	74 14                	je     402022 <runtime::arena_alloc+0xb2>
  40200e:	4c 8b 7f 08          	mov    0x8(%rdi),%r15
  402012:	eb 26                	jmp    40203a <runtime::arena_alloc+0xca>
  402014:	0f 57 c0             	xorps  %xmm0,%xmm0
  402017:	41 0f 11 00          	movups %xmm0,(%r8)
  40201b:	31 c0                	xor    %eax,%eax
  40201d:	e9 46 01 00 00       	jmp    402168 <runtime::arena_alloc+0x1f8>
  402022:	48 c7 07 b0 16 40 00 	movq   $0x4016b0,(%rdi)
  402029:	41 bd b0 16 40 00    	mov    $0x4016b0,%r13d
  40202f:	48 c7 47 08 00 00 00 	movq   $0x0,0x8(%rdi)
  402036:	00 
  402037:	45 31 ff             	xor    %r15d,%r15d
  40203a:	48 83 fa 31          	cmp    $0x31,%rdx
  40203e:	bd 30 00 00 00       	mov    $0x30,%ebp
  402043:	48 0f 43 ea          	cmovae %rdx,%rbp
  402047:	48 83 fa 11          	cmp    $0x11,%rdx
  40204b:	41 bb 10 00 00 00    	mov    $0x10,%r11d
  402051:	4c 0f 4d da          	cmovge %rdx,%r11
  402055:	4d 8d 53 ff          	lea    -0x1(%r11),%r10
  402059:	4d 85 d3             	test   %r10,%r11
  40205c:	0f 85 9f 01 00 00    	jne    402201 <runtime::arena_alloc+0x291>
  402062:	48 01 e8             	add    %rbp,%rax
  402065:	74 54                	je     4020bb <runtime::arena_alloc+0x14b>
  402067:	4d 89 ce             	mov    %r9,%r14
  40206a:	4c 89 c3             	mov    %r8,%rbx
  40206d:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  402072:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402077:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40207c:	0f 57 c0             	xorps  %xmm0,%xmm0
  40207f:	0f 29 04 24          	movaps %xmm0,(%rsp)
  402083:	48 83 ec 08          	sub    $0x8,%rsp
  402087:	4c 8d 54 24 08       	lea    0x8(%rsp),%r10
  40208c:	4c 89 ff             	mov    %r15,%rdi
  40208f:	31 f6                	xor    %esi,%esi
  402091:	48 89 c2             	mov    %rax,%rdx
  402094:	48 89 c8             	mov    %rcx,%rax
  402097:	4c 89 d9             	mov    %r11,%rcx
  40209a:	45 31 c0             	xor    %r8d,%r8d
  40209d:	45 31 c9             	xor    %r9d,%r9d
  4020a0:	41 56                	push   %r14
  4020a2:	41 52                	push   %r10
  4020a4:	50                   	push   %rax
  4020a5:	41 ff d5             	call   *%r13
  4020a8:	48 83 c4 20          	add    $0x20,%rsp
  4020ac:	84 c0                	test   %al,%al
  4020ae:	74 11                	je     4020c1 <runtime::arena_alloc+0x151>
  4020b0:	0f 57 c0             	xorps  %xmm0,%xmm0
  4020b3:	0f 11 03             	movups %xmm0,(%rbx)
  4020b6:	e9 ad 00 00 00       	jmp    402168 <runtime::arena_alloc+0x1f8>
  4020bb:	31 c0                	xor    %eax,%eax
  4020bd:	31 c9                	xor    %ecx,%ecx
  4020bf:	eb 1e                	jmp    4020df <runtime::arena_alloc+0x16f>
  4020c1:	48 8b 0c 24          	mov    (%rsp),%rcx
  4020c5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4020ca:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4020cf:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4020d4:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4020d9:	49 89 d8             	mov    %rbx,%r8
  4020dc:	4d 89 f1             	mov    %r14,%r9
  4020df:	48 01 c8             	add    %rcx,%rax
  4020e2:	4c 89 69 08          	mov    %r13,0x8(%rcx)
  4020e6:	4c 89 79 10          	mov    %r15,0x10(%rcx)
  4020ea:	48 01 cd             	add    %rcx,%rbp
  4020ed:	48 89 69 18          	mov    %rbp,0x18(%rcx)
  4020f1:	48 29 e8             	sub    %rbp,%rax
  4020f4:	48 89 41 28          	mov    %rax,0x28(%rcx)
  4020f8:	48 83 79 20 00       	cmpq   $0x0,0x20(%rcx)
  4020fd:	0f 85 29 01 00 00    	jne    40222c <runtime::arena_alloc+0x2bc>
  402103:	48 83 39 00          	cmpq   $0x0,(%rcx)
  402107:	0f 85 4d 01 00 00    	jne    40225a <runtime::arena_alloc+0x2ea>
  40210d:	48 8b 47 10          	mov    0x10(%rdi),%rax
  402111:	48 89 01             	mov    %rax,(%rcx)
  402114:	48 89 4f 10          	mov    %rcx,0x10(%rdi)
  402118:	48 8b 41 28          	mov    0x28(%rcx),%rax
  40211c:	48 01 47 20          	add    %rax,0x20(%rdi)
  402120:	4c 8b 59 20          	mov    0x20(%rcx),%r11
  402124:	4c 8b 51 18          	mov    0x18(%rcx),%r10
  402128:	4d 01 da             	add    %r11,%r10
  40212b:	4d 21 d4             	and    %r10,%r12
  40212e:	4c 29 e2             	sub    %r12,%rdx
  402131:	4d 85 e4             	test   %r12,%r12
  402134:	49 0f 44 d4          	cmove  %r12,%rdx
  402138:	45 31 f6             	xor    %r14d,%r14d
  40213b:	49 89 f1             	mov    %rsi,%r9
  40213e:	b0 01                	mov    $0x1,%al
  402140:	49 01 d1             	add    %rdx,%r9
  402143:	72 08                	jb     40214d <runtime::arena_alloc+0x1dd>
  402145:	45 31 f6             	xor    %r14d,%r14d
  402148:	4d 01 cb             	add    %r9,%r11
  40214b:	73 2a                	jae    402177 <runtime::arena_alloc+0x207>
  40214d:	31 db                	xor    %ebx,%ebx
  40214f:	45 31 ff             	xor    %r15d,%r15d
  402152:	48 8b 4f 10          	mov    0x10(%rdi),%rcx
  402156:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  40215a:	4c 29 f1             	sub    %r14,%rcx
  40215d:	48 01 4f 18          	add    %rcx,0x18(%rdi)
  402161:	4d 89 38             	mov    %r15,(%r8)
  402164:	49 89 58 08          	mov    %rbx,0x8(%r8)
  402168:	48 83 c4 28          	add    $0x28,%rsp
  40216c:	5b                   	pop    %rbx
  40216d:	41 5c                	pop    %r12
  40216f:	41 5d                	pop    %r13
  402171:	41 5e                	pop    %r14
  402173:	41 5f                	pop    %r15
  402175:	5d                   	pop    %rbp
  402176:	c3                   	ret
  402177:	45 31 f6             	xor    %r14d,%r14d
  40217a:	bb 00 00 00 00       	mov    $0x0,%ebx
  40217f:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  402185:	4c 3b 59 28          	cmp    0x28(%rcx),%r11
  402189:	77 c7                	ja     402152 <runtime::arena_alloc+0x1e2>
  40218b:	48 85 f6             	test   %rsi,%rsi
  40218e:	78 27                	js     4021b7 <runtime::arena_alloc+0x247>
  402190:	49 01 d2             	add    %rdx,%r10
  402193:	4c 89 59 20          	mov    %r11,0x20(%rcx)
  402197:	45 31 f6             	xor    %r14d,%r14d
  40219a:	31 c0                	xor    %eax,%eax
  40219c:	48 89 f3             	mov    %rsi,%rbx
  40219f:	4d 89 d7             	mov    %r10,%r15
  4021a2:	eb ae                	jmp    402152 <runtime::arena_alloc+0x1e2>
  4021a4:	48 85 f6             	test   %rsi,%rsi
  4021a7:	78 0e                	js     4021b7 <runtime::arena_alloc+0x247>
  4021a9:	4d 01 d7             	add    %r10,%r15
  4021ac:	4c 89 58 20          	mov    %r11,0x20(%rax)
  4021b0:	31 c0                	xor    %eax,%eax
  4021b2:	48 89 f3             	mov    %rsi,%rbx
  4021b5:	eb 9b                	jmp    402152 <runtime::arena_alloc+0x1e2>
  4021b7:	bf b0 30 40 00       	mov    $0x4030b0,%edi
  4021bc:	49 89 f1             	mov    %rsi,%r9
  4021bf:	be 3e 00 00 00       	mov    $0x3e,%esi
  4021c4:	ba 55 00 00 00       	mov    $0x55,%edx
  4021c9:	b9 31 00 00 00       	mov    $0x31,%ecx
  4021ce:	45 31 c0             	xor    %r8d,%r8d
  4021d1:	e8 6a ef ff ff       	call   401140 <runtime::multi_pointer_slice_handle_error>
  4021d6:	49 8b 41 20          	mov    0x20(%r9),%rax
  4021da:	48 85 c0             	test   %rax,%rax
  4021dd:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  4021e3:	4c 0f 45 d0          	cmovne %rax,%r10
  4021e7:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  4021ec:	be 11 00 00 00       	mov    $0x11,%esi
  4021f1:	ba 65 32 40 00       	mov    $0x403265,%edx
  4021f6:	49 89 c8             	mov    %rcx,%r8
  4021f9:	b9 1a 00 00 00       	mov    $0x1a,%ecx
  4021fe:	41 ff d2             	call   *%r10
  402201:	49 8b 41 20          	mov    0x20(%r9),%rax
  402205:	48 85 c0             	test   %rax,%rax
  402208:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  40220e:	4c 0f 45 d0          	cmovne %rax,%r10
  402212:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  402217:	be 11 00 00 00       	mov    $0x11,%esi
  40221c:	ba 80 32 40 00       	mov    $0x403280,%edx
  402221:	49 89 c8             	mov    %rcx,%r8
  402224:	b9 20 00 00 00       	mov    $0x20,%ecx
  402229:	41 ff d2             	call   *%r10
  40222c:	49 8b 41 20          	mov    0x20(%r9),%rax
  402230:	48 85 c0             	test   %rax,%rax
  402233:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  402239:	4c 0f 45 d0          	cmovne %rax,%r10
  40223d:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  402242:	be 11 00 00 00       	mov    $0x11,%esi
  402247:	ba a0 30 40 00       	mov    $0x4030a0,%edx
  40224c:	b9 0f 00 00 00       	mov    $0xf,%ecx
  402251:	41 b8 10 31 40 00    	mov    $0x403110,%r8d
  402257:	41 ff d2             	call   *%r10
  40225a:	49 8b 41 20          	mov    0x20(%r9),%rax
  40225e:	48 85 c0             	test   %rax,%rax
  402261:	41 ba b0 11 40 00    	mov    $0x4011b0,%r10d
  402267:	4c 0f 45 d0          	cmovne %rax,%r10
  40226b:	bf d7 32 40 00       	mov    $0x4032d7,%edi
  402270:	be 11 00 00 00       	mov    $0x11,%esi
  402275:	ba 38 31 40 00       	mov    $0x403138,%edx
  40227a:	b9 11 00 00 00       	mov    $0x11,%ecx
  40227f:	41 b8 50 31 40 00    	mov    $0x403150,%r8d
  402285:	41 ff d2             	call   *%r10
  402288:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40228f:	00 

0000000000402290 <runtime::print_string>:
  402290:	48 89 f2             	mov    %rsi,%rdx
  402293:	48 89 fe             	mov    %rdi,%rsi
  402296:	b8 01 00 00 00       	mov    $0x1,%eax
  40229b:	bf 02 00 00 00       	mov    $0x2,%edi
  4022a0:	0f 05                	syscall
  4022a2:	c3                   	ret
  4022a3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4022aa:	84 00 00 00 00 00 

00000000004022b0 <runtime::print_byte>:
  4022b0:	40 88 7c 24 f8       	mov    %dil,-0x8(%rsp)
  4022b5:	48 8d 74 24 f8       	lea    -0x8(%rsp),%rsi
  4022ba:	b8 01 00 00 00       	mov    $0x1,%eax
  4022bf:	bf 02 00 00 00       	mov    $0x2,%edi
  4022c4:	ba 01 00 00 00       	mov    $0x1,%edx
  4022c9:	0f 05                	syscall
  4022cb:	c3                   	ret
  4022cc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004022d0 <runtime::print_u64>:
  4022d0:	50                   	push   %rax
  4022d1:	0f 57 c0             	xorps  %xmm0,%xmm0
  4022d4:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  4022d9:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  4022de:	0f 29 44 24 d0       	movaps %xmm0,-0x30(%rsp)
  4022e3:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  4022e8:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  4022ed:	0f 29 44 24 a0       	movaps %xmm0,-0x60(%rsp)
  4022f2:	0f 29 44 24 90       	movaps %xmm0,-0x70(%rsp)
  4022f7:	0f 29 44 24 80       	movaps %xmm0,-0x80(%rsp)
  4022fc:	c6 04 24 00          	movb   $0x0,(%rsp)
  402300:	b9 81 00 00 00       	mov    $0x81,%ecx
  402305:	48 83 ff 0a          	cmp    $0xa,%rdi
  402309:	72 47                	jb     402352 <runtime::print_u64+0x82>
  40230b:	48 be cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rsi
  402312:	cc cc cc 
  402315:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40231c:	00 00 00 00 
  402320:	48 89 f8             	mov    %rdi,%rax
  402323:	48 f7 e6             	mul    %rsi
  402326:	48 c1 ea 03          	shr    $0x3,%rdx
  40232a:	48 8d 04 12          	lea    (%rdx,%rdx,1),%rax
  40232e:	48 8d 04 80          	lea    (%rax,%rax,4),%rax
  402332:	48 f7 d8             	neg    %rax
  402335:	0f b6 84 07 e9 32 40 	movzbl 0x4032e9(%rdi,%rax,1),%eax
  40233c:	00 
  40233d:	88 84 0c 7f ff ff ff 	mov    %al,-0x81(%rsp,%rcx,1)
  402344:	48 ff c9             	dec    %rcx
  402347:	48 83 ff 63          	cmp    $0x63,%rdi
  40234b:	48 89 d7             	mov    %rdx,%rdi
  40234e:	77 d0                	ja     402320 <runtime::print_u64+0x50>
  402350:	eb 03                	jmp    402355 <runtime::print_u64+0x85>
  402352:	48 89 fa             	mov    %rdi,%rdx
  402355:	48 8d 44 24 80       	lea    -0x80(%rsp),%rax
  40235a:	48 8d 34 01          	lea    (%rcx,%rax,1),%rsi
  40235e:	48 ff ce             	dec    %rsi
  402361:	0f b6 92 e9 32 40 00 	movzbl 0x4032e9(%rdx),%edx
  402368:	88 54 01 ff          	mov    %dl,-0x1(%rcx,%rax,1)
  40236c:	ba 82 00 00 00       	mov    $0x82,%edx
  402371:	48 29 ca             	sub    %rcx,%rdx
  402374:	b8 01 00 00 00       	mov    $0x1,%eax
  402379:	bf 02 00 00 00       	mov    $0x2,%edi
  40237e:	0f 05                	syscall
  402380:	58                   	pop    %rax
  402381:	c3                   	ret
  402382:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402389:	1f 84 00 00 00 00 00 

0000000000402390 <runtime::print_i64>:
  402390:	50                   	push   %rax
  402391:	48 89 f9             	mov    %rdi,%rcx
  402394:	48 f7 d9             	neg    %rcx
  402397:	48 0f 48 cf          	cmovs  %rdi,%rcx
  40239b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40239e:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  4023a3:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  4023a8:	0f 29 44 24 d0       	movaps %xmm0,-0x30(%rsp)
  4023ad:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  4023b2:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  4023b7:	0f 29 44 24 a0       	movaps %xmm0,-0x60(%rsp)
  4023bc:	0f 29 44 24 90       	movaps %xmm0,-0x70(%rsp)
  4023c1:	0f 29 44 24 80       	movaps %xmm0,-0x80(%rsp)
  4023c6:	c6 04 24 00          	movb   $0x0,(%rsp)
  4023ca:	41 b8 81 00 00 00    	mov    $0x81,%r8d
  4023d0:	48 83 f9 0a          	cmp    $0xa,%rcx
  4023d4:	7c 3d                	jl     402413 <runtime::print_i64+0x83>
  4023d6:	48 be cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rsi
  4023dd:	cc cc cc 
  4023e0:	48 89 c8             	mov    %rcx,%rax
  4023e3:	48 f7 e6             	mul    %rsi
  4023e6:	48 c1 ea 03          	shr    $0x3,%rdx
  4023ea:	48 8d 04 12          	lea    (%rdx,%rdx,1),%rax
  4023ee:	48 8d 04 80          	lea    (%rax,%rax,4),%rax
  4023f2:	48 f7 d8             	neg    %rax
  4023f5:	0f b6 84 01 e9 32 40 	movzbl 0x4032e9(%rcx,%rax,1),%eax
  4023fc:	00 
  4023fd:	42 88 84 04 7f ff ff 	mov    %al,-0x81(%rsp,%r8,1)
  402404:	ff 
  402405:	49 ff c8             	dec    %r8
  402408:	48 83 f9 63          	cmp    $0x63,%rcx
  40240c:	48 89 d1             	mov    %rdx,%rcx
  40240f:	77 cf                	ja     4023e0 <runtime::print_i64+0x50>
  402411:	eb 03                	jmp    402416 <runtime::print_i64+0x86>
  402413:	48 89 ca             	mov    %rcx,%rdx
  402416:	48 b9 67 66 66 66 66 	movabs $0x6666666666666667,%rcx
  40241d:	66 66 66 
  402420:	48 89 d0             	mov    %rdx,%rax
  402423:	48 89 d6             	mov    %rdx,%rsi
  402426:	48 f7 e9             	imul   %rcx
  402429:	48 89 d0             	mov    %rdx,%rax
  40242c:	48 c1 e8 3f          	shr    $0x3f,%rax
  402430:	48 c1 fa 02          	sar    $0x2,%rdx
  402434:	48 01 c2             	add    %rax,%rdx
  402437:	48 01 d2             	add    %rdx,%rdx
  40243a:	48 8d 04 92          	lea    (%rdx,%rdx,4),%rax
  40243e:	48 f7 d8             	neg    %rax
  402441:	0f b6 84 06 e9 32 40 	movzbl 0x4032e9(%rsi,%rax,1),%eax
  402448:	00 
  402449:	42 88 84 04 7f ff ff 	mov    %al,-0x81(%rsp,%r8,1)
  402450:	ff 
  402451:	48 85 ff             	test   %rdi,%rdi
  402454:	78 05                	js     40245b <runtime::print_i64+0xcb>
  402456:	49 ff c8             	dec    %r8
  402459:	eb 0d                	jmp    402468 <runtime::print_i64+0xd8>
  40245b:	42 c6 84 04 7e ff ff 	movb   $0x2d,-0x82(%rsp,%r8,1)
  402462:	ff 2d 
  402464:	49 83 c0 fe          	add    $0xfffffffffffffffe,%r8
  402468:	4a 8d 34 04          	lea    (%rsp,%r8,1),%rsi
  40246c:	48 83 c6 80          	add    $0xffffffffffffff80,%rsi
  402470:	ba 81 00 00 00       	mov    $0x81,%edx
  402475:	4c 29 c2             	sub    %r8,%rdx
  402478:	b8 01 00 00 00       	mov    $0x1,%eax
  40247d:	bf 02 00 00 00       	mov    $0x2,%edi
  402482:	0f 05                	syscall
  402484:	58                   	pop    %rax
  402485:	c3                   	ret
  402486:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40248d:	00 00 00 

0000000000402490 <runtime::print_caller_location>:
  402490:	53                   	push   %rbx
  402491:	48 89 fb             	mov    %rdi,%rbx
  402494:	48 8b 3f             	mov    (%rdi),%rdi
  402497:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  40249b:	e8 f0 fd ff ff       	call   402290 <runtime::print_string>
  4024a0:	bf 28 00 00 00       	mov    $0x28,%edi
  4024a5:	e8 06 fe ff ff       	call   4022b0 <runtime::print_byte>
  4024aa:	48 63 7b 10          	movslq 0x10(%rbx),%rdi
  4024ae:	e8 1d fe ff ff       	call   4022d0 <runtime::print_u64>
  4024b3:	83 7b 14 00          	cmpl   $0x0,0x14(%rbx)
  4024b7:	74 13                	je     4024cc <runtime::print_caller_location+0x3c>
  4024b9:	bf 3a 00 00 00       	mov    $0x3a,%edi
  4024be:	e8 ed fd ff ff       	call   4022b0 <runtime::print_byte>
  4024c3:	48 63 7b 14          	movslq 0x14(%rbx),%rdi
  4024c7:	e8 04 fe ff ff       	call   4022d0 <runtime::print_u64>
  4024cc:	bf 29 00 00 00       	mov    $0x29,%edi
  4024d1:	5b                   	pop    %rbx
  4024d2:	e9 d9 fd ff ff       	jmp    4022b0 <runtime::print_byte>

00000000004024d7 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)>:
  4024d7:	48 8b 47 08          	mov    0x8(%rdi),%rax
  4024db:	48 8b 08             	mov    (%rax),%rcx
  4024de:	48 89 ca             	mov    %rcx,%rdx
  4024e1:	48 c1 ea 06          	shr    $0x6,%rdx
  4024e5:	48 8d 71 14          	lea    0x14(%rcx),%rsi
  4024e9:	41 89 f0             	mov    %esi,%r8d
  4024ec:	41 83 e0 3f          	and    $0x3f,%r8d
  4024f0:	49 c7 c1 ff ff ff ff 	mov    $0xffffffffffffffff,%r9
  4024f7:	c4 42 b8 f5 c1       	bzhi   %r8,%r9,%r8
  4024fc:	48 c1 ee 06          	shr    $0x6,%rsi
  402500:	48 39 d6             	cmp    %rdx,%rsi
  402503:	4d 0f 44 c8          	cmove  %r8,%r9
  402507:	4c 89 0c d0          	mov    %r9,(%rax,%rdx,8)
  40250b:	4c 89 44 d0 08       	mov    %r8,0x8(%rax,%rdx,8)
  402510:	48 8b 47 08          	mov    0x8(%rdi),%rax
  402514:	48 83 00 14          	addq   $0x14,(%rax)
  402518:	48 83 c1 c0          	add    $0xffffffffffffffc0,%rcx
  40251c:	48 b8 00 00 00 00 14 	movabs $0x1400000000,%rax
  402523:	00 00 00 
  402526:	48 09 c8             	or     %rcx,%rax
  402529:	c3                   	ret

000000000040252a <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)>:
  40252a:	48 8b 47 08          	mov    0x8(%rdi),%rax
  40252e:	48 8b 08             	mov    (%rax),%rcx
  402531:	48 89 ca             	mov    %rcx,%rdx
  402534:	48 c1 ea 06          	shr    $0x6,%rdx
  402538:	48 8d 71 32          	lea    0x32(%rcx),%rsi
  40253c:	41 89 f0             	mov    %esi,%r8d
  40253f:	41 83 e0 3f          	and    $0x3f,%r8d
  402543:	49 c7 c1 ff ff ff ff 	mov    $0xffffffffffffffff,%r9
  40254a:	c4 42 b8 f5 c1       	bzhi   %r8,%r9,%r8
  40254f:	48 c1 ee 06          	shr    $0x6,%rsi
  402553:	48 39 d6             	cmp    %rdx,%rsi
  402556:	4d 0f 44 c8          	cmove  %r8,%r9
  40255a:	4c 89 0c d0          	mov    %r9,(%rax,%rdx,8)
  40255e:	4c 89 44 d0 08       	mov    %r8,0x8(%rax,%rdx,8)
  402563:	48 8b 47 08          	mov    0x8(%rdi),%rax
  402567:	48 83 00 32          	addq   $0x32,(%rax)
  40256b:	48 83 c1 c0          	add    $0xffffffffffffffc0,%rcx
  40256f:	48 b8 00 00 00 00 32 	movabs $0x3200000000,%rax
  402576:	00 00 00 
  402579:	48 09 c8             	or     %rcx,%rax
  40257c:	c3                   	ret
  40257d:	0f 1f 00             	nopl   (%rax)

0000000000402580 <runtime::default_logger_proc>:
  402580:	c3                   	ret
  402581:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402588:	0f 1f 84 00 00 00 00 
  40258f:	00 

0000000000402590 <__truncsfhf2>:
  402590:	66 0f 7e c7          	movd   %xmm0,%edi
  402594:	89 f8                	mov    %edi,%eax
  402596:	c1 e8 10             	shr    $0x10,%eax
  402599:	25 00 80 00 00       	and    $0x8000,%eax
  40259e:	41 89 f8             	mov    %edi,%r8d
  4025a1:	41 c1 e8 17          	shr    $0x17,%r8d
  4025a5:	41 0f b6 f0          	movzbl %r8b,%esi
  4025a9:	89 fa                	mov    %edi,%edx
  4025ab:	81 e2 ff ff 7f 00    	and    $0x7fffff,%edx
  4025b1:	83 fe 70             	cmp    $0x70,%esi
  4025b4:	77 28                	ja     4025de <__truncsfhf2+0x4e>
  4025b6:	83 fe 66             	cmp    $0x66,%esi
  4025b9:	0f 82 36 01 00 00    	jb     4026f5 <__truncsfhf2+0x165>
  4025bf:	81 ca 00 00 80 00    	or     $0x800000,%edx
  4025c5:	b1 71                	mov    $0x71,%cl
  4025c7:	44 28 c1             	sub    %r8b,%cl
  4025ca:	d3 ea                	shr    %cl,%edx
  4025cc:	89 d1                	mov    %edx,%ecx
  4025ce:	81 e1 00 10 00 00    	and    $0x1000,%ecx
  4025d4:	8d 0c 4a             	lea    (%rdx,%rcx,2),%ecx
  4025d7:	c1 e9 0d             	shr    $0xd,%ecx
  4025da:	09 c1                	or     %eax,%ecx
  4025dc:	eb 2e                	jmp    40260c <__truncsfhf2+0x7c>
  4025de:	8d 4e 90             	lea    -0x70(%rsi),%ecx
  4025e1:	81 f9 8f 00 00 00    	cmp    $0x8f,%ecx
  4025e7:	75 2b                	jne    402614 <__truncsfhf2+0x84>
  4025e9:	85 d2                	test   %edx,%edx
  4025eb:	0f 84 ed 00 00 00    	je     4026de <__truncsfhf2+0x14e>
  4025f1:	89 d1                	mov    %edx,%ecx
  4025f3:	c1 e9 0d             	shr    $0xd,%ecx
  4025f6:	31 f6                	xor    %esi,%esi
  4025f8:	81 fa 00 20 00 00    	cmp    $0x2000,%edx
  4025fe:	40 0f 92 c6          	setb   %sil
  402602:	09 c1                	or     %eax,%ecx
  402604:	09 f1                	or     %esi,%ecx
  402606:	81 c9 00 7c 00 00    	or     $0x7c00,%ecx
  40260c:	89 c8                	mov    %ecx,%eax
  40260e:	66 0f c4 c0 00       	pinsrw $0x0,%eax,%xmm0
  402613:	c3                   	ret
  402614:	f7 c7 00 10 00 00    	test   $0x1000,%edi
  40261a:	74 1e                	je     40263a <__truncsfhf2+0xaa>
  40261c:	8d ba 00 20 00 00    	lea    0x2000(%rdx),%edi
  402622:	83 c6 91             	add    $0xffffff91,%esi
  402625:	45 31 c0             	xor    %r8d,%r8d
  402628:	81 fa 00 e0 7f 00    	cmp    $0x7fe000,%edx
  40262e:	44 0f 42 c7          	cmovb  %edi,%r8d
  402632:	0f 42 f1             	cmovb  %ecx,%esi
  402635:	44 89 c2             	mov    %r8d,%edx
  402638:	89 f1                	mov    %esi,%ecx
  40263a:	83 f9 1f             	cmp    $0x1f,%ecx
  40263d:	0f 82 a6 00 00 00    	jb     4026e9 <__truncsfhf2+0x159>
  402643:	48 b9 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rcx
  40264a:	00 00 00 
  40264d:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402652:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402657:	48 0f af c9          	imul   %rcx,%rcx
  40265b:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402660:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402665:	48 0f af c9          	imul   %rcx,%rcx
  402669:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40266e:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402673:	48 0f af c9          	imul   %rcx,%rcx
  402677:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40267c:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402681:	48 0f af c9          	imul   %rcx,%rcx
  402685:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40268a:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40268f:	48 0f af c9          	imul   %rcx,%rcx
  402693:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  402698:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40269d:	48 0f af c9          	imul   %rcx,%rcx
  4026a1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4026a6:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4026ab:	48 0f af c9          	imul   %rcx,%rcx
  4026af:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4026b4:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4026b9:	48 0f af c9          	imul   %rcx,%rcx
  4026bd:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4026c2:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4026c7:	48 0f af c9          	imul   %rcx,%rcx
  4026cb:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4026d0:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4026d5:	48 0f af c9          	imul   %rcx,%rcx
  4026d9:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4026de:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4026e3:	66 0f c4 c0 00       	pinsrw $0x0,%eax,%xmm0
  4026e8:	c3                   	ret
  4026e9:	c1 e1 0a             	shl    $0xa,%ecx
  4026ec:	c1 ea 0d             	shr    $0xd,%edx
  4026ef:	09 c2                	or     %eax,%edx
  4026f1:	09 ca                	or     %ecx,%edx
  4026f3:	89 d0                	mov    %edx,%eax
  4026f5:	66 0f c4 c0 00       	pinsrw $0x0,%eax,%xmm0
  4026fa:	c3                   	ret
  4026fb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402700 <__truncdfhf2>:
  402700:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  402704:	e9 87 fe ff ff       	jmp    402590 <__truncsfhf2>
  402709:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000402710 <__gnu_h2f_ieee>:
  402710:	66 0f c5 c0 00       	pextrw $0x0,%xmm0,%eax
  402715:	89 c1                	mov    %eax,%ecx
  402717:	81 e1 ff 7f 00 00    	and    $0x7fff,%ecx
  40271d:	c1 e1 0d             	shl    $0xd,%ecx
  402720:	66 0f 6e c1          	movd   %ecx,%xmm0
  402724:	f3 0f 59 05 d8 08 00 	mulss  0x8d8(%rip),%xmm0        # 403004 <_IO_stdin_used+0x4>
  40272b:	00 
  40272c:	66 0f 7e c1          	movd   %xmm0,%ecx
  402730:	89 ca                	mov    %ecx,%edx
  402732:	81 ca 00 00 80 7f    	or     $0x7f800000,%edx
  402738:	0f 2e 05 c9 08 00 00 	ucomiss 0x8c9(%rip),%xmm0        # 403008 <_IO_stdin_used+0x8>
  40273f:	0f 42 d1             	cmovb  %ecx,%edx
  402742:	25 00 80 00 00       	and    $0x8000,%eax
  402747:	c1 e0 10             	shl    $0x10,%eax
  40274a:	09 d0                	or     %edx,%eax
  40274c:	66 0f 6e c0          	movd   %eax,%xmm0
  402750:	c3                   	ret
  402751:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402758:	0f 1f 84 00 00 00 00 
  40275f:	00 

0000000000402760 <__gnu_f2h_ieee>:
  402760:	e9 2b fe ff ff       	jmp    402590 <__truncsfhf2>
  402765:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40276c:	00 00 00 00 

0000000000402770 <__extendhfsf2>:
  402770:	66 0f c5 c0 00       	pextrw $0x0,%xmm0,%eax
  402775:	89 c1                	mov    %eax,%ecx
  402777:	81 e1 ff 7f 00 00    	and    $0x7fff,%ecx
  40277d:	c1 e1 0d             	shl    $0xd,%ecx
  402780:	66 0f 6e c1          	movd   %ecx,%xmm0
  402784:	f3 0f 59 05 78 08 00 	mulss  0x878(%rip),%xmm0        # 403004 <_IO_stdin_used+0x4>
  40278b:	00 
  40278c:	66 0f 7e c1          	movd   %xmm0,%ecx
  402790:	89 ca                	mov    %ecx,%edx
  402792:	81 ca 00 00 80 7f    	or     $0x7f800000,%edx
  402798:	0f 2e 05 69 08 00 00 	ucomiss 0x869(%rip),%xmm0        # 403008 <_IO_stdin_used+0x8>
  40279f:	0f 42 d1             	cmovb  %ecx,%edx
  4027a2:	25 00 80 00 00       	and    $0x8000,%eax
  4027a7:	c1 e0 10             	shl    $0x10,%eax
  4027aa:	09 d0                	or     %edx,%eax
  4027ac:	66 0f 6e c0          	movd   %eax,%xmm0
  4027b0:	c3                   	ret
  4027b1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4027b8:	0f 1f 84 00 00 00 00 
  4027bf:	00 

00000000004027c0 <__floattidf>:
  4027c0:	48 89 f8             	mov    %rdi,%rax
  4027c3:	48 09 f0             	or     %rsi,%rax
  4027c6:	74 5d                	je     402825 <__floattidf+0x65>
  4027c8:	48 89 f2             	mov    %rsi,%rdx
  4027cb:	48 c1 fa 3f          	sar    $0x3f,%rdx
  4027cf:	48 31 d6             	xor    %rdx,%rsi
  4027d2:	48 31 d7             	xor    %rdx,%rdi
  4027d5:	48 29 d7             	sub    %rdx,%rdi
  4027d8:	48 19 d6             	sbb    %rdx,%rsi
  4027db:	48 0f bd ce          	bsr    %rsi,%rcx
  4027df:	48 83 f1 3f          	xor    $0x3f,%rcx
  4027e3:	48 0f bd c7          	bsr    %rdi,%rax
  4027e7:	48 83 f0 3f          	xor    $0x3f,%rax
  4027eb:	48 83 c8 40          	or     $0x40,%rax
  4027ef:	48 85 f6             	test   %rsi,%rsi
  4027f2:	48 0f 45 c1          	cmovne %rcx,%rax
  4027f6:	41 89 c0             	mov    %eax,%r8d
  4027f9:	41 83 f0 7f          	xor    $0x7f,%r8d
  4027fd:	48 89 f9             	mov    %rdi,%rcx
  402800:	48 c1 e9 35          	shr    $0x35,%rcx
  402804:	48 09 f1             	or     %rsi,%rcx
  402807:	74 20                	je     402829 <__floattidf+0x69>
  402809:	48 83 f8 49          	cmp    $0x49,%rax
  40280d:	0f 84 b9 00 00 00    	je     4028cc <__floattidf+0x10c>
  402813:	83 f8 4a             	cmp    $0x4a,%eax
  402816:	75 25                	jne    40283d <__floattidf+0x7d>
  402818:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  40281d:	48 01 ff             	add    %rdi,%rdi
  402820:	e9 a7 00 00 00       	jmp    4028cc <__floattidf+0x10c>
  402825:	0f 57 c0             	xorps  %xmm0,%xmm0
  402828:	c3                   	ret
  402829:	04 35                	add    $0x35,%al
  40282b:	89 c1                	mov    %eax,%ecx
  40282d:	48 d3 e7             	shl    %cl,%rdi
  402830:	31 f6                	xor    %esi,%esi
  402832:	a8 40                	test   $0x40,%al
  402834:	48 0f 44 f7          	cmove  %rdi,%rsi
  402838:	e9 be 00 00 00       	jmp    4028fb <__floattidf+0x13b>
  40283d:	41 57                	push   %r15
  40283f:	41 56                	push   %r14
  402841:	53                   	push   %rbx
  402842:	45 31 db             	xor    %r11d,%r11d
  402845:	b9 49 00 00 00       	mov    $0x49,%ecx
  40284a:	48 29 c1             	sub    %rax,%rcx
  40284d:	bb 00 00 00 00       	mov    $0x0,%ebx
  402852:	48 19 db             	sbb    %rbx,%rbx
  402855:	49 89 f9             	mov    %rdi,%r9
  402858:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40285c:	49 89 f2             	mov    %rsi,%r10
  40285f:	49 d3 ea             	shr    %cl,%r10
  402862:	f6 c1 40             	test   $0x40,%cl
  402865:	4d 0f 45 ca          	cmovne %r10,%r9
  402869:	4d 0f 45 d3          	cmovne %r11,%r10
  40286d:	48 81 f9 80 00 00 00 	cmp    $0x80,%rcx
  402874:	48 83 db 00          	sbb    $0x0,%rbx
  402878:	4d 0f 43 d3          	cmovae %r11,%r10
  40287c:	4d 0f 43 cb          	cmovae %r11,%r9
  402880:	48 89 fb             	mov    %rdi,%rbx
  402883:	41 be ff 01 00 00    	mov    $0x1ff,%r14d
  402889:	49 c7 c7 ff ff ff ff 	mov    $0xffffffffffffffff,%r15
  402890:	89 c1                	mov    %eax,%ecx
  402892:	4d 0f ad f7          	shrd   %cl,%r14,%r15
  402896:	49 d3 ee             	shr    %cl,%r14
  402899:	48 c1 eb 37          	shr    $0x37,%rbx
  40289d:	a8 40                	test   $0x40,%al
  40289f:	4d 0f 45 fe          	cmovne %r14,%r15
  4028a3:	4d 0f 45 f3          	cmovne %r11,%r14
  4028a7:	48 09 f3             	or     %rsi,%rbx
  4028aa:	4d 0f 44 fb          	cmove  %r11,%r15
  4028ae:	45 0f 44 f3          	cmove  %r11d,%r14d
  4028b2:	49 21 ff             	and    %rdi,%r15
  4028b5:	44 21 f6             	and    %r14d,%esi
  4028b8:	31 ff                	xor    %edi,%edi
  4028ba:	4c 09 fe             	or     %r15,%rsi
  4028bd:	40 0f 95 c7          	setne  %dil
  4028c1:	4c 09 cf             	or     %r9,%rdi
  4028c4:	4c 89 d6             	mov    %r10,%rsi
  4028c7:	5b                   	pop    %rbx
  4028c8:	41 5e                	pop    %r14
  4028ca:	41 5f                	pop    %r15
  4028cc:	89 f9                	mov    %edi,%ecx
  4028ce:	c1 e9 02             	shr    $0x2,%ecx
  4028d1:	83 e1 01             	and    $0x1,%ecx
  4028d4:	48 09 f9             	or     %rdi,%rcx
  4028d7:	48 83 c1 01          	add    $0x1,%rcx
  4028db:	48 83 d6 00          	adc    $0x0,%rsi
  4028df:	48 0f ba e1 37       	bt     $0x37,%rcx
  4028e4:	72 07                	jb     4028ed <__floattidf+0x12d>
  4028e6:	48 0f a4 ce 3e       	shld   $0x3e,%rcx,%rsi
  4028eb:	eb 0e                	jmp    4028fb <__floattidf+0x13b>
  4028ed:	48 0f a4 ce 3d       	shld   $0x3d,%rcx,%rsi
  4028f2:	41 b8 80 00 00 00    	mov    $0x80,%r8d
  4028f8:	41 29 c0             	sub    %eax,%r8d
  4028fb:	81 e2 00 00 00 80    	and    $0x80000000,%edx
  402901:	41 c1 e0 14          	shl    $0x14,%r8d
  402905:	41 09 d0             	or     %edx,%r8d
  402908:	48 89 f0             	mov    %rsi,%rax
  40290b:	48 c1 e8 20          	shr    $0x20,%rax
  40290f:	25 ff ff 0f 00       	and    $0xfffff,%eax
  402914:	44 01 c0             	add    %r8d,%eax
  402917:	05 00 00 f0 3f       	add    $0x3ff00000,%eax
  40291c:	48 c1 e0 20          	shl    $0x20,%rax
  402920:	89 f1                	mov    %esi,%ecx
  402922:	48 09 c1             	or     %rax,%rcx
  402925:	66 48 0f 6e c1       	movq   %rcx,%xmm0
  40292a:	c3                   	ret
  40292b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402930 <__floattidf_unsigned>:
  402930:	48 89 f8             	mov    %rdi,%rax
  402933:	48 09 f0             	or     %rsi,%rax
  402936:	74 48                	je     402980 <__floattidf_unsigned+0x50>
  402938:	48 0f bd ce          	bsr    %rsi,%rcx
  40293c:	48 83 f1 3f          	xor    $0x3f,%rcx
  402940:	48 0f bd c7          	bsr    %rdi,%rax
  402944:	48 83 f0 3f          	xor    $0x3f,%rax
  402948:	48 83 c8 40          	or     $0x40,%rax
  40294c:	48 85 f6             	test   %rsi,%rsi
  40294f:	48 0f 45 c1          	cmovne %rcx,%rax
  402953:	89 c2                	mov    %eax,%edx
  402955:	83 f2 7f             	xor    $0x7f,%edx
  402958:	48 89 f9             	mov    %rdi,%rcx
  40295b:	48 c1 e9 35          	shr    $0x35,%rcx
  40295f:	48 09 f1             	or     %rsi,%rcx
  402962:	74 20                	je     402984 <__floattidf_unsigned+0x54>
  402964:	48 83 f8 49          	cmp    $0x49,%rax
  402968:	0f 84 b4 00 00 00    	je     402a22 <__floattidf_unsigned+0xf2>
  40296e:	83 f8 4a             	cmp    $0x4a,%eax
  402971:	75 25                	jne    402998 <__floattidf_unsigned+0x68>
  402973:	48 0f a4 fe 01       	shld   $0x1,%rdi,%rsi
  402978:	48 01 ff             	add    %rdi,%rdi
  40297b:	e9 a2 00 00 00       	jmp    402a22 <__floattidf_unsigned+0xf2>
  402980:	0f 57 c0             	xorps  %xmm0,%xmm0
  402983:	c3                   	ret
  402984:	04 b5                	add    $0xb5,%al
  402986:	89 c1                	mov    %eax,%ecx
  402988:	48 d3 e7             	shl    %cl,%rdi
  40298b:	31 f6                	xor    %esi,%esi
  40298d:	a8 40                	test   $0x40,%al
  40298f:	48 0f 44 f7          	cmove  %rdi,%rsi
  402993:	e9 b7 00 00 00       	jmp    402a4f <__floattidf_unsigned+0x11f>
  402998:	41 56                	push   %r14
  40299a:	53                   	push   %rbx
  40299b:	45 31 d2             	xor    %r10d,%r10d
  40299e:	b9 49 00 00 00       	mov    $0x49,%ecx
  4029a3:	48 29 c1             	sub    %rax,%rcx
  4029a6:	41 bb 00 00 00 00    	mov    $0x0,%r11d
  4029ac:	4d 19 db             	sbb    %r11,%r11
  4029af:	49 89 f8             	mov    %rdi,%r8
  4029b2:	49 0f ad f0          	shrd   %cl,%rsi,%r8
  4029b6:	49 89 f1             	mov    %rsi,%r9
  4029b9:	49 d3 e9             	shr    %cl,%r9
  4029bc:	f6 c1 40             	test   $0x40,%cl
  4029bf:	4d 0f 45 c1          	cmovne %r9,%r8
  4029c3:	4d 0f 45 ca          	cmovne %r10,%r9
  4029c7:	48 81 f9 80 00 00 00 	cmp    $0x80,%rcx
  4029ce:	49 83 db 00          	sbb    $0x0,%r11
  4029d2:	4d 0f 43 ca          	cmovae %r10,%r9
  4029d6:	4d 0f 43 c2          	cmovae %r10,%r8
  4029da:	49 89 fb             	mov    %rdi,%r11
  4029dd:	bb ff 01 00 00       	mov    $0x1ff,%ebx
  4029e2:	49 c7 c6 ff ff ff ff 	mov    $0xffffffffffffffff,%r14
  4029e9:	89 c1                	mov    %eax,%ecx
  4029eb:	49 0f ad de          	shrd   %cl,%rbx,%r14
  4029ef:	48 d3 eb             	shr    %cl,%rbx
  4029f2:	49 c1 eb 37          	shr    $0x37,%r11
  4029f6:	a8 40                	test   $0x40,%al
  4029f8:	4c 0f 45 f3          	cmovne %rbx,%r14
  4029fc:	49 0f 45 da          	cmovne %r10,%rbx
  402a00:	49 09 f3             	or     %rsi,%r11
  402a03:	4d 0f 44 f2          	cmove  %r10,%r14
  402a07:	41 0f 44 da          	cmove  %r10d,%ebx
  402a0b:	49 21 fe             	and    %rdi,%r14
  402a0e:	21 de                	and    %ebx,%esi
  402a10:	31 ff                	xor    %edi,%edi
  402a12:	4c 09 f6             	or     %r14,%rsi
  402a15:	40 0f 95 c7          	setne  %dil
  402a19:	4c 09 c7             	or     %r8,%rdi
  402a1c:	4c 89 ce             	mov    %r9,%rsi
  402a1f:	5b                   	pop    %rbx
  402a20:	41 5e                	pop    %r14
  402a22:	89 f9                	mov    %edi,%ecx
  402a24:	c1 e9 02             	shr    $0x2,%ecx
  402a27:	83 e1 01             	and    $0x1,%ecx
  402a2a:	48 09 f9             	or     %rdi,%rcx
  402a2d:	48 83 c1 01          	add    $0x1,%rcx
  402a31:	48 83 d6 00          	adc    $0x0,%rsi
  402a35:	48 0f ba e1 37       	bt     $0x37,%rcx
  402a3a:	72 07                	jb     402a43 <__floattidf_unsigned+0x113>
  402a3c:	48 0f a4 ce 3e       	shld   $0x3e,%rcx,%rsi
  402a41:	eb 0c                	jmp    402a4f <__floattidf_unsigned+0x11f>
  402a43:	48 0f a4 ce 3d       	shld   $0x3d,%rcx,%rsi
  402a48:	ba 80 00 00 00       	mov    $0x80,%edx
  402a4d:	29 c2                	sub    %eax,%edx
  402a4f:	c1 e2 14             	shl    $0x14,%edx
  402a52:	48 89 f0             	mov    %rsi,%rax
  402a55:	48 c1 e8 20          	shr    $0x20,%rax
  402a59:	25 ff ff 0f 00       	and    $0xfffff,%eax
  402a5e:	01 d0                	add    %edx,%eax
  402a60:	05 00 00 f0 3f       	add    $0x3ff00000,%eax
  402a65:	48 c1 e0 20          	shl    $0x20,%rax
  402a69:	89 f1                	mov    %esi,%ecx
  402a6b:	48 09 c1             	or     %rax,%rcx
  402a6e:	66 48 0f 6e c1       	movq   %rcx,%xmm0
  402a73:	c3                   	ret
  402a74:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402a7b:	00 00 00 00 00 

0000000000402a80 <__umodti3>:
  402a80:	48 83 ec 18          	sub    $0x18,%rsp
  402a84:	49 89 e0             	mov    %rsp,%r8
  402a87:	e8 84 e9 ff ff       	call   401410 <runtime::udivmod128>
  402a8c:	48 8b 04 24          	mov    (%rsp),%rax
  402a90:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402a95:	48 83 c4 18          	add    $0x18,%rsp
  402a99:	c3                   	ret
  402a9a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402aa0 <__udivmodti4>:
  402aa0:	e9 6b e9 ff ff       	jmp    401410 <runtime::udivmod128>
  402aa5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  402aac:	00 00 00 00 

0000000000402ab0 <__udivti3>:
  402ab0:	45 31 c0             	xor    %r8d,%r8d
  402ab3:	e9 58 e9 ff ff       	jmp    401410 <runtime::udivmod128>

Disassembly of section .fini:

0000000000402ab8 <_fini>:
  402ab8:	f3 0f 1e fa          	endbr64
  402abc:	48 83 ec 08          	sub    $0x8,%rsp
  402ac0:	48 83 c4 08          	add    $0x8,%rsp
  402ac4:	c3                   	ret
