
/home/khalidd/Desktop/Github/Journey_ECS/main-debug.bin:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 c9 af 00 00 	mov    0xafc9(%rip),%rax        # 40bfd8 <__gmon_start__@Base>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca af 00 00    	push   0xafca(%rip)        # 40bff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc af 00 00    	jmp    *0xafcc(%rip)        # 40bff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca af 00 00    	jmp    *0xafca(%rip)        # 40c000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <memset@plt>:
  401040:	ff 25 c2 af 00 00    	jmp    *0xafc2(%rip)        # 40c008 <memset@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <calloc@plt>:
  401050:	ff 25 ba af 00 00    	jmp    *0xafba(%rip)        # 40c010 <calloc@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <memcpy@plt>:
  401060:	ff 25 b2 af 00 00    	jmp    *0xafb2(%rip)        # 40c018 <memcpy@GLIBC_2.14>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <malloc@plt>:
  401070:	ff 25 aa af 00 00    	jmp    *0xafaa(%rip)        # 40c020 <malloc@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <realloc@plt>:
  401080:	ff 25 a2 af 00 00    	jmp    *0xafa2(%rip)        # 40c028 <realloc@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <memmove@plt>:
  401090:	ff 25 9a af 00 00    	jmp    *0xaf9a(%rip)        # 40c030 <memmove@GLIBC_2.2.5>
  401096:	68 06 00 00 00       	push   $0x6
  40109b:	e9 80 ff ff ff       	jmp    401020 <_init+0x20>

Disassembly of section .text:

00000000004010a0 <_start>:
  4010a0:	f3 0f 1e fa          	endbr64
  4010a4:	31 ed                	xor    %ebp,%ebp
  4010a6:	49 89 d1             	mov    %rdx,%r9
  4010a9:	5e                   	pop    %rsi
  4010aa:	48 89 e2             	mov    %rsp,%rdx
  4010ad:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  4010b1:	50                   	push   %rax
  4010b2:	54                   	push   %rsp
  4010b3:	45 31 c0             	xor    %r8d,%r8d
  4010b6:	31 c9                	xor    %ecx,%ecx
  4010b8:	48 c7 c7 50 30 40 00 	mov    $0x403050,%rdi
  4010bf:	ff 15 03 af 00 00    	call   *0xaf03(%rip)        # 40bfc8 <__libc_start_main@GLIBC_2.34>
  4010c5:	f4                   	hlt
  4010c6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4010cd:	00 00 00 

00000000004010d0 <_dl_relocate_static_pie>:
  4010d0:	f3 0f 1e fa          	endbr64
  4010d4:	c3                   	ret
  4010d5:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4010dc:	00 00 00 
  4010df:	90                   	nop

00000000004010e0 <deregister_tm_clones>:
  4010e0:	b8 58 c0 40 00       	mov    $0x40c058,%eax
  4010e5:	48 3d 58 c0 40 00    	cmp    $0x40c058,%rax
  4010eb:	74 13                	je     401100 <deregister_tm_clones+0x20>
  4010ed:	48 8b 05 dc ae 00 00 	mov    0xaedc(%rip),%rax        # 40bfd0 <_ITM_deregisterTMCloneTable@Base>
  4010f4:	48 85 c0             	test   %rax,%rax
  4010f7:	74 07                	je     401100 <deregister_tm_clones+0x20>
  4010f9:	bf 58 c0 40 00       	mov    $0x40c058,%edi
  4010fe:	ff e0                	jmp    *%rax
  401100:	c3                   	ret
  401101:	0f 1f 40 00          	nopl   0x0(%rax)
  401105:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40110c:	00 00 00 00 

0000000000401110 <register_tm_clones>:
  401110:	be 58 c0 40 00       	mov    $0x40c058,%esi
  401115:	48 81 ee 58 c0 40 00 	sub    $0x40c058,%rsi
  40111c:	48 89 f0             	mov    %rsi,%rax
  40111f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401123:	48 c1 f8 03          	sar    $0x3,%rax
  401127:	48 01 c6             	add    %rax,%rsi
  40112a:	48 d1 fe             	sar    $1,%rsi
  40112d:	74 19                	je     401148 <register_tm_clones+0x38>
  40112f:	48 8b 05 aa ae 00 00 	mov    0xaeaa(%rip),%rax        # 40bfe0 <_ITM_registerTMCloneTable@Base>
  401136:	48 85 c0             	test   %rax,%rax
  401139:	74 0d                	je     401148 <register_tm_clones+0x38>
  40113b:	bf 58 c0 40 00       	mov    $0x40c058,%edi
  401140:	ff e0                	jmp    *%rax
  401142:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401148:	c3                   	ret
  401149:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401150 <__do_global_dtors_aux>:
  401150:	f3 0f 1e fa          	endbr64
  401154:	80 3d fd ae 00 00 00 	cmpb   $0x0,0xaefd(%rip)        # 40c058 <__TMC_END__>
  40115b:	75 13                	jne    401170 <__do_global_dtors_aux+0x20>
  40115d:	55                   	push   %rbp
  40115e:	48 89 e5             	mov    %rsp,%rbp
  401161:	e8 7a ff ff ff       	call   4010e0 <deregister_tm_clones>
  401166:	c6 05 eb ae 00 00 01 	movb   $0x1,0xaeeb(%rip)        # 40c058 <__TMC_END__>
  40116d:	5d                   	pop    %rbp
  40116e:	c3                   	ret
  40116f:	90                   	nop
  401170:	c3                   	ret
  401171:	0f 1f 40 00          	nopl   0x0(%rax)
  401175:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40117c:	00 00 00 00 

0000000000401180 <frame_dummy>:
  401180:	f3 0f 1e fa          	endbr64
  401184:	eb 8a                	jmp    401110 <register_tm_clones>
  401186:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40118d:	00 00 00 

0000000000401190 <runtime::heap_allocator>:
  401190:	48 c7 c0 b0 11 40 00 	mov    $0x4011b0,%rax
  401197:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40119c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  4011a3:	00 00 
  4011a5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4011aa:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4011af:	c3                   	ret

00000000004011b0 <runtime::heap_allocator_proc>:
  4011b0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4011b7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  4011bc:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  4011c1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4011c6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4011cb:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  4011d0:	40 88 f0             	mov    %sil,%al
  4011d3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  4011d7:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  4011de:	00 
  4011df:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4011e4:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  4011eb:	00 
  4011ec:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4011f1:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  4011f5:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4011fa:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4011ff:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401204:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401209:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  40120e:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  401215:	00 
  401216:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  40121d:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  401224:	00 
  401225:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  40122c:	00 
  40122d:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  401234:	00 
  401235:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40123c:	00 
  40123d:	0f b6 c8             	movzbl %al,%ecx
  401240:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  401245:	2c 07                	sub    $0x7,%al
  401247:	0f 87 5f 01 00 00    	ja     4013ac <runtime::heap_allocator_proc+0x1fc>
  40124d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401252:	48 8b 04 c5 08 90 40 	mov    0x409008(,%rax,8),%rax
  401259:	00 
  40125a:	ff e0                	jmp    *%rax
  40125c:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401261:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401266:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40126b:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  40126f:	84 c0                	test   %al,%al
  401271:	0f 94 c0             	sete   %al
  401274:	0f 57 c0             	xorps  %xmm0,%xmm0
  401277:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40127e:	00 
  40127f:	48 89 e1             	mov    %rsp,%rcx
  401282:	48 89 11             	mov    %rdx,(%rcx)
  401285:	44 0f b6 c0          	movzbl %al,%r8d
  401289:	31 c0                	xor    %eax,%eax
  40128b:	89 c1                	mov    %eax,%ecx
  40128d:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  401294:	00 
  401295:	48 89 ca             	mov    %rcx,%rdx
  401298:	e8 b3 01 00 00       	call   401450 <runtime::heap_allocator_proc.aligned_alloc-0>
  40129d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4012a2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4012a9:	00 
  4012aa:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4012b1:	00 
  4012b2:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4012b6:	48 89 11             	mov    %rdx,(%rcx)
  4012b9:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4012c0:	c3                   	ret
  4012c1:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4012c6:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4012cb:	e8 d0 03 00 00       	call   4016a0 <runtime::heap_allocator_proc.aligned_free-1>
  4012d0:	e9 d7 00 00 00       	jmp    4013ac <runtime::heap_allocator_proc+0x1fc>
  4012d5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4012da:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4012e1:	00 
  4012e2:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4012e9:	b0 04                	mov    $0x4,%al
  4012eb:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4012f2:	c3                   	ret
  4012f3:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4012f8:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4012fd:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401302:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401307:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  40130c:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401310:	2c 03                	sub    $0x3,%al
  401312:	0f 94 c0             	sete   %al
  401315:	0f 57 c0             	xorps  %xmm0,%xmm0
  401318:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40131d:	49 89 e0             	mov    %rsp,%r8
  401320:	4d 89 08             	mov    %r9,(%r8)
  401323:	44 0f b6 c0          	movzbl %al,%r8d
  401327:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  40132c:	e8 af 03 00 00       	call   4016e0 <runtime::heap_allocator_proc.aligned_resize-2>
  401331:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401336:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40133b:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  401340:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401344:	48 89 11             	mov    %rdx,(%rcx)
  401347:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40134e:	c3                   	ret
  40134f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401354:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401359:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  40135f:	0f 95 c0             	setne  %al
  401362:	24 01                	and    $0x1,%al
  401364:	3c 00                	cmp    $0x0,%al
  401366:	74 08                	je     401370 <runtime::heap_allocator_proc+0x1c0>
  401368:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40136d:	c6 00 db             	movb   $0xdb,(%rax)
  401370:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401375:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40137c:	00 
  40137d:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401384:	31 c0                	xor    %eax,%eax
  401386:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40138d:	c3                   	ret
  40138e:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401393:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40139a:	00 
  40139b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4013a2:	b0 04                	mov    $0x4,%al
  4013a4:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4013ab:	c3                   	ret
  4013ac:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4013b1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4013b8:	00 
  4013b9:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4013c0:	31 c0                	xor    %eax,%eax
  4013c2:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4013c9:	c3                   	ret
  4013ca:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004013d0 <runtime::heap_alloc>:
  4013d0:	48 83 ec 18          	sub    $0x18,%rsp
  4013d4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4013d8:	40 88 f0             	mov    %sil,%al
  4013db:	88 44 24 0e          	mov    %al,0xe(%rsp)
  4013df:	8a 44 24 0e          	mov    0xe(%rsp),%al
  4013e3:	48 8b 3c 24          	mov    (%rsp),%rdi
  4013e7:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4013ec:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4013f0:	0f b6 f0             	movzbl %al,%esi
  4013f3:	e8 c8 12 00 00       	call   4026c0 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  4013f8:	48 83 c4 18          	add    $0x18,%rsp
  4013fc:	c3                   	ret
  4013fd:	0f 1f 00             	nopl   (%rax)

0000000000401400 <runtime::heap_resize>:
  401400:	48 83 ec 28          	sub    $0x28,%rsp
  401404:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  401409:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40140e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401413:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401418:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40141d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  401422:	e8 f9 12 00 00       	call   402720 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  401427:	48 83 c4 28          	add    $0x28,%rsp
  40142b:	c3                   	ret
  40142c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401430 <runtime::heap_free>:
  401430:	48 83 ec 18          	sub    $0x18,%rsp
  401434:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  401439:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40143e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  401443:	e8 08 13 00 00       	call   402750 <runtime::[heap_allocator_unix.odin]::_heap_free>
  401448:	48 83 c4 18          	add    $0x18,%rsp
  40144c:	c3                   	ret
  40144d:	0f 1f 00             	nopl   (%rax)

0000000000401450 <runtime::heap_allocator_proc.aligned_alloc-0>:
  401450:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  401457:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40145c:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  401461:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  401466:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40146b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  401470:	44 88 c0             	mov    %r8b,%al
  401473:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  401477:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40147e:	00 
  40147f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401484:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401489:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40148e:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  401493:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  401497:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40149c:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  4014a3:	00 
  4014a4:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4014ab:	00 
  4014ac:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4014b3:	00 
  4014b4:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  4014bb:	00 
  4014bc:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  4014c3:	b9 08 00 00 00       	mov    $0x8,%ecx
  4014c8:	48 83 fe 08          	cmp    $0x8,%rsi
  4014cc:	48 0f 4f ce          	cmovg  %rsi,%rcx
  4014d0:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  4014d7:	00 
  4014d8:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  4014df:	00 
  4014e0:	48 83 e9 01          	sub    $0x1,%rcx
  4014e4:	48 83 c1 08          	add    $0x8,%rcx
  4014e8:	48 01 d1             	add    %rdx,%rcx
  4014eb:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  4014f2:	00 
  4014f3:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  4014fa:	00 00 
  4014fc:	48 83 f8 00          	cmp    $0x0,%rax
  401500:	0f 95 c1             	setne  %cl
  401503:	80 e1 01             	and    $0x1,%cl
  401506:	31 c0                	xor    %eax,%eax
  401508:	80 f9 00             	cmp    $0x0,%cl
  40150b:	88 44 24 0f          	mov    %al,0xf(%rsp)
  40150f:	74 17                	je     401528 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  401511:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401516:	48 83 f8 08          	cmp    $0x8,%rax
  40151a:	0f 9f c0             	setg   %al
  40151d:	24 01                	and    $0x1,%al
  40151f:	3c 00                	cmp    $0x0,%al
  401521:	0f 95 c0             	setne  %al
  401524:	88 44 24 0f          	mov    %al,0xf(%rsp)
  401528:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40152d:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  401531:	80 e1 01             	and    $0x1,%cl
  401534:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  401538:	48 83 f8 00          	cmp    $0x0,%rax
  40153c:	0f 95 c0             	setne  %al
  40153f:	24 01                	and    $0x1,%al
  401541:	3c 00                	cmp    $0x0,%al
  401543:	74 2e                	je     401573 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  401545:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40154a:	75 27                	jne    401573 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  40154c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401551:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  401555:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40155a:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40155f:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  401566:	00 
  401567:	e8 94 fe ff ff       	call   401400 <runtime::heap_resize>
  40156c:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  401571:	eb 19                	jmp    40158c <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  401573:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401577:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  40157e:	00 
  40157f:	0f b6 f0             	movzbl %al,%esi
  401582:	e8 49 fe ff ff       	call   4013d0 <runtime::heap_alloc>
  401587:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40158c:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  401591:	48 83 c0 08          	add    $0x8,%rax
  401595:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40159a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40159f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4015a4:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4015a9:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4015ae:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4015b3:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  4015ba:	00 
  4015bb:	48 83 e8 01          	sub    $0x1,%rax
  4015bf:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  4015c6:	00 
  4015c7:	48 83 e9 01          	sub    $0x1,%rcx
  4015cb:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  4015cf:	48 21 c8             	and    %rcx,%rax
  4015d2:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4015d7:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  4015dd:	0f 94 c0             	sete   %al
  4015e0:	24 01                	and    $0x1,%al
  4015e2:	3c 00                	cmp    $0x0,%al
  4015e4:	74 3c                	je     401622 <runtime::heap_allocator_proc.aligned_alloc-0+0x1d2>
  4015e6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4015eb:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4015f0:	e8 ab 00 00 00       	call   4016a0 <runtime::heap_allocator_proc.aligned_free-1>
  4015f5:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4015fa:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  4015ff:	e8 9c 00 00 00       	call   4016a0 <runtime::heap_allocator_proc.aligned_free-1>
  401604:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401609:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401610:	00 
  401611:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401618:	b0 01                	mov    $0x1,%al
  40161a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401621:	c3                   	ret
  401622:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401627:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40162c:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  401631:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  401636:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  40163a:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40163f:	74 2f                	je     401670 <runtime::heap_allocator_proc.aligned_alloc-0+0x220>
  401641:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401646:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40164b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  401650:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  401655:	48 39 d0             	cmp    %rdx,%rax
  401658:	48 0f 4c d0          	cmovl  %rax,%rdx
  40165c:	e8 5f 4e 00 00       	call   4064c0 <runtime::mem_copy_non_overlapping>
  401661:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  401666:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40166b:	e8 30 00 00 00       	call   4016a0 <runtime::heap_allocator_proc.aligned_free-1>
  401670:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  401675:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  40167a:	e8 a1 4d 00 00       	call   406420 <runtime::[internal.odin]::byte_slice>
  40167f:	48 89 c1             	mov    %rax,%rcx
  401682:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401687:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40168b:	48 89 08             	mov    %rcx,(%rax)
  40168e:	31 c0                	xor    %eax,%eax
  401690:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401697:	c3                   	ret
  401698:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40169f:	00 

00000000004016a0 <runtime::heap_allocator_proc.aligned_free-1>:
  4016a0:	48 83 ec 18          	sub    $0x18,%rsp
  4016a4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4016a9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4016ae:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4016b3:	48 83 f8 00          	cmp    $0x0,%rax
  4016b7:	0f 95 c0             	setne  %al
  4016ba:	24 01                	and    $0x1,%al
  4016bc:	3c 00                	cmp    $0x0,%al
  4016be:	74 0e                	je     4016ce <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  4016c0:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4016c5:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  4016c9:	e8 62 fd ff ff       	call   401430 <runtime::heap_free>
  4016ce:	48 83 c4 18          	add    $0x18,%rsp
  4016d2:	c3                   	ret
  4016d3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4016da:	84 00 00 00 00 00 

00000000004016e0 <runtime::heap_allocator_proc.aligned_resize-2>:
  4016e0:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  4016e7:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  4016ec:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4016f1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4016f6:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4016fb:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  401700:	44 88 c0             	mov    %r8b,%al
  401703:	88 44 24 57          	mov    %al,0x57(%rsp)
  401707:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40170e:	00 
  40170f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  401714:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401719:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  40171d:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  401722:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  401727:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40172c:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  401733:	00 
  401734:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  40173b:	00 
  40173c:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  401743:	00 
  401744:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  40174b:	00 
  40174c:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  401753:	0f 57 c0             	xorps  %xmm0,%xmm0
  401756:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  40175d:	00 
  40175e:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  401765:	00 
  401766:	48 83 f8 00          	cmp    $0x0,%rax
  40176a:	0f 94 c0             	sete   %al
  40176d:	24 01                	and    $0x1,%al
  40176f:	3c 00                	cmp    $0x0,%al
  401771:	0f 84 80 00 00 00    	je     4017f7 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  401777:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40177c:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  401781:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  401786:	8a 44 24 57          	mov    0x57(%rsp),%al
  40178a:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  40178f:	0f 57 c0             	xorps  %xmm0,%xmm0
  401792:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  401799:	00 
  40179a:	48 89 e2             	mov    %rsp,%rdx
  40179d:	4c 89 02             	mov    %r8,(%rdx)
  4017a0:	44 0f b6 c0          	movzbl %al,%r8d
  4017a4:	31 c0                	xor    %eax,%eax
  4017a6:	89 c2                	mov    %eax,%edx
  4017a8:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  4017af:	00 
  4017b0:	e8 9b fc ff ff       	call   401450 <runtime::heap_allocator_proc.aligned_alloc-0>
  4017b5:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4017ba:	40 88 c7             	mov    %al,%dil
  4017bd:	40 88 f8             	mov    %dil,%al
  4017c0:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  4017c7:	00 
  4017c8:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  4017cf:	00 
  4017d0:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  4017d7:	00 
  4017d8:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4017df:	00 
  4017e0:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  4017e7:	00 
  4017e8:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4017ec:	48 89 11             	mov    %rdx,(%rcx)
  4017ef:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4017f6:	c3                   	ret
  4017f7:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4017fc:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  401801:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  401806:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40180b:	8a 44 24 57          	mov    0x57(%rsp),%al
  40180f:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  401814:	0f 57 c0             	xorps  %xmm0,%xmm0
  401817:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40181e:	00 
  40181f:	49 89 e0             	mov    %rsp,%r8
  401822:	4d 89 08             	mov    %r9,(%r8)
  401825:	44 0f b6 c0          	movzbl %al,%r8d
  401829:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  401830:	00 
  401831:	e8 1a fc ff ff       	call   401450 <runtime::heap_allocator_proc.aligned_alloc-0>
  401836:	88 44 24 17          	mov    %al,0x17(%rsp)
  40183a:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  401841:	00 
  401842:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  401847:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40184e:	00 
  40184f:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401854:	3c 00                	cmp    $0x0,%al
  401856:	74 4d                	je     4018a5 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  401858:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40185d:	8a 44 24 17          	mov    0x17(%rsp),%al
  401861:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401868:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40186f:	00 
  401870:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  401877:	00 
  401878:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40187f:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  401886:	00 
  401887:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40188e:	00 
  40188f:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401896:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40189a:	48 89 11             	mov    %rdx,(%rcx)
  40189d:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4018a4:	c3                   	ret
  4018a5:	8a 44 24 57          	mov    0x57(%rsp),%al
  4018a9:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4018ae:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4018b3:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4018ba:	00 
  4018bb:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  4018c2:	00 
  4018c3:	3c 00                	cmp    $0x0,%al
  4018c5:	0f 84 85 00 00 00    	je     401950 <runtime::heap_allocator_proc.aligned_resize-2+0x270>
  4018cb:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4018d0:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4018d5:	48 39 c8             	cmp    %rcx,%rax
  4018d8:	0f 9f c0             	setg   %al
  4018db:	24 01                	and    $0x1,%al
  4018dd:	3c 00                	cmp    $0x0,%al
  4018df:	74 6f                	je     401950 <runtime::heap_allocator_proc.aligned_resize-2+0x270>
  4018e1:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  4018e6:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  4018ed:	00 
  4018ee:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4018f3:	48 89 e0             	mov    %rsp,%rax
  4018f6:	4c 89 08             	mov    %r9,(%rax)
  4018f9:	bf 48 90 40 00       	mov    $0x409048,%edi
  4018fe:	be 2e 00 00 00       	mov    $0x2e,%esi
  401903:	ba 4d 00 00 00       	mov    $0x4d,%edx
  401908:	b9 26 00 00 00       	mov    $0x26,%ecx
  40190d:	e8 2e 1e 00 00       	call   403740 <runtime::slice_expr_error_lo_hi>
  401912:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401917:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40191c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401921:	48 89 c2             	mov    %rax,%rdx
  401924:	48 03 94 24 e0 00 00 	add    0xe0(%rsp),%rdx
  40192b:	00 
  40192c:	48 29 c1             	sub    %rax,%rcx
  40192f:	48 89 54 24 68       	mov    %rdx,0x68(%rsp)
  401934:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  401939:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40193e:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  401943:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  401948:	48 29 c6             	sub    %rax,%rsi
  40194b:	e8 00 50 00 00       	call   406950 <runtime::conditional_mem_zero>
  401950:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  401955:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40195c:	00 
  40195d:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  401964:	00 
  401965:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40196c:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  401973:	00 
  401974:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40197b:	00 
  40197c:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401983:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401987:	48 89 11             	mov    %rdx,(%rcx)
  40198a:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  401991:	c3                   	ret
  401992:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401999:	00 00 00 
  40199c:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004019a0 <runtime::[os_specific_linux.odin]::_stderr_write>:
  4019a0:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4019a5:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  4019aa:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  4019af:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  4019b4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  4019b9:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  4019be:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4019c3:	b8 01 00 00 00       	mov    $0x1,%eax
  4019c8:	bf 02 00 00 00       	mov    $0x2,%edi
  4019cd:	0f 05                	syscall
  4019cf:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4019d4:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  4019da:	0f 9c c0             	setl   %al
  4019dd:	24 01                	and    $0x1,%al
  4019df:	3c 00                	cmp    $0x0,%al
  4019e1:	74 26                	je     401a09 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4019e3:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  4019ea:	ff ff 
  4019ec:	0f 9f c0             	setg   %al
  4019ef:	24 01                	and    $0x1,%al
  4019f1:	3c 00                	cmp    $0x0,%al
  4019f3:	74 14                	je     401a09 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4019f5:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  4019fa:	31 c0                	xor    %eax,%eax
  4019fc:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  401a01:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  401a08:	c3                   	ret
  401a09:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401a0e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401a13:	48 89 08             	mov    %rcx,(%rax)
  401a16:	31 c0                	xor    %eax,%eax
  401a18:	c3                   	ret
  401a19:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401a20 <runtime::udivmod128>:
  401a20:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  401a27:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  401a2c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  401a31:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  401a36:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  401a3b:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  401a40:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  401a45:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401a4a:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  401a4f:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  401a54:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401a59:	48 89 94 24 20 01 00 	mov    %rdx,0x120(%rsp)
  401a60:	00 
  401a61:	48 89 b4 24 28 01 00 	mov    %rsi,0x128(%rsp)
  401a68:	00 
  401a69:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  401a70:	00 
  401a71:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  401a78:	00 
  401a79:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  401a80:	00 
  401a81:	48 89 b4 24 f8 00 00 	mov    %rsi,0xf8(%rsp)
  401a88:	00 
  401a89:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  401a90:	00 
  401a91:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  401a98:	00 
  401a99:	48 8b b4 24 f8 00 00 	mov    0xf8(%rsp),%rsi
  401aa0:	00 
  401aa1:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  401aa8:	00 
  401aa9:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  401ab0:	00 
  401ab1:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  401ab8:	00 
  401ab9:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  401ac0:	00 
  401ac1:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  401ac8:	00 
  401ac9:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  401ad0:	00 
  401ad1:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  401ad8:	00 
  401ad9:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  401ae0:	00 
  401ae1:	0f 57 c0             	xorps  %xmm0,%xmm0
  401ae4:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  401aeb:	00 
  401aec:	0f 57 c0             	xorps  %xmm0,%xmm0
  401aef:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  401af6:	00 
  401af7:	c7 84 24 9c 00 00 00 	movl   $0x0,0x9c(%rsp)
  401afe:	00 00 00 00 
  401b02:	48 83 bc 24 e8 00 00 	cmpq   $0x0,0xe8(%rsp)
  401b09:	00 00 
  401b0b:	0f 94 c0             	sete   %al
  401b0e:	24 01                	and    $0x1,%al
  401b10:	3c 00                	cmp    $0x0,%al
  401b12:	0f 84 05 01 00 00    	je     401c1d <runtime::udivmod128+0x1fd>
  401b18:	48 83 bc 24 c8 00 00 	cmpq   $0x0,0xc8(%rsp)
  401b1f:	00 00 
  401b21:	0f 94 c0             	sete   %al
  401b24:	24 01                	and    $0x1,%al
  401b26:	3c 00                	cmp    $0x0,%al
  401b28:	0f 84 b6 00 00 00    	je     401be4 <runtime::udivmod128+0x1c4>
  401b2e:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401b33:	48 83 f8 00          	cmp    $0x0,%rax
  401b37:	0f 95 c0             	setne  %al
  401b3a:	24 01                	and    $0x1,%al
  401b3c:	3c 00                	cmp    $0x0,%al
  401b3e:	74 5d                	je     401b9d <runtime::udivmod128+0x17d>
  401b40:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401b47:	00 
  401b48:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  401b4d:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  401b54:	00 
  401b55:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401b5a:	48 83 f8 00          	cmp    $0x0,%rax
  401b5e:	74 16                	je     401b76 <runtime::udivmod128+0x156>
  401b60:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401b65:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401b6a:	31 d2                	xor    %edx,%edx
  401b6c:	48 f7 f1             	div    %rcx
  401b6f:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  401b74:	eb 02                	jmp    401b78 <runtime::udivmod128+0x158>
  401b76:	0f 0b                	ud2
  401b78:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401b7d:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401b82:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  401b89:	00 
  401b8a:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  401b91:	00 
  401b92:	48 89 08             	mov    %rcx,(%rax)
  401b95:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401b9c:	00 
  401b9d:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401ba4:	00 
  401ba5:	48 89 04 24          	mov    %rax,(%rsp)
  401ba9:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  401bb0:	00 
  401bb1:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  401bb6:	48 83 f8 00          	cmp    $0x0,%rax
  401bba:	74 15                	je     401bd1 <runtime::udivmod128+0x1b1>
  401bbc:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401bc1:	48 8b 04 24          	mov    (%rsp),%rax
  401bc5:	31 d2                	xor    %edx,%edx
  401bc7:	48 f7 f1             	div    %rcx
  401bca:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401bcf:	eb 02                	jmp    401bd3 <runtime::udivmod128+0x1b3>
  401bd1:	0f 0b                	ud2
  401bd3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401bd8:	31 c9                	xor    %ecx,%ecx
  401bda:	89 ca                	mov    %ecx,%edx
  401bdc:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401be3:	c3                   	ret
  401be4:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401be9:	48 83 f8 00          	cmp    $0x0,%rax
  401bed:	0f 95 c0             	setne  %al
  401bf0:	24 01                	and    $0x1,%al
  401bf2:	3c 00                	cmp    $0x0,%al
  401bf4:	74 18                	je     401c0e <runtime::udivmod128+0x1ee>
  401bf6:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401bfb:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  401c02:	00 
  401c03:	48 89 08             	mov    %rcx,(%rax)
  401c06:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401c0d:	00 
  401c0e:	31 c0                	xor    %eax,%eax
  401c10:	89 c2                	mov    %eax,%edx
  401c12:	48 89 d0             	mov    %rdx,%rax
  401c15:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401c1c:	c3                   	ret
  401c1d:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  401c24:	00 00 
  401c26:	0f 94 c0             	sete   %al
  401c29:	24 01                	and    $0x1,%al
  401c2b:	3c 00                	cmp    $0x0,%al
  401c2d:	0f 84 bd 03 00 00    	je     401ff0 <runtime::udivmod128+0x5d0>
  401c33:	48 83 bc 24 c8 00 00 	cmpq   $0x0,0xc8(%rsp)
  401c3a:	00 00 
  401c3c:	0f 94 c0             	sete   %al
  401c3f:	24 01                	and    $0x1,%al
  401c41:	3c 00                	cmp    $0x0,%al
  401c43:	0f 84 a8 00 00 00    	je     401cf1 <runtime::udivmod128+0x2d1>
  401c49:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401c4e:	48 83 f8 00          	cmp    $0x0,%rax
  401c52:	0f 95 c0             	setne  %al
  401c55:	24 01                	and    $0x1,%al
  401c57:	3c 00                	cmp    $0x0,%al
  401c59:	74 4d                	je     401ca8 <runtime::udivmod128+0x288>
  401c5b:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401c62:	00 
  401c63:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401c68:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  401c6f:	00 
  401c70:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401c75:	48 83 f8 00          	cmp    $0x0,%rax
  401c79:	74 16                	je     401c91 <runtime::udivmod128+0x271>
  401c7b:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401c80:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  401c85:	31 d2                	xor    %edx,%edx
  401c87:	48 f7 f1             	div    %rcx
  401c8a:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  401c8f:	eb 02                	jmp    401c93 <runtime::udivmod128+0x273>
  401c91:	0f 0b                	ud2
  401c93:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401c98:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401c9d:	48 89 08             	mov    %rcx,(%rax)
  401ca0:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401ca7:	00 
  401ca8:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401caf:	00 
  401cb0:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  401cb5:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  401cbc:	00 
  401cbd:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401cc2:	48 83 f8 00          	cmp    $0x0,%rax
  401cc6:	74 16                	je     401cde <runtime::udivmod128+0x2be>
  401cc8:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  401ccd:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401cd2:	31 d2                	xor    %edx,%edx
  401cd4:	48 f7 f1             	div    %rcx
  401cd7:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  401cdc:	eb 02                	jmp    401ce0 <runtime::udivmod128+0x2c0>
  401cde:	0f 0b                	ud2
  401ce0:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401ce5:	31 c9                	xor    %ecx,%ecx
  401ce7:	89 ca                	mov    %ecx,%edx
  401ce9:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401cf0:	c3                   	ret
  401cf1:	48 83 bc 24 e0 00 00 	cmpq   $0x0,0xe0(%rsp)
  401cf8:	00 00 
  401cfa:	0f 94 c0             	sete   %al
  401cfd:	24 01                	and    $0x1,%al
  401cff:	3c 00                	cmp    $0x0,%al
  401d01:	0f 84 d9 00 00 00    	je     401de0 <runtime::udivmod128+0x3c0>
  401d07:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401d0c:	48 83 f8 00          	cmp    $0x0,%rax
  401d10:	0f 95 c0             	setne  %al
  401d13:	24 01                	and    $0x1,%al
  401d15:	3c 00                	cmp    $0x0,%al
  401d17:	74 7e                	je     401d97 <runtime::udivmod128+0x377>
  401d19:	48 8d 84 24 a0 00 00 	lea    0xa0(%rsp),%rax
  401d20:	00 
  401d21:	48 83 c0 08          	add    $0x8,%rax
  401d25:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  401d2a:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401d31:	00 
  401d32:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  401d37:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  401d3e:	00 
  401d3f:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  401d44:	48 83 f8 00          	cmp    $0x0,%rax
  401d48:	74 16                	je     401d60 <runtime::udivmod128+0x340>
  401d4a:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  401d4f:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401d54:	31 d2                	xor    %edx,%edx
  401d56:	48 f7 f1             	div    %rcx
  401d59:	48 89 54 24 a8       	mov    %rdx,-0x58(%rsp)
  401d5e:	eb 02                	jmp    401d62 <runtime::udivmod128+0x342>
  401d60:	0f 0b                	ud2
  401d62:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401d67:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  401d6c:	48 8b 54 24 a8       	mov    -0x58(%rsp),%rdx
  401d71:	48 89 11             	mov    %rdx,(%rcx)
  401d74:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  401d7b:	00 00 00 00 00 
  401d80:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  401d87:	00 
  401d88:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  401d8f:	00 
  401d90:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401d94:	48 89 08             	mov    %rcx,(%rax)
  401d97:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401d9e:	00 
  401d9f:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  401da4:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  401dab:	00 
  401dac:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  401db1:	48 83 f8 00          	cmp    $0x0,%rax
  401db5:	74 16                	je     401dcd <runtime::udivmod128+0x3ad>
  401db7:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  401dbc:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  401dc1:	31 d2                	xor    %edx,%edx
  401dc3:	48 f7 f1             	div    %rcx
  401dc6:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  401dcb:	eb 02                	jmp    401dcf <runtime::udivmod128+0x3af>
  401dcd:	0f 0b                	ud2
  401dcf:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  401dd4:	31 c9                	xor    %ecx,%ecx
  401dd6:	89 ca                	mov    %ecx,%edx
  401dd8:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401ddf:	c3                   	ret
  401de0:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  401de7:	00 
  401de8:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  401def:	00 
  401df0:	48 83 e9 01          	sub    $0x1,%rcx
  401df4:	48 21 c8             	and    %rcx,%rax
  401df7:	48 83 f8 00          	cmp    $0x0,%rax
  401dfb:	0f 94 c0             	sete   %al
  401dfe:	24 01                	and    $0x1,%al
  401e00:	3c 00                	cmp    $0x0,%al
  401e02:	0f 84 95 00 00 00    	je     401e9d <runtime::udivmod128+0x47d>
  401e08:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401e0d:	48 83 f8 00          	cmp    $0x0,%rax
  401e11:	0f 95 c0             	setne  %al
  401e14:	24 01                	and    $0x1,%al
  401e16:	3c 00                	cmp    $0x0,%al
  401e18:	74 4a                	je     401e64 <runtime::udivmod128+0x444>
  401e1a:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401e1f:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  401e26:	00 
  401e27:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  401e2e:	00 
  401e2f:	48 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%rcx
  401e36:	00 
  401e37:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  401e3e:	00 
  401e3f:	48 ff ca             	dec    %rdx
  401e42:	48 21 d1             	and    %rdx,%rcx
  401e45:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  401e4c:	00 
  401e4d:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  401e54:	00 
  401e55:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  401e5c:	00 
  401e5d:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401e61:	48 89 08             	mov    %rcx,(%rax)
  401e64:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401e6b:	00 
  401e6c:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  401e73:	00 
  401e74:	ba 40 00 00 00       	mov    $0x40,%edx
  401e79:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  401e7e:	88 d1                	mov    %dl,%cl
  401e80:	48 d3 e8             	shr    %cl,%rax
  401e83:	48 89 c1             	mov    %rax,%rcx
  401e86:	31 c0                	xor    %eax,%eax
  401e88:	48 83 ea 40          	sub    $0x40,%rdx
  401e8c:	89 c2                	mov    %eax,%edx
  401e8e:	48 89 d0             	mov    %rdx,%rax
  401e91:	48 0f 42 c1          	cmovb  %rcx,%rax
  401e95:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401e9c:	c3                   	ret
  401e9d:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  401ea4:	00 
  401ea5:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401eaa:	48 0f bd c1          	bsr    %rcx,%rax
  401eae:	48 83 f0 3f          	xor    $0x3f,%rax
  401eb2:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  401eb9:	00 
  401eba:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401ebf:	48 0f bd ca          	bsr    %rdx,%rcx
  401ec3:	48 83 f1 3f          	xor    $0x3f,%rcx
  401ec7:	29 c8                	sub    %ecx,%eax
  401ec9:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  401ed0:	83 bc 24 9c 00 00 00 	cmpl   $0x3e,0x9c(%rsp)
  401ed7:	3e 
  401ed8:	0f 97 c0             	seta   %al
  401edb:	24 01                	and    $0x1,%al
  401edd:	3c 00                	cmp    $0x0,%al
  401edf:	74 37                	je     401f18 <runtime::udivmod128+0x4f8>
  401ee1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401ee6:	48 83 f8 00          	cmp    $0x0,%rax
  401eea:	0f 95 c0             	setne  %al
  401eed:	24 01                	and    $0x1,%al
  401eef:	3c 00                	cmp    $0x0,%al
  401ef1:	74 16                	je     401f09 <runtime::udivmod128+0x4e9>
  401ef3:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401ef8:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401efd:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  401f02:	48 89 10             	mov    %rdx,(%rax)
  401f05:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401f09:	31 c0                	xor    %eax,%eax
  401f0b:	89 c2                	mov    %eax,%edx
  401f0d:	48 89 d0             	mov    %rdx,%rax
  401f10:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401f17:	c3                   	ret
  401f18:	8b 84 24 9c 00 00 00 	mov    0x9c(%rsp),%eax
  401f1f:	83 c0 01             	add    $0x1,%eax
  401f22:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  401f29:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  401f30:	00 00 00 00 00 
  401f35:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401f3c:	00 
  401f3d:	b9 40 00 00 00       	mov    $0x40,%ecx
  401f42:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  401f49:	89 c9                	mov    %ecx,%ecx
  401f4b:	89 ca                	mov    %ecx,%edx
  401f4d:	48 89 d1             	mov    %rdx,%rcx
  401f50:	48 d3 e0             	shl    %cl,%rax
  401f53:	48 89 c1             	mov    %rax,%rcx
  401f56:	31 c0                	xor    %eax,%eax
  401f58:	48 83 fa 40          	cmp    $0x40,%rdx
  401f5c:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f60:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  401f67:	00 
  401f68:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401f6f:	00 
  401f70:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  401f77:	89 ca                	mov    %ecx,%edx
  401f79:	48 89 d1             	mov    %rdx,%rcx
  401f7c:	48 d3 e8             	shr    %cl,%rax
  401f7f:	48 89 c1             	mov    %rax,%rcx
  401f82:	31 c0                	xor    %eax,%eax
  401f84:	48 83 fa 40          	cmp    $0x40,%rdx
  401f88:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f8c:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401f93:	00 
  401f94:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401f9b:	00 
  401f9c:	b9 40 00 00 00       	mov    $0x40,%ecx
  401fa1:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  401fa8:	89 c9                	mov    %ecx,%ecx
  401faa:	89 ca                	mov    %ecx,%edx
  401fac:	48 89 d1             	mov    %rdx,%rcx
  401faf:	48 d3 e0             	shl    %cl,%rax
  401fb2:	48 89 c1             	mov    %rax,%rcx
  401fb5:	31 c0                	xor    %eax,%eax
  401fb7:	48 83 fa 40          	cmp    $0x40,%rdx
  401fbb:	48 0f 42 c1          	cmovb  %rcx,%rax
  401fbf:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  401fc6:	00 
  401fc7:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  401fce:	89 ce                	mov    %ecx,%esi
  401fd0:	48 89 f1             	mov    %rsi,%rcx
  401fd3:	48 d3 ea             	shr    %cl,%rdx
  401fd6:	31 c9                	xor    %ecx,%ecx
  401fd8:	48 83 fe 40          	cmp    $0x40,%rsi
  401fdc:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401fe0:	48 09 c8             	or     %rcx,%rax
  401fe3:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401fea:	00 
  401feb:	e9 14 05 00 00       	jmp    402504 <runtime::udivmod128+0xae4>
  401ff0:	48 83 bc 24 c8 00 00 	cmpq   $0x0,0xc8(%rsp)
  401ff7:	00 00 
  401ff9:	0f 94 c0             	sete   %al
  401ffc:	24 01                	and    $0x1,%al
  401ffe:	3c 00                	cmp    $0x0,%al
  402000:	0f 84 6a 03 00 00    	je     402370 <runtime::udivmod128+0x950>
  402006:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40200d:	00 
  40200e:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402015:	00 
  402016:	48 83 e9 01          	sub    $0x1,%rcx
  40201a:	48 21 c8             	and    %rcx,%rax
  40201d:	48 83 f8 00          	cmp    $0x0,%rax
  402021:	0f 94 c0             	sete   %al
  402024:	24 01                	and    $0x1,%al
  402026:	3c 00                	cmp    $0x0,%al
  402028:	0f 84 08 01 00 00    	je     402136 <runtime::udivmod128+0x716>
  40202e:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402033:	48 83 f8 00          	cmp    $0x0,%rax
  402037:	0f 95 c0             	setne  %al
  40203a:	24 01                	and    $0x1,%al
  40203c:	3c 00                	cmp    $0x0,%al
  40203e:	74 26                	je     402066 <runtime::udivmod128+0x646>
  402040:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402045:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  40204c:	00 
  40204d:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  402054:	00 
  402055:	48 ff ca             	dec    %rdx
  402058:	48 21 d1             	and    %rdx,%rcx
  40205b:	48 89 08             	mov    %rcx,(%rax)
  40205e:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402065:	00 
  402066:	48 83 bc 24 c0 00 00 	cmpq   $0x1,0xc0(%rsp)
  40206d:	00 01 
  40206f:	0f 94 c0             	sete   %al
  402072:	24 01                	and    $0x1,%al
  402074:	3c 00                	cmp    $0x0,%al
  402076:	74 12                	je     40208a <runtime::udivmod128+0x66a>
  402078:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40207d:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402082:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  402089:	c3                   	ret
  40208a:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402091:	00 
  402092:	b8 40 00 00 00       	mov    $0x40,%eax
  402097:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  40209c:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  4020a3:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4020aa:	00 
  4020ab:	8b 84 24 9c 00 00 00 	mov    0x9c(%rsp),%eax
  4020b2:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  4020b6:	88 c1                	mov    %al,%cl
  4020b8:	48 d3 ea             	shr    %cl,%rdx
  4020bb:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  4020bf:	31 c0                	xor    %eax,%eax
  4020c1:	83 e9 40             	sub    $0x40,%ecx
  4020c4:	48 89 c1             	mov    %rax,%rcx
  4020c7:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4020cb:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  4020d2:	00 
  4020d3:	48 8b bc 24 e0 00 00 	mov    0xe0(%rsp),%rdi
  4020da:	00 
  4020db:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4020e2:	00 
  4020e3:	8b b4 24 9c 00 00 00 	mov    0x9c(%rsp),%esi
  4020ea:	40 88 f1             	mov    %sil,%cl
  4020ed:	48 d3 ef             	shr    %cl,%rdi
  4020f0:	83 ee 40             	sub    $0x40,%esi
  4020f3:	48 89 c1             	mov    %rax,%rcx
  4020f6:	48 0f 42 cf          	cmovb  %rdi,%rcx
  4020fa:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  4020ff:	f7 de                	neg    %esi
  402101:	40 88 f1             	mov    %sil,%cl
  402104:	48 d3 e2             	shl    %cl,%rdx
  402107:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  40210c:	83 ee 40             	sub    $0x40,%esi
  40210f:	48 0f 42 c2          	cmovb  %rdx,%rax
  402113:	48 09 c8             	or     %rcx,%rax
  402116:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  40211d:	00 
  40211e:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  402125:	00 
  402126:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  40212d:	00 
  40212e:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  402135:	c3                   	ret
  402136:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  40213d:	00 
  40213e:	b8 7f 00 00 00       	mov    $0x7f,%eax
  402143:	48 0f bd c1          	bsr    %rcx,%rax
  402147:	48 83 f0 3f          	xor    $0x3f,%rax
  40214b:	83 c0 41             	add    $0x41,%eax
  40214e:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  402155:	00 
  402156:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40215b:	48 0f bd ca          	bsr    %rdx,%rcx
  40215f:	48 83 f1 3f          	xor    $0x3f,%rcx
  402163:	29 c8                	sub    %ecx,%eax
  402165:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  40216c:	83 bc 24 9c 00 00 00 	cmpl   $0x40,0x9c(%rsp)
  402173:	40 
  402174:	0f 94 c1             	sete   %cl
  402177:	80 e1 01             	and    $0x1,%cl
  40217a:	b0 01                	mov    $0x1,%al
  40217c:	38 c8                	cmp    %cl,%al
  40217e:	74 16                	je     402196 <runtime::udivmod128+0x776>
  402180:	83 bc 24 9c 00 00 00 	cmpl   $0x40,0x9c(%rsp)
  402187:	40 
  402188:	0f 92 c1             	setb   %cl
  40218b:	80 e1 01             	and    $0x1,%cl
  40218e:	b0 01                	mov    $0x1,%al
  402190:	38 c8                	cmp    %cl,%al
  402192:	74 44                	je     4021d8 <runtime::udivmod128+0x7b8>
  402194:	eb 3d                	jmp    4021d3 <runtime::udivmod128+0x7b3>
  402196:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  40219d:	00 00 00 00 00 
  4021a2:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4021a9:	00 
  4021aa:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4021b1:	00 
  4021b2:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  4021b9:	00 00 00 00 00 
  4021be:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4021c5:	00 
  4021c6:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4021cd:	00 
  4021ce:	e9 98 01 00 00       	jmp    40236b <runtime::udivmod128+0x94b>
  4021d3:	e9 c7 00 00 00       	jmp    40229f <runtime::udivmod128+0x87f>
  4021d8:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  4021df:	00 00 00 00 00 
  4021e4:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4021eb:	00 
  4021ec:	b9 40 00 00 00       	mov    $0x40,%ecx
  4021f1:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  4021f8:	89 c9                	mov    %ecx,%ecx
  4021fa:	89 ca                	mov    %ecx,%edx
  4021fc:	48 89 d1             	mov    %rdx,%rcx
  4021ff:	48 d3 e0             	shl    %cl,%rax
  402202:	48 89 c1             	mov    %rax,%rcx
  402205:	31 c0                	xor    %eax,%eax
  402207:	48 83 fa 40          	cmp    $0x40,%rdx
  40220b:	48 0f 42 c1          	cmovb  %rcx,%rax
  40220f:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  402216:	00 
  402217:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40221e:	00 
  40221f:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  402226:	89 ca                	mov    %ecx,%edx
  402228:	48 89 d1             	mov    %rdx,%rcx
  40222b:	48 d3 e8             	shr    %cl,%rax
  40222e:	48 89 c1             	mov    %rax,%rcx
  402231:	31 c0                	xor    %eax,%eax
  402233:	48 83 fa 40          	cmp    $0x40,%rdx
  402237:	48 0f 42 c1          	cmovb  %rcx,%rax
  40223b:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  402242:	00 
  402243:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40224a:	00 
  40224b:	b9 40 00 00 00       	mov    $0x40,%ecx
  402250:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  402257:	89 c9                	mov    %ecx,%ecx
  402259:	89 ca                	mov    %ecx,%edx
  40225b:	48 89 d1             	mov    %rdx,%rcx
  40225e:	48 d3 e0             	shl    %cl,%rax
  402261:	48 89 c1             	mov    %rax,%rcx
  402264:	31 c0                	xor    %eax,%eax
  402266:	48 83 fa 40          	cmp    $0x40,%rdx
  40226a:	48 0f 42 c1          	cmovb  %rcx,%rax
  40226e:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  402275:	00 
  402276:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  40227d:	89 ce                	mov    %ecx,%esi
  40227f:	48 89 f1             	mov    %rsi,%rcx
  402282:	48 d3 ea             	shr    %cl,%rdx
  402285:	31 c9                	xor    %ecx,%ecx
  402287:	48 83 fe 40          	cmp    $0x40,%rsi
  40228b:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40228f:	48 09 c8             	or     %rcx,%rax
  402292:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402299:	00 
  40229a:	e9 cc 00 00 00       	jmp    40236b <runtime::udivmod128+0x94b>
  40229f:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4022a6:	00 
  4022a7:	b9 80 00 00 00       	mov    $0x80,%ecx
  4022ac:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  4022b3:	89 c9                	mov    %ecx,%ecx
  4022b5:	89 ca                	mov    %ecx,%edx
  4022b7:	48 89 d1             	mov    %rdx,%rcx
  4022ba:	48 d3 e0             	shl    %cl,%rax
  4022bd:	48 89 c1             	mov    %rax,%rcx
  4022c0:	31 c0                	xor    %eax,%eax
  4022c2:	48 83 fa 40          	cmp    $0x40,%rdx
  4022c6:	48 0f 42 c1          	cmovb  %rcx,%rax
  4022ca:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  4022d1:	00 
  4022d2:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4022d9:	00 
  4022da:	b9 80 00 00 00       	mov    $0x80,%ecx
  4022df:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  4022e6:	89 c9                	mov    %ecx,%ecx
  4022e8:	89 ca                	mov    %ecx,%edx
  4022ea:	48 89 d1             	mov    %rdx,%rcx
  4022ed:	48 d3 e0             	shl    %cl,%rax
  4022f0:	48 89 c1             	mov    %rax,%rcx
  4022f3:	31 c0                	xor    %eax,%eax
  4022f5:	48 83 fa 40          	cmp    $0x40,%rdx
  4022f9:	48 0f 42 c1          	cmovb  %rcx,%rax
  4022fd:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  402304:	00 
  402305:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  40230c:	83 e9 40             	sub    $0x40,%ecx
  40230f:	89 c9                	mov    %ecx,%ecx
  402311:	89 ce                	mov    %ecx,%esi
  402313:	48 89 f1             	mov    %rsi,%rcx
  402316:	48 d3 ea             	shr    %cl,%rdx
  402319:	31 c9                	xor    %ecx,%ecx
  40231b:	48 83 fe 40          	cmp    $0x40,%rsi
  40231f:	48 0f 42 ca          	cmovb  %rdx,%rcx
  402323:	48 09 c8             	or     %rcx,%rax
  402326:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  40232d:	00 
  40232e:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  402335:	00 00 00 00 00 
  40233a:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  402341:	00 
  402342:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  402349:	83 e9 40             	sub    $0x40,%ecx
  40234c:	89 c9                	mov    %ecx,%ecx
  40234e:	89 ca                	mov    %ecx,%edx
  402350:	48 89 d1             	mov    %rdx,%rcx
  402353:	48 d3 e8             	shr    %cl,%rax
  402356:	48 89 c1             	mov    %rax,%rcx
  402359:	31 c0                	xor    %eax,%eax
  40235b:	48 83 fa 40          	cmp    $0x40,%rdx
  40235f:	48 0f 42 c1          	cmovb  %rcx,%rax
  402363:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40236a:	00 
  40236b:	e9 92 01 00 00       	jmp    402502 <runtime::udivmod128+0xae2>
  402370:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  402377:	00 
  402378:	b8 7f 00 00 00       	mov    $0x7f,%eax
  40237d:	48 0f bd c1          	bsr    %rcx,%rax
  402381:	48 83 f0 3f          	xor    $0x3f,%rax
  402385:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  40238c:	00 
  40238d:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  402392:	48 0f bd ca          	bsr    %rdx,%rcx
  402396:	48 83 f1 3f          	xor    $0x3f,%rcx
  40239a:	29 c8                	sub    %ecx,%eax
  40239c:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  4023a3:	83 bc 24 9c 00 00 00 	cmpl   $0x3f,0x9c(%rsp)
  4023aa:	3f 
  4023ab:	0f 97 c0             	seta   %al
  4023ae:	24 01                	and    $0x1,%al
  4023b0:	3c 00                	cmp    $0x0,%al
  4023b2:	74 37                	je     4023eb <runtime::udivmod128+0x9cb>
  4023b4:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4023b9:	48 83 f8 00          	cmp    $0x0,%rax
  4023bd:	0f 95 c0             	setne  %al
  4023c0:	24 01                	and    $0x1,%al
  4023c2:	3c 00                	cmp    $0x0,%al
  4023c4:	74 16                	je     4023dc <runtime::udivmod128+0x9bc>
  4023c6:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4023cb:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4023d0:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4023d5:	48 89 10             	mov    %rdx,(%rax)
  4023d8:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4023dc:	31 c0                	xor    %eax,%eax
  4023de:	89 c2                	mov    %eax,%edx
  4023e0:	48 89 d0             	mov    %rdx,%rax
  4023e3:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  4023ea:	c3                   	ret
  4023eb:	8b 84 24 9c 00 00 00 	mov    0x9c(%rsp),%eax
  4023f2:	83 c0 01             	add    $0x1,%eax
  4023f5:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  4023fc:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  402403:	00 00 00 00 00 
  402408:	83 bc 24 9c 00 00 00 	cmpl   $0x40,0x9c(%rsp)
  40240f:	40 
  402410:	0f 94 c0             	sete   %al
  402413:	24 01                	and    $0x1,%al
  402415:	3c 00                	cmp    $0x0,%al
  402417:	74 31                	je     40244a <runtime::udivmod128+0xa2a>
  402419:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  402420:	00 
  402421:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  402428:	00 
  402429:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  402430:	00 00 00 00 00 
  402435:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40243c:	00 
  40243d:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402444:	00 
  402445:	e9 b6 00 00 00       	jmp    402500 <runtime::udivmod128+0xae0>
  40244a:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  402451:	00 
  402452:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  402459:	89 ca                	mov    %ecx,%edx
  40245b:	48 89 d1             	mov    %rdx,%rcx
  40245e:	48 d3 e8             	shr    %cl,%rax
  402461:	48 89 c1             	mov    %rax,%rcx
  402464:	31 c0                	xor    %eax,%eax
  402466:	48 83 fa 40          	cmp    $0x40,%rdx
  40246a:	48 0f 42 c1          	cmovb  %rcx,%rax
  40246e:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  402475:	00 
  402476:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40247d:	00 
  40247e:	b9 40 00 00 00       	mov    $0x40,%ecx
  402483:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  40248a:	89 c9                	mov    %ecx,%ecx
  40248c:	89 ca                	mov    %ecx,%edx
  40248e:	48 89 d1             	mov    %rdx,%rcx
  402491:	48 d3 e0             	shl    %cl,%rax
  402494:	48 89 c1             	mov    %rax,%rcx
  402497:	31 c0                	xor    %eax,%eax
  402499:	48 83 fa 40          	cmp    $0x40,%rdx
  40249d:	48 0f 42 c1          	cmovb  %rcx,%rax
  4024a1:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4024a8:	00 
  4024a9:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  4024b0:	89 ce                	mov    %ecx,%esi
  4024b2:	48 89 f1             	mov    %rsi,%rcx
  4024b5:	48 d3 ea             	shr    %cl,%rdx
  4024b8:	31 c9                	xor    %ecx,%ecx
  4024ba:	48 83 fe 40          	cmp    $0x40,%rsi
  4024be:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4024c2:	48 09 c8             	or     %rcx,%rax
  4024c5:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4024cc:	00 
  4024cd:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4024d4:	00 
  4024d5:	b9 40 00 00 00       	mov    $0x40,%ecx
  4024da:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  4024e1:	89 c9                	mov    %ecx,%ecx
  4024e3:	89 ca                	mov    %ecx,%edx
  4024e5:	48 89 d1             	mov    %rdx,%rcx
  4024e8:	48 d3 e0             	shl    %cl,%rax
  4024eb:	48 89 c1             	mov    %rax,%rcx
  4024ee:	31 c0                	xor    %eax,%eax
  4024f0:	48 83 fa 40          	cmp    $0x40,%rdx
  4024f4:	48 0f 42 c1          	cmovb  %rcx,%rax
  4024f8:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4024ff:	00 
  402500:	eb 00                	jmp    402502 <runtime::udivmod128+0xae2>
  402502:	eb 00                	jmp    402504 <runtime::udivmod128+0xae4>
  402504:	c7 84 24 8c 00 00 00 	movl   $0x0,0x8c(%rsp)
  40250b:	00 00 00 00 
  40250f:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  402516:	00 00 
  402518:	48 c7 44 24 70 00 00 	movq   $0x0,0x70(%rsp)
  40251f:	00 00 
  402521:	83 bc 24 9c 00 00 00 	cmpl   $0x0,0x9c(%rsp)
  402528:	00 
  402529:	0f 97 c0             	seta   %al
  40252c:	24 01                	and    $0x1,%al
  40252e:	3c 00                	cmp    $0x0,%al
  402530:	0f 84 24 01 00 00    	je     40265a <runtime::udivmod128+0xc3a>
  402536:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40253b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402540:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  402547:	00 
  402548:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40254f:	00 
  402550:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  402555:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  40255c:	00 
  40255d:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  402564:	00 
  402565:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  40256c:	00 
  40256d:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  402572:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402579:	00 
  40257a:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  402581:	00 
  402582:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  402589:	00 
  40258a:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  40258f:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  402596:	00 
  402597:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40259e:	00 
  40259f:	48 01 c0             	add    %rax,%rax
  4025a2:	8b 8c 24 8c 00 00 00 	mov    0x8c(%rsp),%ecx
  4025a9:	48 09 c8             	or     %rcx,%rax
  4025ac:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  4025b3:	00 
  4025b4:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  4025bb:	00 
  4025bc:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  4025c3:	00 
  4025c4:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  4025c9:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4025ce:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4025d3:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4025d8:	48 f7 d0             	not    %rax
  4025db:	48 f7 d1             	not    %rcx
  4025de:	48 01 f1             	add    %rsi,%rcx
  4025e1:	48 11 d0             	adc    %rdx,%rax
  4025e4:	48 c1 f8 3f          	sar    $0x3f,%rax
  4025e8:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4025ed:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4025f2:	8b 44 24 60          	mov    0x60(%rsp),%eax
  4025f6:	83 e0 01             	and    $0x1,%eax
  4025f9:	89 84 24 8c 00 00 00 	mov    %eax,0x8c(%rsp)
  402600:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402605:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40260a:	48 21 ca             	and    %rcx,%rdx
  40260d:	48 21 c6             	and    %rax,%rsi
  402610:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  402615:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40261a:	48 29 f1             	sub    %rsi,%rcx
  40261d:	48 19 d0             	sbb    %rdx,%rax
  402620:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  402625:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40262a:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40262f:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  402634:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  40263b:	00 
  40263c:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402643:	00 
  402644:	8b 84 24 9c 00 00 00 	mov    0x9c(%rsp),%eax
  40264b:	83 e8 01             	sub    $0x1,%eax
  40264e:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  402655:	e9 c7 fe ff ff       	jmp    402521 <runtime::udivmod128+0xb01>
  40265a:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40265f:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  402666:	00 
  402667:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  40266e:	00 
  40266f:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  402674:	48 01 c9             	add    %rcx,%rcx
  402677:	8b b4 24 8c 00 00 00 	mov    0x8c(%rsp),%esi
  40267e:	48 09 f1             	or     %rsi,%rcx
  402681:	48 89 54 24 58       	mov    %rdx,0x58(%rsp)
  402686:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  40268b:	48 83 f8 00          	cmp    $0x0,%rax
  40268f:	0f 95 c0             	setne  %al
  402692:	24 01                	and    $0x1,%al
  402694:	3c 00                	cmp    $0x0,%al
  402696:	74 16                	je     4026ae <runtime::udivmod128+0xc8e>
  402698:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40269d:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4026a2:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  4026a7:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4026ab:	48 89 08             	mov    %rcx,(%rax)
  4026ae:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4026b3:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  4026b8:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  4026bf:	c3                   	ret

00000000004026c0 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  4026c0:	48 83 ec 18          	sub    $0x18,%rsp
  4026c4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4026c8:	40 88 f0             	mov    %sil,%al
  4026cb:	88 44 24 0e          	mov    %al,0xe(%rsp)
  4026cf:	48 8b 04 24          	mov    (%rsp),%rax
  4026d3:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  4026d7:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4026dc:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  4026e0:	48 83 f8 00          	cmp    $0x0,%rax
  4026e4:	0f 9e c0             	setle  %al
  4026e7:	24 01                	and    $0x1,%al
  4026e9:	3c 00                	cmp    $0x0,%al
  4026eb:	74 07                	je     4026f4 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  4026ed:	31 c0                	xor    %eax,%eax
  4026ef:	48 83 c4 18          	add    $0x18,%rsp
  4026f3:	c3                   	ret
  4026f4:	8a 44 24 0e          	mov    0xe(%rsp),%al
  4026f8:	3c 00                	cmp    $0x0,%al
  4026fa:	74 13                	je     40270f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  4026fc:	48 8b 34 24          	mov    (%rsp),%rsi
  402700:	bf 01 00 00 00       	mov    $0x1,%edi
  402705:	e8 46 e9 ff ff       	call   401050 <calloc@plt>
  40270a:	48 83 c4 18          	add    $0x18,%rsp
  40270e:	c3                   	ret
  40270f:	48 8b 3c 24          	mov    (%rsp),%rdi
  402713:	e8 58 e9 ff ff       	call   401070 <malloc@plt>
  402718:	48 83 c4 18          	add    $0x18,%rsp
  40271c:	c3                   	ret
  40271d:	0f 1f 00             	nopl   (%rax)

0000000000402720 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  402720:	48 83 ec 28          	sub    $0x28,%rsp
  402724:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402729:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40272e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402733:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402738:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40273d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402742:	e8 39 e9 ff ff       	call   401080 <realloc@plt>
  402747:	48 83 c4 28          	add    $0x28,%rsp
  40274b:	c3                   	ret
  40274c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402750 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  402750:	48 83 ec 18          	sub    $0x18,%rsp
  402754:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402759:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40275e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402763:	e8 c8 e8 ff ff       	call   401030 <free@plt>
  402768:	48 83 c4 18          	add    $0x18,%rsp
  40276c:	c3                   	ret
  40276d:	0f 1f 00             	nopl   (%rax)

0000000000402770 <runtime::memset>:
  402770:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  402775:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  402779:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  40277e:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  402783:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  402788:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  40278c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402791:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  402795:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40279a:	48 83 f8 00          	cmp    $0x0,%rax
  40279e:	0f 95 c0             	setne  %al
  4027a1:	24 01                	and    $0x1,%al
  4027a3:	3c 00                	cmp    $0x0,%al
  4027a5:	74 63                	je     40280a <runtime::memset+0x9a>
  4027a7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4027ac:	48 83 f8 00          	cmp    $0x0,%rax
  4027b0:	0f 95 c0             	setne  %al
  4027b3:	24 01                	and    $0x1,%al
  4027b5:	3c 00                	cmp    $0x0,%al
  4027b7:	74 51                	je     40280a <runtime::memset+0x9a>
  4027b9:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4027be:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  4027c2:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  4027c6:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4027cb:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  4027d2:	00 00 
  4027d4:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4027d9:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  4027de:	0f 9c c0             	setl   %al
  4027e1:	24 01                	and    $0x1,%al
  4027e3:	3c 00                	cmp    $0x0,%al
  4027e5:	74 21                	je     402808 <runtime::memset+0x98>
  4027e7:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4027ec:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  4027f1:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  4027f5:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  4027f8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4027fd:	48 83 c0 01          	add    $0x1,%rax
  402801:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  402806:	eb cc                	jmp    4027d4 <runtime::memset+0x64>
  402808:	eb 00                	jmp    40280a <runtime::memset+0x9a>
  40280a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40280f:	c3                   	ret

0000000000402810 <runtime::copy_slice_raw>:
  402810:	48 83 ec 58          	sub    $0x58,%rsp
  402814:	48 89 3c 24          	mov    %rdi,(%rsp)
  402818:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40281d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  402822:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  402827:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40282c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402831:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402836:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40283b:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  402840:	48 8b 3c 24          	mov    (%rsp),%rdi
  402844:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402849:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40284e:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  402853:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402858:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40285d:	48 39 c1             	cmp    %rax,%rcx
  402860:	48 0f 4c c1          	cmovl  %rcx,%rax
  402864:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402869:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
  40286f:	0f 9f c0             	setg   %al
  402872:	24 01                	and    $0x1,%al
  402874:	3c 00                	cmp    $0x0,%al
  402876:	74 19                	je     402891 <runtime::copy_slice_raw+0x81>
  402878:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40287d:	48 8b 3c 24          	mov    (%rsp),%rdi
  402881:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402886:	48 0f af 54 24 28    	imul   0x28(%rsp),%rdx
  40288c:	e8 ff e7 ff ff       	call   401090 <memmove@plt>
  402891:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402896:	48 83 c4 58          	add    $0x58,%rsp
  40289a:	c3                   	ret
  40289b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004028a0 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  4028a0:	48 83 ec 48          	sub    $0x48,%rsp
  4028a4:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4028a9:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4028ae:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4028b3:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4028b8:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4028bd:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4028c2:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4028c7:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4028cc:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4028d1:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4028d6:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4028db:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  4028e0:	41 b8 01 00 00 00    	mov    $0x1,%r8d
  4028e6:	e8 25 ff ff ff       	call   402810 <runtime::copy_slice_raw>
  4028eb:	48 83 c4 48          	add    $0x48,%rsp
  4028ef:	c3                   	ret

00000000004028f0 <runtime::_make_aligned_type_erased>:
  4028f0:	48 81 ec f8 00 00 00 	sub    $0xf8,%rsp
  4028f7:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4028fc:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402901:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  402906:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40290b:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  402910:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402915:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  40291c:	00 
  40291d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402922:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  402929:	00 
  40292a:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40292f:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  402934:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402939:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40293e:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  402943:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402948:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40294d:	4c 8b 4c 24 38       	mov    0x38(%rsp),%r9
  402952:	4c 89 8c 24 f0 00 00 	mov    %r9,0xf0(%rsp)
  402959:	00 
  40295a:	4c 89 84 24 e8 00 00 	mov    %r8,0xe8(%rsp)
  402961:	00 
  402962:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  402969:	00 
  40296a:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  402971:	00 
  402972:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  402979:	00 
  40297a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  402981:	00 
  402982:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  402989:	00 
  40298a:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40298f:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  402996:	00 
  402997:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40299c:	e8 7f 0f 00 00       	call   403920 <runtime::make_slice_error_loc>
  4029a1:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4029a6:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4029ab:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4029b0:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4029b5:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  4029ba:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4029bf:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  4029c4:	48 0f af fa          	imul   %rdx,%rdi
  4029c8:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  4029cf:	00 
  4029d0:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  4029d7:	00 
  4029d8:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  4029df:	00 
  4029e0:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4029e7:	00 
  4029e8:	0f 57 c0             	xorps  %xmm0,%xmm0
  4029eb:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4029f2:	00 
  4029f3:	48 89 e0             	mov    %rsp,%rax
  4029f6:	4c 89 08             	mov    %r9,(%rax)
  4029f9:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  402a00:	00 
  402a01:	e8 4a 3b 00 00       	call   406550 <runtime::mem_alloc_bytes>
  402a06:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  402a0d:	00 
  402a0e:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  402a15:	00 
  402a16:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  402a1d:	00 
  402a1e:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  402a23:	88 44 24 77          	mov    %al,0x77(%rsp)
  402a27:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  402a2d:	0f 94 c0             	sete   %al
  402a30:	24 01                	and    $0x1,%al
  402a32:	3c 00                	cmp    $0x0,%al
  402a34:	74 1e                	je     402a54 <runtime::_make_aligned_type_erased+0x164>
  402a36:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  402a3b:	48 83 f8 00          	cmp    $0x0,%rax
  402a3f:	0f 95 c0             	setne  %al
  402a42:	24 01                	and    $0x1,%al
  402a44:	3c 00                	cmp    $0x0,%al
  402a46:	74 0c                	je     402a54 <runtime::_make_aligned_type_erased+0x164>
  402a48:	8a 44 24 77          	mov    0x77(%rsp),%al
  402a4c:	48 81 c4 f8 00 00 00 	add    $0xf8,%rsp
  402a53:	c3                   	ret
  402a54:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  402a59:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402a5e:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  402a63:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  402a68:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  402a6d:	48 89 32             	mov    %rsi,(%rdx)
  402a70:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402a75:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402a7a:	48 89 48 08          	mov    %rcx,0x8(%rax)
  402a7e:	8a 44 24 77          	mov    0x77(%rsp),%al
  402a82:	48 81 c4 f8 00 00 00 	add    $0xf8,%rsp
  402a89:	c3                   	ret
  402a8a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402a90 <runtime::make_slice:proc(T:$[]string,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]string,err:runtime::Allocator_Error)>:
  402a90:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402a97:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  402a9c:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  402aa1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402aa6:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402aab:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402ab0:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  402ab5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402aba:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402abf:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402ac4:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402ac9:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  402ace:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  402ad5:	00 
  402ad6:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  402adb:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  402ae0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  402ae5:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  402aea:	0f 57 c0             	xorps  %xmm0,%xmm0
  402aed:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  402af2:	c6 44 24 5f 00       	movb   $0x0,0x5f(%rsp)
  402af7:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  402afc:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402b01:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  402b06:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  402b0b:	48 89 e0             	mov    %rsp,%rax
  402b0e:	48 89 70 08          	mov    %rsi,0x8(%rax)
  402b12:	48 89 08             	mov    %rcx,(%rax)
  402b15:	48 8d 7c 24 60       	lea    0x60(%rsp),%rdi
  402b1a:	be 10 00 00 00       	mov    $0x10,%esi
  402b1f:	b9 08 00 00 00       	mov    $0x8,%ecx
  402b24:	e8 c7 fd ff ff       	call   4028f0 <runtime::_make_aligned_type_erased>
  402b29:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402b2e:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  402b32:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  402b37:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  402b3c:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  402b40:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  402b45:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  402b4a:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  402b4e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402b52:	48 89 11             	mov    %rdx,(%rcx)
  402b55:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402b5c:	c3                   	ret
  402b5d:	0f 1f 00             	nopl   (%rax)

0000000000402b60 <runtime::delete_slice:proc(array:[]string,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>:
  402b60:	48 83 ec 68          	sub    $0x68,%rsp
  402b64:	4c 89 0c 24          	mov    %r9,(%rsp)
  402b68:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  402b6d:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  402b72:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402b77:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402b7c:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402b81:	4c 8b 0c 24          	mov    (%rsp),%r9
  402b85:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  402b8a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402b8f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402b94:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402b99:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402b9e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402ba3:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  402ba8:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  402bad:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402bb2:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  402bb7:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402bbc:	48 c1 e6 04          	shl    $0x4,%rsi
  402bc0:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  402bc5:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402bca:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402bcf:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  402bd4:	e8 97 3c 00 00       	call   406870 <runtime::mem_free_with_size>
  402bd9:	48 83 c4 68          	add    $0x68,%rsp
  402bdd:	c3                   	ret
  402bde:	66 90                	xchg   %ax,%ax

0000000000402be0 <runtime::assert>:
  402be0:	48 83 ec 48          	sub    $0x48,%rsp
  402be4:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  402be9:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  402bee:	40 88 f8             	mov    %dil,%al
  402bf1:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  402bf5:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  402bfa:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  402bff:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  402c03:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  402c08:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402c0d:	88 44 24 47          	mov    %al,0x47(%rsp)
  402c11:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  402c16:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402c1b:	3c 00                	cmp    $0x0,%al
  402c1d:	75 19                	jne    402c38 <runtime::assert+0x58>
  402c1f:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  402c24:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402c29:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402c2e:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402c33:	e8 08 00 00 00       	call   402c40 <runtime::assert.internal-0>
  402c38:	48 83 c4 48          	add    $0x48,%rsp
  402c3c:	c3                   	ret
  402c3d:	0f 1f 00             	nopl   (%rax)

0000000000402c40 <runtime::assert.internal-0>:
  402c40:	48 83 ec 38          	sub    $0x38,%rsp
  402c44:	48 89 0c 24          	mov    %rcx,(%rsp)
  402c48:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  402c4d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402c52:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  402c57:	48 8b 04 24          	mov    (%rsp),%rax
  402c5b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402c60:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402c65:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402c6a:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402c6f:	48 8b 40 20          	mov    0x20(%rax),%rax
  402c73:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402c78:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  402c7e:	0f 94 c0             	sete   %al
  402c81:	24 01                	and    $0x1,%al
  402c83:	3c 00                	cmp    $0x0,%al
  402c85:	74 0c                	je     402c93 <runtime::assert.internal-0+0x53>
  402c87:	48 c7 c0 50 5b 40 00 	mov    $0x405b50,%rax
  402c8e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402c93:	4c 8b 0c 24          	mov    (%rsp),%r9
  402c97:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  402c9c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402ca1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402ca6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402cab:	bf 77 90 40 00       	mov    $0x409077,%edi
  402cb0:	be 11 00 00 00       	mov    $0x11,%esi
  402cb5:	ff d0                	call   *%rax
  402cb7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  402cbe:	00 00 

0000000000402cc0 <runtime::default_random_generator_proc>:
  402cc0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402cc7:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402ccc:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402cd1:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  402cd6:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  402cdb:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402ce0:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402ce5:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402cea:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  402cef:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402cf6:	00 
  402cf7:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  402cfc:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  402d01:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  402d06:	48 83 f8 00          	cmp    $0x0,%rax
  402d0a:	0f 94 c0             	sete   %al
  402d0d:	24 01                	and    $0x1,%al
  402d0f:	3c 00                	cmp    $0x0,%al
  402d11:	74 1a                	je     402d2d <runtime::default_random_generator_proc+0x6d>
  402d13:	48 c7 c1 b8 ff ff ff 	mov    $0xffffffffffffffb8,%rcx
  402d1a:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402d21:	00 00 
  402d23:	48 01 c8             	add    %rcx,%rax
  402d26:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402d2b:	eb 0a                	jmp    402d37 <runtime::default_random_generator_proc+0x77>
  402d2d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402d32:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402d37:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402d3c:	48 85 c0             	test   %rax,%rax
  402d3f:	74 27                	je     402d68 <runtime::default_random_generator_proc+0xa8>
  402d41:	eb 00                	jmp    402d43 <runtime::default_random_generator_proc+0x83>
  402d43:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402d48:	48 83 e8 01          	sub    $0x1,%rax
  402d4c:	0f 84 17 01 00 00    	je     402e69 <runtime::default_random_generator_proc+0x1a9>
  402d52:	eb 00                	jmp    402d54 <runtime::default_random_generator_proc+0x94>
  402d54:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402d59:	48 83 e8 02          	sub    $0x2,%rax
  402d5d:	0f 84 40 01 00 00    	je     402ea3 <runtime::default_random_generator_proc+0x1e3>
  402d63:	e9 6b 01 00 00       	jmp    402ed3 <runtime::default_random_generator_proc+0x213>
  402d68:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402d6d:	48 83 38 00          	cmpq   $0x0,(%rax)
  402d71:	0f 94 c0             	sete   %al
  402d74:	24 01                	and    $0x1,%al
  402d76:	3c 00                	cmp    $0x0,%al
  402d78:	74 21                	je     402d9b <runtime::default_random_generator_proc+0xdb>
  402d7a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402d7f:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  402d84:	0f 94 c0             	sete   %al
  402d87:	24 01                	and    $0x1,%al
  402d89:	3c 00                	cmp    $0x0,%al
  402d8b:	74 0e                	je     402d9b <runtime::default_random_generator_proc+0xdb>
  402d8d:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402d92:	31 c0                	xor    %eax,%eax
  402d94:	89 c6                	mov    %eax,%esi
  402d96:	e8 15 02 00 00       	call   402fb0 <runtime::default_random_generator_proc.init-1>
  402d9b:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402da0:	48 83 e8 08          	sub    $0x8,%rax
  402da4:	75 26                	jne    402dcc <runtime::default_random_generator_proc+0x10c>
  402da6:	eb 00                	jmp    402da8 <runtime::default_random_generator_proc+0xe8>
  402da8:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402dad:	e8 2e 01 00 00       	call   402ee0 <runtime::default_random_generator_proc.read_u64-0>
  402db2:	48 89 c1             	mov    %rax,%rcx
  402db5:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402dba:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  402dbf:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402dc4:	48 89 08             	mov    %rcx,(%rax)
  402dc7:	e9 9b 00 00 00       	jmp    402e67 <runtime::default_random_generator_proc+0x1a7>
  402dcc:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402dd1:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  402dd6:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  402ddd:	00 00 
  402ddf:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402de4:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  402deb:	ff ff 
  402ded:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402df2:	48 83 c0 01          	add    $0x1,%rax
  402df6:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402dfb:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  402e00:	7d 63                	jge    402e65 <runtime::default_random_generator_proc+0x1a5>
  402e02:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402e07:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  402e0c:	48 01 c8             	add    %rcx,%rax
  402e0f:	48 89 04 24          	mov    %rax,(%rsp)
  402e13:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  402e18:	0f 94 c0             	sete   %al
  402e1b:	24 01                	and    $0x1,%al
  402e1d:	3c 00                	cmp    $0x0,%al
  402e1f:	74 14                	je     402e35 <runtime::default_random_generator_proc+0x175>
  402e21:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402e26:	e8 b5 00 00 00       	call   402ee0 <runtime::default_random_generator_proc.read_u64-0>
  402e2b:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402e30:	c6 44 24 57 08       	movb   $0x8,0x57(%rsp)
  402e35:	48 8b 04 24          	mov    (%rsp),%rax
  402e39:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402e3e:	88 08                	mov    %cl,(%rax)
  402e40:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402e45:	48 c1 e9 08          	shr    $0x8,%rcx
  402e49:	b2 01                	mov    $0x1,%dl
  402e4b:	31 c0                	xor    %eax,%eax
  402e4d:	f6 c2 01             	test   $0x1,%dl
  402e50:	48 0f 45 c1          	cmovne %rcx,%rax
  402e54:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402e59:	8a 44 24 57          	mov    0x57(%rsp),%al
  402e5d:	2c 01                	sub    $0x1,%al
  402e5f:	88 44 24 57          	mov    %al,0x57(%rsp)
  402e63:	eb 88                	jmp    402ded <runtime::default_random_generator_proc+0x12d>
  402e65:	eb 00                	jmp    402e67 <runtime::default_random_generator_proc+0x1a7>
  402e67:	eb 6a                	jmp    402ed3 <runtime::default_random_generator_proc+0x213>
  402e69:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402e6e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402e73:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  402e7a:	00 00 
  402e7c:	b8 08 00 00 00       	mov    $0x8,%eax
  402e81:	48 39 d0             	cmp    %rdx,%rax
  402e84:	48 0f 4c d0          	cmovl  %rax,%rdx
  402e88:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402e8d:	e8 2e 36 00 00       	call   4064c0 <runtime::mem_copy_non_overlapping>
  402e92:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402e97:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  402e9c:	e8 0f 01 00 00       	call   402fb0 <runtime::default_random_generator_proc.init-1>
  402ea1:	eb 30                	jmp    402ed3 <runtime::default_random_generator_proc+0x213>
  402ea3:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402ea8:	48 83 f8 04          	cmp    $0x4,%rax
  402eac:	0f 95 c0             	setne  %al
  402eaf:	24 01                	and    $0x1,%al
  402eb1:	3c 00                	cmp    $0x0,%al
  402eb3:	74 08                	je     402ebd <runtime::default_random_generator_proc+0x1fd>
  402eb5:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402ebc:	c3                   	ret
  402ebd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402ec2:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402ec7:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402ecc:	8b 08                	mov    (%rax),%ecx
  402ece:	83 c9 0a             	or     $0xa,%ecx
  402ed1:	89 08                	mov    %ecx,(%rax)
  402ed3:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402eda:	c3                   	ret
  402edb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402ee0 <runtime::default_random_generator_proc.read_u64-0>:
  402ee0:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  402ee5:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  402eea:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402eef:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  402ef4:	48 8b 00             	mov    (%rax),%rax
  402ef7:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  402efc:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  402f01:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  402f08:	f4 51 58 
  402f0b:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  402f11:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  402f16:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  402f1a:	48 83 ca 01          	or     $0x1,%rdx
  402f1e:	48 01 d1             	add    %rdx,%rcx
  402f21:	48 89 08             	mov    %rcx,(%rax)
  402f24:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  402f29:	48 c1 e9 3b          	shr    $0x3b,%rcx
  402f2d:	b2 01                	mov    $0x1,%dl
  402f2f:	31 c0                	xor    %eax,%eax
  402f31:	f6 c2 01             	test   $0x1,%dl
  402f34:	48 0f 45 c1          	cmovne %rcx,%rax
  402f38:	48 83 c0 05          	add    $0x5,%rax
  402f3c:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  402f41:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  402f48:	75 f1 ae 
  402f4b:	48 0f af c1          	imul   %rcx,%rax
  402f4f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402f54:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  402f59:	48 c1 e9 3b          	shr    $0x3b,%rcx
  402f5d:	b2 01                	mov    $0x1,%dl
  402f5f:	31 c0                	xor    %eax,%eax
  402f61:	f6 c2 01             	test   $0x1,%dl
  402f64:	48 0f 45 c1          	cmovne %rcx,%rax
  402f68:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  402f6d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  402f72:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  402f77:	48 89 d1             	mov    %rdx,%rcx
  402f7a:	48 d3 e8             	shr    %cl,%rax
  402f7d:	48 89 c1             	mov    %rax,%rcx
  402f80:	31 c0                	xor    %eax,%eax
  402f82:	48 83 fa 40          	cmp    $0x40,%rdx
  402f86:	48 0f 42 c1          	cmovb  %rcx,%rax
  402f8a:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  402f8f:	31 c9                	xor    %ecx,%ecx
  402f91:	89 ce                	mov    %ecx,%esi
  402f93:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  402f98:	48 83 e6 3f          	and    $0x3f,%rsi
  402f9c:	48 89 f1             	mov    %rsi,%rcx
  402f9f:	48 d3 e2             	shl    %cl,%rdx
  402fa2:	31 c9                	xor    %ecx,%ecx
  402fa4:	48 83 fe 40          	cmp    $0x40,%rsi
  402fa8:	48 0f 42 ca          	cmovb  %rdx,%rcx
  402fac:	48 09 c8             	or     %rcx,%rax
  402faf:	c3                   	ret

0000000000402fb0 <runtime::default_random_generator_proc.init-1>:
  402fb0:	48 83 ec 28          	sub    $0x28,%rsp
  402fb4:	48 89 3c 24          	mov    %rdi,(%rsp)
  402fb8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402fbd:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402fc2:	48 8b 0c 24          	mov    (%rsp),%rcx
  402fc6:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402fcb:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  402fd0:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402fd5:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  402fdb:	0f 94 c0             	sete   %al
  402fde:	24 01                	and    $0x1,%al
  402fe0:	3c 00                	cmp    $0x0,%al
  402fe2:	74 0e                	je     402ff2 <runtime::default_random_generator_proc.init-1+0x42>
  402fe4:	0f 31                	rdtsc
  402fe6:	48 c1 e2 20          	shl    $0x20,%rdx
  402fea:	48 09 d0             	or     %rdx,%rax
  402fed:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402ff2:	48 8b 3c 24          	mov    (%rsp),%rdi
  402ff6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402ffb:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  403002:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403007:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40300c:	48 d1 e2             	shl    $1,%rdx
  40300f:	40 b6 01             	mov    $0x1,%sil
  403012:	31 c9                	xor    %ecx,%ecx
  403014:	40 f6 c6 01          	test   $0x1,%sil
  403018:	48 0f 45 ca          	cmovne %rdx,%rcx
  40301c:	48 83 c9 01          	or     $0x1,%rcx
  403020:	48 89 48 08          	mov    %rcx,0x8(%rax)
  403024:	e8 b7 fe ff ff       	call   402ee0 <runtime::default_random_generator_proc.read_u64-0>
  403029:	48 8b 3c 24          	mov    (%rsp),%rdi
  40302d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403032:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  403037:	48 03 08             	add    (%rax),%rcx
  40303a:	48 89 08             	mov    %rcx,(%rax)
  40303d:	e8 9e fe ff ff       	call   402ee0 <runtime::default_random_generator_proc.read_u64-0>
  403042:	48 83 c4 28          	add    $0x28,%rsp
  403046:	c3                   	ret
  403047:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40304e:	00 00 

0000000000403050 <main>:
  403050:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  403057:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  40305b:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403060:	8b 44 24 14          	mov    0x14(%rsp),%eax
  403064:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403069:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  403070:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  403077:	00 
  403078:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  40307f:	00 
  403080:	48 89 0c 24          	mov    %rcx,(%rsp)
  403084:	4c 63 c8             	movslq %eax,%r9
  403087:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40308c:	bf 89 90 40 00       	mov    $0x409089,%edi
  403091:	31 c0                	xor    %eax,%eax
  403093:	41 89 c0             	mov    %eax,%r8d
  403096:	be 2a 00 00 00       	mov    $0x2a,%esi
  40309b:	ba 36 00 00 00       	mov    $0x36,%edx
  4030a0:	b9 11 00 00 00       	mov    $0x11,%ecx
  4030a5:	e8 56 05 00 00       	call   403600 <runtime::multi_pointer_slice_expr_error>
  4030aa:	48 8b 0c 24          	mov    (%rsp),%rcx
  4030ae:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4030b3:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  4030ba:	00 
  4030bb:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4030c2:	00 
  4030c3:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  4030ca:	00 
  4030cb:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  4030d2:	00 
  4030d3:	48 c7 c0 60 c0 40 00 	mov    $0x40c060,%rax
  4030da:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4030de:	48 89 08             	mov    %rcx,(%rax)
  4030e1:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4030e8:	00 
  4030e9:	31 f6                	xor    %esi,%esi
  4030eb:	ba 70 00 00 00       	mov    $0x70,%edx
  4030f0:	e8 4b df ff ff       	call   401040 <memset@plt>
  4030f5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4030fc:	00 
  4030fd:	e8 9e 29 00 00       	call   405aa0 <runtime::[core.odin]::__init_context>
  403102:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  403107:	31 f6                	xor    %esi,%esi
  403109:	ba 70 00 00 00       	mov    $0x70,%edx
  40310e:	e8 2d df ff ff       	call   401040 <memset@plt>
  403113:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  403118:	e8 33 29 00 00       	call   405a50 <runtime::default_context>
  40311d:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  403122:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  403127:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  40312c:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  403131:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  403136:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  40313b:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  403142:	00 
  403143:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  40314a:	00 
  40314b:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  403152:	00 
  403153:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  40315a:	00 
  40315b:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  403162:	00 
  403163:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  40316a:	00 
  40316b:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  403172:	00 
  403173:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40317a:	00 
  40317b:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  403182:	00 
  403183:	e8 c8 2a 00 00       	call   405c50 <__$startup_runtime>
  403188:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40318f:	00 
  403190:	e8 7b 2b 00 00       	call   405d10 <journey::main>
  403195:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40319c:	00 
  40319d:	e8 5e 2b 00 00       	call   405d00 <__$cleanup_runtime>
  4031a2:	31 c0                	xor    %eax,%eax
  4031a4:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  4031ab:	c3                   	ret
  4031ac:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004031b0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  4031b0:	50                   	push   %rax
  4031b1:	eb 00                	jmp    4031b3 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x3>
  4031b3:	48 c7 c0 c8 ff ff ff 	mov    $0xffffffffffffffc8,%rax
  4031ba:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  4031c1:	00 00 
  4031c3:	48 01 c7             	add    %rax,%rdi
  4031c6:	e8 05 00 00 00       	call   4031d0 <runtime::default_temp_allocator_destroy>
  4031cb:	58                   	pop    %rax
  4031cc:	c3                   	ret
  4031cd:	0f 1f 00             	nopl   (%rax)

00000000004031d0 <runtime::default_temp_allocator_destroy>:
  4031d0:	48 83 ec 18          	sub    $0x18,%rsp
  4031d4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4031d9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4031de:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4031e3:	48 83 f8 00          	cmp    $0x0,%rax
  4031e7:	0f 95 c0             	setne  %al
  4031ea:	24 01                	and    $0x1,%al
  4031ec:	3c 00                	cmp    $0x0,%al
  4031ee:	74 25                	je     403215 <runtime::default_temp_allocator_destroy+0x45>
  4031f0:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4031f5:	48 be 20 91 40 00 00 	movabs $0x409120,%rsi
  4031fc:	00 00 00 
  4031ff:	e8 0c 1c 00 00       	call   404e10 <runtime::arena_destroy>
  403204:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403209:	31 f6                	xor    %esi,%esi
  40320b:	ba 38 00 00 00       	mov    $0x38,%edx
  403210:	e8 2b de ff ff       	call   401040 <memset@plt>
  403215:	48 83 c4 18          	add    $0x18,%rsp
  403219:	c3                   	ret
  40321a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000403220 <runtime::default_temp_allocator_proc>:
  403220:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403227:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  40322c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  403231:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403236:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  40323b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  403240:	40 88 f0             	mov    %sil,%al
  403243:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  403247:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40324e:	00 
  40324f:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403254:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40325b:	00 
  40325c:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403261:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403268:	00 
  403269:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40326e:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  403273:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403278:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40327d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403282:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  403286:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  40328b:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  403290:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  403295:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  40329c:	00 
  40329d:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4032a4:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  4032ab:	00 
  4032ac:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  4032b3:	00 
  4032b4:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  4032bb:	00 
  4032bc:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  4032c3:	00 
  4032c4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4032c7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4032ce:	00 
  4032cf:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  4032d6:	00 
  4032d7:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  4032de:	00 
  4032df:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  4032e6:	00 
  4032e7:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  4032ee:	00 
  4032ef:	48 89 e6             	mov    %rsp,%rsi
  4032f2:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  4032f6:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  4032fd:	00 
  4032fe:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  403302:	4c 89 16             	mov    %r10,(%rsi)
  403305:	0f b6 f0             	movzbl %al,%esi
  403308:	e8 a3 1b 00 00       	call   404eb0 <runtime::arena_allocator_proc>
  40330d:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  403312:	40 88 c7             	mov    %al,%dil
  403315:	40 88 f8             	mov    %dil,%al
  403318:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  40331f:	00 
  403320:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  403327:	00 
  403328:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40332f:	00 
  403330:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403337:	00 
  403338:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  40333f:	00 
  403340:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403344:	48 89 11             	mov    %rdx,(%rcx)
  403347:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40334e:	c3                   	ret
  40334f:	90                   	nop

0000000000403350 <runtime::bounds_trap>:
  403350:	eb 00                	jmp    403352 <runtime::bounds_trap+0x2>
  403352:	0f 0b                	ud2
  403354:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40335b:	00 00 00 00 00 

0000000000403360 <runtime::bounds_check_error>:
  403360:	48 83 ec 58          	sub    $0x58,%rsp
  403364:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403369:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40336e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403372:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403376:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40337b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403380:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403385:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40338a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40338e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403392:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403397:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40339c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4033a1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  4033a6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  4033aa:	89 54 24 40          	mov    %edx,0x40(%rsp)
  4033ae:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4033b3:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4033b8:	48 39 c8             	cmp    %rcx,%rax
  4033bb:	0f 92 c0             	setb   %al
  4033be:	24 01                	and    $0x1,%al
  4033c0:	3c 00                	cmp    $0x0,%al
  4033c2:	74 05                	je     4033c9 <runtime::bounds_check_error+0x69>
  4033c4:	48 83 c4 58          	add    $0x58,%rsp
  4033c8:	c3                   	ret
  4033c9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4033ce:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4033d3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4033d7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4033db:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4033e0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4033e5:	e8 76 05 00 00       	call   403960 <runtime::bounds_check_error.handle_error-0>
  4033ea:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004033f0 <runtime::slice_handle_error>:
  4033f0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4033f7:	4c 89 0c 24          	mov    %r9,(%rsp)
  4033fb:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  403400:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  403404:	89 54 24 14          	mov    %edx,0x14(%rsp)
  403408:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40340d:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  403412:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403419:	00 
  40341a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40341f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403424:	4c 8b 04 24          	mov    (%rsp),%r8
  403428:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40342d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  403431:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  403435:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40343a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40343f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403444:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40344b:	00 
  40344c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  403450:	89 44 24 70          	mov    %eax,0x70(%rsp)
  403454:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  403459:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  40345e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  403463:	0f 57 c0             	xorps  %xmm0,%xmm0
  403466:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40346b:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  403470:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403477:	00 00 
  403479:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40347e:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  403483:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40348a:	00 00 
  40348c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403491:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403496:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  40349a:	89 44 24 44          	mov    %eax,0x44(%rsp)
  40349e:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4034a3:	e8 08 0c 00 00       	call   4040b0 <runtime::print_caller_location>
  4034a8:	bf 49 91 40 00       	mov    $0x409149,%edi
  4034ad:	be 17 00 00 00       	mov    $0x17,%esi
  4034b2:	e8 59 07 00 00       	call   403c10 <runtime::print_string>
  4034b7:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4034bc:	e8 6f 0a 00 00       	call   403f30 <runtime::print_i64>
  4034c1:	bf 61 91 40 00       	mov    $0x409161,%edi
  4034c6:	be 01 00 00 00       	mov    $0x1,%esi
  4034cb:	e8 40 07 00 00       	call   403c10 <runtime::print_string>
  4034d0:	48 8b 3c 24          	mov    (%rsp),%rdi
  4034d4:	e8 57 0a 00 00       	call   403f30 <runtime::print_i64>
  4034d9:	bf 63 91 40 00       	mov    $0x409163,%edi
  4034de:	be 15 00 00 00       	mov    $0x15,%esi
  4034e3:	e8 28 07 00 00       	call   403c10 <runtime::print_string>
  4034e8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4034ed:	e8 3e 0a 00 00       	call   403f30 <runtime::print_i64>
  4034f2:	bf 0a 00 00 00       	mov    $0xa,%edi
  4034f7:	e8 84 07 00 00       	call   403c80 <runtime::print_byte>
  4034fc:	e8 4f fe ff ff       	call   403350 <runtime::bounds_trap>
  403501:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403508:	0f 1f 84 00 00 00 00 
  40350f:	00 

0000000000403510 <runtime::multi_pointer_slice_handle_error>:
  403510:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403517:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40351c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403521:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403525:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403529:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40352e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403533:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403538:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40353d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  403541:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  403545:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40354a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40354f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403554:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40355b:	00 
  40355c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  403560:	89 44 24 70          	mov    %eax,0x70(%rsp)
  403564:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  403569:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40356e:	0f 57 c0             	xorps  %xmm0,%xmm0
  403571:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403576:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40357b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403582:	00 00 
  403584:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403589:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40358e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403595:	00 00 
  403597:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40359c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4035a1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4035a5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4035a9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4035ae:	e8 fd 0a 00 00       	call   4040b0 <runtime::print_caller_location>
  4035b3:	bf 49 91 40 00       	mov    $0x409149,%edi
  4035b8:	be 17 00 00 00       	mov    $0x17,%esi
  4035bd:	e8 4e 06 00 00       	call   403c10 <runtime::print_string>
  4035c2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4035c7:	e8 64 09 00 00       	call   403f30 <runtime::print_i64>
  4035cc:	bf 61 91 40 00       	mov    $0x409161,%edi
  4035d1:	be 01 00 00 00       	mov    $0x1,%esi
  4035d6:	e8 35 06 00 00       	call   403c10 <runtime::print_string>
  4035db:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4035e0:	e8 4b 09 00 00       	call   403f30 <runtime::print_i64>
  4035e5:	bf 0a 00 00 00       	mov    $0xa,%edi
  4035ea:	e8 91 06 00 00       	call   403c80 <runtime::print_byte>
  4035ef:	e8 5c fd ff ff       	call   403350 <runtime::bounds_trap>
  4035f4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4035fb:	00 00 00 00 00 

0000000000403600 <runtime::multi_pointer_slice_expr_error>:
  403600:	48 83 ec 58          	sub    $0x58,%rsp
  403604:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403609:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40360e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403612:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403616:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40361b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403620:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403625:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40362a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40362e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403632:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403637:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40363c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403641:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403646:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40364a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40364e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403653:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403658:	48 39 c8             	cmp    %rcx,%rax
  40365b:	0f 9e c0             	setle  %al
  40365e:	24 01                	and    $0x1,%al
  403660:	3c 00                	cmp    $0x0,%al
  403662:	74 05                	je     403669 <runtime::multi_pointer_slice_expr_error+0x69>
  403664:	48 83 c4 58          	add    $0x58,%rsp
  403668:	c3                   	ret
  403669:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40366e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  403673:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  403677:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40367b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403680:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403685:	e8 86 fe ff ff       	call   403510 <runtime::multi_pointer_slice_handle_error>
  40368a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000403690 <runtime::slice_expr_error_hi>:
  403690:	48 83 ec 58          	sub    $0x58,%rsp
  403694:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403699:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40369e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4036a2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4036a6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4036ab:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4036b0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4036b5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4036ba:	8b 54 24 18          	mov    0x18(%rsp),%edx
  4036be:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  4036c2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4036c7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4036cc:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4036d1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  4036d6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  4036da:	89 54 24 40          	mov    %edx,0x40(%rsp)
  4036de:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4036e3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4036e8:	31 c0                	xor    %eax,%eax
  4036ea:	48 39 c8             	cmp    %rcx,%rax
  4036ed:	0f 9e c0             	setle  %al
  4036f0:	24 01                	and    $0x1,%al
  4036f2:	3c 00                	cmp    $0x0,%al
  4036f4:	74 1b                	je     403711 <runtime::slice_expr_error_hi+0x81>
  4036f6:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4036fb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403700:	48 39 c8             	cmp    %rcx,%rax
  403703:	0f 9e c0             	setle  %al
  403706:	24 01                	and    $0x1,%al
  403708:	3c 00                	cmp    $0x0,%al
  40370a:	74 05                	je     403711 <runtime::slice_expr_error_hi+0x81>
  40370c:	48 83 c4 58          	add    $0x58,%rsp
  403710:	c3                   	ret
  403711:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403716:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40371a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40371e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403723:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403728:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40372d:	48 89 e0             	mov    %rsp,%rax
  403730:	4c 89 00             	mov    %r8,(%rax)
  403733:	31 c0                	xor    %eax,%eax
  403735:	41 89 c0             	mov    %eax,%r8d
  403738:	e8 b3 fc ff ff       	call   4033f0 <runtime::slice_handle_error>
  40373d:	0f 1f 00             	nopl   (%rax)

0000000000403740 <runtime::slice_expr_error_lo_hi>:
  403740:	48 83 ec 68          	sub    $0x68,%rsp
  403744:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403749:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40374e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403752:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403756:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40375b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403760:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403765:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40376a:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40376f:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403774:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403779:	8b 74 24 18          	mov    0x18(%rsp),%esi
  40377d:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  403781:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403786:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  40378b:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  403790:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  403795:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  403799:	89 74 24 50          	mov    %esi,0x50(%rsp)
  40379d:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4037a2:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4037a7:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4037ac:	31 c0                	xor    %eax,%eax
  4037ae:	48 39 c8             	cmp    %rcx,%rax
  4037b1:	0f 9e c0             	setle  %al
  4037b4:	24 01                	and    $0x1,%al
  4037b6:	3c 00                	cmp    $0x0,%al
  4037b8:	74 47                	je     403801 <runtime::slice_expr_error_lo_hi+0xc1>
  4037ba:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4037bf:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4037c4:	48 39 c8             	cmp    %rcx,%rax
  4037c7:	0f 9e c0             	setle  %al
  4037ca:	24 01                	and    $0x1,%al
  4037cc:	3c 00                	cmp    $0x0,%al
  4037ce:	74 31                	je     403801 <runtime::slice_expr_error_lo_hi+0xc1>
  4037d0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4037d5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4037da:	48 39 c8             	cmp    %rcx,%rax
  4037dd:	0f 9e c0             	setle  %al
  4037e0:	24 01                	and    $0x1,%al
  4037e2:	3c 00                	cmp    $0x0,%al
  4037e4:	74 1b                	je     403801 <runtime::slice_expr_error_lo_hi+0xc1>
  4037e6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4037eb:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4037f0:	48 39 c8             	cmp    %rcx,%rax
  4037f3:	0f 9e c0             	setle  %al
  4037f6:	24 01                	and    $0x1,%al
  4037f8:	3c 00                	cmp    $0x0,%al
  4037fa:	74 05                	je     403801 <runtime::slice_expr_error_lo_hi+0xc1>
  4037fc:	48 83 c4 68          	add    $0x68,%rsp
  403800:	c3                   	ret
  403801:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403806:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40380b:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40380f:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  403813:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403818:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40381d:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  403822:	48 89 e0             	mov    %rsp,%rax
  403825:	4c 89 10             	mov    %r10,(%rax)
  403828:	e8 c3 fb ff ff       	call   4033f0 <runtime::slice_handle_error>
  40382d:	0f 1f 00             	nopl   (%rax)

0000000000403830 <runtime::matrix_bounds_check_error>:
  403830:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403837:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40383c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403841:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  403845:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  403849:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  40384e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403853:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  40385a:	00 
  40385b:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403860:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403867:	00 
  403868:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40386d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403872:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403877:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40387c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403881:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  403885:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  40388a:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  40388f:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  403894:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  403899:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  4038a0:	00 
  4038a1:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  4038a6:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  4038aa:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4038af:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  4038b4:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4038b9:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  4038be:	48 39 c8             	cmp    %rcx,%rax
  4038c1:	0f 92 c0             	setb   %al
  4038c4:	24 01                	and    $0x1,%al
  4038c6:	3c 00                	cmp    $0x0,%al
  4038c8:	74 1e                	je     4038e8 <runtime::matrix_bounds_check_error+0xb8>
  4038ca:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4038cf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4038d4:	48 39 c8             	cmp    %rcx,%rax
  4038d7:	0f 92 c0             	setb   %al
  4038da:	24 01                	and    $0x1,%al
  4038dc:	3c 00                	cmp    $0x0,%al
  4038de:	74 08                	je     4038e8 <runtime::matrix_bounds_check_error+0xb8>
  4038e0:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4038e7:	c3                   	ret
  4038e8:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  4038ed:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  4038f2:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  4038f6:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  4038fa:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4038ff:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403904:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  403909:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  40390e:	48 89 e0             	mov    %rsp,%rax
  403911:	4c 89 58 08          	mov    %r11,0x8(%rax)
  403915:	4c 89 10             	mov    %r10,(%rax)
  403918:	e8 33 01 00 00       	call   403a50 <runtime::matrix_bounds_check_error.handle_error-0>
  40391d:	0f 1f 00             	nopl   (%rax)

0000000000403920 <runtime::make_slice_error_loc>:
  403920:	48 83 ec 18          	sub    $0x18,%rsp
  403924:	48 89 3c 24          	mov    %rdi,(%rsp)
  403928:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40392d:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403932:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  403937:	31 c0                	xor    %eax,%eax
  403939:	48 39 c8             	cmp    %rcx,%rax
  40393c:	0f 9e c0             	setle  %al
  40393f:	24 01                	and    $0x1,%al
  403941:	3c 00                	cmp    $0x0,%al
  403943:	74 05                	je     40394a <runtime::make_slice_error_loc+0x2a>
  403945:	48 83 c4 18          	add    $0x18,%rsp
  403949:	c3                   	ret
  40394a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40394f:	48 8b 3c 24          	mov    (%rsp),%rdi
  403953:	e8 68 02 00 00       	call   403bc0 <runtime::make_slice_error_loc.handle_error-0>
  403958:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40395f:	00 

0000000000403960 <runtime::bounds_check_error.handle_error-0>:
  403960:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403967:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40396c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403971:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403975:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403979:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40397e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403983:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403988:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40398d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  403991:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  403995:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40399a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40399f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4039a4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  4039ab:	00 
  4039ac:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  4039b0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  4039b4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  4039b9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  4039be:	0f 57 c0             	xorps  %xmm0,%xmm0
  4039c1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4039c6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4039cb:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4039d2:	00 00 
  4039d4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4039d9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4039de:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4039e5:	00 00 
  4039e7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4039ec:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4039f1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4039f5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4039f9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4039fe:	e8 ad 06 00 00       	call   4040b0 <runtime::print_caller_location>
  403a03:	bf 79 91 40 00       	mov    $0x409179,%edi
  403a08:	be 07 00 00 00       	mov    $0x7,%esi
  403a0d:	e8 fe 01 00 00       	call   403c10 <runtime::print_string>
  403a12:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403a17:	e8 14 05 00 00       	call   403f30 <runtime::print_i64>
  403a1c:	bf 63 91 40 00       	mov    $0x409163,%edi
  403a21:	be 15 00 00 00       	mov    $0x15,%esi
  403a26:	e8 e5 01 00 00       	call   403c10 <runtime::print_string>
  403a2b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403a30:	e8 fb 04 00 00       	call   403f30 <runtime::print_i64>
  403a35:	bf 0a 00 00 00       	mov    $0xa,%edi
  403a3a:	e8 41 02 00 00       	call   403c80 <runtime::print_byte>
  403a3f:	e8 0c f9 ff ff       	call   403350 <runtime::bounds_trap>
  403a44:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403a4b:	00 00 00 00 00 

0000000000403a50 <runtime::matrix_bounds_check_error.handle_error-0>:
  403a50:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  403a57:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403a5c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403a61:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403a65:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403a69:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  403a6e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403a73:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  403a7a:	00 
  403a7b:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403a80:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403a87:	00 
  403a88:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403a8d:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403a92:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  403a97:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403a9c:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  403aa1:	8b 44 24 18          	mov    0x18(%rsp),%eax
  403aa5:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  403aa9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  403aae:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403ab3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  403aba:	00 
  403abb:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  403ac2:	00 
  403ac3:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  403aca:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  403ad1:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  403ad8:	00 
  403ad9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  403ae0:	00 
  403ae1:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  403ae6:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  403aeb:	0f 57 c0             	xorps  %xmm0,%xmm0
  403aee:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  403af3:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403af8:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  403aff:	00 00 
  403b01:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  403b06:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403b0b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  403b12:	00 00 
  403b14:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  403b19:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  403b1e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  403b22:	89 44 24 54          	mov    %eax,0x54(%rsp)
  403b26:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  403b2b:	e8 80 05 00 00       	call   4040b0 <runtime::print_caller_location>
  403b30:	bf 81 91 40 00       	mov    $0x409181,%edi
  403b35:	be 11 00 00 00       	mov    $0x11,%esi
  403b3a:	e8 d1 00 00 00       	call   403c10 <runtime::print_string>
  403b3f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403b44:	e8 e7 03 00 00       	call   403f30 <runtime::print_i64>
  403b49:	bf 93 91 40 00       	mov    $0x409193,%edi
  403b4e:	be 02 00 00 00       	mov    $0x2,%esi
  403b53:	e8 b8 00 00 00       	call   403c10 <runtime::print_string>
  403b58:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403b5d:	e8 ce 03 00 00       	call   403f30 <runtime::print_i64>
  403b62:	bf 96 91 40 00       	mov    $0x409196,%edi
  403b67:	be 16 00 00 00       	mov    $0x16,%esi
  403b6c:	e8 9f 00 00 00       	call   403c10 <runtime::print_string>
  403b71:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403b76:	e8 b5 03 00 00       	call   403f30 <runtime::print_i64>
  403b7b:	bf ad 91 40 00       	mov    $0x4091ad,%edi
  403b80:	be 06 00 00 00       	mov    $0x6,%esi
  403b85:	e8 86 00 00 00       	call   403c10 <runtime::print_string>
  403b8a:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403b8f:	e8 9c 03 00 00       	call   403f30 <runtime::print_i64>
  403b94:	bf b4 91 40 00       	mov    $0x4091b4,%edi
  403b99:	be 01 00 00 00       	mov    $0x1,%esi
  403b9e:	e8 6d 00 00 00       	call   403c10 <runtime::print_string>
  403ba3:	bf 0a 00 00 00       	mov    $0xa,%edi
  403ba8:	e8 d3 00 00 00       	call   403c80 <runtime::print_byte>
  403bad:	e8 9e f7 ff ff       	call   403350 <runtime::bounds_trap>
  403bb2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403bb9:	1f 84 00 00 00 00 00 

0000000000403bc0 <runtime::make_slice_error_loc.handle_error-0>:
  403bc0:	48 83 ec 18          	sub    $0x18,%rsp
  403bc4:	48 89 3c 24          	mov    %rdi,(%rsp)
  403bc8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403bcd:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403bd2:	48 8b 3c 24          	mov    (%rsp),%rdi
  403bd6:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  403bdb:	e8 d0 04 00 00       	call   4040b0 <runtime::print_caller_location>
  403be0:	bf b6 91 40 00       	mov    $0x4091b6,%edi
  403be5:	be 20 00 00 00       	mov    $0x20,%esi
  403bea:	e8 21 00 00 00       	call   403c10 <runtime::print_string>
  403bef:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403bf4:	e8 37 03 00 00       	call   403f30 <runtime::print_i64>
  403bf9:	bf 0a 00 00 00       	mov    $0xa,%edi
  403bfe:	e8 7d 00 00 00       	call   403c80 <runtime::print_byte>
  403c03:	e8 48 f7 ff ff       	call   403350 <runtime::bounds_trap>
  403c08:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  403c0f:	00 

0000000000403c10 <runtime::print_string>:
  403c10:	48 83 ec 58          	sub    $0x58,%rsp
  403c14:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403c19:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403c1e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403c23:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403c28:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403c2d:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  403c32:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  403c39:	00 00 
  403c3b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403c40:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403c45:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403c4a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403c4f:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403c56:	00 00 
  403c58:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403c5d:	e8 2e 1c 00 00       	call   405890 <runtime::stderr_write>
  403c62:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403c67:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403c6c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403c71:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403c76:	48 83 c4 58          	add    $0x58,%rsp
  403c7a:	c3                   	ret
  403c7b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403c80 <runtime::print_byte>:
  403c80:	48 83 ec 68          	sub    $0x68,%rsp
  403c84:	40 88 f8             	mov    %dil,%al
  403c87:	88 44 24 07          	mov    %al,0x7(%rsp)
  403c8b:	8a 54 24 07          	mov    0x7(%rsp),%dl
  403c8f:	88 54 24 67          	mov    %dl,0x67(%rsp)
  403c93:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  403c9a:	00 00 
  403c9c:	0f 57 c0             	xorps  %xmm0,%xmm0
  403c9f:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403ca4:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  403ca9:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  403cae:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403cb3:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  403cba:	00 00 
  403cbc:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403cc1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403cc6:	88 11                	mov    %dl,(%rcx)
  403cc8:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403ccd:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403cd2:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  403cd7:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403cdc:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  403ce3:	00 00 
  403ce5:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  403cea:	e8 a1 1b 00 00       	call   405890 <runtime::stderr_write>
  403cef:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403cf4:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403cf9:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  403cfe:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403d03:	48 83 c4 68          	add    $0x68,%rsp
  403d07:	c3                   	ret
  403d08:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  403d0f:	00 

0000000000403d10 <runtime::print_u64>:
  403d10:	48 81 ec 48 01 00 00 	sub    $0x148,%rsp
  403d17:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  403d1c:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403d21:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  403d28:	00 
  403d29:	48 8d bc 24 bf 00 00 	lea    0xbf(%rsp),%rdi
  403d30:	00 
  403d31:	31 f6                	xor    %esi,%esi
  403d33:	ba 81 00 00 00       	mov    $0x81,%edx
  403d38:	e8 03 d3 ff ff       	call   401040 <memset@plt>
  403d3d:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403d42:	48 c7 84 24 b0 00 00 	movq   $0x81,0xb0(%rsp)
  403d49:	00 81 00 00 00 
  403d4e:	48 c7 84 24 a8 00 00 	movq   $0xa,0xa8(%rsp)
  403d55:	00 0a 00 00 00 
  403d5a:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  403d61:	00 
  403d62:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  403d69:	00 
  403d6a:	48 3b 84 24 a8 00 00 	cmp    0xa8(%rsp),%rax
  403d71:	00 
  403d72:	0f 93 c0             	setae  %al
  403d75:	24 01                	and    $0x1,%al
  403d77:	3c 00                	cmp    $0x0,%al
  403d79:	0f 84 ce 00 00 00    	je     403e4d <runtime::print_u64+0x13d>
  403d7f:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403d86:	00 
  403d87:	48 83 e8 01          	sub    $0x1,%rax
  403d8b:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  403d92:	00 
  403d93:	48 8d 84 24 bf 00 00 	lea    0xbf(%rsp),%rax
  403d9a:	00 
  403d9b:	48 03 84 24 b0 00 00 	add    0xb0(%rsp),%rax
  403da2:	00 
  403da3:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403da8:	48 c7 c0 48 c0 40 00 	mov    $0x40c048,%rax
  403daf:	48 8b 00             	mov    (%rax),%rax
  403db2:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403db7:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  403dbe:	00 
  403dbf:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  403dc4:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  403dcb:	00 
  403dcc:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  403dd1:	48 83 f8 00          	cmp    $0x0,%rax
  403dd5:	74 16                	je     403ded <runtime::print_u64+0xdd>
  403dd7:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  403ddc:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  403de1:	31 d2                	xor    %edx,%edx
  403de3:	48 f7 f1             	div    %rcx
  403de6:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  403deb:	eb 02                	jmp    403def <runtime::print_u64+0xdf>
  403ded:	0f 0b                	ud2
  403def:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403df4:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  403df9:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  403dfe:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403e01:	88 08                	mov    %cl,(%rax)
  403e03:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  403e0a:	00 
  403e0b:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403e10:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403e17:	00 
  403e18:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403e1d:	48 83 f8 00          	cmp    $0x0,%rax
  403e21:	74 16                	je     403e39 <runtime::print_u64+0x129>
  403e23:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  403e28:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403e2d:	31 d2                	xor    %edx,%edx
  403e2f:	48 f7 f1             	div    %rcx
  403e32:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403e37:	eb 02                	jmp    403e3b <runtime::print_u64+0x12b>
  403e39:	0f 0b                	ud2
  403e3b:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403e40:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  403e47:	00 
  403e48:	e9 15 ff ff ff       	jmp    403d62 <runtime::print_u64+0x52>
  403e4d:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403e54:	00 
  403e55:	48 83 e8 01          	sub    $0x1,%rax
  403e59:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  403e60:	00 
  403e61:	48 8d 84 24 bf 00 00 	lea    0xbf(%rsp),%rax
  403e68:	00 
  403e69:	48 03 84 24 b0 00 00 	add    0xb0(%rsp),%rax
  403e70:	00 
  403e71:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  403e76:	48 c7 c0 48 c0 40 00 	mov    $0x40c048,%rax
  403e7d:	48 8b 00             	mov    (%rax),%rax
  403e80:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403e85:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  403e8c:	00 
  403e8d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403e92:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  403e99:	00 
  403e9a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  403e9f:	48 83 f8 00          	cmp    $0x0,%rax
  403ea3:	74 16                	je     403ebb <runtime::print_u64+0x1ab>
  403ea5:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403eaa:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403eaf:	31 d2                	xor    %edx,%edx
  403eb1:	48 f7 f1             	div    %rcx
  403eb4:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  403eb9:	eb 02                	jmp    403ebd <runtime::print_u64+0x1ad>
  403ebb:	0f 0b                	ud2
  403ebd:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403ec2:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403ec7:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403ecc:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403ecf:	88 08                	mov    %cl,(%rax)
  403ed1:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  403ed8:	00 
  403ed9:	48 8d 8c 14 bf 00 00 	lea    0xbf(%rsp,%rdx,1),%rcx
  403ee0:	00 
  403ee1:	b8 81 00 00 00       	mov    $0x81,%eax
  403ee6:	48 29 d0             	sub    %rdx,%rax
  403ee9:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  403ef0:	00 
  403ef1:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  403ef8:	00 
  403ef9:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  403f00:	00 
  403f01:	48 8b b4 24 98 00 00 	mov    0x98(%rsp),%rsi
  403f08:	00 
  403f09:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  403f10:	00 00 00 00 00 
  403f15:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  403f1c:	00 
  403f1d:	e8 6e 19 00 00       	call   405890 <runtime::stderr_write>
  403f22:	48 81 c4 48 01 00 00 	add    $0x148,%rsp
  403f29:	c3                   	ret
  403f2a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000403f30 <runtime::print_i64>:
  403f30:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403f37:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403f3c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403f41:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403f48:	00 
  403f49:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403f50:	00 
  403f51:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  403f58:	00 00 
  403f5a:	0f 9c c0             	setl   %al
  403f5d:	24 01                	and    $0x1,%al
  403f5f:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  403f66:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403f6d:	00 
  403f6e:	31 c9                	xor    %ecx,%ecx
  403f70:	48 29 c1             	sub    %rax,%rcx
  403f73:	48 83 f8 00          	cmp    $0x0,%rax
  403f77:	48 0f 4c c1          	cmovl  %rcx,%rax
  403f7b:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403f82:	00 
  403f83:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  403f88:	31 f6                	xor    %esi,%esi
  403f8a:	ba 81 00 00 00       	mov    $0x81,%edx
  403f8f:	e8 ac d0 ff ff       	call   401040 <memset@plt>
  403f94:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  403f9b:	00 00 
  403f9d:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  403fa4:	00 0a 
  403fa6:	0f 9d c0             	setge  %al
  403fa9:	24 01                	and    $0x1,%al
  403fab:	3c 00                	cmp    $0x0,%al
  403fad:	74 5c                	je     40400b <runtime::print_i64+0xdb>
  403faf:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403fb4:	48 83 e8 01          	sub    $0x1,%rax
  403fb8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403fbd:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403fc2:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  403fc7:	48 c7 c0 48 c0 40 00 	mov    $0x40c048,%rax
  403fce:	48 8b 08             	mov    (%rax),%rcx
  403fd1:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403fd8:	00 
  403fd9:	be 0a 00 00 00       	mov    $0xa,%esi
  403fde:	48 99                	cqto
  403fe0:	48 f7 fe             	idiv   %rsi
  403fe3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403fe8:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403feb:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  403fef:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403ff6:	00 
  403ff7:	b9 0a 00 00 00       	mov    $0xa,%ecx
  403ffc:	48 99                	cqto
  403ffe:	48 f7 f9             	idiv   %rcx
  404001:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404008:	00 
  404009:	eb 92                	jmp    403f9d <runtime::print_i64+0x6d>
  40400b:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404010:	48 83 e8 01          	sub    $0x1,%rax
  404014:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404019:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40401e:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404023:	48 c7 c0 48 c0 40 00 	mov    $0x40c048,%rax
  40402a:	48 8b 08             	mov    (%rax),%rcx
  40402d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404034:	00 
  404035:	be 0a 00 00 00       	mov    $0xa,%esi
  40403a:	48 99                	cqto
  40403c:	48 f7 fe             	idiv   %rsi
  40403f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404044:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  404047:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  40404b:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  404052:	00 
  404053:	74 18                	je     40406d <runtime::print_i64+0x13d>
  404055:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40405a:	48 83 e8 01          	sub    $0x1,%rax
  40405e:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404063:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404068:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  40406d:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  404072:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  404077:	b8 81 00 00 00       	mov    $0x81,%eax
  40407c:	48 29 d0             	sub    %rdx,%rax
  40407f:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  404084:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404089:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40408e:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  404093:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40409a:	00 00 
  40409c:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  4040a1:	e8 ea 17 00 00       	call   405890 <runtime::stderr_write>
  4040a6:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4040ad:	c3                   	ret
  4040ae:	66 90                	xchg   %ax,%ax

00000000004040b0 <runtime::print_caller_location>:
  4040b0:	50                   	push   %rax
  4040b1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4040b5:	eb 00                	jmp    4040b7 <runtime::print_caller_location+0x7>
  4040b7:	48 8b 04 24          	mov    (%rsp),%rax
  4040bb:	48 8b 38             	mov    (%rax),%rdi
  4040be:	48 8b 70 08          	mov    0x8(%rax),%rsi
  4040c2:	e8 49 fb ff ff       	call   403c10 <runtime::print_string>
  4040c7:	bf 28 00 00 00       	mov    $0x28,%edi
  4040cc:	e8 af fb ff ff       	call   403c80 <runtime::print_byte>
  4040d1:	48 8b 04 24          	mov    (%rsp),%rax
  4040d5:	48 63 78 10          	movslq 0x10(%rax),%rdi
  4040d9:	e8 32 fc ff ff       	call   403d10 <runtime::print_u64>
  4040de:	48 8b 04 24          	mov    (%rsp),%rax
  4040e2:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  4040e6:	0f 95 c0             	setne  %al
  4040e9:	24 01                	and    $0x1,%al
  4040eb:	3c 00                	cmp    $0x0,%al
  4040ed:	74 17                	je     404106 <runtime::print_caller_location+0x56>
  4040ef:	bf 3a 00 00 00       	mov    $0x3a,%edi
  4040f4:	e8 87 fb ff ff       	call   403c80 <runtime::print_byte>
  4040f9:	48 8b 04 24          	mov    (%rsp),%rax
  4040fd:	48 63 78 14          	movslq 0x14(%rax),%rdi
  404101:	e8 0a fc ff ff       	call   403d10 <runtime::print_u64>
  404106:	bf 29 00 00 00       	mov    $0x29,%edi
  40410b:	e8 70 fb ff ff       	call   403c80 <runtime::print_byte>
  404110:	58                   	pop    %rax
  404111:	c3                   	ret
  404112:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  404119:	00 00 00 
  40411c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000404120 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  404120:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  404125:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  40412a:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  40412f:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  404134:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404139:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40413e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404143:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  404148:	48 01 c2             	add    %rax,%rdx
  40414b:	0f 92 c0             	setb   %al
  40414e:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  404153:	24 01                	and    $0x1,%al
  404155:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  404159:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40415e:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  404163:	0f 94 c0             	sete   %al
  404166:	24 01                	and    $0x1,%al
  404168:	48 89 11             	mov    %rdx,(%rcx)
  40416b:	c3                   	ret
  40416c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000404170 <runtime::memory_block_alloc>:
  404170:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  404177:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  40417c:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  404181:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  404186:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40418b:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  404190:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  404195:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40419c:	00 
  40419d:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4041a2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  4041a7:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  4041ac:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  4041b1:	48 8b 7c 24 48       	mov    0x48(%rsp),%rdi
  4041b6:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4041bb:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4041c0:	48 89 8c 24 00 01 00 	mov    %rcx,0x100(%rsp)
  4041c7:	00 
  4041c8:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  4041cf:	00 
  4041d0:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  4041d7:	00 
  4041d8:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4041dd:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  4041e4:	00 
  4041e5:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4041ea:	48 89 bc 24 f8 00 00 	mov    %rdi,0xf8(%rsp)
  4041f1:	00 
  4041f2:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  4041f9:	00 
  4041fa:	48 c7 84 24 e8 00 00 	movq   $0x0,0xe8(%rsp)
  404201:	00 00 00 00 00 
  404206:	c6 84 24 e7 00 00 00 	movb   $0x0,0xe7(%rsp)
  40420d:	00 
  40420e:	48 89 d6             	mov    %rdx,%rsi
  404211:	48 83 ee 31          	sub    $0x31,%rsi
  404215:	be 30 00 00 00       	mov    $0x30,%esi
  40421a:	48 0f 43 f2          	cmovae %rdx,%rsi
  40421e:	48 01 f7             	add    %rsi,%rdi
  404221:	48 89 bc 24 d8 00 00 	mov    %rdi,0xd8(%rsp)
  404228:	00 
  404229:	48 89 b4 24 d0 00 00 	mov    %rsi,0xd0(%rsp)
  404230:	00 
  404231:	48 89 d6             	mov    %rdx,%rsi
  404234:	48 83 ee 10          	sub    $0x10,%rsi
  404238:	be 10 00 00 00       	mov    $0x10,%esi
  40423d:	48 0f 4c d6          	cmovl  %rsi,%rdx
  404241:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  404248:	00 
  404249:	48 8b bc 24 d8 00 00 	mov    0xd8(%rsp),%rdi
  404250:	00 
  404251:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  404258:	00 
  404259:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  404260:	00 
  404261:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  404268:	00 
  404269:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  404270:	00 
  404271:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404278:	00 
  404279:	0f 57 c0             	xorps  %xmm0,%xmm0
  40427c:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  404283:	00 
  404284:	48 89 e0             	mov    %rsp,%rax
  404287:	4c 89 08             	mov    %r9,(%rax)
  40428a:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  404291:	00 
  404292:	e8 e9 23 00 00       	call   406680 <runtime::mem_alloc>
  404297:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  40429b:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4042a2:	00 
  4042a3:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4042a8:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  4042af:	00 
  4042b0:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4042b5:	3c 00                	cmp    $0x0,%al
  4042b7:	74 39                	je     4042f2 <runtime::memory_block_alloc+0x182>
  4042b9:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4042be:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  4042c2:	88 84 24 e7 00 00 00 	mov    %al,0xe7(%rsp)
  4042c9:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4042d0:	00 
  4042d1:	8a 84 24 e7 00 00 00 	mov    0xe7(%rsp),%al
  4042d8:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  4042df:	00 
  4042e0:	88 84 24 e7 00 00 00 	mov    %al,0xe7(%rsp)
  4042e7:	48 89 11             	mov    %rdx,(%rcx)
  4042ea:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4042f1:	c3                   	ret
  4042f2:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  4042f7:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4042fc:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  404301:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  404306:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40430b:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  404310:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  404317:	00 
  404318:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40431d:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  404324:	00 
  404325:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40432a:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  404331:	00 
  404332:	48 01 f0             	add    %rsi,%rax
  404335:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40433a:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40433f:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  404344:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40434b:	00 
  40434c:	48 89 50 10          	mov    %rdx,0x10(%rax)
  404350:	48 89 48 08          	mov    %rcx,0x8(%rax)
  404354:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40435b:	00 
  40435c:	48 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%rcx
  404363:	00 
  404364:	48 03 8c 24 d0 00 00 	add    0xd0(%rsp),%rcx
  40436b:	00 
  40436c:	48 89 48 18          	mov    %rcx,0x18(%rax)
  404370:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  404377:	00 
  404378:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40437d:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  404384:	00 
  404385:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  404389:	48 29 d1             	sub    %rdx,%rcx
  40438c:	48 89 48 28          	mov    %rcx,0x28(%rax)
  404390:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  404397:	00 
  404398:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  40439d:	0f 94 c0             	sete   %al
  4043a0:	24 01                	and    $0x1,%al
  4043a2:	0f b6 f8             	movzbl %al,%edi
  4043a5:	be 20 92 40 00       	mov    $0x409220,%esi
  4043aa:	b9 80 92 40 00       	mov    $0x409280,%ecx
  4043af:	ba 0f 00 00 00       	mov    $0xf,%edx
  4043b4:	e8 27 e8 ff ff       	call   402be0 <runtime::assert>
  4043b9:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  4043be:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4043c5:	00 
  4043c6:	48 83 38 00          	cmpq   $0x0,(%rax)
  4043ca:	0f 94 c0             	sete   %al
  4043cd:	24 01                	and    $0x1,%al
  4043cf:	0f b6 f8             	movzbl %al,%edi
  4043d2:	be a8 92 40 00       	mov    $0x4092a8,%esi
  4043d7:	b9 c0 92 40 00       	mov    $0x4092c0,%ecx
  4043dc:	ba 11 00 00 00       	mov    $0x11,%edx
  4043e1:	e8 fa e7 ff ff       	call   402be0 <runtime::assert>
  4043e6:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4043eb:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4043f2:	00 
  4043f3:	8a 84 24 e7 00 00 00 	mov    0xe7(%rsp),%al
  4043fa:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  404401:	00 
  404402:	88 84 24 e7 00 00 00 	mov    %al,0xe7(%rsp)
  404409:	48 89 11             	mov    %rdx,(%rcx)
  40440c:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  404413:	c3                   	ret
  404414:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40441b:	00 00 00 00 00 

0000000000404420 <runtime::memory_block_dealloc>:
  404420:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  404427:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40442c:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  404431:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404436:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40443d:	00 
  40443e:	48 83 f8 00          	cmp    $0x0,%rax
  404442:	0f 95 c0             	setne  %al
  404445:	24 01                	and    $0x1,%al
  404447:	3c 00                	cmp    $0x0,%al
  404449:	0f 84 5f 01 00 00    	je     4045ae <runtime::memory_block_dealloc+0x18e>
  40444f:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  404456:	00 
  404457:	48 8b 41 08          	mov    0x8(%rcx),%rax
  40445b:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  40445f:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  404466:	00 
  404467:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40446e:	00 
  40446f:	0f 57 c0             	xorps  %xmm0,%xmm0
  404472:	0f 29 04 24          	movaps %xmm0,(%rsp)
  404476:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40447d:	00 
  40447e:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  404485:	00 
  404486:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  40448d:	00 
  40448e:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  404495:	00 
  404496:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  40449d:	00 
  40449e:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  4044a5:	00 
  4044a6:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4044ad:	00 
  4044ae:	48 8d bc 24 a0 00 00 	lea    0xa0(%rsp),%rdi
  4044b5:	00 
  4044b6:	e8 e5 15 00 00       	call   405aa0 <runtime::[core.odin]::__init_context>
  4044bb:	0f 28 04 24          	movaps (%rsp),%xmm0
  4044bf:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4044c6:	00 
  4044c7:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  4044ce:	00 
  4044cf:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4044d4:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  4044d9:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  4044de:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4044e3:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4044e8:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4044ed:	e8 5e 15 00 00       	call   405a50 <runtime::default_context>
  4044f2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4044f7:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4044fc:	0f 28 44 24 30       	movaps 0x30(%rsp),%xmm0
  404501:	0f 28 4c 24 40       	movaps 0x40(%rsp),%xmm1
  404506:	0f 28 54 24 50       	movaps 0x50(%rsp),%xmm2
  40450b:	0f 28 5c 24 60       	movaps 0x60(%rsp),%xmm3
  404510:	0f 28 64 24 70       	movaps 0x70(%rsp),%xmm4
  404515:	0f 28 ac 24 80 00 00 	movaps 0x80(%rsp),%xmm5
  40451c:	00 
  40451d:	0f 28 b4 24 90 00 00 	movaps 0x90(%rsp),%xmm6
  404524:	00 
  404525:	0f 29 b4 24 00 01 00 	movaps %xmm6,0x100(%rsp)
  40452c:	00 
  40452d:	0f 29 ac 24 f0 00 00 	movaps %xmm5,0xf0(%rsp)
  404534:	00 
  404535:	0f 29 a4 24 e0 00 00 	movaps %xmm4,0xe0(%rsp)
  40453c:	00 
  40453d:	0f 29 9c 24 d0 00 00 	movaps %xmm3,0xd0(%rsp)
  404544:	00 
  404545:	0f 29 94 24 c0 00 00 	movaps %xmm2,0xc0(%rsp)
  40454c:	00 
  40454d:	0f 29 8c 24 b0 00 00 	movaps %xmm1,0xb0(%rsp)
  404554:	00 
  404555:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40455c:	00 
  40455d:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  404564:	00 
  404565:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  40456c:	00 
  40456d:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  404574:	00 
  404575:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40457c:	00 
  40457d:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  404584:	00 
  404585:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  40458c:	00 
  40458d:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  404592:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  404597:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40459c:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4045a1:	4c 8d 84 24 a0 00 00 	lea    0xa0(%rsp),%r8
  4045a8:	00 
  4045a9:	e8 02 22 00 00       	call   4067b0 <runtime::mem_free>
  4045ae:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  4045b5:	c3                   	ret
  4045b6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4045bd:	00 00 00 

00000000004045c0 <runtime::alloc_from_memory_block>:
  4045c0:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  4045c7:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4045cc:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4045d1:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4045d6:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4045db:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4045e0:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4045e5:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4045ea:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4045f1:	00 
  4045f2:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  4045f9:	00 
  4045fa:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  404601:	00 
  404602:	0f 57 c0             	xorps  %xmm0,%xmm0
  404605:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40460c:	00 
  40460d:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  404614:	00 
  404615:	48 83 f8 00          	cmp    $0x0,%rax
  404619:	0f 94 c0             	sete   %al
  40461c:	24 01                	and    $0x1,%al
  40461e:	3c 00                	cmp    $0x0,%al
  404620:	74 3e                	je     404660 <runtime::alloc_from_memory_block+0xa0>
  404622:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404627:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  40462e:	00 00 00 00 00 
  404633:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  40463a:	00 00 00 00 00 
  40463f:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  404646:	01 
  404647:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40464e:	00 
  40464f:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  404656:	b0 01                	mov    $0x1,%al
  404658:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40465f:	c3                   	ret
  404660:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404665:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40466a:	e8 31 11 00 00       	call   4057a0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  40466f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  404674:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40467b:	00 
  40467c:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404683:	00 
  404684:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40468b:	00 00 00 00 00 
  404690:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  404697:	00 
  404698:	e8 83 fa ff ff       	call   404120 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  40469d:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  4046a4:	00 
  4046a5:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4046aa:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  4046ae:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  4046b3:	75 4a                	jne    4046ff <runtime::alloc_from_memory_block+0x13f>
  4046b5:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4046ba:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  4046c1:	01 
  4046c2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4046c9:	00 
  4046ca:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4046d1:	00 
  4046d2:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  4046d9:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4046e0:	00 
  4046e1:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4046e8:	00 
  4046e9:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  4046f0:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4046f4:	48 89 11             	mov    %rdx,(%rcx)
  4046f7:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  4046fe:	c3                   	ret
  4046ff:	eb 00                	jmp    404701 <runtime::alloc_from_memory_block+0x141>
  404701:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404708:	00 
  404709:	48 8b 78 20          	mov    0x20(%rax),%rdi
  40470d:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  404712:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  404719:	00 00 
  40471b:	48 8d 54 24 60       	lea    0x60(%rsp),%rdx
  404720:	e8 fb f9 ff ff       	call   404120 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  404725:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40472a:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40472f:	88 44 24 47          	mov    %al,0x47(%rsp)
  404733:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  404738:	74 1a                	je     404754 <runtime::alloc_from_memory_block+0x194>
  40473a:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40473f:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  404746:	00 
  404747:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  40474b:	0f 97 c0             	seta   %al
  40474e:	24 01                	and    $0x1,%al
  404750:	3c 00                	cmp    $0x0,%al
  404752:	74 4a                	je     40479e <runtime::alloc_from_memory_block+0x1de>
  404754:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  404759:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  404760:	01 
  404761:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  404768:	00 
  404769:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  404770:	00 
  404771:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  404778:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40477f:	00 
  404780:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  404787:	00 
  404788:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  40478f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404793:	48 89 11             	mov    %rdx,(%rcx)
  404796:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40479d:	c3                   	ret
  40479e:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  4047a3:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4047aa:	00 
  4047ab:	48 8b 41 18          	mov    0x18(%rcx),%rax
  4047af:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  4047b3:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  4047ba:	00 
  4047bb:	48 01 d1             	add    %rdx,%rcx
  4047be:	48 01 c8             	add    %rcx,%rax
  4047c1:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4047c6:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4047cb:	48 89 04 24          	mov    %rax,(%rsp)
  4047cf:	bf 30 92 40 00       	mov    $0x409230,%edi
  4047d4:	31 c0                	xor    %eax,%eax
  4047d6:	41 89 c0             	mov    %eax,%r8d
  4047d9:	be 3c 00 00 00       	mov    $0x3c,%esi
  4047de:	ba 5c 00 00 00       	mov    $0x5c,%edx
  4047e3:	b9 31 00 00 00       	mov    $0x31,%ecx
  4047e8:	e8 13 ee ff ff       	call   403600 <runtime::multi_pointer_slice_expr_error>
  4047ed:	48 8b 14 24          	mov    (%rsp),%rdx
  4047f1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4047f6:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4047fb:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  404800:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  404805:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40480a:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40480f:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  404816:	00 
  404817:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40481e:	00 
  40481f:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404826:	00 
  404827:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  40482c:	48 8b 50 20          	mov    0x20(%rax),%rdx
  404830:	48 01 f2             	add    %rsi,%rdx
  404833:	48 89 50 20          	mov    %rdx,0x20(%rax)
  404837:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  40483e:	00 
  40483f:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  404846:	00 
  404847:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  40484e:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  404855:	00 
  404856:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40485d:	00 
  40485e:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  404865:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404869:	48 89 11             	mov    %rdx,(%rcx)
  40486c:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  404873:	c3                   	ret
  404874:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40487b:	00 00 00 00 00 

0000000000404880 <runtime::arena_alloc>:
  404880:	48 81 ec 48 01 00 00 	sub    $0x148,%rsp
  404887:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40488c:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  404891:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  404896:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40489b:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4048a0:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  4048a5:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4048aa:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  4048af:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4048b4:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4048b9:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  4048be:	48 89 b4 24 40 01 00 	mov    %rsi,0x140(%rsp)
  4048c5:	00 
  4048c6:	48 89 94 24 38 01 00 	mov    %rdx,0x138(%rsp)
  4048cd:	00 
  4048ce:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  4048d5:	00 
  4048d6:	0f 57 c0             	xorps  %xmm0,%xmm0
  4048d9:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  4048e0:	00 
  4048e1:	c6 84 24 1f 01 00 00 	movb   $0x0,0x11f(%rsp)
  4048e8:	00 
  4048e9:	48 89 c2             	mov    %rax,%rdx
  4048ec:	48 83 ea 01          	sub    $0x1,%rdx
  4048f0:	48 21 d0             	and    %rdx,%rax
  4048f3:	48 83 f8 00          	cmp    $0x0,%rax
  4048f7:	0f 94 c0             	sete   %al
  4048fa:	24 01                	and    $0x1,%al
  4048fc:	0f b6 f8             	movzbl %al,%edi
  4048ff:	be e8 92 40 00       	mov    $0x4092e8,%esi
  404904:	ba 1a 00 00 00       	mov    $0x1a,%edx
  404909:	e8 d2 e2 ff ff       	call   402be0 <runtime::assert>
  40490e:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404913:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40491a:	00 
  40491b:	48 83 bc 24 10 01 00 	cmpq   $0x0,0x110(%rsp)
  404922:	00 00 
  404924:	0f 94 c0             	sete   %al
  404927:	24 01                	and    $0x1,%al
  404929:	3c 00                	cmp    $0x0,%al
  40492b:	74 42                	je     40496f <runtime::arena_alloc+0xef>
  40492d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  404932:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  404939:	00 
  40493a:	48 8b b4 24 28 01 00 	mov    0x128(%rsp),%rsi
  404941:	00 
  404942:	8a 84 24 1f 01 00 00 	mov    0x11f(%rsp),%al
  404949:	48 89 b4 24 28 01 00 	mov    %rsi,0x128(%rsp)
  404950:	00 
  404951:	48 89 94 24 20 01 00 	mov    %rdx,0x120(%rsp)
  404958:	00 
  404959:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  404960:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404964:	48 89 11             	mov    %rdx,(%rcx)
  404967:	48 81 c4 48 01 00 00 	add    $0x148,%rsp
  40496e:	c3                   	ret
  40496f:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404976:	00 
  404977:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  40497c:	0f 94 c0             	sete   %al
  40497f:	24 01                	and    $0x1,%al
  404981:	3c 00                	cmp    $0x0,%al
  404983:	74 09                	je     40498e <runtime::arena_alloc+0x10e>
  404985:	31 c0                	xor    %eax,%eax
  404987:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40498c:	eb 15                	jmp    4049a3 <runtime::arena_alloc+0x123>
  40498e:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404995:	00 
  404996:	48 8b 40 10          	mov    0x10(%rax),%rax
  40499a:	48 8b 40 20          	mov    0x20(%rax),%rax
  40499e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4049a3:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  4049a8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4049ad:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4049b2:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  4049b9:	00 
  4049ba:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4049c1:	00 
  4049c2:	48 8b 78 10          	mov    0x10(%rax),%rdi
  4049c6:	48 8b b4 24 10 01 00 	mov    0x110(%rsp),%rsi
  4049cd:	00 
  4049ce:	0f 57 c0             	xorps  %xmm0,%xmm0
  4049d1:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  4049d8:	00 
  4049d9:	48 8d 8c 24 f0 00 00 	lea    0xf0(%rsp),%rcx
  4049e0:	00 
  4049e1:	e8 da fb ff ff       	call   4045c0 <runtime::alloc_from_memory_block>
  4049e6:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  4049ed:	00 
  4049ee:	48 8b 94 24 f8 00 00 	mov    0xf8(%rsp),%rdx
  4049f5:	00 
  4049f6:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  4049fd:	00 
  4049fe:	48 89 8c 24 20 01 00 	mov    %rcx,0x120(%rsp)
  404a05:	00 
  404a06:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  404a0d:	80 bc 24 1f 01 00 00 	cmpb   $0x1,0x11f(%rsp)
  404a14:	01 
  404a15:	0f 94 c0             	sete   %al
  404a18:	24 01                	and    $0x1,%al
  404a1a:	3c 00                	cmp    $0x0,%al
  404a1c:	0f 84 39 02 00 00    	je     404c5b <runtime::arena_alloc+0x3db>
  404a22:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404a29:	00 
  404a2a:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  404a2f:	0f 94 c0             	sete   %al
  404a32:	24 01                	and    $0x1,%al
  404a34:	3c 00                	cmp    $0x0,%al
  404a36:	74 10                	je     404a48 <runtime::arena_alloc+0x1c8>
  404a38:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404a3f:	00 
  404a40:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  404a47:	00 
  404a48:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  404a4d:	48 8b bc 24 10 01 00 	mov    0x110(%rsp),%rdi
  404a54:	00 
  404a55:	e8 d6 0d 00 00       	call   405830 <runtime::arena_alloc.align_forward_uint-0>
  404a5a:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404a61:	00 
  404a62:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  404a69:	00 
  404a6a:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404a71:	00 
  404a72:	48 8b 40 28          	mov    0x28(%rax),%rax
  404a76:	48 39 c1             	cmp    %rax,%rcx
  404a79:	48 0f 47 c1          	cmova  %rcx,%rax
  404a7d:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  404a84:	00 
  404a85:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404a8c:	00 
  404a8d:	48 83 38 00          	cmpq   $0x0,(%rax)
  404a91:	0f 94 c0             	sete   %al
  404a94:	24 01                	and    $0x1,%al
  404a96:	3c 00                	cmp    $0x0,%al
  404a98:	74 46                	je     404ae0 <runtime::arena_alloc+0x260>
  404a9a:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  404a9f:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404aa6:	00 
  404aa7:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404aac:	e8 df c6 ff ff       	call   401190 <runtime::heap_allocator>
  404ab1:	48 89 c1             	mov    %rax,%rcx
  404ab4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  404ab9:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  404ac0:	00 
  404ac1:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  404ac8:	00 
  404ac9:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404ad0:	00 
  404ad1:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  404ad8:	00 
  404ad9:	48 89 50 08          	mov    %rdx,0x8(%rax)
  404add:	48 89 08             	mov    %rcx,(%rax)
  404ae0:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  404ae5:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  404aea:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  404aef:	48 8b 94 24 40 01 00 	mov    0x140(%rsp),%rdx
  404af6:	00 
  404af7:	48 8b 02             	mov    (%rdx),%rax
  404afa:	48 8b 72 08          	mov    0x8(%rdx),%rsi
  404afe:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  404b05:	00 
  404b06:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  404b0d:	00 
  404b0e:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  404b15:	00 
  404b16:	48 8b bc 24 a0 00 00 	mov    0xa0(%rsp),%rdi
  404b1d:	00 
  404b1e:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  404b25:	00 
  404b26:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  404b2d:	00 00 00 00 00 
  404b32:	48 89 e0             	mov    %rsp,%rax
  404b35:	4c 89 08             	mov    %r9,(%rax)
  404b38:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  404b3f:	00 
  404b40:	e8 2b f6 ff ff       	call   404170 <runtime::memory_block_alloc>
  404b45:	88 44 24 0f          	mov    %al,0xf(%rsp)
  404b49:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  404b50:	00 
  404b51:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  404b56:	3c 00                	cmp    $0x0,%al
  404b58:	74 4d                	je     404ba7 <runtime::arena_alloc+0x327>
  404b5a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  404b5f:	8a 44 24 0f          	mov    0xf(%rsp),%al
  404b63:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  404b6a:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  404b71:	00 
  404b72:	48 8b b4 24 28 01 00 	mov    0x128(%rsp),%rsi
  404b79:	00 
  404b7a:	8a 84 24 1f 01 00 00 	mov    0x11f(%rsp),%al
  404b81:	48 89 b4 24 28 01 00 	mov    %rsi,0x128(%rsp)
  404b88:	00 
  404b89:	48 89 94 24 20 01 00 	mov    %rdx,0x120(%rsp)
  404b90:	00 
  404b91:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  404b98:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404b9c:	48 89 11             	mov    %rdx,(%rcx)
  404b9f:	48 81 c4 48 01 00 00 	add    $0x148,%rsp
  404ba6:	c3                   	ret
  404ba7:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  404bac:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404bb1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404bb6:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  404bbd:	00 
  404bbe:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404bc5:	00 
  404bc6:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404bcd:	00 
  404bce:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  404bd2:	48 89 08             	mov    %rcx,(%rax)
  404bd5:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404bdc:	00 
  404bdd:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  404be4:	00 
  404be5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404be9:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404bf0:	00 
  404bf1:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  404bf8:	00 
  404bf9:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  404bfd:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404c01:	48 01 f1             	add    %rsi,%rcx
  404c04:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404c08:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  404c0f:	00 00 00 00 00 
  404c14:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404c1b:	00 
  404c1c:	48 8b 78 10          	mov    0x10(%rax),%rdi
  404c20:	48 8b b4 24 10 01 00 	mov    0x110(%rsp),%rsi
  404c27:	00 
  404c28:	0f 57 c0             	xorps  %xmm0,%xmm0
  404c2b:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  404c30:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  404c35:	e8 86 f9 ff ff       	call   4045c0 <runtime::alloc_from_memory_block>
  404c3a:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  404c3f:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  404c44:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  404c4b:	00 
  404c4c:	48 89 8c 24 20 01 00 	mov    %rcx,0x120(%rsp)
  404c53:	00 
  404c54:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  404c5b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  404c60:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404c67:	00 
  404c68:	48 8b 70 10          	mov    0x10(%rax),%rsi
  404c6c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  404c70:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  404c74:	48 8b bc 24 08 01 00 	mov    0x108(%rsp),%rdi
  404c7b:	00 
  404c7c:	48 29 fe             	sub    %rdi,%rsi
  404c7f:	48 01 f2             	add    %rsi,%rdx
  404c82:	48 89 50 18          	mov    %rdx,0x18(%rax)
  404c86:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  404c8d:	00 
  404c8e:	48 8b b4 24 28 01 00 	mov    0x128(%rsp),%rsi
  404c95:	00 
  404c96:	8a 84 24 1f 01 00 00 	mov    0x11f(%rsp),%al
  404c9d:	48 89 b4 24 28 01 00 	mov    %rsi,0x128(%rsp)
  404ca4:	00 
  404ca5:	48 89 94 24 20 01 00 	mov    %rdx,0x120(%rsp)
  404cac:	00 
  404cad:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  404cb4:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404cb8:	48 89 11             	mov    %rdx,(%rcx)
  404cbb:	48 81 c4 48 01 00 00 	add    $0x148,%rsp
  404cc2:	c3                   	ret
  404cc3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404cca:	84 00 00 00 00 00 

0000000000404cd0 <runtime::arena_free_last_memory_block>:
  404cd0:	48 83 ec 28          	sub    $0x28,%rsp
  404cd4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404cd9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  404cde:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404ce3:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  404ce8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404ced:	48 8b 40 10          	mov    0x10(%rax),%rax
  404cf1:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404cf6:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  404cfc:	0f 95 c0             	setne  %al
  404cff:	24 01                	and    $0x1,%al
  404d01:	3c 00                	cmp    $0x0,%al
  404d03:	74 39                	je     404d3e <runtime::arena_free_last_memory_block+0x6e>
  404d05:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404d0a:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404d0f:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404d14:	48 8b 09             	mov    (%rcx),%rcx
  404d17:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404d1b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404d20:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404d25:	48 8b 51 28          	mov    0x28(%rcx),%rdx
  404d29:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404d2d:	48 29 d1             	sub    %rdx,%rcx
  404d30:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404d34:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  404d39:	e8 e2 f6 ff ff       	call   404420 <runtime::memory_block_dealloc>
  404d3e:	48 83 c4 28          	add    $0x28,%rsp
  404d42:	c3                   	ret
  404d43:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404d4a:	84 00 00 00 00 00 

0000000000404d50 <runtime::arena_free_all>:
  404d50:	48 83 ec 28          	sub    $0x28,%rsp
  404d54:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404d59:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  404d5e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  404d63:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404d68:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  404d6d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404d72:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404d77:	0f 95 c0             	setne  %al
  404d7a:	24 01                	and    $0x1,%al
  404d7c:	3c 00                	cmp    $0x0,%al
  404d7e:	74 2c                	je     404dac <runtime::arena_free_all+0x5c>
  404d80:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404d85:	48 8b 40 10          	mov    0x10(%rax),%rax
  404d89:	48 83 38 00          	cmpq   $0x0,(%rax)
  404d8d:	0f 95 c0             	setne  %al
  404d90:	24 01                	and    $0x1,%al
  404d92:	3c 00                	cmp    $0x0,%al
  404d94:	74 16                	je     404dac <runtime::arena_free_all+0x5c>
  404d96:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  404d9b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404da0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  404da5:	e8 26 ff ff ff       	call   404cd0 <runtime::arena_free_last_memory_block>
  404daa:	eb c1                	jmp    404d6d <runtime::arena_free_all+0x1d>
  404dac:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404db1:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404db6:	0f 95 c0             	setne  %al
  404db9:	24 01                	and    $0x1,%al
  404dbb:	3c 00                	cmp    $0x0,%al
  404dbd:	74 32                	je     404df1 <runtime::arena_free_all+0xa1>
  404dbf:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404dc4:	48 8b 40 10          	mov    0x10(%rax),%rax
  404dc8:	48 8b 78 18          	mov    0x18(%rax),%rdi
  404dcc:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404dd1:	48 8b 40 10          	mov    0x10(%rax),%rax
  404dd5:	48 8b 50 20          	mov    0x20(%rax),%rdx
  404dd9:	31 f6                	xor    %esi,%esi
  404ddb:	e8 60 c2 ff ff       	call   401040 <memset@plt>
  404de0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404de5:	48 8b 40 10          	mov    0x10(%rax),%rax
  404de9:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  404df0:	00 
  404df1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404df6:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  404dfd:	00 
  404dfe:	48 83 c4 28          	add    $0x28,%rsp
  404e02:	c3                   	ret
  404e03:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404e0a:	84 00 00 00 00 00 

0000000000404e10 <runtime::arena_destroy>:
  404e10:	48 83 ec 28          	sub    $0x28,%rsp
  404e14:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404e19:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  404e1e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404e23:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  404e28:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404e2d:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404e32:	0f 95 c0             	setne  %al
  404e35:	24 01                	and    $0x1,%al
  404e37:	3c 00                	cmp    $0x0,%al
  404e39:	74 49                	je     404e84 <runtime::arena_destroy+0x74>
  404e3b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404e40:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404e45:	48 8b 40 10          	mov    0x10(%rax),%rax
  404e49:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404e4e:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404e53:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404e58:	48 8b 09             	mov    (%rcx),%rcx
  404e5b:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404e5f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404e64:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404e69:	48 8b 51 28          	mov    0x28(%rcx),%rdx
  404e6d:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404e71:	48 29 d1             	sub    %rdx,%rcx
  404e74:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404e78:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  404e7d:	e8 9e f5 ff ff       	call   404420 <runtime::memory_block_dealloc>
  404e82:	eb a4                	jmp    404e28 <runtime::arena_destroy+0x18>
  404e84:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404e89:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  404e90:	00 
  404e91:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404e96:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  404e9d:	00 
  404e9e:	48 83 c4 28          	add    $0x28,%rsp
  404ea2:	c3                   	ret
  404ea3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404eaa:	84 00 00 00 00 00 

0000000000404eb0 <runtime::arena_allocator_proc>:
  404eb0:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  404eb7:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  404ebc:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  404ec3:	00 
  404ec4:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  404ecb:	00 
  404ecc:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  404ed3:	00 
  404ed4:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  404edb:	00 
  404edc:	40 88 f0             	mov    %sil,%al
  404edf:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  404ee6:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  404eed:	00 
  404eee:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  404ef5:	00 
  404ef6:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  404efd:	00 
  404efe:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  404f05:	00 
  404f06:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  404f0d:	00 
  404f0e:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  404f15:	00 
  404f16:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  404f1d:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  404f22:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  404f29:	00 
  404f2a:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404f31:	00 
  404f32:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  404f39:	00 
  404f3a:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  404f41:	00 
  404f42:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  404f49:	00 
  404f4a:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  404f51:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  404f58:	00 
  404f59:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  404f60:	00 
  404f61:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  404f68:	00 
  404f69:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  404f70:	00 
  404f71:	0f 57 c0             	xorps  %xmm0,%xmm0
  404f74:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  404f7b:	00 
  404f7c:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404f83:	00 
  404f84:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  404f8b:	00 
  404f8c:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  404f93:	00 
  404f94:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  404f9b:	00 
  404f9c:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  404fa3:	00 
  404fa4:	0f b6 c8             	movzbl %al,%ecx
  404fa7:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  404fac:	2c 07                	sub    $0x7,%al
  404fae:	0f 87 9a 07 00 00    	ja     40574e <runtime::arena_allocator_proc+0x89e>
  404fb4:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  404fb9:	48 8b 04 c5 e0 91 40 	mov    0x4091e0(,%rax,8),%rax
  404fc0:	00 
  404fc1:	ff e0                	jmp    *%rax
  404fc3:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404fca:	00 
  404fcb:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404fd2:	00 
  404fd3:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404fda:	00 
  404fdb:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404fe2:	00 
  404fe3:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404fea:	00 
  404feb:	0f 57 c0             	xorps  %xmm0,%xmm0
  404fee:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  404ff5:	00 
  404ff6:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  404ffd:	00 
  404ffe:	e8 7d f8 ff ff       	call   404880 <runtime::arena_alloc>
  405003:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40500a:	00 
  40500b:	40 88 c7             	mov    %al,%dil
  40500e:	40 88 f8             	mov    %dil,%al
  405011:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  405018:	00 
  405019:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  405020:	00 
  405021:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  405028:	00 
  405029:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  405030:	00 
  405031:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  405038:	00 
  405039:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40503d:	48 89 11             	mov    %rdx,(%rcx)
  405040:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  405047:	c3                   	ret
  405048:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  40504f:	04 
  405050:	e9 f9 06 00 00       	jmp    40574e <runtime::arena_allocator_proc+0x89e>
  405055:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  40505c:	00 
  40505d:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  405064:	00 
  405065:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  40506c:	00 
  40506d:	e8 de fc ff ff       	call   404d50 <runtime::arena_free_all>
  405072:	e9 d7 06 00 00       	jmp    40574e <runtime::arena_allocator_proc+0x89e>
  405077:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40507e:	00 
  40507f:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  405086:	00 
  405087:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  40508e:	00 00 
  405090:	0f 94 c1             	sete   %cl
  405093:	80 e1 01             	and    $0x1,%cl
  405096:	b0 01                	mov    $0x1,%al
  405098:	38 c8                	cmp    %cl,%al
  40509a:	74 25                	je     4050c1 <runtime::arena_allocator_proc+0x211>
  40509c:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  4050a3:	00 
  4050a4:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  4050ab:	00 
  4050ac:	0f 94 c1             	sete   %cl
  4050af:	80 e1 01             	and    $0x1,%cl
  4050b2:	b0 01                	mov    $0x1,%al
  4050b4:	38 c8                	cmp    %cl,%al
  4050b6:	0f 84 a8 00 00 00    	je     405164 <runtime::arena_allocator_proc+0x2b4>
  4050bc:	e9 85 00 00 00       	jmp    405146 <runtime::arena_allocator_proc+0x296>
  4050c1:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  4050c8:	00 
  4050c9:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4050d0:	00 
  4050d1:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4050d8:	00 
  4050d9:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4050e0:	00 
  4050e1:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4050e8:	00 
  4050e9:	0f 57 c0             	xorps  %xmm0,%xmm0
  4050ec:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  4050f3:	00 
  4050f4:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  4050fb:	00 
  4050fc:	e8 7f f7 ff ff       	call   404880 <runtime::arena_alloc>
  405101:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  405108:	00 
  405109:	40 88 c7             	mov    %al,%dil
  40510c:	40 88 f8             	mov    %dil,%al
  40510f:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  405116:	00 
  405117:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  40511e:	00 
  40511f:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  405126:	00 
  405127:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40512e:	00 
  40512f:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  405136:	00 
  405137:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40513b:	48 89 11             	mov    %rdx,(%rcx)
  40513e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  405145:	c3                   	ret
  405146:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  40514d:	00 00 
  40514f:	0f 94 c1             	sete   %cl
  405152:	80 e1 01             	and    $0x1,%cl
  405155:	b0 01                	mov    $0x1,%al
  405157:	38 c8                	cmp    %cl,%al
  405159:	0f 84 e5 00 00 00    	je     405244 <runtime::arena_allocator_proc+0x394>
  40515f:	e9 b7 00 00 00       	jmp    40521b <runtime::arena_allocator_proc+0x36b>
  405164:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40516b:	00 
  40516c:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  405171:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  405178:	00 
  405179:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  40517e:	bf 30 92 40 00       	mov    $0x409230,%edi
  405183:	31 c0                	xor    %eax,%eax
  405185:	41 89 c0             	mov    %eax,%r8d
  405188:	be 3c 00 00 00       	mov    $0x3c,%esi
  40518d:	ba db 00 00 00       	mov    $0xdb,%edx
  405192:	b9 13 00 00 00       	mov    $0x13,%ecx
  405197:	e8 64 e4 ff ff       	call   403600 <runtime::multi_pointer_slice_expr_error>
  40519c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4051a1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4051a6:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4051ad:	00 
  4051ae:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  4051b5:	00 
  4051b6:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  4051bd:	00 
  4051be:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  4051c5:	00 
  4051c6:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  4051cd:	00 
  4051ce:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4051d5:	00 
  4051d6:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4051dd:	00 
  4051de:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4051e5:	00 
  4051e6:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4051ed:	00 
  4051ee:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4051f5:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4051fc:	00 
  4051fd:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  405204:	00 
  405205:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40520c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405210:	48 89 11             	mov    %rdx,(%rcx)
  405213:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40521a:	c3                   	ret
  40521b:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  405222:	00 
  405223:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  40522a:	00 
  40522b:	48 83 e9 01          	sub    $0x1,%rcx
  40522f:	48 21 c8             	and    %rcx,%rax
  405232:	48 83 f8 00          	cmp    $0x0,%rax
  405236:	0f 94 c1             	sete   %cl
  405239:	80 e1 01             	and    $0x1,%cl
  40523c:	b0 01                	mov    $0x1,%al
  40523e:	38 c8                	cmp    %cl,%al
  405240:	74 54                	je     405296 <runtime::arena_allocator_proc+0x3e6>
  405242:	eb 4d                	jmp    405291 <runtime::arena_allocator_proc+0x3e1>
  405244:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40524b:	00 
  40524c:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  405253:	04 
  405254:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  40525b:	00 
  40525c:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  405263:	00 
  405264:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40526b:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  405272:	00 
  405273:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40527a:	00 
  40527b:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  405282:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405286:	48 89 11             	mov    %rdx,(%rcx)
  405289:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  405290:	c3                   	ret
  405291:	e9 94 02 00 00       	jmp    40552a <runtime::arena_allocator_proc+0x67a>
  405296:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  40529d:	00 
  40529e:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  4052a5:	00 
  4052a6:	0f 92 c0             	setb   %al
  4052a9:	24 01                	and    $0x1,%al
  4052ab:	3c 00                	cmp    $0x0,%al
  4052ad:	0f 84 b7 00 00 00    	je     40536a <runtime::arena_allocator_proc+0x4ba>
  4052b3:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4052ba:	00 
  4052bb:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4052c0:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  4052c7:	00 
  4052c8:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  4052cd:	bf 30 92 40 00       	mov    $0x409230,%edi
  4052d2:	31 c0                	xor    %eax,%eax
  4052d4:	41 89 c0             	mov    %eax,%r8d
  4052d7:	be 3c 00 00 00       	mov    $0x3c,%esi
  4052dc:	ba e3 00 00 00       	mov    $0xe3,%edx
  4052e1:	b9 14 00 00 00       	mov    $0x14,%ecx
  4052e6:	e8 15 e3 ff ff       	call   403600 <runtime::multi_pointer_slice_expr_error>
  4052eb:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4052f0:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4052f5:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4052fc:	00 
  4052fd:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  405304:	00 
  405305:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  40530c:	00 
  40530d:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  405314:	00 
  405315:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  40531c:	00 
  40531d:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  405324:	00 
  405325:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  40532c:	00 
  40532d:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  405334:	00 
  405335:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40533c:	00 
  40533d:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  405344:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40534b:	00 
  40534c:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  405353:	00 
  405354:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40535b:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40535f:	48 89 11             	mov    %rdx,(%rcx)
  405362:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  405369:	c3                   	ret
  40536a:	eb 00                	jmp    40536c <runtime::arena_allocator_proc+0x4bc>
  40536c:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  405373:	00 
  405374:	48 8b 40 10          	mov    0x10(%rax),%rax
  405378:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40537f:	00 
  405380:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  405387:	00 00 
  405389:	0f 95 c0             	setne  %al
  40538c:	24 01                	and    $0x1,%al
  40538e:	3c 00                	cmp    $0x0,%al
  405390:	0f 84 92 01 00 00    	je     405528 <runtime::arena_allocator_proc+0x678>
  405396:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40539d:	00 
  40539e:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  4053a5:	00 
  4053a6:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  4053aa:	48 29 c8             	sub    %rcx,%rax
  4053ad:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  4053b4:	00 
  4053b5:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  4053bc:	00 
  4053bd:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  4053c4:	00 
  4053c5:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  4053cc:	00 
  4053cd:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  4053d4:	00 
  4053d5:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  4053dc:	00 
  4053dd:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  4053e4:	00 
  4053e5:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  4053ec:	00 
  4053ed:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  4053f4:	00 
  4053f5:	0f 92 c0             	setb   %al
  4053f8:	24 01                	and    $0x1,%al
  4053fa:	3c 00                	cmp    $0x0,%al
  4053fc:	0f 84 24 01 00 00    	je     405526 <runtime::arena_allocator_proc+0x676>
  405402:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  405409:	00 
  40540a:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  405411:	00 
  405412:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  405416:	0f 94 c0             	sete   %al
  405419:	24 01                	and    $0x1,%al
  40541b:	3c 00                	cmp    $0x0,%al
  40541d:	0f 84 03 01 00 00    	je     405526 <runtime::arena_allocator_proc+0x676>
  405423:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  40542a:	00 
  40542b:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  405432:	00 
  405433:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  405437:	0f 96 c0             	setbe  %al
  40543a:	24 01                	and    $0x1,%al
  40543c:	3c 00                	cmp    $0x0,%al
  40543e:	0f 84 e2 00 00 00    	je     405526 <runtime::arena_allocator_proc+0x676>
  405444:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40544b:	00 
  40544c:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  405453:	00 
  405454:	48 89 48 20          	mov    %rcx,0x20(%rax)
  405458:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40545f:	00 
  405460:	48 8b 40 18          	mov    0x18(%rax),%rax
  405464:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  405469:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  405470:	00 
  405471:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  405476:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  40547d:	00 
  40547e:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  405483:	bf 30 92 40 00       	mov    $0x409230,%edi
  405488:	be 3c 00 00 00       	mov    $0x3c,%esi
  40548d:	ba ee 00 00 00       	mov    $0xee,%edx
  405492:	b9 17 00 00 00       	mov    $0x17,%ecx
  405497:	e8 64 e1 ff ff       	call   403600 <runtime::multi_pointer_slice_expr_error>
  40549c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4054a1:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4054a6:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4054ab:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4054b2:	00 
  4054b3:	48 01 f2             	add    %rsi,%rdx
  4054b6:	48 29 f0             	sub    %rsi,%rax
  4054b9:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  4054c0:	00 
  4054c1:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4054c8:	00 
  4054c9:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  4054d0:	00 
  4054d1:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  4054d8:	00 
  4054d9:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4054e0:	00 
  4054e1:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4054e8:	00 
  4054e9:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4054f0:	00 
  4054f1:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4054f8:	00 
  4054f9:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  405500:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  405507:	00 
  405508:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40550f:	00 
  405510:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  405517:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40551b:	48 89 11             	mov    %rdx,(%rcx)
  40551e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  405525:	c3                   	ret
  405526:	eb 00                	jmp    405528 <runtime::arena_allocator_proc+0x678>
  405528:	eb 00                	jmp    40552a <runtime::arena_allocator_proc+0x67a>
  40552a:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  405531:	00 
  405532:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  405539:	00 
  40553a:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  405541:	00 
  405542:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  405549:	00 
  40554a:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  405551:	00 
  405552:	0f 57 c0             	xorps  %xmm0,%xmm0
  405555:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40555c:	00 
  40555d:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  405564:	00 
  405565:	e8 16 f3 ff ff       	call   404880 <runtime::arena_alloc>
  40556a:	88 44 24 27          	mov    %al,0x27(%rsp)
  40556e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  405575:	00 
  405576:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40557b:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  405582:	00 
  405583:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405588:	3c 00                	cmp    $0x0,%al
  40558a:	74 50                	je     4055dc <runtime::arena_allocator_proc+0x72c>
  40558c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  405593:	00 
  405594:	8a 44 24 27          	mov    0x27(%rsp),%al
  405598:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40559f:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4055a6:	00 
  4055a7:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4055ae:	00 
  4055af:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4055b6:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4055bd:	00 
  4055be:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4055c5:	00 
  4055c6:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4055cd:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4055d1:	48 89 11             	mov    %rdx,(%rcx)
  4055d4:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4055db:	c3                   	ret
  4055dc:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4055e1:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4055e6:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  4055ed:	00 
  4055ee:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4055f5:	00 
  4055f6:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  4055fd:	00 00 
  4055ff:	0f 94 c0             	sete   %al
  405602:	24 01                	and    $0x1,%al
  405604:	3c 00                	cmp    $0x0,%al
  405606:	74 45                	je     40564d <runtime::arena_allocator_proc+0x79d>
  405608:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40560f:	00 
  405610:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  405617:	00 
  405618:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40561f:	00 
  405620:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  405627:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40562e:	00 
  40562f:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  405636:	00 
  405637:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40563e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405642:	48 89 11             	mov    %rdx,(%rcx)
  405645:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40564c:	c3                   	ret
  40564d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  405654:	00 
  405655:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40565a:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  405661:	00 
  405662:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  405667:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40566e:	00 
  40566f:	48 89 04 24          	mov    %rax,(%rsp)
  405673:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  40567a:	00 
  40567b:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  405680:	bf 30 92 40 00       	mov    $0x409230,%edi
  405685:	31 c0                	xor    %eax,%eax
  405687:	41 89 c0             	mov    %eax,%r8d
  40568a:	be 3c 00 00 00       	mov    $0x3c,%esi
  40568f:	ba f9 00 00 00       	mov    $0xf9,%edx
  405694:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  405699:	e8 62 df ff ff       	call   403600 <runtime::multi_pointer_slice_expr_error>
  40569e:	48 8b 0c 24          	mov    (%rsp),%rcx
  4056a2:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4056a7:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4056ac:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4056b1:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  4056b8:	00 
  4056b9:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  4056c0:	00 
  4056c1:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  4056c8:	00 
  4056c9:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  4056d0:	00 
  4056d1:	e8 ca d1 ff ff       	call   4028a0 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  4056d6:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4056dd:	00 
  4056de:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  4056e5:	00 
  4056e6:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4056ed:	00 
  4056ee:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4056f5:	00 
  4056f6:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  4056fd:	00 
  4056fe:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  405705:	00 
  405706:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40570a:	48 89 08             	mov    %rcx,(%rax)
  40570d:	31 c0                	xor    %eax,%eax
  40570f:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  405716:	c3                   	ret
  405717:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40571e:	00 
  40571f:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  405726:	00 
  405727:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  40572e:	00 00 
  405730:	0f 95 c0             	setne  %al
  405733:	24 01                	and    $0x1,%al
  405735:	3c 00                	cmp    $0x0,%al
  405737:	74 0b                	je     405744 <runtime::arena_allocator_proc+0x894>
  405739:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  405740:	00 
  405741:	c6 00 5d             	movb   $0x5d,(%rax)
  405744:	eb 08                	jmp    40574e <runtime::arena_allocator_proc+0x89e>
  405746:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  40574d:	04 
  40574e:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  405755:	00 
  405756:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  40575d:	00 
  40575e:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  405765:	00 
  405766:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40576d:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  405774:	00 
  405775:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40577c:	00 
  40577d:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  405784:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405788:	48 89 11             	mov    %rdx,(%rcx)
  40578b:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  405792:	c3                   	ret
  405793:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40579a:	84 00 00 00 00 00 

00000000004057a0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  4057a0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  4057a5:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  4057aa:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4057af:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4057b4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4057b9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4057be:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4057c5:	00 00 
  4057c7:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4057cc:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  4057d0:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4057d5:	48 03 4a 20          	add    0x20(%rdx),%rcx
  4057d9:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  4057de:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4057e3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4057e8:	48 83 e8 01          	sub    $0x1,%rax
  4057ec:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4057f1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4057f6:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  4057fb:	48 83 f8 00          	cmp    $0x0,%rax
  4057ff:	0f 95 c0             	setne  %al
  405802:	24 01                	and    $0x1,%al
  405804:	3c 00                	cmp    $0x0,%al
  405806:	74 17                	je     40581f <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  405808:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40580d:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405812:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  405817:	48 29 c8             	sub    %rcx,%rax
  40581a:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40581f:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405824:	c3                   	ret
  405825:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40582c:	00 00 00 00 

0000000000405830 <runtime::arena_alloc.align_forward_uint-0>:
  405830:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  405835:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40583a:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40583f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405844:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405849:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40584e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405853:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405858:	48 83 e9 01          	sub    $0x1,%rcx
  40585c:	48 21 c8             	and    %rcx,%rax
  40585f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405864:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  40586a:	0f 95 c0             	setne  %al
  40586d:	24 01                	and    $0x1,%al
  40586f:	3c 00                	cmp    $0x0,%al
  405871:	74 14                	je     405887 <runtime::arena_alloc.align_forward_uint-0+0x57>
  405873:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405878:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  40587d:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  405882:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405887:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40588c:	c3                   	ret
  40588d:	0f 1f 00             	nopl   (%rax)

0000000000405890 <runtime::stderr_write>:
  405890:	48 83 ec 48          	sub    $0x48,%rsp
  405894:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405899:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40589e:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4058a3:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4058a8:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4058ad:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4058b2:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4058b7:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4058be:	00 00 
  4058c0:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  4058c5:	e8 d6 c0 ff ff       	call   4019a0 <runtime::[os_specific_linux.odin]::_stderr_write>
  4058ca:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4058cf:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4058d4:	48 89 11             	mov    %rdx,(%rcx)
  4058d7:	48 83 c4 48          	add    $0x48,%rsp
  4058db:	c3                   	ret
  4058dc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004058e0 <runtime::__type_info_of>:
  4058e0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  4058e5:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4058ea:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4058ef:	48 c7 c0 10 93 40 00 	mov    $0x409310,%rax
  4058f6:	48 8b 40 08          	mov    0x8(%rax),%rax
  4058fa:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4058ff:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405904:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  405909:	48 83 f8 00          	cmp    $0x0,%rax
  40590d:	74 16                	je     405925 <runtime::__type_info_of+0x45>
  40590f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  405914:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405919:	31 d2                	xor    %edx,%edx
  40591b:	48 f7 f1             	div    %rcx
  40591e:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  405923:	eb 02                	jmp    405927 <runtime::__type_info_of+0x47>
  405925:	0f 0b                	ud2
  405927:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40592c:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405931:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  405938:	00 00 
  40593a:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  405941:	00 00 
  405943:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405948:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  40594d:	0f 83 9f 00 00 00    	jae    4059f2 <runtime::__type_info_of+0x112>
  405953:	48 c7 c0 10 93 40 00 	mov    $0x409310,%rax
  40595a:	48 8b 00             	mov    (%rax),%rax
  40595d:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405962:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  405966:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40596b:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  405971:	0f 95 c0             	setne  %al
  405974:	24 01                	and    $0x1,%al
  405976:	3c 00                	cmp    $0x0,%al
  405978:	74 1d                	je     405997 <runtime::__type_info_of+0xb7>
  40597a:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  40597f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405984:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  405988:	0f 94 c0             	sete   %al
  40598b:	24 01                	and    $0x1,%al
  40598d:	3c 00                	cmp    $0x0,%al
  40598f:	74 06                	je     405997 <runtime::__type_info_of+0xb7>
  405991:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405996:	c3                   	ret
  405997:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40599c:	48 83 c0 01          	add    $0x1,%rax
  4059a0:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  4059a5:	0f 92 c0             	setb   %al
  4059a8:	24 01                	and    $0x1,%al
  4059aa:	3c 00                	cmp    $0x0,%al
  4059ac:	74 10                	je     4059be <runtime::__type_info_of+0xde>
  4059ae:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4059b3:	48 83 c0 01          	add    $0x1,%rax
  4059b7:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4059bc:	eb 09                	jmp    4059c7 <runtime::__type_info_of+0xe7>
  4059be:	31 c0                	xor    %eax,%eax
  4059c0:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4059c5:	eb 00                	jmp    4059c7 <runtime::__type_info_of+0xe7>
  4059c7:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4059cc:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4059d1:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4059d6:	48 83 c0 01          	add    $0x1,%rax
  4059da:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4059df:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4059e4:	48 83 c0 01          	add    $0x1,%rax
  4059e8:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4059ed:	e9 51 ff ff ff       	jmp    405943 <runtime::__type_info_of+0x63>
  4059f2:	48 c7 c0 10 93 40 00 	mov    $0x409310,%rax
  4059f9:	48 8b 00             	mov    (%rax),%rax
  4059fc:	48 8b 00             	mov    (%rax),%rax
  4059ff:	c3                   	ret

0000000000405a00 <runtime::default_logger_proc>:
  405a00:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405a05:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  405a0a:	66 44 89 c0          	mov    %r8w,%ax
  405a0e:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  405a13:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  405a18:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405a1d:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  405a22:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  405a27:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  405a2c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  405a31:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  405a36:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  405a3b:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  405a40:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  405a45:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  405a4a:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  405a4f:	c3                   	ret

0000000000405a50 <runtime::default_context>:
  405a50:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  405a57:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  405a5c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  405a61:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405a66:	31 f6                	xor    %esi,%esi
  405a68:	ba 70 00 00 00       	mov    $0x70,%edx
  405a6d:	e8 ce b5 ff ff       	call   401040 <memset@plt>
  405a72:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405a77:	e8 24 00 00 00       	call   405aa0 <runtime::[core.odin]::__init_context>
  405a7c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  405a81:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  405a86:	ba 70 00 00 00       	mov    $0x70,%edx
  405a8b:	e8 d0 b5 ff ff       	call   401060 <memcpy@plt>
  405a90:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405a95:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  405a9c:	c3                   	ret
  405a9d:	0f 1f 00             	nopl   (%rax)

0000000000405aa0 <runtime::[core.odin]::__init_context>:
  405aa0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  405aa5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405aaa:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405aaf:	48 83 f8 00          	cmp    $0x0,%rax
  405ab3:	0f 94 c0             	sete   %al
  405ab6:	24 01                	and    $0x1,%al
  405ab8:	3c 00                	cmp    $0x0,%al
  405aba:	74 01                	je     405abd <runtime::[core.odin]::__init_context+0x1d>
  405abc:	c3                   	ret
  405abd:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405ac2:	48 c7 c1 b0 11 40 00 	mov    $0x4011b0,%rcx
  405ac9:	48 89 08             	mov    %rcx,(%rax)
  405acc:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405ad1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405ad8:	00 
  405ad9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405ade:	48 c7 c1 20 32 40 00 	mov    $0x403220,%rcx
  405ae5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  405ae9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405aee:	48 c7 c2 c8 ff ff ff 	mov    $0xffffffffffffffc8,%rdx
  405af5:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  405afc:	00 00 
  405afe:	48 01 d1             	add    %rdx,%rcx
  405b01:	48 89 48 18          	mov    %rcx,0x18(%rax)
  405b05:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405b0a:	48 c7 c1 50 5b 40 00 	mov    $0x405b50,%rcx
  405b11:	48 89 48 20          	mov    %rcx,0x20(%rax)
  405b15:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405b1a:	48 c7 c1 00 5a 40 00 	mov    $0x405a00,%rcx
  405b21:	48 89 48 28          	mov    %rcx,0x28(%rax)
  405b25:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405b2a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  405b31:	00 
  405b32:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405b37:	48 c7 c1 c0 2c 40 00 	mov    $0x402cc0,%rcx
  405b3e:	48 89 48 48          	mov    %rcx,0x48(%rax)
  405b42:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405b47:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  405b4e:	00 
  405b4f:	c3                   	ret

0000000000405b50 <runtime::default_assertion_failure_proc>:
  405b50:	48 83 ec 48          	sub    $0x48,%rsp
  405b54:	4c 89 04 24          	mov    %r8,(%rsp)
  405b58:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405b5d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405b62:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405b67:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  405b6c:	4c 8b 04 24          	mov    (%rsp),%r8
  405b70:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405b75:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405b7a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405b7f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405b84:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405b89:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  405b8e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405b93:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  405b98:	e8 03 00 00 00       	call   405ba0 <runtime::default_assertion_contextless_failure_proc>
  405b9d:	0f 1f 00             	nopl   (%rax)

0000000000405ba0 <runtime::default_assertion_contextless_failure_proc>:
  405ba0:	48 83 ec 48          	sub    $0x48,%rsp
  405ba4:	4c 89 04 24          	mov    %r8,(%rsp)
  405ba8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405bad:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405bb2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405bb7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  405bbc:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405bc1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  405bc6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405bcb:	48 8b 3c 24          	mov    (%rsp),%rdi
  405bcf:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405bd4:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405bd9:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  405bde:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405be3:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  405be8:	e8 c3 e4 ff ff       	call   4040b0 <runtime::print_caller_location>
  405bed:	bf 03 93 40 00       	mov    $0x409303,%edi
  405bf2:	be 01 00 00 00       	mov    $0x1,%esi
  405bf7:	e8 14 e0 ff ff       	call   403c10 <runtime::print_string>
  405bfc:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405c01:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405c06:	e8 05 e0 ff ff       	call   403c10 <runtime::print_string>
  405c0b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405c10:	48 83 f8 00          	cmp    $0x0,%rax
  405c14:	0f 9f c0             	setg   %al
  405c17:	24 01                	and    $0x1,%al
  405c19:	3c 00                	cmp    $0x0,%al
  405c1b:	74 1e                	je     405c3b <runtime::default_assertion_contextless_failure_proc+0x9b>
  405c1d:	bf 05 93 40 00       	mov    $0x409305,%edi
  405c22:	be 02 00 00 00       	mov    $0x2,%esi
  405c27:	e8 e4 df ff ff       	call   403c10 <runtime::print_string>
  405c2c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  405c31:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  405c36:	e8 d5 df ff ff       	call   403c10 <runtime::print_string>
  405c3b:	bf 0a 00 00 00       	mov    $0xa,%edi
  405c40:	e8 3b e0 ff ff       	call   403c80 <runtime::print_byte>
  405c45:	0f 0b                	ud2
  405c47:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405c4e:	00 00 

0000000000405c50 <__$startup_runtime>:
  405c50:	50                   	push   %rax
  405c51:	48 89 3c 24          	mov    %rdi,(%rsp)
  405c55:	eb 00                	jmp    405c57 <__$startup_runtime+0x7>
  405c57:	48 8b 3c 24          	mov    (%rsp),%rdi
  405c5b:	e8 80 00 00 00       	call   405ce0 <__$startup$os::args>
  405c60:	58                   	pop    %rax
  405c61:	c3                   	ret
  405c62:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  405c69:	1f 84 00 00 00 00 00 

0000000000405c70 <__$equal-17266089877833985010>:
  405c70:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  405c75:	48 89 74 24 f8       	mov    %rsi,-0x8(%rsp)
  405c7a:	eb 00                	jmp    405c7c <__$equal-17266089877833985010+0xc>
  405c7c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405c81:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405c86:	48 39 c8             	cmp    %rcx,%rax
  405c89:	75 03                	jne    405c8e <__$equal-17266089877833985010+0x1e>
  405c8b:	b0 01                	mov    $0x1,%al
  405c8d:	c3                   	ret
  405c8e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405c93:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405c98:	f3 0f 10 01          	movss  (%rcx),%xmm0
  405c9c:	0f 2e 00             	ucomiss (%rax),%xmm0
  405c9f:	75 33                	jne    405cd4 <__$equal-17266089877833985010+0x64>
  405ca1:	7a 31                	jp     405cd4 <__$equal-17266089877833985010+0x64>
  405ca3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405ca8:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405cad:	f3 0f 10 41 04       	movss  0x4(%rcx),%xmm0
  405cb2:	0f 2e 40 04          	ucomiss 0x4(%rax),%xmm0
  405cb6:	75 1c                	jne    405cd4 <__$equal-17266089877833985010+0x64>
  405cb8:	7a 1a                	jp     405cd4 <__$equal-17266089877833985010+0x64>
  405cba:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405cbf:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405cc4:	f3 0f 10 41 08       	movss  0x8(%rcx),%xmm0
  405cc9:	0f 2e 40 08          	ucomiss 0x8(%rax),%xmm0
  405ccd:	75 05                	jne    405cd4 <__$equal-17266089877833985010+0x64>
  405ccf:	7a 03                	jp     405cd4 <__$equal-17266089877833985010+0x64>
  405cd1:	b0 01                	mov    $0x1,%al
  405cd3:	c3                   	ret
  405cd4:	31 c0                	xor    %eax,%eax
  405cd6:	c3                   	ret
  405cd7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405cde:	00 00 

0000000000405ce0 <__$startup$os::args>:
  405ce0:	50                   	push   %rax
  405ce1:	eb 00                	jmp    405ce3 <__$startup$os::args+0x3>
  405ce3:	e8 d8 23 00 00       	call   4080c0 <os::[os_linux.odin]::_alloc_command_line_arguments>
  405ce8:	48 89 15 89 63 00 00 	mov    %rdx,0x6389(%rip)        # 40c078 <os::args+0x8>
  405cef:	48 89 05 7a 63 00 00 	mov    %rax,0x637a(%rip)        # 40c070 <os::args>
  405cf6:	58                   	pop    %rax
  405cf7:	c3                   	ret
  405cf8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  405cff:	00 

0000000000405d00 <__$cleanup_runtime>:
  405d00:	50                   	push   %rax
  405d01:	eb 00                	jmp    405d03 <__$cleanup_runtime+0x3>
  405d03:	e8 98 26 00 00       	call   4083a0 <os::[os_linux.odin]::_delete_command_line_arguments>
  405d08:	e8 a3 d4 ff ff       	call   4031b0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  405d0d:	58                   	pop    %rax
  405d0e:	c3                   	ret
  405d0f:	90                   	nop

0000000000405d10 <journey::main>:
  405d10:	55                   	push   %rbp
  405d11:	48 89 e5             	mov    %rsp,%rbp
  405d14:	41 57                	push   %r15
  405d16:	41 56                	push   %r14
  405d18:	41 54                	push   %r12
  405d1a:	53                   	push   %rbx
  405d1b:	48 83 e4 c0          	and    $0xffffffffffffffc0,%rsp
  405d1f:	48 81 ec 40 02 00 00 	sub    $0x240,%rsp
  405d26:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  405d2b:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  405d30:	c7 84 24 0c 02 00 00 	movl   $0x40000000,0x20c(%rsp)
  405d37:	00 00 00 40 
  405d3b:	c7 84 24 08 02 00 00 	movl   $0x40e00000,0x208(%rsp)
  405d42:	00 00 e0 40 
  405d46:	c7 84 24 04 02 00 00 	movl   $0x40a00000,0x204(%rsp)
  405d4d:	00 00 a0 40 
  405d51:	48 8d 84 24 08 02 00 	lea    0x208(%rsp),%rax
  405d58:	00 
  405d59:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  405d5e:	c7 84 24 00 02 00 00 	movl   $0x42c80000,0x200(%rsp)
  405d65:	00 00 c8 42 
  405d69:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  405d70:	00 
  405d71:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405d76:	e8 1b 02 00 00       	call   405f96 <journey::init_world:proc(world:^journey::World(THREAD_COUNT:$$4),unique_data_capacity:$$28)>
  405d7b:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  405d80:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  405d85:	e8 f6 02 00 00       	call   406080 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)>
  405d8a:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  405d8f:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  405d94:	e8 a7 03 00 00       	call   406140 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$2,start:$$0,end:$$19)>
  405d99:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  405d9e:	48 8d bc 24 04 02 00 	lea    0x204(%rsp),%rdi
  405da5:	00 
  405da6:	e8 05 04 00 00       	call   4061b0 <journey::build_sym_predicate:proc(data_typeid:$journey::main::Position::$1,field_name:$$"x",cmp_op:$$2,val:^f32,comb_op:$$2)->(:journey::SystemPredicate)>
  405dab:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  405db0:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  405db5:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  405dbc:	00 
  405dbd:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  405dc4:	00 
  405dc5:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  405dcc:	00 
  405dcd:	66 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%cx
  405dd4:	00 
  405dd5:	66 8b 94 24 ba 00 00 	mov    0xba(%rsp),%dx
  405ddc:	00 
  405ddd:	66 44 8b 84 24 bc 00 	mov    0xbc(%rsp),%r8w
  405de4:	00 00 
  405de6:	44 8a 8c 24 be 00 00 	mov    0xbe(%rsp),%r9b
  405ded:	00 
  405dee:	44 8a 94 24 bf 00 00 	mov    0xbf(%rsp),%r10b
  405df5:	00 
  405df6:	44 88 94 24 af 00 00 	mov    %r10b,0xaf(%rsp)
  405dfd:	00 
  405dfe:	44 88 8c 24 ae 00 00 	mov    %r9b,0xae(%rsp)
  405e05:	00 
  405e06:	66 44 89 84 24 ac 00 	mov    %r8w,0xac(%rsp)
  405e0d:	00 00 
  405e0f:	66 89 94 24 aa 00 00 	mov    %dx,0xaa(%rsp)
  405e16:	00 
  405e17:	66 89 8c 24 a8 00 00 	mov    %cx,0xa8(%rsp)
  405e1e:	00 
  405e1f:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  405e26:	00 
  405e27:	e8 34 04 00 00       	call   406260 <journey::build_sym_predicate:proc(data_typeid:$journey::main::Position::$1,field_name:$$"y",cmp_op:$$2,val:^f32,comb_op:$$0)->(:journey::SystemPredicate)>
  405e2c:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  405e31:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  405e38:	00 
  405e39:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  405e40:	00 
  405e41:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  405e48:	00 
  405e49:	66 8b 8c 24 98 00 00 	mov    0x98(%rsp),%cx
  405e50:	00 
  405e51:	66 8b 94 24 9a 00 00 	mov    0x9a(%rsp),%dx
  405e58:	00 
  405e59:	66 8b b4 24 9c 00 00 	mov    0x9c(%rsp),%si
  405e60:	00 
  405e61:	40 8a bc 24 9e 00 00 	mov    0x9e(%rsp),%dil
  405e68:	00 
  405e69:	44 8a 84 24 9f 00 00 	mov    0x9f(%rsp),%r8b
  405e70:	00 
  405e71:	44 88 84 24 8f 00 00 	mov    %r8b,0x8f(%rsp)
  405e78:	00 
  405e79:	40 88 bc 24 8e 00 00 	mov    %dil,0x8e(%rsp)
  405e80:	00 
  405e81:	66 89 b4 24 8c 00 00 	mov    %si,0x8c(%rsp)
  405e88:	00 
  405e89:	66 89 94 24 8a 00 00 	mov    %dx,0x8a(%rsp)
  405e90:	00 
  405e91:	66 89 8c 24 88 00 00 	mov    %cx,0x88(%rsp)
  405e98:	00 
  405e99:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  405ea0:	00 
  405ea1:	4c 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%r10
  405ea8:	00 
  405ea9:	66 44 8b 9c 24 a8 00 	mov    0xa8(%rsp),%r11w
  405eb0:	00 00 
  405eb2:	66 8b 9c 24 aa 00 00 	mov    0xaa(%rsp),%bx
  405eb9:	00 
  405eba:	66 44 8b b4 24 ac 00 	mov    0xac(%rsp),%r14w
  405ec1:	00 00 
  405ec3:	44 8a bc 24 ae 00 00 	mov    0xae(%rsp),%r15b
  405eca:	00 
  405ecb:	44 8a a4 24 af 00 00 	mov    0xaf(%rsp),%r12b
  405ed2:	00 
  405ed3:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  405eda:	00 
  405edb:	66 8b 8c 24 88 00 00 	mov    0x88(%rsp),%cx
  405ee2:	00 
  405ee3:	66 8b 94 24 8a 00 00 	mov    0x8a(%rsp),%dx
  405eea:	00 
  405eeb:	66 8b b4 24 8c 00 00 	mov    0x8c(%rsp),%si
  405ef2:	00 
  405ef3:	40 8a bc 24 8e 00 00 	mov    0x8e(%rsp),%dil
  405efa:	00 
  405efb:	44 8a 84 24 8f 00 00 	mov    0x8f(%rsp),%r8b
  405f02:	00 
  405f03:	0f 57 c0             	xorps  %xmm0,%xmm0
  405f06:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  405f0b:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  405f10:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  405f15:	44 88 64 24 5f       	mov    %r12b,0x5f(%rsp)
  405f1a:	44 88 7c 24 5e       	mov    %r15b,0x5e(%rsp)
  405f1f:	66 44 89 74 24 5c    	mov    %r14w,0x5c(%rsp)
  405f25:	66 89 5c 24 5a       	mov    %bx,0x5a(%rsp)
  405f2a:	66 44 89 5c 24 58    	mov    %r11w,0x58(%rsp)
  405f30:	4c 89 54 24 50       	mov    %r10,0x50(%rsp)
  405f35:	44 88 44 24 6f       	mov    %r8b,0x6f(%rsp)
  405f3a:	40 88 7c 24 6e       	mov    %dil,0x6e(%rsp)
  405f3f:	66 89 74 24 6c       	mov    %si,0x6c(%rsp)
  405f44:	66 89 54 24 6a       	mov    %dx,0x6a(%rsp)
  405f49:	66 89 4c 24 68       	mov    %cx,0x68(%rsp)
  405f4e:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  405f53:	48 8d 44 24 50       	lea    0x50(%rsp),%rax
  405f58:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  405f5d:	48 c7 44 24 78 02 00 	movq   $0x2,0x78(%rsp)
  405f64:	00 00 
  405f66:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  405f6b:	4c 8b 44 24 78       	mov    0x78(%rsp),%r8
  405f70:	48 c7 c2 90 63 40 00 	mov    $0x406390,%rdx
  405f77:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  405f7e:	00 
  405f7f:	be 01 00 00 00       	mov    $0x1,%esi
  405f84:	e8 7a 03 00 00       	call   406303 <journey::run_0:proc(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>
  405f89:	48 8d 65 e0          	lea    -0x20(%rbp),%rsp
  405f8d:	5b                   	pop    %rbx
  405f8e:	41 5c                	pop    %r12
  405f90:	41 5e                	pop    %r14
  405f92:	41 5f                	pop    %r15
  405f94:	5d                   	pop    %rbp
  405f95:	c3                   	ret

0000000000405f96 <journey::init_world:proc(world:^journey::World(THREAD_COUNT:$$4),unique_data_capacity:$$28)>:
  405f96:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  405f9b:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405fa0:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405fa5:	31 c0                	xor    %eax,%eax
  405fa7:	41 89 c1             	mov    %eax,%r9d
  405faa:	b8 09 00 00 00       	mov    $0x9,%eax
  405faf:	be 00 e0 01 00       	mov    $0x1e000,%esi
  405fb4:	ba 03 00 00 00       	mov    $0x3,%edx
  405fb9:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  405fbf:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  405fc6:	4c 89 cf             	mov    %r9,%rdi
  405fc9:	0f 05                	syscall
  405fcb:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405fd0:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405fd5:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405fda:	48 89 08             	mov    %rcx,(%rax)
  405fdd:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405fe2:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405fe7:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  405fee:	48 89 48 08          	mov    %rcx,0x8(%rax)
  405ff2:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405ff7:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405ffc:	48 81 c1 00 20 00 00 	add    $0x2000,%rcx
  406003:	48 89 48 10          	mov    %rcx,0x10(%rax)
  406007:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40600c:	48 c7 40 18 1c 00 00 	movq   $0x1c,0x18(%rax)
  406013:	00 
  406014:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406019:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40601e:	b8 ba 00 00 00       	mov    $0xba,%eax
  406023:	0f 05                	syscall
  406025:	48 89 c1             	mov    %rax,%rcx
  406028:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40602d:	48 89 48 20          	mov    %rcx,0x20(%rax)
  406031:	31 c0                	xor    %eax,%eax
  406033:	41 89 c1             	mov    %eax,%r9d
  406036:	b8 09 00 00 00       	mov    $0x9,%eax
  40603b:	be 00 10 02 00       	mov    $0x21000,%esi
  406040:	ba 03 00 00 00       	mov    $0x3,%edx
  406045:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  40604b:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  406052:	4c 89 cf             	mov    %r9,%rdi
  406055:	0f 05                	syscall
  406057:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40605c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406061:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406066:	48 89 48 28          	mov    %rcx,0x28(%rax)
  40606a:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40606f:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406074:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  40607b:	48 89 48 30          	mov    %rcx,0x30(%rax)
  40607f:	c3                   	ret

0000000000406080 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)>:
  406080:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  406085:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40608a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40608f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406094:	48 8b 00             	mov    (%rax),%rax
  406097:	c7 40 1c 11 33 00 00 	movl   $0x3311,0x1c(%rax)
  40609e:	c7 40 18 3f 01 00 00 	movl   $0x13f,0x18(%rax)
  4060a5:	c7 40 14 15 00 00 00 	movl   $0x15,0x14(%rax)
  4060ac:	c7 40 10 0c 00 00 00 	movl   $0xc,0x10(%rax)
  4060b3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4060b8:	48 8b 40 08          	mov    0x8(%rax),%rax
  4060bc:	c6 40 03 ef          	movb   $0xef,0x3(%rax)
  4060c0:	c6 40 02 0f          	movb   $0xf,0x2(%rax)
  4060c4:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4060c9:	48 8b 40 10          	mov    0x10(%rax),%rax
  4060cd:	48 05 00 10 00 00    	add    $0x1000,%rax
  4060d3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4060d8:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4060df:	00 00 
  4060e1:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  4060e8:	00 00 
  4060ea:	48 83 7c 24 e8 04    	cmpq   $0x4,-0x18(%rsp)
  4060f0:	73 3a                	jae    40612c <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)+0xac>
  4060f2:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4060f7:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4060fc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406101:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406106:	48 c7 04 c8 ff ff ff 	movq   $0xffffffffffffffff,(%rax,%rcx,8)
  40610d:	ff 
  40610e:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406113:	48 83 c0 01          	add    $0x1,%rax
  406117:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40611c:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  406121:	48 83 c0 01          	add    $0x1,%rax
  406125:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40612a:	eb be                	jmp    4060ea <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)+0x6a>
  40612c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406131:	48 b9 ff ff ff ff ff 	movabs $0x3ffffffffff,%rcx
  406138:	03 00 00 
  40613b:	48 89 48 20          	mov    %rcx,0x20(%rax)
  40613f:	c3                   	ret

0000000000406140 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$2,start:$$0,end:$$19)>:
  406140:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  406145:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40614a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40614f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406154:	48 8b 00             	mov    (%rax),%rax
  406157:	c7 40 2c 11 33 00 00 	movl   $0x3311,0x2c(%rax)
  40615e:	c7 40 28 2a 00 00 00 	movl   $0x2a,0x28(%rax)
  406165:	c7 40 24 00 00 00 00 	movl   $0x0,0x24(%rax)
  40616c:	c7 40 20 0c 00 00 00 	movl   $0xc,0x20(%rax)
  406173:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406178:	48 8b 40 08          	mov    0x8(%rax),%rax
  40617c:	c6 40 05 fe          	movb   $0xfe,0x5(%rax)
  406180:	c6 40 04 00          	movb   $0x0,0x4(%rax)
  406184:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406189:	48 8b 40 10          	mov    0x10(%rax),%rax
  40618d:	48 05 00 20 00 00    	add    $0x2000,%rax
  406193:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406198:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40619d:	48 b9 ff ff ff ff ff 	movabs $0x3ffffffffff,%rcx
  4061a4:	03 00 00 
  4061a7:	48 89 08             	mov    %rcx,(%rax)
  4061aa:	c3                   	ret
  4061ab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004061b0 <journey::build_sym_predicate:proc(data_typeid:$journey::main::Position::$1,field_name:$$"x",cmp_op:$$2,val:^f32,comb_op:$$2)->(:journey::SystemPredicate)>:
  4061b0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  4061b5:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4061ba:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4061bf:	66 c7 44 24 f6 00 00 	movw   $0x0,-0xa(%rsp)
  4061c6:	66 c7 44 24 f6 00 00 	movw   $0x0,-0xa(%rsp)
  4061cd:	0f 57 c0             	xorps  %xmm0,%xmm0
  4061d0:	0f 29 44 24 d8       	movaps %xmm0,-0x28(%rsp)
  4061d5:	c6 44 24 e7 02       	movb   $0x2,-0x19(%rsp)
  4061da:	c6 44 24 e6 02       	movb   $0x2,-0x1a(%rsp)
  4061df:	66 c7 44 24 e4 00 00 	movw   $0x0,-0x1c(%rsp)
  4061e6:	66 c7 44 24 e2 40 00 	movw   $0x40,-0x1e(%rsp)
  4061ed:	66 c7 44 24 e0 00 00 	movw   $0x0,-0x20(%rsp)
  4061f4:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4061fb:	00 00 
  4061fd:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  406202:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  406207:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  40620c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406211:	66 8b 4c 24 e0       	mov    -0x20(%rsp),%cx
  406216:	66 8b 54 24 e2       	mov    -0x1e(%rsp),%dx
  40621b:	66 8b 74 24 e4       	mov    -0x1c(%rsp),%si
  406220:	40 8a 7c 24 e6       	mov    -0x1a(%rsp),%dil
  406225:	44 8a 44 24 e7       	mov    -0x19(%rsp),%r8b
  40622a:	44 88 44 24 d7       	mov    %r8b,-0x29(%rsp)
  40622f:	40 88 7c 24 d6       	mov    %dil,-0x2a(%rsp)
  406234:	66 89 74 24 d4       	mov    %si,-0x2c(%rsp)
  406239:	66 89 54 24 d2       	mov    %dx,-0x2e(%rsp)
  40623e:	66 89 4c 24 d0       	mov    %cx,-0x30(%rsp)
  406243:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  406248:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40624d:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  406252:	c3                   	ret
  406253:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40625a:	84 00 00 00 00 00 

0000000000406260 <journey::build_sym_predicate:proc(data_typeid:$journey::main::Position::$1,field_name:$$"y",cmp_op:$$2,val:^f32,comb_op:$$0)->(:journey::SystemPredicate)>:
  406260:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  406265:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40626a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40626f:	66 c7 44 24 f6 00 00 	movw   $0x0,-0xa(%rsp)
  406276:	66 c7 44 24 f6 20 00 	movw   $0x20,-0xa(%rsp)
  40627d:	0f 57 c0             	xorps  %xmm0,%xmm0
  406280:	0f 29 44 24 d8       	movaps %xmm0,-0x28(%rsp)
  406285:	c6 44 24 e7 00       	movb   $0x0,-0x19(%rsp)
  40628a:	c6 44 24 e6 02       	movb   $0x2,-0x1a(%rsp)
  40628f:	66 c7 44 24 e4 00 00 	movw   $0x0,-0x1c(%rsp)
  406296:	66 c7 44 24 e2 40 00 	movw   $0x40,-0x1e(%rsp)
  40629d:	66 c7 44 24 e0 00 00 	movw   $0x0,-0x20(%rsp)
  4062a4:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4062ab:	00 00 
  4062ad:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4062b2:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  4062b7:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  4062bc:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4062c1:	66 8b 4c 24 e0       	mov    -0x20(%rsp),%cx
  4062c6:	66 8b 54 24 e2       	mov    -0x1e(%rsp),%dx
  4062cb:	66 8b 74 24 e4       	mov    -0x1c(%rsp),%si
  4062d0:	40 8a 7c 24 e6       	mov    -0x1a(%rsp),%dil
  4062d5:	44 8a 44 24 e7       	mov    -0x19(%rsp),%r8b
  4062da:	44 88 44 24 d7       	mov    %r8b,-0x29(%rsp)
  4062df:	40 88 7c 24 d6       	mov    %dil,-0x2a(%rsp)
  4062e4:	66 89 74 24 d4       	mov    %si,-0x2c(%rsp)
  4062e9:	66 89 54 24 d2       	mov    %dx,-0x2e(%rsp)
  4062ee:	66 89 4c 24 d0       	mov    %cx,-0x30(%rsp)
  4062f3:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4062f8:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4062fd:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  406302:	c3                   	ret

0000000000406303 <journey::run_0:proc(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>:
  406303:	48 83 ec 78          	sub    $0x78,%rsp
  406307:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40630c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406311:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406316:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40631b:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  406320:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406325:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40632a:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40632f:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  406334:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  406339:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  40633e:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  406343:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  406348:	48 89 54 24 58       	mov    %rdx,0x58(%rsp)
  40634d:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  406352:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  406357:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  40635b:	48 81 c1 40 10 00 00 	add    $0x1040,%rcx
  406362:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  406367:	48 c7 44 24 48 c0 0f 	movq   $0xfc0,0x48(%rsp)
  40636e:	00 00 
  406370:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  406375:	31 c9                	xor    %ecx,%ecx
  406377:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  40637b:	89 4c 24 30          	mov    %ecx,0x30(%rsp)
  40637f:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406384:	ff d0                	call   *%rax
  406386:	48 83 c4 78          	add    $0x78,%rsp
  40638a:	c3                   	ret
  40638b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000406390 <journey::main.readjust_npc_position-0>:
  406390:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  406395:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40639a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40639f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4063a4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4063a9:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4063ae:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4063b3:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4063ba:	00 00 
  4063bc:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  4063c3:	00 00 
  4063c5:	48 83 7c 24 d8 01    	cmpq   $0x1,-0x28(%rsp)
  4063cb:	7d 4a                	jge    406417 <journey::main.readjust_npc_position-0+0x87>
  4063cd:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4063d2:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4063d7:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4063dc:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4063e1:	48 8d 0c 49          	lea    (%rcx,%rcx,2),%rcx
  4063e5:	48 c1 e1 05          	shl    $0x5,%rcx
  4063e9:	0f 28 05 f0 2f 00 00 	movaps 0x2ff0(%rip),%xmm0        # 4093e0 <runtime::type_table+0xd0>
  4063f0:	0f 29 44 08 10       	movaps %xmm0,0x10(%rax,%rcx,1)
  4063f5:	0f 29 04 08          	movaps %xmm0,(%rax,%rcx,1)
  4063f9:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4063fe:	48 83 c0 01          	add    $0x1,%rax
  406402:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  406407:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40640c:	48 83 c0 01          	add    $0x1,%rax
  406410:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  406415:	eb ae                	jmp    4063c5 <journey::main.readjust_npc_position-0+0x35>
  406417:	c3                   	ret
  406418:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40641f:	00 

0000000000406420 <runtime::[internal.odin]::byte_slice>:
  406420:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  406425:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40642a:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  40642f:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  406434:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406439:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  40643e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406443:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406448:	31 c0                	xor    %eax,%eax
  40644a:	48 85 d2             	test   %rdx,%rdx
  40644d:	48 0f 49 c2          	cmovns %rdx,%rax
  406451:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406456:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40645b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406460:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406465:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40646a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40646f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406474:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  406479:	c3                   	ret
  40647a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000406480 <runtime::is_power_of_two_int>:
  406480:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  406485:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40648a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40648f:	48 83 f8 00          	cmp    $0x0,%rax
  406493:	0f 9e c0             	setle  %al
  406496:	24 01                	and    $0x1,%al
  406498:	3c 00                	cmp    $0x0,%al
  40649a:	74 03                	je     40649f <runtime::is_power_of_two_int+0x1f>
  40649c:	31 c0                	xor    %eax,%eax
  40649e:	c3                   	ret
  40649f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4064a4:	48 89 c1             	mov    %rax,%rcx
  4064a7:	48 83 e9 01          	sub    $0x1,%rcx
  4064ab:	48 21 c8             	and    %rcx,%rax
  4064ae:	48 83 f8 00          	cmp    $0x0,%rax
  4064b2:	0f 94 c0             	sete   %al
  4064b5:	24 01                	and    $0x1,%al
  4064b7:	c3                   	ret
  4064b8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4064bf:	00 

00000000004064c0 <runtime::mem_copy_non_overlapping>:
  4064c0:	48 83 ec 38          	sub    $0x38,%rsp
  4064c4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4064c9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4064ce:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4064d3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4064d8:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4064dd:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4064e2:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4064e7:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4064ec:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4064f1:	48 83 f8 00          	cmp    $0x0,%rax
  4064f5:	0f 95 c0             	setne  %al
  4064f8:	24 01                	and    $0x1,%al
  4064fa:	3c 00                	cmp    $0x0,%al
  4064fc:	74 3c                	je     40653a <runtime::mem_copy_non_overlapping+0x7a>
  4064fe:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406503:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406508:	48 39 c8             	cmp    %rcx,%rax
  40650b:	0f 95 c0             	setne  %al
  40650e:	24 01                	and    $0x1,%al
  406510:	3c 00                	cmp    $0x0,%al
  406512:	74 26                	je     40653a <runtime::mem_copy_non_overlapping+0x7a>
  406514:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  406519:	48 83 f8 00          	cmp    $0x0,%rax
  40651d:	0f 9f c0             	setg   %al
  406520:	24 01                	and    $0x1,%al
  406522:	3c 00                	cmp    $0x0,%al
  406524:	74 14                	je     40653a <runtime::mem_copy_non_overlapping+0x7a>
  406526:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40652b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  406530:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406535:	e8 26 ab ff ff       	call   401060 <memcpy@plt>
  40653a:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40653f:	48 83 c4 38          	add    $0x38,%rsp
  406543:	c3                   	ret
  406544:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40654b:	00 00 00 00 00 

0000000000406550 <runtime::mem_alloc_bytes>:
  406550:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  406557:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  40655c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  406561:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  406566:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40656b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  406570:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  406575:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40657c:	00 
  40657d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  406582:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406587:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40658c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  406591:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  406596:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40659d:	00 
  40659e:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  4065a5:	00 
  4065a6:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  4065ad:	00 
  4065ae:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4065b5:	00 
  4065b6:	e8 c5 fe ff ff       	call   406480 <runtime::is_power_of_two_int>
  4065bb:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4065c0:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  4065c5:	0f b6 f8             	movzbl %al,%edi
  4065c8:	be 14 94 40 00       	mov    $0x409414,%esi
  4065cd:	ba 20 00 00 00       	mov    $0x20,%edx
  4065d2:	e8 09 c6 ff ff       	call   402be0 <runtime::assert>
  4065d7:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4065dc:	48 83 f8 00          	cmp    $0x0,%rax
  4065e0:	0f 94 c0             	sete   %al
  4065e3:	24 01                	and    $0x1,%al
  4065e5:	3c 00                	cmp    $0x0,%al
  4065e7:	75 12                	jne    4065fb <runtime::mem_alloc_bytes+0xab>
  4065e9:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  4065f0:	00 00 
  4065f2:	0f 94 c0             	sete   %al
  4065f5:	24 01                	and    $0x1,%al
  4065f7:	3c 00                	cmp    $0x0,%al
  4065f9:	74 1e                	je     406619 <runtime::mem_alloc_bytes+0xc9>
  4065fb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406600:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  406607:	00 
  406608:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40660f:	31 c0                	xor    %eax,%eax
  406611:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  406618:	c3                   	ret
  406619:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40661e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406623:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  406628:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  40662d:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  406634:	00 
  406635:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  40663c:	00 
  40663d:	0f 57 c0             	xorps  %xmm0,%xmm0
  406640:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  406645:	48 89 e6             	mov    %rsp,%rsi
  406648:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  40664c:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  406651:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  406655:	4c 89 06             	mov    %r8,(%rsi)
  406658:	31 f6                	xor    %esi,%esi
  40665a:	41 89 f1             	mov    %esi,%r9d
  40665d:	4d 89 c8             	mov    %r9,%r8
  406660:	ff d0                	call   *%rax
  406662:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  406667:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40666c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  406671:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  406675:	48 89 11             	mov    %rdx,(%rcx)
  406678:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  40667f:	c3                   	ret

0000000000406680 <runtime::mem_alloc>:
  406680:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  406687:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  40668c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  406691:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  406696:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40669b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4066a0:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4066a5:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4066ac:	00 
  4066ad:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4066b2:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4066b7:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4066bc:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4066c1:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4066c6:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4066cd:	00 
  4066ce:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  4066d5:	00 
  4066d6:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  4066dd:	00 
  4066de:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4066e5:	00 
  4066e6:	e8 95 fd ff ff       	call   406480 <runtime::is_power_of_two_int>
  4066eb:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4066f0:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  4066f5:	0f b6 f8             	movzbl %al,%edi
  4066f8:	be 14 94 40 00       	mov    $0x409414,%esi
  4066fd:	ba 20 00 00 00       	mov    $0x20,%edx
  406702:	e8 d9 c4 ff ff       	call   402be0 <runtime::assert>
  406707:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40670c:	48 83 f8 00          	cmp    $0x0,%rax
  406710:	0f 94 c0             	sete   %al
  406713:	24 01                	and    $0x1,%al
  406715:	3c 00                	cmp    $0x0,%al
  406717:	75 12                	jne    40672b <runtime::mem_alloc+0xab>
  406719:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  406720:	00 00 
  406722:	0f 94 c0             	sete   %al
  406725:	24 01                	and    $0x1,%al
  406727:	3c 00                	cmp    $0x0,%al
  406729:	74 1e                	je     406749 <runtime::mem_alloc+0xc9>
  40672b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406730:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  406737:	00 
  406738:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40673f:	31 c0                	xor    %eax,%eax
  406741:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  406748:	c3                   	ret
  406749:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40674e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406753:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  406758:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  40675d:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  406764:	00 
  406765:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  40676c:	00 
  40676d:	0f 57 c0             	xorps  %xmm0,%xmm0
  406770:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  406775:	48 89 e6             	mov    %rsp,%rsi
  406778:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  40677c:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  406781:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  406785:	4c 89 06             	mov    %r8,(%rsi)
  406788:	31 f6                	xor    %esi,%esi
  40678a:	41 89 f1             	mov    %esi,%r9d
  40678d:	4d 89 c8             	mov    %r9,%r8
  406790:	ff d0                	call   *%rax
  406792:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  406797:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40679c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4067a1:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4067a5:	48 89 11             	mov    %rdx,(%rcx)
  4067a8:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  4067af:	c3                   	ret

00000000004067b0 <runtime::mem_free>:
  4067b0:	53                   	push   %rbx
  4067b1:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  4067b8:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  4067bd:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4067c2:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4067c7:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4067cc:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4067d1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4067d6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4067db:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4067e0:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  4067e7:	00 
  4067e8:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4067ed:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4067f2:	48 83 f8 00          	cmp    $0x0,%rax
  4067f6:	0f 94 c0             	sete   %al
  4067f9:	24 01                	and    $0x1,%al
  4067fb:	3c 00                	cmp    $0x0,%al
  4067fd:	75 0f                	jne    40680e <runtime::mem_free+0x5e>
  4067ff:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  406805:	0f 94 c0             	sete   %al
  406808:	24 01                	and    $0x1,%al
  40680a:	3c 00                	cmp    $0x0,%al
  40680c:	74 0b                	je     406819 <runtime::mem_free+0x69>
  40680e:	31 c0                	xor    %eax,%eax
  406810:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  406817:	5b                   	pop    %rbx
  406818:	c3                   	ret
  406819:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  40681e:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  406823:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  406828:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40682d:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  406832:	0f 57 c0             	xorps  %xmm0,%xmm0
  406835:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  40683a:	be 01 00 00 00       	mov    $0x1,%esi
  40683f:	31 c9                	xor    %ecx,%ecx
  406841:	41 89 c9             	mov    %ecx,%r9d
  406844:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  406849:	4c 89 ca             	mov    %r9,%rdx
  40684c:	4c 89 c9             	mov    %r9,%rcx
  40684f:	48 89 1c 24          	mov    %rbx,(%rsp)
  406853:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  406858:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  40685d:	ff d0                	call   *%rax
  40685f:	88 44 24 47          	mov    %al,0x47(%rsp)
  406863:	8a 44 24 47          	mov    0x47(%rsp),%al
  406867:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40686e:	5b                   	pop    %rbx
  40686f:	c3                   	ret

0000000000406870 <runtime::mem_free_with_size>:
  406870:	53                   	push   %rbx
  406871:	48 81 ec a0 00 00 00 	sub    $0xa0,%rsp
  406878:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  40687d:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  406882:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  406887:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40688c:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  406891:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  406896:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40689b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4068a0:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  4068a5:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4068aa:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  4068b1:	00 
  4068b2:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  4068b9:	00 
  4068ba:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  4068c1:	00 
  4068c2:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  4068c9:	00 
  4068ca:	48 83 f8 00          	cmp    $0x0,%rax
  4068ce:	0f 94 c0             	sete   %al
  4068d1:	24 01                	and    $0x1,%al
  4068d3:	3c 00                	cmp    $0x0,%al
  4068d5:	75 12                	jne    4068e9 <runtime::mem_free_with_size+0x79>
  4068d7:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  4068de:	00 00 
  4068e0:	0f 94 c0             	sete   %al
  4068e3:	24 01                	and    $0x1,%al
  4068e5:	3c 00                	cmp    $0x0,%al
  4068e7:	74 0b                	je     4068f4 <runtime::mem_free_with_size+0x84>
  4068e9:	31 c0                	xor    %eax,%eax
  4068eb:	48 81 c4 a0 00 00 00 	add    $0xa0,%rsp
  4068f2:	5b                   	pop    %rbx
  4068f3:	c3                   	ret
  4068f4:	4c 8b 54 24 20       	mov    0x20(%rsp),%r10
  4068f9:	48 8b 5c 24 28       	mov    0x28(%rsp),%rbx
  4068fe:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  406903:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  406908:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40690f:	00 
  406910:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  406917:	00 
  406918:	0f 57 c0             	xorps  %xmm0,%xmm0
  40691b:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  406920:	be 01 00 00 00       	mov    $0x1,%esi
  406925:	31 c9                	xor    %ecx,%ecx
  406927:	4c 8d 5c 24 70       	lea    0x70(%rsp),%r11
  40692c:	48 89 ca             	mov    %rcx,%rdx
  40692f:	48 89 1c 24          	mov    %rbx,(%rsp)
  406933:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  406938:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  40693d:	ff d0                	call   *%rax
  40693f:	88 44 24 57          	mov    %al,0x57(%rsp)
  406943:	8a 44 24 57          	mov    0x57(%rsp),%al
  406947:	48 81 c4 a0 00 00 00 	add    $0xa0,%rsp
  40694e:	5b                   	pop    %rbx
  40694f:	c3                   	ret

0000000000406950 <runtime::conditional_mem_zero>:
  406950:	48 83 ec 30          	sub    $0x30,%rsp
  406954:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  406959:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  40695e:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  406963:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  406968:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40696d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406972:	48 83 f8 00          	cmp    $0x0,%rax
  406976:	0f 9e c0             	setle  %al
  406979:	24 01                	and    $0x1,%al
  40697b:	3c 00                	cmp    $0x0,%al
  40697d:	74 05                	je     406984 <runtime::conditional_mem_zero+0x34>
  40697f:	48 83 c4 30          	add    $0x30,%rsp
  406983:	c3                   	ret
  406984:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  406989:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40698e:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406993:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  406998:	48 c1 e9 03          	shr    $0x3,%rcx
  40699c:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4069a1:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4069a6:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4069ab:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4069b0:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4069b5:	48 89 0c 24          	mov    %rcx,(%rsp)
  4069b9:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4069be:	48 8b 14 24          	mov    (%rsp),%rdx
  4069c2:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  4069c7:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4069cc:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4069d1:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4069d6:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4069db:	48 89 f2             	mov    %rsi,%rdx
  4069de:	48 c1 e2 03          	shl    $0x3,%rdx
  4069e2:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4069e7:	48 8d 0c f1          	lea    (%rcx,%rsi,8),%rcx
  4069eb:	48 29 d0             	sub    %rdx,%rax
  4069ee:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  4069f3:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4069f8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4069fd:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406a02:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  406a07:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  406a0c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406a11:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  406a16:	48 c7 44 24 b0 ff ff 	movq   $0xffffffffffffffff,-0x50(%rsp)
  406a1d:	ff ff 
  406a1f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  406a24:	48 83 c0 01          	add    $0x1,%rax
  406a28:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  406a2d:	48 3b 44 24 b8       	cmp    -0x48(%rsp),%rax
  406a32:	7d 38                	jge    406a6c <runtime::conditional_mem_zero+0x11c>
  406a34:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  406a39:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406a3e:	48 89 ce             	mov    %rcx,%rsi
  406a41:	48 c1 e6 03          	shl    $0x3,%rsi
  406a45:	48 89 c2             	mov    %rax,%rdx
  406a48:	48 01 f2             	add    %rsi,%rdx
  406a4b:	48 89 54 24 88       	mov    %rdx,-0x78(%rsp)
  406a50:	48 83 3c c8 00       	cmpq   $0x0,(%rax,%rcx,8)
  406a55:	0f 95 c0             	setne  %al
  406a58:	24 01                	and    $0x1,%al
  406a5a:	3c 00                	cmp    $0x0,%al
  406a5c:	74 0c                	je     406a6a <runtime::conditional_mem_zero+0x11a>
  406a5e:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  406a63:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  406a6a:	eb b3                	jmp    406a1f <runtime::conditional_mem_zero+0xcf>
  406a6c:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406a71:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  406a76:	48 c7 44 24 a0 ff ff 	movq   $0xffffffffffffffff,-0x60(%rsp)
  406a7d:	ff ff 
  406a7f:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  406a84:	48 83 c0 01          	add    $0x1,%rax
  406a88:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  406a8d:	48 3b 44 24 a8       	cmp    -0x58(%rsp),%rax
  406a92:	7d 2c                	jge    406ac0 <runtime::conditional_mem_zero+0x170>
  406a94:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  406a99:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  406a9e:	48 89 c2             	mov    %rax,%rdx
  406aa1:	48 01 ca             	add    %rcx,%rdx
  406aa4:	48 89 54 24 80       	mov    %rdx,-0x80(%rsp)
  406aa9:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  406aad:	0f 95 c0             	setne  %al
  406ab0:	24 01                	and    $0x1,%al
  406ab2:	3c 00                	cmp    $0x0,%al
  406ab4:	74 08                	je     406abe <runtime::conditional_mem_zero+0x16e>
  406ab6:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  406abb:	c6 00 00             	movb   $0x0,(%rax)
  406abe:	eb bf                	jmp    406a7f <runtime::conditional_mem_zero+0x12f>
  406ac0:	48 83 c4 30          	add    $0x30,%rsp
  406ac4:	c3                   	ret
  406ac5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  406acc:	00 00 00 00 

0000000000406ad0 <runtime::memory_equal>:
  406ad0:	48 83 ec 28          	sub    $0x28,%rsp
  406ad4:	48 89 7c 24 88       	mov    %rdi,-0x78(%rsp)
  406ad9:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  406ade:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  406ae3:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  406ae8:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  406aed:	48 8b 54 24 88       	mov    -0x78(%rsp),%rdx
  406af2:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  406af7:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406afc:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406b01:	48 83 f8 00          	cmp    $0x0,%rax
  406b05:	0f 94 c1             	sete   %cl
  406b08:	80 e1 01             	and    $0x1,%cl
  406b0b:	b0 01                	mov    $0x1,%al
  406b0d:	38 c8                	cmp    %cl,%al
  406b0f:	74 1b                	je     406b2c <runtime::memory_equal+0x5c>
  406b11:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  406b16:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  406b1b:	48 39 c8             	cmp    %rcx,%rax
  406b1e:	0f 94 c1             	sete   %cl
  406b21:	80 e1 01             	and    $0x1,%cl
  406b24:	b0 01                	mov    $0x1,%al
  406b26:	38 c8                	cmp    %cl,%al
  406b28:	74 0b                	je     406b35 <runtime::memory_equal+0x65>
  406b2a:	eb 07                	jmp    406b33 <runtime::memory_equal+0x63>
  406b2c:	b0 01                	mov    $0x1,%al
  406b2e:	48 83 c4 28          	add    $0x28,%rsp
  406b32:	c3                   	ret
  406b33:	eb 07                	jmp    406b3c <runtime::memory_equal+0x6c>
  406b35:	b0 01                	mov    $0x1,%al
  406b37:	48 83 c4 28          	add    $0x28,%rsp
  406b3b:	c3                   	ret
  406b3c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  406b41:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  406b46:	48 8b 54 24 88       	mov    -0x78(%rsp),%rdx
  406b4b:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  406b50:	48 89 0c 24          	mov    %rcx,(%rsp)
  406b54:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406b59:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  406b60:	00 00 
  406b62:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  406b69:	00 00 
  406b6b:	48 83 7c 24 f8 08    	cmpq   $0x8,-0x8(%rsp)
  406b71:	0f 93 c0             	setae  %al
  406b74:	24 01                	and    $0x1,%al
  406b76:	3c 00                	cmp    $0x0,%al
  406b78:	0f 84 43 01 00 00    	je     406cc1 <runtime::memory_equal+0x1f1>
  406b7e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406b83:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  406b88:	48 c1 e8 04          	shr    $0x4,%rax
  406b8c:	48 c1 e0 04          	shl    $0x4,%rax
  406b90:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406b95:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406b9a:	48 3b 44 24 e8       	cmp    -0x18(%rsp),%rax
  406b9f:	0f 92 c0             	setb   %al
  406ba2:	24 01                	and    $0x1,%al
  406ba4:	3c 00                	cmp    $0x0,%al
  406ba6:	0f 84 9a 00 00 00    	je     406c46 <runtime::memory_equal+0x176>
  406bac:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406bb1:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406bb6:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  406bba:	0f 29 44 24 d0       	movaps %xmm0,-0x30(%rsp)
  406bbf:	48 8b 04 24          	mov    (%rsp),%rax
  406bc3:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406bc8:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  406bcc:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  406bd1:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  406bd6:	0f 28 4c 24 c0       	movaps -0x40(%rsp),%xmm1
  406bdb:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  406bdf:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  406be3:	66 0f ef c1          	pxor   %xmm1,%xmm0
  406be7:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  406bec:	0f 28 44 24 b0       	movaps -0x50(%rsp),%xmm0
  406bf1:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  406bf6:	66 0f eb c1          	por    %xmm1,%xmm0
  406bfa:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  406bff:	66 0f eb c1          	por    %xmm1,%xmm0
  406c03:	0f 28 c8             	movaps %xmm0,%xmm1
  406c06:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  406c0b:	66 0f eb c1          	por    %xmm1,%xmm0
  406c0f:	0f 28 c8             	movaps %xmm0,%xmm1
  406c12:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  406c17:	66 0f eb c1          	por    %xmm1,%xmm0
  406c1b:	66 0f 7e c0          	movd   %xmm0,%eax
  406c1f:	3c 00                	cmp    $0x0,%al
  406c21:	0f 95 c0             	setne  %al
  406c24:	24 01                	and    $0x1,%al
  406c26:	3c 00                	cmp    $0x0,%al
  406c28:	74 07                	je     406c31 <runtime::memory_equal+0x161>
  406c2a:	31 c0                	xor    %eax,%eax
  406c2c:	48 83 c4 28          	add    $0x28,%rsp
  406c30:	c3                   	ret
  406c31:	eb 00                	jmp    406c33 <runtime::memory_equal+0x163>
  406c33:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406c38:	48 83 c0 10          	add    $0x10,%rax
  406c3c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406c41:	e9 4f ff ff ff       	jmp    406b95 <runtime::memory_equal+0xc5>
  406c46:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406c4b:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  406c50:	48 c1 e8 03          	shr    $0x3,%rax
  406c54:	48 c1 e0 03          	shl    $0x3,%rax
  406c58:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406c5d:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406c62:	48 3b 44 24 e8       	cmp    -0x18(%rsp),%rax
  406c67:	0f 92 c0             	setb   %al
  406c6a:	24 01                	and    $0x1,%al
  406c6c:	3c 00                	cmp    $0x0,%al
  406c6e:	74 4f                	je     406cbf <runtime::memory_equal+0x1ef>
  406c70:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406c75:	48 03 44 24 f0       	add    -0x10(%rsp),%rax
  406c7a:	48 8b 00             	mov    (%rax),%rax
  406c7d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  406c82:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  406c87:	48 8b 0c 24          	mov    (%rsp),%rcx
  406c8b:	48 03 4c 24 f0       	add    -0x10(%rsp),%rcx
  406c90:	48 8b 09             	mov    (%rcx),%rcx
  406c93:	48 89 4c 24 a0       	mov    %rcx,-0x60(%rsp)
  406c98:	48 3b 44 24 a0       	cmp    -0x60(%rsp),%rax
  406c9d:	0f 95 c0             	setne  %al
  406ca0:	24 01                	and    $0x1,%al
  406ca2:	3c 00                	cmp    $0x0,%al
  406ca4:	74 07                	je     406cad <runtime::memory_equal+0x1dd>
  406ca6:	31 c0                	xor    %eax,%eax
  406ca8:	48 83 c4 28          	add    $0x28,%rsp
  406cac:	c3                   	ret
  406cad:	eb 00                	jmp    406caf <runtime::memory_equal+0x1df>
  406caf:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406cb4:	48 83 c0 08          	add    $0x8,%rax
  406cb8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406cbd:	eb 9e                	jmp    406c5d <runtime::memory_equal+0x18d>
  406cbf:	eb 00                	jmp    406cc1 <runtime::memory_equal+0x1f1>
  406cc1:	eb 00                	jmp    406cc3 <runtime::memory_equal+0x1f3>
  406cc3:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406cc8:	48 3b 44 24 f8       	cmp    -0x8(%rsp),%rax
  406ccd:	0f 92 c0             	setb   %al
  406cd0:	24 01                	and    $0x1,%al
  406cd2:	3c 00                	cmp    $0x0,%al
  406cd4:	74 3b                	je     406d11 <runtime::memory_equal+0x241>
  406cd6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406cdb:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406ce0:	8a 04 08             	mov    (%rax,%rcx,1),%al
  406ce3:	48 8b 0c 24          	mov    (%rsp),%rcx
  406ce7:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  406cec:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  406cef:	0f 95 c0             	setne  %al
  406cf2:	24 01                	and    $0x1,%al
  406cf4:	3c 00                	cmp    $0x0,%al
  406cf6:	74 07                	je     406cff <runtime::memory_equal+0x22f>
  406cf8:	31 c0                	xor    %eax,%eax
  406cfa:	48 83 c4 28          	add    $0x28,%rsp
  406cfe:	c3                   	ret
  406cff:	eb 00                	jmp    406d01 <runtime::memory_equal+0x231>
  406d01:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406d06:	48 83 c0 01          	add    $0x1,%rax
  406d0a:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406d0f:	eb b2                	jmp    406cc3 <runtime::memory_equal+0x1f3>
  406d11:	b0 01                	mov    $0x1,%al
  406d13:	48 83 c4 28          	add    $0x28,%rsp
  406d17:	c3                   	ret
  406d18:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  406d1f:	00 

0000000000406d20 <runtime::memory_compare>:
  406d20:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
  406d27:	48 89 7c 24 98       	mov    %rdi,-0x68(%rsp)
  406d2c:	48 89 74 24 a0       	mov    %rsi,-0x60(%rsp)
  406d31:	48 89 54 24 a8       	mov    %rdx,-0x58(%rsp)
  406d36:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  406d3b:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  406d40:	48 8b 54 24 a8       	mov    -0x58(%rsp),%rdx
  406d45:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  406d4c:	00 
  406d4d:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  406d54:	00 
  406d55:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  406d5c:	00 
  406d5d:	48 39 c8             	cmp    %rcx,%rax
  406d60:	0f 94 c1             	sete   %cl
  406d63:	80 e1 01             	and    $0x1,%cl
  406d66:	b0 01                	mov    $0x1,%al
  406d68:	38 c8                	cmp    %cl,%al
  406d6a:	74 17                	je     406d83 <runtime::memory_compare+0x63>
  406d6c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  406d71:	48 83 f8 00          	cmp    $0x0,%rax
  406d75:	0f 94 c1             	sete   %cl
  406d78:	80 e1 01             	and    $0x1,%cl
  406d7b:	b0 01                	mov    $0x1,%al
  406d7d:	38 c8                	cmp    %cl,%al
  406d7f:	74 23                	je     406da4 <runtime::memory_compare+0x84>
  406d81:	eb 0a                	jmp    406d8d <runtime::memory_compare+0x6d>
  406d83:	31 c0                	xor    %eax,%eax
  406d85:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  406d8c:	c3                   	ret
  406d8d:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  406d92:	48 83 f8 00          	cmp    $0x0,%rax
  406d96:	0f 94 c1             	sete   %cl
  406d99:	80 e1 01             	and    $0x1,%cl
  406d9c:	b0 01                	mov    $0x1,%al
  406d9e:	38 c8                	cmp    %cl,%al
  406da0:	74 13                	je     406db5 <runtime::memory_compare+0x95>
  406da2:	eb 0f                	jmp    406db3 <runtime::memory_compare+0x93>
  406da4:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  406dab:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  406db2:	c3                   	ret
  406db3:	eb 0d                	jmp    406dc2 <runtime::memory_compare+0xa2>
  406db5:	b8 01 00 00 00       	mov    $0x1,%eax
  406dba:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  406dc1:	c3                   	ret
  406dc2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  406dc7:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  406dcc:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  406dd1:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  406dd6:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  406ddb:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  406de0:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406de7:	00 00 
  406de9:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  406df0:	00 00 
  406df2:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  406df7:	48 2b 44 24 60       	sub    0x60(%rsp),%rax
  406dfc:	48 c1 e8 04          	shr    $0x4,%rax
  406e00:	48 c1 e0 04          	shl    $0x4,%rax
  406e04:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  406e09:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  406e0e:	48 3b 44 24 58       	cmp    0x58(%rsp),%rax
  406e13:	0f 92 c0             	setb   %al
  406e16:	24 01                	and    $0x1,%al
  406e18:	3c 00                	cmp    $0x0,%al
  406e1a:	0f 84 44 01 00 00    	je     406f64 <runtime::memory_compare+0x244>
  406e20:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  406e25:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  406e2a:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  406e2e:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406e33:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  406e38:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  406e3d:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  406e41:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  406e46:	0f 28 44 24 40       	movaps 0x40(%rsp),%xmm0
  406e4b:	0f 28 4c 24 30       	movaps 0x30(%rsp),%xmm1
  406e50:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  406e54:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  406e58:	66 0f ef c1          	pxor   %xmm1,%xmm0
  406e5c:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  406e61:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  406e66:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  406e6b:	66 0f eb c1          	por    %xmm1,%xmm0
  406e6f:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  406e74:	66 0f eb c1          	por    %xmm1,%xmm0
  406e78:	0f 28 c8             	movaps %xmm0,%xmm1
  406e7b:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  406e80:	66 0f eb c1          	por    %xmm1,%xmm0
  406e84:	0f 28 c8             	movaps %xmm0,%xmm1
  406e87:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  406e8c:	66 0f eb c1          	por    %xmm1,%xmm0
  406e90:	66 0f 7e c0          	movd   %xmm0,%eax
  406e94:	3c 00                	cmp    $0x0,%al
  406e96:	0f 95 c0             	setne  %al
  406e99:	24 01                	and    $0x1,%al
  406e9b:	3c 00                	cmp    $0x0,%al
  406e9d:	0f 84 ac 00 00 00    	je     406f4f <runtime::memory_compare+0x22f>
  406ea3:	66 0f 76 c0          	pcmpeqd %xmm0,%xmm0
  406ea7:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  406eac:	0f 28 05 3d 25 00 00 	movaps 0x253d(%rip),%xmm0        # 4093f0 <runtime::type_table+0xe0>
  406eb3:	0f 29 04 24          	movaps %xmm0,(%rsp)
  406eb7:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  406ebc:	0f 28 0c 24          	movaps (%rsp),%xmm1
  406ec0:	0f 28 54 24 10       	movaps 0x10(%rsp),%xmm2
  406ec5:	0f 57 db             	xorps  %xmm3,%xmm3
  406ec8:	66 0f 74 c3          	pcmpeqb %xmm3,%xmm0
  406ecc:	66 0f 38 10 ca       	pblendvb %xmm0,%xmm2,%xmm1
  406ed1:	0f 28 c1             	movaps %xmm1,%xmm0
  406ed4:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  406ed9:	0f 28 44 24 f0       	movaps -0x10(%rsp),%xmm0
  406ede:	0f 28 c8             	movaps %xmm0,%xmm1
  406ee1:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  406ee6:	66 0f da c1          	pminub %xmm1,%xmm0
  406eea:	66 0f 38 41 c0       	phminposuw %xmm0,%xmm0
  406eef:	66 0f 7e c0          	movd   %xmm0,%eax
  406ef3:	0f b6 c0             	movzbl %al,%eax
  406ef6:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406efb:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  406f00:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  406f05:	48 03 4c 24 e8       	add    -0x18(%rsp),%rcx
  406f0a:	8a 04 08             	mov    (%rax,%rcx,1),%al
  406f0d:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  406f12:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  406f17:	48 03 54 24 e8       	add    -0x18(%rsp),%rdx
  406f1c:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  406f1f:	0f 92 c0             	setb   %al
  406f22:	24 01                	and    $0x1,%al
  406f24:	3c 00                	cmp    $0x0,%al
  406f26:	74 0e                	je     406f36 <runtime::memory_compare+0x216>
  406f28:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  406f2f:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  406f34:	eb 0c                	jmp    406f42 <runtime::memory_compare+0x222>
  406f36:	b8 01 00 00 00       	mov    $0x1,%eax
  406f3b:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  406f40:	eb 00                	jmp    406f42 <runtime::memory_compare+0x222>
  406f42:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  406f47:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  406f4e:	c3                   	ret
  406f4f:	eb 00                	jmp    406f51 <runtime::memory_compare+0x231>
  406f51:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  406f56:	48 83 c0 10          	add    $0x10,%rax
  406f5a:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  406f5f:	e9 a5 fe ff ff       	jmp    406e09 <runtime::memory_compare+0xe9>
  406f64:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  406f69:	48 2b 44 24 60       	sub    0x60(%rsp),%rax
  406f6e:	48 c1 e8 03          	shr    $0x3,%rax
  406f72:	48 c1 e0 03          	shl    $0x3,%rax
  406f76:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  406f7b:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  406f80:	48 3b 44 24 58       	cmp    0x58(%rsp),%rax
  406f85:	0f 92 c0             	setb   %al
  406f88:	24 01                	and    $0x1,%al
  406f8a:	3c 00                	cmp    $0x0,%al
  406f8c:	0f 84 59 01 00 00    	je     4070eb <runtime::memory_compare+0x3cb>
  406f92:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  406f97:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  406f9c:	48 8b 04 08          	mov    (%rax,%rcx,1),%rax
  406fa0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406fa5:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  406faa:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  406faf:	48 8b 04 08          	mov    (%rax,%rcx,1),%rax
  406fb3:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  406fb8:	f3 0f 7e 44 24 e0    	movq   -0x20(%rsp),%xmm0
  406fbe:	f3 0f 7e 4c 24 d8    	movq   -0x28(%rsp),%xmm1
  406fc4:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  406fc8:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  406fcc:	66 0f ef c1          	pxor   %xmm1,%xmm0
  406fd0:	66 0f d6 44 24 d0    	movq   %xmm0,-0x30(%rsp)
  406fd6:	f3 0f 7e 44 24 d0    	movq   -0x30(%rsp),%xmm0
  406fdc:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  406fe1:	66 0f eb c1          	por    %xmm1,%xmm0
  406fe5:	0f 28 c8             	movaps %xmm0,%xmm1
  406fe8:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  406fed:	66 0f eb c1          	por    %xmm1,%xmm0
  406ff1:	0f 28 c8             	movaps %xmm0,%xmm1
  406ff4:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  406ff9:	66 0f eb c1          	por    %xmm1,%xmm0
  406ffd:	66 0f 7e c0          	movd   %xmm0,%eax
  407001:	3c 00                	cmp    $0x0,%al
  407003:	0f 95 c0             	setne  %al
  407006:	24 01                	and    $0x1,%al
  407008:	3c 00                	cmp    $0x0,%al
  40700a:	0f 84 c6 00 00 00    	je     4070d6 <runtime::memory_compare+0x3b6>
  407010:	48 c7 44 24 c8 ff ff 	movq   $0xffffffffffffffff,-0x38(%rsp)
  407017:	ff ff 
  407019:	48 b8 00 01 02 03 04 	movabs $0x706050403020100,%rax
  407020:	05 06 07 
  407023:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  407028:	f3 0f 7e 44 24 d0    	movq   -0x30(%rsp),%xmm0
  40702e:	f3 0f 7e 4c 24 c0    	movq   -0x40(%rsp),%xmm1
  407034:	f3 0f 7e 54 24 c8    	movq   -0x38(%rsp),%xmm2
  40703a:	0f 57 db             	xorps  %xmm3,%xmm3
  40703d:	66 0f 74 c3          	pcmpeqb %xmm3,%xmm0
  407041:	66 0f 38 10 ca       	pblendvb %xmm0,%xmm2,%xmm1
  407046:	0f 28 c1             	movaps %xmm1,%xmm0
  407049:	66 0f d6 44 24 b8    	movq   %xmm0,-0x48(%rsp)
  40704f:	f3 0f 7e 44 24 b8    	movq   -0x48(%rsp),%xmm0
  407055:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40705a:	66 0f da c1          	pminub %xmm1,%xmm0
  40705e:	0f 28 c8             	movaps %xmm0,%xmm1
  407061:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  407066:	66 0f da c1          	pminub %xmm1,%xmm0
  40706a:	0f 28 c8             	movaps %xmm0,%xmm1
  40706d:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  407072:	66 0f da c1          	pminub %xmm1,%xmm0
  407076:	66 0f 7e c0          	movd   %xmm0,%eax
  40707a:	0f b6 c0             	movzbl %al,%eax
  40707d:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  407082:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  407087:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40708c:	48 03 4c 24 b0       	add    -0x50(%rsp),%rcx
  407091:	8a 04 08             	mov    (%rax,%rcx,1),%al
  407094:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  407099:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40709e:	48 03 54 24 b0       	add    -0x50(%rsp),%rdx
  4070a3:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  4070a6:	0f 92 c0             	setb   %al
  4070a9:	24 01                	and    $0x1,%al
  4070ab:	3c 00                	cmp    $0x0,%al
  4070ad:	74 0e                	je     4070bd <runtime::memory_compare+0x39d>
  4070af:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4070b6:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  4070bb:	eb 0c                	jmp    4070c9 <runtime::memory_compare+0x3a9>
  4070bd:	b8 01 00 00 00       	mov    $0x1,%eax
  4070c2:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  4070c7:	eb 00                	jmp    4070c9 <runtime::memory_compare+0x3a9>
  4070c9:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  4070ce:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  4070d5:	c3                   	ret
  4070d6:	eb 00                	jmp    4070d8 <runtime::memory_compare+0x3b8>
  4070d8:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4070dd:	48 83 c0 08          	add    $0x8,%rax
  4070e1:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4070e6:	e9 90 fe ff ff       	jmp    406f7b <runtime::memory_compare+0x25b>
  4070eb:	eb 00                	jmp    4070ed <runtime::memory_compare+0x3cd>
  4070ed:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4070f2:	48 3b 44 24 68       	cmp    0x68(%rsp),%rax
  4070f7:	0f 92 c0             	setb   %al
  4070fa:	24 01                	and    $0x1,%al
  4070fc:	3c 00                	cmp    $0x0,%al
  4070fe:	0f 84 8d 00 00 00    	je     407191 <runtime::memory_compare+0x471>
  407104:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  407109:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40710e:	8a 04 08             	mov    (%rax,%rcx,1),%al
  407111:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  407116:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40711b:	32 04 11             	xor    (%rcx,%rdx,1),%al
  40711e:	3c 00                	cmp    $0x0,%al
  407120:	0f 95 c0             	setne  %al
  407123:	24 01                	and    $0x1,%al
  407125:	3c 00                	cmp    $0x0,%al
  407127:	74 53                	je     40717c <runtime::memory_compare+0x45c>
  407129:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40712e:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  407133:	0f b6 04 08          	movzbl (%rax,%rcx,1),%eax
  407137:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40713c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  407141:	0f b6 0c 11          	movzbl (%rcx,%rdx,1),%ecx
  407145:	48 29 c8             	sub    %rcx,%rax
  407148:	48 83 f8 00          	cmp    $0x0,%rax
  40714c:	0f 9c c0             	setl   %al
  40714f:	24 01                	and    $0x1,%al
  407151:	3c 00                	cmp    $0x0,%al
  407153:	74 0e                	je     407163 <runtime::memory_compare+0x443>
  407155:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40715c:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  407161:	eb 0c                	jmp    40716f <runtime::memory_compare+0x44f>
  407163:	b8 01 00 00 00       	mov    $0x1,%eax
  407168:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40716d:	eb 00                	jmp    40716f <runtime::memory_compare+0x44f>
  40716f:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  407174:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40717b:	c3                   	ret
  40717c:	eb 00                	jmp    40717e <runtime::memory_compare+0x45e>
  40717e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  407183:	48 83 c0 01          	add    $0x1,%rax
  407187:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40718c:	e9 5c ff ff ff       	jmp    4070ed <runtime::memory_compare+0x3cd>
  407191:	31 c0                	xor    %eax,%eax
  407193:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40719a:	c3                   	ret
  40719b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004071a0 <runtime::memory_compare_zero>:
  4071a0:	50                   	push   %rax
  4071a1:	48 89 7c 24 88       	mov    %rdi,-0x78(%rsp)
  4071a6:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  4071ab:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  4071b0:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  4071b5:	48 89 04 24          	mov    %rax,(%rsp)
  4071b9:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4071be:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4071c3:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4071ca:	00 00 
  4071cc:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  4071d3:	00 00 
  4071d5:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4071da:	48 83 7c 24 f0 08    	cmpq   $0x8,-0x10(%rsp)
  4071e0:	0f 93 c0             	setae  %al
  4071e3:	24 01                	and    $0x1,%al
  4071e5:	3c 00                	cmp    $0x0,%al
  4071e7:	0f 84 24 01 00 00    	je     407311 <runtime::memory_compare_zero+0x171>
  4071ed:	0f 57 c0             	xorps  %xmm0,%xmm0
  4071f0:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  4071f5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4071fa:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  4071ff:	48 c1 e8 04          	shr    $0x4,%rax
  407203:	48 c1 e0 04          	shl    $0x4,%rax
  407207:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40720c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  407211:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  407216:	0f 92 c0             	setb   %al
  407219:	24 01                	and    $0x1,%al
  40721b:	3c 00                	cmp    $0x0,%al
  40721d:	0f 84 88 00 00 00    	je     4072ab <runtime::memory_compare_zero+0x10b>
  407223:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407228:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40722d:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  407231:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  407236:	0f 28 44 24 c0       	movaps -0x40(%rsp),%xmm0
  40723b:	0f 28 4c 24 b0       	movaps -0x50(%rsp),%xmm1
  407240:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  407244:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  407248:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40724c:	0f 29 44 24 a0       	movaps %xmm0,-0x60(%rsp)
  407251:	0f 28 44 24 a0       	movaps -0x60(%rsp),%xmm0
  407256:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40725b:	66 0f eb c1          	por    %xmm1,%xmm0
  40725f:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  407264:	66 0f eb c1          	por    %xmm1,%xmm0
  407268:	0f 28 c8             	movaps %xmm0,%xmm1
  40726b:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  407270:	66 0f eb c1          	por    %xmm1,%xmm0
  407274:	0f 28 c8             	movaps %xmm0,%xmm1
  407277:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40727c:	66 0f eb c1          	por    %xmm1,%xmm0
  407280:	66 0f 7e c0          	movd   %xmm0,%eax
  407284:	3c 00                	cmp    $0x0,%al
  407286:	0f 95 c0             	setne  %al
  407289:	24 01                	and    $0x1,%al
  40728b:	3c 00                	cmp    $0x0,%al
  40728d:	74 07                	je     407296 <runtime::memory_compare_zero+0xf6>
  40728f:	b8 01 00 00 00       	mov    $0x1,%eax
  407294:	59                   	pop    %rcx
  407295:	c3                   	ret
  407296:	eb 00                	jmp    407298 <runtime::memory_compare_zero+0xf8>
  407298:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40729d:	48 83 c0 10          	add    $0x10,%rax
  4072a1:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4072a6:	e9 61 ff ff ff       	jmp    40720c <runtime::memory_compare_zero+0x6c>
  4072ab:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4072b0:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  4072b5:	48 c1 e8 03          	shr    $0x3,%rax
  4072b9:	48 c1 e0 03          	shl    $0x3,%rax
  4072bd:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4072c2:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4072c7:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4072cc:	0f 92 c0             	setb   %al
  4072cf:	24 01                	and    $0x1,%al
  4072d1:	3c 00                	cmp    $0x0,%al
  4072d3:	74 3a                	je     40730f <runtime::memory_compare_zero+0x16f>
  4072d5:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4072da:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  4072df:	48 8b 00             	mov    (%rax),%rax
  4072e2:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  4072e7:	48 83 7c 24 98 00    	cmpq   $0x0,-0x68(%rsp)
  4072ed:	0f 95 c0             	setne  %al
  4072f0:	24 01                	and    $0x1,%al
  4072f2:	3c 00                	cmp    $0x0,%al
  4072f4:	74 07                	je     4072fd <runtime::memory_compare_zero+0x15d>
  4072f6:	b8 01 00 00 00       	mov    $0x1,%eax
  4072fb:	59                   	pop    %rcx
  4072fc:	c3                   	ret
  4072fd:	eb 00                	jmp    4072ff <runtime::memory_compare_zero+0x15f>
  4072ff:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  407304:	48 83 c0 08          	add    $0x8,%rax
  407308:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40730d:	eb b3                	jmp    4072c2 <runtime::memory_compare_zero+0x122>
  40730f:	eb 00                	jmp    407311 <runtime::memory_compare_zero+0x171>
  407311:	eb 00                	jmp    407313 <runtime::memory_compare_zero+0x173>
  407313:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  407318:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  40731d:	0f 92 c0             	setb   %al
  407320:	24 01                	and    $0x1,%al
  407322:	3c 00                	cmp    $0x0,%al
  407324:	74 30                	je     407356 <runtime::memory_compare_zero+0x1b6>
  407326:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40732b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  407330:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  407334:	0f 95 c0             	setne  %al
  407337:	24 01                	and    $0x1,%al
  407339:	3c 00                	cmp    $0x0,%al
  40733b:	74 07                	je     407344 <runtime::memory_compare_zero+0x1a4>
  40733d:	b8 01 00 00 00       	mov    $0x1,%eax
  407342:	59                   	pop    %rcx
  407343:	c3                   	ret
  407344:	eb 00                	jmp    407346 <runtime::memory_compare_zero+0x1a6>
  407346:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40734b:	48 83 c0 01          	add    $0x1,%rax
  40734f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  407354:	eb bd                	jmp    407313 <runtime::memory_compare_zero+0x173>
  407356:	31 c0                	xor    %eax,%eax
  407358:	59                   	pop    %rcx
  407359:	c3                   	ret
  40735a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000407360 <runtime::cstring_len>:
  407360:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  407365:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40736a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40736f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407374:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407379:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40737e:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  407384:	0f 95 c0             	setne  %al
  407387:	24 01                	and    $0x1,%al
  407389:	3c 00                	cmp    $0x0,%al
  40738b:	74 21                	je     4073ae <runtime::cstring_len+0x4e>
  40738d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  407392:	80 38 00             	cmpb   $0x0,(%rax)
  407395:	0f 95 c0             	setne  %al
  407398:	24 01                	and    $0x1,%al
  40739a:	3c 00                	cmp    $0x0,%al
  40739c:	74 10                	je     4073ae <runtime::cstring_len+0x4e>
  40739e:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4073a3:	48 83 c0 01          	add    $0x1,%rax
  4073a7:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4073ac:	eb d0                	jmp    40737e <runtime::cstring_len+0x1e>
  4073ae:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4073b3:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  4073b8:	c3                   	ret
  4073b9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004073c0 <runtime::cstring_to_string>:
  4073c0:	48 83 ec 48          	sub    $0x48,%rsp
  4073c4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4073c9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4073ce:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4073d3:	48 83 f8 00          	cmp    $0x0,%rax
  4073d7:	0f 94 c0             	sete   %al
  4073da:	24 01                	and    $0x1,%al
  4073dc:	3c 00                	cmp    $0x0,%al
  4073de:	74 21                	je     407401 <runtime::cstring_to_string+0x41>
  4073e0:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  4073e7:	00 00 
  4073e9:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4073f0:	00 00 
  4073f2:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4073f7:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4073fc:	48 83 c4 48          	add    $0x48,%rsp
  407400:	c3                   	ret
  407401:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  407406:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40740b:	e8 50 ff ff ff       	call   407360 <runtime::cstring_len>
  407410:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407415:	0f 57 c0             	xorps  %xmm0,%xmm0
  407418:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  40741d:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  407424:	00 00 
  407426:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  40742d:	00 00 
  40742f:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  407434:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  407439:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40743e:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  407443:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  407448:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40744d:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  407452:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  407457:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40745c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  407461:	48 83 c4 48          	add    $0x48,%rsp
  407465:	c3                   	ret
  407466:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40746d:	00 00 00 

0000000000407470 <runtime::cstring_eq>:
  407470:	48 83 ec 48          	sub    $0x48,%rsp
  407474:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  407479:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40747e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  407483:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  407488:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40748d:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  407492:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  407497:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40749c:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4074a1:	48 3b 44 24 28       	cmp    0x28(%rsp),%rax
  4074a6:	0f 94 c0             	sete   %al
  4074a9:	24 01                	and    $0x1,%al
  4074ab:	3c 00                	cmp    $0x0,%al
  4074ad:	74 07                	je     4074b6 <runtime::cstring_eq+0x46>
  4074af:	b0 01                	mov    $0x1,%al
  4074b1:	48 83 c4 48          	add    $0x48,%rsp
  4074b5:	c3                   	ret
  4074b6:	48 83 7c 24 30 00    	cmpq   $0x0,0x30(%rsp)
  4074bc:	0f 94 c0             	sete   %al
  4074bf:	24 01                	and    $0x1,%al
  4074c1:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
  4074c7:	0f 94 c1             	sete   %cl
  4074ca:	80 e1 01             	and    $0x1,%cl
  4074cd:	30 c8                	xor    %cl,%al
  4074cf:	3c 00                	cmp    $0x0,%al
  4074d1:	74 07                	je     4074da <runtime::cstring_eq+0x6a>
  4074d3:	31 c0                	xor    %eax,%eax
  4074d5:	48 83 c4 48          	add    $0x48,%rsp
  4074d9:	c3                   	ret
  4074da:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4074df:	e8 7c fe ff ff       	call   407360 <runtime::cstring_len>
  4074e4:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4074e9:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4074ee:	e8 6d fe ff ff       	call   407360 <runtime::cstring_len>
  4074f3:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4074f8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4074fd:	48 3b 44 24 18       	cmp    0x18(%rsp),%rax
  407502:	0f 95 c0             	setne  %al
  407505:	24 01                	and    $0x1,%al
  407507:	3c 00                	cmp    $0x0,%al
  407509:	74 07                	je     407512 <runtime::cstring_eq+0xa2>
  40750b:	31 c0                	xor    %eax,%eax
  40750d:	48 83 c4 48          	add    $0x48,%rsp
  407511:	c3                   	ret
  407512:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  407517:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40751c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  407521:	e8 aa f5 ff ff       	call   406ad0 <runtime::memory_equal>
  407526:	48 83 c4 48          	add    $0x48,%rsp
  40752a:	c3                   	ret
  40752b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000407530 <runtime::cstring_ne>:
  407530:	48 83 ec 28          	sub    $0x28,%rsp
  407534:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  407539:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40753e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  407543:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  407548:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40754d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  407552:	e8 19 ff ff ff       	call   407470 <runtime::cstring_eq>
  407557:	3c 00                	cmp    $0x0,%al
  407559:	0f 94 c0             	sete   %al
  40755c:	24 01                	and    $0x1,%al
  40755e:	48 83 c4 28          	add    $0x28,%rsp
  407562:	c3                   	ret
  407563:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40756a:	84 00 00 00 00 00 

0000000000407570 <__truncsfhf2>:
  407570:	48 83 ec 18          	sub    $0x18,%rsp
  407574:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  40757a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  407580:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  407586:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40758d:	00 
  40758e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  407595:	00 
  407596:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40759d:	00 
  40759e:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  4075a5:	00 
  4075a6:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  4075ad:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  4075b3:	8b 44 24 10          	mov    0x10(%rsp),%eax
  4075b7:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  4075bb:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4075bf:	c1 f9 10             	sar    $0x10,%ecx
  4075c2:	b2 01                	mov    $0x1,%dl
  4075c4:	31 c0                	xor    %eax,%eax
  4075c6:	f6 c2 01             	test   $0x1,%dl
  4075c9:	0f 45 c1             	cmovne %ecx,%eax
  4075cc:	25 00 80 00 00       	and    $0x8000,%eax
  4075d1:	89 44 24 08          	mov    %eax,0x8(%rsp)
  4075d5:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4075d9:	c1 f9 17             	sar    $0x17,%ecx
  4075dc:	b2 01                	mov    $0x1,%dl
  4075de:	31 c0                	xor    %eax,%eax
  4075e0:	f6 c2 01             	test   $0x1,%dl
  4075e3:	0f 45 c1             	cmovne %ecx,%eax
  4075e6:	25 ff 00 00 00       	and    $0xff,%eax
  4075eb:	83 e8 70             	sub    $0x70,%eax
  4075ee:	89 44 24 04          	mov    %eax,0x4(%rsp)
  4075f2:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  4075f6:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  4075fb:	89 04 24             	mov    %eax,(%rsp)
  4075fe:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  407603:	0f 9e c0             	setle  %al
  407606:	24 01                	and    $0x1,%al
  407608:	3c 00                	cmp    $0x0,%al
  40760a:	0f 84 82 00 00 00    	je     407692 <__truncsfhf2+0x122>
  407610:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  407615:	0f 9c c0             	setl   %al
  407618:	24 01                	and    $0x1,%al
  40761a:	3c 00                	cmp    $0x0,%al
  40761c:	74 16                	je     407634 <__truncsfhf2+0xc4>
  40761e:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  407623:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  407628:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  40762f:	48 83 c4 18          	add    $0x18,%rsp
  407633:	c3                   	ret
  407634:	8b 04 24             	mov    (%rsp),%eax
  407637:	0d 00 00 80 00       	or     $0x800000,%eax
  40763c:	ba 01 00 00 00       	mov    $0x1,%edx
  407641:	2b 54 24 04          	sub    0x4(%rsp),%edx
  407645:	89 d1                	mov    %edx,%ecx
  407647:	d3 f8                	sar    %cl,%eax
  407649:	89 c1                	mov    %eax,%ecx
  40764b:	31 c0                	xor    %eax,%eax
  40764d:	83 fa 20             	cmp    $0x20,%edx
  407650:	0f 42 c1             	cmovb  %ecx,%eax
  407653:	89 04 24             	mov    %eax,(%rsp)
  407656:	8b 04 24             	mov    (%rsp),%eax
  407659:	25 00 10 00 00       	and    $0x1000,%eax
  40765e:	83 f8 00             	cmp    $0x0,%eax
  407661:	0f 95 c0             	setne  %al
  407664:	24 01                	and    $0x1,%al
  407666:	3c 00                	cmp    $0x0,%al
  407668:	74 0b                	je     407675 <__truncsfhf2+0x105>
  40766a:	8b 04 24             	mov    (%rsp),%eax
  40766d:	05 00 20 00 00       	add    $0x2000,%eax
  407672:	89 04 24             	mov    %eax,(%rsp)
  407675:	8b 44 24 08          	mov    0x8(%rsp),%eax
  407679:	8b 0c 24             	mov    (%rsp),%ecx
  40767c:	c1 e9 0d             	shr    $0xd,%ecx
  40767f:	09 c8                	or     %ecx,%eax
  407681:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  407686:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40768d:	48 83 c4 18          	add    $0x18,%rsp
  407691:	c3                   	ret
  407692:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  407699:	00 
  40769a:	0f 94 c0             	sete   %al
  40769d:	24 01                	and    $0x1,%al
  40769f:	3c 00                	cmp    $0x0,%al
  4076a1:	74 59                	je     4076fc <__truncsfhf2+0x18c>
  4076a3:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  4076a7:	0f 94 c0             	sete   %al
  4076aa:	24 01                	and    $0x1,%al
  4076ac:	3c 00                	cmp    $0x0,%al
  4076ae:	74 1a                	je     4076ca <__truncsfhf2+0x15a>
  4076b0:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4076b4:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4076b9:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  4076be:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  4076c5:	48 83 c4 18          	add    $0x18,%rsp
  4076c9:	c3                   	ret
  4076ca:	8b 04 24             	mov    (%rsp),%eax
  4076cd:	c1 f8 0d             	sar    $0xd,%eax
  4076d0:	89 04 24             	mov    %eax,(%rsp)
  4076d3:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4076d7:	8b 0c 24             	mov    (%rsp),%ecx
  4076da:	09 c8                	or     %ecx,%eax
  4076dc:	85 c9                	test   %ecx,%ecx
  4076de:	0f 94 c1             	sete   %cl
  4076e1:	0f b6 c9             	movzbl %cl,%ecx
  4076e4:	09 c8                	or     %ecx,%eax
  4076e6:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4076eb:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  4076f0:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  4076f7:	48 83 c4 18          	add    $0x18,%rsp
  4076fb:	c3                   	ret
  4076fc:	8b 04 24             	mov    (%rsp),%eax
  4076ff:	25 00 10 00 00       	and    $0x1000,%eax
  407704:	83 f8 00             	cmp    $0x0,%eax
  407707:	0f 95 c0             	setne  %al
  40770a:	24 01                	and    $0x1,%al
  40770c:	3c 00                	cmp    $0x0,%al
  40770e:	74 33                	je     407743 <__truncsfhf2+0x1d3>
  407710:	8b 04 24             	mov    (%rsp),%eax
  407713:	05 00 20 00 00       	add    $0x2000,%eax
  407718:	89 04 24             	mov    %eax,(%rsp)
  40771b:	8b 04 24             	mov    (%rsp),%eax
  40771e:	25 00 00 80 00       	and    $0x800000,%eax
  407723:	83 f8 00             	cmp    $0x0,%eax
  407726:	0f 95 c0             	setne  %al
  407729:	24 01                	and    $0x1,%al
  40772b:	3c 00                	cmp    $0x0,%al
  40772d:	74 12                	je     407741 <__truncsfhf2+0x1d1>
  40772f:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  407736:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40773a:	83 c0 01             	add    $0x1,%eax
  40773d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  407741:	eb 00                	jmp    407743 <__truncsfhf2+0x1d3>
  407743:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  407748:	0f 9f c0             	setg   %al
  40774b:	24 01                	and    $0x1,%al
  40774d:	3c 00                	cmp    $0x0,%al
  40774f:	74 75                	je     4077c6 <__truncsfhf2+0x256>
  407751:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  407758:	00 00 00 
  40775b:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  407760:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  407767:	00 00 
  407769:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  40776f:	0f 9c c0             	setl   %al
  407772:	24 01                	and    $0x1,%al
  407774:	3c 00                	cmp    $0x0,%al
  407776:	74 34                	je     4077ac <__truncsfhf2+0x23c>
  407778:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40777d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  407782:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  407787:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  40778d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  407792:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  407797:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40779c:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4077a1:	48 83 c0 01          	add    $0x1,%rax
  4077a5:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4077aa:	eb bd                	jmp    407769 <__truncsfhf2+0x1f9>
  4077ac:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4077b0:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4077b5:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  4077ba:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  4077c1:	48 83 c4 18          	add    $0x18,%rsp
  4077c5:	c3                   	ret
  4077c6:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4077ca:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  4077ce:	c1 e1 0a             	shl    $0xa,%ecx
  4077d1:	09 c8                	or     %ecx,%eax
  4077d3:	8b 0c 24             	mov    (%rsp),%ecx
  4077d6:	c1 e9 0d             	shr    $0xd,%ecx
  4077d9:	09 c8                	or     %ecx,%eax
  4077db:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  4077e0:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  4077e7:	48 83 c4 18          	add    $0x18,%rsp
  4077eb:	c3                   	ret
  4077ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004077f0 <__truncdfhf2>:
  4077f0:	48 83 ec 18          	sub    $0x18,%rsp
  4077f4:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  4077fa:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  407800:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  407806:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  40780a:	e8 61 fd ff ff       	call   407570 <__truncsfhf2>
  40780f:	48 83 c4 18          	add    $0x18,%rsp
  407813:	c3                   	ret
  407814:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40781b:	00 00 00 00 00 

0000000000407820 <__gnu_h2f_ieee>:
  407820:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  407826:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  40782c:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  407833:	00 
  407834:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  40783b:	00 
  40783c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  407841:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  407846:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  40784d:	00 
  40784e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  407855:	00 
  407856:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  40785d:	00 
  40785e:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  407865:	77 
  407866:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  40786d:	47 
  40786e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  407873:	66 25 ff 7f          	and    $0x7fff,%ax
  407877:	0f b7 c8             	movzwl %ax,%ecx
  40787a:	c1 e1 0d             	shl    $0xd,%ecx
  40787d:	b2 01                	mov    $0x1,%dl
  40787f:	31 c0                	xor    %eax,%eax
  407881:	f6 c2 01             	test   $0x1,%dl
  407884:	0f 45 c1             	cmovne %ecx,%eax
  407887:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40788b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  407891:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  407897:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  40789d:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  4078a3:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  4078a8:	0f 93 c0             	setae  %al
  4078ab:	24 01                	and    $0x1,%al
  4078ad:	3c 00                	cmp    $0x0,%al
  4078af:	74 0d                	je     4078be <__gnu_h2f_ieee+0x9e>
  4078b1:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4078b5:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  4078ba:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4078be:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  4078c3:	66 25 00 80          	and    $0x8000,%ax
  4078c7:	0f b7 c8             	movzwl %ax,%ecx
  4078ca:	c1 e1 10             	shl    $0x10,%ecx
  4078cd:	b2 01                	mov    $0x1,%dl
  4078cf:	31 c0                	xor    %eax,%eax
  4078d1:	f6 c2 01             	test   $0x1,%dl
  4078d4:	0f 45 c1             	cmovne %ecx,%eax
  4078d7:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  4078db:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4078df:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  4078e5:	c3                   	ret
  4078e6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4078ed:	00 00 00 

00000000004078f0 <__gnu_f2h_ieee>:
  4078f0:	50                   	push   %rax
  4078f1:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  4078f6:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  4078fb:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  407901:	e8 6a fc ff ff       	call   407570 <__truncsfhf2>
  407906:	58                   	pop    %rax
  407907:	c3                   	ret
  407908:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40790f:	00 

0000000000407910 <__extendhfsf2>:
  407910:	50                   	push   %rax
  407911:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  407917:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  40791d:	0f 28 c8             	movaps %xmm0,%xmm1
  407920:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  407927:	00 
  407928:	e8 f3 fe ff ff       	call   407820 <__gnu_h2f_ieee>
  40792d:	58                   	pop    %rax
  40792e:	c3                   	ret
  40792f:	90                   	nop

0000000000407930 <__floattidf>:
  407930:	53                   	push   %rbx
  407931:	48 83 ec 10          	sub    $0x10,%rsp
  407935:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40793a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40793f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  407944:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  407949:	48 89 04 24          	mov    %rax,(%rsp)
  40794d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  407952:	48 09 c8             	or     %rcx,%rax
  407955:	0f 94 c0             	sete   %al
  407958:	24 01                	and    $0x1,%al
  40795a:	3c 00                	cmp    $0x0,%al
  40795c:	74 09                	je     407967 <__floattidf+0x37>
  40795e:	0f 57 c0             	xorps  %xmm0,%xmm0
  407961:	48 83 c4 10          	add    $0x10,%rsp
  407965:	5b                   	pop    %rbx
  407966:	c3                   	ret
  407967:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40796c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  407971:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  407976:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40797b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  407980:	48 c1 f8 3f          	sar    $0x3f,%rax
  407984:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  407989:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40798e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  407993:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  407998:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40799d:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4079a2:	48 31 d0             	xor    %rdx,%rax
  4079a5:	48 31 f1             	xor    %rsi,%rcx
  4079a8:	48 29 f1             	sub    %rsi,%rcx
  4079ab:	48 19 d0             	sbb    %rdx,%rax
  4079ae:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4079b3:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4079b8:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  4079bd:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4079c2:	48 0f bd c2          	bsr    %rdx,%rax
  4079c6:	48 83 f0 3f          	xor    $0x3f,%rax
  4079ca:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  4079cf:	48 0f bd ce          	bsr    %rsi,%rcx
  4079d3:	48 83 f1 3f          	xor    $0x3f,%rcx
  4079d7:	48 83 c1 40          	add    $0x40,%rcx
  4079db:	48 85 d2             	test   %rdx,%rdx
  4079de:	48 0f 45 c8          	cmovne %rax,%rcx
  4079e2:	31 c0                	xor    %eax,%eax
  4079e4:	ba 80 00 00 00       	mov    $0x80,%edx
  4079e9:	48 29 ca             	sub    %rcx,%rdx
  4079ec:	48 89 c1             	mov    %rax,%rcx
  4079ef:	48 19 c9             	sbb    %rcx,%rcx
  4079f2:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4079f7:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4079fc:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  407a00:	ff c9                	dec    %ecx
  407a02:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  407a06:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  407a0b:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  407a10:	ba 35 00 00 00       	mov    $0x35,%edx
  407a15:	48 29 f2             	sub    %rsi,%rdx
  407a18:	48 19 c8             	sbb    %rcx,%rax
  407a1b:	0f 9c c0             	setl   %al
  407a1e:	24 01                	and    $0x1,%al
  407a20:	3c 00                	cmp    $0x0,%al
  407a22:	0f 84 c0 01 00 00    	je     407be8 <__floattidf+0x2b8>
  407a28:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  407a2d:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  407a32:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407a37:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  407a3c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  407a41:	0f 28 0d b8 19 00 00 	movaps 0x19b8(%rip),%xmm1        # 409400 <runtime::type_table+0xf0>
  407a48:	66 0f ef c1          	pxor   %xmm1,%xmm0
  407a4c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  407a51:	74 17                	je     407a6a <__floattidf+0x13a>
  407a53:	eb 00                	jmp    407a55 <__floattidf+0x125>
  407a55:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  407a5a:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  407a5f:	48 83 f0 37          	xor    $0x37,%rax
  407a63:	48 09 c8             	or     %rcx,%rax
  407a66:	74 26                	je     407a8e <__floattidf+0x15e>
  407a68:	eb 29                	jmp    407a93 <__floattidf+0x163>
  407a6a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  407a6f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  407a74:	48 89 d0             	mov    %rdx,%rax
  407a77:	48 01 c0             	add    %rax,%rax
  407a7a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  407a7f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  407a84:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407a89:	e9 d6 00 00 00       	jmp    407b64 <__floattidf+0x234>
  407a8e:	e9 d1 00 00 00       	jmp    407b64 <__floattidf+0x234>
  407a93:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  407a98:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  407a9d:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  407aa2:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407aa7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  407aac:	49 89 fb             	mov    %rdi,%r11
  407aaf:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  407ab3:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  407ab7:	44 88 db             	mov    %r11b,%bl
  407aba:	88 d9                	mov    %bl,%cl
  407abc:	49 89 f2             	mov    %rsi,%r10
  407abf:	49 d3 ea             	shr    %cl,%r10
  407ac2:	88 d9                	mov    %bl,%cl
  407ac4:	49 89 d1             	mov    %rdx,%r9
  407ac7:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  407acb:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  407ad0:	45 31 c0             	xor    %r8d,%r8d
  407ad3:	f6 c3 40             	test   $0x40,%bl
  407ad6:	4d 0f 45 ca          	cmovne %r10,%r9
  407ada:	4d 0f 45 d0          	cmovne %r8,%r10
  407ade:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  407ae5:	48 83 d8 00          	sbb    $0x0,%rax
  407ae9:	4c 89 c0             	mov    %r8,%rax
  407aec:	49 0f 42 c2          	cmovb  %r10,%rax
  407af0:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  407af5:	4c 89 c0             	mov    %r8,%rax
  407af8:	49 0f 42 c1          	cmovb  %r9,%rax
  407afc:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  407b02:	49 29 fb             	sub    %rdi,%r11
  407b05:	4c 89 c7             	mov    %r8,%rdi
  407b08:	48 19 cf             	sbb    %rcx,%rdi
  407b0b:	45 88 d9             	mov    %r11b,%r9b
  407b0e:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  407b15:	44 88 c9             	mov    %r9b,%cl
  407b18:	4c 89 d3             	mov    %r10,%rbx
  407b1b:	48 d3 eb             	shr    %cl,%rbx
  407b1e:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  407b23:	41 f6 c1 40          	test   $0x40,%r9b
  407b27:	49 89 d9             	mov    %rbx,%r9
  407b2a:	4d 0f 45 c8          	cmovne %r8,%r9
  407b2e:	4c 0f 45 d3          	cmovne %rbx,%r10
  407b32:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  407b39:	48 83 df 00          	sbb    $0x0,%rdi
  407b3d:	4c 89 c7             	mov    %r8,%rdi
  407b40:	49 0f 42 fa          	cmovb  %r10,%rdi
  407b44:	4d 0f 42 c1          	cmovb  %r9,%r8
  407b48:	4c 21 c6             	and    %r8,%rsi
  407b4b:	48 21 fa             	and    %rdi,%rdx
  407b4e:	48 09 f2             	or     %rsi,%rdx
  407b51:	0f 95 c2             	setne  %dl
  407b54:	0f b6 d2             	movzbl %dl,%edx
  407b57:	48 09 d0             	or     %rdx,%rax
  407b5a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  407b5f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407b64:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407b69:	89 c1                	mov    %eax,%ecx
  407b6b:	83 e1 04             	and    $0x4,%ecx
  407b6e:	c1 e9 02             	shr    $0x2,%ecx
  407b71:	48 09 c8             	or     %rcx,%rax
  407b74:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407b79:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407b7e:	48 83 c0 01          	add    $0x1,%rax
  407b82:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  407b88:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407b8d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  407b92:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  407b97:	48 89 c8             	mov    %rcx,%rax
  407b9a:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  407b9f:	48 c1 f9 02          	sar    $0x2,%rcx
  407ba3:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  407ba8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407bad:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  407bb1:	24 20                	and    $0x20,%al
  407bb3:	c0 e8 05             	shr    $0x5,%al
  407bb6:	24 01                	and    $0x1,%al
  407bb8:	3c 00                	cmp    $0x0,%al
  407bba:	74 2a                	je     407be6 <__floattidf+0x2b6>
  407bbc:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  407bc1:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  407bc6:	48 89 c8             	mov    %rcx,%rax
  407bc9:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  407bce:	48 d1 f9             	sar    $1,%rcx
  407bd1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  407bd6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407bdb:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  407bdf:	83 c0 01             	add    $0x1,%eax
  407be2:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  407be6:	eb 5c                	jmp    407c44 <__floattidf+0x314>
  407be8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  407bec:	b9 35 00 00 00       	mov    $0x35,%ecx
  407bf1:	29 c1                	sub    %eax,%ecx
  407bf3:	83 e1 7f             	and    $0x7f,%ecx
  407bf6:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  407bfa:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407bff:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  407c04:	40 88 cf             	mov    %cl,%dil
  407c07:	40 88 f9             	mov    %dil,%cl
  407c0a:	48 89 c2             	mov    %rax,%rdx
  407c0d:	48 d3 e2             	shl    %cl,%rdx
  407c10:	40 88 f9             	mov    %dil,%cl
  407c13:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  407c17:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  407c1b:	31 c0                	xor    %eax,%eax
  407c1d:	40 f6 c7 40          	test   $0x40,%dil
  407c21:	48 0f 45 f2          	cmovne %rdx,%rsi
  407c25:	48 0f 45 d0          	cmovne %rax,%rdx
  407c29:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  407c2f:	48 89 c1             	mov    %rax,%rcx
  407c32:	48 0f 42 ce          	cmovb  %rsi,%rcx
  407c36:	48 0f 42 c2          	cmovb  %rdx,%rax
  407c3a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  407c3f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407c44:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  407c4b:	00 00 
  407c4d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  407c51:	25 00 00 00 80       	and    $0x80000000,%eax
  407c56:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  407c5a:	c1 e1 14             	shl    $0x14,%ecx
  407c5d:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  407c63:	09 c8                	or     %ecx,%eax
  407c65:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  407c69:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  407c6f:	09 c8                	or     %ecx,%eax
  407c71:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  407c75:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  407c79:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  407c7d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  407c83:	48 83 c4 10          	add    $0x10,%rsp
  407c87:	5b                   	pop    %rbx
  407c88:	c3                   	ret
  407c89:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000407c90 <__floattidf_unsigned>:
  407c90:	53                   	push   %rbx
  407c91:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  407c96:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  407c9b:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  407ca0:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  407ca5:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407caa:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  407caf:	48 09 c8             	or     %rcx,%rax
  407cb2:	0f 94 c0             	sete   %al
  407cb5:	24 01                	and    $0x1,%al
  407cb7:	3c 00                	cmp    $0x0,%al
  407cb9:	74 05                	je     407cc0 <__floattidf_unsigned+0x30>
  407cbb:	0f 57 c0             	xorps  %xmm0,%xmm0
  407cbe:	5b                   	pop    %rbx
  407cbf:	c3                   	ret
  407cc0:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  407cc5:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  407cca:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  407ccf:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  407cd4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  407cd9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  407cde:	48 0f bd c2          	bsr    %rdx,%rax
  407ce2:	48 83 f0 3f          	xor    $0x3f,%rax
  407ce6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  407ceb:	48 0f bd ce          	bsr    %rsi,%rcx
  407cef:	48 83 f1 3f          	xor    $0x3f,%rcx
  407cf3:	48 83 c1 40          	add    $0x40,%rcx
  407cf7:	48 85 d2             	test   %rdx,%rdx
  407cfa:	48 0f 45 c8          	cmovne %rax,%rcx
  407cfe:	31 c0                	xor    %eax,%eax
  407d00:	ba 80 00 00 00       	mov    $0x80,%edx
  407d05:	48 29 ca             	sub    %rcx,%rdx
  407d08:	48 89 c1             	mov    %rax,%rcx
  407d0b:	48 19 c9             	sbb    %rcx,%rcx
  407d0e:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  407d13:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  407d18:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  407d1c:	ff c9                	dec    %ecx
  407d1e:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  407d22:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  407d27:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  407d2c:	ba 35 00 00 00       	mov    $0x35,%edx
  407d31:	48 29 f2             	sub    %rsi,%rdx
  407d34:	48 19 c8             	sbb    %rcx,%rax
  407d37:	0f 92 c0             	setb   %al
  407d3a:	24 01                	and    $0x1,%al
  407d3c:	3c 00                	cmp    $0x0,%al
  407d3e:	0f 84 c0 01 00 00    	je     407f04 <__floattidf_unsigned+0x274>
  407d44:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  407d49:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  407d4e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407d53:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  407d58:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  407d5d:	0f 28 0d 9c 16 00 00 	movaps 0x169c(%rip),%xmm1        # 409400 <runtime::type_table+0xf0>
  407d64:	66 0f ef c1          	pxor   %xmm1,%xmm0
  407d68:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  407d6d:	74 17                	je     407d86 <__floattidf_unsigned+0xf6>
  407d6f:	eb 00                	jmp    407d71 <__floattidf_unsigned+0xe1>
  407d71:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  407d76:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  407d7b:	48 83 f0 37          	xor    $0x37,%rax
  407d7f:	48 09 c8             	or     %rcx,%rax
  407d82:	74 26                	je     407daa <__floattidf_unsigned+0x11a>
  407d84:	eb 29                	jmp    407daf <__floattidf_unsigned+0x11f>
  407d86:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  407d8b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  407d90:	48 89 d0             	mov    %rdx,%rax
  407d93:	48 01 c0             	add    %rax,%rax
  407d96:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  407d9b:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  407da0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407da5:	e9 d6 00 00 00       	jmp    407e80 <__floattidf_unsigned+0x1f0>
  407daa:	e9 d1 00 00 00       	jmp    407e80 <__floattidf_unsigned+0x1f0>
  407daf:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  407db4:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  407db9:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  407dbe:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407dc3:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  407dc8:	49 89 fb             	mov    %rdi,%r11
  407dcb:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  407dcf:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  407dd3:	44 88 db             	mov    %r11b,%bl
  407dd6:	88 d9                	mov    %bl,%cl
  407dd8:	49 89 f2             	mov    %rsi,%r10
  407ddb:	49 d3 ea             	shr    %cl,%r10
  407dde:	88 d9                	mov    %bl,%cl
  407de0:	49 89 d1             	mov    %rdx,%r9
  407de3:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  407de7:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  407dec:	45 31 c0             	xor    %r8d,%r8d
  407def:	f6 c3 40             	test   $0x40,%bl
  407df2:	4d 0f 45 ca          	cmovne %r10,%r9
  407df6:	4d 0f 45 d0          	cmovne %r8,%r10
  407dfa:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  407e01:	48 83 d8 00          	sbb    $0x0,%rax
  407e05:	4c 89 c0             	mov    %r8,%rax
  407e08:	49 0f 42 c2          	cmovb  %r10,%rax
  407e0c:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  407e11:	4c 89 c0             	mov    %r8,%rax
  407e14:	49 0f 42 c1          	cmovb  %r9,%rax
  407e18:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  407e1e:	49 29 fb             	sub    %rdi,%r11
  407e21:	4c 89 c7             	mov    %r8,%rdi
  407e24:	48 19 cf             	sbb    %rcx,%rdi
  407e27:	45 88 d9             	mov    %r11b,%r9b
  407e2a:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  407e31:	44 88 c9             	mov    %r9b,%cl
  407e34:	4c 89 d3             	mov    %r10,%rbx
  407e37:	48 d3 eb             	shr    %cl,%rbx
  407e3a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  407e3f:	41 f6 c1 40          	test   $0x40,%r9b
  407e43:	49 89 d9             	mov    %rbx,%r9
  407e46:	4d 0f 45 c8          	cmovne %r8,%r9
  407e4a:	4c 0f 45 d3          	cmovne %rbx,%r10
  407e4e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  407e55:	48 83 df 00          	sbb    $0x0,%rdi
  407e59:	4c 89 c7             	mov    %r8,%rdi
  407e5c:	49 0f 42 fa          	cmovb  %r10,%rdi
  407e60:	4d 0f 42 c1          	cmovb  %r9,%r8
  407e64:	4c 21 c6             	and    %r8,%rsi
  407e67:	48 21 fa             	and    %rdi,%rdx
  407e6a:	48 09 f2             	or     %rsi,%rdx
  407e6d:	0f 95 c2             	setne  %dl
  407e70:	0f b6 d2             	movzbl %dl,%edx
  407e73:	48 09 d0             	or     %rdx,%rax
  407e76:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  407e7b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407e80:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  407e85:	89 c1                	mov    %eax,%ecx
  407e87:	83 e1 04             	and    $0x4,%ecx
  407e8a:	c1 e9 02             	shr    $0x2,%ecx
  407e8d:	48 09 c8             	or     %rcx,%rax
  407e90:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407e95:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  407e9a:	48 83 c0 01          	add    $0x1,%rax
  407e9e:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  407ea4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407ea9:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  407eae:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  407eb3:	48 89 c8             	mov    %rcx,%rax
  407eb6:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  407ebb:	48 c1 e9 02          	shr    $0x2,%rcx
  407ebf:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  407ec4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407ec9:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  407ecd:	24 20                	and    $0x20,%al
  407ecf:	c0 e8 05             	shr    $0x5,%al
  407ed2:	24 01                	and    $0x1,%al
  407ed4:	3c 00                	cmp    $0x0,%al
  407ed6:	74 2a                	je     407f02 <__floattidf_unsigned+0x272>
  407ed8:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  407edd:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  407ee2:	48 89 c8             	mov    %rcx,%rax
  407ee5:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  407eea:	48 d1 e9             	shr    $1,%rcx
  407eed:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  407ef2:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407ef7:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  407efb:	83 c0 01             	add    $0x1,%eax
  407efe:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  407f02:	eb 6a                	jmp    407f6e <__floattidf_unsigned+0x2de>
  407f04:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  407f09:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  407f0e:	31 c0                	xor    %eax,%eax
  407f10:	bf 35 00 00 00       	mov    $0x35,%edi
  407f15:	48 29 d7             	sub    %rdx,%rdi
  407f18:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  407f1d:	48 19 c8             	sbb    %rcx,%rax
  407f20:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  407f25:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  407f2a:	41 88 f8             	mov    %dil,%r8b
  407f2d:	44 88 c1             	mov    %r8b,%cl
  407f30:	4c 89 ce             	mov    %r9,%rsi
  407f33:	48 d3 e6             	shl    %cl,%rsi
  407f36:	44 88 c1             	mov    %r8b,%cl
  407f39:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  407f3d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  407f42:	41 f6 c0 40          	test   $0x40,%r8b
  407f46:	48 0f 45 d6          	cmovne %rsi,%rdx
  407f4a:	48 0f 45 f1          	cmovne %rcx,%rsi
  407f4e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  407f55:	48 83 d8 00          	sbb    $0x0,%rax
  407f59:	48 89 c8             	mov    %rcx,%rax
  407f5c:	48 0f 42 c6          	cmovb  %rsi,%rax
  407f60:	48 0f 42 ca          	cmovb  %rdx,%rcx
  407f64:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  407f69:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407f6e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  407f75:	00 00 
  407f77:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  407f7b:	c1 e2 14             	shl    $0x14,%edx
  407f7e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  407f82:	25 ff ff 0f 00       	and    $0xfffff,%eax
  407f87:	89 c1                	mov    %eax,%ecx
  407f89:	89 d0                	mov    %edx,%eax
  407f8b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  407f92:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  407f96:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  407f9a:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  407f9e:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  407fa4:	5b                   	pop    %rbx
  407fa5:	c3                   	ret
  407fa6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  407fad:	00 00 00 

0000000000407fb0 <__umodti3>:
  407fb0:	48 83 ec 58          	sub    $0x58,%rsp
  407fb4:	48 89 0c 24          	mov    %rcx,(%rsp)
  407fb8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  407fbd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407fc2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  407fc7:	48 8b 0c 24          	mov    (%rsp),%rcx
  407fcb:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  407fd0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  407fd5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  407fda:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  407fdf:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  407fe4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  407fe9:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  407fee:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  407ff3:	e8 28 9a ff ff       	call   401a20 <runtime::udivmod128>
  407ff8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407ffd:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  408002:	48 83 c4 58          	add    $0x58,%rsp
  408006:	c3                   	ret
  408007:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40800e:	00 00 

0000000000408010 <__udivmodti4>:
  408010:	48 83 ec 58          	sub    $0x58,%rsp
  408014:	4c 89 04 24          	mov    %r8,(%rsp)
  408018:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40801d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  408022:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  408027:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40802c:	4c 8b 04 24          	mov    (%rsp),%r8
  408030:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  408035:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40803a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40803f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  408044:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  408049:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40804e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  408053:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  408058:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40805d:	e8 be 99 ff ff       	call   401a20 <runtime::udivmod128>
  408062:	48 83 c4 58          	add    $0x58,%rsp
  408066:	c3                   	ret
  408067:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40806e:	00 00 

0000000000408070 <__udivti3>:
  408070:	48 83 ec 48          	sub    $0x48,%rsp
  408074:	48 89 0c 24          	mov    %rcx,(%rsp)
  408078:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40807d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  408082:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  408087:	48 8b 0c 24          	mov    (%rsp),%rcx
  40808b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  408090:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  408095:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40809a:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40809f:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4080a4:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4080a9:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4080ae:	31 c0                	xor    %eax,%eax
  4080b0:	41 89 c0             	mov    %eax,%r8d
  4080b3:	e8 58 ff ff ff       	call   408010 <__udivmodti4>
  4080b8:	48 83 c4 48          	add    $0x48,%rsp
  4080bc:	c3                   	ret
  4080bd:	0f 1f 00             	nopl   (%rax)

00000000004080c0 <os::[os_linux.odin]::_alloc_command_line_arguments>:
  4080c0:	48 81 ec c8 01 00 00 	sub    $0x1c8,%rsp
  4080c7:	0f 57 c0             	xorps  %xmm0,%xmm0
  4080ca:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  4080cf:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  4080d6:	00 
  4080d7:	0f 29 84 24 a0 01 00 	movaps %xmm0,0x1a0(%rsp)
  4080de:	00 
  4080df:	0f 29 84 24 90 01 00 	movaps %xmm0,0x190(%rsp)
  4080e6:	00 
  4080e7:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  4080ee:	00 
  4080ef:	0f 29 84 24 70 01 00 	movaps %xmm0,0x170(%rsp)
  4080f6:	00 
  4080f7:	0f 29 84 24 60 01 00 	movaps %xmm0,0x160(%rsp)
  4080fe:	00 
  4080ff:	0f 29 84 24 50 01 00 	movaps %xmm0,0x150(%rsp)
  408106:	00 
  408107:	48 8d bc 24 50 01 00 	lea    0x150(%rsp),%rdi
  40810e:	00 
  40810f:	48 89 7c 24 68       	mov    %rdi,0x68(%rsp)
  408114:	e8 87 d9 ff ff       	call   405aa0 <runtime::[core.odin]::__init_context>
  408119:	0f 28 44 24 50       	movaps 0x50(%rsp),%xmm0
  40811e:	0f 29 84 24 40 01 00 	movaps %xmm0,0x140(%rsp)
  408125:	00 
  408126:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  40812d:	00 
  40812e:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  408135:	00 
  408136:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  40813d:	00 
  40813e:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  408145:	00 
  408146:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  40814d:	00 
  40814e:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  408155:	00 
  408156:	48 8d bc 24 e0 00 00 	lea    0xe0(%rsp),%rdi
  40815d:	00 
  40815e:	e8 ed d8 ff ff       	call   405a50 <runtime::default_context>
  408163:	0f 28 44 24 50       	movaps 0x50(%rsp),%xmm0
  408168:	4c 8b 4c 24 68       	mov    0x68(%rsp),%r9
  40816d:	0f 28 8c 24 e0 00 00 	movaps 0xe0(%rsp),%xmm1
  408174:	00 
  408175:	0f 28 94 24 f0 00 00 	movaps 0xf0(%rsp),%xmm2
  40817c:	00 
  40817d:	0f 28 9c 24 00 01 00 	movaps 0x100(%rsp),%xmm3
  408184:	00 
  408185:	0f 28 a4 24 10 01 00 	movaps 0x110(%rsp),%xmm4
  40818c:	00 
  40818d:	0f 28 ac 24 20 01 00 	movaps 0x120(%rsp),%xmm5
  408194:	00 
  408195:	0f 28 b4 24 30 01 00 	movaps 0x130(%rsp),%xmm6
  40819c:	00 
  40819d:	0f 28 bc 24 40 01 00 	movaps 0x140(%rsp),%xmm7
  4081a4:	00 
  4081a5:	0f 29 bc 24 b0 01 00 	movaps %xmm7,0x1b0(%rsp)
  4081ac:	00 
  4081ad:	0f 29 b4 24 a0 01 00 	movaps %xmm6,0x1a0(%rsp)
  4081b4:	00 
  4081b5:	0f 29 ac 24 90 01 00 	movaps %xmm5,0x190(%rsp)
  4081bc:	00 
  4081bd:	0f 29 a4 24 80 01 00 	movaps %xmm4,0x180(%rsp)
  4081c4:	00 
  4081c5:	0f 29 9c 24 70 01 00 	movaps %xmm3,0x170(%rsp)
  4081cc:	00 
  4081cd:	0f 29 94 24 60 01 00 	movaps %xmm2,0x160(%rsp)
  4081d4:	00 
  4081d5:	0f 29 8c 24 50 01 00 	movaps %xmm1,0x150(%rsp)
  4081dc:	00 
  4081dd:	48 c7 c0 60 c0 40 00 	mov    $0x40c060,%rax
  4081e4:	48 8b 78 08          	mov    0x8(%rax),%rdi
  4081e8:	48 8b 84 24 50 01 00 	mov    0x150(%rsp),%rax
  4081ef:	00 
  4081f0:	48 8b 8c 24 58 01 00 	mov    0x158(%rsp),%rcx
  4081f7:	00 
  4081f8:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  4081ff:	00 
  408200:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  408207:	00 
  408208:	48 8b b4 24 d0 00 00 	mov    0xd0(%rsp),%rsi
  40820f:	00 
  408210:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  408217:	00 
  408218:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  40821f:	00 
  408220:	b9 90 94 40 00       	mov    $0x409490,%ecx
  408225:	4c 8d 84 24 c0 00 00 	lea    0xc0(%rsp),%r8
  40822c:	00 
  40822d:	e8 5e a8 ff ff       	call   402a90 <runtime::make_slice:proc(T:$[]string,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]string,err:runtime::Allocator_Error)>
  408232:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  408239:	00 
  40823a:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  408241:	00 
  408242:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  408249:	00 
  40824a:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  408251:	00 
  408252:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  408259:	00 
  40825a:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  408261:	00 
  408262:	48 c7 84 24 88 00 00 	movq   $0xffffffffffffffff,0x88(%rsp)
  408269:	00 ff ff ff ff 
  40826e:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  408275:	00 
  408276:	48 83 c0 01          	add    $0x1,%rax
  40827a:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  408281:	00 
  408282:	48 3b 84 24 90 00 00 	cmp    0x90(%rsp),%rax
  408289:	00 
  40828a:	0f 8d da 00 00 00    	jge    40836a <os::[os_linux.odin]::_alloc_command_line_arguments+0x2aa>
  408290:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  408297:	00 
  408298:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40829f:	00 
  4082a0:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  4082a7:	00 
  4082a8:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  4082ad:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  4082b4:	00 
  4082b5:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4082ba:	4c 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%r9
  4082c1:	00 
  4082c2:	bf 40 94 40 00       	mov    $0x409440,%edi
  4082c7:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4082cc:	be 23 00 00 00       	mov    $0x23,%esi
  4082d1:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4082d6:	ba 51 04 00 00       	mov    $0x451,%edx
  4082db:	89 54 24 24          	mov    %edx,0x24(%rsp)
  4082df:	b9 07 00 00 00       	mov    $0x7,%ecx
  4082e4:	e8 77 b0 ff ff       	call   403360 <runtime::bounds_check_error>
  4082e9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4082ee:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4082f3:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4082f8:	8b 54 24 24          	mov    0x24(%rsp),%edx
  4082fc:	48 c1 e0 04          	shl    $0x4,%rax
  408300:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  408305:	48 c7 c0 60 c0 40 00 	mov    $0x40c060,%rax
  40830c:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  408313:	00 
  408314:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  408319:	48 8b 08             	mov    (%rax),%rcx
  40831c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  408321:	4c 8b 48 08          	mov    0x8(%rax),%r9
  408325:	b9 22 00 00 00       	mov    $0x22,%ecx
  40832a:	e8 31 b0 ff ff       	call   403360 <runtime::bounds_check_error>
  40832f:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  408334:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  408339:	48 8b 3c c8          	mov    (%rax,%rcx,8),%rdi
  40833d:	e8 7e f0 ff ff       	call   4073c0 <runtime::cstring_to_string>
  408342:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  408347:	48 89 c6             	mov    %rax,%rsi
  40834a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40834f:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  408354:	48 89 d6             	mov    %rdx,%rsi
  408357:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40835c:	48 89 74 08 08       	mov    %rsi,0x8(%rax,%rcx,1)
  408361:	48 89 14 08          	mov    %rdx,(%rax,%rcx,1)
  408365:	e9 04 ff ff ff       	jmp    40826e <os::[os_linux.odin]::_alloc_command_line_arguments+0x1ae>
  40836a:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  408371:	00 
  408372:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  408379:	00 
  40837a:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  40837f:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  408384:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  408389:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  40838e:	48 81 c4 c8 01 00 00 	add    $0x1c8,%rsp
  408395:	c3                   	ret
  408396:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40839d:	00 00 00 

00000000004083a0 <os::[os_linux.odin]::_delete_command_line_arguments>:
  4083a0:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  4083a7:	0f 57 c0             	xorps  %xmm0,%xmm0
  4083aa:	0f 29 04 24          	movaps %xmm0,(%rsp)
  4083ae:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  4083b5:	00 
  4083b6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4083bd:	00 
  4083be:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  4083c5:	00 
  4083c6:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  4083cd:	00 
  4083ce:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  4083d5:	00 
  4083d6:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4083dd:	00 
  4083de:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4083e5:	00 
  4083e6:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4083ed:	00 
  4083ee:	e8 ad d6 ff ff       	call   405aa0 <runtime::[core.odin]::__init_context>
  4083f3:	0f 28 04 24          	movaps (%rsp),%xmm0
  4083f7:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  4083fe:	00 
  4083ff:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  408404:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  408409:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40840e:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  408413:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  408418:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  40841d:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  408422:	e8 29 d6 ff ff       	call   405a50 <runtime::default_context>
  408427:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  40842c:	0f 28 4c 24 30       	movaps 0x30(%rsp),%xmm1
  408431:	0f 28 54 24 40       	movaps 0x40(%rsp),%xmm2
  408436:	0f 28 5c 24 50       	movaps 0x50(%rsp),%xmm3
  40843b:	0f 28 64 24 60       	movaps 0x60(%rsp),%xmm4
  408440:	0f 28 6c 24 70       	movaps 0x70(%rsp),%xmm5
  408445:	0f 28 b4 24 80 00 00 	movaps 0x80(%rsp),%xmm6
  40844c:	00 
  40844d:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  408454:	00 
  408455:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  40845c:	00 
  40845d:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  408464:	00 
  408465:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  40846c:	00 
  40846d:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  408474:	00 
  408475:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  40847c:	00 
  40847d:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  408484:	00 
  408485:	48 c7 c0 70 c0 40 00 	mov    $0x40c070,%rax
  40848c:	48 8b 38             	mov    (%rax),%rdi
  40848f:	48 8b 70 08          	mov    0x8(%rax),%rsi
  408493:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40849a:	00 
  40849b:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4084a2:	00 
  4084a3:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4084a8:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4084ad:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4084b2:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4084b7:	41 b8 e0 94 40 00    	mov    $0x4094e0,%r8d
  4084bd:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  4084c4:	00 
  4084c5:	e8 96 a6 ff ff       	call   402b60 <runtime::delete_slice:proc(array:[]string,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  4084ca:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  4084d1:	c3                   	ret

Disassembly of section .fini:

00000000004084d4 <_fini>:
  4084d4:	f3 0f 1e fa          	endbr64
  4084d8:	48 83 ec 08          	sub    $0x8,%rsp
  4084dc:	48 83 c4 08          	add    $0x8,%rsp
  4084e0:	c3                   	ret
