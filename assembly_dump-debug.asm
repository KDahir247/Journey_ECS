
/home/khalid/Documents/GitHub/Journey_ECS/main-debug.bin:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 c9 1f 01 00 	mov    0x11fc9(%rip),%rax        # 412fd8 <__gmon_start__>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 1f 01 00    	push   0x11fca(%rip)        # 412ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 1f 01 00    	jmp    *0x11fcc(%rip)        # 412ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 1f 01 00    	jmp    *0x11fca(%rip)        # 413000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <memset@plt>:
  401040:	ff 25 c2 1f 01 00    	jmp    *0x11fc2(%rip)        # 413008 <memset@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <calloc@plt>:
  401050:	ff 25 ba 1f 01 00    	jmp    *0x11fba(%rip)        # 413010 <calloc@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <memcpy@plt>:
  401060:	ff 25 b2 1f 01 00    	jmp    *0x11fb2(%rip)        # 413018 <memcpy@GLIBC_2.14>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <malloc@plt>:
  401070:	ff 25 aa 1f 01 00    	jmp    *0x11faa(%rip)        # 413020 <malloc@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <realloc@plt>:
  401080:	ff 25 a2 1f 01 00    	jmp    *0x11fa2(%rip)        # 413028 <realloc@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <memmove@plt>:
  401090:	ff 25 9a 1f 01 00    	jmp    *0x11f9a(%rip)        # 413030 <memmove@GLIBC_2.2.5>
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
  4010b8:	48 c7 c7 90 72 40 00 	mov    $0x407290,%rdi
  4010bf:	ff 15 03 1f 01 00    	call   *0x11f03(%rip)        # 412fc8 <__libc_start_main@GLIBC_2.34>
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
  4010e0:	b8 68 30 41 00       	mov    $0x413068,%eax
  4010e5:	48 3d 68 30 41 00    	cmp    $0x413068,%rax
  4010eb:	74 13                	je     401100 <deregister_tm_clones+0x20>
  4010ed:	48 8b 05 dc 1e 01 00 	mov    0x11edc(%rip),%rax        # 412fd0 <_ITM_deregisterTMCloneTable>
  4010f4:	48 85 c0             	test   %rax,%rax
  4010f7:	74 07                	je     401100 <deregister_tm_clones+0x20>
  4010f9:	bf 68 30 41 00       	mov    $0x413068,%edi
  4010fe:	ff e0                	jmp    *%rax
  401100:	c3                   	ret
  401101:	0f 1f 40 00          	nopl   0x0(%rax)
  401105:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40110c:	00 00 00 00 

0000000000401110 <register_tm_clones>:
  401110:	be 68 30 41 00       	mov    $0x413068,%esi
  401115:	48 81 ee 68 30 41 00 	sub    $0x413068,%rsi
  40111c:	48 89 f0             	mov    %rsi,%rax
  40111f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401123:	48 c1 f8 03          	sar    $0x3,%rax
  401127:	48 01 c6             	add    %rax,%rsi
  40112a:	48 d1 fe             	sar    $1,%rsi
  40112d:	74 19                	je     401148 <register_tm_clones+0x38>
  40112f:	48 8b 05 aa 1e 01 00 	mov    0x11eaa(%rip),%rax        # 412fe0 <_ITM_registerTMCloneTable>
  401136:	48 85 c0             	test   %rax,%rax
  401139:	74 0d                	je     401148 <register_tm_clones+0x38>
  40113b:	bf 68 30 41 00       	mov    $0x413068,%edi
  401140:	ff e0                	jmp    *%rax
  401142:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401148:	c3                   	ret
  401149:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401150 <__do_global_dtors_aux>:
  401150:	f3 0f 1e fa          	endbr64
  401154:	80 3d 0d 1f 01 00 00 	cmpb   $0x0,0x11f0d(%rip)        # 413068 <__TMC_END__>
  40115b:	75 13                	jne    401170 <__do_global_dtors_aux+0x20>
  40115d:	55                   	push   %rbp
  40115e:	48 89 e5             	mov    %rsp,%rbp
  401161:	e8 7a ff ff ff       	call   4010e0 <deregister_tm_clones>
  401166:	c6 05 fb 1e 01 00 01 	movb   $0x1,0x11efb(%rip)        # 413068 <__TMC_END__>
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

0000000000401190 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  401190:	48 83 ec 18          	sub    $0x18,%rsp
  401194:	48 89 3c 24          	mov    %rdi,(%rsp)
  401198:	40 88 f0             	mov    %sil,%al
  40119b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40119f:	48 8b 04 24          	mov    (%rsp),%rax
  4011a3:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  4011a7:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4011ac:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  4011b0:	48 83 f8 00          	cmp    $0x0,%rax
  4011b4:	0f 9e c0             	setle  %al
  4011b7:	24 01                	and    $0x1,%al
  4011b9:	3c 00                	cmp    $0x0,%al
  4011bb:	74 07                	je     4011c4 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  4011bd:	31 c0                	xor    %eax,%eax
  4011bf:	48 83 c4 18          	add    $0x18,%rsp
  4011c3:	c3                   	ret
  4011c4:	8a 44 24 0e          	mov    0xe(%rsp),%al
  4011c8:	3c 00                	cmp    $0x0,%al
  4011ca:	74 13                	je     4011df <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  4011cc:	48 8b 34 24          	mov    (%rsp),%rsi
  4011d0:	bf 01 00 00 00       	mov    $0x1,%edi
  4011d5:	e8 76 fe ff ff       	call   401050 <calloc@plt>
  4011da:	48 83 c4 18          	add    $0x18,%rsp
  4011de:	c3                   	ret
  4011df:	48 8b 3c 24          	mov    (%rsp),%rdi
  4011e3:	e8 88 fe ff ff       	call   401070 <malloc@plt>
  4011e8:	48 83 c4 18          	add    $0x18,%rsp
  4011ec:	c3                   	ret
  4011ed:	0f 1f 00             	nopl   (%rax)

00000000004011f0 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  4011f0:	48 83 ec 28          	sub    $0x28,%rsp
  4011f4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4011f9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4011fe:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401203:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401208:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40120d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  401212:	e8 69 fe ff ff       	call   401080 <realloc@plt>
  401217:	48 83 c4 28          	add    $0x28,%rsp
  40121b:	c3                   	ret
  40121c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401220 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  401220:	48 83 ec 18          	sub    $0x18,%rsp
  401224:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  401229:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40122e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  401233:	e8 f8 fd ff ff       	call   401030 <free@plt>
  401238:	48 83 c4 18          	add    $0x18,%rsp
  40123c:	c3                   	ret
  40123d:	0f 1f 00             	nopl   (%rax)

0000000000401240 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  401240:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  401245:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  40124a:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  40124f:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  401254:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401259:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40125e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  401263:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401268:	48 01 c2             	add    %rax,%rdx
  40126b:	0f 92 c0             	setb   %al
  40126e:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  401273:	24 01                	and    $0x1,%al
  401275:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  401279:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40127e:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  401283:	0f 94 c0             	sete   %al
  401286:	24 01                	and    $0x1,%al
  401288:	48 89 11             	mov    %rdx,(%rcx)
  40128b:	c3                   	ret
  40128c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401290 <runtime::memory_block_alloc>:
  401290:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  401297:	4c 89 4c 24 38       	mov    %r9,0x38(%rsp)
  40129c:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  4012a1:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4012a6:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  4012ab:	48 89 74 24 58       	mov    %rsi,0x58(%rsp)
  4012b0:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  4012b5:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  4012bc:	00 
  4012bd:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4012c2:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  4012c7:	4c 8b 4c 24 68       	mov    0x68(%rsp),%r9
  4012cc:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4012d1:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  4012d6:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4012db:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4012e0:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  4012e7:	00 
  4012e8:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  4012ef:	00 
  4012f0:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4012f7:	00 
  4012f8:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4012fd:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  401304:	00 
  401305:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40130a:	48 89 bc 24 e8 00 00 	mov    %rdi,0xe8(%rsp)
  401311:	00 
  401312:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  401319:	00 
  40131a:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  401321:	00 00 00 00 00 
  401326:	c6 84 24 d7 00 00 00 	movb   $0x0,0xd7(%rsp)
  40132d:	00 
  40132e:	48 89 d6             	mov    %rdx,%rsi
  401331:	48 83 ee 31          	sub    $0x31,%rsi
  401335:	be 30 00 00 00       	mov    $0x30,%esi
  40133a:	48 0f 43 f2          	cmovae %rdx,%rsi
  40133e:	48 01 f7             	add    %rsi,%rdi
  401341:	48 89 bc 24 c8 00 00 	mov    %rdi,0xc8(%rsp)
  401348:	00 
  401349:	48 89 b4 24 c0 00 00 	mov    %rsi,0xc0(%rsp)
  401350:	00 
  401351:	48 89 d6             	mov    %rdx,%rsi
  401354:	48 83 ee 10          	sub    $0x10,%rsi
  401358:	be 10 00 00 00       	mov    $0x10,%esi
  40135d:	48 0f 4c d6          	cmovl  %rsi,%rdx
  401361:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  401368:	00 
  401369:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  401370:	00 
  401371:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  401378:	00 
  401379:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  401380:	00 
  401381:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401388:	00 
  401389:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  401390:	00 
  401391:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  401398:	00 
  401399:	0f 57 c0             	xorps  %xmm0,%xmm0
  40139c:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4013a3:	00 
  4013a4:	48 89 e0             	mov    %rsp,%rax
  4013a7:	4c 89 08             	mov    %r9,(%rax)
  4013aa:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  4013b1:	00 
  4013b2:	e8 c9 b1 00 00       	call   40c580 <runtime::mem_alloc>
  4013b7:	88 44 24 27          	mov    %al,0x27(%rsp)
  4013bb:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  4013c2:	00 
  4013c3:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4013c8:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4013cf:	00 
  4013d0:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4013d5:	3c 00                	cmp    $0x0,%al
  4013d7:	74 39                	je     401412 <runtime::memory_block_alloc+0x182>
  4013d9:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4013de:	8a 44 24 27          	mov    0x27(%rsp),%al
  4013e2:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  4013e9:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  4013f0:	00 
  4013f1:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  4013f8:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  4013ff:	00 
  401400:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  401407:	48 89 11             	mov    %rdx,(%rcx)
  40140a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  401411:	c3                   	ret
  401412:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  401417:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40141c:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401421:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401426:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40142b:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  401432:	00 
  401433:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  40143a:	00 
  40143b:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  401442:	00 
  401443:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  40144a:	00 
  40144b:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  401452:	00 
  401453:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  40145a:	00 
  40145b:	48 01 f0             	add    %rsi,%rax
  40145e:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  401463:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  401468:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40146d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  401474:	00 
  401475:	48 89 50 10          	mov    %rdx,0x10(%rax)
  401479:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40147d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  401484:	00 
  401485:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  40148c:	00 
  40148d:	48 03 8c 24 c0 00 00 	add    0xc0(%rsp),%rcx
  401494:	00 
  401495:	48 89 48 18          	mov    %rcx,0x18(%rax)
  401499:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4014a0:	00 
  4014a1:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4014a6:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  4014ad:	00 
  4014ae:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  4014b2:	48 29 d1             	sub    %rdx,%rcx
  4014b5:	48 89 48 28          	mov    %rcx,0x28(%rax)
  4014b9:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4014c0:	00 
  4014c1:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  4014c6:	0f 94 c0             	sete   %al
  4014c9:	24 01                	and    $0x1,%al
  4014cb:	0f b6 f8             	movzbl %al,%edi
  4014ce:	be 50 e0 40 00       	mov    $0x40e050,%esi
  4014d3:	b9 b0 e0 40 00       	mov    $0x40e0b0,%ecx
  4014d8:	ba 0f 00 00 00       	mov    $0xf,%edx
  4014dd:	e8 ae ac 00 00       	call   40c190 <runtime::assert>
  4014e2:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  4014e7:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4014ee:	00 
  4014ef:	48 83 38 00          	cmpq   $0x0,(%rax)
  4014f3:	0f 94 c0             	sete   %al
  4014f6:	24 01                	and    $0x1,%al
  4014f8:	0f b6 f8             	movzbl %al,%edi
  4014fb:	be d8 e0 40 00       	mov    $0x40e0d8,%esi
  401500:	b9 f0 e0 40 00       	mov    $0x40e0f0,%ecx
  401505:	ba 11 00 00 00       	mov    $0x11,%edx
  40150a:	e8 81 ac 00 00       	call   40c190 <runtime::assert>
  40150f:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  401514:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  40151b:	00 
  40151c:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  401523:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  40152a:	00 
  40152b:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  401532:	48 89 11             	mov    %rdx,(%rcx)
  401535:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  40153c:	c3                   	ret
  40153d:	0f 1f 00             	nopl   (%rax)

0000000000401540 <runtime::memory_block_dealloc>:
  401540:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  401547:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40154c:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  401551:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401556:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40155d:	00 
  40155e:	48 83 f8 00          	cmp    $0x0,%rax
  401562:	0f 95 c0             	setne  %al
  401565:	24 01                	and    $0x1,%al
  401567:	3c 00                	cmp    $0x0,%al
  401569:	0f 84 5f 01 00 00    	je     4016ce <runtime::memory_block_dealloc+0x18e>
  40156f:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  401576:	00 
  401577:	48 8b 41 08          	mov    0x8(%rcx),%rax
  40157b:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  40157f:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  401586:	00 
  401587:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40158e:	00 
  40158f:	0f 57 c0             	xorps  %xmm0,%xmm0
  401592:	0f 29 04 24          	movaps %xmm0,(%rsp)
  401596:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40159d:	00 
  40159e:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  4015a5:	00 
  4015a6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4015ad:	00 
  4015ae:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  4015b5:	00 
  4015b6:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  4015bd:	00 
  4015be:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  4015c5:	00 
  4015c6:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4015cd:	00 
  4015ce:	48 8d bc 24 a0 00 00 	lea    0xa0(%rsp),%rdi
  4015d5:	00 
  4015d6:	e8 e5 9a 00 00       	call   40b0c0 <runtime::[core.odin]::__init_context>
  4015db:	0f 28 04 24          	movaps (%rsp),%xmm0
  4015df:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4015e6:	00 
  4015e7:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  4015ee:	00 
  4015ef:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4015f4:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  4015f9:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  4015fe:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  401603:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401608:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40160d:	e8 5e 9a 00 00       	call   40b070 <runtime::default_context>
  401612:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401617:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40161c:	0f 28 44 24 30       	movaps 0x30(%rsp),%xmm0
  401621:	0f 28 4c 24 40       	movaps 0x40(%rsp),%xmm1
  401626:	0f 28 54 24 50       	movaps 0x50(%rsp),%xmm2
  40162b:	0f 28 5c 24 60       	movaps 0x60(%rsp),%xmm3
  401630:	0f 28 64 24 70       	movaps 0x70(%rsp),%xmm4
  401635:	0f 28 ac 24 80 00 00 	movaps 0x80(%rsp),%xmm5
  40163c:	00 
  40163d:	0f 28 b4 24 90 00 00 	movaps 0x90(%rsp),%xmm6
  401644:	00 
  401645:	0f 29 b4 24 00 01 00 	movaps %xmm6,0x100(%rsp)
  40164c:	00 
  40164d:	0f 29 ac 24 f0 00 00 	movaps %xmm5,0xf0(%rsp)
  401654:	00 
  401655:	0f 29 a4 24 e0 00 00 	movaps %xmm4,0xe0(%rsp)
  40165c:	00 
  40165d:	0f 29 9c 24 d0 00 00 	movaps %xmm3,0xd0(%rsp)
  401664:	00 
  401665:	0f 29 94 24 c0 00 00 	movaps %xmm2,0xc0(%rsp)
  40166c:	00 
  40166d:	0f 29 8c 24 b0 00 00 	movaps %xmm1,0xb0(%rsp)
  401674:	00 
  401675:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40167c:	00 
  40167d:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  401684:	00 
  401685:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  40168c:	00 
  40168d:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  401694:	00 
  401695:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40169c:	00 
  40169d:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  4016a4:	00 
  4016a5:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  4016ac:	00 
  4016ad:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4016b2:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4016b7:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4016bc:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4016c1:	4c 8d 84 24 a0 00 00 	lea    0xa0(%rsp),%r8
  4016c8:	00 
  4016c9:	e8 d2 af 00 00       	call   40c6a0 <runtime::mem_free>
  4016ce:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  4016d5:	c3                   	ret
  4016d6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4016dd:	00 00 00 

00000000004016e0 <runtime::alloc_from_memory_block>:
  4016e0:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  4016e7:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4016ec:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4016f1:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4016f6:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4016fb:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401700:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401705:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40170a:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  401711:	00 
  401712:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  401719:	00 
  40171a:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  401721:	00 
  401722:	48 8d bc 24 80 00 00 	lea    0x80(%rsp),%rdi
  401729:	00 
  40172a:	31 f6                	xor    %esi,%esi
  40172c:	ba 10 00 00 00       	mov    $0x10,%edx
  401731:	e8 0a f9 ff ff       	call   401040 <memset@plt>
  401736:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40173b:	c6 44 24 7f 00       	movb   $0x0,0x7f(%rsp)
  401740:	48 83 f8 00          	cmp    $0x0,%rax
  401744:	0f 94 c0             	sete   %al
  401747:	24 01                	and    $0x1,%al
  401749:	3c 00                	cmp    $0x0,%al
  40174b:	74 34                	je     401781 <runtime::alloc_from_memory_block+0xa1>
  40174d:	48 8d bc 24 80 00 00 	lea    0x80(%rsp),%rdi
  401754:	00 
  401755:	31 f6                	xor    %esi,%esi
  401757:	ba 10 00 00 00       	mov    $0x10,%edx
  40175c:	e8 df f8 ff ff       	call   401040 <memset@plt>
  401761:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  401766:	c6 44 24 7f 01       	movb   $0x1,0x7f(%rsp)
  40176b:	31 f6                	xor    %esi,%esi
  40176d:	ba 10 00 00 00       	mov    $0x10,%edx
  401772:	e8 c9 f8 ff ff       	call   401040 <memset@plt>
  401777:	b0 01                	mov    $0x1,%al
  401779:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  401780:	c3                   	ret
  401781:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  401786:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40178b:	e8 00 11 00 00       	call   402890 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  401790:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401795:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40179a:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  40179f:	48 c7 44 24 68 00 00 	movq   $0x0,0x68(%rsp)
  4017a6:	00 00 
  4017a8:	48 8d 54 24 68       	lea    0x68(%rsp),%rdx
  4017ad:	e8 8e fa ff ff       	call   401240 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  4017b2:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4017b7:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  4017bc:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  4017c0:	80 7c 24 5f 00       	cmpb   $0x0,0x5f(%rsp)
  4017c5:	75 41                	jne    401808 <runtime::alloc_from_memory_block+0x128>
  4017c7:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4017cc:	c6 44 24 7f 01       	movb   $0x1,0x7f(%rsp)
  4017d1:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  4017d8:	00 
  4017d9:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  4017e0:	00 
  4017e1:	8a 44 24 7f          	mov    0x7f(%rsp),%al
  4017e5:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  4017ec:	00 
  4017ed:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  4017f4:	00 
  4017f5:	88 44 24 7f          	mov    %al,0x7f(%rsp)
  4017f9:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4017fd:	48 89 11             	mov    %rdx,(%rcx)
  401800:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  401807:	c3                   	ret
  401808:	eb 00                	jmp    40180a <runtime::alloc_from_memory_block+0x12a>
  40180a:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  401811:	00 
  401812:	48 8b 78 20          	mov    0x20(%rax),%rdi
  401816:	48 8b 74 24 60       	mov    0x60(%rsp),%rsi
  40181b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  401822:	00 00 
  401824:	48 8d 54 24 50       	lea    0x50(%rsp),%rdx
  401829:	e8 12 fa ff ff       	call   401240 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  40182e:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  401833:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  401838:	88 44 24 47          	mov    %al,0x47(%rsp)
  40183c:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  401841:	74 1a                	je     40185d <runtime::alloc_from_memory_block+0x17d>
  401843:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401848:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  40184f:	00 
  401850:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  401854:	0f 97 c0             	seta   %al
  401857:	24 01                	and    $0x1,%al
  401859:	3c 00                	cmp    $0x0,%al
  40185b:	74 41                	je     40189e <runtime::alloc_from_memory_block+0x1be>
  40185d:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401862:	c6 44 24 7f 01       	movb   $0x1,0x7f(%rsp)
  401867:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  40186e:	00 
  40186f:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  401876:	00 
  401877:	8a 44 24 7f          	mov    0x7f(%rsp),%al
  40187b:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  401882:	00 
  401883:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40188a:	00 
  40188b:	88 44 24 7f          	mov    %al,0x7f(%rsp)
  40188f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401893:	48 89 11             	mov    %rdx,(%rcx)
  401896:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  40189d:	c3                   	ret
  40189e:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  4018a3:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4018aa:	00 
  4018ab:	48 8b 41 18          	mov    0x18(%rcx),%rax
  4018af:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  4018b3:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4018b8:	48 01 d1             	add    %rdx,%rcx
  4018bb:	48 01 c8             	add    %rcx,%rax
  4018be:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4018c3:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4018c8:	48 89 04 24          	mov    %rax,(%rsp)
  4018cc:	bf 60 e0 40 00       	mov    $0x40e060,%edi
  4018d1:	31 c0                	xor    %eax,%eax
  4018d3:	41 89 c0             	mov    %eax,%r8d
  4018d6:	be 3c 00 00 00       	mov    $0x3c,%esi
  4018db:	ba 5c 00 00 00       	mov    $0x5c,%edx
  4018e0:	b9 31 00 00 00       	mov    $0x31,%ecx
  4018e5:	e8 06 90 00 00       	call   40a8f0 <runtime::multi_pointer_slice_expr_error>
  4018ea:	48 8b 14 24          	mov    (%rsp),%rdx
  4018ee:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4018f3:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4018f8:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4018fd:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401902:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401907:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40190c:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  401913:	00 
  401914:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40191b:	00 
  40191c:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  401923:	00 
  401924:	48 8b 74 24 60       	mov    0x60(%rsp),%rsi
  401929:	48 8b 50 20          	mov    0x20(%rax),%rdx
  40192d:	48 01 f2             	add    %rsi,%rdx
  401930:	48 89 50 20          	mov    %rdx,0x20(%rax)
  401934:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  40193b:	00 
  40193c:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  401943:	00 
  401944:	8a 44 24 7f          	mov    0x7f(%rsp),%al
  401948:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  40194f:	00 
  401950:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  401957:	00 
  401958:	88 44 24 7f          	mov    %al,0x7f(%rsp)
  40195c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401960:	48 89 11             	mov    %rdx,(%rcx)
  401963:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  40196a:	c3                   	ret
  40196b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401970 <runtime::arena_alloc>:
  401970:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  401977:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40197c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  401981:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  401986:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40198b:	4c 89 44 24 50       	mov    %r8,0x50(%rsp)
  401990:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  401995:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40199a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40199f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4019a4:	48 89 94 24 00 01 00 	mov    %rdx,0x100(%rsp)
  4019ab:	00 
  4019ac:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  4019b3:	00 
  4019b4:	48 89 84 24 f0 00 00 	mov    %rax,0xf0(%rsp)
  4019bb:	00 
  4019bc:	48 8d bc 24 e0 00 00 	lea    0xe0(%rsp),%rdi
  4019c3:	00 
  4019c4:	31 f6                	xor    %esi,%esi
  4019c6:	ba 10 00 00 00       	mov    $0x10,%edx
  4019cb:	e8 70 f6 ff ff       	call   401040 <memset@plt>
  4019d0:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4019d5:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4019da:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  4019df:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  4019e6:	00 
  4019e7:	48 89 c2             	mov    %rax,%rdx
  4019ea:	48 83 ea 01          	sub    $0x1,%rdx
  4019ee:	48 21 d0             	and    %rdx,%rax
  4019f1:	48 83 f8 00          	cmp    $0x0,%rax
  4019f5:	0f 94 c0             	sete   %al
  4019f8:	24 01                	and    $0x1,%al
  4019fa:	0f b6 f8             	movzbl %al,%edi
  4019fd:	be 18 e1 40 00       	mov    $0x40e118,%esi
  401a02:	ba 1a 00 00 00       	mov    $0x1a,%edx
  401a07:	e8 84 a7 00 00       	call   40c190 <runtime::assert>
  401a0c:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  401a11:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  401a18:	00 
  401a19:	48 83 bc 24 d0 00 00 	cmpq   $0x0,0xd0(%rsp)
  401a20:	00 00 
  401a22:	0f 94 c0             	sete   %al
  401a25:	24 01                	and    $0x1,%al
  401a27:	3c 00                	cmp    $0x0,%al
  401a29:	74 42                	je     401a6d <runtime::arena_alloc+0xfd>
  401a2b:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  401a30:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  401a37:	00 
  401a38:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  401a3f:	00 
  401a40:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  401a47:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  401a4e:	00 
  401a4f:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  401a56:	00 
  401a57:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401a5e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401a62:	48 89 11             	mov    %rdx,(%rcx)
  401a65:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  401a6c:	c3                   	ret
  401a6d:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401a74:	00 
  401a75:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  401a7a:	0f 94 c0             	sete   %al
  401a7d:	24 01                	and    $0x1,%al
  401a7f:	3c 00                	cmp    $0x0,%al
  401a81:	74 09                	je     401a8c <runtime::arena_alloc+0x11c>
  401a83:	31 c0                	xor    %eax,%eax
  401a85:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401a8a:	eb 15                	jmp    401aa1 <runtime::arena_alloc+0x131>
  401a8c:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401a93:	00 
  401a94:	48 8b 40 10          	mov    0x10(%rax),%rax
  401a98:	48 8b 40 20          	mov    0x20(%rax),%rax
  401a9c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401aa1:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  401aa6:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401aab:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401ab0:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  401ab7:	00 
  401ab8:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401abf:	00 
  401ac0:	48 8b 78 10          	mov    0x10(%rax),%rdi
  401ac4:	48 8b b4 24 d0 00 00 	mov    0xd0(%rsp),%rsi
  401acb:	00 
  401acc:	0f 57 c0             	xorps  %xmm0,%xmm0
  401acf:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  401ad6:	00 
  401ad7:	48 8d 8c 24 b0 00 00 	lea    0xb0(%rsp),%rcx
  401ade:	00 
  401adf:	e8 fc fb ff ff       	call   4016e0 <runtime::alloc_from_memory_block>
  401ae4:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  401aeb:	00 
  401aec:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  401af3:	00 
  401af4:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  401afb:	00 
  401afc:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  401b03:	00 
  401b04:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401b0b:	80 bc 24 df 00 00 00 	cmpb   $0x1,0xdf(%rsp)
  401b12:	01 
  401b13:	0f 94 c0             	sete   %al
  401b16:	24 01                	and    $0x1,%al
  401b18:	3c 00                	cmp    $0x0,%al
  401b1a:	0f 84 24 02 00 00    	je     401d44 <runtime::arena_alloc+0x3d4>
  401b20:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401b27:	00 
  401b28:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  401b2d:	0f 94 c0             	sete   %al
  401b30:	24 01                	and    $0x1,%al
  401b32:	3c 00                	cmp    $0x0,%al
  401b34:	74 10                	je     401b46 <runtime::arena_alloc+0x1d6>
  401b36:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401b3d:	00 
  401b3e:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  401b45:	00 
  401b46:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  401b4b:	48 8b bc 24 d0 00 00 	mov    0xd0(%rsp),%rdi
  401b52:	00 
  401b53:	e8 c8 0d 00 00       	call   402920 <runtime::arena_alloc.align_forward_uint-0>
  401b58:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401b5f:	00 
  401b60:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  401b67:	00 
  401b68:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401b6f:	00 
  401b70:	48 8b 40 28          	mov    0x28(%rax),%rax
  401b74:	48 39 c1             	cmp    %rax,%rcx
  401b77:	48 0f 47 c1          	cmova  %rcx,%rax
  401b7b:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401b82:	00 
  401b83:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401b8a:	00 
  401b8b:	48 83 38 00          	cmpq   $0x0,(%rax)
  401b8f:	0f 94 c0             	sete   %al
  401b92:	24 01                	and    $0x1,%al
  401b94:	3c 00                	cmp    $0x0,%al
  401b96:	74 46                	je     401bde <runtime::arena_alloc+0x26e>
  401b98:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  401b9d:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401ba4:	00 
  401ba5:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401baa:	e8 41 97 00 00       	call   40b2f0 <runtime::heap_allocator>
  401baf:	48 89 c1             	mov    %rax,%rcx
  401bb2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401bb7:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  401bbe:	00 
  401bbf:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  401bc6:	00 
  401bc7:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  401bce:	00 
  401bcf:	48 8b 94 24 98 00 00 	mov    0x98(%rsp),%rdx
  401bd6:	00 
  401bd7:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401bdb:	48 89 08             	mov    %rcx,(%rax)
  401bde:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  401be3:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401be8:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  401bed:	48 8b 94 24 00 01 00 	mov    0x100(%rsp),%rdx
  401bf4:	00 
  401bf5:	48 8b 02             	mov    (%rdx),%rax
  401bf8:	48 8b 72 08          	mov    0x8(%rdx),%rsi
  401bfc:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  401c03:	00 
  401c04:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  401c0b:	00 
  401c0c:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  401c13:	00 
  401c14:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  401c1b:	00 
  401c1c:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  401c23:	00 
  401c24:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  401c2b:	00 00 
  401c2d:	48 89 e0             	mov    %rsp,%rax
  401c30:	4c 89 08             	mov    %r9,(%rax)
  401c33:	4c 8d 4c 24 78       	lea    0x78(%rsp),%r9
  401c38:	e8 53 f6 ff ff       	call   401290 <runtime::memory_block_alloc>
  401c3d:	88 44 24 17          	mov    %al,0x17(%rsp)
  401c41:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  401c46:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  401c4b:	3c 00                	cmp    $0x0,%al
  401c4d:	74 4d                	je     401c9c <runtime::arena_alloc+0x32c>
  401c4f:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  401c54:	8a 44 24 17          	mov    0x17(%rsp),%al
  401c58:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401c5f:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  401c66:	00 
  401c67:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  401c6e:	00 
  401c6f:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  401c76:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  401c7d:	00 
  401c7e:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  401c85:	00 
  401c86:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401c8d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401c91:	48 89 11             	mov    %rdx,(%rcx)
  401c94:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  401c9b:	c3                   	ret
  401c9c:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  401ca1:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401ca6:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401cab:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  401cb0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  401cb5:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  401cbc:	00 
  401cbd:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  401cc1:	48 89 08             	mov    %rcx,(%rax)
  401cc4:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401ccb:	00 
  401ccc:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  401cd1:	48 89 48 10          	mov    %rcx,0x10(%rax)
  401cd5:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401cdc:	00 
  401cdd:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  401ce2:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  401ce6:	48 8b 48 20          	mov    0x20(%rax),%rcx
  401cea:	48 01 f1             	add    %rsi,%rcx
  401ced:	48 89 48 20          	mov    %rcx,0x20(%rax)
  401cf1:	48 c7 84 24 c8 00 00 	movq   $0x0,0xc8(%rsp)
  401cf8:	00 00 00 00 00 
  401cfd:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401d04:	00 
  401d05:	48 8b 78 10          	mov    0x10(%rax),%rdi
  401d09:	48 8b b4 24 d0 00 00 	mov    0xd0(%rsp),%rsi
  401d10:	00 
  401d11:	0f 57 c0             	xorps  %xmm0,%xmm0
  401d14:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  401d19:	48 8d 4c 24 60       	lea    0x60(%rsp),%rcx
  401d1e:	e8 bd f9 ff ff       	call   4016e0 <runtime::alloc_from_memory_block>
  401d23:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401d28:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401d2d:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  401d34:	00 
  401d35:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  401d3c:	00 
  401d3d:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401d44:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  401d49:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401d50:	00 
  401d51:	48 8b 70 10          	mov    0x10(%rax),%rsi
  401d55:	48 8b 50 18          	mov    0x18(%rax),%rdx
  401d59:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  401d5d:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  401d64:	00 
  401d65:	48 29 fe             	sub    %rdi,%rsi
  401d68:	48 01 f2             	add    %rsi,%rdx
  401d6b:	48 89 50 18          	mov    %rdx,0x18(%rax)
  401d6f:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  401d76:	00 
  401d77:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  401d7e:	00 
  401d7f:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  401d86:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  401d8d:	00 
  401d8e:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  401d95:	00 
  401d96:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401d9d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401da1:	48 89 11             	mov    %rdx,(%rcx)
  401da4:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  401dab:	c3                   	ret
  401dac:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401db0 <runtime::arena_free_last_memory_block>:
  401db0:	48 83 ec 28          	sub    $0x28,%rsp
  401db4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  401db9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  401dbe:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401dc3:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401dc8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401dcd:	48 8b 40 10          	mov    0x10(%rax),%rax
  401dd1:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  401dd6:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  401ddc:	0f 95 c0             	setne  %al
  401ddf:	24 01                	and    $0x1,%al
  401de1:	3c 00                	cmp    $0x0,%al
  401de3:	74 39                	je     401e1e <runtime::arena_free_last_memory_block+0x6e>
  401de5:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401dea:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401def:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  401df4:	48 8b 09             	mov    (%rcx),%rcx
  401df7:	48 89 48 10          	mov    %rcx,0x10(%rax)
  401dfb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401e00:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  401e05:	48 8b 51 28          	mov    0x28(%rcx),%rdx
  401e09:	48 8b 48 20          	mov    0x20(%rax),%rcx
  401e0d:	48 29 d1             	sub    %rdx,%rcx
  401e10:	48 89 48 20          	mov    %rcx,0x20(%rax)
  401e14:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401e19:	e8 22 f7 ff ff       	call   401540 <runtime::memory_block_dealloc>
  401e1e:	48 83 c4 28          	add    $0x28,%rsp
  401e22:	c3                   	ret
  401e23:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  401e2a:	84 00 00 00 00 00 

0000000000401e30 <runtime::arena_free_all>:
  401e30:	48 83 ec 28          	sub    $0x28,%rsp
  401e34:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  401e39:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  401e3e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  401e43:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401e48:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401e4d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401e52:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  401e57:	0f 95 c0             	setne  %al
  401e5a:	24 01                	and    $0x1,%al
  401e5c:	3c 00                	cmp    $0x0,%al
  401e5e:	74 2c                	je     401e8c <runtime::arena_free_all+0x5c>
  401e60:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401e65:	48 8b 40 10          	mov    0x10(%rax),%rax
  401e69:	48 83 38 00          	cmpq   $0x0,(%rax)
  401e6d:	0f 95 c0             	setne  %al
  401e70:	24 01                	and    $0x1,%al
  401e72:	3c 00                	cmp    $0x0,%al
  401e74:	74 16                	je     401e8c <runtime::arena_free_all+0x5c>
  401e76:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401e7b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401e80:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401e85:	e8 26 ff ff ff       	call   401db0 <runtime::arena_free_last_memory_block>
  401e8a:	eb c1                	jmp    401e4d <runtime::arena_free_all+0x1d>
  401e8c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401e91:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  401e96:	0f 95 c0             	setne  %al
  401e99:	24 01                	and    $0x1,%al
  401e9b:	3c 00                	cmp    $0x0,%al
  401e9d:	74 32                	je     401ed1 <runtime::arena_free_all+0xa1>
  401e9f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401ea4:	48 8b 40 10          	mov    0x10(%rax),%rax
  401ea8:	48 8b 78 18          	mov    0x18(%rax),%rdi
  401eac:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401eb1:	48 8b 40 10          	mov    0x10(%rax),%rax
  401eb5:	48 8b 50 20          	mov    0x20(%rax),%rdx
  401eb9:	31 f6                	xor    %esi,%esi
  401ebb:	e8 80 f1 ff ff       	call   401040 <memset@plt>
  401ec0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401ec5:	48 8b 40 10          	mov    0x10(%rax),%rax
  401ec9:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  401ed0:	00 
  401ed1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401ed6:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  401edd:	00 
  401ede:	48 83 c4 28          	add    $0x28,%rsp
  401ee2:	c3                   	ret
  401ee3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  401eea:	84 00 00 00 00 00 

0000000000401ef0 <runtime::arena_destroy>:
  401ef0:	48 83 ec 28          	sub    $0x28,%rsp
  401ef4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  401ef9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  401efe:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401f03:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401f08:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401f0d:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  401f12:	0f 95 c0             	setne  %al
  401f15:	24 01                	and    $0x1,%al
  401f17:	3c 00                	cmp    $0x0,%al
  401f19:	74 49                	je     401f64 <runtime::arena_destroy+0x74>
  401f1b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401f20:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401f25:	48 8b 40 10          	mov    0x10(%rax),%rax
  401f29:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  401f2e:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401f33:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  401f38:	48 8b 09             	mov    (%rcx),%rcx
  401f3b:	48 89 48 10          	mov    %rcx,0x10(%rax)
  401f3f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401f44:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  401f49:	48 8b 51 28          	mov    0x28(%rcx),%rdx
  401f4d:	48 8b 48 20          	mov    0x20(%rax),%rcx
  401f51:	48 29 d1             	sub    %rdx,%rcx
  401f54:	48 89 48 20          	mov    %rcx,0x20(%rax)
  401f58:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401f5d:	e8 de f5 ff ff       	call   401540 <runtime::memory_block_dealloc>
  401f62:	eb a4                	jmp    401f08 <runtime::arena_destroy+0x18>
  401f64:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401f69:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  401f70:	00 
  401f71:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401f76:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  401f7d:	00 
  401f7e:	48 83 c4 28          	add    $0x28,%rsp
  401f82:	c3                   	ret
  401f83:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  401f8a:	84 00 00 00 00 00 

0000000000401f90 <runtime::arena_allocator_proc>:
  401f90:	48 81 ec f8 01 00 00 	sub    $0x1f8,%rsp
  401f97:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  401f9e:	00 
  401f9f:	4c 89 84 24 88 00 00 	mov    %r8,0x88(%rsp)
  401fa6:	00 
  401fa7:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  401fae:	00 
  401faf:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  401fb6:	00 
  401fb7:	48 89 bc 24 a0 00 00 	mov    %rdi,0xa0(%rsp)
  401fbe:	00 
  401fbf:	40 88 f0             	mov    %sil,%al
  401fc2:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  401fc9:	48 8b 84 24 10 02 00 	mov    0x210(%rsp),%rax
  401fd0:	00 
  401fd1:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  401fd8:	00 
  401fd9:	48 8b 84 24 08 02 00 	mov    0x208(%rsp),%rax
  401fe0:	00 
  401fe1:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  401fe8:	00 
  401fe9:	48 8b 84 24 00 02 00 	mov    0x200(%rsp),%rax
  401ff0:	00 
  401ff1:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  401ff8:	00 
  401ff9:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  402000:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  402007:	00 
  402008:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40200f:	00 
  402010:	48 8b b4 24 98 00 00 	mov    0x98(%rsp),%rsi
  402017:	00 
  402018:	48 8b bc 24 a0 00 00 	mov    0xa0(%rsp),%rdi
  40201f:	00 
  402020:	4c 8b 84 24 88 00 00 	mov    0x88(%rsp),%r8
  402027:	00 
  402028:	48 89 bc 24 f0 01 00 	mov    %rdi,0x1f0(%rsp)
  40202f:	00 
  402030:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  402037:	48 89 b4 24 e0 01 00 	mov    %rsi,0x1e0(%rsp)
  40203e:	00 
  40203f:	48 89 94 24 d8 01 00 	mov    %rdx,0x1d8(%rsp)
  402046:	00 
  402047:	4c 89 84 24 d0 01 00 	mov    %r8,0x1d0(%rsp)
  40204e:	00 
  40204f:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  402056:	00 
  402057:	0f 57 c0             	xorps  %xmm0,%xmm0
  40205a:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  402061:	00 
  402062:	c6 84 24 af 01 00 00 	movb   $0x0,0x1af(%rsp)
  402069:	00 
  40206a:	48 89 bc 24 a0 01 00 	mov    %rdi,0x1a0(%rsp)
  402071:	00 
  402072:	48 89 b4 24 98 01 00 	mov    %rsi,0x198(%rsp)
  402079:	00 
  40207a:	48 89 94 24 90 01 00 	mov    %rdx,0x190(%rsp)
  402081:	00 
  402082:	48 89 8c 24 88 01 00 	mov    %rcx,0x188(%rsp)
  402089:	00 
  40208a:	0f b6 c8             	movzbl %al,%ecx
  40208d:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  402092:	2c 07                	sub    $0x7,%al
  402094:	0f 87 b0 07 00 00    	ja     40284a <runtime::arena_allocator_proc+0x8ba>
  40209a:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40209f:	48 8b 04 c5 10 e0 40 	mov    0x40e010(,%rax,8),%rax
  4020a6:	00 
  4020a7:	ff e0                	jmp    *%rax
  4020a9:	4c 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%r9
  4020b0:	00 
  4020b1:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4020b8:	00 
  4020b9:	48 8b bc 24 a0 01 00 	mov    0x1a0(%rsp),%rdi
  4020c0:	00 
  4020c1:	48 8b b4 24 98 01 00 	mov    0x198(%rsp),%rsi
  4020c8:	00 
  4020c9:	48 8b 94 24 90 01 00 	mov    0x190(%rsp),%rdx
  4020d0:	00 
  4020d1:	0f 57 c0             	xorps  %xmm0,%xmm0
  4020d4:	0f 29 84 24 70 01 00 	movaps %xmm0,0x170(%rsp)
  4020db:	00 
  4020dc:	4c 8d 84 24 70 01 00 	lea    0x170(%rsp),%r8
  4020e3:	00 
  4020e4:	e8 87 f8 ff ff       	call   401970 <runtime::arena_alloc>
  4020e9:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4020f0:	00 
  4020f1:	40 88 c7             	mov    %al,%dil
  4020f4:	40 88 f8             	mov    %dil,%al
  4020f7:	48 8b 94 24 70 01 00 	mov    0x170(%rsp),%rdx
  4020fe:	00 
  4020ff:	48 8b b4 24 78 01 00 	mov    0x178(%rsp),%rsi
  402106:	00 
  402107:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  40210e:	00 
  40210f:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  402116:	00 
  402117:	40 88 bc 24 af 01 00 	mov    %dil,0x1af(%rsp)
  40211e:	00 
  40211f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402123:	48 89 11             	mov    %rdx,(%rcx)
  402126:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  40212d:	c3                   	ret
  40212e:	c6 84 24 af 01 00 00 	movb   $0x4,0x1af(%rsp)
  402135:	04 
  402136:	e9 0f 07 00 00       	jmp    40284a <runtime::arena_allocator_proc+0x8ba>
  40213b:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  402142:	00 
  402143:	48 8b b4 24 c0 00 00 	mov    0xc0(%rsp),%rsi
  40214a:	00 
  40214b:	48 8b bc 24 a0 01 00 	mov    0x1a0(%rsp),%rdi
  402152:	00 
  402153:	e8 d8 fc ff ff       	call   401e30 <runtime::arena_free_all>
  402158:	e9 ed 06 00 00       	jmp    40284a <runtime::arena_allocator_proc+0x8ba>
  40215d:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  402164:	00 
  402165:	48 89 84 24 68 01 00 	mov    %rax,0x168(%rsp)
  40216c:	00 
  40216d:	48 83 bc 24 68 01 00 	cmpq   $0x0,0x168(%rsp)
  402174:	00 00 
  402176:	0f 94 c1             	sete   %cl
  402179:	80 e1 01             	and    $0x1,%cl
  40217c:	b0 01                	mov    $0x1,%al
  40217e:	38 c8                	cmp    %cl,%al
  402180:	74 25                	je     4021a7 <runtime::arena_allocator_proc+0x217>
  402182:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  402189:	00 
  40218a:	48 3b 84 24 88 01 00 	cmp    0x188(%rsp),%rax
  402191:	00 
  402192:	0f 94 c1             	sete   %cl
  402195:	80 e1 01             	and    $0x1,%cl
  402198:	b0 01                	mov    $0x1,%al
  40219a:	38 c8                	cmp    %cl,%al
  40219c:	0f 84 a8 00 00 00    	je     40224a <runtime::arena_allocator_proc+0x2ba>
  4021a2:	e9 85 00 00 00       	jmp    40222c <runtime::arena_allocator_proc+0x29c>
  4021a7:	4c 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%r9
  4021ae:	00 
  4021af:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4021b6:	00 
  4021b7:	48 8b bc 24 a0 01 00 	mov    0x1a0(%rsp),%rdi
  4021be:	00 
  4021bf:	48 8b b4 24 98 01 00 	mov    0x198(%rsp),%rsi
  4021c6:	00 
  4021c7:	48 8b 94 24 90 01 00 	mov    0x190(%rsp),%rdx
  4021ce:	00 
  4021cf:	0f 57 c0             	xorps  %xmm0,%xmm0
  4021d2:	0f 29 84 24 50 01 00 	movaps %xmm0,0x150(%rsp)
  4021d9:	00 
  4021da:	4c 8d 84 24 50 01 00 	lea    0x150(%rsp),%r8
  4021e1:	00 
  4021e2:	e8 89 f7 ff ff       	call   401970 <runtime::arena_alloc>
  4021e7:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4021ee:	00 
  4021ef:	40 88 c7             	mov    %al,%dil
  4021f2:	40 88 f8             	mov    %dil,%al
  4021f5:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  4021fc:	00 
  4021fd:	48 8b b4 24 58 01 00 	mov    0x158(%rsp),%rsi
  402204:	00 
  402205:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  40220c:	00 
  40220d:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  402214:	00 
  402215:	40 88 bc 24 af 01 00 	mov    %dil,0x1af(%rsp)
  40221c:	00 
  40221d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402221:	48 89 11             	mov    %rdx,(%rcx)
  402224:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  40222b:	c3                   	ret
  40222c:	48 83 bc 24 98 01 00 	cmpq   $0x0,0x198(%rsp)
  402233:	00 00 
  402235:	0f 94 c1             	sete   %cl
  402238:	80 e1 01             	and    $0x1,%cl
  40223b:	b0 01                	mov    $0x1,%al
  40223d:	38 c8                	cmp    %cl,%al
  40223f:	0f 84 e5 00 00 00    	je     40232a <runtime::arena_allocator_proc+0x39a>
  402245:	e9 b7 00 00 00       	jmp    402301 <runtime::arena_allocator_proc+0x371>
  40224a:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  402251:	00 
  402252:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  402257:	4c 8b 8c 24 98 01 00 	mov    0x198(%rsp),%r9
  40225e:	00 
  40225f:	4c 89 4c 24 70       	mov    %r9,0x70(%rsp)
  402264:	bf 60 e0 40 00       	mov    $0x40e060,%edi
  402269:	31 c0                	xor    %eax,%eax
  40226b:	41 89 c0             	mov    %eax,%r8d
  40226e:	be 3c 00 00 00       	mov    $0x3c,%esi
  402273:	ba db 00 00 00       	mov    $0xdb,%edx
  402278:	b9 13 00 00 00       	mov    $0x13,%ecx
  40227d:	e8 6e 86 00 00       	call   40a8f0 <runtime::multi_pointer_slice_expr_error>
  402282:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  402287:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40228c:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  402293:	00 
  402294:	48 89 94 24 40 01 00 	mov    %rdx,0x140(%rsp)
  40229b:	00 
  40229c:	48 89 84 24 48 01 00 	mov    %rax,0x148(%rsp)
  4022a3:	00 
  4022a4:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4022ab:	00 
  4022ac:	48 8b 94 24 48 01 00 	mov    0x148(%rsp),%rdx
  4022b3:	00 
  4022b4:	48 89 94 24 b8 01 00 	mov    %rdx,0x1b8(%rsp)
  4022bb:	00 
  4022bc:	48 89 84 24 b0 01 00 	mov    %rax,0x1b0(%rsp)
  4022c3:	00 
  4022c4:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4022cb:	00 
  4022cc:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4022d3:	00 
  4022d4:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  4022db:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  4022e2:	00 
  4022e3:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  4022ea:	00 
  4022eb:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  4022f2:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4022f6:	48 89 11             	mov    %rdx,(%rcx)
  4022f9:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  402300:	c3                   	ret
  402301:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  402308:	00 
  402309:	48 8b 8c 24 90 01 00 	mov    0x190(%rsp),%rcx
  402310:	00 
  402311:	48 83 e9 01          	sub    $0x1,%rcx
  402315:	48 21 c8             	and    %rcx,%rax
  402318:	48 83 f8 00          	cmp    $0x0,%rax
  40231c:	0f 94 c1             	sete   %cl
  40231f:	80 e1 01             	and    $0x1,%cl
  402322:	b0 01                	mov    $0x1,%al
  402324:	38 c8                	cmp    %cl,%al
  402326:	74 54                	je     40237c <runtime::arena_allocator_proc+0x3ec>
  402328:	eb 4d                	jmp    402377 <runtime::arena_allocator_proc+0x3e7>
  40232a:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  402331:	00 
  402332:	c6 84 24 af 01 00 00 	movb   $0x4,0x1af(%rsp)
  402339:	04 
  40233a:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  402341:	00 
  402342:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  402349:	00 
  40234a:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  402351:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  402358:	00 
  402359:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  402360:	00 
  402361:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  402368:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40236c:	48 89 11             	mov    %rdx,(%rcx)
  40236f:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  402376:	c3                   	ret
  402377:	e9 a8 02 00 00       	jmp    402624 <runtime::arena_allocator_proc+0x694>
  40237c:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  402383:	00 
  402384:	48 3b 84 24 88 01 00 	cmp    0x188(%rsp),%rax
  40238b:	00 
  40238c:	0f 92 c0             	setb   %al
  40238f:	24 01                	and    $0x1,%al
  402391:	3c 00                	cmp    $0x0,%al
  402393:	0f 84 b7 00 00 00    	je     402450 <runtime::arena_allocator_proc+0x4c0>
  402399:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  4023a0:	00 
  4023a1:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4023a6:	4c 8b 8c 24 98 01 00 	mov    0x198(%rsp),%r9
  4023ad:	00 
  4023ae:	4c 89 4c 24 60       	mov    %r9,0x60(%rsp)
  4023b3:	bf 60 e0 40 00       	mov    $0x40e060,%edi
  4023b8:	31 c0                	xor    %eax,%eax
  4023ba:	41 89 c0             	mov    %eax,%r8d
  4023bd:	be 3c 00 00 00       	mov    $0x3c,%esi
  4023c2:	ba e3 00 00 00       	mov    $0xe3,%edx
  4023c7:	b9 14 00 00 00       	mov    $0x14,%ecx
  4023cc:	e8 1f 85 00 00       	call   40a8f0 <runtime::multi_pointer_slice_expr_error>
  4023d1:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  4023d6:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4023db:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4023e2:	00 
  4023e3:	48 89 94 24 30 01 00 	mov    %rdx,0x130(%rsp)
  4023ea:	00 
  4023eb:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  4023f2:	00 
  4023f3:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4023fa:	00 
  4023fb:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  402402:	00 
  402403:	48 89 94 24 b8 01 00 	mov    %rdx,0x1b8(%rsp)
  40240a:	00 
  40240b:	48 89 84 24 b0 01 00 	mov    %rax,0x1b0(%rsp)
  402412:	00 
  402413:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  40241a:	00 
  40241b:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  402422:	00 
  402423:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  40242a:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  402431:	00 
  402432:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  402439:	00 
  40243a:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  402441:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402445:	48 89 11             	mov    %rdx,(%rcx)
  402448:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  40244f:	c3                   	ret
  402450:	eb 00                	jmp    402452 <runtime::arena_allocator_proc+0x4c2>
  402452:	48 8b 84 24 a0 01 00 	mov    0x1a0(%rsp),%rax
  402459:	00 
  40245a:	48 8b 40 10          	mov    0x10(%rax),%rax
  40245e:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  402465:	00 
  402466:	48 83 bc 24 28 01 00 	cmpq   $0x0,0x128(%rsp)
  40246d:	00 00 
  40246f:	0f 95 c0             	setne  %al
  402472:	24 01                	and    $0x1,%al
  402474:	3c 00                	cmp    $0x0,%al
  402476:	0f 84 a6 01 00 00    	je     402622 <runtime::arena_allocator_proc+0x692>
  40247c:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  402483:	00 
  402484:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  40248b:	00 
  40248c:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  402490:	48 29 c8             	sub    %rcx,%rax
  402493:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40249a:	00 
  40249b:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4024a2:	00 
  4024a3:	48 03 84 24 88 01 00 	add    0x188(%rsp),%rax
  4024aa:	00 
  4024ab:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  4024b2:	00 
  4024b3:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4024ba:	00 
  4024bb:	48 03 84 24 98 01 00 	add    0x198(%rsp),%rax
  4024c2:	00 
  4024c3:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4024ca:	00 
  4024cb:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4024d2:	00 
  4024d3:	48 3b 84 24 18 01 00 	cmp    0x118(%rsp),%rax
  4024da:	00 
  4024db:	0f 92 c0             	setb   %al
  4024de:	24 01                	and    $0x1,%al
  4024e0:	3c 00                	cmp    $0x0,%al
  4024e2:	0f 84 38 01 00 00    	je     402620 <runtime::arena_allocator_proc+0x690>
  4024e8:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  4024ef:	00 
  4024f0:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  4024f7:	00 
  4024f8:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  4024fc:	0f 94 c0             	sete   %al
  4024ff:	24 01                	and    $0x1,%al
  402501:	3c 00                	cmp    $0x0,%al
  402503:	0f 84 17 01 00 00    	je     402620 <runtime::arena_allocator_proc+0x690>
  402509:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  402510:	00 
  402511:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  402518:	00 
  402519:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  40251d:	0f 96 c0             	setbe  %al
  402520:	24 01                	and    $0x1,%al
  402522:	3c 00                	cmp    $0x0,%al
  402524:	0f 84 f6 00 00 00    	je     402620 <runtime::arena_allocator_proc+0x690>
  40252a:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  402531:	00 
  402532:	48 8b 8c 24 10 01 00 	mov    0x110(%rsp),%rcx
  402539:	00 
  40253a:	48 89 48 20          	mov    %rcx,0x20(%rax)
  40253e:	48 8b 84 24 a0 01 00 	mov    0x1a0(%rsp),%rax
  402545:	00 
  402546:	48 8b 8c 24 10 01 00 	mov    0x110(%rsp),%rcx
  40254d:	00 
  40254e:	48 89 48 18          	mov    %rcx,0x18(%rax)
  402552:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  402559:	00 
  40255a:	48 8b 40 18          	mov    0x18(%rax),%rax
  40255e:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402563:	4c 8b 84 24 20 01 00 	mov    0x120(%rsp),%r8
  40256a:	00 
  40256b:	4c 89 44 24 50       	mov    %r8,0x50(%rsp)
  402570:	4c 8b 8c 24 10 01 00 	mov    0x110(%rsp),%r9
  402577:	00 
  402578:	4c 89 4c 24 48       	mov    %r9,0x48(%rsp)
  40257d:	bf 60 e0 40 00       	mov    $0x40e060,%edi
  402582:	be 3c 00 00 00       	mov    $0x3c,%esi
  402587:	ba ef 00 00 00       	mov    $0xef,%edx
  40258c:	b9 17 00 00 00       	mov    $0x17,%ecx
  402591:	e8 5a 83 00 00       	call   40a8f0 <runtime::multi_pointer_slice_expr_error>
  402596:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40259b:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4025a0:	48 8b 74 24 50       	mov    0x50(%rsp),%rsi
  4025a5:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4025ac:	00 
  4025ad:	48 01 f2             	add    %rsi,%rdx
  4025b0:	48 29 f0             	sub    %rsi,%rax
  4025b3:	48 89 94 24 00 01 00 	mov    %rdx,0x100(%rsp)
  4025ba:	00 
  4025bb:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  4025c2:	00 
  4025c3:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  4025ca:	00 
  4025cb:	48 8b 94 24 08 01 00 	mov    0x108(%rsp),%rdx
  4025d2:	00 
  4025d3:	48 89 94 24 b8 01 00 	mov    %rdx,0x1b8(%rsp)
  4025da:	00 
  4025db:	48 89 84 24 b0 01 00 	mov    %rax,0x1b0(%rsp)
  4025e2:	00 
  4025e3:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4025ea:	00 
  4025eb:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4025f2:	00 
  4025f3:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  4025fa:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  402601:	00 
  402602:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  402609:	00 
  40260a:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  402611:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402615:	48 89 11             	mov    %rdx,(%rcx)
  402618:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  40261f:	c3                   	ret
  402620:	eb 00                	jmp    402622 <runtime::arena_allocator_proc+0x692>
  402622:	eb 00                	jmp    402624 <runtime::arena_allocator_proc+0x694>
  402624:	4c 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%r9
  40262b:	00 
  40262c:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402633:	00 
  402634:	48 8b bc 24 a0 01 00 	mov    0x1a0(%rsp),%rdi
  40263b:	00 
  40263c:	48 8b b4 24 98 01 00 	mov    0x198(%rsp),%rsi
  402643:	00 
  402644:	48 8b 94 24 90 01 00 	mov    0x190(%rsp),%rdx
  40264b:	00 
  40264c:	0f 57 c0             	xorps  %xmm0,%xmm0
  40264f:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  402656:	00 
  402657:	4c 8d 84 24 f0 00 00 	lea    0xf0(%rsp),%r8
  40265e:	00 
  40265f:	e8 0c f3 ff ff       	call   401970 <runtime::arena_alloc>
  402664:	88 44 24 2f          	mov    %al,0x2f(%rsp)
  402668:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  40266f:	00 
  402670:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402675:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  40267c:	00 
  40267d:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  402682:	3c 00                	cmp    $0x0,%al
  402684:	74 50                	je     4026d6 <runtime::arena_allocator_proc+0x746>
  402686:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  40268d:	00 
  40268e:	8a 44 24 2f          	mov    0x2f(%rsp),%al
  402692:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  402699:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4026a0:	00 
  4026a1:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4026a8:	00 
  4026a9:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  4026b0:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  4026b7:	00 
  4026b8:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  4026bf:	00 
  4026c0:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  4026c7:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4026cb:	48 89 11             	mov    %rdx,(%rcx)
  4026ce:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  4026d5:	c3                   	ret
  4026d6:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4026db:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4026e0:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  4026e7:	00 
  4026e8:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  4026ef:	00 
  4026f0:	48 83 bc 24 e0 00 00 	cmpq   $0x0,0xe0(%rsp)
  4026f7:	00 00 
  4026f9:	0f 94 c0             	sete   %al
  4026fc:	24 01                	and    $0x1,%al
  4026fe:	3c 00                	cmp    $0x0,%al
  402700:	74 45                	je     402747 <runtime::arena_allocator_proc+0x7b7>
  402702:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  402709:	00 
  40270a:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  402711:	00 
  402712:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  402719:	00 
  40271a:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  402721:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  402728:	00 
  402729:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  402730:	00 
  402731:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  402738:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40273c:	48 89 11             	mov    %rdx,(%rcx)
  40273f:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  402746:	c3                   	ret
  402747:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40274e:	00 
  40274f:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  402754:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40275b:	00 
  40275c:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402761:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  402768:	00 
  402769:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  40276e:	4c 8b 8c 24 88 01 00 	mov    0x188(%rsp),%r9
  402775:	00 
  402776:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40277b:	bf 60 e0 40 00       	mov    $0x40e060,%edi
  402780:	31 c0                	xor    %eax,%eax
  402782:	41 89 c0             	mov    %eax,%r8d
  402785:	be 3c 00 00 00       	mov    $0x3c,%esi
  40278a:	ba fa 00 00 00       	mov    $0xfa,%edx
  40278f:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  402794:	e8 57 81 00 00       	call   40a8f0 <runtime::multi_pointer_slice_expr_error>
  402799:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40279e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4027a3:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4027a8:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4027ad:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  4027b4:	00 
  4027b5:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4027bc:	00 
  4027bd:	48 8b 94 24 d0 00 00 	mov    0xd0(%rsp),%rdx
  4027c4:	00 
  4027c5:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  4027cc:	00 
  4027cd:	e8 2e 95 00 00       	call   40bd00 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  4027d2:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  4027d9:	00 
  4027da:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  4027e1:	00 
  4027e2:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4027e9:	00 
  4027ea:	48 89 94 24 b8 01 00 	mov    %rdx,0x1b8(%rsp)
  4027f1:	00 
  4027f2:	48 89 8c 24 b0 01 00 	mov    %rcx,0x1b0(%rsp)
  4027f9:	00 
  4027fa:	c6 84 24 af 01 00 00 	movb   $0x0,0x1af(%rsp)
  402801:	00 
  402802:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402806:	48 89 08             	mov    %rcx,(%rax)
  402809:	31 c0                	xor    %eax,%eax
  40280b:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  402812:	c3                   	ret
  402813:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  40281a:	00 
  40281b:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  402822:	00 
  402823:	48 83 bc 24 c8 00 00 	cmpq   $0x0,0xc8(%rsp)
  40282a:	00 00 
  40282c:	0f 95 c0             	setne  %al
  40282f:	24 01                	and    $0x1,%al
  402831:	3c 00                	cmp    $0x0,%al
  402833:	74 0b                	je     402840 <runtime::arena_allocator_proc+0x8b0>
  402835:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  40283c:	00 
  40283d:	c6 00 5d             	movb   $0x5d,(%rax)
  402840:	eb 08                	jmp    40284a <runtime::arena_allocator_proc+0x8ba>
  402842:	c6 84 24 af 01 00 00 	movb   $0x4,0x1af(%rsp)
  402849:	04 
  40284a:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  402851:	00 
  402852:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  402859:	00 
  40285a:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  402861:	00 
  402862:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  402869:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  402870:	00 
  402871:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  402878:	00 
  402879:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  402880:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402884:	48 89 11             	mov    %rdx,(%rcx)
  402887:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  40288e:	c3                   	ret
  40288f:	90                   	nop

0000000000402890 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  402890:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  402895:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  40289a:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40289f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4028a4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4028a9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4028ae:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4028b5:	00 00 
  4028b7:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4028bc:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  4028c0:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4028c5:	48 03 4a 20          	add    0x20(%rdx),%rcx
  4028c9:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  4028ce:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4028d3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4028d8:	48 83 e8 01          	sub    $0x1,%rax
  4028dc:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4028e1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4028e6:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  4028eb:	48 83 f8 00          	cmp    $0x0,%rax
  4028ef:	0f 95 c0             	setne  %al
  4028f2:	24 01                	and    $0x1,%al
  4028f4:	3c 00                	cmp    $0x0,%al
  4028f6:	74 17                	je     40290f <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  4028f8:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4028fd:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  402902:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  402907:	48 29 c8             	sub    %rcx,%rax
  40290a:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40290f:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  402914:	c3                   	ret
  402915:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40291c:	00 00 00 00 

0000000000402920 <runtime::arena_alloc.align_forward_uint-0>:
  402920:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  402925:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40292a:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40292f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  402934:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402939:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40293e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402943:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  402948:	48 83 e9 01          	sub    $0x1,%rcx
  40294c:	48 21 c8             	and    %rcx,%rax
  40294f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  402954:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  40295a:	0f 95 c0             	setne  %al
  40295d:	24 01                	and    $0x1,%al
  40295f:	3c 00                	cmp    $0x0,%al
  402961:	74 14                	je     402977 <runtime::arena_alloc.align_forward_uint-0+0x57>
  402963:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  402968:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  40296d:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  402972:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402977:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40297c:	c3                   	ret
  40297d:	0f 1f 00             	nopl   (%rax)

0000000000402980 <linux::read>:
  402980:	48 83 ec 48          	sub    $0x48,%rsp
  402984:	48 89 0c 24          	mov    %rcx,(%rsp)
  402988:	89 7c 24 0c          	mov    %edi,0xc(%rsp)
  40298c:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  402991:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402996:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40299b:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4029a0:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  4029a4:	89 74 24 44          	mov    %esi,0x44(%rsp)
  4029a8:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4029ad:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4029b2:	31 c0                	xor    %eax,%eax
  4029b4:	89 c7                	mov    %eax,%edi
  4029b6:	e8 85 01 00 00       	call   402b40 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int)->(:int)>
  4029bb:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4029c0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4029c5:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  4029cc:	00 00 
  4029ce:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
  4029d3:	e8 78 02 00 00       	call   402c50 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  4029d8:	48 8b 0c 24          	mov    (%rsp),%rcx
  4029dc:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4029e1:	48 89 11             	mov    %rdx,(%rcx)
  4029e4:	48 83 c4 48          	add    $0x48,%rsp
  4029e8:	c3                   	ret
  4029e9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004029f0 <linux::write>:
  4029f0:	48 83 ec 48          	sub    $0x48,%rsp
  4029f4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4029f8:	89 7c 24 0c          	mov    %edi,0xc(%rsp)
  4029fc:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  402a01:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402a06:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402a0b:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402a10:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  402a14:	89 74 24 44          	mov    %esi,0x44(%rsp)
  402a18:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  402a1d:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402a22:	bf 01 00 00 00       	mov    $0x1,%edi
  402a27:	e8 14 01 00 00       	call   402b40 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int)->(:int)>
  402a2c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402a31:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402a36:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  402a3d:	00 00 
  402a3f:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
  402a44:	e8 07 02 00 00       	call   402c50 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  402a49:	48 8b 0c 24          	mov    (%rsp),%rcx
  402a4d:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402a52:	48 89 11             	mov    %rdx,(%rcx)
  402a55:	48 83 c4 48          	add    $0x48,%rsp
  402a59:	c3                   	ret
  402a5a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402a60 <linux::close>:
  402a60:	48 83 ec 18          	sub    $0x18,%rsp
  402a64:	89 7c 24 04          	mov    %edi,0x4(%rsp)
  402a68:	8b 74 24 04          	mov    0x4(%rsp),%esi
  402a6c:	89 74 24 14          	mov    %esi,0x14(%rsp)
  402a70:	bf 03 00 00 00       	mov    $0x3,%edi
  402a75:	e8 16 00 00 00       	call   402a90 <linux::syscall1:proc"contextless"(nr:uintptr,p1:linux::Fd)->(:int)>
  402a7a:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  402a7f:	31 c0                	xor    %eax,%eax
  402a81:	48 2b 44 24 08       	sub    0x8(%rsp),%rax
  402a86:	48 83 c4 18          	add    $0x18,%rsp
  402a8a:	c3                   	ret
  402a8b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402a90 <linux::syscall1:proc"contextless"(nr:uintptr,p1:linux::Fd)->(:int)>:
  402a90:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  402a95:	89 74 24 f0          	mov    %esi,-0x10(%rsp)
  402a99:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  402a9e:	8b 4c 24 f0          	mov    -0x10(%rsp),%ecx
  402aa2:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402aa7:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  402aab:	48 63 f9             	movslq %ecx,%rdi
  402aae:	0f 05                	syscall
  402ab0:	c3                   	ret
  402ab1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402ab8:	0f 1f 84 00 00 00 00 
  402abf:	00 

0000000000402ac0 <linux::syscall2:proc"contextless"(nr:uintptr,p1:i32,p2:^linux::Stat)->(:int)>:
  402ac0:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  402ac5:	89 74 24 dc          	mov    %esi,-0x24(%rsp)
  402ac9:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  402ace:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  402ad3:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  402ad8:	8b 4c 24 dc          	mov    -0x24(%rsp),%ecx
  402adc:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402ae1:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  402ae5:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  402aea:	48 63 f9             	movslq %ecx,%rdi
  402aed:	0f 05                	syscall
  402aef:	c3                   	ret

0000000000402af0 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:i64,p3:linux::Seek_Whence)->(:int)>:
  402af0:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  402af5:	89 74 24 d4          	mov    %esi,-0x2c(%rsp)
  402af9:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  402afe:	66 89 c8             	mov    %cx,%ax
  402b01:	66 89 44 24 e4       	mov    %ax,-0x1c(%rsp)
  402b06:	48 8b 74 24 c8       	mov    -0x38(%rsp),%rsi
  402b0b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  402b10:	66 8b 4c 24 e4       	mov    -0x1c(%rsp),%cx
  402b15:	8b 54 24 d4          	mov    -0x2c(%rsp),%edx
  402b19:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402b1e:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  402b22:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  402b27:	66 89 4c 24 e6       	mov    %cx,-0x1a(%rsp)
  402b2c:	48 63 fa             	movslq %edx,%rdi
  402b2f:	48 0f bf d1          	movswq %cx,%rdx
  402b33:	0f 05                	syscall
  402b35:	c3                   	ret
  402b36:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  402b3d:	00 00 00 

0000000000402b40 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int)->(:int)>:
  402b40:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  402b45:	89 74 24 cc          	mov    %esi,-0x34(%rsp)
  402b49:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  402b4e:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  402b53:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  402b58:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  402b5d:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  402b62:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  402b66:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402b6b:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  402b6f:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  402b74:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  402b79:	48 63 f9             	movslq %ecx,%rdi
  402b7c:	0f 05                	syscall
  402b7e:	c3                   	ret
  402b7f:	90                   	nop

0000000000402b80 <linux::syscall3:proc"contextless"(nr:uintptr,p1:rawptr,p2:[^]u8,p3:int)->(:int)>:
  402b80:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  402b85:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  402b8a:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  402b8f:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  402b94:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  402b99:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  402b9e:	48 8b 7c 24 c8       	mov    -0x38(%rsp),%rdi
  402ba3:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  402ba8:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402bad:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  402bb2:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  402bb7:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  402bbc:	0f 05                	syscall
  402bbe:	c3                   	ret
  402bbf:	90                   	nop

0000000000402bc0 <linux::syscall4:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int,p4:uint)->(:int)>:
  402bc0:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  402bc5:	89 74 24 bc          	mov    %esi,-0x44(%rsp)
  402bc9:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  402bce:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  402bd3:	4c 89 44 24 d0       	mov    %r8,-0x30(%rsp)
  402bd8:	4c 8b 54 24 d0       	mov    -0x30(%rsp),%r10
  402bdd:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  402be2:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  402be7:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  402bec:	8b 4c 24 bc          	mov    -0x44(%rsp),%ecx
  402bf0:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402bf5:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  402bf9:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  402bfe:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  402c03:	4c 89 54 24 d8       	mov    %r10,-0x28(%rsp)
  402c08:	48 63 f9             	movslq %ecx,%rdi
  402c0b:	0f 05                	syscall
  402c0d:	c3                   	ret
  402c0e:	66 90                	xchg   %ax,%ax

0000000000402c10 <linux::fstat>:
  402c10:	48 83 ec 28          	sub    $0x28,%rsp
  402c14:	89 7c 24 04          	mov    %edi,0x4(%rsp)
  402c18:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402c1d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402c22:	8b 74 24 04          	mov    0x4(%rsp),%esi
  402c26:	89 74 24 24          	mov    %esi,0x24(%rsp)
  402c2a:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402c2f:	bf 05 00 00 00       	mov    $0x5,%edi
  402c34:	e8 87 fe ff ff       	call   402ac0 <linux::syscall2:proc"contextless"(nr:uintptr,p1:i32,p2:^linux::Stat)->(:int)>
  402c39:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402c3e:	31 c0                	xor    %eax,%eax
  402c40:	48 2b 44 24 10       	sub    0x10(%rsp),%rax
  402c45:	48 83 c4 28          	add    $0x28,%rsp
  402c49:	c3                   	ret
  402c4a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402c50 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>:
  402c50:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  402c55:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  402c5a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  402c5f:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402c64:	48 83 f8 00          	cmp    $0x0,%rax
  402c68:	0f 9c c0             	setl   %al
  402c6b:	24 01                	and    $0x1,%al
  402c6d:	3c 00                	cmp    $0x0,%al
  402c6f:	74 21                	je     402c92 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)+0x42>
  402c71:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  402c76:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  402c7b:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  402c82:	00 00 
  402c84:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  402c89:	31 c0                	xor    %eax,%eax
  402c8b:	48 29 f0             	sub    %rsi,%rax
  402c8e:	48 89 11             	mov    %rdx,(%rcx)
  402c91:	c3                   	ret
  402c92:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  402c97:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  402c9c:	48 89 08             	mov    %rcx,(%rax)
  402c9f:	31 c0                	xor    %eax,%eax
  402ca1:	c3                   	ret
  402ca2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402ca9:	1f 84 00 00 00 00 00 

0000000000402cb0 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$i64)->(:i64,:linux::Errno)>:
  402cb0:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  402cb5:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  402cba:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  402cbf:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402cc4:	48 83 f8 00          	cmp    $0x0,%rax
  402cc8:	0f 9c c0             	setl   %al
  402ccb:	24 01                	and    $0x1,%al
  402ccd:	3c 00                	cmp    $0x0,%al
  402ccf:	74 21                	je     402cf2 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$i64)->(:i64,:linux::Errno)+0x42>
  402cd1:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  402cd6:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  402cdb:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  402ce2:	00 00 
  402ce4:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  402ce9:	31 c0                	xor    %eax,%eax
  402ceb:	48 29 f0             	sub    %rsi,%rax
  402cee:	48 89 11             	mov    %rdx,(%rcx)
  402cf1:	c3                   	ret
  402cf2:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  402cf7:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  402cfc:	48 89 08             	mov    %rcx,(%rax)
  402cff:	31 c0                	xor    %eax,%eax
  402d01:	c3                   	ret
  402d02:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402d09:	1f 84 00 00 00 00 00 

0000000000402d10 <linux::[helpers.odin]::compat64_arg_pair>:
  402d10:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  402d15:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  402d1a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402d1f:	c3                   	ret

0000000000402d20 <linux::lseek>:
  402d20:	48 83 ec 48          	sub    $0x48,%rsp
  402d24:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  402d29:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402d2e:	89 7c 24 18          	mov    %edi,0x18(%rsp)
  402d32:	66 89 d0             	mov    %dx,%ax
  402d35:	66 89 44 24 1e       	mov    %ax,0x1e(%rsp)
  402d3a:	66 8b 44 24 1e       	mov    0x1e(%rsp),%ax
  402d3f:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402d44:	8b 74 24 18          	mov    0x18(%rsp),%esi
  402d48:	89 74 24 44          	mov    %esi,0x44(%rsp)
  402d4c:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  402d51:	66 89 44 24 36       	mov    %ax,0x36(%rsp)
  402d56:	bf 08 00 00 00       	mov    $0x8,%edi
  402d5b:	0f b7 c8             	movzwl %ax,%ecx
  402d5e:	e8 8d fd ff ff       	call   402af0 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:i64,p3:linux::Seek_Whence)->(:int)>
  402d63:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402d68:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402d6d:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  402d74:	00 00 
  402d76:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
  402d7b:	e8 30 ff ff ff       	call   402cb0 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$i64)->(:i64,:linux::Errno)>
  402d80:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  402d85:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402d8a:	48 89 11             	mov    %rdx,(%rcx)
  402d8d:	48 83 c4 48          	add    $0x48,%rsp
  402d91:	c3                   	ret
  402d92:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402d99:	1f 84 00 00 00 00 00 

0000000000402da0 <linux::pread>:
  402da0:	48 83 ec 58          	sub    $0x58,%rsp
  402da4:	4c 89 04 24          	mov    %r8,(%rsp)
  402da8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  402dad:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  402db1:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402db6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402dbb:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402dc0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402dc5:	8b 54 24 14          	mov    0x14(%rsp),%edx
  402dc9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402dce:	89 54 24 54          	mov    %edx,0x54(%rsp)
  402dd2:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  402dd7:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402ddc:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  402de1:	e8 2a ff ff ff       	call   402d10 <linux::[helpers.odin]::compat64_arg_pair>
  402de6:	8b 74 24 14          	mov    0x14(%rsp),%esi
  402dea:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402def:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402df4:	49 89 c0             	mov    %rax,%r8
  402df7:	bf 11 00 00 00       	mov    $0x11,%edi
  402dfc:	e8 bf fd ff ff       	call   402bc0 <linux::syscall4:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int,p4:uint)->(:int)>
  402e01:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402e06:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  402e0b:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  402e12:	00 00 
  402e14:	48 8d 74 24 28       	lea    0x28(%rsp),%rsi
  402e19:	e8 32 fe ff ff       	call   402c50 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  402e1e:	48 8b 0c 24          	mov    (%rsp),%rcx
  402e22:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402e27:	48 89 11             	mov    %rdx,(%rcx)
  402e2a:	48 83 c4 58          	add    $0x58,%rsp
  402e2e:	c3                   	ret
  402e2f:	90                   	nop

0000000000402e30 <linux::pwrite>:
  402e30:	48 83 ec 58          	sub    $0x58,%rsp
  402e34:	4c 89 04 24          	mov    %r8,(%rsp)
  402e38:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  402e3d:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  402e41:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402e46:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402e4b:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402e50:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402e55:	8b 54 24 14          	mov    0x14(%rsp),%edx
  402e59:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402e5e:	89 54 24 54          	mov    %edx,0x54(%rsp)
  402e62:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  402e67:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402e6c:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  402e71:	e8 9a fe ff ff       	call   402d10 <linux::[helpers.odin]::compat64_arg_pair>
  402e76:	8b 74 24 14          	mov    0x14(%rsp),%esi
  402e7a:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402e7f:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402e84:	49 89 c0             	mov    %rax,%r8
  402e87:	bf 12 00 00 00       	mov    $0x12,%edi
  402e8c:	e8 2f fd ff ff       	call   402bc0 <linux::syscall4:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int,p4:uint)->(:int)>
  402e91:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402e96:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  402e9b:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  402ea2:	00 00 
  402ea4:	48 8d 74 24 28       	lea    0x28(%rsp),%rsi
  402ea9:	e8 a2 fd ff ff       	call   402c50 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  402eae:	48 8b 0c 24          	mov    (%rsp),%rcx
  402eb2:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402eb7:	48 89 11             	mov    %rdx,(%rcx)
  402eba:	48 83 c4 58          	add    $0x58,%rsp
  402ebe:	c3                   	ret
  402ebf:	90                   	nop

0000000000402ec0 <linux::fsync>:
  402ec0:	48 83 ec 18          	sub    $0x18,%rsp
  402ec4:	89 7c 24 04          	mov    %edi,0x4(%rsp)
  402ec8:	8b 74 24 04          	mov    0x4(%rsp),%esi
  402ecc:	89 74 24 14          	mov    %esi,0x14(%rsp)
  402ed0:	bf 4a 00 00 00       	mov    $0x4a,%edi
  402ed5:	e8 b6 fb ff ff       	call   402a90 <linux::syscall1:proc"contextless"(nr:uintptr,p1:linux::Fd)->(:int)>
  402eda:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  402edf:	31 c0                	xor    %eax,%eax
  402ee1:	48 2b 44 24 08       	sub    0x8(%rsp),%rax
  402ee6:	48 83 c4 18          	add    $0x18,%rsp
  402eea:	c3                   	ret
  402eeb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402ef0 <linux::readlink>:
  402ef0:	48 83 ec 48          	sub    $0x48,%rsp
  402ef4:	48 89 0c 24          	mov    %rcx,(%rsp)
  402ef8:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402efd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  402f02:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402f07:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402f0c:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402f11:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  402f16:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  402f1b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  402f20:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402f25:	bf 59 00 00 00       	mov    $0x59,%edi
  402f2a:	e8 51 fc ff ff       	call   402b80 <linux::syscall3:proc"contextless"(nr:uintptr,p1:rawptr,p2:[^]u8,p3:int)->(:int)>
  402f2f:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402f34:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402f39:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  402f40:	00 00 
  402f42:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
  402f47:	e8 04 fd ff ff       	call   402c50 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  402f4c:	48 8b 0c 24          	mov    (%rsp),%rcx
  402f50:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402f55:	48 89 11             	mov    %rdx,(%rcx)
  402f58:	48 83 c4 48          	add    $0x48,%rsp
  402f5c:	c3                   	ret
  402f5d:	0f 1f 00             	nopl   (%rax)

0000000000402f60 <os::[allocators.odin]::init_thread_local_cleaner>:
  402f60:	48 83 ec 18          	sub    $0x18,%rsp
  402f64:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  402f69:	31 f6                	xor    %esi,%esi
  402f6b:	ba 10 00 00 00       	mov    $0x10,%edx
  402f70:	e8 cb e0 ff ff       	call   401040 <memset@plt>
  402f75:	31 c0                	xor    %eax,%eax
  402f77:	a8 01                	test   $0x1,%al
  402f79:	75 02                	jne    402f7d <os::[allocators.odin]::init_thread_local_cleaner+0x1d>
  402f7b:	eb 13                	jmp    402f90 <os::[allocators.odin]::init_thread_local_cleaner+0x30>
  402f7d:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  402f82:	31 f6                	xor    %esi,%esi
  402f84:	ba 10 00 00 00       	mov    $0x10,%edx
  402f89:	e8 b2 e0 ff ff       	call   401040 <memset@plt>
  402f8e:	eb 15                	jmp    402fa5 <os::[allocators.odin]::init_thread_local_cleaner+0x45>
  402f90:	48 c7 c0 f0 31 40 00 	mov    $0x4031f0,%rax
  402f97:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  402f9c:	48 c7 44 24 10 02 00 	movq   $0x2,0x10(%rsp)
  402fa3:	00 00 
  402fa5:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402faa:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402faf:	e8 ec 57 00 00       	call   4087a0 <runtime::add_thread_local_cleaner>
  402fb4:	48 83 c4 18          	add    $0x18,%rsp
  402fb8:	c3                   	ret
  402fb9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000402fc0 <os::[file_linux.odin]::_standard_stream_init>:
  402fc0:	50                   	push   %rax
  402fc1:	eb 00                	jmp    402fc3 <os::[file_linux.odin]::_standard_stream_init+0x3>
  402fc3:	31 c0                	xor    %eax,%eax
  402fc5:	bf 70 30 41 00       	mov    $0x413070,%edi
  402fca:	ba 68 e2 40 00       	mov    $0x40e268,%edx
  402fcf:	31 f6                	xor    %esi,%esi
  402fd1:	b9 0f 00 00 00       	mov    $0xf,%ecx
  402fd6:	e8 05 39 00 00       	call   4068e0 <os::[file_linux.odin]::_standard_stream_init.new_std-0>
  402fdb:	48 89 c1             	mov    %rax,%rcx
  402fde:	48 c7 c0 38 32 41 00 	mov    $0x413238,%rax
  402fe5:	48 89 08             	mov    %rcx,(%rax)
  402fe8:	48 b8 70 30 41 00 00 	movabs $0x413070,%rax
  402fef:	00 00 00 
  402ff2:	48 83 c0 68          	add    $0x68,%rax
  402ff6:	ba 78 e2 40 00       	mov    $0x40e278,%edx
  402ffb:	bf d8 30 41 00       	mov    $0x4130d8,%edi
  403000:	be 01 00 00 00       	mov    $0x1,%esi
  403005:	b9 0f 00 00 00       	mov    $0xf,%ecx
  40300a:	e8 d1 38 00 00       	call   4068e0 <os::[file_linux.odin]::_standard_stream_init.new_std-0>
  40300f:	48 89 c1             	mov    %rax,%rcx
  403012:	48 c7 c0 40 32 41 00 	mov    $0x413240,%rax
  403019:	48 89 08             	mov    %rcx,(%rax)
  40301c:	48 b8 70 30 41 00 00 	movabs $0x413070,%rax
  403023:	00 00 00 
  403026:	48 05 d0 00 00 00    	add    $0xd0,%rax
  40302c:	ba 88 e2 40 00       	mov    $0x40e288,%edx
  403031:	bf 40 31 41 00       	mov    $0x413140,%edi
  403036:	be 02 00 00 00       	mov    $0x2,%esi
  40303b:	b9 0f 00 00 00       	mov    $0xf,%ecx
  403040:	e8 9b 38 00 00       	call   4068e0 <os::[file_linux.odin]::_standard_stream_init.new_std-0>
  403045:	48 89 c1             	mov    %rax,%rcx
  403048:	48 c7 c0 48 32 41 00 	mov    $0x413248,%rax
  40304f:	48 89 08             	mov    %rcx,(%rax)
  403052:	58                   	pop    %rax
  403053:	c3                   	ret
  403054:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40305b:	00 00 00 00 00 

0000000000403060 <os::[process.odin]::delete_args>:
  403060:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  403067:	48 c7 c0 50 32 41 00 	mov    $0x413250,%rax
  40306e:	48 83 38 00          	cmpq   $0x0,(%rax)
  403072:	0f 95 c0             	setne  %al
  403075:	24 01                	and    $0x1,%al
  403077:	3c 00                	cmp    $0x0,%al
  403079:	0f 84 5c 01 00 00    	je     4031db <os::[process.odin]::delete_args+0x17b>
  40307f:	0f 57 c0             	xorps  %xmm0,%xmm0
  403082:	0f 29 04 24          	movaps %xmm0,(%rsp)
  403086:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  40308d:	00 
  40308e:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  403095:	00 
  403096:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40309d:	00 
  40309e:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  4030a5:	00 
  4030a6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4030ad:	00 
  4030ae:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  4030b5:	00 
  4030b6:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  4030bd:	00 
  4030be:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  4030c5:	00 
  4030c6:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4030cb:	e8 f0 7f 00 00       	call   40b0c0 <runtime::[core.odin]::__init_context>
  4030d0:	0f 28 04 24          	movaps (%rsp),%xmm0
  4030d4:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  4030db:	00 
  4030dc:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4030e3:	00 
  4030e4:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4030eb:	00 
  4030ec:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  4030f3:	00 
  4030f4:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4030f9:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  4030fe:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  403103:	48 8d 7c 24 50       	lea    0x50(%rsp),%rdi
  403108:	e8 63 7f 00 00       	call   40b070 <runtime::default_context>
  40310d:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403112:	0f 28 44 24 50       	movaps 0x50(%rsp),%xmm0
  403117:	0f 28 4c 24 60       	movaps 0x60(%rsp),%xmm1
  40311c:	0f 28 54 24 70       	movaps 0x70(%rsp),%xmm2
  403121:	0f 28 9c 24 80 00 00 	movaps 0x80(%rsp),%xmm3
  403128:	00 
  403129:	0f 28 a4 24 90 00 00 	movaps 0x90(%rsp),%xmm4
  403130:	00 
  403131:	0f 28 ac 24 a0 00 00 	movaps 0xa0(%rsp),%xmm5
  403138:	00 
  403139:	0f 28 b4 24 b0 00 00 	movaps 0xb0(%rsp),%xmm6
  403140:	00 
  403141:	0f 29 b4 24 20 01 00 	movaps %xmm6,0x120(%rsp)
  403148:	00 
  403149:	0f 29 ac 24 10 01 00 	movaps %xmm5,0x110(%rsp)
  403150:	00 
  403151:	0f 29 a4 24 00 01 00 	movaps %xmm4,0x100(%rsp)
  403158:	00 
  403159:	0f 29 9c 24 f0 00 00 	movaps %xmm3,0xf0(%rsp)
  403160:	00 
  403161:	0f 29 94 24 e0 00 00 	movaps %xmm2,0xe0(%rsp)
  403168:	00 
  403169:	0f 29 8c 24 d0 00 00 	movaps %xmm1,0xd0(%rsp)
  403170:	00 
  403171:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  403178:	00 
  403179:	48 c7 c0 50 32 41 00 	mov    $0x413250,%rax
  403180:	48 8b 08             	mov    (%rax),%rcx
  403183:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  403188:	48 8b 40 08          	mov    0x8(%rax),%rax
  40318c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  403191:	e8 2a 02 00 00       	call   4033c0 <os::heap_allocator>
  403196:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40319b:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  4031a0:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4031a5:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4031aa:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4031af:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4031b4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4031b9:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4031be:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4031c3:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4031c8:	41 b8 40 e2 40 00    	mov    $0x40e240,%r8d
  4031ce:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  4031d5:	00 
  4031d6:	e8 c5 8c 00 00       	call   40bea0 <runtime::delete_slice:proc(array:[]string,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  4031db:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  4031e2:	c3                   	ret
  4031e3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4031ea:	84 00 00 00 00 00 

00000000004031f0 <os::[allocators.odin]::temp_allocator_fini>:
  4031f0:	48 83 ec 18          	sub    $0x18,%rsp
  4031f4:	48 c7 44 24 10 02 00 	movq   $0x2,0x10(%rsp)
  4031fb:	00 00 
  4031fd:	48 c7 44 24 08 ff ff 	movq   $0xffffffffffffffff,0x8(%rsp)
  403204:	ff ff 
  403206:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40320b:	48 83 c0 01          	add    $0x1,%rax
  40320f:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403214:	48 83 f8 02          	cmp    $0x2,%rax
  403218:	7d 30                	jge    40324a <os::[allocators.odin]::temp_allocator_fini+0x5a>
  40321a:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40321f:	48 c7 c1 90 ff ff ff 	mov    $0xffffffffffffff90,%rcx
  403226:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  40322d:	00 00 
  40322f:	48 01 cf             	add    %rcx,%rdi
  403232:	48 6b c0 38          	imul   $0x38,%rax,%rax
  403236:	48 01 c7             	add    %rax,%rdi
  403239:	48 be e0 e1 40 00 00 	movabs $0x40e1e0,%rsi
  403240:	00 00 00 
  403243:	e8 a8 ec ff ff       	call   401ef0 <runtime::arena_destroy>
  403248:	eb bc                	jmp    403206 <os::[allocators.odin]::temp_allocator_fini+0x16>
  40324a:	0f 57 c0             	xorps  %xmm0,%xmm0
  40324d:	48 c7 c0 90 ff ff ff 	mov    $0xffffffffffffff90,%rax
  403254:	64 0f 11 40 60       	movups %xmm0,%fs:0x60(%rax)
  403259:	64 0f 11 40 50       	movups %xmm0,%fs:0x50(%rax)
  40325e:	64 0f 11 40 40       	movups %xmm0,%fs:0x40(%rax)
  403263:	64 0f 11 40 30       	movups %xmm0,%fs:0x30(%rax)
  403268:	64 0f 11 40 20       	movups %xmm0,%fs:0x20(%rax)
  40326d:	64 0f 11 40 10       	movups %xmm0,%fs:0x10(%rax)
  403272:	64 0f 11 00          	movups %xmm0,%fs:(%rax)
  403276:	48 83 c4 18          	add    $0x18,%rsp
  40327a:	c3                   	ret
  40327b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403280 <os::[stat_linux.odin]::_fstat>:
  403280:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403287:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40328c:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  403291:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403296:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40329b:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  4032a0:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  4032a5:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4032aa:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4032af:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4032b4:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4032bb:	00 
  4032bc:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  4032c3:	00 
  4032c4:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  4032cb:	00 
  4032cc:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  4032d3:	00 
  4032d4:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  4032db:	00 
  4032dc:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4032e3:	00 
  4032e4:	48 8b 12             	mov    (%rdx),%rdx
  4032e7:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  4032ee:	00 
  4032ef:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  4032f6:	00 
  4032f7:	8b 7a 28             	mov    0x28(%rdx),%edi
  4032fa:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  403301:	00 
  403302:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  403309:	00 
  40330a:	48 8b b4 24 b0 00 00 	mov    0xb0(%rsp),%rsi
  403311:	00 
  403312:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  403319:	00 
  40331a:	0f 57 c0             	xorps  %xmm0,%xmm0
  40331d:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  403324:	00 
  403325:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40332c:	00 
  40332d:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  403334:	00 
  403335:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40333a:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  40333f:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  403344:	48 8d 4c 24 50       	lea    0x50(%rsp),%rcx
  403349:	e8 92 00 00 00       	call   4033e0 <os::[stat_linux.odin]::_fstat_internal>
  40334e:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  403353:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403358:	8b 44 24 40          	mov    0x40(%rsp),%eax
  40335c:	8b 4c 24 44          	mov    0x44(%rsp),%ecx
  403360:	0f 28 44 24 50       	movaps 0x50(%rsp),%xmm0
  403365:	0f 28 4c 24 60       	movaps 0x60(%rsp),%xmm1
  40336a:	0f 28 54 24 70       	movaps 0x70(%rsp),%xmm2
  40336f:	0f 28 9c 24 80 00 00 	movaps 0x80(%rsp),%xmm3
  403376:	00 
  403377:	0f 28 a4 24 90 00 00 	movaps 0x90(%rsp),%xmm4
  40337e:	00 
  40337f:	0f 28 ac 24 a0 00 00 	movaps 0xa0(%rsp),%xmm5
  403386:	00 
  403387:	0f 11 6a 50          	movups %xmm5,0x50(%rdx)
  40338b:	0f 11 62 40          	movups %xmm4,0x40(%rdx)
  40338f:	0f 11 5a 30          	movups %xmm3,0x30(%rdx)
  403393:	0f 11 52 20          	movups %xmm2,0x20(%rdx)
  403397:	0f 11 4a 10          	movups %xmm1,0x10(%rdx)
  40339b:	0f 11 02             	movups %xmm0,(%rdx)
  40339e:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  4033a2:	89 44 24 30          	mov    %eax,0x30(%rsp)
  4033a6:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4033ab:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4033b2:	c3                   	ret
  4033b3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4033ba:	84 00 00 00 00 00 

00000000004033c0 <os::heap_allocator>:
  4033c0:	48 c7 c0 30 3a 40 00 	mov    $0x403a30,%rax
  4033c7:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4033cc:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  4033d3:	00 00 
  4033d5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4033da:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4033df:	c3                   	ret

00000000004033e0 <os::[stat_linux.odin]::_fstat_internal>:
  4033e0:	48 81 ec 98 02 00 00 	sub    $0x298,%rsp
  4033e7:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4033ec:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  4033f1:	89 7c 24 5c          	mov    %edi,0x5c(%rsp)
  4033f5:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  4033fa:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4033ff:	8b 54 24 5c          	mov    0x5c(%rsp),%edx
  403403:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  403408:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40340d:	89 94 24 94 02 00 00 	mov    %edx,0x294(%rsp)
  403414:	48 89 8c 24 88 02 00 	mov    %rcx,0x288(%rsp)
  40341b:	00 
  40341c:	48 89 84 24 80 02 00 	mov    %rax,0x280(%rsp)
  403423:	00 
  403424:	48 8b 84 24 80 02 00 	mov    0x280(%rsp),%rax
  40342b:	00 
  40342c:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403431:	48 8b 84 24 88 02 00 	mov    0x288(%rsp),%rax
  403438:	00 
  403439:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40343e:	48 8d bc 24 20 02 00 	lea    0x220(%rsp),%rdi
  403445:	00 
  403446:	31 f6                	xor    %esi,%esi
  403448:	ba 60 00 00 00       	mov    $0x60,%edx
  40344d:	e8 ee db ff ff       	call   401040 <memset@plt>
  403452:	48 c7 84 24 18 02 00 	movq   $0x0,0x218(%rsp)
  403459:	00 00 00 00 00 
  40345e:	48 8d bc 24 88 01 00 	lea    0x188(%rsp),%rdi
  403465:	00 
  403466:	31 f6                	xor    %esi,%esi
  403468:	ba 90 00 00 00       	mov    $0x90,%edx
  40346d:	e8 ce db ff ff       	call   401040 <memset@plt>
  403472:	8b 7c 24 5c          	mov    0x5c(%rsp),%edi
  403476:	48 8d b4 24 88 01 00 	lea    0x188(%rsp),%rsi
  40347d:	00 
  40347e:	e8 8d f7 ff ff       	call   402c10 <linux::fstat>
  403483:	89 84 24 84 01 00 00 	mov    %eax,0x184(%rsp)
  40348a:	83 bc 24 84 01 00 00 	cmpl   $0x0,0x184(%rsp)
  403491:	00 
  403492:	0f 95 c0             	setne  %al
  403495:	24 01                	and    $0x1,%al
  403497:	3c 00                	cmp    $0x0,%al
  403499:	0f 84 a2 00 00 00    	je     403541 <os::[stat_linux.odin]::_fstat_internal+0x161>
  40349f:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4034a4:	8b bc 24 84 01 00 00 	mov    0x184(%rsp),%edi
  4034ab:	e8 70 22 00 00       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  4034b0:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4034b5:	48 89 84 24 70 01 00 	mov    %rax,0x170(%rsp)
  4034bc:	00 
  4034bd:	8b 84 24 70 01 00 00 	mov    0x170(%rsp),%eax
  4034c4:	8b 8c 24 74 01 00 00 	mov    0x174(%rsp),%ecx
  4034cb:	0f 57 c0             	xorps  %xmm0,%xmm0
  4034ce:	0f 29 84 24 70 02 00 	movaps %xmm0,0x270(%rsp)
  4034d5:	00 
  4034d6:	0f 29 84 24 60 02 00 	movaps %xmm0,0x260(%rsp)
  4034dd:	00 
  4034de:	0f 29 84 24 50 02 00 	movaps %xmm0,0x250(%rsp)
  4034e5:	00 
  4034e6:	0f 29 84 24 40 02 00 	movaps %xmm0,0x240(%rsp)
  4034ed:	00 
  4034ee:	0f 29 84 24 30 02 00 	movaps %xmm0,0x230(%rsp)
  4034f5:	00 
  4034f6:	0f 29 84 24 20 02 00 	movaps %xmm0,0x220(%rsp)
  4034fd:	00 
  4034fe:	89 8c 24 1c 02 00 00 	mov    %ecx,0x21c(%rsp)
  403505:	89 84 24 18 02 00 00 	mov    %eax,0x218(%rsp)
  40350c:	0f 11 42 50          	movups %xmm0,0x50(%rdx)
  403510:	0f 11 42 40          	movups %xmm0,0x40(%rdx)
  403514:	0f 11 42 30          	movups %xmm0,0x30(%rdx)
  403518:	0f 11 42 20          	movups %xmm0,0x20(%rdx)
  40351c:	0f 11 42 10          	movups %xmm0,0x10(%rdx)
  403520:	0f 11 02             	movups %xmm0,(%rdx)
  403523:	89 8c 24 64 01 00 00 	mov    %ecx,0x164(%rsp)
  40352a:	89 84 24 60 01 00 00 	mov    %eax,0x160(%rsp)
  403531:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  403538:	00 
  403539:	48 81 c4 98 02 00 00 	add    $0x298,%rsp
  403540:	c3                   	ret
  403541:	48 c7 84 24 58 01 00 	movq   $0x1,0x158(%rsp)
  403548:	00 01 00 00 00 
  40354d:	8b 84 24 a0 01 00 00 	mov    0x1a0(%rsp),%eax
  403554:	25 00 f0 00 00       	and    $0xf000,%eax
  403559:	89 44 24 34          	mov    %eax,0x34(%rsp)
  40355d:	3d 00 60 00 00       	cmp    $0x6000,%eax
  403562:	74 0d                	je     403571 <os::[stat_linux.odin]::_fstat_internal+0x191>
  403564:	8b 44 24 34          	mov    0x34(%rsp),%eax
  403568:	3d 00 20 00 00       	cmp    $0x2000,%eax
  40356d:	74 20                	je     40358f <os::[stat_linux.odin]::_fstat_internal+0x1af>
  40356f:	eb 11                	jmp    403582 <os::[stat_linux.odin]::_fstat_internal+0x1a2>
  403571:	48 c7 84 24 58 01 00 	movq   $0x6,0x158(%rsp)
  403578:	00 06 00 00 00 
  40357d:	e9 95 00 00 00       	jmp    403617 <os::[stat_linux.odin]::_fstat_internal+0x237>
  403582:	8b 44 24 34          	mov    0x34(%rsp),%eax
  403586:	3d 00 40 00 00       	cmp    $0x4000,%eax
  40358b:	74 1d                	je     4035aa <os::[stat_linux.odin]::_fstat_internal+0x1ca>
  40358d:	eb 0e                	jmp    40359d <os::[stat_linux.odin]::_fstat_internal+0x1bd>
  40358f:	48 c7 84 24 58 01 00 	movq   $0x7,0x158(%rsp)
  403596:	00 07 00 00 00 
  40359b:	eb 7a                	jmp    403617 <os::[stat_linux.odin]::_fstat_internal+0x237>
  40359d:	8b 44 24 34          	mov    0x34(%rsp),%eax
  4035a1:	3d 00 10 00 00       	cmp    $0x1000,%eax
  4035a6:	74 1d                	je     4035c5 <os::[stat_linux.odin]::_fstat_internal+0x1e5>
  4035a8:	eb 0e                	jmp    4035b8 <os::[stat_linux.odin]::_fstat_internal+0x1d8>
  4035aa:	48 c7 84 24 58 01 00 	movq   $0x2,0x158(%rsp)
  4035b1:	00 02 00 00 00 
  4035b6:	eb 5f                	jmp    403617 <os::[stat_linux.odin]::_fstat_internal+0x237>
  4035b8:	8b 44 24 34          	mov    0x34(%rsp),%eax
  4035bc:	3d 00 a0 00 00       	cmp    $0xa000,%eax
  4035c1:	74 1d                	je     4035e0 <os::[stat_linux.odin]::_fstat_internal+0x200>
  4035c3:	eb 0e                	jmp    4035d3 <os::[stat_linux.odin]::_fstat_internal+0x1f3>
  4035c5:	48 c7 84 24 58 01 00 	movq   $0x4,0x158(%rsp)
  4035cc:	00 04 00 00 00 
  4035d1:	eb 44                	jmp    403617 <os::[stat_linux.odin]::_fstat_internal+0x237>
  4035d3:	8b 44 24 34          	mov    0x34(%rsp),%eax
  4035d7:	3d 00 80 00 00       	cmp    $0x8000,%eax
  4035dc:	74 1d                	je     4035fb <os::[stat_linux.odin]::_fstat_internal+0x21b>
  4035de:	eb 0e                	jmp    4035ee <os::[stat_linux.odin]::_fstat_internal+0x20e>
  4035e0:	48 c7 84 24 58 01 00 	movq   $0x3,0x158(%rsp)
  4035e7:	00 03 00 00 00 
  4035ec:	eb 29                	jmp    403617 <os::[stat_linux.odin]::_fstat_internal+0x237>
  4035ee:	8b 44 24 34          	mov    0x34(%rsp),%eax
  4035f2:	3d 00 c0 00 00       	cmp    $0xc000,%eax
  4035f7:	74 12                	je     40360b <os::[stat_linux.odin]::_fstat_internal+0x22b>
  4035f9:	eb 0e                	jmp    403609 <os::[stat_linux.odin]::_fstat_internal+0x229>
  4035fb:	48 c7 84 24 58 01 00 	movq   $0x1,0x158(%rsp)
  403602:	00 01 00 00 00 
  403607:	eb 0e                	jmp    403617 <os::[stat_linux.odin]::_fstat_internal+0x237>
  403609:	eb 0c                	jmp    403617 <os::[stat_linux.odin]::_fstat_internal+0x237>
  40360b:	48 c7 84 24 58 01 00 	movq   $0x5,0x158(%rsp)
  403612:	00 05 00 00 00 
  403617:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40361c:	8b 7c 24 5c          	mov    0x5c(%rsp),%edi
  403620:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403625:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40362a:	8b 94 24 a0 01 00 00 	mov    0x1a0(%rsp),%edx
  403631:	81 e2 ff 0f 00 00    	and    $0xfff,%edx
  403637:	89 94 24 54 01 00 00 	mov    %edx,0x154(%rsp)
  40363e:	0f 57 c0             	xorps  %xmm0,%xmm0
  403641:	0f 29 04 24          	movaps %xmm0,(%rsp)
  403645:	0f 29 84 24 40 01 00 	movaps %xmm0,0x140(%rsp)
  40364c:	00 
  40364d:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  403654:	00 
  403655:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  40365c:	00 
  40365d:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  403664:	00 
  403665:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40366c:	00 
  40366d:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  403674:	00 
  403675:	0f 29 84 24 40 01 00 	movaps %xmm0,0x140(%rsp)
  40367c:	00 
  40367d:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  403684:	00 
  403685:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  40368c:	00 
  40368d:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  403694:	00 
  403695:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40369c:	00 
  40369d:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  4036a4:	00 
  4036a5:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  4036ac:	00 
  4036ad:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4036b4:	00 
  4036b5:	48 8b b4 24 e0 00 00 	mov    0xe0(%rsp),%rsi
  4036bc:	00 
  4036bd:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4036c4:	00 
  4036c5:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  4036cc:	00 
  4036cd:	48 8d 8c 24 d0 00 00 	lea    0xd0(%rsp),%rcx
  4036d4:	00 
  4036d5:	e8 36 12 00 00       	call   404910 <os::[path_linux.odin]::_get_full_path>
  4036da:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4036e1:	00 
  4036e2:	8b 84 24 c0 00 00 00 	mov    0xc0(%rsp),%eax
  4036e9:	89 44 24 18          	mov    %eax,0x18(%rsp)
  4036ed:	8b 84 24 c4 00 00 00 	mov    0xc4(%rsp),%eax
  4036f4:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4036f8:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  4036ff:	00 
  403700:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403705:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40370c:	00 
  40370d:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  403712:	83 bc 24 c4 00 00 00 	cmpl   $0x0,0xc4(%rsp)
  403719:	00 
  40371a:	0f 84 9c 00 00 00    	je     4037bc <os::[stat_linux.odin]::_fstat_internal+0x3dc>
  403720:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  403725:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  403729:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40372d:	89 8c 24 18 02 00 00 	mov    %ecx,0x218(%rsp)
  403734:	89 84 24 1c 02 00 00 	mov    %eax,0x21c(%rsp)
  40373b:	8b 84 24 18 02 00 00 	mov    0x218(%rsp),%eax
  403742:	8b 8c 24 1c 02 00 00 	mov    0x21c(%rsp),%ecx
  403749:	89 8c 24 1c 02 00 00 	mov    %ecx,0x21c(%rsp)
  403750:	89 84 24 18 02 00 00 	mov    %eax,0x218(%rsp)
  403757:	0f 28 84 24 20 02 00 	movaps 0x220(%rsp),%xmm0
  40375e:	00 
  40375f:	0f 28 8c 24 30 02 00 	movaps 0x230(%rsp),%xmm1
  403766:	00 
  403767:	0f 28 94 24 40 02 00 	movaps 0x240(%rsp),%xmm2
  40376e:	00 
  40376f:	0f 28 9c 24 50 02 00 	movaps 0x250(%rsp),%xmm3
  403776:	00 
  403777:	0f 28 a4 24 60 02 00 	movaps 0x260(%rsp),%xmm4
  40377e:	00 
  40377f:	0f 28 ac 24 70 02 00 	movaps 0x270(%rsp),%xmm5
  403786:	00 
  403787:	0f 11 6a 50          	movups %xmm5,0x50(%rdx)
  40378b:	0f 11 62 40          	movups %xmm4,0x40(%rdx)
  40378f:	0f 11 5a 30          	movups %xmm3,0x30(%rdx)
  403793:	0f 11 52 20          	movups %xmm2,0x20(%rdx)
  403797:	0f 11 4a 10          	movups %xmm1,0x10(%rdx)
  40379b:	0f 11 02             	movups %xmm0,(%rdx)
  40379e:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  4037a5:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  4037ac:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4037b3:	00 
  4037b4:	48 81 c4 98 02 00 00 	add    $0x298,%rsp
  4037bb:	c3                   	ret
  4037bc:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4037c1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4037c6:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4037cb:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  4037d2:	00 
  4037d3:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  4037da:	00 
  4037db:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4037e2:	00 
  4037e3:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4037ea:	00 
  4037eb:	48 c7 84 24 18 01 00 	movq   $0x0,0x118(%rsp)
  4037f2:	00 00 00 00 00 
  4037f7:	48 8b 84 24 b8 01 00 	mov    0x1b8(%rsp),%rax
  4037fe:	00 
  4037ff:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  403806:	00 
  403807:	8b 84 24 54 01 00 00 	mov    0x154(%rsp),%eax
  40380e:	89 84 24 28 01 00 00 	mov    %eax,0x128(%rsp)
  403815:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  40381c:	00 
  40381d:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  403824:	00 
  403825:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  40382c:	00 00 00 00 00 
  403831:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  403838:	00 00 00 00 00 
  40383d:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  403844:	00 
  403845:	48 8b 94 24 e8 01 00 	mov    0x1e8(%rsp),%rdx
  40384c:	00 
  40384d:	48 69 c0 00 ca 9a 3b 	imul   $0x3b9aca00,%rax,%rax
  403854:	48 01 d0             	add    %rdx,%rax
  403857:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  40385e:	00 
  40385f:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  403866:	00 
  403867:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40386e:	00 
  40386f:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  403876:	00 00 00 00 00 
  40387b:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  403882:	00 00 00 00 00 
  403887:	48 8b 84 24 d0 01 00 	mov    0x1d0(%rsp),%rax
  40388e:	00 
  40388f:	48 8b 94 24 d8 01 00 	mov    0x1d8(%rsp),%rdx
  403896:	00 
  403897:	48 69 c0 00 ca 9a 3b 	imul   $0x3b9aca00,%rax,%rax
  40389e:	48 01 d0             	add    %rdx,%rax
  4038a1:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4038a8:	00 
  4038a9:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  4038b0:	00 
  4038b1:	48 89 84 24 48 01 00 	mov    %rax,0x148(%rsp)
  4038b8:	00 
  4038b9:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  4038c0:	00 00 00 00 00 
  4038c5:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  4038cc:	00 00 00 00 00 
  4038d1:	48 8b 84 24 f0 01 00 	mov    0x1f0(%rsp),%rax
  4038d8:	00 
  4038d9:	48 8b 94 24 f8 01 00 	mov    0x1f8(%rsp),%rdx
  4038e0:	00 
  4038e1:	48 69 c0 00 ca 9a 3b 	imul   $0x3b9aca00,%rax,%rax
  4038e8:	48 01 d0             	add    %rdx,%rax
  4038eb:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  4038f2:	00 
  4038f3:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  4038fa:	00 
  4038fb:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  403902:	00 
  403903:	0f 28 84 24 f0 00 00 	movaps 0xf0(%rsp),%xmm0
  40390a:	00 
  40390b:	0f 28 8c 24 00 01 00 	movaps 0x100(%rsp),%xmm1
  403912:	00 
  403913:	0f 28 94 24 10 01 00 	movaps 0x110(%rsp),%xmm2
  40391a:	00 
  40391b:	0f 28 9c 24 20 01 00 	movaps 0x120(%rsp),%xmm3
  403922:	00 
  403923:	0f 28 a4 24 30 01 00 	movaps 0x130(%rsp),%xmm4
  40392a:	00 
  40392b:	0f 28 ac 24 40 01 00 	movaps 0x140(%rsp),%xmm5
  403932:	00 
  403933:	0f 29 ac 24 70 02 00 	movaps %xmm5,0x270(%rsp)
  40393a:	00 
  40393b:	0f 29 a4 24 60 02 00 	movaps %xmm4,0x260(%rsp)
  403942:	00 
  403943:	0f 29 9c 24 50 02 00 	movaps %xmm3,0x250(%rsp)
  40394a:	00 
  40394b:	0f 29 94 24 40 02 00 	movaps %xmm2,0x240(%rsp)
  403952:	00 
  403953:	0f 29 8c 24 30 02 00 	movaps %xmm1,0x230(%rsp)
  40395a:	00 
  40395b:	0f 29 84 24 20 02 00 	movaps %xmm0,0x220(%rsp)
  403962:	00 
  403963:	48 8b 84 24 70 02 00 	mov    0x270(%rsp),%rax
  40396a:	00 
  40396b:	48 89 84 24 68 02 00 	mov    %rax,0x268(%rsp)
  403972:	00 
  403973:	48 8b bc 24 20 02 00 	mov    0x220(%rsp),%rdi
  40397a:	00 
  40397b:	48 8b b4 24 28 02 00 	mov    0x228(%rsp),%rsi
  403982:	00 
  403983:	0f 57 c0             	xorps  %xmm0,%xmm0
  403986:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40398d:	00 
  40398e:	48 8d 94 24 80 00 00 	lea    0x80(%rsp),%rdx
  403995:	00 
  403996:	e8 35 26 00 00       	call   405fd0 <os::split_path>
  40399b:	48 89 d1             	mov    %rdx,%rcx
  40399e:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4039a3:	48 89 8c 24 38 02 00 	mov    %rcx,0x238(%rsp)
  4039aa:	00 
  4039ab:	48 89 84 24 30 02 00 	mov    %rax,0x230(%rsp)
  4039b2:	00 
  4039b3:	8b 84 24 18 02 00 00 	mov    0x218(%rsp),%eax
  4039ba:	8b 8c 24 1c 02 00 00 	mov    0x21c(%rsp),%ecx
  4039c1:	89 8c 24 1c 02 00 00 	mov    %ecx,0x21c(%rsp)
  4039c8:	89 84 24 18 02 00 00 	mov    %eax,0x218(%rsp)
  4039cf:	0f 28 84 24 20 02 00 	movaps 0x220(%rsp),%xmm0
  4039d6:	00 
  4039d7:	0f 28 8c 24 30 02 00 	movaps 0x230(%rsp),%xmm1
  4039de:	00 
  4039df:	0f 28 94 24 40 02 00 	movaps 0x240(%rsp),%xmm2
  4039e6:	00 
  4039e7:	0f 28 9c 24 50 02 00 	movaps 0x250(%rsp),%xmm3
  4039ee:	00 
  4039ef:	0f 28 a4 24 60 02 00 	movaps 0x260(%rsp),%xmm4
  4039f6:	00 
  4039f7:	0f 28 ac 24 70 02 00 	movaps 0x270(%rsp),%xmm5
  4039fe:	00 
  4039ff:	0f 11 6a 50          	movups %xmm5,0x50(%rdx)
  403a03:	0f 11 62 40          	movups %xmm4,0x40(%rdx)
  403a07:	0f 11 5a 30          	movups %xmm3,0x30(%rdx)
  403a0b:	0f 11 52 20          	movups %xmm2,0x20(%rdx)
  403a0f:	0f 11 4a 10          	movups %xmm1,0x10(%rdx)
  403a13:	0f 11 02             	movups %xmm0,(%rdx)
  403a16:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  403a1a:	89 44 24 70          	mov    %eax,0x70(%rsp)
  403a1e:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403a23:	48 81 c4 98 02 00 00 	add    $0x298,%rsp
  403a2a:	c3                   	ret
  403a2b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403a30 <os::heap_allocator_proc>:
  403a30:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  403a37:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  403a3c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403a41:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  403a46:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403a4b:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403a50:	40 88 f0             	mov    %sil,%al
  403a53:	88 44 24 47          	mov    %al,0x47(%rsp)
  403a57:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  403a5e:	00 
  403a5f:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403a64:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  403a6b:	00 
  403a6c:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403a71:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403a78:	00 
  403a79:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403a7e:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  403a83:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403a88:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403a8d:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  403a92:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403a97:	8a 44 24 47          	mov    0x47(%rsp),%al
  403a9b:	4c 8b 54 24 58       	mov    0x58(%rsp),%r10
  403aa0:	4c 8b 5c 24 48       	mov    0x48(%rsp),%r11
  403aa5:	48 89 bc 24 a0 00 00 	mov    %rdi,0xa0(%rsp)
  403aac:	00 
  403aad:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  403ab4:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  403abb:	00 
  403abc:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  403ac3:	00 
  403ac4:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  403acb:	00 
  403acc:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  403ad1:	0f 57 c0             	xorps  %xmm0,%xmm0
  403ad4:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  403ad9:	48 89 e6             	mov    %rsp,%rsi
  403adc:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  403ae0:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  403ae5:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  403ae9:	4c 89 16             	mov    %r10,(%rsi)
  403aec:	0f b6 f0             	movzbl %al,%esi
  403aef:	e8 1c 78 00 00       	call   40b310 <runtime::heap_allocator_proc>
  403af4:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  403af9:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  403afe:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  403b03:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403b07:	48 89 11             	mov    %rdx,(%rcx)
  403b0a:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403b11:	c3                   	ret
  403b12:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403b19:	1f 84 00 00 00 00 00 

0000000000403b20 <os::[path_linux.odin]::_is_path_separator>:
  403b20:	40 88 f8             	mov    %dil,%al
  403b23:	88 44 24 fe          	mov    %al,-0x2(%rsp)
  403b27:	8a 44 24 fe          	mov    -0x2(%rsp),%al
  403b2b:	88 44 24 ff          	mov    %al,-0x1(%rsp)
  403b2f:	3c 2f                	cmp    $0x2f,%al
  403b31:	0f 94 c0             	sete   %al
  403b34:	24 01                	and    $0x1,%al
  403b36:	c3                   	ret
  403b37:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  403b3e:	00 00 

0000000000403b40 <os::[path_posixfs.odin]::_split_path>:
  403b40:	48 81 ec 58 01 00 00 	sub    $0x158,%rsp
  403b47:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  403b4e:	00 
  403b4f:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  403b56:	00 
  403b57:	48 89 b4 24 b8 00 00 	mov    %rsi,0xb8(%rsp)
  403b5e:	00 
  403b5f:	48 89 bc 24 c0 00 00 	mov    %rdi,0xc0(%rsp)
  403b66:	00 
  403b67:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  403b6e:	00 
  403b6f:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403b76:	00 
  403b77:	48 89 8c 24 48 01 00 	mov    %rcx,0x148(%rsp)
  403b7e:	00 
  403b7f:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  403b86:	00 
  403b87:	48 8d bc 24 30 01 00 	lea    0x130(%rsp),%rdi
  403b8e:	00 
  403b8f:	31 f6                	xor    %esi,%esi
  403b91:	ba 10 00 00 00       	mov    $0x10,%edx
  403b96:	e8 a5 d4 ff ff       	call   401040 <memset@plt>
  403b9b:	48 8d bc 24 20 01 00 	lea    0x120(%rsp),%rdi
  403ba2:	00 
  403ba3:	31 f6                	xor    %esi,%esi
  403ba5:	ba 10 00 00 00       	mov    $0x10,%edx
  403baa:	e8 91 d4 ff ff       	call   401040 <memset@plt>
  403baf:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  403bb6:	00 
  403bb7:	48 83 e8 01          	sub    $0x1,%rax
  403bbb:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  403bc2:	00 
  403bc3:	48 83 bc 24 18 01 00 	cmpq   $0x0,0x118(%rsp)
  403bca:	00 00 
  403bcc:	0f 9d c0             	setge  %al
  403bcf:	24 01                	and    $0x1,%al
  403bd1:	3c 00                	cmp    $0x0,%al
  403bd3:	74 6c                	je     403c41 <os::[path_posixfs.odin]::_split_path+0x101>
  403bd5:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  403bdc:	00 
  403bdd:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  403be4:	00 
  403be5:	4c 89 84 24 a0 00 00 	mov    %r8,0xa0(%rsp)
  403bec:	00 
  403bed:	bf 98 e2 40 00       	mov    $0x40e298,%edi
  403bf2:	be 27 00 00 00       	mov    $0x27,%esi
  403bf7:	ba 30 00 00 00       	mov    $0x30,%edx
  403bfc:	b9 29 00 00 00       	mov    $0x29,%ecx
  403c01:	e8 4a 6a 00 00       	call   40a650 <runtime::bounds_check_error>
  403c06:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403c0d:	00 
  403c0e:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  403c15:	00 
  403c16:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403c1d:	00 
  403c1e:	0f b6 3c 08          	movzbl (%rax,%rcx,1),%edi
  403c22:	e8 f9 fe ff ff       	call   403b20 <os::[path_linux.odin]::_is_path_separator>
  403c27:	3c 00                	cmp    $0x0,%al
  403c29:	75 16                	jne    403c41 <os::[path_posixfs.odin]::_split_path+0x101>
  403c2b:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  403c32:	00 
  403c33:	48 83 e8 01          	sub    $0x1,%rax
  403c37:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  403c3e:	00 
  403c3f:	eb 82                	jmp    403bc3 <os::[path_posixfs.odin]::_split_path+0x83>
  403c41:	48 83 bc 24 18 01 00 	cmpq   $0x0,0x118(%rsp)
  403c48:	00 00 
  403c4a:	0f 94 c0             	sete   %al
  403c4d:	24 01                	and    $0x1,%al
  403c4f:	3c 00                	cmp    $0x0,%al
  403c51:	0f 84 6e 01 00 00    	je     403dc5 <os::[path_posixfs.odin]::_split_path+0x285>
  403c57:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  403c5e:	00 
  403c5f:	49 ff c0             	inc    %r8
  403c62:	4c 89 44 24 58       	mov    %r8,0x58(%rsp)
  403c67:	4c 8b 8c 24 50 01 00 	mov    0x150(%rsp),%r9
  403c6e:	00 
  403c6f:	bf 98 e2 40 00       	mov    $0x40e298,%edi
  403c74:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  403c79:	be 27 00 00 00       	mov    $0x27,%esi
  403c7e:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  403c83:	ba 34 00 00 00       	mov    $0x34,%edx
  403c88:	89 54 24 74          	mov    %edx,0x74(%rsp)
  403c8c:	b9 0e 00 00 00       	mov    $0xe,%ecx
  403c91:	e8 ea 6c 00 00       	call   40a980 <runtime::slice_expr_error_hi>
  403c96:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  403c9b:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  403ca0:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  403ca5:	8b 54 24 74          	mov    0x74(%rsp),%edx
  403ca9:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  403cb0:	00 
  403cb1:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  403cb8:	00 
  403cb9:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  403cc0:	00 
  403cc1:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  403cc8:	00 
  403cc9:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  403cd0:	00 
  403cd1:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  403cd8:	00 
  403cd9:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  403ce0:	00 
  403ce1:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  403ce8:	00 
  403ce9:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  403cee:	49 ff c0             	inc    %r8
  403cf1:	4c 89 84 24 88 00 00 	mov    %r8,0x88(%rsp)
  403cf8:	00 
  403cf9:	4c 8b 8c 24 50 01 00 	mov    0x150(%rsp),%r9
  403d00:	00 
  403d01:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  403d08:	00 
  403d09:	48 89 e0             	mov    %rsp,%rax
  403d0c:	4c 89 08             	mov    %r9,(%rax)
  403d0f:	b9 1a 00 00 00       	mov    $0x1a,%ecx
  403d14:	e8 17 6d 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  403d19:	4c 8b 4c 24 78       	mov    0x78(%rsp),%r9
  403d1e:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403d25:	00 
  403d26:	4c 8b 84 24 88 00 00 	mov    0x88(%rsp),%r8
  403d2d:	00 
  403d2e:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  403d35:	00 
  403d36:	48 8b b4 24 98 00 00 	mov    0x98(%rsp),%rsi
  403d3d:	00 
  403d3e:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  403d45:	00 
  403d46:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  403d4d:	00 
  403d4e:	4a 8d 4c 09 01       	lea    0x1(%rcx,%r9,1),%rcx
  403d53:	4c 29 c0             	sub    %r8,%rax
  403d56:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  403d5d:	00 
  403d5e:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  403d65:	00 
  403d66:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  403d6d:	00 
  403d6e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  403d75:	00 
  403d76:	48 89 bc 24 38 01 00 	mov    %rdi,0x138(%rsp)
  403d7d:	00 
  403d7e:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  403d85:	00 
  403d86:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  403d8d:	00 
  403d8e:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  403d95:	00 
  403d96:	48 89 7a 08          	mov    %rdi,0x8(%rdx)
  403d9a:	48 89 32             	mov    %rsi,(%rdx)
  403d9d:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  403da4:	00 
  403da5:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  403dac:	00 
  403dad:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  403db4:	00 
  403db5:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  403dbc:	00 
  403dbd:	48 81 c4 58 01 00 00 	add    $0x158,%rsp
  403dc4:	c3                   	ret
  403dc5:	48 83 bc 24 18 01 00 	cmpq   $0x0,0x118(%rsp)
  403dcc:	00 00 
  403dce:	0f 9f c0             	setg   %al
  403dd1:	24 01                	and    $0x1,%al
  403dd3:	3c 00                	cmp    $0x0,%al
  403dd5:	0f 84 53 01 00 00    	je     403f2e <os::[path_posixfs.odin]::_split_path+0x3ee>
  403ddb:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  403de2:	00 
  403de3:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403de8:	4c 8b 8c 24 50 01 00 	mov    0x150(%rsp),%r9
  403def:	00 
  403df0:	bf 98 e2 40 00       	mov    $0x40e298,%edi
  403df5:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403dfa:	be 27 00 00 00       	mov    $0x27,%esi
  403dff:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  403e04:	ba 36 00 00 00       	mov    $0x36,%edx
  403e09:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  403e0d:	b9 0e 00 00 00       	mov    $0xe,%ecx
  403e12:	e8 69 6b 00 00       	call   40a980 <runtime::slice_expr_error_hi>
  403e17:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403e1c:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403e21:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403e26:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  403e2a:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  403e31:	00 
  403e32:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  403e39:	00 
  403e3a:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403e41:	00 
  403e42:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403e49:	00 
  403e4a:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403e4f:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  403e56:	00 
  403e57:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403e5c:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  403e63:	00 
  403e64:	4c 89 44 24 30       	mov    %r8,0x30(%rsp)
  403e69:	49 ff c0             	inc    %r8
  403e6c:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  403e71:	4c 8b 8c 24 50 01 00 	mov    0x150(%rsp),%r9
  403e78:	00 
  403e79:	4c 89 4c 24 38       	mov    %r9,0x38(%rsp)
  403e7e:	48 89 e0             	mov    %rsp,%rax
  403e81:	4c 89 08             	mov    %r9,(%rax)
  403e84:	b9 18 00 00 00       	mov    $0x18,%ecx
  403e89:	e8 a2 6b 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  403e8e:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  403e93:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403e98:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  403e9d:	48 8b 7c 24 48       	mov    0x48(%rsp),%rdi
  403ea2:	48 8b 74 24 50       	mov    0x50(%rsp),%rsi
  403ea7:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  403eae:	00 
  403eaf:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  403eb6:	00 
  403eb7:	4a 8d 4c 09 01       	lea    0x1(%rcx,%r9,1),%rcx
  403ebc:	4c 29 c0             	sub    %r8,%rax
  403ebf:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  403ec6:	00 
  403ec7:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  403ece:	00 
  403ecf:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  403ed6:	00 
  403ed7:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  403ede:	00 
  403edf:	48 89 bc 24 38 01 00 	mov    %rdi,0x138(%rsp)
  403ee6:	00 
  403ee7:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  403eee:	00 
  403eef:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  403ef6:	00 
  403ef7:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  403efe:	00 
  403eff:	48 89 7a 08          	mov    %rdi,0x8(%rdx)
  403f03:	48 89 32             	mov    %rsi,(%rdx)
  403f06:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  403f0d:	00 
  403f0e:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  403f15:	00 
  403f16:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  403f1d:	00 
  403f1e:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  403f25:	00 
  403f26:	48 81 c4 58 01 00 00 	add    $0x158,%rsp
  403f2d:	c3                   	ret
  403f2e:	eb 00                	jmp    403f30 <os::[path_posixfs.odin]::_split_path+0x3f0>
  403f30:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  403f37:	00 
  403f38:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  403f3f:	00 
  403f40:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  403f47:	00 
  403f48:	0f 57 c0             	xorps  %xmm0,%xmm0
  403f4b:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  403f52:	00 
  403f53:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  403f5a:	00 
  403f5b:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  403f62:	00 
  403f63:	0f 11 02             	movups %xmm0,(%rdx)
  403f66:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  403f6d:	00 
  403f6e:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  403f75:	00 
  403f76:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  403f7d:	00 
  403f7e:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  403f85:	00 
  403f86:	48 81 c4 58 01 00 00 	add    $0x158,%rsp
  403f8d:	c3                   	ret
  403f8e:	66 90                	xchg   %ax,%ax

0000000000403f90 <os::[file_stream.odin]::file_stream_fstat_utility>:
  403f90:	55                   	push   %rbp
  403f91:	41 57                	push   %r15
  403f93:	41 56                	push   %r14
  403f95:	41 55                	push   %r13
  403f97:	41 54                	push   %r12
  403f99:	53                   	push   %rbx
  403f9a:	48 81 ec 88 01 00 00 	sub    $0x188,%rsp
  403fa1:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  403fa6:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403fab:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  403fb0:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  403fb5:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  403fba:	48 89 74 24 58       	mov    %rsi,0x58(%rsp)
  403fbf:	48 8b 74 24 50       	mov    0x50(%rsp),%rsi
  403fc4:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403fc9:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403fce:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  403fd3:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403fd8:	48 89 bc 24 80 01 00 	mov    %rdi,0x180(%rsp)
  403fdf:	00 
  403fe0:	48 89 b4 24 78 01 00 	mov    %rsi,0x178(%rsp)
  403fe7:	00 
  403fe8:	48 89 94 24 70 01 00 	mov    %rdx,0x170(%rsp)
  403fef:	00 
  403ff0:	48 89 8c 24 68 01 00 	mov    %rcx,0x168(%rsp)
  403ff7:	00 
  403ff8:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  403fff:	00 
  404000:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  404007:	00 
  404008:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40400d:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  404014:	00 
  404015:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40401a:	48 c7 84 24 58 01 00 	movq   $0x0,0x158(%rsp)
  404021:	00 00 00 00 00 
  404026:	48 8d bc 24 f0 00 00 	lea    0xf0(%rsp),%rdi
  40402d:	00 
  40402e:	31 f6                	xor    %esi,%esi
  404030:	ba 60 00 00 00       	mov    $0x60,%edx
  404035:	e8 06 d0 ff ff       	call   401040 <memset@plt>
  40403a:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40403f:	48 83 f8 60          	cmp    $0x60,%rax
  404043:	0f 9d c0             	setge  %al
  404046:	24 01                	and    $0x1,%al
  404048:	3c 00                	cmp    $0x0,%al
  40404a:	0f 84 c5 01 00 00    	je     404215 <os::[file_stream.odin]::file_stream_fstat_utility+0x285>
  404050:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  404055:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40405a:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40405f:	48 8b bc 24 80 01 00 	mov    0x180(%rsp),%rdi
  404066:	00 
  404067:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  40406e:	00 
  40406f:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  404076:	00 
  404077:	48 8b b4 24 e0 00 00 	mov    0xe0(%rsp),%rsi
  40407e:	00 
  40407f:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  404086:	00 
  404087:	0f 57 c0             	xorps  %xmm0,%xmm0
  40408a:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  404091:	00 
  404092:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  404099:	00 
  40409a:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  4040a1:	00 
  4040a2:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4040a9:	00 
  4040aa:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4040b1:	00 
  4040b2:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  4040b9:	00 
  4040ba:	48 8d 8c 24 80 00 00 	lea    0x80(%rsp),%rcx
  4040c1:	00 
  4040c2:	e8 b9 f1 ff ff       	call   403280 <os::[stat_linux.odin]::_fstat>
  4040c7:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4040cc:	8b 44 24 70          	mov    0x70(%rsp),%eax
  4040d0:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4040d4:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  4040d8:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  4040df:	00 
  4040e0:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  4040e7:	00 
  4040e8:	4c 8b 84 24 90 00 00 	mov    0x90(%rsp),%r8
  4040ef:	00 
  4040f0:	4c 8b 8c 24 98 00 00 	mov    0x98(%rsp),%r9
  4040f7:	00 
  4040f8:	4c 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%r10
  4040ff:	00 
  404100:	44 8b 9c 24 b8 00 00 	mov    0xb8(%rsp),%r11d
  404107:	00 
  404108:	8a 9c 24 bc 00 00 00 	mov    0xbc(%rsp),%bl
  40410f:	40 8a ac 24 bd 00 00 	mov    0xbd(%rsp),%bpl
  404116:	00 
  404117:	44 8a b4 24 be 00 00 	mov    0xbe(%rsp),%r14b
  40411e:	00 
  40411f:	44 8a bc 24 bf 00 00 	mov    0xbf(%rsp),%r15b
  404126:	00 
  404127:	4c 8b a4 24 c0 00 00 	mov    0xc0(%rsp),%r12
  40412e:	00 
  40412f:	4c 8b ac 24 c8 00 00 	mov    0xc8(%rsp),%r13
  404136:	00 
  404137:	48 8b bc 24 d0 00 00 	mov    0xd0(%rsp),%rdi
  40413e:	00 
  40413f:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404146:	00 
  404147:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40414c:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  404153:	00 
  404154:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404159:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  404160:	00 
  404161:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  404168:	00 
  404169:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40416e:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  404175:	00 
  404176:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40417b:	48 89 84 24 48 01 00 	mov    %rax,0x148(%rsp)
  404182:	00 
  404183:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  404187:	48 89 bc 24 40 01 00 	mov    %rdi,0x140(%rsp)
  40418e:	00 
  40418f:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  404194:	4c 89 ac 24 38 01 00 	mov    %r13,0x138(%rsp)
  40419b:	00 
  40419c:	4c 89 a4 24 30 01 00 	mov    %r12,0x130(%rsp)
  4041a3:	00 
  4041a4:	44 88 bc 24 2f 01 00 	mov    %r15b,0x12f(%rsp)
  4041ab:	00 
  4041ac:	44 88 b4 24 2e 01 00 	mov    %r14b,0x12e(%rsp)
  4041b3:	00 
  4041b4:	40 88 ac 24 2d 01 00 	mov    %bpl,0x12d(%rsp)
  4041bb:	00 
  4041bc:	88 9c 24 2c 01 00 00 	mov    %bl,0x12c(%rsp)
  4041c3:	44 89 9c 24 28 01 00 	mov    %r11d,0x128(%rsp)
  4041ca:	00 
  4041cb:	4c 89 94 24 20 01 00 	mov    %r10,0x120(%rsp)
  4041d2:	00 
  4041d3:	4c 89 8c 24 08 01 00 	mov    %r9,0x108(%rsp)
  4041da:	00 
  4041db:	4c 89 84 24 00 01 00 	mov    %r8,0x100(%rsp)
  4041e2:	00 
  4041e3:	48 89 b4 24 f8 00 00 	mov    %rsi,0xf8(%rsp)
  4041ea:	00 
  4041eb:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  4041f2:	00 
  4041f3:	89 8c 24 5c 01 00 00 	mov    %ecx,0x15c(%rsp)
  4041fa:	89 84 24 58 01 00 00 	mov    %eax,0x158(%rsp)
  404201:	48 8d b4 24 f0 00 00 	lea    0xf0(%rsp),%rsi
  404208:	00 
  404209:	ba 60 00 00 00       	mov    $0x60,%edx
  40420e:	e8 bd 81 00 00       	call   40c3d0 <runtime::mem_copy_non_overlapping>
  404213:	eb 49                	jmp    40425e <os::[file_stream.odin]::file_stream_fstat_utility+0x2ce>
  404215:	48 c7 44 24 68 00 00 	movq   $0x0,0x68(%rsp)
  40421c:	00 00 
  40421e:	31 c0                	xor    %eax,%eax
  404220:	a8 01                	test   $0x1,%al
  404222:	75 02                	jne    404226 <os::[file_stream.odin]::file_stream_fstat_utility+0x296>
  404224:	eb 12                	jmp    404238 <os::[file_stream.odin]::file_stream_fstat_utility+0x2a8>
  404226:	c7 44 24 6c 00 00 00 	movl   $0x0,0x6c(%rsp)
  40422d:	00 
  40422e:	c7 44 24 68 00 00 00 	movl   $0x0,0x68(%rsp)
  404235:	00 
  404236:	eb 10                	jmp    404248 <os::[file_stream.odin]::file_stream_fstat_utility+0x2b8>
  404238:	c7 44 24 68 05 00 00 	movl   $0x5,0x68(%rsp)
  40423f:	00 
  404240:	c7 44 24 6c 02 00 00 	movl   $0x2,0x6c(%rsp)
  404247:	00 
  404248:	8b 44 24 68          	mov    0x68(%rsp),%eax
  40424c:	8b 4c 24 6c          	mov    0x6c(%rsp),%ecx
  404250:	89 8c 24 5c 01 00 00 	mov    %ecx,0x15c(%rsp)
  404257:	89 84 24 58 01 00 00 	mov    %eax,0x158(%rsp)
  40425e:	8b 84 24 58 01 00 00 	mov    0x158(%rsp),%eax
  404265:	8b 8c 24 5c 01 00 00 	mov    0x15c(%rsp),%ecx
  40426c:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  404270:	89 44 24 60          	mov    %eax,0x60(%rsp)
  404274:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  404279:	48 81 c4 88 01 00 00 	add    $0x188,%rsp
  404280:	5b                   	pop    %rbx
  404281:	41 5c                	pop    %r12
  404283:	41 5d                	pop    %r13
  404285:	41 5e                	pop    %r14
  404287:	41 5f                	pop    %r15
  404289:	5d                   	pop    %rbp
  40428a:	c3                   	ret
  40428b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000404290 <os::[file_linux.odin]::_destroy>:
  404290:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  404297:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40429c:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4042a1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4042a6:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4042ad:	00 
  4042ae:	48 83 f8 00          	cmp    $0x0,%rax
  4042b2:	0f 94 c0             	sete   %al
  4042b5:	24 01                	and    $0x1,%al
  4042b7:	3c 00                	cmp    $0x0,%al
  4042b9:	74 26                	je     4042e1 <os::[file_linux.odin]::_destroy+0x51>
  4042bb:	c7 84 24 d4 00 00 00 	movl   $0x0,0xd4(%rsp)
  4042c2:	00 00 00 00 
  4042c6:	c7 84 24 d0 00 00 00 	movl   $0x0,0xd0(%rsp)
  4042cd:	00 00 00 00 
  4042d1:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  4042d8:	00 
  4042d9:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4042e0:	c3                   	ret
  4042e1:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  4042e6:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  4042ed:	00 
  4042ee:	48 8b 41 30          	mov    0x30(%rcx),%rax
  4042f2:	48 8b 49 38          	mov    0x38(%rcx),%rcx
  4042f6:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  4042fd:	00 
  4042fe:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  404305:	00 
  404306:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40430d:	00 
  40430e:	48 8b 78 18          	mov    0x18(%rax),%rdi
  404312:	48 8b 70 20          	mov    0x20(%rax),%rsi
  404316:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40431d:	00 
  40431e:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  404325:	00 
  404326:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40432d:	00 
  40432e:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  404335:	00 
  404336:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  40433d:	00 
  40433e:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404345:	00 
  404346:	41 b8 f0 e2 40 00    	mov    $0x40e2f0,%r8d
  40434c:	e8 4f 7a 00 00       	call   40bda0 <runtime::delete_string>
  404351:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  404356:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  40435d:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  404364:	00 
  404365:	48 8b 78 40          	mov    0x40(%rax),%rdi
  404369:	48 8b 70 48          	mov    0x48(%rax),%rsi
  40436d:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404374:	00 
  404375:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  40437c:	00 
  40437d:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  404384:	00 
  404385:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40438c:	00 
  40438d:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  404394:	00 
  404395:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40439c:	00 
  40439d:	41 b8 20 e3 40 00    	mov    $0x40e320,%r8d
  4043a3:	e8 78 7a 00 00       	call   40be20 <runtime::delete_slice:proc(array:[]u8,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  4043a8:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4043ad:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  4043b2:	88 84 24 8f 00 00 00 	mov    %al,0x8f(%rsp)
  4043b9:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4043c0:	00 
  4043c1:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  4043c8:	00 
  4043c9:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  4043ce:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4043d3:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4043d8:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  4043dd:	b9 50 e3 40 00       	mov    $0x40e350,%ecx
  4043e2:	e8 b9 82 00 00       	call   40c6a0 <runtime::mem_free>
  4043e7:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  4043eb:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  4043f2:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4043f6:	3c 00                	cmp    $0x0,%al
  4043f8:	74 50                	je     40444a <os::[file_linux.odin]::_destroy+0x1ba>
  4043fa:	8a 44 24 0f          	mov    0xf(%rsp),%al
  4043fe:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  404405:	00 00 
  404407:	3c 00                	cmp    $0x0,%al
  404409:	75 12                	jne    40441d <os::[file_linux.odin]::_destroy+0x18d>
  40440b:	c7 44 24 64 00 00 00 	movl   $0x0,0x64(%rsp)
  404412:	00 
  404413:	c7 44 24 60 00 00 00 	movl   $0x0,0x60(%rsp)
  40441a:	00 
  40441b:	eb 10                	jmp    40442d <os::[file_linux.odin]::_destroy+0x19d>
  40441d:	8a 44 24 0f          	mov    0xf(%rsp),%al
  404421:	88 44 24 60          	mov    %al,0x60(%rsp)
  404425:	c7 44 24 64 03 00 00 	movl   $0x3,0x64(%rsp)
  40442c:	00 
  40442d:	8b 44 24 60          	mov    0x60(%rsp),%eax
  404431:	8b 4c 24 64          	mov    0x64(%rsp),%ecx
  404435:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  404439:	89 44 24 50          	mov    %eax,0x50(%rsp)
  40443d:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  404442:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  404449:	c3                   	ret
  40444a:	8a 84 24 8f 00 00 00 	mov    0x8f(%rsp),%al
  404451:	88 44 24 0e          	mov    %al,0xe(%rsp)
  404455:	3c 00                	cmp    $0x0,%al
  404457:	74 50                	je     4044a9 <os::[file_linux.odin]::_destroy+0x219>
  404459:	8a 44 24 0e          	mov    0xe(%rsp),%al
  40445d:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  404464:	00 00 
  404466:	3c 00                	cmp    $0x0,%al
  404468:	75 12                	jne    40447c <os::[file_linux.odin]::_destroy+0x1ec>
  40446a:	c7 44 24 4c 00 00 00 	movl   $0x0,0x4c(%rsp)
  404471:	00 
  404472:	c7 44 24 48 00 00 00 	movl   $0x0,0x48(%rsp)
  404479:	00 
  40447a:	eb 10                	jmp    40448c <os::[file_linux.odin]::_destroy+0x1fc>
  40447c:	8a 44 24 0e          	mov    0xe(%rsp),%al
  404480:	88 44 24 48          	mov    %al,0x48(%rsp)
  404484:	c7 44 24 4c 03 00 00 	movl   $0x3,0x4c(%rsp)
  40448b:	00 
  40448c:	8b 44 24 48          	mov    0x48(%rsp),%eax
  404490:	8b 4c 24 4c          	mov    0x4c(%rsp),%ecx
  404494:	89 4c 24 44          	mov    %ecx,0x44(%rsp)
  404498:	89 44 24 40          	mov    %eax,0x40(%rsp)
  40449c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4044a1:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4044a8:	c3                   	ret
  4044a9:	8a 44 24 6f          	mov    0x6f(%rsp),%al
  4044ad:	88 44 24 0d          	mov    %al,0xd(%rsp)
  4044b1:	3c 00                	cmp    $0x0,%al
  4044b3:	74 50                	je     404505 <os::[file_linux.odin]::_destroy+0x275>
  4044b5:	8a 44 24 0d          	mov    0xd(%rsp),%al
  4044b9:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  4044c0:	00 00 
  4044c2:	3c 00                	cmp    $0x0,%al
  4044c4:	75 12                	jne    4044d8 <os::[file_linux.odin]::_destroy+0x248>
  4044c6:	c7 44 24 3c 00 00 00 	movl   $0x0,0x3c(%rsp)
  4044cd:	00 
  4044ce:	c7 44 24 38 00 00 00 	movl   $0x0,0x38(%rsp)
  4044d5:	00 
  4044d6:	eb 10                	jmp    4044e8 <os::[file_linux.odin]::_destroy+0x258>
  4044d8:	8a 44 24 0d          	mov    0xd(%rsp),%al
  4044dc:	88 44 24 38          	mov    %al,0x38(%rsp)
  4044e0:	c7 44 24 3c 03 00 00 	movl   $0x3,0x3c(%rsp)
  4044e7:	00 
  4044e8:	8b 44 24 38          	mov    0x38(%rsp),%eax
  4044ec:	8b 4c 24 3c          	mov    0x3c(%rsp),%ecx
  4044f0:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  4044f4:	89 44 24 30          	mov    %eax,0x30(%rsp)
  4044f8:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4044fd:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  404504:	c3                   	ret
  404505:	c7 44 24 24 00 00 00 	movl   $0x0,0x24(%rsp)
  40450c:	00 
  40450d:	c7 44 24 20 00 00 00 	movl   $0x0,0x20(%rsp)
  404514:	00 
  404515:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40451a:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  404521:	c3                   	ret
  404522:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404529:	1f 84 00 00 00 00 00 

0000000000404530 <os::[file_linux.odin]::_close>:
  404530:	48 83 ec 78          	sub    $0x78,%rsp
  404534:	48 89 3c 24          	mov    %rdi,(%rsp)
  404538:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40453d:	48 8b 04 24          	mov    (%rsp),%rax
  404541:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  404546:	48 83 f8 00          	cmp    $0x0,%rax
  40454a:	0f 94 c0             	sete   %al
  40454d:	24 01                	and    $0x1,%al
  40454f:	3c 00                	cmp    $0x0,%al
  404551:	74 1a                	je     40456d <os::[file_linux.odin]::_close+0x3d>
  404553:	c7 44 24 64 00 00 00 	movl   $0x0,0x64(%rsp)
  40455a:	00 
  40455b:	c7 44 24 60 00 00 00 	movl   $0x0,0x60(%rsp)
  404562:	00 
  404563:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  404568:	48 83 c4 78          	add    $0x78,%rsp
  40456c:	c3                   	ret
  40456d:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  404572:	8b 78 28             	mov    0x28(%rax),%edi
  404575:	e8 e6 e4 ff ff       	call   402a60 <linux::close>
  40457a:	89 44 24 5c          	mov    %eax,0x5c(%rsp)
  40457e:	83 7c 24 5c 09       	cmpl   $0x9,0x5c(%rsp)
  404583:	0f 94 c0             	sete   %al
  404586:	24 01                	and    $0x1,%al
  404588:	3c 00                	cmp    $0x0,%al
  40458a:	74 2d                	je     4045b9 <os::[file_linux.odin]::_close+0x89>
  40458c:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404591:	8b 7c 24 5c          	mov    0x5c(%rsp),%edi
  404595:	e8 86 11 00 00       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  40459a:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40459f:	8b 44 24 50          	mov    0x50(%rsp),%eax
  4045a3:	8b 4c 24 54          	mov    0x54(%rsp),%ecx
  4045a7:	89 4c 24 44          	mov    %ecx,0x44(%rsp)
  4045ab:	89 44 24 40          	mov    %eax,0x40(%rsp)
  4045af:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4045b4:	48 83 c4 78          	add    $0x78,%rsp
  4045b8:	c3                   	ret
  4045b9:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4045be:	48 8b 3c 24          	mov    (%rsp),%rdi
  4045c2:	e8 c9 fc ff ff       	call   404290 <os::[file_linux.odin]::_destroy>
  4045c7:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4045cc:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4045d1:	8b 7c 24 5c          	mov    0x5c(%rsp),%edi
  4045d5:	e8 46 11 00 00       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  4045da:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4045df:	8b 44 24 20          	mov    0x20(%rsp),%eax
  4045e3:	8b 4c 24 24          	mov    0x24(%rsp),%ecx
  4045e7:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  4045eb:	89 44 24 10          	mov    %eax,0x10(%rsp)
  4045ef:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4045f4:	48 83 c4 78          	add    $0x78,%rsp
  4045f8:	c3                   	ret
  4045f9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000404600 <os::[file_linux.odin]::_seek>:
  404600:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  404607:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40460c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  404611:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  404616:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40461b:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  404620:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  404625:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40462a:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40462f:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  404636:	00 
  404637:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  40463e:	00 
  40463f:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  404646:	00 
  404647:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  40464e:	00 00 00 00 00 
  404653:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  40465a:	00 00 00 00 00 
  40465f:	48 83 e8 02          	sub    $0x2,%rax
  404663:	77 07                	ja     40466c <os::[file_linux.odin]::_seek+0x6c>
  404665:	eb 00                	jmp    404667 <os::[file_linux.odin]::_seek+0x67>
  404667:	e9 e4 00 00 00       	jmp    404750 <os::[file_linux.odin]::_seek+0x150>
  40466c:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  404673:	00 00 00 00 00 
  404678:	31 c0                	xor    %eax,%eax
  40467a:	a8 01                	test   $0x1,%al
  40467c:	75 02                	jne    404680 <os::[file_linux.odin]::_seek+0x80>
  40467e:	eb 18                	jmp    404698 <os::[file_linux.odin]::_seek+0x98>
  404680:	c7 84 24 ac 00 00 00 	movl   $0x0,0xac(%rsp)
  404687:	00 00 00 00 
  40468b:	c7 84 24 a8 00 00 00 	movl   $0x0,0xa8(%rsp)
  404692:	00 00 00 00 
  404696:	eb 16                	jmp    4046ae <os::[file_linux.odin]::_seek+0xae>
  404698:	c7 84 24 a8 00 00 00 	movl   $0x7,0xa8(%rsp)
  40469f:	07 00 00 00 
  4046a3:	c7 84 24 ac 00 00 00 	movl   $0x2,0xac(%rsp)
  4046aa:	02 00 00 00 
  4046ae:	8b 84 24 a8 00 00 00 	mov    0xa8(%rsp),%eax
  4046b5:	8b 8c 24 ac 00 00 00 	mov    0xac(%rsp),%ecx
  4046bc:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  4046c3:	00 00 00 00 00 
  4046c8:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  4046cf:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  4046d6:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  4046dd:	00 00 00 00 00 
  4046e2:	31 c0                	xor    %eax,%eax
  4046e4:	a8 01                	test   $0x1,%al
  4046e6:	75 02                	jne    4046ea <os::[file_linux.odin]::_seek+0xea>
  4046e8:	eb 18                	jmp    404702 <os::[file_linux.odin]::_seek+0x102>
  4046ea:	c7 84 24 a4 00 00 00 	movl   $0x0,0xa4(%rsp)
  4046f1:	00 00 00 00 
  4046f5:	c7 84 24 a0 00 00 00 	movl   $0x0,0xa0(%rsp)
  4046fc:	00 00 00 00 
  404700:	eb 16                	jmp    404718 <os::[file_linux.odin]::_seek+0x118>
  404702:	c7 84 24 a0 00 00 00 	movl   $0x7,0xa0(%rsp)
  404709:	07 00 00 00 
  40470d:	c7 84 24 a4 00 00 00 	movl   $0x2,0xa4(%rsp)
  404714:	02 00 00 00 
  404718:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40471d:	8b 84 24 a0 00 00 00 	mov    0xa0(%rsp),%eax
  404724:	8b 8c 24 a4 00 00 00 	mov    0xa4(%rsp),%ecx
  40472b:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  404732:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  404739:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  404740:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  404747:	00 
  404748:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40474f:	c3                   	ret
  404750:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404755:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40475a:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  404761:	00 
  404762:	8b 79 28             	mov    0x28(%rcx),%edi
  404765:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40476c:	00 00 00 00 00 
  404771:	89 c2                	mov    %eax,%edx
  404773:	48 8d 8c 24 88 00 00 	lea    0x88(%rsp),%rcx
  40477a:	00 
  40477b:	e8 a0 e5 ff ff       	call   402d20 <linux::lseek>
  404780:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  404787:	00 
  404788:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  40478f:	00 
  404790:	89 44 24 7c          	mov    %eax,0x7c(%rsp)
  404794:	8b 44 24 7c          	mov    0x7c(%rsp),%eax
  404798:	89 44 24 04          	mov    %eax,0x4(%rsp)
  40479c:	85 c0                	test   %eax,%eax
  40479e:	0f 84 c2 00 00 00    	je     404866 <os::[file_linux.odin]::_seek+0x266>
  4047a4:	eb 00                	jmp    4047a6 <os::[file_linux.odin]::_seek+0x1a6>
  4047a6:	8b 44 24 04          	mov    0x4(%rsp),%eax
  4047aa:	83 e8 16             	sub    $0x16,%eax
  4047ad:	0f 85 fe 00 00 00    	jne    4048b1 <os::[file_linux.odin]::_seek+0x2b1>
  4047b3:	eb 00                	jmp    4047b5 <os::[file_linux.odin]::_seek+0x1b5>
  4047b5:	48 c7 44 24 70 00 00 	movq   $0x0,0x70(%rsp)
  4047bc:	00 00 
  4047be:	31 c0                	xor    %eax,%eax
  4047c0:	a8 01                	test   $0x1,%al
  4047c2:	75 02                	jne    4047c6 <os::[file_linux.odin]::_seek+0x1c6>
  4047c4:	eb 12                	jmp    4047d8 <os::[file_linux.odin]::_seek+0x1d8>
  4047c6:	c7 44 24 74 00 00 00 	movl   $0x0,0x74(%rsp)
  4047cd:	00 
  4047ce:	c7 44 24 70 00 00 00 	movl   $0x0,0x70(%rsp)
  4047d5:	00 
  4047d6:	eb 10                	jmp    4047e8 <os::[file_linux.odin]::_seek+0x1e8>
  4047d8:	c7 44 24 70 08 00 00 	movl   $0x8,0x70(%rsp)
  4047df:	00 
  4047e0:	c7 44 24 74 02 00 00 	movl   $0x2,0x74(%rsp)
  4047e7:	00 
  4047e8:	8b 44 24 70          	mov    0x70(%rsp),%eax
  4047ec:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  4047f0:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  4047f7:	00 00 00 00 00 
  4047fc:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  404803:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  40480a:	48 c7 44 24 68 00 00 	movq   $0x0,0x68(%rsp)
  404811:	00 00 
  404813:	31 c0                	xor    %eax,%eax
  404815:	a8 01                	test   $0x1,%al
  404817:	75 02                	jne    40481b <os::[file_linux.odin]::_seek+0x21b>
  404819:	eb 12                	jmp    40482d <os::[file_linux.odin]::_seek+0x22d>
  40481b:	c7 44 24 6c 00 00 00 	movl   $0x0,0x6c(%rsp)
  404822:	00 
  404823:	c7 44 24 68 00 00 00 	movl   $0x0,0x68(%rsp)
  40482a:	00 
  40482b:	eb 10                	jmp    40483d <os::[file_linux.odin]::_seek+0x23d>
  40482d:	c7 44 24 68 08 00 00 	movl   $0x8,0x68(%rsp)
  404834:	00 
  404835:	c7 44 24 6c 02 00 00 	movl   $0x2,0x6c(%rsp)
  40483c:	00 
  40483d:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  404842:	8b 44 24 68          	mov    0x68(%rsp),%eax
  404846:	8b 4c 24 6c          	mov    0x6c(%rsp),%ecx
  40484a:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  404851:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  404855:	89 44 24 60          	mov    %eax,0x60(%rsp)
  404859:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40485e:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  404865:	c3                   	ret
  404866:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40486b:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  404872:	00 
  404873:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40487a:	00 
  40487b:	c7 84 24 b4 00 00 00 	movl   $0x0,0xb4(%rsp)
  404882:	00 00 00 00 
  404886:	c7 84 24 b0 00 00 00 	movl   $0x0,0xb0(%rsp)
  40488d:	00 00 00 00 
  404891:	48 89 08             	mov    %rcx,(%rax)
  404894:	c7 44 24 54 00 00 00 	movl   $0x0,0x54(%rsp)
  40489b:	00 
  40489c:	c7 44 24 50 00 00 00 	movl   $0x0,0x50(%rsp)
  4048a3:	00 
  4048a4:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4048a9:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  4048b0:	c3                   	ret
  4048b1:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  4048b6:	8b 7c 24 7c          	mov    0x7c(%rsp),%edi
  4048ba:	e8 61 0e 00 00       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  4048bf:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4048c4:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4048c9:	8b 44 24 40          	mov    0x40(%rsp),%eax
  4048cd:	8b 4c 24 44          	mov    0x44(%rsp),%ecx
  4048d1:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  4048d8:	00 00 00 00 00 
  4048dd:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  4048e4:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  4048eb:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  4048f2:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  4048f6:	89 44 24 30          	mov    %eax,0x30(%rsp)
  4048fa:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4048ff:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  404906:	c3                   	ret
  404907:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40490e:	00 00 

0000000000404910 <os::[path_linux.odin]::_get_full_path>:
  404910:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  404917:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40491c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404921:	89 7c 24 3c          	mov    %edi,0x3c(%rsp)
  404925:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40492a:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40492f:	8b 54 24 3c          	mov    0x3c(%rsp),%edx
  404933:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404938:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40493d:	89 94 24 14 01 00 00 	mov    %edx,0x114(%rsp)
  404944:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  40494b:	00 
  40494c:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  404953:	00 
  404954:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40495b:	00 
  40495c:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  404961:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  404968:	00 
  404969:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40496e:	0f 57 c0             	xorps  %xmm0,%xmm0
  404971:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  404978:	00 
  404979:	48 c7 84 24 e8 00 00 	movq   $0x0,0xe8(%rsp)
  404980:	00 00 00 00 00 
  404985:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  40498c:	00 
  40498d:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  404994:	00 
  404995:	48 8d 84 24 c0 00 00 	lea    0xc0(%rsp),%rax
  40499c:	00 
  40499d:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  4049a4:	00 
  4049a5:	48 c7 84 24 b8 00 00 	movq   $0x20,0xb8(%rsp)
  4049ac:	00 20 00 00 00 
  4049b1:	48 8b bc 24 b0 00 00 	mov    0xb0(%rsp),%rdi
  4049b8:	00 
  4049b9:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  4049c0:	00 
  4049c1:	ba 78 e3 40 00       	mov    $0x40e378,%edx
  4049c6:	b9 0e 00 00 00       	mov    $0xe,%ecx
  4049cb:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4049d0:	e8 7b 73 00 00       	call   40bd50 <runtime::copy_from_string:proc"contextless"(dst:[]u8,src:string)->(:int)>
  4049d5:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  4049da:	48 89 e0             	mov    %rsp,%rax
  4049dd:	48 c7 00 20 00 00 00 	movq   $0x20,(%rax)
  4049e4:	bf 87 e3 40 00       	mov    $0x40e387,%edi
  4049e9:	be 25 00 00 00       	mov    $0x25,%esi
  4049ee:	ba ca 00 00 00       	mov    $0xca,%edx
  4049f3:	b9 17 00 00 00       	mov    $0x17,%ecx
  4049f8:	41 b9 20 00 00 00    	mov    $0x20,%r9d
  4049fe:	e8 2d 60 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  404a03:	8b 44 24 3c          	mov    0x3c(%rsp),%eax
  404a07:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  404a0c:	48 8d 8c 24 ce 00 00 	lea    0xce(%rsp),%rcx
  404a13:	00 
  404a14:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  404a1b:	00 
  404a1c:	48 c7 84 24 a8 00 00 	movq   $0x12,0xa8(%rsp)
  404a23:	00 12 00 00 00 
  404a28:	48 8b bc 24 a0 00 00 	mov    0xa0(%rsp),%rdi
  404a2f:	00 
  404a30:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  404a37:	00 
  404a38:	48 63 d0             	movslq %eax,%rdx
  404a3b:	b9 0a 00 00 00       	mov    $0xa,%ecx
  404a40:	e8 ab 47 00 00       	call   4091f0 <strconv::write_int>
  404a45:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  404a4a:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  404a4f:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  404a54:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  404a5b:	00 
  404a5c:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  404a63:	00 
  404a64:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404a6b:	00 
  404a6c:	48 8b 94 24 98 00 00 	mov    0x98(%rsp),%rdx
  404a73:	00 
  404a74:	0f 57 c0             	xorps  %xmm0,%xmm0
  404a77:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  404a7e:	00 
  404a7f:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  404a86:	00 
  404a87:	48 8d 8c 24 80 00 00 	lea    0x80(%rsp),%rcx
  404a8e:	00 
  404a8f:	e8 1c 12 00 00       	call   405cb0 <os::[file_linux.odin]::_read_link_cstr>
  404a94:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  404a99:	8b 44 24 70          	mov    0x70(%rsp),%eax
  404a9d:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  404aa1:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  404aa8:	00 
  404aa9:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  404ab0:	00 
  404ab1:	48 89 b4 24 f8 00 00 	mov    %rsi,0xf8(%rsp)
  404ab8:	00 
  404ab9:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  404ac0:	00 
  404ac1:	89 8c 24 ec 00 00 00 	mov    %ecx,0xec(%rsp)
  404ac8:	89 84 24 e8 00 00 00 	mov    %eax,0xe8(%rsp)
  404acf:	83 bc 24 ec 00 00 00 	cmpl   $0x0,0xec(%rsp)
  404ad6:	00 
  404ad7:	0f 95 c0             	setne  %al
  404ada:	24 01                	and    $0x1,%al
  404adc:	3c 00                	cmp    $0x0,%al
  404ade:	75 44                	jne    404b24 <os::[path_linux.odin]::_get_full_path+0x214>
  404ae0:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  404ae7:	00 
  404ae8:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404aed:	4c 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%r9
  404af4:	00 
  404af5:	bf 87 e3 40 00       	mov    $0x40e387,%edi
  404afa:	31 c0                	xor    %eax,%eax
  404afc:	41 89 c0             	mov    %eax,%r8d
  404aff:	be 25 00 00 00       	mov    $0x25,%esi
  404b04:	ba cc 00 00 00       	mov    $0xcc,%edx
  404b09:	b9 5a 00 00 00       	mov    $0x5a,%ecx
  404b0e:	e8 3d 5b 00 00       	call   40a650 <runtime::bounds_check_error>
  404b13:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404b18:	80 38 2f             	cmpb   $0x2f,(%rax)
  404b1b:	0f 95 c0             	setne  %al
  404b1e:	24 01                	and    $0x1,%al
  404b20:	3c 00                	cmp    $0x0,%al
  404b22:	74 52                	je     404b76 <os::[path_linux.odin]::_get_full_path+0x266>
  404b24:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  404b29:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  404b2e:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  404b33:	48 8b bc 24 f0 00 00 	mov    0xf0(%rsp),%rdi
  404b3a:	00 
  404b3b:	48 8b b4 24 f8 00 00 	mov    0xf8(%rsp),%rsi
  404b42:	00 
  404b43:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  404b48:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  404b4d:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  404b52:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  404b57:	41 b8 c0 e3 40 00    	mov    $0x40e3c0,%r8d
  404b5d:	e8 3e 72 00 00       	call   40bda0 <runtime::delete_string>
  404b62:	48 8d bc 24 f0 00 00 	lea    0xf0(%rsp),%rdi
  404b69:	00 
  404b6a:	31 f6                	xor    %esi,%esi
  404b6c:	ba 10 00 00 00       	mov    $0x10,%edx
  404b71:	e8 ca c4 ff ff       	call   401040 <memset@plt>
  404b76:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  404b7b:	48 8b b4 24 f0 00 00 	mov    0xf0(%rsp),%rsi
  404b82:	00 
  404b83:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  404b8a:	00 
  404b8b:	8b 84 24 e8 00 00 00 	mov    0xe8(%rsp),%eax
  404b92:	8b 8c 24 ec 00 00 00 	mov    0xec(%rsp),%ecx
  404b99:	48 89 bc 24 f8 00 00 	mov    %rdi,0xf8(%rsp)
  404ba0:	00 
  404ba1:	48 89 b4 24 f0 00 00 	mov    %rsi,0xf0(%rsp)
  404ba8:	00 
  404ba9:	89 8c 24 ec 00 00 00 	mov    %ecx,0xec(%rsp)
  404bb0:	89 84 24 e8 00 00 00 	mov    %eax,0xe8(%rsp)
  404bb7:	48 89 7a 08          	mov    %rdi,0x8(%rdx)
  404bbb:	48 89 32             	mov    %rsi,(%rdx)
  404bbe:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  404bc2:	89 44 24 50          	mov    %eax,0x50(%rsp)
  404bc6:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  404bcb:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  404bd2:	c3                   	ret
  404bd3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404bda:	84 00 00 00 00 00 

0000000000404be0 <os::[file_linux.odin]::_read>:
  404be0:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  404be7:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  404bec:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404bf1:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  404bf6:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  404bfb:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  404c00:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404c05:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  404c0a:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404c0f:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  404c16:	00 
  404c17:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  404c1e:	00 
  404c1f:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  404c26:	00 
  404c27:	48 83 f8 00          	cmp    $0x0,%rax
  404c2b:	0f 9e c0             	setle  %al
  404c2e:	24 01                	and    $0x1,%al
  404c30:	3c 00                	cmp    $0x0,%al
  404c32:	74 32                	je     404c66 <os::[file_linux.odin]::_read+0x86>
  404c34:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404c39:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  404c40:	c7 84 24 a4 00 00 00 	movl   $0x0,0xa4(%rsp)
  404c47:	00 00 00 00 
  404c4b:	c7 84 24 a0 00 00 00 	movl   $0x0,0xa0(%rsp)
  404c52:	00 00 00 00 
  404c56:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  404c5d:	00 
  404c5e:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  404c65:	c3                   	ret
  404c66:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404c6b:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  404c72:	00 
  404c73:	8b 49 28             	mov    0x28(%rcx),%ecx
  404c76:	89 4c 24 24          	mov    %ecx,0x24(%rsp)
  404c7a:	48 89 c1             	mov    %rax,%rcx
  404c7d:	48 81 e9 00 00 00 40 	sub    $0x40000000,%rcx
  404c84:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  404c8a:	4c 0f 4c c0          	cmovl  %rax,%r8
  404c8e:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  404c93:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  404c9a:	00 
  404c9b:	bf c0 e2 40 00       	mov    $0x40e2c0,%edi
  404ca0:	be 25 00 00 00       	mov    $0x25,%esi
  404ca5:	ba da 00 00 00       	mov    $0xda,%edx
  404caa:	b9 20 00 00 00       	mov    $0x20,%ecx
  404caf:	e8 cc 5c 00 00       	call   40a980 <runtime::slice_expr_error_hi>
  404cb4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  404cb9:	8b 7c 24 24          	mov    0x24(%rsp),%edi
  404cbd:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404cc4:	00 
  404cc5:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  404ccc:	00 
  404ccd:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  404cd4:	00 
  404cd5:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404cdc:	00 
  404cdd:	48 8b 94 24 98 00 00 	mov    0x98(%rsp),%rdx
  404ce4:	00 
  404ce5:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  404cec:	00 00 00 00 00 
  404cf1:	48 8d 8c 24 88 00 00 	lea    0x88(%rsp),%rcx
  404cf8:	00 
  404cf9:	e8 82 dc ff ff       	call   402980 <linux::read>
  404cfe:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  404d05:	00 
  404d06:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  404d0d:	00 
  404d0e:	89 44 24 7c          	mov    %eax,0x7c(%rsp)
  404d12:	83 7c 24 7c 00       	cmpl   $0x0,0x7c(%rsp)
  404d17:	0f 95 c0             	setne  %al
  404d1a:	24 01                	and    $0x1,%al
  404d1c:	3c 00                	cmp    $0x0,%al
  404d1e:	74 3c                	je     404d5c <os::[file_linux.odin]::_read+0x17c>
  404d20:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  404d25:	8b 7c 24 7c          	mov    0x7c(%rsp),%edi
  404d29:	e8 f2 09 00 00       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  404d2e:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  404d33:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  404d38:	8b 44 24 70          	mov    0x70(%rsp),%eax
  404d3c:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  404d40:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  404d47:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  404d4b:	89 44 24 60          	mov    %eax,0x60(%rsp)
  404d4f:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  404d54:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  404d5b:	c3                   	ret
  404d5c:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404d63:	00 
  404d64:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  404d69:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  404d70:	00 00 
  404d72:	0f 94 c0             	sete   %al
  404d75:	24 01                	and    $0x1,%al
  404d77:	3c 00                	cmp    $0x0,%al
  404d79:	74 45                	je     404dc0 <os::[file_linux.odin]::_read+0x1e0>
  404d7b:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  404d82:	00 00 
  404d84:	31 c0                	xor    %eax,%eax
  404d86:	a8 01                	test   $0x1,%al
  404d88:	75 02                	jne    404d8c <os::[file_linux.odin]::_read+0x1ac>
  404d8a:	eb 12                	jmp    404d9e <os::[file_linux.odin]::_read+0x1be>
  404d8c:	c7 44 24 5c 00 00 00 	movl   $0x0,0x5c(%rsp)
  404d93:	00 
  404d94:	c7 44 24 58 00 00 00 	movl   $0x0,0x58(%rsp)
  404d9b:	00 
  404d9c:	eb 10                	jmp    404dae <os::[file_linux.odin]::_read+0x1ce>
  404d9e:	c7 44 24 58 01 00 00 	movl   $0x1,0x58(%rsp)
  404da5:	00 
  404da6:	c7 44 24 5c 02 00 00 	movl   $0x2,0x5c(%rsp)
  404dad:	00 
  404dae:	8b 4c 24 58          	mov    0x58(%rsp),%ecx
  404db2:	8b 44 24 5c          	mov    0x5c(%rsp),%eax
  404db6:	89 4c 24 08          	mov    %ecx,0x8(%rsp)
  404dba:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  404dbe:	eb 0e                	jmp    404dce <os::[file_linux.odin]::_read+0x1ee>
  404dc0:	31 c9                	xor    %ecx,%ecx
  404dc2:	89 c8                	mov    %ecx,%eax
  404dc4:	89 4c 24 08          	mov    %ecx,0x8(%rsp)
  404dc8:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  404dcc:	eb 00                	jmp    404dce <os::[file_linux.odin]::_read+0x1ee>
  404dce:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  404dd3:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404dd8:	8b 44 24 08          	mov    0x8(%rsp),%eax
  404ddc:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  404de0:	48 89 32             	mov    %rsi,(%rdx)
  404de3:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  404de7:	89 44 24 50          	mov    %eax,0x50(%rsp)
  404deb:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  404df0:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  404df7:	c3                   	ret
  404df8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  404dff:	00 

0000000000404e00 <os::[file_linux.odin]::_read_at>:
  404e00:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  404e07:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  404e0c:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  404e11:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  404e16:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  404e1b:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  404e20:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  404e25:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404e2a:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  404e2f:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404e34:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  404e39:	48 89 b4 24 d0 00 00 	mov    %rsi,0xd0(%rsp)
  404e40:	00 
  404e41:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  404e48:	00 
  404e49:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  404e50:	00 
  404e51:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  404e58:	00 
  404e59:	48 83 f8 00          	cmp    $0x0,%rax
  404e5d:	0f 9e c0             	setle  %al
  404e60:	24 01                	and    $0x1,%al
  404e62:	3c 00                	cmp    $0x0,%al
  404e64:	74 32                	je     404e98 <os::[file_linux.odin]::_read_at+0x98>
  404e66:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  404e6b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  404e72:	c7 84 24 b4 00 00 00 	movl   $0x0,0xb4(%rsp)
  404e79:	00 00 00 00 
  404e7d:	c7 84 24 b0 00 00 00 	movl   $0x0,0xb0(%rsp)
  404e84:	00 00 00 00 
  404e88:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  404e8f:	00 
  404e90:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  404e97:	c3                   	ret
  404e98:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404e9d:	48 83 f8 00          	cmp    $0x0,%rax
  404ea1:	0f 9c c0             	setl   %al
  404ea4:	24 01                	and    $0x1,%al
  404ea6:	3c 00                	cmp    $0x0,%al
  404ea8:	74 7a                	je     404f24 <os::[file_linux.odin]::_read_at+0x124>
  404eaa:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  404eb1:	00 00 00 00 00 
  404eb6:	31 c0                	xor    %eax,%eax
  404eb8:	a8 01                	test   $0x1,%al
  404eba:	75 02                	jne    404ebe <os::[file_linux.odin]::_read_at+0xbe>
  404ebc:	eb 18                	jmp    404ed6 <os::[file_linux.odin]::_read_at+0xd6>
  404ebe:	c7 84 24 ac 00 00 00 	movl   $0x0,0xac(%rsp)
  404ec5:	00 00 00 00 
  404ec9:	c7 84 24 a8 00 00 00 	movl   $0x0,0xa8(%rsp)
  404ed0:	00 00 00 00 
  404ed4:	eb 16                	jmp    404eec <os::[file_linux.odin]::_read_at+0xec>
  404ed6:	c7 84 24 a8 00 00 00 	movl   $0x8,0xa8(%rsp)
  404edd:	08 00 00 00 
  404ee1:	c7 84 24 ac 00 00 00 	movl   $0x2,0xac(%rsp)
  404ee8:	02 00 00 00 
  404eec:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  404ef1:	8b 84 24 a8 00 00 00 	mov    0xa8(%rsp),%eax
  404ef8:	8b 8c 24 ac 00 00 00 	mov    0xac(%rsp),%ecx
  404eff:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  404f06:	89 8c 24 a4 00 00 00 	mov    %ecx,0xa4(%rsp)
  404f0d:	89 84 24 a0 00 00 00 	mov    %eax,0xa0(%rsp)
  404f14:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  404f1b:	00 
  404f1c:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  404f23:	c3                   	ret
  404f24:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404f29:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  404f30:	00 
  404f31:	8b 49 28             	mov    0x28(%rcx),%ecx
  404f34:	89 4c 24 0c          	mov    %ecx,0xc(%rsp)
  404f38:	48 89 c1             	mov    %rax,%rcx
  404f3b:	48 81 e9 00 00 00 40 	sub    $0x40000000,%rcx
  404f42:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  404f48:	4c 0f 4c c0          	cmovl  %rax,%r8
  404f4c:	4c 89 04 24          	mov    %r8,(%rsp)
  404f50:	4c 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%r9
  404f57:	00 
  404f58:	bf c0 e2 40 00       	mov    $0x40e2c0,%edi
  404f5d:	be 25 00 00 00       	mov    $0x25,%esi
  404f62:	ba e8 00 00 00       	mov    $0xe8,%edx
  404f67:	b9 21 00 00 00       	mov    $0x21,%ecx
  404f6c:	e8 0f 5a 00 00       	call   40a980 <runtime::slice_expr_error_hi>
  404f71:	48 8b 04 24          	mov    (%rsp),%rax
  404f75:	8b 7c 24 0c          	mov    0xc(%rsp),%edi
  404f79:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  404f7e:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  404f85:	00 
  404f86:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  404f8d:	00 
  404f8e:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  404f95:	00 
  404f96:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404f9d:	00 
  404f9e:	48 8b 94 24 98 00 00 	mov    0x98(%rsp),%rdx
  404fa5:	00 
  404fa6:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  404fad:	00 00 00 00 00 
  404fb2:	4c 8d 84 24 88 00 00 	lea    0x88(%rsp),%r8
  404fb9:	00 
  404fba:	e8 e1 dd ff ff       	call   402da0 <linux::pread>
  404fbf:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  404fc6:	00 
  404fc7:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  404fce:	00 
  404fcf:	89 44 24 7c          	mov    %eax,0x7c(%rsp)
  404fd3:	83 7c 24 7c 00       	cmpl   $0x0,0x7c(%rsp)
  404fd8:	0f 95 c0             	setne  %al
  404fdb:	24 01                	and    $0x1,%al
  404fdd:	3c 00                	cmp    $0x0,%al
  404fdf:	74 3c                	je     40501d <os::[file_linux.odin]::_read_at+0x21d>
  404fe1:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404fe6:	8b 7c 24 7c          	mov    0x7c(%rsp),%edi
  404fea:	e8 31 07 00 00       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  404fef:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  404ff4:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  404ff9:	8b 44 24 70          	mov    0x70(%rsp),%eax
  404ffd:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  405001:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  405008:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  40500c:	89 44 24 60          	mov    %eax,0x60(%rsp)
  405010:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  405015:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40501c:	c3                   	ret
  40501d:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  405024:	00 00 
  405026:	0f 94 c0             	sete   %al
  405029:	24 01                	and    $0x1,%al
  40502b:	3c 00                	cmp    $0x0,%al
  40502d:	74 5c                	je     40508b <os::[file_linux.odin]::_read_at+0x28b>
  40502f:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  405036:	00 00 
  405038:	31 c0                	xor    %eax,%eax
  40503a:	a8 01                	test   $0x1,%al
  40503c:	75 02                	jne    405040 <os::[file_linux.odin]::_read_at+0x240>
  40503e:	eb 12                	jmp    405052 <os::[file_linux.odin]::_read_at+0x252>
  405040:	c7 44 24 5c 00 00 00 	movl   $0x0,0x5c(%rsp)
  405047:	00 
  405048:	c7 44 24 58 00 00 00 	movl   $0x0,0x58(%rsp)
  40504f:	00 
  405050:	eb 10                	jmp    405062 <os::[file_linux.odin]::_read_at+0x262>
  405052:	c7 44 24 58 01 00 00 	movl   $0x1,0x58(%rsp)
  405059:	00 
  40505a:	c7 44 24 5c 02 00 00 	movl   $0x2,0x5c(%rsp)
  405061:	00 
  405062:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  405067:	8b 44 24 58          	mov    0x58(%rsp),%eax
  40506b:	8b 4c 24 5c          	mov    0x5c(%rsp),%ecx
  40506f:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  405076:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  40507a:	89 44 24 50          	mov    %eax,0x50(%rsp)
  40507e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  405083:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40508a:	c3                   	ret
  40508b:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  405090:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  405097:	00 
  405098:	48 89 08             	mov    %rcx,(%rax)
  40509b:	c7 44 24 44 00 00 00 	movl   $0x0,0x44(%rsp)
  4050a2:	00 
  4050a3:	c7 44 24 40 00 00 00 	movl   $0x0,0x40(%rsp)
  4050aa:	00 
  4050ab:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4050b0:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  4050b7:	c3                   	ret
  4050b8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4050bf:	00 

00000000004050c0 <os::[file_linux.odin]::_write>:
  4050c0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4050c7:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  4050cc:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4050d1:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4050d6:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4050db:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  4050e0:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4050e5:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4050ea:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4050ef:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4050f6:	00 
  4050f7:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  4050fe:	00 
  4050ff:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  405106:	00 
  405107:	48 c7 84 24 c8 00 00 	movq   $0x0,0xc8(%rsp)
  40510e:	00 00 00 00 00 
  405113:	48 c7 84 24 c0 00 00 	movq   $0x0,0xc0(%rsp)
  40511a:	00 00 00 00 00 
  40511f:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  405126:	00 
  405127:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  40512e:	00 
  40512f:	48 83 bc 24 b8 00 00 	cmpq   $0x0,0xb8(%rsp)
  405136:	00 00 
  405138:	0f 9f c0             	setg   %al
  40513b:	24 01                	and    $0x1,%al
  40513d:	3c 00                	cmp    $0x0,%al
  40513f:	0f 84 c2 01 00 00    	je     405307 <os::[file_linux.odin]::_write+0x247>
  405145:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40514c:	00 
  40514d:	8b 40 28             	mov    0x28(%rax),%eax
  405150:	89 44 24 24          	mov    %eax,0x24(%rsp)
  405154:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  40515b:	00 
  40515c:	4c 89 c8             	mov    %r9,%rax
  40515f:	48 2d 00 00 00 40    	sub    $0x40000000,%rax
  405165:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  40516b:	4d 0f 4c c1          	cmovl  %r9,%r8
  40516f:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  405174:	bf c0 e2 40 00       	mov    $0x40e2c0,%edi
  405179:	be 25 00 00 00       	mov    $0x25,%esi
  40517e:	ba f5 00 00 00       	mov    $0xf5,%edx
  405183:	b9 22 00 00 00       	mov    $0x22,%ecx
  405188:	e8 f3 57 00 00       	call   40a980 <runtime::slice_expr_error_hi>
  40518d:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  405192:	8b 7c 24 24          	mov    0x24(%rsp),%edi
  405196:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40519d:	00 
  40519e:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  4051a5:	00 
  4051a6:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4051ad:	00 
  4051ae:	48 8b b4 24 a0 00 00 	mov    0xa0(%rsp),%rsi
  4051b5:	00 
  4051b6:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  4051bd:	00 
  4051be:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  4051c5:	00 00 00 00 00 
  4051ca:	48 8d 8c 24 98 00 00 	lea    0x98(%rsp),%rcx
  4051d1:	00 
  4051d2:	e8 19 d8 ff ff       	call   4029f0 <linux::write>
  4051d7:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4051de:	00 
  4051df:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  4051e6:	00 
  4051e7:	89 84 24 8c 00 00 00 	mov    %eax,0x8c(%rsp)
  4051ee:	83 bc 24 8c 00 00 00 	cmpl   $0x0,0x8c(%rsp)
  4051f5:	00 
  4051f6:	0f 95 c0             	setne  %al
  4051f9:	24 01                	and    $0x1,%al
  4051fb:	3c 00                	cmp    $0x0,%al
  4051fd:	74 76                	je     405275 <os::[file_linux.odin]::_write+0x1b5>
  4051ff:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  405204:	8b bc 24 8c 00 00 00 	mov    0x8c(%rsp),%edi
  40520b:	e8 10 05 00 00       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  405210:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405215:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40521c:	00 
  40521d:	8b 84 24 80 00 00 00 	mov    0x80(%rsp),%eax
  405224:	8b 8c 24 84 00 00 00 	mov    0x84(%rsp),%ecx
  40522b:	89 8c 24 c4 00 00 00 	mov    %ecx,0xc4(%rsp)
  405232:	89 84 24 c0 00 00 00 	mov    %eax,0xc0(%rsp)
  405239:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  405240:	00 
  405241:	8b 84 24 c0 00 00 00 	mov    0xc0(%rsp),%eax
  405248:	8b 8c 24 c4 00 00 00 	mov    0xc4(%rsp),%ecx
  40524f:	89 8c 24 c4 00 00 00 	mov    %ecx,0xc4(%rsp)
  405256:	89 84 24 c0 00 00 00 	mov    %eax,0xc0(%rsp)
  40525d:	48 89 32             	mov    %rsi,(%rdx)
  405260:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  405264:	89 44 24 70          	mov    %eax,0x70(%rsp)
  405268:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40526d:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  405274:	c3                   	ret
  405275:	4c 8b 84 24 90 00 00 	mov    0x90(%rsp),%r8
  40527c:	00 
  40527d:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  405282:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  405289:	00 
  40528a:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40528f:	48 89 e0             	mov    %rsp,%rax
  405292:	4c 89 08             	mov    %r9,(%rax)
  405295:	bf c0 e2 40 00       	mov    $0x40e2c0,%edi
  40529a:	be 25 00 00 00       	mov    $0x25,%esi
  40529f:	ba fb 00 00 00       	mov    $0xfb,%edx
  4052a4:	b9 08 00 00 00       	mov    $0x8,%ecx
  4052a9:	e8 82 57 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  4052ae:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4052b3:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4052b8:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4052bf:	00 
  4052c0:	48 01 d1             	add    %rdx,%rcx
  4052c3:	48 29 d0             	sub    %rdx,%rax
  4052c6:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  4052cb:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4052d0:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4052d5:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4052da:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  4052e1:	00 
  4052e2:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  4052e9:	00 
  4052ea:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  4052f1:	00 
  4052f2:	48 03 84 24 c8 00 00 	add    0xc8(%rsp),%rax
  4052f9:	00 
  4052fa:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  405301:	00 
  405302:	e9 28 fe ff ff       	jmp    40512f <os::[file_linux.odin]::_write+0x6f>
  405307:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40530c:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  405313:	00 
  405314:	8b 84 24 c0 00 00 00 	mov    0xc0(%rsp),%eax
  40531b:	8b 8c 24 c4 00 00 00 	mov    0xc4(%rsp),%ecx
  405322:	89 8c 24 c4 00 00 00 	mov    %ecx,0xc4(%rsp)
  405329:	89 84 24 c0 00 00 00 	mov    %eax,0xc0(%rsp)
  405330:	48 89 32             	mov    %rsi,(%rdx)
  405333:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  405337:	89 44 24 50          	mov    %eax,0x50(%rsp)
  40533b:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  405340:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  405347:	c3                   	ret
  405348:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40534f:	00 

0000000000405350 <os::[file_linux.odin]::_write_at>:
  405350:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  405357:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  40535c:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  405361:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  405366:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  40536b:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  405370:	48 89 74 24 58       	mov    %rsi,0x58(%rsp)
  405375:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40537a:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  40537f:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  405384:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  405389:	48 89 b4 24 20 01 00 	mov    %rsi,0x120(%rsp)
  405390:	00 
  405391:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  405398:	00 
  405399:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  4053a0:	00 
  4053a1:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  4053a8:	00 
  4053a9:	48 c7 84 24 00 01 00 	movq   $0x0,0x100(%rsp)
  4053b0:	00 00 00 00 00 
  4053b5:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  4053bc:	00 00 00 00 00 
  4053c1:	48 83 f8 00          	cmp    $0x0,%rax
  4053c5:	0f 9c c0             	setl   %al
  4053c8:	24 01                	and    $0x1,%al
  4053ca:	3c 00                	cmp    $0x0,%al
  4053cc:	0f 84 e4 00 00 00    	je     4054b6 <os::[file_linux.odin]::_write_at+0x166>
  4053d2:	48 c7 84 24 f0 00 00 	movq   $0x0,0xf0(%rsp)
  4053d9:	00 00 00 00 00 
  4053de:	31 c0                	xor    %eax,%eax
  4053e0:	a8 01                	test   $0x1,%al
  4053e2:	75 02                	jne    4053e6 <os::[file_linux.odin]::_write_at+0x96>
  4053e4:	eb 18                	jmp    4053fe <os::[file_linux.odin]::_write_at+0xae>
  4053e6:	c7 84 24 f4 00 00 00 	movl   $0x0,0xf4(%rsp)
  4053ed:	00 00 00 00 
  4053f1:	c7 84 24 f0 00 00 00 	movl   $0x0,0xf0(%rsp)
  4053f8:	00 00 00 00 
  4053fc:	eb 16                	jmp    405414 <os::[file_linux.odin]::_write_at+0xc4>
  4053fe:	c7 84 24 f0 00 00 00 	movl   $0x8,0xf0(%rsp)
  405405:	08 00 00 00 
  405409:	c7 84 24 f4 00 00 00 	movl   $0x2,0xf4(%rsp)
  405410:	02 00 00 00 
  405414:	8b 84 24 f0 00 00 00 	mov    0xf0(%rsp),%eax
  40541b:	8b 8c 24 f4 00 00 00 	mov    0xf4(%rsp),%ecx
  405422:	48 c7 84 24 00 01 00 	movq   $0x0,0x100(%rsp)
  405429:	00 00 00 00 00 
  40542e:	89 8c 24 fc 00 00 00 	mov    %ecx,0xfc(%rsp)
  405435:	89 84 24 f8 00 00 00 	mov    %eax,0xf8(%rsp)
  40543c:	48 c7 84 24 e8 00 00 	movq   $0x0,0xe8(%rsp)
  405443:	00 00 00 00 00 
  405448:	31 c0                	xor    %eax,%eax
  40544a:	a8 01                	test   $0x1,%al
  40544c:	75 02                	jne    405450 <os::[file_linux.odin]::_write_at+0x100>
  40544e:	eb 18                	jmp    405468 <os::[file_linux.odin]::_write_at+0x118>
  405450:	c7 84 24 ec 00 00 00 	movl   $0x0,0xec(%rsp)
  405457:	00 00 00 00 
  40545b:	c7 84 24 e8 00 00 00 	movl   $0x0,0xe8(%rsp)
  405462:	00 00 00 00 
  405466:	eb 16                	jmp    40547e <os::[file_linux.odin]::_write_at+0x12e>
  405468:	c7 84 24 e8 00 00 00 	movl   $0x8,0xe8(%rsp)
  40546f:	08 00 00 00 
  405473:	c7 84 24 ec 00 00 00 	movl   $0x2,0xec(%rsp)
  40547a:	02 00 00 00 
  40547e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  405483:	8b 84 24 e8 00 00 00 	mov    0xe8(%rsp),%eax
  40548a:	8b 8c 24 ec 00 00 00 	mov    0xec(%rsp),%ecx
  405491:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  405498:	89 8c 24 e4 00 00 00 	mov    %ecx,0xe4(%rsp)
  40549f:	89 84 24 e0 00 00 00 	mov    %eax,0xe0(%rsp)
  4054a6:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4054ad:	00 
  4054ae:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  4054b5:	c3                   	ret
  4054b6:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4054bb:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  4054c0:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  4054c5:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  4054cc:	00 
  4054cd:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  4054d4:	00 
  4054d5:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  4054dc:	00 
  4054dd:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  4054e4:	00 00 
  4054e6:	0f 9f c0             	setg   %al
  4054e9:	24 01                	and    $0x1,%al
  4054eb:	3c 00                	cmp    $0x0,%al
  4054ed:	0f 84 eb 01 00 00    	je     4056de <os::[file_linux.odin]::_write_at+0x38e>
  4054f3:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4054fa:	00 
  4054fb:	8b 40 28             	mov    0x28(%rax),%eax
  4054fe:	89 44 24 2c          	mov    %eax,0x2c(%rsp)
  405502:	4c 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%r9
  405509:	00 
  40550a:	4c 89 c8             	mov    %r9,%rax
  40550d:	48 2d 00 00 00 40    	sub    $0x40000000,%rax
  405513:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  405519:	4d 0f 4c c1          	cmovl  %r9,%r8
  40551d:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  405522:	bf c0 e2 40 00       	mov    $0x40e2c0,%edi
  405527:	be 25 00 00 00       	mov    $0x25,%esi
  40552c:	ba 0a 01 00 00       	mov    $0x10a,%edx
  405531:	b9 23 00 00 00       	mov    $0x23,%ecx
  405536:	e8 45 54 00 00       	call   40a980 <runtime::slice_expr_error_hi>
  40553b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405540:	8b 7c 24 2c          	mov    0x2c(%rsp),%edi
  405544:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  40554b:	00 
  40554c:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  405553:	00 
  405554:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  40555b:	00 
  40555c:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  405563:	00 
  405564:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  40556b:	00 
  40556c:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  405573:	00 
  405574:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  40557b:	00 00 00 00 00 
  405580:	4c 8d 84 24 b0 00 00 	lea    0xb0(%rsp),%r8
  405587:	00 
  405588:	e8 a3 d8 ff ff       	call   402e30 <linux::pwrite>
  40558d:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  405594:	00 
  405595:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  40559c:	00 
  40559d:	89 84 24 a4 00 00 00 	mov    %eax,0xa4(%rsp)
  4055a4:	83 bc 24 a4 00 00 00 	cmpl   $0x0,0xa4(%rsp)
  4055ab:	00 
  4055ac:	0f 95 c0             	setne  %al
  4055af:	24 01                	and    $0x1,%al
  4055b1:	3c 00                	cmp    $0x0,%al
  4055b3:	74 7f                	je     405634 <os::[file_linux.odin]::_write_at+0x2e4>
  4055b5:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4055ba:	8b bc 24 a4 00 00 00 	mov    0xa4(%rsp),%edi
  4055c1:	e8 5a 01 00 00       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  4055c6:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4055cb:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4055d2:	00 
  4055d3:	8b 84 24 90 00 00 00 	mov    0x90(%rsp),%eax
  4055da:	8b 8c 24 94 00 00 00 	mov    0x94(%rsp),%ecx
  4055e1:	89 8c 24 fc 00 00 00 	mov    %ecx,0xfc(%rsp)
  4055e8:	89 84 24 f8 00 00 00 	mov    %eax,0xf8(%rsp)
  4055ef:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  4055f6:	00 
  4055f7:	8b 84 24 f8 00 00 00 	mov    0xf8(%rsp),%eax
  4055fe:	8b 8c 24 fc 00 00 00 	mov    0xfc(%rsp),%ecx
  405605:	89 8c 24 fc 00 00 00 	mov    %ecx,0xfc(%rsp)
  40560c:	89 84 24 f8 00 00 00 	mov    %eax,0xf8(%rsp)
  405613:	48 89 32             	mov    %rsi,(%rdx)
  405616:	89 8c 24 84 00 00 00 	mov    %ecx,0x84(%rsp)
  40561d:	89 84 24 80 00 00 00 	mov    %eax,0x80(%rsp)
  405624:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40562b:	00 
  40562c:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  405633:	c3                   	ret
  405634:	4c 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%r8
  40563b:	00 
  40563c:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  405641:	4c 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%r9
  405648:	00 
  405649:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40564e:	48 89 e0             	mov    %rsp,%rax
  405651:	4c 89 08             	mov    %r9,(%rax)
  405654:	bf c0 e2 40 00       	mov    $0x40e2c0,%edi
  405659:	be 25 00 00 00       	mov    $0x25,%esi
  40565e:	ba 10 01 00 00       	mov    $0x110,%edx
  405663:	b9 08 00 00 00       	mov    $0x8,%ecx
  405668:	e8 c3 53 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  40566d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405672:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  405677:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  40567e:	00 
  40567f:	48 01 d1             	add    %rdx,%rcx
  405682:	48 29 d0             	sub    %rdx,%rax
  405685:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40568a:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40568f:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  405694:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  405699:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  4056a0:	00 
  4056a1:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  4056a8:	00 
  4056a9:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4056b0:	00 
  4056b1:	48 03 84 24 00 01 00 	add    0x100(%rsp),%rax
  4056b8:	00 
  4056b9:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  4056c0:	00 
  4056c1:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4056c8:	00 
  4056c9:	48 03 84 24 c8 00 00 	add    0xc8(%rsp),%rax
  4056d0:	00 
  4056d1:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  4056d8:	00 
  4056d9:	e9 ff fd ff ff       	jmp    4054dd <os::[file_linux.odin]::_write_at+0x18d>
  4056de:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4056e3:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  4056ea:	00 
  4056eb:	8b 84 24 f8 00 00 00 	mov    0xf8(%rsp),%eax
  4056f2:	8b 8c 24 fc 00 00 00 	mov    0xfc(%rsp),%ecx
  4056f9:	89 8c 24 fc 00 00 00 	mov    %ecx,0xfc(%rsp)
  405700:	89 84 24 f8 00 00 00 	mov    %eax,0xf8(%rsp)
  405707:	48 89 32             	mov    %rsi,(%rdx)
  40570a:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  40570e:	89 44 24 60          	mov    %eax,0x60(%rsp)
  405712:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  405717:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  40571e:	c3                   	ret
  40571f:	90                   	nop

0000000000405720 <os::[errors_linux.odin]::_get_platform_error>:
  405720:	48 83 ec 38          	sub    $0x38,%rsp
  405724:	89 7c 24 8c          	mov    %edi,-0x74(%rsp)
  405728:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40572c:	89 44 24 34          	mov    %eax,0x34(%rsp)
  405730:	85 c0                	test   %eax,%eax
  405732:	0f 84 88 00 00 00    	je     4057c0 <os::[errors_linux.odin]::_get_platform_error+0xa0>
  405738:	eb 00                	jmp    40573a <os::[errors_linux.odin]::_get_platform_error+0x1a>
  40573a:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40573e:	83 e8 01             	sub    $0x1,%eax
  405741:	0f 84 93 00 00 00    	je     4057da <os::[errors_linux.odin]::_get_platform_error+0xba>
  405747:	eb 00                	jmp    405749 <os::[errors_linux.odin]::_get_platform_error+0x29>
  405749:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40574d:	83 e8 02             	sub    $0x2,%eax
  405750:	0f 84 1c 01 00 00    	je     405872 <os::[errors_linux.odin]::_get_platform_error+0x152>
  405756:	eb 00                	jmp    405758 <os::[errors_linux.odin]::_get_platform_error+0x38>
  405758:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40575c:	83 e8 09             	sub    $0x9,%eax
  40575f:	0f 84 f4 01 00 00    	je     405959 <os::[errors_linux.odin]::_get_platform_error+0x239>
  405765:	eb 00                	jmp    405767 <os::[errors_linux.odin]::_get_platform_error+0x47>
  405767:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40576b:	83 e8 0c             	sub    $0xc,%eax
  40576e:	0f 84 32 02 00 00    	je     4059a6 <os::[errors_linux.odin]::_get_platform_error+0x286>
  405774:	eb 00                	jmp    405776 <os::[errors_linux.odin]::_get_platform_error+0x56>
  405776:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40577a:	83 e8 0d             	sub    $0xd,%eax
  40577d:	74 5b                	je     4057da <os::[errors_linux.odin]::_get_platform_error+0xba>
  40577f:	eb 00                	jmp    405781 <os::[errors_linux.odin]::_get_platform_error+0x61>
  405781:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  405785:	83 e8 11             	sub    $0x11,%eax
  405788:	0f 84 99 00 00 00    	je     405827 <os::[errors_linux.odin]::_get_platform_error+0x107>
  40578e:	eb 00                	jmp    405790 <os::[errors_linux.odin]::_get_platform_error+0x70>
  405790:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  405794:	83 e8 20             	sub    $0x20,%eax
  405797:	0f 84 6f 01 00 00    	je     40590c <os::[errors_linux.odin]::_get_platform_error+0x1ec>
  40579d:	eb 00                	jmp    40579f <os::[errors_linux.odin]::_get_platform_error+0x7f>
  40579f:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  4057a3:	83 e8 26             	sub    $0x26,%eax
  4057a6:	0f 84 44 02 00 00    	je     4059f0 <os::[errors_linux.odin]::_get_platform_error+0x2d0>
  4057ac:	eb 00                	jmp    4057ae <os::[errors_linux.odin]::_get_platform_error+0x8e>
  4057ae:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  4057b2:	83 e8 6e             	sub    $0x6e,%eax
  4057b5:	0f 84 04 01 00 00    	je     4058bf <os::[errors_linux.odin]::_get_platform_error+0x19f>
  4057bb:	e9 7d 02 00 00       	jmp    405a3d <os::[errors_linux.odin]::_get_platform_error+0x31d>
  4057c0:	c7 44 24 24 00 00 00 	movl   $0x0,0x24(%rsp)
  4057c7:	00 
  4057c8:	c7 44 24 20 00 00 00 	movl   $0x0,0x20(%rsp)
  4057cf:	00 
  4057d0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4057d5:	48 83 c4 38          	add    $0x38,%rsp
  4057d9:	c3                   	ret
  4057da:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  4057e1:	00 00 
  4057e3:	31 c0                	xor    %eax,%eax
  4057e5:	a8 01                	test   $0x1,%al
  4057e7:	75 02                	jne    4057eb <os::[errors_linux.odin]::_get_platform_error+0xcb>
  4057e9:	eb 12                	jmp    4057fd <os::[errors_linux.odin]::_get_platform_error+0xdd>
  4057eb:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  4057f2:	00 
  4057f3:	c7 44 24 18 00 00 00 	movl   $0x0,0x18(%rsp)
  4057fa:	00 
  4057fb:	eb 10                	jmp    40580d <os::[errors_linux.odin]::_get_platform_error+0xed>
  4057fd:	c7 44 24 18 10 00 00 	movl   $0x10,0x18(%rsp)
  405804:	00 
  405805:	c7 44 24 1c 02 00 00 	movl   $0x2,0x1c(%rsp)
  40580c:	00 
  40580d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  405811:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  405815:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  405819:	89 44 24 10          	mov    %eax,0x10(%rsp)
  40581d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405822:	48 83 c4 38          	add    $0x38,%rsp
  405826:	c3                   	ret
  405827:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  40582e:	00 00 
  405830:	31 c0                	xor    %eax,%eax
  405832:	a8 01                	test   $0x1,%al
  405834:	75 02                	jne    405838 <os::[errors_linux.odin]::_get_platform_error+0x118>
  405836:	eb 12                	jmp    40584a <os::[errors_linux.odin]::_get_platform_error+0x12a>
  405838:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  40583f:	00 
  405840:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  405847:	00 
  405848:	eb 10                	jmp    40585a <os::[errors_linux.odin]::_get_platform_error+0x13a>
  40584a:	c7 44 24 08 01 00 00 	movl   $0x1,0x8(%rsp)
  405851:	00 
  405852:	c7 44 24 0c 01 00 00 	movl   $0x1,0xc(%rsp)
  405859:	00 
  40585a:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40585e:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  405862:	89 4c 24 04          	mov    %ecx,0x4(%rsp)
  405866:	89 04 24             	mov    %eax,(%rsp)
  405869:	48 8b 04 24          	mov    (%rsp),%rax
  40586d:	48 83 c4 38          	add    $0x38,%rsp
  405871:	c3                   	ret
  405872:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  405879:	00 00 
  40587b:	31 c0                	xor    %eax,%eax
  40587d:	a8 01                	test   $0x1,%al
  40587f:	75 02                	jne    405883 <os::[errors_linux.odin]::_get_platform_error+0x163>
  405881:	eb 12                	jmp    405895 <os::[errors_linux.odin]::_get_platform_error+0x175>
  405883:	c7 44 24 fc 00 00 00 	movl   $0x0,-0x4(%rsp)
  40588a:	00 
  40588b:	c7 44 24 f8 00 00 00 	movl   $0x0,-0x8(%rsp)
  405892:	00 
  405893:	eb 10                	jmp    4058a5 <os::[errors_linux.odin]::_get_platform_error+0x185>
  405895:	c7 44 24 f8 02 00 00 	movl   $0x2,-0x8(%rsp)
  40589c:	00 
  40589d:	c7 44 24 fc 01 00 00 	movl   $0x1,-0x4(%rsp)
  4058a4:	00 
  4058a5:	8b 44 24 f8          	mov    -0x8(%rsp),%eax
  4058a9:	8b 4c 24 fc          	mov    -0x4(%rsp),%ecx
  4058ad:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  4058b1:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4058b5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4058ba:	48 83 c4 38          	add    $0x38,%rsp
  4058be:	c3                   	ret
  4058bf:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4058c6:	00 00 
  4058c8:	31 c0                	xor    %eax,%eax
  4058ca:	a8 01                	test   $0x1,%al
  4058cc:	75 02                	jne    4058d0 <os::[errors_linux.odin]::_get_platform_error+0x1b0>
  4058ce:	eb 12                	jmp    4058e2 <os::[errors_linux.odin]::_get_platform_error+0x1c2>
  4058d0:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  4058d7:	00 
  4058d8:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  4058df:	00 
  4058e0:	eb 10                	jmp    4058f2 <os::[errors_linux.odin]::_get_platform_error+0x1d2>
  4058e2:	c7 44 24 e8 03 00 00 	movl   $0x3,-0x18(%rsp)
  4058e9:	00 
  4058ea:	c7 44 24 ec 01 00 00 	movl   $0x1,-0x14(%rsp)
  4058f1:	00 
  4058f2:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4058f6:	8b 4c 24 ec          	mov    -0x14(%rsp),%ecx
  4058fa:	89 4c 24 e4          	mov    %ecx,-0x1c(%rsp)
  4058fe:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  405902:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405907:	48 83 c4 38          	add    $0x38,%rsp
  40590b:	c3                   	ret
  40590c:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  405913:	00 00 
  405915:	31 c0                	xor    %eax,%eax
  405917:	a8 01                	test   $0x1,%al
  405919:	75 02                	jne    40591d <os::[errors_linux.odin]::_get_platform_error+0x1fd>
  40591b:	eb 12                	jmp    40592f <os::[errors_linux.odin]::_get_platform_error+0x20f>
  40591d:	c7 44 24 dc 00 00 00 	movl   $0x0,-0x24(%rsp)
  405924:	00 
  405925:	c7 44 24 d8 00 00 00 	movl   $0x0,-0x28(%rsp)
  40592c:	00 
  40592d:	eb 10                	jmp    40593f <os::[errors_linux.odin]::_get_platform_error+0x21f>
  40592f:	c7 44 24 d8 04 00 00 	movl   $0x4,-0x28(%rsp)
  405936:	00 
  405937:	c7 44 24 dc 01 00 00 	movl   $0x1,-0x24(%rsp)
  40593e:	00 
  40593f:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  405943:	8b 4c 24 dc          	mov    -0x24(%rsp),%ecx
  405947:	89 4c 24 d4          	mov    %ecx,-0x2c(%rsp)
  40594b:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  40594f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405954:	48 83 c4 38          	add    $0x38,%rsp
  405958:	c3                   	ret
  405959:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  405960:	00 00 
  405962:	31 c0                	xor    %eax,%eax
  405964:	a8 01                	test   $0x1,%al
  405966:	75 02                	jne    40596a <os::[errors_linux.odin]::_get_platform_error+0x24a>
  405968:	eb 12                	jmp    40597c <os::[errors_linux.odin]::_get_platform_error+0x25c>
  40596a:	c7 44 24 cc 00 00 00 	movl   $0x0,-0x34(%rsp)
  405971:	00 
  405972:	c7 44 24 c8 00 00 00 	movl   $0x0,-0x38(%rsp)
  405979:	00 
  40597a:	eb 10                	jmp    40598c <os::[errors_linux.odin]::_get_platform_error+0x26c>
  40597c:	c7 44 24 c8 05 00 00 	movl   $0x5,-0x38(%rsp)
  405983:	00 
  405984:	c7 44 24 cc 01 00 00 	movl   $0x1,-0x34(%rsp)
  40598b:	00 
  40598c:	8b 44 24 c8          	mov    -0x38(%rsp),%eax
  405990:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  405994:	89 4c 24 c4          	mov    %ecx,-0x3c(%rsp)
  405998:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40599c:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4059a1:	48 83 c4 38          	add    $0x38,%rsp
  4059a5:	c3                   	ret
  4059a6:	48 c7 44 24 b8 00 00 	movq   $0x0,-0x48(%rsp)
  4059ad:	00 00 
  4059af:	31 c0                	xor    %eax,%eax
  4059b1:	a8 01                	test   $0x1,%al
  4059b3:	75 02                	jne    4059b7 <os::[errors_linux.odin]::_get_platform_error+0x297>
  4059b5:	eb 12                	jmp    4059c9 <os::[errors_linux.odin]::_get_platform_error+0x2a9>
  4059b7:	c7 44 24 bc 00 00 00 	movl   $0x0,-0x44(%rsp)
  4059be:	00 
  4059bf:	c7 44 24 b8 00 00 00 	movl   $0x0,-0x48(%rsp)
  4059c6:	00 
  4059c7:	eb 0d                	jmp    4059d6 <os::[errors_linux.odin]::_get_platform_error+0x2b6>
  4059c9:	c6 44 24 b8 01       	movb   $0x1,-0x48(%rsp)
  4059ce:	c7 44 24 bc 03 00 00 	movl   $0x3,-0x44(%rsp)
  4059d5:	00 
  4059d6:	8b 44 24 b8          	mov    -0x48(%rsp),%eax
  4059da:	8b 4c 24 bc          	mov    -0x44(%rsp),%ecx
  4059de:	89 4c 24 b4          	mov    %ecx,-0x4c(%rsp)
  4059e2:	89 44 24 b0          	mov    %eax,-0x50(%rsp)
  4059e6:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4059eb:	48 83 c4 38          	add    $0x38,%rsp
  4059ef:	c3                   	ret
  4059f0:	48 c7 44 24 a8 00 00 	movq   $0x0,-0x58(%rsp)
  4059f7:	00 00 
  4059f9:	31 c0                	xor    %eax,%eax
  4059fb:	a8 01                	test   $0x1,%al
  4059fd:	75 02                	jne    405a01 <os::[errors_linux.odin]::_get_platform_error+0x2e1>
  4059ff:	eb 12                	jmp    405a13 <os::[errors_linux.odin]::_get_platform_error+0x2f3>
  405a01:	c7 44 24 ac 00 00 00 	movl   $0x0,-0x54(%rsp)
  405a08:	00 
  405a09:	c7 44 24 a8 00 00 00 	movl   $0x0,-0x58(%rsp)
  405a10:	00 
  405a11:	eb 10                	jmp    405a23 <os::[errors_linux.odin]::_get_platform_error+0x303>
  405a13:	c7 44 24 a8 ff ff ff 	movl   $0xffffffff,-0x58(%rsp)
  405a1a:	ff 
  405a1b:	c7 44 24 ac 02 00 00 	movl   $0x2,-0x54(%rsp)
  405a22:	00 
  405a23:	8b 44 24 a8          	mov    -0x58(%rsp),%eax
  405a27:	8b 4c 24 ac          	mov    -0x54(%rsp),%ecx
  405a2b:	89 4c 24 a4          	mov    %ecx,-0x5c(%rsp)
  405a2f:	89 44 24 a0          	mov    %eax,-0x60(%rsp)
  405a33:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405a38:	48 83 c4 38          	add    $0x38,%rsp
  405a3c:	c3                   	ret
  405a3d:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  405a41:	48 c7 44 24 98 00 00 	movq   $0x0,-0x68(%rsp)
  405a48:	00 00 
  405a4a:	83 f8 00             	cmp    $0x0,%eax
  405a4d:	75 12                	jne    405a61 <os::[errors_linux.odin]::_get_platform_error+0x341>
  405a4f:	c7 44 24 9c 00 00 00 	movl   $0x0,-0x64(%rsp)
  405a56:	00 
  405a57:	c7 44 24 98 00 00 00 	movl   $0x0,-0x68(%rsp)
  405a5e:	00 
  405a5f:	eb 10                	jmp    405a71 <os::[errors_linux.odin]::_get_platform_error+0x351>
  405a61:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  405a65:	89 44 24 98          	mov    %eax,-0x68(%rsp)
  405a69:	c7 44 24 9c 04 00 00 	movl   $0x4,-0x64(%rsp)
  405a70:	00 
  405a71:	8b 44 24 98          	mov    -0x68(%rsp),%eax
  405a75:	8b 4c 24 9c          	mov    -0x64(%rsp),%ecx
  405a79:	89 4c 24 94          	mov    %ecx,-0x6c(%rsp)
  405a7d:	89 44 24 90          	mov    %eax,-0x70(%rsp)
  405a81:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  405a86:	48 83 c4 38          	add    $0x38,%rsp
  405a8a:	c3                   	ret
  405a8b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000405a90 <os::[file_linux.odin]::_file_size>:
  405a90:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  405a97:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  405a9c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405aa1:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  405aa6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405aab:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  405ab2:	00 
  405ab3:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  405aba:	00 00 00 00 00 
  405abf:	48 c7 84 24 00 01 00 	movq   $0x0,0x100(%rsp)
  405ac6:	00 00 00 00 00 
  405acb:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  405ad2:	00 
  405ad3:	8b 78 28             	mov    0x28(%rax),%edi
  405ad6:	48 8d 74 24 70       	lea    0x70(%rsp),%rsi
  405adb:	e8 30 d1 ff ff       	call   402c10 <linux::fstat>
  405ae0:	89 44 24 6c          	mov    %eax,0x6c(%rsp)
  405ae4:	83 7c 24 6c 00       	cmpl   $0x0,0x6c(%rsp)
  405ae9:	0f 95 c0             	setne  %al
  405aec:	24 01                	and    $0x1,%al
  405aee:	3c 00                	cmp    $0x0,%al
  405af0:	74 56                	je     405b48 <os::[file_linux.odin]::_file_size+0xb8>
  405af2:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405af7:	8b 7c 24 6c          	mov    0x6c(%rsp),%edi
  405afb:	e8 20 fc ff ff       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  405b00:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405b05:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  405b0a:	8b 44 24 60          	mov    0x60(%rsp),%eax
  405b0e:	8b 4c 24 64          	mov    0x64(%rsp),%ecx
  405b12:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  405b19:	00 00 00 00 00 
  405b1e:	89 8c 24 04 01 00 00 	mov    %ecx,0x104(%rsp)
  405b25:	89 84 24 00 01 00 00 	mov    %eax,0x100(%rsp)
  405b2c:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  405b33:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  405b37:	89 44 24 50          	mov    %eax,0x50(%rsp)
  405b3b:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  405b40:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  405b47:	c3                   	ret
  405b48:	8b 84 24 88 00 00 00 	mov    0x88(%rsp),%eax
  405b4f:	25 00 f0 00 00       	and    $0xf000,%eax
  405b54:	3d 00 80 00 00       	cmp    $0x8000,%eax
  405b59:	0f 94 c0             	sete   %al
  405b5c:	24 01                	and    $0x1,%al
  405b5e:	3c 00                	cmp    $0x0,%al
  405b60:	74 4b                	je     405bad <os::[file_linux.odin]::_file_size+0x11d>
  405b62:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405b67:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  405b6e:	00 
  405b6f:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  405b76:	00 
  405b77:	c7 84 24 04 01 00 00 	movl   $0x0,0x104(%rsp)
  405b7e:	00 00 00 00 
  405b82:	c7 84 24 00 01 00 00 	movl   $0x0,0x100(%rsp)
  405b89:	00 00 00 00 
  405b8d:	48 89 08             	mov    %rcx,(%rax)
  405b90:	c7 44 24 44 00 00 00 	movl   $0x0,0x44(%rsp)
  405b97:	00 
  405b98:	c7 44 24 40 00 00 00 	movl   $0x0,0x40(%rsp)
  405b9f:	00 
  405ba0:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  405ba5:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  405bac:	c3                   	ret
  405bad:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  405bb4:	00 00 
  405bb6:	31 c0                	xor    %eax,%eax
  405bb8:	a8 01                	test   $0x1,%al
  405bba:	75 02                	jne    405bbe <os::[file_linux.odin]::_file_size+0x12e>
  405bbc:	eb 12                	jmp    405bd0 <os::[file_linux.odin]::_file_size+0x140>
  405bbe:	c7 44 24 3c 00 00 00 	movl   $0x0,0x3c(%rsp)
  405bc5:	00 
  405bc6:	c7 44 24 38 00 00 00 	movl   $0x0,0x38(%rsp)
  405bcd:	00 
  405bce:	eb 10                	jmp    405be0 <os::[file_linux.odin]::_file_size+0x150>
  405bd0:	c7 44 24 38 0f 00 00 	movl   $0xf,0x38(%rsp)
  405bd7:	00 
  405bd8:	c7 44 24 3c 02 00 00 	movl   $0x2,0x3c(%rsp)
  405bdf:	00 
  405be0:	8b 44 24 38          	mov    0x38(%rsp),%eax
  405be4:	8b 4c 24 3c          	mov    0x3c(%rsp),%ecx
  405be8:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  405bef:	00 00 00 00 00 
  405bf4:	89 8c 24 04 01 00 00 	mov    %ecx,0x104(%rsp)
  405bfb:	89 84 24 00 01 00 00 	mov    %eax,0x100(%rsp)
  405c02:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  405c09:	00 00 
  405c0b:	31 c0                	xor    %eax,%eax
  405c0d:	a8 01                	test   $0x1,%al
  405c0f:	75 02                	jne    405c13 <os::[file_linux.odin]::_file_size+0x183>
  405c11:	eb 12                	jmp    405c25 <os::[file_linux.odin]::_file_size+0x195>
  405c13:	c7 44 24 34 00 00 00 	movl   $0x0,0x34(%rsp)
  405c1a:	00 
  405c1b:	c7 44 24 30 00 00 00 	movl   $0x0,0x30(%rsp)
  405c22:	00 
  405c23:	eb 10                	jmp    405c35 <os::[file_linux.odin]::_file_size+0x1a5>
  405c25:	c7 44 24 30 0f 00 00 	movl   $0xf,0x30(%rsp)
  405c2c:	00 
  405c2d:	c7 44 24 34 02 00 00 	movl   $0x2,0x34(%rsp)
  405c34:	00 
  405c35:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405c3a:	8b 44 24 30          	mov    0x30(%rsp),%eax
  405c3e:	8b 4c 24 34          	mov    0x34(%rsp),%ecx
  405c42:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  405c49:	89 4c 24 24          	mov    %ecx,0x24(%rsp)
  405c4d:	89 44 24 20          	mov    %eax,0x20(%rsp)
  405c51:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405c56:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  405c5d:	c3                   	ret
  405c5e:	66 90                	xchg   %ax,%ax

0000000000405c60 <os::[file_linux.odin]::_flush>:
  405c60:	48 83 ec 38          	sub    $0x38,%rsp
  405c64:	48 89 3c 24          	mov    %rdi,(%rsp)
  405c68:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  405c6d:	48 8b 04 24          	mov    (%rsp),%rax
  405c71:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  405c76:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  405c7b:	8b 78 28             	mov    0x28(%rax),%edi
  405c7e:	e8 3d d2 ff ff       	call   402ec0 <linux::fsync>
  405c83:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  405c88:	89 c7                	mov    %eax,%edi
  405c8a:	e8 91 fa ff ff       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  405c8f:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  405c94:	8b 44 24 20          	mov    0x20(%rsp),%eax
  405c98:	8b 4c 24 24          	mov    0x24(%rsp),%ecx
  405c9c:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  405ca0:	89 44 24 10          	mov    %eax,0x10(%rsp)
  405ca4:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405ca9:	48 83 c4 38          	add    $0x38,%rsp
  405cad:	c3                   	ret
  405cae:	66 90                	xchg   %ax,%ax

0000000000405cb0 <os::[file_linux.odin]::_read_link_cstr>:
  405cb0:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  405cb7:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  405cbc:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  405cc1:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  405cc6:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405ccb:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405cd0:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  405cd5:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  405cda:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  405cdf:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  405ce4:	48 89 94 24 30 01 00 	mov    %rdx,0x130(%rsp)
  405ceb:	00 
  405cec:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  405cf3:	00 
  405cf4:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  405cfb:	00 
  405cfc:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  405d03:	00 
  405d04:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  405d09:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  405d10:	00 
  405d11:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  405d16:	48 c7 84 24 18 01 00 	movq   $0x100,0x118(%rsp)
  405d1d:	00 00 01 00 00 
  405d22:	48 8b bc 24 18 01 00 	mov    0x118(%rsp),%rdi
  405d29:	00 
  405d2a:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  405d31:	00 
  405d32:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  405d39:	00 
  405d3a:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  405d41:	00 
  405d42:	48 8b 94 24 08 01 00 	mov    0x108(%rsp),%rdx
  405d49:	00 
  405d4a:	0f 57 c0             	xorps  %xmm0,%xmm0
  405d4d:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  405d54:	00 
  405d55:	b9 00 e4 40 00       	mov    $0x40e400,%ecx
  405d5a:	4c 8d 84 24 f0 00 00 	lea    0xf0(%rsp),%r8
  405d61:	00 
  405d62:	e8 59 63 00 00       	call   40c0c0 <runtime::make_slice:proc(T:$[]u8,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]u8,err:runtime::Allocator_Error)>
  405d67:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  405d6e:	00 
  405d6f:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  405d76:	00 
  405d77:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  405d7e:	00 
  405d7f:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  405d86:	00 
  405d87:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  405d8c:	48 8b b4 24 e0 00 00 	mov    0xe0(%rsp),%rsi
  405d93:	00 
  405d94:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  405d9b:	00 
  405d9c:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  405da3:	00 00 00 00 00 
  405da8:	48 8d 8c 24 d8 00 00 	lea    0xd8(%rsp),%rcx
  405daf:	00 
  405db0:	e8 3b d1 ff ff       	call   402ef0 <linux::readlink>
  405db5:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  405dbc:	00 
  405dbd:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  405dc4:	00 
  405dc5:	89 84 24 cc 00 00 00 	mov    %eax,0xcc(%rsp)
  405dcc:	83 bc 24 cc 00 00 00 	cmpl   $0x0,0xcc(%rsp)
  405dd3:	00 
  405dd4:	0f 95 c0             	setne  %al
  405dd7:	24 01                	and    $0x1,%al
  405dd9:	3c 00                	cmp    $0x0,%al
  405ddb:	0f 84 9a 00 00 00    	je     405e7b <os::[file_linux.odin]::_read_link_cstr+0x1cb>
  405de1:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  405de6:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405deb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405df0:	48 8b bc 24 e0 00 00 	mov    0xe0(%rsp),%rdi
  405df7:	00 
  405df8:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  405dff:	00 
  405e00:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  405e07:	00 
  405e08:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  405e0f:	00 
  405e10:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  405e17:	00 
  405e18:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  405e1f:	00 
  405e20:	41 b8 30 e4 40 00    	mov    $0x40e430,%r8d
  405e26:	e8 f5 5f 00 00       	call   40be20 <runtime::delete_slice:proc(array:[]u8,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  405e2b:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405e30:	8b bc 24 cc 00 00 00 	mov    0xcc(%rsp),%edi
  405e37:	e8 e4 f8 ff ff       	call   405720 <os::[errors_linux.odin]::_get_platform_error>
  405e3c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  405e41:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  405e48:	00 
  405e49:	8b 84 24 a0 00 00 00 	mov    0xa0(%rsp),%eax
  405e50:	8b 8c 24 a4 00 00 00 	mov    0xa4(%rsp),%ecx
  405e57:	0f 57 c0             	xorps  %xmm0,%xmm0
  405e5a:	0f 11 02             	movups %xmm0,(%rdx)
  405e5d:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  405e64:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  405e6b:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  405e72:	00 
  405e73:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  405e7a:	c3                   	ret
  405e7b:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  405e82:	00 
  405e83:	48 3b 84 24 18 01 00 	cmp    0x118(%rsp),%rax
  405e8a:	00 
  405e8b:	0f 94 c0             	sete   %al
  405e8e:	24 01                	and    $0x1,%al
  405e90:	3c 00                	cmp    $0x0,%al
  405e92:	0f 84 bb 00 00 00    	je     405f53 <os::[file_linux.odin]::_read_link_cstr+0x2a3>
  405e98:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  405e9d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405ea2:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  405ea7:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  405eae:	00 
  405eaf:	48 01 d2             	add    %rdx,%rdx
  405eb2:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  405eb9:	00 
  405eba:	48 8b bc 24 e0 00 00 	mov    0xe0(%rsp),%rdi
  405ec1:	00 
  405ec2:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  405ec9:	00 
  405eca:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  405ed1:	00 
  405ed2:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  405ed9:	00 
  405eda:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  405ee1:	00 
  405ee2:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405ee9:	00 
  405eea:	41 b8 60 e4 40 00    	mov    $0x40e460,%r8d
  405ef0:	e8 2b 5f 00 00       	call   40be20 <runtime::delete_slice:proc(array:[]u8,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  405ef5:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  405efa:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  405eff:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405f04:	48 8b bc 24 18 01 00 	mov    0x118(%rsp),%rdi
  405f0b:	00 
  405f0c:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  405f11:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  405f16:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  405f1b:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  405f20:	0f 57 c0             	xorps  %xmm0,%xmm0
  405f23:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  405f28:	b9 90 e4 40 00       	mov    $0x40e490,%ecx
  405f2d:	4c 8d 44 24 60       	lea    0x60(%rsp),%r8
  405f32:	e8 89 61 00 00       	call   40c0c0 <runtime::make_slice:proc(T:$[]u8,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]u8,err:runtime::Allocator_Error)>
  405f37:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  405f3c:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  405f41:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  405f48:	00 
  405f49:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  405f50:	00 
  405f51:	eb 76                	jmp    405fc9 <os::[file_linux.odin]::_read_link_cstr+0x319>
  405f53:	4c 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%r8
  405f5a:	00 
  405f5b:	4c 89 04 24          	mov    %r8,(%rsp)
  405f5f:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  405f66:	00 
  405f67:	bf c0 e2 40 00       	mov    $0x40e2c0,%edi
  405f6c:	be 25 00 00 00       	mov    $0x25,%esi
  405f71:	ba 66 01 00 00       	mov    $0x166,%edx
  405f76:	b9 15 00 00 00       	mov    $0x15,%ecx
  405f7b:	e8 00 4a 00 00       	call   40a980 <runtime::slice_expr_error_hi>
  405f80:	48 8b 0c 24          	mov    (%rsp),%rcx
  405f84:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405f89:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  405f90:	00 
  405f91:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  405f96:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  405f9b:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  405fa0:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  405fa5:	48 89 50 08          	mov    %rdx,0x8(%rax)
  405fa9:	48 89 08             	mov    %rcx,(%rax)
  405fac:	c7 44 24 44 00 00 00 	movl   $0x0,0x44(%rsp)
  405fb3:	00 
  405fb4:	c7 44 24 40 00 00 00 	movl   $0x0,0x40(%rsp)
  405fbb:	00 
  405fbc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  405fc1:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  405fc8:	c3                   	ret
  405fc9:	eb 00                	jmp    405fcb <os::[file_linux.odin]::_read_link_cstr+0x31b>
  405fcb:	e9 b7 fd ff ff       	jmp    405d87 <os::[file_linux.odin]::_read_link_cstr+0xd7>

0000000000405fd0 <os::split_path>:
  405fd0:	48 83 ec 78          	sub    $0x78,%rsp
  405fd4:	48 89 0c 24          	mov    %rcx,(%rsp)
  405fd8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405fdd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405fe2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405fe7:	48 8b 0c 24          	mov    (%rsp),%rcx
  405feb:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405ff0:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405ff5:	48 89 7c 24 68       	mov    %rdi,0x68(%rsp)
  405ffa:	48 89 74 24 70       	mov    %rsi,0x70(%rsp)
  405fff:	0f 57 c0             	xorps  %xmm0,%xmm0
  406002:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406007:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40600c:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  406011:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  406016:	e8 25 db ff ff       	call   403b40 <os::[path_posixfs.odin]::_split_path>
  40601b:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406020:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406025:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40602a:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  40602f:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  406034:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  406039:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40603e:	48 89 79 08          	mov    %rdi,0x8(%rcx)
  406042:	48 89 31             	mov    %rsi,(%rcx)
  406045:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40604a:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40604f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406054:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406059:	48 83 c4 78          	add    $0x78,%rsp
  40605d:	c3                   	ret
  40605e:	66 90                	xchg   %ax,%ax

0000000000406060 <os::[file_linux.odin]::_file_stream_proc>:
  406060:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  406067:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40606c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  406071:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  406076:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40607b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  406080:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  406085:	48 8b 84 24 58 02 00 	mov    0x258(%rsp),%rax
  40608c:	00 
  40608d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  406092:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  406099:	00 
  40609a:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40609f:	48 8d 84 24 40 02 00 	lea    0x240(%rsp),%rax
  4060a6:	00 
  4060a7:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4060ac:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4060b1:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4060b6:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  4060bb:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4060c0:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4060c5:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  4060ca:	4c 8b 4c 24 38       	mov    0x38(%rsp),%r9
  4060cf:	48 89 8c 24 30 02 00 	mov    %rcx,0x230(%rsp)
  4060d6:	00 
  4060d7:	48 89 84 24 28 02 00 	mov    %rax,0x228(%rsp)
  4060de:	00 
  4060df:	4c 89 8c 24 20 02 00 	mov    %r9,0x220(%rsp)
  4060e6:	00 
  4060e7:	4c 89 84 24 18 02 00 	mov    %r8,0x218(%rsp)
  4060ee:	00 
  4060ef:	48 89 bc 24 10 02 00 	mov    %rdi,0x210(%rsp)
  4060f6:	00 
  4060f7:	48 89 b4 24 08 02 00 	mov    %rsi,0x208(%rsp)
  4060fe:	00 
  4060ff:	0f 10 02             	movups (%rdx),%xmm0
  406102:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  406109:	00 
  40610a:	48 c7 84 24 e8 01 00 	movq   $0x0,0x1e8(%rsp)
  406111:	00 00 00 00 00 
  406116:	48 c7 84 24 e0 01 00 	movq   $0x0,0x1e0(%rsp)
  40611d:	00 00 00 00 00 
  406122:	48 89 8c 24 d8 01 00 	mov    %rcx,0x1d8(%rsp)
  406129:	00 
  40612a:	48 89 c1             	mov    %rax,%rcx
  40612d:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  406132:	48 83 e8 0a          	sub    $0xa,%rax
  406136:	0f 87 f0 06 00 00    	ja     40682c <os::[file_linux.odin]::_file_stream_proc+0x7cc>
  40613c:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406141:	48 8b 04 c5 40 e1 40 	mov    0x40e140(,%rax,8),%rax
  406148:	00 
  406149:	ff e0                	jmp    *%rax
  40614b:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  406150:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406155:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40615a:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  406161:	00 
  406162:	48 c7 84 24 d0 01 00 	movq   $0x0,0x1d0(%rsp)
  406169:	00 00 00 00 00 
  40616e:	48 8d 8c 24 d0 01 00 	lea    0x1d0(%rsp),%rcx
  406175:	00 
  406176:	e8 65 ea ff ff       	call   404be0 <os::[file_linux.odin]::_read>
  40617b:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  406180:	48 89 84 24 c0 01 00 	mov    %rax,0x1c0(%rsp)
  406187:	00 
  406188:	8b 84 24 c0 01 00 00 	mov    0x1c0(%rsp),%eax
  40618f:	8b 8c 24 c4 01 00 00 	mov    0x1c4(%rsp),%ecx
  406196:	48 8b b4 24 d0 01 00 	mov    0x1d0(%rsp),%rsi
  40619d:	00 
  40619e:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  4061a5:	00 
  4061a6:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  4061ad:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  4061b4:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  4061bb:	00 
  4061bc:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  4061c3:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  4061ca:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  4061d1:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  4061d8:	48 89 32             	mov    %rsi,(%rdx)
  4061db:	89 8c 24 b4 01 00 00 	mov    %ecx,0x1b4(%rsp)
  4061e2:	89 84 24 b0 01 00 00 	mov    %eax,0x1b0(%rsp)
  4061e9:	48 8b 84 24 b0 01 00 	mov    0x1b0(%rsp),%rax
  4061f0:	00 
  4061f1:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4061f8:	c3                   	ret
  4061f9:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  4061fe:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  406203:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406208:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40620d:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  406214:	00 
  406215:	48 c7 84 24 a8 01 00 	movq   $0x0,0x1a8(%rsp)
  40621c:	00 00 00 00 00 
  406221:	4c 8d 84 24 a8 01 00 	lea    0x1a8(%rsp),%r8
  406228:	00 
  406229:	e8 d2 eb ff ff       	call   404e00 <os::[file_linux.odin]::_read_at>
  40622e:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  406233:	48 89 84 24 a0 01 00 	mov    %rax,0x1a0(%rsp)
  40623a:	00 
  40623b:	8b 84 24 a0 01 00 00 	mov    0x1a0(%rsp),%eax
  406242:	8b 8c 24 a4 01 00 00 	mov    0x1a4(%rsp),%ecx
  406249:	48 8b b4 24 a8 01 00 	mov    0x1a8(%rsp),%rsi
  406250:	00 
  406251:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  406258:	00 
  406259:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406260:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  406267:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40626e:	00 
  40626f:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  406276:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40627d:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406284:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40628b:	48 89 32             	mov    %rsi,(%rdx)
  40628e:	89 8c 24 94 01 00 00 	mov    %ecx,0x194(%rsp)
  406295:	89 84 24 90 01 00 00 	mov    %eax,0x190(%rsp)
  40629c:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4062a3:	00 
  4062a4:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4062ab:	c3                   	ret
  4062ac:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  4062b1:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4062b6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4062bb:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  4062c2:	00 
  4062c3:	48 c7 84 24 88 01 00 	movq   $0x0,0x188(%rsp)
  4062ca:	00 00 00 00 00 
  4062cf:	48 8d 8c 24 88 01 00 	lea    0x188(%rsp),%rcx
  4062d6:	00 
  4062d7:	e8 e4 ed ff ff       	call   4050c0 <os::[file_linux.odin]::_write>
  4062dc:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4062e1:	48 89 84 24 80 01 00 	mov    %rax,0x180(%rsp)
  4062e8:	00 
  4062e9:	8b 84 24 80 01 00 00 	mov    0x180(%rsp),%eax
  4062f0:	8b 8c 24 84 01 00 00 	mov    0x184(%rsp),%ecx
  4062f7:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  4062fe:	00 
  4062ff:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  406306:	00 
  406307:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40630e:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  406315:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40631c:	00 
  40631d:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  406324:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40632b:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406332:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  406339:	48 89 32             	mov    %rsi,(%rdx)
  40633c:	89 8c 24 74 01 00 00 	mov    %ecx,0x174(%rsp)
  406343:	89 84 24 70 01 00 00 	mov    %eax,0x170(%rsp)
  40634a:	48 8b 84 24 70 01 00 	mov    0x170(%rsp),%rax
  406351:	00 
  406352:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  406359:	c3                   	ret
  40635a:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40635f:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  406364:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406369:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40636e:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  406375:	00 
  406376:	48 c7 84 24 68 01 00 	movq   $0x0,0x168(%rsp)
  40637d:	00 00 00 00 00 
  406382:	4c 8d 84 24 68 01 00 	lea    0x168(%rsp),%r8
  406389:	00 
  40638a:	e8 c1 ef ff ff       	call   405350 <os::[file_linux.odin]::_write_at>
  40638f:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  406394:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  40639b:	00 
  40639c:	8b 84 24 60 01 00 00 	mov    0x160(%rsp),%eax
  4063a3:	8b 8c 24 64 01 00 00 	mov    0x164(%rsp),%ecx
  4063aa:	48 8b b4 24 68 01 00 	mov    0x168(%rsp),%rsi
  4063b1:	00 
  4063b2:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  4063b9:	00 
  4063ba:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  4063c1:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  4063c8:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  4063cf:	00 
  4063d0:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  4063d7:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  4063de:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  4063e5:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  4063ec:	48 89 32             	mov    %rsi,(%rdx)
  4063ef:	89 8c 24 54 01 00 00 	mov    %ecx,0x154(%rsp)
  4063f6:	89 84 24 50 01 00 00 	mov    %eax,0x150(%rsp)
  4063fd:	48 8b 84 24 50 01 00 	mov    0x150(%rsp),%rax
  406404:	00 
  406405:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40640c:	c3                   	ret
  40640d:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  406412:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406417:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40641c:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  406423:	00 
  406424:	48 c7 84 24 48 01 00 	movq   $0x0,0x148(%rsp)
  40642b:	00 00 00 00 00 
  406430:	48 8d 8c 24 48 01 00 	lea    0x148(%rsp),%rcx
  406437:	00 
  406438:	e8 c3 e1 ff ff       	call   404600 <os::[file_linux.odin]::_seek>
  40643d:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  406442:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  406449:	00 
  40644a:	8b 84 24 40 01 00 00 	mov    0x140(%rsp),%eax
  406451:	8b 8c 24 44 01 00 00 	mov    0x144(%rsp),%ecx
  406458:	48 8b b4 24 48 01 00 	mov    0x148(%rsp),%rsi
  40645f:	00 
  406460:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  406467:	00 
  406468:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40646f:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  406476:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40647d:	00 
  40647e:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  406485:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40648c:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406493:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40649a:	48 89 32             	mov    %rsi,(%rdx)
  40649d:	89 8c 24 34 01 00 00 	mov    %ecx,0x134(%rsp)
  4064a4:	89 84 24 30 01 00 00 	mov    %eax,0x130(%rsp)
  4064ab:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4064b2:	00 
  4064b3:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4064ba:	c3                   	ret
  4064bb:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4064c0:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  4064c7:	00 
  4064c8:	48 c7 84 24 28 01 00 	movq   $0x0,0x128(%rsp)
  4064cf:	00 00 00 00 00 
  4064d4:	48 8d b4 24 28 01 00 	lea    0x128(%rsp),%rsi
  4064db:	00 
  4064dc:	e8 af f5 ff ff       	call   405a90 <os::[file_linux.odin]::_file_size>
  4064e1:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4064e6:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4064ed:	00 
  4064ee:	8b 84 24 20 01 00 00 	mov    0x120(%rsp),%eax
  4064f5:	8b 8c 24 24 01 00 00 	mov    0x124(%rsp),%ecx
  4064fc:	48 8b b4 24 28 01 00 	mov    0x128(%rsp),%rsi
  406503:	00 
  406504:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  40650b:	00 
  40650c:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406513:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40651a:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  406521:	00 
  406522:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  406529:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  406530:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406537:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40653e:	48 89 32             	mov    %rsi,(%rdx)
  406541:	89 8c 24 14 01 00 00 	mov    %ecx,0x114(%rsp)
  406548:	89 84 24 10 01 00 00 	mov    %eax,0x110(%rsp)
  40654f:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  406556:	00 
  406557:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40655e:	c3                   	ret
  40655f:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  406564:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40656b:	00 
  40656c:	e8 ef f6 ff ff       	call   405c60 <os::[file_linux.odin]::_flush>
  406571:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  406576:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  40657d:	00 
  40657e:	8b 84 24 00 01 00 00 	mov    0x100(%rsp),%eax
  406585:	8b 8c 24 04 01 00 00 	mov    0x104(%rsp),%ecx
  40658c:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406593:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40659a:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  4065a1:	00 
  4065a2:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  4065a9:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  4065b0:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  4065b7:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  4065be:	48 89 32             	mov    %rsi,(%rdx)
  4065c1:	89 8c 24 f4 00 00 00 	mov    %ecx,0xf4(%rsp)
  4065c8:	89 84 24 f0 00 00 00 	mov    %eax,0xf0(%rsp)
  4065cf:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4065d6:	00 
  4065d7:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4065de:	c3                   	ret
  4065df:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4065e4:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  4065eb:	00 
  4065ec:	e8 3f df ff ff       	call   404530 <os::[file_linux.odin]::_close>
  4065f1:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4065f6:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4065fd:	00 
  4065fe:	8b 84 24 e0 00 00 00 	mov    0xe0(%rsp),%eax
  406605:	8b 8c 24 e4 00 00 00 	mov    0xe4(%rsp),%ecx
  40660c:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406613:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40661a:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  406621:	00 
  406622:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  406629:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  406630:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406637:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40663e:	48 89 32             	mov    %rsi,(%rdx)
  406641:	89 8c 24 d4 00 00 00 	mov    %ecx,0xd4(%rsp)
  406648:	89 84 24 d0 00 00 00 	mov    %eax,0xd0(%rsp)
  40664f:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  406656:	00 
  406657:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40665e:	c3                   	ret
  40665f:	48 c7 84 24 c8 00 00 	movq   $0x0,0xc8(%rsp)
  406666:	00 00 00 00 00 
  40666b:	bf ff 03 00 00       	mov    $0x3ff,%edi
  406670:	48 8d b4 24 c8 00 00 	lea    0xc8(%rsp),%rsi
  406677:	00 
  406678:	e8 03 08 00 00       	call   406e80 <io::query_utility>
  40667d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  406681:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  406688:	00 
  406689:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40668e:	48 c7 84 24 c0 00 00 	movq   $0x0,0xc0(%rsp)
  406695:	00 00 00 00 00 
  40669a:	83 f8 00             	cmp    $0x0,%eax
  40669d:	75 18                	jne    4066b7 <os::[file_linux.odin]::_file_stream_proc+0x657>
  40669f:	c7 84 24 c4 00 00 00 	movl   $0x0,0xc4(%rsp)
  4066a6:	00 00 00 00 
  4066aa:	c7 84 24 c0 00 00 00 	movl   $0x0,0xc0(%rsp)
  4066b1:	00 00 00 00 
  4066b5:	eb 16                	jmp    4066cd <os::[file_linux.odin]::_file_stream_proc+0x66d>
  4066b7:	8b 44 24 04          	mov    0x4(%rsp),%eax
  4066bb:	89 84 24 c0 00 00 00 	mov    %eax,0xc0(%rsp)
  4066c2:	c7 84 24 c4 00 00 00 	movl   $0x2,0xc4(%rsp)
  4066c9:	02 00 00 00 
  4066cd:	8b 44 24 04          	mov    0x4(%rsp),%eax
  4066d1:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4066d6:	8b 8c 24 c0 00 00 00 	mov    0xc0(%rsp),%ecx
  4066dd:	8b 94 24 c4 00 00 00 	mov    0xc4(%rsp),%edx
  4066e4:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  4066eb:	00 
  4066ec:	89 94 24 e4 01 00 00 	mov    %edx,0x1e4(%rsp)
  4066f3:	89 8c 24 e0 01 00 00 	mov    %ecx,0x1e0(%rsp)
  4066fa:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  406701:	00 00 00 00 00 
  406706:	83 f8 00             	cmp    $0x0,%eax
  406709:	75 18                	jne    406723 <os::[file_linux.odin]::_file_stream_proc+0x6c3>
  40670b:	c7 84 24 bc 00 00 00 	movl   $0x0,0xbc(%rsp)
  406712:	00 00 00 00 
  406716:	c7 84 24 b8 00 00 00 	movl   $0x0,0xb8(%rsp)
  40671d:	00 00 00 00 
  406721:	eb 16                	jmp    406739 <os::[file_linux.odin]::_file_stream_proc+0x6d9>
  406723:	8b 44 24 04          	mov    0x4(%rsp),%eax
  406727:	89 84 24 b8 00 00 00 	mov    %eax,0xb8(%rsp)
  40672e:	c7 84 24 bc 00 00 00 	movl   $0x2,0xbc(%rsp)
  406735:	02 00 00 00 
  406739:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40673e:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  406743:	8b 84 24 b8 00 00 00 	mov    0xb8(%rsp),%eax
  40674a:	8b 8c 24 bc 00 00 00 	mov    0xbc(%rsp),%ecx
  406751:	48 89 32             	mov    %rsi,(%rdx)
  406754:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  40675b:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  406762:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  406769:	00 
  40676a:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  406771:	c3                   	ret
  406772:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  406777:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40677c:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  406781:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  406788:	00 
  406789:	48 8b 84 24 f0 01 00 	mov    0x1f0(%rsp),%rax
  406790:	00 
  406791:	48 8b 8c 24 f8 01 00 	mov    0x1f8(%rsp),%rcx
  406798:	00 
  406799:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  4067a0:	00 
  4067a1:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4067a8:	00 
  4067a9:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4067b0:	00 
  4067b1:	4c 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%r8
  4067b8:	00 
  4067b9:	e8 d2 d7 ff ff       	call   403f90 <os::[file_stream.odin]::file_stream_fstat_utility>
  4067be:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4067c3:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4067ca:	00 
  4067cb:	8b 84 24 90 00 00 00 	mov    0x90(%rsp),%eax
  4067d2:	8b 8c 24 94 00 00 00 	mov    0x94(%rsp),%ecx
  4067d9:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  4067e0:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  4067e7:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  4067ee:	00 
  4067ef:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  4067f6:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  4067fd:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  406804:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40680b:	48 89 32             	mov    %rsi,(%rdx)
  40680e:	89 8c 24 84 00 00 00 	mov    %ecx,0x84(%rsp)
  406815:	89 84 24 80 00 00 00 	mov    %eax,0x80(%rsp)
  40681c:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  406823:	00 
  406824:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40682b:	c3                   	ret
  40682c:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  406833:	00 00 
  406835:	31 c0                	xor    %eax,%eax
  406837:	a8 01                	test   $0x1,%al
  406839:	75 02                	jne    40683d <os::[file_linux.odin]::_file_stream_proc+0x7dd>
  40683b:	eb 12                	jmp    40684f <os::[file_linux.odin]::_file_stream_proc+0x7ef>
  40683d:	c7 44 24 7c 00 00 00 	movl   $0x0,0x7c(%rsp)
  406844:	00 
  406845:	c7 44 24 78 00 00 00 	movl   $0x0,0x78(%rsp)
  40684c:	00 
  40684d:	eb 10                	jmp    40685f <os::[file_linux.odin]::_file_stream_proc+0x7ff>
  40684f:	c7 44 24 78 ff ff ff 	movl   $0xffffffff,0x78(%rsp)
  406856:	ff 
  406857:	c7 44 24 7c 02 00 00 	movl   $0x2,0x7c(%rsp)
  40685e:	00 
  40685f:	8b 44 24 78          	mov    0x78(%rsp),%eax
  406863:	8b 4c 24 7c          	mov    0x7c(%rsp),%ecx
  406867:	48 c7 84 24 e8 01 00 	movq   $0x0,0x1e8(%rsp)
  40686e:	00 00 00 00 00 
  406873:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40687a:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  406881:	48 c7 44 24 70 00 00 	movq   $0x0,0x70(%rsp)
  406888:	00 00 
  40688a:	31 c0                	xor    %eax,%eax
  40688c:	a8 01                	test   $0x1,%al
  40688e:	75 02                	jne    406892 <os::[file_linux.odin]::_file_stream_proc+0x832>
  406890:	eb 12                	jmp    4068a4 <os::[file_linux.odin]::_file_stream_proc+0x844>
  406892:	c7 44 24 74 00 00 00 	movl   $0x0,0x74(%rsp)
  406899:	00 
  40689a:	c7 44 24 70 00 00 00 	movl   $0x0,0x70(%rsp)
  4068a1:	00 
  4068a2:	eb 10                	jmp    4068b4 <os::[file_linux.odin]::_file_stream_proc+0x854>
  4068a4:	c7 44 24 70 ff ff ff 	movl   $0xffffffff,0x70(%rsp)
  4068ab:	ff 
  4068ac:	c7 44 24 74 02 00 00 	movl   $0x2,0x74(%rsp)
  4068b3:	00 
  4068b4:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4068b9:	8b 44 24 70          	mov    0x70(%rsp),%eax
  4068bd:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  4068c1:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  4068c8:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  4068cc:	89 44 24 60          	mov    %eax,0x60(%rsp)
  4068d0:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4068d5:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4068dc:	c3                   	ret
  4068dd:	0f 1f 00             	nopl   (%rax)

00000000004068e0 <os::[file_linux.odin]::_standard_stream_init.new_std-0>:
  4068e0:	48 83 ec 78          	sub    $0x78,%rsp
  4068e4:	89 74 24 14          	mov    %esi,0x14(%rsp)
  4068e8:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4068ed:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4068f2:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4068f7:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4068fc:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  406901:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406906:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  40690a:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  40690f:	89 4c 24 6c          	mov    %ecx,0x6c(%rsp)
  406913:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  406918:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40691d:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  406922:	48 89 10             	mov    %rdx,(%rax)
  406925:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40692a:	89 48 28             	mov    %ecx,0x28(%rax)
  40692d:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  406932:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  406937:	e8 14 53 00 00       	call   40bc50 <runtime::nil_allocator>
  40693c:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406941:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  406946:	48 89 c7             	mov    %rax,%rdi
  406949:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40694e:	49 89 d0             	mov    %rdx,%r8
  406951:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406956:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  40695b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  406960:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  406965:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40696a:	4c 89 40 38          	mov    %r8,0x38(%rax)
  40696e:	48 89 78 30          	mov    %rdi,0x30(%rax)
  406972:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  406977:	48 89 70 20          	mov    %rsi,0x20(%rax)
  40697b:	48 89 50 18          	mov    %rdx,0x18(%rax)
  40697f:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  406984:	0f 57 c0             	xorps  %xmm0,%xmm0
  406987:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40698c:	48 c7 c2 60 60 40 00 	mov    $0x406060,%rdx
  406993:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406998:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  40699f:	00 00 
  4069a1:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4069a6:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4069ab:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4069b0:	48 89 50 10          	mov    %rdx,0x10(%rax)
  4069b4:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4069b8:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4069bd:	48 83 c4 78          	add    $0x78,%rsp
  4069c1:	c3                   	ret
  4069c2:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4069c9:	00 00 00 
  4069cc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004069d0 <runtime::stderr_write>:
  4069d0:	48 83 ec 38          	sub    $0x38,%rsp
  4069d4:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4069d9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4069de:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4069e3:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4069e8:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4069ed:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4069f2:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  4069f7:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  4069fe:	00 00 
  406a00:	48 8d 54 24 20       	lea    0x20(%rsp),%rdx
  406a05:	e8 b6 04 00 00       	call   406ec0 <runtime::[os_specific_linux.odin]::_stderr_write>
  406a0a:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406a0f:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  406a14:	48 89 11             	mov    %rdx,(%rcx)
  406a17:	48 83 c4 38          	add    $0x38,%rsp
  406a1b:	c3                   	ret
  406a1c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000406a20 <runtime::rand_bytes>:
  406a20:	48 83 ec 28          	sub    $0x28,%rsp
  406a24:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  406a29:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  406a2e:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  406a33:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406a38:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406a3d:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  406a42:	e8 f9 04 00 00       	call   406f40 <runtime::[os_specific_linux.odin]::_rand_bytes>
  406a47:	48 83 c4 28          	add    $0x28,%rsp
  406a4b:	c3                   	ret
  406a4c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000406a50 <runtime::print_string>:
  406a50:	48 83 ec 48          	sub    $0x48,%rsp
  406a54:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  406a59:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  406a5e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406a63:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406a68:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406a6d:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  406a72:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  406a79:	00 00 
  406a7b:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  406a80:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406a85:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406a8a:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  406a8f:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  406a96:	00 00 
  406a98:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  406a9d:	e8 2e ff ff ff       	call   4069d0 <runtime::stderr_write>
  406aa2:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  406aa7:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406aac:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  406ab1:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406ab6:	48 83 c4 48          	add    $0x48,%rsp
  406aba:	c3                   	ret
  406abb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000406ac0 <runtime::print_byte>:
  406ac0:	48 83 ec 58          	sub    $0x58,%rsp
  406ac4:	40 88 f8             	mov    %dil,%al
  406ac7:	88 44 24 07          	mov    %al,0x7(%rsp)
  406acb:	8a 54 24 07          	mov    0x7(%rsp),%dl
  406acf:	88 54 24 57          	mov    %dl,0x57(%rsp)
  406ad3:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  406ada:	00 00 
  406adc:	0f 57 c0             	xorps  %xmm0,%xmm0
  406adf:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  406ae4:	c6 44 24 20 00       	movb   $0x0,0x20(%rsp)
  406ae9:	48 8d 44 24 20       	lea    0x20(%rsp),%rax
  406aee:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406af3:	48 c7 44 24 18 01 00 	movq   $0x1,0x18(%rsp)
  406afa:	00 00 
  406afc:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406b01:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  406b06:	88 11                	mov    %dl,(%rcx)
  406b08:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406b0d:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406b12:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406b17:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  406b1c:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  406b23:	00 00 
  406b25:	48 8d 54 24 08       	lea    0x8(%rsp),%rdx
  406b2a:	e8 a1 fe ff ff       	call   4069d0 <runtime::stderr_write>
  406b2f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406b34:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  406b39:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  406b3e:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  406b43:	48 83 c4 58          	add    $0x58,%rsp
  406b47:	c3                   	ret
  406b48:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  406b4f:	00 

0000000000406b50 <runtime::print_u64>:
  406b50:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  406b57:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406b5c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  406b61:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  406b68:	00 
  406b69:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  406b70:	00 
  406b71:	48 8d 7c 24 47       	lea    0x47(%rsp),%rdi
  406b76:	31 f6                	xor    %esi,%esi
  406b78:	ba 81 00 00 00       	mov    $0x81,%edx
  406b7d:	e8 be a4 ff ff       	call   401040 <memset@plt>
  406b82:	48 c7 44 24 38 81 00 	movq   $0x81,0x38(%rsp)
  406b89:	00 00 
  406b8b:	48 83 bc 24 c8 00 00 	cmpq   $0xa,0xc8(%rsp)
  406b92:	00 0a 
  406b94:	0f 93 c0             	setae  %al
  406b97:	24 01                	and    $0x1,%al
  406b99:	3c 00                	cmp    $0x0,%al
  406b9b:	74 5c                	je     406bf9 <runtime::print_u64+0xa9>
  406b9d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406ba2:	48 83 e8 01          	sub    $0x1,%rax
  406ba6:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406bab:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406bb0:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406bb5:	48 c7 c0 48 30 41 00 	mov    $0x413048,%rax
  406bbc:	48 8b 08             	mov    (%rax),%rcx
  406bbf:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406bc6:	00 
  406bc7:	be 0a 00 00 00       	mov    $0xa,%esi
  406bcc:	31 d2                	xor    %edx,%edx
  406bce:	48 f7 f6             	div    %rsi
  406bd1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406bd6:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  406bd9:	88 4c 04 47          	mov    %cl,0x47(%rsp,%rax,1)
  406bdd:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406be4:	00 
  406be5:	b9 0a 00 00 00       	mov    $0xa,%ecx
  406bea:	31 d2                	xor    %edx,%edx
  406bec:	48 f7 f1             	div    %rcx
  406bef:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  406bf6:	00 
  406bf7:	eb 92                	jmp    406b8b <runtime::print_u64+0x3b>
  406bf9:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406bfe:	48 ff c8             	dec    %rax
  406c01:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406c06:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406c0b:	48 89 04 24          	mov    %rax,(%rsp)
  406c0f:	48 c7 c0 48 30 41 00 	mov    $0x413048,%rax
  406c16:	48 8b 08             	mov    (%rax),%rcx
  406c19:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406c20:	00 
  406c21:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  406c26:	48 ba cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rdx
  406c2d:	cc cc cc 
  406c30:	48 f7 e2             	mul    %rdx
  406c33:	48 8b 04 24          	mov    (%rsp),%rax
  406c37:	48 89 d6             	mov    %rdx,%rsi
  406c3a:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  406c3f:	48 c1 ee 02          	shr    $0x2,%rsi
  406c43:	48 83 e6 fe          	and    $0xfffffffffffffffe,%rsi
  406c47:	48 8d 34 b6          	lea    (%rsi,%rsi,4),%rsi
  406c4b:	48 29 f2             	sub    %rsi,%rdx
  406c4e:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  406c51:	88 4c 04 47          	mov    %cl,0x47(%rsp,%rax,1)
  406c55:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406c5a:	48 8d 4c 14 47       	lea    0x47(%rsp,%rdx,1),%rcx
  406c5f:	b8 81 00 00 00       	mov    $0x81,%eax
  406c64:	48 29 d0             	sub    %rdx,%rax
  406c67:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  406c6c:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406c71:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406c76:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406c7b:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  406c82:	00 00 
  406c84:	48 8d 54 24 20       	lea    0x20(%rsp),%rdx
  406c89:	e8 42 fd ff ff       	call   4069d0 <runtime::stderr_write>
  406c8e:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  406c95:	c3                   	ret
  406c96:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  406c9d:	00 00 00 

0000000000406ca0 <runtime::print_i64>:
  406ca0:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  406ca7:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406cac:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  406cb1:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  406cb8:	00 
  406cb9:	31 c9                	xor    %ecx,%ecx
  406cbb:	89 ca                	mov    %ecx,%edx
  406cbd:	48 29 c2             	sub    %rax,%rdx
  406cc0:	48 83 f8 00          	cmp    $0x0,%rax
  406cc4:	48 89 c1             	mov    %rax,%rcx
  406cc7:	48 0f 4c ca          	cmovl  %rdx,%rcx
  406ccb:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  406cd2:	00 
  406cd3:	48 83 f8 00          	cmp    $0x0,%rax
  406cd7:	0f 9c c0             	setl   %al
  406cda:	24 01                	and    $0x1,%al
  406cdc:	88 84 24 c7 00 00 00 	mov    %al,0xc7(%rsp)
  406ce3:	48 8d 7c 24 46       	lea    0x46(%rsp),%rdi
  406ce8:	31 f6                	xor    %esi,%esi
  406cea:	ba 81 00 00 00       	mov    $0x81,%edx
  406cef:	e8 4c a3 ff ff       	call   401040 <memset@plt>
  406cf4:	48 c7 44 24 38 81 00 	movq   $0x81,0x38(%rsp)
  406cfb:	00 00 
  406cfd:	48 83 bc 24 c8 00 00 	cmpq   $0xa,0xc8(%rsp)
  406d04:	00 0a 
  406d06:	0f 93 c0             	setae  %al
  406d09:	24 01                	and    $0x1,%al
  406d0b:	3c 00                	cmp    $0x0,%al
  406d0d:	74 5c                	je     406d6b <runtime::print_i64+0xcb>
  406d0f:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406d14:	48 83 e8 01          	sub    $0x1,%rax
  406d18:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406d1d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406d22:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406d27:	48 c7 c0 48 30 41 00 	mov    $0x413048,%rax
  406d2e:	48 8b 08             	mov    (%rax),%rcx
  406d31:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406d38:	00 
  406d39:	be 0a 00 00 00       	mov    $0xa,%esi
  406d3e:	31 d2                	xor    %edx,%edx
  406d40:	48 f7 f6             	div    %rsi
  406d43:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406d48:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  406d4b:	88 4c 04 46          	mov    %cl,0x46(%rsp,%rax,1)
  406d4f:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406d56:	00 
  406d57:	b9 0a 00 00 00       	mov    $0xa,%ecx
  406d5c:	31 d2                	xor    %edx,%edx
  406d5e:	48 f7 f1             	div    %rcx
  406d61:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  406d68:	00 
  406d69:	eb 92                	jmp    406cfd <runtime::print_i64+0x5d>
  406d6b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406d70:	48 83 e8 01          	sub    $0x1,%rax
  406d74:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406d79:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406d7e:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  406d83:	48 c7 c0 48 30 41 00 	mov    $0x413048,%rax
  406d8a:	48 8b 08             	mov    (%rax),%rcx
  406d8d:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406d94:	00 
  406d95:	be 0a 00 00 00       	mov    $0xa,%esi
  406d9a:	31 d2                	xor    %edx,%edx
  406d9c:	48 f7 f6             	div    %rsi
  406d9f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406da4:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  406da7:	88 4c 04 46          	mov    %cl,0x46(%rsp,%rax,1)
  406dab:	80 bc 24 c7 00 00 00 	cmpb   $0x0,0xc7(%rsp)
  406db2:	00 
  406db3:	74 18                	je     406dcd <runtime::print_i64+0x12d>
  406db5:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406dba:	48 83 e8 01          	sub    $0x1,%rax
  406dbe:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406dc3:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406dc8:	c6 44 04 46 2d       	movb   $0x2d,0x46(%rsp,%rax,1)
  406dcd:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406dd2:	48 8d 4c 14 46       	lea    0x46(%rsp,%rdx,1),%rcx
  406dd7:	b8 81 00 00 00       	mov    $0x81,%eax
  406ddc:	48 29 d0             	sub    %rdx,%rax
  406ddf:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  406de4:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406de9:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406dee:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406df3:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  406dfa:	00 00 
  406dfc:	48 8d 54 24 20       	lea    0x20(%rsp),%rdx
  406e01:	e8 ca fb ff ff       	call   4069d0 <runtime::stderr_write>
  406e06:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  406e0d:	c3                   	ret
  406e0e:	66 90                	xchg   %ax,%ax

0000000000406e10 <runtime::print_caller_location>:
  406e10:	50                   	push   %rax
  406e11:	48 89 3c 24          	mov    %rdi,(%rsp)
  406e15:	eb 00                	jmp    406e17 <runtime::print_caller_location+0x7>
  406e17:	48 8b 04 24          	mov    (%rsp),%rax
  406e1b:	48 8b 38             	mov    (%rax),%rdi
  406e1e:	48 8b 70 08          	mov    0x8(%rax),%rsi
  406e22:	e8 29 fc ff ff       	call   406a50 <runtime::print_string>
  406e27:	bf 28 00 00 00       	mov    $0x28,%edi
  406e2c:	e8 8f fc ff ff       	call   406ac0 <runtime::print_byte>
  406e31:	48 8b 04 24          	mov    (%rsp),%rax
  406e35:	48 63 78 10          	movslq 0x10(%rax),%rdi
  406e39:	e8 12 fd ff ff       	call   406b50 <runtime::print_u64>
  406e3e:	48 8b 04 24          	mov    (%rsp),%rax
  406e42:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  406e46:	0f 95 c0             	setne  %al
  406e49:	24 01                	and    $0x1,%al
  406e4b:	3c 00                	cmp    $0x0,%al
  406e4d:	74 17                	je     406e66 <runtime::print_caller_location+0x56>
  406e4f:	bf 3a 00 00 00       	mov    $0x3a,%edi
  406e54:	e8 67 fc ff ff       	call   406ac0 <runtime::print_byte>
  406e59:	48 8b 04 24          	mov    (%rsp),%rax
  406e5d:	48 63 78 14          	movslq 0x14(%rax),%rdi
  406e61:	e8 ea fc ff ff       	call   406b50 <runtime::print_u64>
  406e66:	bf 29 00 00 00       	mov    $0x29,%edi
  406e6b:	e8 50 fc ff ff       	call   406ac0 <runtime::print_byte>
  406e70:	58                   	pop    %rax
  406e71:	c3                   	ret
  406e72:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  406e79:	00 00 00 
  406e7c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000406e80 <io::query_utility>:
  406e80:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  406e85:	48 89 74 24 e0       	mov    %rsi,-0x20(%rsp)
  406e8a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  406e8f:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406e94:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  406e99:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  406ea0:	00 00 
  406ea2:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  406ea9:	00 
  406eaa:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  406eaf:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  406eb6:	00 
  406eb7:	48 89 08             	mov    %rcx,(%rax)
  406eba:	31 c0                	xor    %eax,%eax
  406ebc:	c3                   	ret
  406ebd:	0f 1f 00             	nopl   (%rax)

0000000000406ec0 <runtime::[os_specific_linux.odin]::_stderr_write>:
  406ec0:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  406ec5:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  406eca:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  406ecf:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  406ed4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  406ed9:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  406ede:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  406ee3:	b8 01 00 00 00       	mov    $0x1,%eax
  406ee8:	bf 02 00 00 00       	mov    $0x2,%edi
  406eed:	0f 05                	syscall
  406eef:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406ef4:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  406efa:	0f 9c c0             	setl   %al
  406efd:	24 01                	and    $0x1,%al
  406eff:	3c 00                	cmp    $0x0,%al
  406f01:	74 26                	je     406f29 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  406f03:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  406f0a:	ff ff 
  406f0c:	0f 9f c0             	setg   %al
  406f0f:	24 01                	and    $0x1,%al
  406f11:	3c 00                	cmp    $0x0,%al
  406f13:	74 14                	je     406f29 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  406f15:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  406f1a:	31 c0                	xor    %eax,%eax
  406f1c:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  406f21:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  406f28:	c3                   	ret
  406f29:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406f2e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406f33:	48 89 08             	mov    %rcx,(%rax)
  406f36:	31 c0                	xor    %eax,%eax
  406f38:	c3                   	ret
  406f39:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000406f40 <runtime::[os_specific_linux.odin]::_rand_bytes>:
  406f40:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
  406f47:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  406f4c:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  406f51:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406f56:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  406f5b:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  406f62:	00 
  406f63:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  406f6a:	00 
  406f6b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  406f72:	00 
  406f73:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  406f78:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  406f7f:	00 
  406f80:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  406f85:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  406f8b:	0f 9f c0             	setg   %al
  406f8e:	24 01                	and    $0x1,%al
  406f90:	3c 00                	cmp    $0x0,%al
  406f92:	0f 84 55 01 00 00    	je     4070ed <runtime::[os_specific_linux.odin]::_rand_bytes+0x1ad>
  406f98:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  406f9d:	48 89 c8             	mov    %rcx,%rax
  406fa0:	48 2d ff ff ff 01    	sub    $0x1ffffff,%rax
  406fa6:	b8 ff ff ff 01       	mov    $0x1ffffff,%eax
  406fab:	48 0f 4c c1          	cmovl  %rcx,%rax
  406faf:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  406fb4:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  406fb9:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  406fbe:	4c 8b 8c 24 80 00 00 	mov    0x80(%rsp),%r9
  406fc5:	00 
  406fc6:	bf c0 e4 40 00       	mov    $0x40e4c0,%edi
  406fcb:	be 31 00 00 00       	mov    $0x31,%esi
  406fd0:	ba 37 00 00 00       	mov    $0x37,%edx
  406fd5:	b9 44 00 00 00       	mov    $0x44,%ecx
  406fda:	e8 a1 39 00 00       	call   40a980 <runtime::slice_expr_error_hi>
  406fdf:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406fe4:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  406fe9:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  406fee:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  406ff3:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  406ff8:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  406ffd:	31 c0                	xor    %eax,%eax
  406fff:	89 c2                	mov    %eax,%edx
  407001:	b8 3e 01 00 00       	mov    $0x13e,%eax
  407006:	0f 05                	syscall
  407008:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40700d:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  407012:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  407017:	48 83 e8 da          	sub    $0xffffffffffffffda,%rax
  40701b:	74 14                	je     407031 <runtime::[os_specific_linux.odin]::_rand_bytes+0xf1>
  40701d:	eb 00                	jmp    40701f <runtime::[os_specific_linux.odin]::_rand_bytes+0xdf>
  40701f:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  407024:	48 83 e8 fc          	sub    $0xfffffffffffffffc,%rax
  407028:	75 1b                	jne    407045 <runtime::[os_specific_linux.odin]::_rand_bytes+0x105>
  40702a:	eb 00                	jmp    40702c <runtime::[os_specific_linux.odin]::_rand_bytes+0xec>
  40702c:	e9 54 ff ff ff       	jmp    406f85 <runtime::[os_specific_linux.odin]::_rand_bytes+0x45>
  407031:	bf f2 e4 40 00       	mov    $0x40e4f2,%edi
  407036:	ba 30 e5 40 00       	mov    $0x40e530,%edx
  40703b:	be 2f 00 00 00       	mov    $0x2f,%esi
  407040:	e8 2b 52 00 00       	call   40c270 <runtime::panic_contextless>
  407045:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  40704b:	0f 9c c0             	setl   %al
  40704e:	24 01                	and    $0x1,%al
  407050:	3c 00                	cmp    $0x0,%al
  407052:	74 14                	je     407068 <runtime::[os_specific_linux.odin]::_rand_bytes+0x128>
  407054:	bf 58 e5 40 00       	mov    $0x40e558,%edi
  407059:	ba 80 e5 40 00       	mov    $0x40e580,%edx
  40705e:	be 1e 00 00 00       	mov    $0x1e,%esi
  407063:	e8 08 52 00 00       	call   40c270 <runtime::panic_contextless>
  407068:	eb 00                	jmp    40706a <runtime::[os_specific_linux.odin]::_rand_bytes+0x12a>
  40706a:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40706f:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  407074:	48 29 c8             	sub    %rcx,%rax
  407077:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40707c:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  407081:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  407086:	4c 8b 8c 24 80 00 00 	mov    0x80(%rsp),%r9
  40708d:	00 
  40708e:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  407093:	48 89 e0             	mov    %rsp,%rax
  407096:	4c 89 08             	mov    %r9,(%rax)
  407099:	bf c0 e4 40 00       	mov    $0x40e4c0,%edi
  40709e:	be 31 00 00 00       	mov    $0x31,%esi
  4070a3:	ba 4a 00 00 00       	mov    $0x4a,%edx
  4070a8:	b9 0c 00 00 00       	mov    $0xc,%ecx
  4070ad:	e8 7e 39 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  4070b2:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4070b7:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4070bc:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  4070c1:	48 01 d1             	add    %rdx,%rcx
  4070c4:	48 29 d0             	sub    %rdx,%rax
  4070c7:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4070cc:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4070d1:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4070d6:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4070db:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  4070e2:	00 
  4070e3:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  4070e8:	e9 98 fe ff ff       	jmp    406f85 <runtime::[os_specific_linux.odin]::_rand_bytes+0x45>
  4070ed:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  4070f4:	c3                   	ret
  4070f5:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4070fc:	00 00 00 
  4070ff:	90                   	nop

0000000000407100 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  407100:	50                   	push   %rax
  407101:	eb 00                	jmp    407103 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x3>
  407103:	48 c7 c0 58 ff ff ff 	mov    $0xffffffffffffff58,%rax
  40710a:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  407111:	00 00 
  407113:	48 01 c7             	add    %rax,%rdi
  407116:	e8 05 00 00 00       	call   407120 <runtime::default_temp_allocator_destroy>
  40711b:	58                   	pop    %rax
  40711c:	c3                   	ret
  40711d:	0f 1f 00             	nopl   (%rax)

0000000000407120 <runtime::default_temp_allocator_destroy>:
  407120:	48 83 ec 18          	sub    $0x18,%rsp
  407124:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  407129:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40712e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  407133:	48 83 f8 00          	cmp    $0x0,%rax
  407137:	0f 95 c0             	setne  %al
  40713a:	24 01                	and    $0x1,%al
  40713c:	3c 00                	cmp    $0x0,%al
  40713e:	74 25                	je     407165 <runtime::default_temp_allocator_destroy+0x45>
  407140:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  407145:	48 be 10 e6 40 00 00 	movabs $0x40e610,%rsi
  40714c:	00 00 00 
  40714f:	e8 9c ad ff ff       	call   401ef0 <runtime::arena_destroy>
  407154:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  407159:	31 f6                	xor    %esi,%esi
  40715b:	ba 38 00 00 00       	mov    $0x38,%edx
  407160:	e8 db 9e ff ff       	call   401040 <memset@plt>
  407165:	48 83 c4 18          	add    $0x18,%rsp
  407169:	c3                   	ret
  40716a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000407170 <runtime::default_temp_allocator_proc>:
  407170:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  407177:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40717c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  407181:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  407186:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40718b:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  407190:	40 88 f0             	mov    %sil,%al
  407193:	88 44 24 47          	mov    %al,0x47(%rsp)
  407197:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40719e:	00 
  40719f:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4071a4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4071ab:	00 
  4071ac:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4071b1:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  4071b8:	00 
  4071b9:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4071be:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  4071c3:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  4071c8:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4071cd:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4071d2:	8a 44 24 47          	mov    0x47(%rsp),%al
  4071d6:	4c 8b 54 24 58       	mov    0x58(%rsp),%r10
  4071db:	4c 8b 5c 24 48       	mov    0x48(%rsp),%r11
  4071e0:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4071e5:	48 89 b4 24 c0 00 00 	mov    %rsi,0xc0(%rsp)
  4071ec:	00 
  4071ed:	88 84 24 bf 00 00 00 	mov    %al,0xbf(%rsp)
  4071f4:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  4071fb:	00 
  4071fc:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  407203:	00 
  407204:	4c 89 84 24 a0 00 00 	mov    %r8,0xa0(%rsp)
  40720b:	00 
  40720c:	4c 89 8c 24 98 00 00 	mov    %r9,0x98(%rsp)
  407213:	00 
  407214:	0f 57 c0             	xorps  %xmm0,%xmm0
  407217:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40721e:	00 
  40721f:	c6 44 24 7f 00       	movb   $0x0,0x7f(%rsp)
  407224:	48 89 74 24 70       	mov    %rsi,0x70(%rsp)
  407229:	48 8b 7c 24 70       	mov    0x70(%rsp),%rdi
  40722e:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  407233:	48 89 e6             	mov    %rsp,%rsi
  407236:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  40723a:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  40723f:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  407243:	4c 89 16             	mov    %r10,(%rsi)
  407246:	0f b6 f0             	movzbl %al,%esi
  407249:	e8 42 ad ff ff       	call   401f90 <runtime::arena_allocator_proc>
  40724e:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  407253:	40 88 c7             	mov    %al,%dil
  407256:	40 88 f8             	mov    %dil,%al
  407259:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40725e:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  407263:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  40726a:	00 
  40726b:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  407272:	00 
  407273:	40 88 7c 24 7f       	mov    %dil,0x7f(%rsp)
  407278:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40727c:	48 89 11             	mov    %rdx,(%rcx)
  40727f:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  407286:	c3                   	ret
  407287:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40728e:	00 00 

0000000000407290 <main>:
  407290:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  407297:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  40729b:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4072a0:	8b 44 24 14          	mov    0x14(%rsp),%eax
  4072a4:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4072a9:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  4072b0:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  4072b7:	00 
  4072b8:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  4072bf:	00 
  4072c0:	48 89 0c 24          	mov    %rcx,(%rsp)
  4072c4:	4c 63 c8             	movslq %eax,%r9
  4072c7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4072cc:	bf 38 e6 40 00       	mov    $0x40e638,%edi
  4072d1:	31 c0                	xor    %eax,%eax
  4072d3:	41 89 c0             	mov    %eax,%r8d
  4072d6:	be 2a 00 00 00       	mov    $0x2a,%esi
  4072db:	ba 36 00 00 00       	mov    $0x36,%edx
  4072e0:	b9 11 00 00 00       	mov    $0x11,%ecx
  4072e5:	e8 06 36 00 00       	call   40a8f0 <runtime::multi_pointer_slice_expr_error>
  4072ea:	48 8b 0c 24          	mov    (%rsp),%rcx
  4072ee:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4072f3:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  4072fa:	00 
  4072fb:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  407302:	00 
  407303:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  40730a:	00 
  40730b:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  407312:	00 
  407313:	48 c7 c0 a8 31 41 00 	mov    $0x4131a8,%rax
  40731a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40731e:	48 89 08             	mov    %rcx,(%rax)
  407321:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  407328:	00 
  407329:	31 f6                	xor    %esi,%esi
  40732b:	ba 70 00 00 00       	mov    $0x70,%edx
  407330:	e8 0b 9d ff ff       	call   401040 <memset@plt>
  407335:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40733c:	00 
  40733d:	e8 7e 3d 00 00       	call   40b0c0 <runtime::[core.odin]::__init_context>
  407342:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  407347:	31 f6                	xor    %esi,%esi
  407349:	ba 70 00 00 00       	mov    $0x70,%edx
  40734e:	e8 ed 9c ff ff       	call   401040 <memset@plt>
  407353:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  407358:	e8 13 3d 00 00       	call   40b070 <runtime::default_context>
  40735d:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  407362:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  407367:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  40736c:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  407371:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  407376:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  40737b:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  407382:	00 
  407383:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  40738a:	00 
  40738b:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  407392:	00 
  407393:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  40739a:	00 
  40739b:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  4073a2:	00 
  4073a3:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  4073aa:	00 
  4073ab:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  4073b2:	00 
  4073b3:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4073ba:	00 
  4073bb:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4073c2:	00 
  4073c3:	e8 a8 3e 00 00       	call   40b270 <__$startup_runtime>
  4073c8:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4073cf:	00 
  4073d0:	e8 1b 00 00 00       	call   4073f0 <journey::main>
  4073d5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4073dc:	00 
  4073dd:	e8 ee 3e 00 00       	call   40b2d0 <__$cleanup_runtime>
  4073e2:	31 c0                	xor    %eax,%eax
  4073e4:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  4073eb:	c3                   	ret
  4073ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004073f0 <journey::main>:
  4073f0:	55                   	push   %rbp
  4073f1:	48 89 e5             	mov    %rsp,%rbp
  4073f4:	48 83 e4 c0          	and    $0xffffffffffffffc0,%rsp
  4073f8:	48 81 ec 00 02 00 00 	sub    $0x200,%rsp
  4073ff:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  407404:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  407409:	48 8d bc 24 80 00 00 	lea    0x80(%rsp),%rdi
  407410:	00 
  407411:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  407416:	e8 b5 01 00 00       	call   4075d0 <journey::init_world:proc(world:^journey::World(THREAD_COUNT:$$4),unique_data_capacity:$$28)>
  40741b:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  407420:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  407425:	e8 90 02 00 00       	call   4076ba <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::Position,table_index:$$1,start:$$21,end:$$300)>
  40742a:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40742f:	0f 57 c0             	xorps  %xmm0,%xmm0
  407432:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  407437:	c6 44 24 7f 00       	movb   $0x0,0x7f(%rsp)
  40743c:	c6 44 24 7e 00       	movb   $0x0,0x7e(%rsp)
  407441:	c6 44 24 7d 00       	movb   $0x0,0x7d(%rsp)
  407446:	c6 44 24 7c 00       	movb   $0x0,0x7c(%rsp)
  40744b:	c6 44 24 7b 00       	movb   $0x0,0x7b(%rsp)
  407450:	c6 44 24 7a 00       	movb   $0x0,0x7a(%rsp)
  407455:	66 c7 44 24 78 00 00 	movw   $0x0,0x78(%rsp)
  40745c:	48 c7 44 24 70 01 00 	movq   $0x1,0x70(%rsp)
  407463:	00 00 
  407465:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40746a:	66 8b 4c 24 78       	mov    0x78(%rsp),%cx
  40746f:	8a 54 24 7a          	mov    0x7a(%rsp),%dl
  407473:	40 8a 74 24 7b       	mov    0x7b(%rsp),%sil
  407478:	44 8a 44 24 7c       	mov    0x7c(%rsp),%r8b
  40747d:	44 8a 4c 24 7d       	mov    0x7d(%rsp),%r9b
  407482:	44 8a 54 24 7e       	mov    0x7e(%rsp),%r10b
  407487:	44 8a 5c 24 7f       	mov    0x7f(%rsp),%r11b
  40748c:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  407491:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  407496:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40749b:	44 88 5c 24 4f       	mov    %r11b,0x4f(%rsp)
  4074a0:	44 88 54 24 4e       	mov    %r10b,0x4e(%rsp)
  4074a5:	44 88 4c 24 4d       	mov    %r9b,0x4d(%rsp)
  4074aa:	44 88 44 24 4c       	mov    %r8b,0x4c(%rsp)
  4074af:	40 88 74 24 4b       	mov    %sil,0x4b(%rsp)
  4074b4:	88 54 24 4a          	mov    %dl,0x4a(%rsp)
  4074b8:	66 89 4c 24 48       	mov    %cx,0x48(%rsp)
  4074bd:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4074c2:	44 88 5c 24 5f       	mov    %r11b,0x5f(%rsp)
  4074c7:	44 88 54 24 5e       	mov    %r10b,0x5e(%rsp)
  4074cc:	44 88 4c 24 5d       	mov    %r9b,0x5d(%rsp)
  4074d1:	44 88 44 24 5c       	mov    %r8b,0x5c(%rsp)
  4074d6:	40 88 74 24 5b       	mov    %sil,0x5b(%rsp)
  4074db:	88 54 24 5a          	mov    %dl,0x5a(%rsp)
  4074df:	66 89 4c 24 58       	mov    %cx,0x58(%rsp)
  4074e4:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4074e9:	48 8d 44 24 40       	lea    0x40(%rsp),%rax
  4074ee:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4074f3:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4074f8:	48 c7 44 24 68 02 00 	movq   $0x2,0x68(%rsp)
  4074ff:	00 00 
  407501:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  407506:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  40750b:	48 c7 c2 e0 77 40 00 	mov    $0x4077e0,%rdx
  407512:	be 01 00 00 00       	mov    $0x1,%esi
  407517:	e8 2f 02 00 00       	call   40774b <journey::run_0:proc"contextless"(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>
  40751c:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  407521:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  407526:	66 8b 54 24 78       	mov    0x78(%rsp),%dx
  40752b:	40 8a 74 24 7a       	mov    0x7a(%rsp),%sil
  407530:	40 8a 7c 24 7b       	mov    0x7b(%rsp),%dil
  407535:	44 8a 44 24 7c       	mov    0x7c(%rsp),%r8b
  40753a:	44 8a 4c 24 7d       	mov    0x7d(%rsp),%r9b
  40753f:	44 8a 54 24 7e       	mov    0x7e(%rsp),%r10b
  407544:	44 8a 5c 24 7f       	mov    0x7f(%rsp),%r11b
  407549:	44 88 5c 24 4f       	mov    %r11b,0x4f(%rsp)
  40754e:	44 88 54 24 4e       	mov    %r10b,0x4e(%rsp)
  407553:	44 88 4c 24 4d       	mov    %r9b,0x4d(%rsp)
  407558:	44 88 44 24 4c       	mov    %r8b,0x4c(%rsp)
  40755d:	40 88 7c 24 4b       	mov    %dil,0x4b(%rsp)
  407562:	40 88 74 24 4a       	mov    %sil,0x4a(%rsp)
  407567:	66 89 54 24 48       	mov    %dx,0x48(%rsp)
  40756c:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  407571:	44 88 5c 24 5f       	mov    %r11b,0x5f(%rsp)
  407576:	44 88 54 24 5e       	mov    %r10b,0x5e(%rsp)
  40757b:	44 88 4c 24 5d       	mov    %r9b,0x5d(%rsp)
  407580:	44 88 44 24 5c       	mov    %r8b,0x5c(%rsp)
  407585:	40 88 7c 24 5b       	mov    %dil,0x5b(%rsp)
  40758a:	40 88 74 24 5a       	mov    %sil,0x5a(%rsp)
  40758f:	66 89 54 24 58       	mov    %dx,0x58(%rsp)
  407594:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  407599:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40759e:	48 c7 44 24 68 02 00 	movq   $0x2,0x68(%rsp)
  4075a5:	00 00 
  4075a7:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4075ac:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  4075b1:	31 c0                	xor    %eax,%eax
  4075b3:	48 c7 c2 80 78 40 00 	mov    $0x407880,%rdx
  4075ba:	31 c0                	xor    %eax,%eax
  4075bc:	89 c6                	mov    %eax,%esi
  4075be:	48 8d bc 24 80 00 00 	lea    0x80(%rsp),%rdi
  4075c5:	00 
  4075c6:	e8 80 01 00 00       	call   40774b <journey::run_0:proc"contextless"(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>
  4075cb:	48 89 ec             	mov    %rbp,%rsp
  4075ce:	5d                   	pop    %rbp
  4075cf:	c3                   	ret

00000000004075d0 <journey::init_world:proc(world:^journey::World(THREAD_COUNT:$$4),unique_data_capacity:$$28)>:
  4075d0:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  4075d5:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4075da:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4075df:	31 c0                	xor    %eax,%eax
  4075e1:	41 89 c1             	mov    %eax,%r9d
  4075e4:	b8 09 00 00 00       	mov    $0x9,%eax
  4075e9:	be 00 e0 01 00       	mov    $0x1e000,%esi
  4075ee:	ba 03 00 00 00       	mov    $0x3,%edx
  4075f3:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4075f9:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  407600:	4c 89 cf             	mov    %r9,%rdi
  407603:	0f 05                	syscall
  407605:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40760a:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40760f:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  407614:	48 89 08             	mov    %rcx,(%rax)
  407617:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40761c:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  407621:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  407628:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40762c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  407631:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  407636:	48 81 c1 00 20 00 00 	add    $0x2000,%rcx
  40763d:	48 89 48 10          	mov    %rcx,0x10(%rax)
  407641:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  407646:	48 c7 40 18 1c 00 00 	movq   $0x1c,0x18(%rax)
  40764d:	00 
  40764e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  407653:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407658:	b8 ba 00 00 00       	mov    $0xba,%eax
  40765d:	0f 05                	syscall
  40765f:	48 89 c1             	mov    %rax,%rcx
  407662:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  407667:	48 89 48 20          	mov    %rcx,0x20(%rax)
  40766b:	31 c0                	xor    %eax,%eax
  40766d:	41 89 c1             	mov    %eax,%r9d
  407670:	b8 09 00 00 00       	mov    $0x9,%eax
  407675:	be 00 10 02 00       	mov    $0x21000,%esi
  40767a:	ba 03 00 00 00       	mov    $0x3,%edx
  40767f:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  407685:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  40768c:	4c 89 cf             	mov    %r9,%rdi
  40768f:	0f 05                	syscall
  407691:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407696:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40769b:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4076a0:	48 89 48 28          	mov    %rcx,0x28(%rax)
  4076a4:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4076a9:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4076ae:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  4076b5:	48 89 48 30          	mov    %rcx,0x30(%rax)
  4076b9:	c3                   	ret

00000000004076ba <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::Position,table_index:$$1,start:$$21,end:$$300)>:
  4076ba:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  4076bf:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4076c4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4076c9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4076ce:	48 8b 00             	mov    (%rax),%rax
  4076d1:	c7 40 1c 11 33 00 00 	movl   $0x3311,0x1c(%rax)
  4076d8:	c7 40 18 35 01 00 00 	movl   $0x135,0x18(%rax)
  4076df:	c7 40 14 15 00 00 00 	movl   $0x15,0x14(%rax)
  4076e6:	c7 40 10 08 00 00 00 	movl   $0x8,0x10(%rax)
  4076ed:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4076f2:	48 8b 40 08          	mov    0x8(%rax),%rax
  4076f6:	c6 40 03 ef          	movb   $0xef,0x3(%rax)
  4076fa:	c6 40 02 0f          	movb   $0xf,0x2(%rax)
  4076fe:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  407703:	48 8b 40 10          	mov    0x10(%rax),%rax
  407707:	48 05 00 10 00 00    	add    $0x1000,%rax
  40770d:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407712:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407717:	48 83 e0 20          	and    $0x20,%rax
  40771b:	48 83 f8 00          	cmp    $0x0,%rax
  40771f:	75 1c                	jne    40773d <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::Position,table_index:$$1,start:$$21,end:$$300)+0x83>
  407721:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407726:	48 c7 00 ff ff ff ff 	movq   $0xffffffffffffffff,(%rax)
  40772d:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407732:	48 83 c0 08          	add    $0x8,%rax
  407736:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40773b:	eb d5                	jmp    407712 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::Position,table_index:$$1,start:$$21,end:$$300)+0x58>
  40773d:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407742:	b9 ff ff ff ff       	mov    $0xffffffff,%ecx
  407747:	48 89 08             	mov    %rcx,(%rax)
  40774a:	c3                   	ret

000000000040774b <journey::run_0:proc"contextless"(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>:
  40774b:	48 83 ec 78          	sub    $0x78,%rsp
  40774f:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  407754:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407759:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40775e:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  407763:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  407768:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40776d:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  407772:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  407777:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40777c:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  407781:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  407786:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  40778b:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  407790:	48 89 54 24 58       	mov    %rdx,0x58(%rsp)
  407795:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  40779a:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40779f:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  4077a3:	48 81 c1 40 10 00 00 	add    $0x1040,%rcx
  4077aa:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4077af:	48 c7 44 24 48 c0 0f 	movq   $0xfc0,0x48(%rsp)
  4077b6:	00 00 
  4077b8:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4077bd:	31 c9                	xor    %ecx,%ecx
  4077bf:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4077c4:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4077c9:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4077ce:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4077d3:	ff d0                	call   *%rax
  4077d5:	48 83 c4 78          	add    $0x78,%rsp
  4077d9:	c3                   	ret
  4077da:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004077e0 <journey::main.set_data-0>:
  4077e0:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  4077e5:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  4077ea:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  4077ef:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4077f4:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4077f9:	48 8b 54 24 b8       	mov    -0x48(%rsp),%rdx
  4077fe:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  407803:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  407808:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40780d:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407812:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  407819:	00 00 
  40781b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  407822:	00 00 
  407824:	48 83 7c 24 d8 01    	cmpq   $0x1,-0x28(%rsp)
  40782a:	7d 46                	jge    407872 <journey::main.set_data-0+0x92>
  40782c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407831:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  407836:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40783b:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  407840:	48 c1 e1 06          	shl    $0x6,%rcx
  407844:	0f 28 05 25 6e 00 00 	movaps 0x6e25(%rip),%xmm0        # 40e670 <_IO_stdin_used+0x670>
  40784b:	0f 29 44 08 10       	movaps %xmm0,0x10(%rax,%rcx,1)
  407850:	0f 29 04 08          	movaps %xmm0,(%rax,%rcx,1)
  407854:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407859:	48 83 c0 01          	add    $0x1,%rax
  40785d:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  407862:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  407867:	48 83 c0 01          	add    $0x1,%rax
  40786b:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  407870:	eb b2                	jmp    407824 <journey::main.set_data-0+0x44>
  407872:	c3                   	ret
  407873:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40787a:	84 00 00 00 00 00 

0000000000407880 <journey::main.print_data-1>:
  407880:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  407885:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  40788a:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40788f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  407894:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  407899:	48 8b 54 24 b8       	mov    -0x48(%rsp),%rdx
  40789e:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4078a3:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  4078a8:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4078ad:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4078b2:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4078b9:	00 00 
  4078bb:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  4078c2:	00 00 
  4078c4:	48 83 7c 24 d8 01    	cmpq   $0x1,-0x28(%rsp)
  4078ca:	7d 28                	jge    4078f4 <journey::main.print_data-1+0x74>
  4078cc:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4078d1:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4078d6:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4078db:	48 83 c0 01          	add    $0x1,%rax
  4078df:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4078e4:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4078e9:	48 83 c0 01          	add    $0x1,%rax
  4078ed:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4078f2:	eb d0                	jmp    4078c4 <journey::main.print_data-1+0x44>
  4078f4:	c3                   	ret
  4078f5:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4078fc:	00 00 00 
  4078ff:	90                   	nop

0000000000407900 <runtime::memset>:
  407900:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  407905:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  407909:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  40790e:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  407913:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  407918:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  40791c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  407921:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  407925:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40792a:	48 83 f8 00          	cmp    $0x0,%rax
  40792e:	0f 95 c0             	setne  %al
  407931:	24 01                	and    $0x1,%al
  407933:	3c 00                	cmp    $0x0,%al
  407935:	74 63                	je     40799a <runtime::memset+0x9a>
  407937:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40793c:	48 83 f8 00          	cmp    $0x0,%rax
  407940:	0f 95 c0             	setne  %al
  407943:	24 01                	and    $0x1,%al
  407945:	3c 00                	cmp    $0x0,%al
  407947:	74 51                	je     40799a <runtime::memset+0x9a>
  407949:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40794e:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  407952:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  407956:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40795b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  407962:	00 00 
  407964:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  407969:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  40796e:	0f 9c c0             	setl   %al
  407971:	24 01                	and    $0x1,%al
  407973:	3c 00                	cmp    $0x0,%al
  407975:	74 21                	je     407998 <runtime::memset+0x98>
  407977:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40797c:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  407981:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  407985:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  407988:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40798d:	48 83 c0 01          	add    $0x1,%rax
  407991:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  407996:	eb cc                	jmp    407964 <runtime::memset+0x64>
  407998:	eb 00                	jmp    40799a <runtime::memset+0x9a>
  40799a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40799f:	c3                   	ret

00000000004079a0 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128>:
  4079a0:	48 81 ec 18 05 00 00 	sub    $0x518,%rsp
  4079a7:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  4079ac:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4079b1:	48 89 84 24 10 05 00 	mov    %rax,0x510(%rsp)
  4079b8:	00 
  4079b9:	48 8b 84 24 10 05 00 	mov    0x510(%rsp),%rax
  4079c0:	00 
  4079c1:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4079c6:	48 89 e0             	mov    %rsp,%rax
  4079c9:	48 c7 00 00 04 00 00 	movq   $0x400,(%rax)
  4079d0:	bf 20 e7 40 00       	mov    $0x40e720,%edi
  4079d5:	be 40 00 00 00       	mov    $0x40,%esi
  4079da:	ba 36 00 00 00       	mov    $0x36,%edx
  4079df:	b9 24 00 00 00       	mov    $0x24,%ecx
  4079e4:	41 b8 e0 03 00 00    	mov    $0x3e0,%r8d
  4079ea:	41 b9 00 04 00 00    	mov    $0x400,%r9d
  4079f0:	e8 3b 30 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  4079f5:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4079fa:	48 05 e0 03 00 00    	add    $0x3e0,%rax
  407a00:	48 89 84 24 00 05 00 	mov    %rax,0x500(%rsp)
  407a07:	00 
  407a08:	48 c7 84 24 08 05 00 	movq   $0x20,0x508(%rsp)
  407a0f:	00 20 00 00 00 
  407a14:	48 8b 84 24 00 05 00 	mov    0x500(%rsp),%rax
  407a1b:	00 
  407a1c:	48 89 84 24 f8 04 00 	mov    %rax,0x4f8(%rsp)
  407a23:	00 
  407a24:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  407a2b:	00 
  407a2c:	8b 00                	mov    (%rax),%eax
  407a2e:	89 84 24 f4 04 00 00 	mov    %eax,0x4f4(%rsp)
  407a35:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  407a3c:	00 
  407a3d:	8b 40 04             	mov    0x4(%rax),%eax
  407a40:	89 84 24 f0 04 00 00 	mov    %eax,0x4f0(%rsp)
  407a47:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  407a4e:	00 
  407a4f:	8b 40 08             	mov    0x8(%rax),%eax
  407a52:	89 84 24 ec 04 00 00 	mov    %eax,0x4ec(%rsp)
  407a59:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  407a60:	00 
  407a61:	8b 40 0c             	mov    0xc(%rax),%eax
  407a64:	89 84 24 e8 04 00 00 	mov    %eax,0x4e8(%rsp)
  407a6b:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  407a72:	00 
  407a73:	8b 40 10             	mov    0x10(%rax),%eax
  407a76:	89 84 24 e4 04 00 00 	mov    %eax,0x4e4(%rsp)
  407a7d:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  407a84:	00 
  407a85:	8b 40 14             	mov    0x14(%rax),%eax
  407a88:	89 84 24 e0 04 00 00 	mov    %eax,0x4e0(%rsp)
  407a8f:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  407a96:	00 
  407a97:	8b 40 18             	mov    0x18(%rax),%eax
  407a9a:	89 84 24 dc 04 00 00 	mov    %eax,0x4dc(%rsp)
  407aa1:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  407aa8:	00 
  407aa9:	8b 40 1c             	mov    0x1c(%rax),%eax
  407aac:	89 84 24 d8 04 00 00 	mov    %eax,0x4d8(%rsp)
  407ab3:	0f 57 c0             	xorps  %xmm0,%xmm0
  407ab6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  407abb:	0f 29 84 24 c0 04 00 	movaps %xmm0,0x4c0(%rsp)
  407ac2:	00 
  407ac3:	66 0f 6e 8c 24 f4 04 	movd   0x4f4(%rsp),%xmm1
  407aca:	00 00 
  407acc:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  407ad1:	0f 29 8c 24 c0 04 00 	movaps %xmm1,0x4c0(%rsp)
  407ad8:	00 
  407ad9:	0f 28 8c 24 c0 04 00 	movaps 0x4c0(%rsp),%xmm1
  407ae0:	00 
  407ae1:	0f 29 8c 24 b0 04 00 	movaps %xmm1,0x4b0(%rsp)
  407ae8:	00 
  407ae9:	0f 29 84 24 a0 04 00 	movaps %xmm0,0x4a0(%rsp)
  407af0:	00 
  407af1:	66 0f 6e 8c 24 f0 04 	movd   0x4f0(%rsp),%xmm1
  407af8:	00 00 
  407afa:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  407aff:	0f 29 8c 24 a0 04 00 	movaps %xmm1,0x4a0(%rsp)
  407b06:	00 
  407b07:	0f 28 8c 24 a0 04 00 	movaps 0x4a0(%rsp),%xmm1
  407b0e:	00 
  407b0f:	0f 29 8c 24 90 04 00 	movaps %xmm1,0x490(%rsp)
  407b16:	00 
  407b17:	0f 29 84 24 80 04 00 	movaps %xmm0,0x480(%rsp)
  407b1e:	00 
  407b1f:	66 0f 6e 8c 24 ec 04 	movd   0x4ec(%rsp),%xmm1
  407b26:	00 00 
  407b28:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  407b2d:	0f 29 8c 24 80 04 00 	movaps %xmm1,0x480(%rsp)
  407b34:	00 
  407b35:	0f 28 8c 24 80 04 00 	movaps 0x480(%rsp),%xmm1
  407b3c:	00 
  407b3d:	0f 29 8c 24 70 04 00 	movaps %xmm1,0x470(%rsp)
  407b44:	00 
  407b45:	0f 29 84 24 60 04 00 	movaps %xmm0,0x460(%rsp)
  407b4c:	00 
  407b4d:	66 0f 6e 8c 24 e8 04 	movd   0x4e8(%rsp),%xmm1
  407b54:	00 00 
  407b56:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  407b5b:	0f 29 8c 24 60 04 00 	movaps %xmm1,0x460(%rsp)
  407b62:	00 
  407b63:	0f 28 8c 24 60 04 00 	movaps 0x460(%rsp),%xmm1
  407b6a:	00 
  407b6b:	0f 29 8c 24 50 04 00 	movaps %xmm1,0x450(%rsp)
  407b72:	00 
  407b73:	0f 29 84 24 40 04 00 	movaps %xmm0,0x440(%rsp)
  407b7a:	00 
  407b7b:	66 0f 6e 8c 24 e4 04 	movd   0x4e4(%rsp),%xmm1
  407b82:	00 00 
  407b84:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  407b89:	0f 29 8c 24 40 04 00 	movaps %xmm1,0x440(%rsp)
  407b90:	00 
  407b91:	0f 28 8c 24 40 04 00 	movaps 0x440(%rsp),%xmm1
  407b98:	00 
  407b99:	0f 29 8c 24 30 04 00 	movaps %xmm1,0x430(%rsp)
  407ba0:	00 
  407ba1:	0f 29 84 24 20 04 00 	movaps %xmm0,0x420(%rsp)
  407ba8:	00 
  407ba9:	66 0f 6e 8c 24 e0 04 	movd   0x4e0(%rsp),%xmm1
  407bb0:	00 00 
  407bb2:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  407bb7:	0f 29 8c 24 20 04 00 	movaps %xmm1,0x420(%rsp)
  407bbe:	00 
  407bbf:	0f 28 8c 24 20 04 00 	movaps 0x420(%rsp),%xmm1
  407bc6:	00 
  407bc7:	0f 29 8c 24 10 04 00 	movaps %xmm1,0x410(%rsp)
  407bce:	00 
  407bcf:	0f 29 84 24 00 04 00 	movaps %xmm0,0x400(%rsp)
  407bd6:	00 
  407bd7:	66 0f 6e 8c 24 dc 04 	movd   0x4dc(%rsp),%xmm1
  407bde:	00 00 
  407be0:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  407be5:	0f 29 8c 24 00 04 00 	movaps %xmm1,0x400(%rsp)
  407bec:	00 
  407bed:	0f 28 8c 24 00 04 00 	movaps 0x400(%rsp),%xmm1
  407bf4:	00 
  407bf5:	0f 29 8c 24 f0 03 00 	movaps %xmm1,0x3f0(%rsp)
  407bfc:	00 
  407bfd:	0f 29 84 24 e0 03 00 	movaps %xmm0,0x3e0(%rsp)
  407c04:	00 
  407c05:	66 0f 6e 8c 24 d8 04 	movd   0x4d8(%rsp),%xmm1
  407c0c:	00 00 
  407c0e:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  407c13:	0f 29 8c 24 e0 03 00 	movaps %xmm1,0x3e0(%rsp)
  407c1a:	00 
  407c1b:	0f 28 8c 24 e0 03 00 	movaps 0x3e0(%rsp),%xmm1
  407c22:	00 
  407c23:	0f 29 8c 24 d0 03 00 	movaps %xmm1,0x3d0(%rsp)
  407c2a:	00 
  407c2b:	0f 28 0d 4e 6a 00 00 	movaps 0x6a4e(%rip),%xmm1        # 40e680 <_IO_stdin_used+0x680>
  407c32:	0f 29 8c 24 c0 03 00 	movaps %xmm1,0x3c0(%rsp)
  407c39:	00 
  407c3a:	0f 29 84 24 b0 03 00 	movaps %xmm0,0x3b0(%rsp)
  407c41:	00 
  407c42:	0f 29 84 24 a0 03 00 	movaps %xmm0,0x3a0(%rsp)
  407c49:	00 
  407c4a:	0f 29 84 24 90 03 00 	movaps %xmm0,0x390(%rsp)
  407c51:	00 
  407c52:	48 8b 84 24 10 05 00 	mov    0x510(%rsp),%rax
  407c59:	00 
  407c5a:	48 89 84 24 80 03 00 	mov    %rax,0x380(%rsp)
  407c61:	00 
  407c62:	48 c7 84 24 88 03 00 	movq   $0x400,0x388(%rsp)
  407c69:	00 00 04 00 00 
  407c6e:	48 8b 84 24 80 03 00 	mov    0x380(%rsp),%rax
  407c75:	00 
  407c76:	48 89 84 24 78 03 00 	mov    %rax,0x378(%rsp)
  407c7d:	00 
  407c7e:	48 c7 c0 10 86 40 00 	mov    $0x408610,%rax
  407c85:	48 89 84 24 70 03 00 	mov    %rax,0x370(%rsp)
  407c8c:	00 
  407c8d:	48 c7 84 24 68 03 00 	movq   $0x0,0x368(%rsp)
  407c94:	00 00 00 00 00 
  407c99:	48 c7 84 24 60 03 00 	movq   $0x0,0x360(%rsp)
  407ca0:	00 00 00 00 00 
  407ca5:	48 83 bc 24 68 03 00 	cmpq   $0x4,0x368(%rsp)
  407cac:	00 04 
  407cae:	0f 8d 54 09 00 00    	jge    408608 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128+0xc68>
  407cb4:	0f 28 05 d5 69 00 00 	movaps 0x69d5(%rip),%xmm0        # 40e690 <_IO_stdin_used+0x690>
  407cbb:	0f 29 84 24 50 03 00 	movaps %xmm0,0x350(%rsp)
  407cc2:	00 
  407cc3:	0f 28 05 d6 69 00 00 	movaps 0x69d6(%rip),%xmm0        # 40e6a0 <_IO_stdin_used+0x6a0>
  407cca:	0f 29 84 24 40 03 00 	movaps %xmm0,0x340(%rsp)
  407cd1:	00 
  407cd2:	0f 28 05 d7 69 00 00 	movaps 0x69d7(%rip),%xmm0        # 40e6b0 <_IO_stdin_used+0x6b0>
  407cd9:	0f 29 84 24 30 03 00 	movaps %xmm0,0x330(%rsp)
  407ce0:	00 
  407ce1:	0f 28 05 d8 69 00 00 	movaps 0x69d8(%rip),%xmm0        # 40e6c0 <_IO_stdin_used+0x6c0>
  407ce8:	0f 29 84 24 20 03 00 	movaps %xmm0,0x320(%rsp)
  407cef:	00 
  407cf0:	66 0f 6f 9c 24 b0 04 	movdqa 0x4b0(%rsp),%xmm3
  407cf7:	00 00 
  407cf9:	66 0f 6f 94 24 90 04 	movdqa 0x490(%rsp),%xmm2
  407d00:	00 00 
  407d02:	66 0f 6f 8c 24 70 04 	movdqa 0x470(%rsp),%xmm1
  407d09:	00 00 
  407d0b:	66 0f 6f 84 24 50 04 	movdqa 0x450(%rsp),%xmm0
  407d12:	00 00 
  407d14:	66 0f 7f 9c 24 10 03 	movdqa %xmm3,0x310(%rsp)
  407d1b:	00 00 
  407d1d:	66 0f 7f 94 24 00 03 	movdqa %xmm2,0x300(%rsp)
  407d24:	00 00 
  407d26:	66 0f 7f 8c 24 f0 02 	movdqa %xmm1,0x2f0(%rsp)
  407d2d:	00 00 
  407d2f:	66 0f 7f 84 24 e0 02 	movdqa %xmm0,0x2e0(%rsp)
  407d36:	00 00 
  407d38:	66 0f 6f 9c 24 30 04 	movdqa 0x430(%rsp),%xmm3
  407d3f:	00 00 
  407d41:	66 0f 6f 94 24 10 04 	movdqa 0x410(%rsp),%xmm2
  407d48:	00 00 
  407d4a:	66 0f 6f 8c 24 f0 03 	movdqa 0x3f0(%rsp),%xmm1
  407d51:	00 00 
  407d53:	66 0f 6f 84 24 d0 03 	movdqa 0x3d0(%rsp),%xmm0
  407d5a:	00 00 
  407d5c:	66 0f 7f 9c 24 d0 02 	movdqa %xmm3,0x2d0(%rsp)
  407d63:	00 00 
  407d65:	66 0f 7f 94 24 c0 02 	movdqa %xmm2,0x2c0(%rsp)
  407d6c:	00 00 
  407d6e:	66 0f 7f 8c 24 b0 02 	movdqa %xmm1,0x2b0(%rsp)
  407d75:	00 00 
  407d77:	66 0f 7f 84 24 a0 02 	movdqa %xmm0,0x2a0(%rsp)
  407d7e:	00 00 
  407d80:	66 0f 6f 9c 24 c0 03 	movdqa 0x3c0(%rsp),%xmm3
  407d87:	00 00 
  407d89:	66 0f 6f 94 24 b0 03 	movdqa 0x3b0(%rsp),%xmm2
  407d90:	00 00 
  407d92:	66 0f 6f 8c 24 a0 03 	movdqa 0x3a0(%rsp),%xmm1
  407d99:	00 00 
  407d9b:	66 0f 6f 84 24 90 03 	movdqa 0x390(%rsp),%xmm0
  407da2:	00 00 
  407da4:	66 0f 7f 9c 24 90 02 	movdqa %xmm3,0x290(%rsp)
  407dab:	00 00 
  407dad:	66 0f 7f 94 24 80 02 	movdqa %xmm2,0x280(%rsp)
  407db4:	00 00 
  407db6:	66 0f 7f 8c 24 70 02 	movdqa %xmm1,0x270(%rsp)
  407dbd:	00 00 
  407dbf:	66 0f 7f 84 24 60 02 	movdqa %xmm0,0x260(%rsp)
  407dc6:	00 00 
  407dc8:	48 c7 84 24 58 02 00 	movq   $0x8,0x258(%rsp)
  407dcf:	00 08 00 00 00 
  407dd4:	48 83 bc 24 58 02 00 	cmpq   $0x0,0x258(%rsp)
  407ddb:	00 00 
  407ddd:	0f 9f c0             	setg   %al
  407de0:	24 01                	and    $0x1,%al
  407de2:	3c 00                	cmp    $0x0,%al
  407de4:	0f 84 db 04 00 00    	je     4082c5 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128+0x925>
  407dea:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  407df1:	00 
  407df2:	0f 28 84 24 50 03 00 	movaps 0x350(%rsp),%xmm0
  407df9:	00 
  407dfa:	0f 28 8c 24 10 03 00 	movaps 0x310(%rsp),%xmm1
  407e01:	00 
  407e02:	0f 28 94 24 d0 02 00 	movaps 0x2d0(%rsp),%xmm2
  407e09:	00 
  407e0a:	0f 28 9c 24 90 02 00 	movaps 0x290(%rsp),%xmm3
  407e11:	00 
  407e12:	0f 57 e4             	xorps  %xmm4,%xmm4
  407e15:	0f 29 64 24 10       	movaps %xmm4,0x10(%rsp)
  407e1a:	0f 29 a4 24 40 02 00 	movaps %xmm4,0x240(%rsp)
  407e21:	00 
  407e22:	0f 29 a4 24 30 02 00 	movaps %xmm4,0x230(%rsp)
  407e29:	00 
  407e2a:	0f 29 a4 24 20 02 00 	movaps %xmm4,0x220(%rsp)
  407e31:	00 
  407e32:	48 8d bc 24 40 02 00 	lea    0x240(%rsp),%rdi
  407e39:	00 
  407e3a:	48 8d b4 24 30 02 00 	lea    0x230(%rsp),%rsi
  407e41:	00 
  407e42:	48 8d 94 24 20 02 00 	lea    0x220(%rsp),%rdx
  407e49:	00 
  407e4a:	ff d0                	call   *%rax
  407e4c:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  407e51:	0f 28 9c 24 40 02 00 	movaps 0x240(%rsp),%xmm3
  407e58:	00 
  407e59:	0f 28 94 24 30 02 00 	movaps 0x230(%rsp),%xmm2
  407e60:	00 
  407e61:	0f 28 8c 24 20 02 00 	movaps 0x220(%rsp),%xmm1
  407e68:	00 
  407e69:	0f 29 9c 24 50 03 00 	movaps %xmm3,0x350(%rsp)
  407e70:	00 
  407e71:	0f 29 94 24 10 03 00 	movaps %xmm2,0x310(%rsp)
  407e78:	00 
  407e79:	0f 29 8c 24 d0 02 00 	movaps %xmm1,0x2d0(%rsp)
  407e80:	00 
  407e81:	0f 29 84 24 90 02 00 	movaps %xmm0,0x290(%rsp)
  407e88:	00 
  407e89:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  407e90:	00 
  407e91:	0f 28 84 24 40 03 00 	movaps 0x340(%rsp),%xmm0
  407e98:	00 
  407e99:	0f 28 8c 24 00 03 00 	movaps 0x300(%rsp),%xmm1
  407ea0:	00 
  407ea1:	0f 28 94 24 c0 02 00 	movaps 0x2c0(%rsp),%xmm2
  407ea8:	00 
  407ea9:	0f 28 9c 24 80 02 00 	movaps 0x280(%rsp),%xmm3
  407eb0:	00 
  407eb1:	0f 29 a4 24 10 02 00 	movaps %xmm4,0x210(%rsp)
  407eb8:	00 
  407eb9:	0f 29 a4 24 00 02 00 	movaps %xmm4,0x200(%rsp)
  407ec0:	00 
  407ec1:	0f 29 a4 24 f0 01 00 	movaps %xmm4,0x1f0(%rsp)
  407ec8:	00 
  407ec9:	48 8d bc 24 10 02 00 	lea    0x210(%rsp),%rdi
  407ed0:	00 
  407ed1:	48 8d b4 24 00 02 00 	lea    0x200(%rsp),%rsi
  407ed8:	00 
  407ed9:	48 8d 94 24 f0 01 00 	lea    0x1f0(%rsp),%rdx
  407ee0:	00 
  407ee1:	ff d0                	call   *%rax
  407ee3:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  407ee8:	0f 28 9c 24 10 02 00 	movaps 0x210(%rsp),%xmm3
  407eef:	00 
  407ef0:	0f 28 94 24 00 02 00 	movaps 0x200(%rsp),%xmm2
  407ef7:	00 
  407ef8:	0f 28 8c 24 f0 01 00 	movaps 0x1f0(%rsp),%xmm1
  407eff:	00 
  407f00:	0f 29 9c 24 40 03 00 	movaps %xmm3,0x340(%rsp)
  407f07:	00 
  407f08:	0f 29 94 24 00 03 00 	movaps %xmm2,0x300(%rsp)
  407f0f:	00 
  407f10:	0f 29 8c 24 c0 02 00 	movaps %xmm1,0x2c0(%rsp)
  407f17:	00 
  407f18:	0f 29 84 24 80 02 00 	movaps %xmm0,0x280(%rsp)
  407f1f:	00 
  407f20:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  407f27:	00 
  407f28:	0f 28 84 24 30 03 00 	movaps 0x330(%rsp),%xmm0
  407f2f:	00 
  407f30:	0f 28 8c 24 f0 02 00 	movaps 0x2f0(%rsp),%xmm1
  407f37:	00 
  407f38:	0f 28 94 24 b0 02 00 	movaps 0x2b0(%rsp),%xmm2
  407f3f:	00 
  407f40:	0f 28 9c 24 70 02 00 	movaps 0x270(%rsp),%xmm3
  407f47:	00 
  407f48:	0f 29 a4 24 e0 01 00 	movaps %xmm4,0x1e0(%rsp)
  407f4f:	00 
  407f50:	0f 29 a4 24 d0 01 00 	movaps %xmm4,0x1d0(%rsp)
  407f57:	00 
  407f58:	0f 29 a4 24 c0 01 00 	movaps %xmm4,0x1c0(%rsp)
  407f5f:	00 
  407f60:	48 8d bc 24 e0 01 00 	lea    0x1e0(%rsp),%rdi
  407f67:	00 
  407f68:	48 8d b4 24 d0 01 00 	lea    0x1d0(%rsp),%rsi
  407f6f:	00 
  407f70:	48 8d 94 24 c0 01 00 	lea    0x1c0(%rsp),%rdx
  407f77:	00 
  407f78:	ff d0                	call   *%rax
  407f7a:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  407f7f:	0f 28 9c 24 e0 01 00 	movaps 0x1e0(%rsp),%xmm3
  407f86:	00 
  407f87:	0f 28 94 24 d0 01 00 	movaps 0x1d0(%rsp),%xmm2
  407f8e:	00 
  407f8f:	0f 28 8c 24 c0 01 00 	movaps 0x1c0(%rsp),%xmm1
  407f96:	00 
  407f97:	0f 29 9c 24 30 03 00 	movaps %xmm3,0x330(%rsp)
  407f9e:	00 
  407f9f:	0f 29 94 24 f0 02 00 	movaps %xmm2,0x2f0(%rsp)
  407fa6:	00 
  407fa7:	0f 29 8c 24 b0 02 00 	movaps %xmm1,0x2b0(%rsp)
  407fae:	00 
  407faf:	0f 29 84 24 70 02 00 	movaps %xmm0,0x270(%rsp)
  407fb6:	00 
  407fb7:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  407fbe:	00 
  407fbf:	0f 28 84 24 20 03 00 	movaps 0x320(%rsp),%xmm0
  407fc6:	00 
  407fc7:	0f 28 8c 24 e0 02 00 	movaps 0x2e0(%rsp),%xmm1
  407fce:	00 
  407fcf:	0f 28 94 24 a0 02 00 	movaps 0x2a0(%rsp),%xmm2
  407fd6:	00 
  407fd7:	0f 28 9c 24 60 02 00 	movaps 0x260(%rsp),%xmm3
  407fde:	00 
  407fdf:	0f 29 a4 24 b0 01 00 	movaps %xmm4,0x1b0(%rsp)
  407fe6:	00 
  407fe7:	0f 29 a4 24 a0 01 00 	movaps %xmm4,0x1a0(%rsp)
  407fee:	00 
  407fef:	0f 29 a4 24 90 01 00 	movaps %xmm4,0x190(%rsp)
  407ff6:	00 
  407ff7:	48 8d bc 24 b0 01 00 	lea    0x1b0(%rsp),%rdi
  407ffe:	00 
  407fff:	48 8d b4 24 a0 01 00 	lea    0x1a0(%rsp),%rsi
  408006:	00 
  408007:	48 8d 94 24 90 01 00 	lea    0x190(%rsp),%rdx
  40800e:	00 
  40800f:	ff d0                	call   *%rax
  408011:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  408016:	0f 28 9c 24 b0 01 00 	movaps 0x1b0(%rsp),%xmm3
  40801d:	00 
  40801e:	0f 28 94 24 a0 01 00 	movaps 0x1a0(%rsp),%xmm2
  408025:	00 
  408026:	0f 28 8c 24 90 01 00 	movaps 0x190(%rsp),%xmm1
  40802d:	00 
  40802e:	0f 29 9c 24 20 03 00 	movaps %xmm3,0x320(%rsp)
  408035:	00 
  408036:	0f 29 94 24 e0 02 00 	movaps %xmm2,0x2e0(%rsp)
  40803d:	00 
  40803e:	0f 29 8c 24 a0 02 00 	movaps %xmm1,0x2a0(%rsp)
  408045:	00 
  408046:	0f 29 84 24 60 02 00 	movaps %xmm0,0x260(%rsp)
  40804d:	00 
  40804e:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  408055:	00 
  408056:	0f 28 84 24 50 03 00 	movaps 0x350(%rsp),%xmm0
  40805d:	00 
  40805e:	0f 28 8c 24 00 03 00 	movaps 0x300(%rsp),%xmm1
  408065:	00 
  408066:	0f 28 94 24 b0 02 00 	movaps 0x2b0(%rsp),%xmm2
  40806d:	00 
  40806e:	0f 28 9c 24 60 02 00 	movaps 0x260(%rsp),%xmm3
  408075:	00 
  408076:	0f 29 a4 24 80 01 00 	movaps %xmm4,0x180(%rsp)
  40807d:	00 
  40807e:	0f 29 a4 24 70 01 00 	movaps %xmm4,0x170(%rsp)
  408085:	00 
  408086:	0f 29 a4 24 60 01 00 	movaps %xmm4,0x160(%rsp)
  40808d:	00 
  40808e:	48 8d bc 24 80 01 00 	lea    0x180(%rsp),%rdi
  408095:	00 
  408096:	48 8d b4 24 70 01 00 	lea    0x170(%rsp),%rsi
  40809d:	00 
  40809e:	48 8d 94 24 60 01 00 	lea    0x160(%rsp),%rdx
  4080a5:	00 
  4080a6:	ff d0                	call   *%rax
  4080a8:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  4080ad:	0f 28 9c 24 80 01 00 	movaps 0x180(%rsp),%xmm3
  4080b4:	00 
  4080b5:	0f 28 94 24 70 01 00 	movaps 0x170(%rsp),%xmm2
  4080bc:	00 
  4080bd:	0f 28 8c 24 60 01 00 	movaps 0x160(%rsp),%xmm1
  4080c4:	00 
  4080c5:	0f 29 9c 24 50 03 00 	movaps %xmm3,0x350(%rsp)
  4080cc:	00 
  4080cd:	0f 29 94 24 00 03 00 	movaps %xmm2,0x300(%rsp)
  4080d4:	00 
  4080d5:	0f 29 8c 24 b0 02 00 	movaps %xmm1,0x2b0(%rsp)
  4080dc:	00 
  4080dd:	0f 29 84 24 60 02 00 	movaps %xmm0,0x260(%rsp)
  4080e4:	00 
  4080e5:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  4080ec:	00 
  4080ed:	0f 28 84 24 40 03 00 	movaps 0x340(%rsp),%xmm0
  4080f4:	00 
  4080f5:	0f 28 8c 24 f0 02 00 	movaps 0x2f0(%rsp),%xmm1
  4080fc:	00 
  4080fd:	0f 28 94 24 a0 02 00 	movaps 0x2a0(%rsp),%xmm2
  408104:	00 
  408105:	0f 28 9c 24 90 02 00 	movaps 0x290(%rsp),%xmm3
  40810c:	00 
  40810d:	0f 29 a4 24 50 01 00 	movaps %xmm4,0x150(%rsp)
  408114:	00 
  408115:	0f 29 a4 24 40 01 00 	movaps %xmm4,0x140(%rsp)
  40811c:	00 
  40811d:	0f 29 a4 24 30 01 00 	movaps %xmm4,0x130(%rsp)
  408124:	00 
  408125:	48 8d bc 24 50 01 00 	lea    0x150(%rsp),%rdi
  40812c:	00 
  40812d:	48 8d b4 24 40 01 00 	lea    0x140(%rsp),%rsi
  408134:	00 
  408135:	48 8d 94 24 30 01 00 	lea    0x130(%rsp),%rdx
  40813c:	00 
  40813d:	ff d0                	call   *%rax
  40813f:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  408144:	0f 28 9c 24 50 01 00 	movaps 0x150(%rsp),%xmm3
  40814b:	00 
  40814c:	0f 28 94 24 40 01 00 	movaps 0x140(%rsp),%xmm2
  408153:	00 
  408154:	0f 28 8c 24 30 01 00 	movaps 0x130(%rsp),%xmm1
  40815b:	00 
  40815c:	0f 29 9c 24 40 03 00 	movaps %xmm3,0x340(%rsp)
  408163:	00 
  408164:	0f 29 94 24 f0 02 00 	movaps %xmm2,0x2f0(%rsp)
  40816b:	00 
  40816c:	0f 29 8c 24 a0 02 00 	movaps %xmm1,0x2a0(%rsp)
  408173:	00 
  408174:	0f 29 84 24 90 02 00 	movaps %xmm0,0x290(%rsp)
  40817b:	00 
  40817c:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  408183:	00 
  408184:	0f 28 84 24 30 03 00 	movaps 0x330(%rsp),%xmm0
  40818b:	00 
  40818c:	0f 28 8c 24 e0 02 00 	movaps 0x2e0(%rsp),%xmm1
  408193:	00 
  408194:	0f 28 94 24 d0 02 00 	movaps 0x2d0(%rsp),%xmm2
  40819b:	00 
  40819c:	0f 28 9c 24 80 02 00 	movaps 0x280(%rsp),%xmm3
  4081a3:	00 
  4081a4:	0f 29 a4 24 20 01 00 	movaps %xmm4,0x120(%rsp)
  4081ab:	00 
  4081ac:	0f 29 a4 24 10 01 00 	movaps %xmm4,0x110(%rsp)
  4081b3:	00 
  4081b4:	0f 29 a4 24 00 01 00 	movaps %xmm4,0x100(%rsp)
  4081bb:	00 
  4081bc:	48 8d bc 24 20 01 00 	lea    0x120(%rsp),%rdi
  4081c3:	00 
  4081c4:	48 8d b4 24 10 01 00 	lea    0x110(%rsp),%rsi
  4081cb:	00 
  4081cc:	48 8d 94 24 00 01 00 	lea    0x100(%rsp),%rdx
  4081d3:	00 
  4081d4:	ff d0                	call   *%rax
  4081d6:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  4081db:	0f 28 9c 24 20 01 00 	movaps 0x120(%rsp),%xmm3
  4081e2:	00 
  4081e3:	0f 28 94 24 10 01 00 	movaps 0x110(%rsp),%xmm2
  4081ea:	00 
  4081eb:	0f 28 8c 24 00 01 00 	movaps 0x100(%rsp),%xmm1
  4081f2:	00 
  4081f3:	0f 29 9c 24 30 03 00 	movaps %xmm3,0x330(%rsp)
  4081fa:	00 
  4081fb:	0f 29 94 24 e0 02 00 	movaps %xmm2,0x2e0(%rsp)
  408202:	00 
  408203:	0f 29 8c 24 d0 02 00 	movaps %xmm1,0x2d0(%rsp)
  40820a:	00 
  40820b:	0f 29 84 24 80 02 00 	movaps %xmm0,0x280(%rsp)
  408212:	00 
  408213:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  40821a:	00 
  40821b:	0f 28 84 24 20 03 00 	movaps 0x320(%rsp),%xmm0
  408222:	00 
  408223:	0f 28 8c 24 10 03 00 	movaps 0x310(%rsp),%xmm1
  40822a:	00 
  40822b:	0f 28 94 24 c0 02 00 	movaps 0x2c0(%rsp),%xmm2
  408232:	00 
  408233:	0f 28 9c 24 70 02 00 	movaps 0x270(%rsp),%xmm3
  40823a:	00 
  40823b:	0f 29 a4 24 f0 00 00 	movaps %xmm4,0xf0(%rsp)
  408242:	00 
  408243:	0f 29 a4 24 e0 00 00 	movaps %xmm4,0xe0(%rsp)
  40824a:	00 
  40824b:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  408252:	00 
  408253:	48 8d bc 24 f0 00 00 	lea    0xf0(%rsp),%rdi
  40825a:	00 
  40825b:	48 8d b4 24 e0 00 00 	lea    0xe0(%rsp),%rsi
  408262:	00 
  408263:	48 8d 94 24 d0 00 00 	lea    0xd0(%rsp),%rdx
  40826a:	00 
  40826b:	ff d0                	call   *%rax
  40826d:	66 0f 6f 9c 24 f0 00 	movdqa 0xf0(%rsp),%xmm3
  408274:	00 00 
  408276:	66 0f 6f 94 24 e0 00 	movdqa 0xe0(%rsp),%xmm2
  40827d:	00 00 
  40827f:	66 0f 6f 8c 24 d0 00 	movdqa 0xd0(%rsp),%xmm1
  408286:	00 00 
  408288:	66 0f 7f 9c 24 20 03 	movdqa %xmm3,0x320(%rsp)
  40828f:	00 00 
  408291:	66 0f 7f 94 24 10 03 	movdqa %xmm2,0x310(%rsp)
  408298:	00 00 
  40829a:	66 0f 7f 8c 24 c0 02 	movdqa %xmm1,0x2c0(%rsp)
  4082a1:	00 00 
  4082a3:	66 0f 7f 84 24 70 02 	movdqa %xmm0,0x270(%rsp)
  4082aa:	00 00 
  4082ac:	48 8b 84 24 58 02 00 	mov    0x258(%rsp),%rax
  4082b3:	00 
  4082b4:	48 83 e8 02          	sub    $0x2,%rax
  4082b8:	48 89 84 24 58 02 00 	mov    %rax,0x258(%rsp)
  4082bf:	00 
  4082c0:	e9 0f fb ff ff       	jmp    407dd4 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128+0x434>
  4082c5:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  4082cc:	00 
  4082cd:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  4082d4:	00 
  4082d5:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  4082dc:	00 
  4082dd:	0f 28 84 24 50 03 00 	movaps 0x350(%rsp),%xmm0
  4082e4:	00 
  4082e5:	0f 11 00             	movups %xmm0,(%rax)
  4082e8:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  4082ef:	00 
  4082f0:	48 83 c0 10          	add    $0x10,%rax
  4082f4:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4082fb:	00 
  4082fc:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  408303:	00 
  408304:	0f 28 84 24 40 03 00 	movaps 0x340(%rsp),%xmm0
  40830b:	00 
  40830c:	0f 11 00             	movups %xmm0,(%rax)
  40830f:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408316:	00 
  408317:	48 83 c0 20          	add    $0x20,%rax
  40831b:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  408322:	00 
  408323:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  40832a:	00 
  40832b:	0f 28 84 24 30 03 00 	movaps 0x330(%rsp),%xmm0
  408332:	00 
  408333:	0f 11 00             	movups %xmm0,(%rax)
  408336:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  40833d:	00 
  40833e:	48 83 c0 30          	add    $0x30,%rax
  408342:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  408349:	00 
  40834a:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  408351:	00 
  408352:	0f 28 84 24 20 03 00 	movaps 0x320(%rsp),%xmm0
  408359:	00 
  40835a:	0f 11 00             	movups %xmm0,(%rax)
  40835d:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408364:	00 
  408365:	48 83 c0 40          	add    $0x40,%rax
  408369:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  408370:	00 
  408371:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  408378:	00 
  408379:	0f 28 84 24 10 03 00 	movaps 0x310(%rsp),%xmm0
  408380:	00 
  408381:	0f 28 8c 24 b0 04 00 	movaps 0x4b0(%rsp),%xmm1
  408388:	00 
  408389:	66 0f fe c1          	paddd  %xmm1,%xmm0
  40838d:	0f 11 00             	movups %xmm0,(%rax)
  408390:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408397:	00 
  408398:	48 83 c0 50          	add    $0x50,%rax
  40839c:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4083a3:	00 
  4083a4:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  4083ab:	00 
  4083ac:	0f 28 84 24 00 03 00 	movaps 0x300(%rsp),%xmm0
  4083b3:	00 
  4083b4:	0f 28 8c 24 90 04 00 	movaps 0x490(%rsp),%xmm1
  4083bb:	00 
  4083bc:	66 0f fe c1          	paddd  %xmm1,%xmm0
  4083c0:	0f 11 00             	movups %xmm0,(%rax)
  4083c3:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  4083ca:	00 
  4083cb:	48 83 c0 60          	add    $0x60,%rax
  4083cf:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  4083d6:	00 
  4083d7:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  4083de:	00 
  4083df:	0f 28 84 24 f0 02 00 	movaps 0x2f0(%rsp),%xmm0
  4083e6:	00 
  4083e7:	0f 28 8c 24 70 04 00 	movaps 0x470(%rsp),%xmm1
  4083ee:	00 
  4083ef:	66 0f fe c1          	paddd  %xmm1,%xmm0
  4083f3:	0f 11 00             	movups %xmm0,(%rax)
  4083f6:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  4083fd:	00 
  4083fe:	48 83 c0 70          	add    $0x70,%rax
  408402:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  408409:	00 
  40840a:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  408411:	00 
  408412:	0f 28 84 24 e0 02 00 	movaps 0x2e0(%rsp),%xmm0
  408419:	00 
  40841a:	0f 28 8c 24 50 04 00 	movaps 0x450(%rsp),%xmm1
  408421:	00 
  408422:	66 0f fe c1          	paddd  %xmm1,%xmm0
  408426:	0f 11 00             	movups %xmm0,(%rax)
  408429:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408430:	00 
  408431:	48 83 e8 80          	sub    $0xffffffffffffff80,%rax
  408435:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  40843c:	00 
  40843d:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  408444:	00 
  408445:	0f 28 84 24 d0 02 00 	movaps 0x2d0(%rsp),%xmm0
  40844c:	00 
  40844d:	0f 28 8c 24 30 04 00 	movaps 0x430(%rsp),%xmm1
  408454:	00 
  408455:	66 0f fe c1          	paddd  %xmm1,%xmm0
  408459:	0f 11 00             	movups %xmm0,(%rax)
  40845c:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408463:	00 
  408464:	48 05 90 00 00 00    	add    $0x90,%rax
  40846a:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  408471:	00 
  408472:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  408479:	00 
  40847a:	0f 28 84 24 c0 02 00 	movaps 0x2c0(%rsp),%xmm0
  408481:	00 
  408482:	0f 28 8c 24 10 04 00 	movaps 0x410(%rsp),%xmm1
  408489:	00 
  40848a:	66 0f fe c1          	paddd  %xmm1,%xmm0
  40848e:	0f 11 00             	movups %xmm0,(%rax)
  408491:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408498:	00 
  408499:	48 05 a0 00 00 00    	add    $0xa0,%rax
  40849f:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  4084a4:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4084a9:	0f 28 84 24 b0 02 00 	movaps 0x2b0(%rsp),%xmm0
  4084b0:	00 
  4084b1:	0f 28 8c 24 f0 03 00 	movaps 0x3f0(%rsp),%xmm1
  4084b8:	00 
  4084b9:	66 0f fe c1          	paddd  %xmm1,%xmm0
  4084bd:	0f 11 00             	movups %xmm0,(%rax)
  4084c0:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  4084c7:	00 
  4084c8:	48 05 b0 00 00 00    	add    $0xb0,%rax
  4084ce:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4084d3:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4084d8:	0f 28 84 24 a0 02 00 	movaps 0x2a0(%rsp),%xmm0
  4084df:	00 
  4084e0:	0f 28 8c 24 d0 03 00 	movaps 0x3d0(%rsp),%xmm1
  4084e7:	00 
  4084e8:	66 0f fe c1          	paddd  %xmm1,%xmm0
  4084ec:	0f 11 00             	movups %xmm0,(%rax)
  4084ef:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  4084f6:	00 
  4084f7:	48 05 c0 00 00 00    	add    $0xc0,%rax
  4084fd:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  408502:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  408507:	0f 28 84 24 90 02 00 	movaps 0x290(%rsp),%xmm0
  40850e:	00 
  40850f:	0f 11 00             	movups %xmm0,(%rax)
  408512:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408519:	00 
  40851a:	48 05 d0 00 00 00    	add    $0xd0,%rax
  408520:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  408525:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40852a:	0f 28 84 24 80 02 00 	movaps 0x280(%rsp),%xmm0
  408531:	00 
  408532:	0f 28 8c 24 b0 03 00 	movaps 0x3b0(%rsp),%xmm1
  408539:	00 
  40853a:	66 0f fe c1          	paddd  %xmm1,%xmm0
  40853e:	0f 11 00             	movups %xmm0,(%rax)
  408541:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408548:	00 
  408549:	48 05 e0 00 00 00    	add    $0xe0,%rax
  40854f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  408554:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  408559:	0f 28 84 24 70 02 00 	movaps 0x270(%rsp),%xmm0
  408560:	00 
  408561:	0f 28 8c 24 a0 03 00 	movaps 0x3a0(%rsp),%xmm1
  408568:	00 
  408569:	66 0f fe c1          	paddd  %xmm1,%xmm0
  40856d:	0f 11 00             	movups %xmm0,(%rax)
  408570:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  408577:	00 
  408578:	48 05 f0 00 00 00    	add    $0xf0,%rax
  40857e:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  408583:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  408588:	0f 28 84 24 60 02 00 	movaps 0x260(%rsp),%xmm0
  40858f:	00 
  408590:	0f 28 8c 24 90 03 00 	movaps 0x390(%rsp),%xmm1
  408597:	00 
  408598:	66 0f fe c1          	paddd  %xmm1,%xmm0
  40859c:	0f 11 00             	movups %xmm0,(%rax)
  40859f:	0f 28 84 24 c0 03 00 	movaps 0x3c0(%rsp),%xmm0
  4085a6:	00 
  4085a7:	0f 28 0d 22 61 00 00 	movaps 0x6122(%rip),%xmm1        # 40e6d0 <_IO_stdin_used+0x6d0>
  4085ae:	66 0f fe c1          	paddd  %xmm1,%xmm0
  4085b2:	66 0f 7f 84 24 c0 03 	movdqa %xmm0,0x3c0(%rsp)
  4085b9:	00 00 
  4085bb:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  4085c2:	00 
  4085c3:	48 05 00 01 00 00    	add    $0x100,%rax
  4085c9:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4085ce:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4085d3:	48 89 84 24 78 03 00 	mov    %rax,0x378(%rsp)
  4085da:	00 
  4085db:	48 8b 84 24 68 03 00 	mov    0x368(%rsp),%rax
  4085e2:	00 
  4085e3:	48 83 c0 01          	add    $0x1,%rax
  4085e7:	48 89 84 24 68 03 00 	mov    %rax,0x368(%rsp)
  4085ee:	00 
  4085ef:	48 8b 84 24 60 03 00 	mov    0x360(%rsp),%rax
  4085f6:	00 
  4085f7:	48 83 c0 01          	add    $0x1,%rax
  4085fb:	48 89 84 24 60 03 00 	mov    %rax,0x360(%rsp)
  408602:	00 
  408603:	e9 9d f6 ff ff       	jmp    407ca5 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128+0x305>
  408608:	48 81 c4 18 05 00 00 	add    $0x518,%rsp
  40860f:	c3                   	ret

0000000000408610 <_proclit$anon-1>:
  408610:	48 83 ec 68          	sub    $0x68,%rsp
  408614:	48 89 54 24 88       	mov    %rdx,-0x78(%rsp)
  408619:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  40861e:	48 89 7c 24 98       	mov    %rdi,-0x68(%rsp)
  408623:	0f 29 5c 24 a0       	movaps %xmm3,-0x60(%rsp)
  408628:	0f 29 54 24 b0       	movaps %xmm2,-0x50(%rsp)
  40862d:	0f 29 4c 24 c0       	movaps %xmm1,-0x40(%rsp)
  408632:	0f 29 44 24 d0       	movaps %xmm0,-0x30(%rsp)
  408637:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40863c:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  408641:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  408646:	0f 28 44 24 a0       	movaps -0x60(%rsp),%xmm0
  40864b:	0f 28 4c 24 b0       	movaps -0x50(%rsp),%xmm1
  408650:	0f 28 54 24 c0       	movaps -0x40(%rsp),%xmm2
  408655:	0f 28 5c 24 d0       	movaps -0x30(%rsp),%xmm3
  40865a:	0f 29 5c 24 50       	movaps %xmm3,0x50(%rsp)
  40865f:	0f 29 54 24 40       	movaps %xmm2,0x40(%rsp)
  408664:	0f 29 4c 24 30       	movaps %xmm1,0x30(%rsp)
  408669:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  40866e:	0f 29 5c 24 10       	movaps %xmm3,0x10(%rsp)
  408673:	0f 29 14 24          	movaps %xmm2,(%rsp)
  408677:	0f 29 4c 24 f0       	movaps %xmm1,-0x10(%rsp)
  40867c:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  408681:	0f 28 44 24 10       	movaps 0x10(%rsp),%xmm0
  408686:	0f 28 0c 24          	movaps (%rsp),%xmm1
  40868a:	66 0f fe c1          	paddd  %xmm1,%xmm0
  40868e:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  408693:	0f 28 44 24 e0       	movaps -0x20(%rsp),%xmm0
  408698:	0f 28 4c 24 10       	movaps 0x10(%rsp),%xmm1
  40869d:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4086a1:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  4086a6:	0f 28 44 24 e0       	movaps -0x20(%rsp),%xmm0
  4086ab:	0f 28 0d 2e 60 00 00 	movaps 0x602e(%rip),%xmm1        # 40e6e0 <_IO_stdin_used+0x6e0>
  4086b2:	66 0f 38 00 c1       	pshufb %xmm1,%xmm0
  4086b7:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  4086bc:	0f 28 44 24 f0       	movaps -0x10(%rsp),%xmm0
  4086c1:	0f 28 4c 24 e0       	movaps -0x20(%rsp),%xmm1
  4086c6:	66 0f fe c1          	paddd  %xmm1,%xmm0
  4086ca:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  4086cf:	0f 28 04 24          	movaps (%rsp),%xmm0
  4086d3:	0f 28 4c 24 f0       	movaps -0x10(%rsp),%xmm1
  4086d8:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4086dc:	0f 29 04 24          	movaps %xmm0,(%rsp)
  4086e0:	0f 28 04 24          	movaps (%rsp),%xmm0
  4086e4:	0f 28 c8             	movaps %xmm0,%xmm1
  4086e7:	66 0f 72 d1 14       	psrld  $0x14,%xmm1
  4086ec:	66 0f 72 f0 0c       	pslld  $0xc,%xmm0
  4086f1:	66 0f eb c1          	por    %xmm1,%xmm0
  4086f5:	0f 29 04 24          	movaps %xmm0,(%rsp)
  4086f9:	0f 28 44 24 10       	movaps 0x10(%rsp),%xmm0
  4086fe:	0f 28 0c 24          	movaps (%rsp),%xmm1
  408702:	66 0f fe c1          	paddd  %xmm1,%xmm0
  408706:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  40870b:	0f 28 44 24 e0       	movaps -0x20(%rsp),%xmm0
  408710:	0f 28 4c 24 10       	movaps 0x10(%rsp),%xmm1
  408715:	66 0f ef c1          	pxor   %xmm1,%xmm0
  408719:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  40871e:	0f 28 44 24 e0       	movaps -0x20(%rsp),%xmm0
  408723:	0f 28 0d c6 5f 00 00 	movaps 0x5fc6(%rip),%xmm1        # 40e6f0 <_IO_stdin_used+0x6f0>
  40872a:	66 0f 38 00 c1       	pshufb %xmm1,%xmm0
  40872f:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  408734:	0f 28 44 24 f0       	movaps -0x10(%rsp),%xmm0
  408739:	0f 28 4c 24 e0       	movaps -0x20(%rsp),%xmm1
  40873e:	66 0f fe c1          	paddd  %xmm1,%xmm0
  408742:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  408747:	0f 28 04 24          	movaps (%rsp),%xmm0
  40874b:	0f 28 4c 24 f0       	movaps -0x10(%rsp),%xmm1
  408750:	66 0f ef c1          	pxor   %xmm1,%xmm0
  408754:	0f 29 04 24          	movaps %xmm0,(%rsp)
  408758:	0f 28 0c 24          	movaps (%rsp),%xmm1
  40875c:	0f 28 c1             	movaps %xmm1,%xmm0
  40875f:	66 0f 72 f0 07       	pslld  $0x7,%xmm0
  408764:	66 0f 72 d1 19       	psrld  $0x19,%xmm1
  408769:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40876d:	66 0f 7f 04 24       	movdqa %xmm0,(%rsp)
  408772:	66 0f 6f 5c 24 10    	movdqa 0x10(%rsp),%xmm3
  408778:	66 0f 6f 14 24       	movdqa (%rsp),%xmm2
  40877d:	66 0f 6f 4c 24 f0    	movdqa -0x10(%rsp),%xmm1
  408783:	66 0f 6f 44 24 e0    	movdqa -0x20(%rsp),%xmm0
  408789:	66 0f 7f 1a          	movdqa %xmm3,(%rdx)
  40878d:	66 0f 7f 11          	movdqa %xmm2,(%rcx)
  408791:	66 0f 7f 08          	movdqa %xmm1,(%rax)
  408795:	48 83 c4 68          	add    $0x68,%rsp
  408799:	c3                   	ret
  40879a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004087a0 <runtime::add_thread_local_cleaner>:
  4087a0:	48 83 ec 38          	sub    $0x38,%rsp
  4087a4:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4087a9:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4087ae:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4087b3:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4087b8:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4087bd:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4087c2:	48 c7 44 24 20 08 00 	movq   $0x8,0x20(%rsp)
  4087c9:	00 00 
  4087cb:	48 c7 44 24 18 ff ff 	movq   $0xffffffffffffffff,0x18(%rsp)
  4087d2:	ff ff 
  4087d4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4087d9:	48 83 c0 01          	add    $0x1,%rax
  4087dd:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4087e2:	48 83 f8 08          	cmp    $0x8,%rax
  4087e6:	7d 41                	jge    408829 <runtime::add_thread_local_cleaner+0x89>
  4087e8:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4087ed:	48 c7 c0 b8 31 41 00 	mov    $0x4131b8,%rax
  4087f4:	48 c1 e1 04          	shl    $0x4,%rcx
  4087f8:	48 01 c8             	add    %rcx,%rax
  4087fb:	48 89 04 24          	mov    %rax,(%rsp)
  4087ff:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  408804:	0f 94 c0             	sete   %al
  408807:	24 01                	and    $0x1,%al
  408809:	3c 00                	cmp    $0x0,%al
  40880b:	74 1a                	je     408827 <runtime::add_thread_local_cleaner+0x87>
  40880d:	48 8b 04 24          	mov    (%rsp),%rax
  408811:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  408816:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40881b:	48 89 10             	mov    %rdx,(%rax)
  40881e:	48 89 48 08          	mov    %rcx,0x8(%rax)
  408822:	48 83 c4 38          	add    $0x38,%rsp
  408826:	c3                   	ret
  408827:	eb ab                	jmp    4087d4 <runtime::add_thread_local_cleaner+0x34>
  408829:	bf 70 e7 40 00       	mov    $0x40e770,%edi
  40882e:	ba 00 e8 40 00       	mov    $0x40e800,%edx
  408833:	be 37 00 00 00       	mov    $0x37,%esi
  408838:	e8 33 3a 00 00       	call   40c270 <runtime::panic_contextless>
  40883d:	0f 1f 00             	nopl   (%rax)

0000000000408840 <strconv::is_integer_negative>:
  408840:	48 83 ec 58          	sub    $0x58,%rsp
  408844:	4c 89 04 24          	mov    %r8,(%rsp)
  408848:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40884d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  408852:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  408857:	40 88 f0             	mov    %sil,%al
  40885a:	88 44 24 27          	mov    %al,0x27(%rsp)
  40885e:	8a 44 24 27          	mov    0x27(%rsp),%al
  408862:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  408867:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40886c:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  408871:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  408875:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40887a:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  408881:	00 00 
  408883:	c6 44 24 37 00       	movb   $0x0,0x37(%rsp)
  408888:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40888d:	3c 00                	cmp    $0x0,%al
  40888f:	0f 84 1e 01 00 00    	je     4089b3 <strconv::is_integer_negative+0x173>
  408895:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40889a:	48 83 e8 08          	sub    $0x8,%rax
  40889e:	74 30                	je     4088d0 <strconv::is_integer_negative+0x90>
  4088a0:	eb 00                	jmp    4088a2 <strconv::is_integer_negative+0x62>
  4088a2:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4088a7:	48 83 e8 10          	sub    $0x10,%rax
  4088ab:	74 57                	je     408904 <strconv::is_integer_negative+0xc4>
  4088ad:	eb 00                	jmp    4088af <strconv::is_integer_negative+0x6f>
  4088af:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4088b4:	48 83 e8 20          	sub    $0x20,%rax
  4088b8:	74 7d                	je     408937 <strconv::is_integer_negative+0xf7>
  4088ba:	eb 00                	jmp    4088bc <strconv::is_integer_negative+0x7c>
  4088bc:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4088c1:	48 83 e8 40          	sub    $0x40,%rax
  4088c5:	0f 84 9c 00 00 00    	je     408967 <strconv::is_integer_negative+0x127>
  4088cb:	e9 c9 00 00 00       	jmp    408999 <strconv::is_integer_negative+0x159>
  4088d0:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4088d5:	88 44 24 36          	mov    %al,0x36(%rsp)
  4088d9:	80 7c 24 36 00       	cmpb   $0x0,0x36(%rsp)
  4088de:	0f 9c c0             	setl   %al
  4088e1:	24 01                	and    $0x1,%al
  4088e3:	88 44 24 37          	mov    %al,0x37(%rsp)
  4088e7:	48 0f be 44 24 36    	movsbq 0x36(%rsp),%rax
  4088ed:	31 c9                	xor    %ecx,%ecx
  4088ef:	48 29 c1             	sub    %rax,%rcx
  4088f2:	48 83 f8 00          	cmp    $0x0,%rax
  4088f6:	48 0f 4c c1          	cmovl  %rcx,%rax
  4088fa:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4088ff:	e9 ad 00 00 00       	jmp    4089b1 <strconv::is_integer_negative+0x171>
  408904:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  408909:	66 89 44 24 34       	mov    %ax,0x34(%rsp)
  40890e:	66 83 7c 24 34 00    	cmpw   $0x0,0x34(%rsp)
  408914:	0f 9c c0             	setl   %al
  408917:	24 01                	and    $0x1,%al
  408919:	88 44 24 37          	mov    %al,0x37(%rsp)
  40891d:	48 0f bf 44 24 34    	movswq 0x34(%rsp),%rax
  408923:	31 c9                	xor    %ecx,%ecx
  408925:	48 29 c1             	sub    %rax,%rcx
  408928:	48 83 f8 00          	cmp    $0x0,%rax
  40892c:	48 0f 4c c1          	cmovl  %rcx,%rax
  408930:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  408935:	eb 7a                	jmp    4089b1 <strconv::is_integer_negative+0x171>
  408937:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40893c:	89 44 24 30          	mov    %eax,0x30(%rsp)
  408940:	83 7c 24 30 00       	cmpl   $0x0,0x30(%rsp)
  408945:	0f 9c c0             	setl   %al
  408948:	24 01                	and    $0x1,%al
  40894a:	88 44 24 37          	mov    %al,0x37(%rsp)
  40894e:	48 63 44 24 30       	movslq 0x30(%rsp),%rax
  408953:	31 c9                	xor    %ecx,%ecx
  408955:	48 29 c1             	sub    %rax,%rcx
  408958:	48 83 f8 00          	cmp    $0x0,%rax
  40895c:	48 0f 4c c1          	cmovl  %rcx,%rax
  408960:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  408965:	eb 4a                	jmp    4089b1 <strconv::is_integer_negative+0x171>
  408967:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40896c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  408971:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
  408977:	0f 9c c0             	setl   %al
  40897a:	24 01                	and    $0x1,%al
  40897c:	88 44 24 37          	mov    %al,0x37(%rsp)
  408980:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  408985:	31 c9                	xor    %ecx,%ecx
  408987:	48 29 c1             	sub    %rax,%rcx
  40898a:	48 83 f8 00          	cmp    $0x0,%rax
  40898e:	48 0f 4c c1          	cmovl  %rcx,%rax
  408992:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  408997:	eb 18                	jmp    4089b1 <strconv::is_integer_negative+0x171>
  408999:	48 8b 0c 24          	mov    (%rsp),%rcx
  40899d:	bf a8 e8 40 00       	mov    $0x40e8a8,%edi
  4089a2:	ba 10 e9 40 00       	mov    $0x40e910,%edx
  4089a7:	be 29 00 00 00       	mov    $0x29,%esi
  4089ac:	e8 3f 38 00 00       	call   40c1f0 <runtime::panic>
  4089b1:	eb 00                	jmp    4089b3 <strconv::is_integer_negative+0x173>
  4089b3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4089b8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4089bd:	8a 44 24 37          	mov    0x37(%rsp),%al
  4089c1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4089c6:	88 44 24 37          	mov    %al,0x37(%rsp)
  4089ca:	48 89 11             	mov    %rdx,(%rcx)
  4089cd:	48 83 c4 58          	add    $0x58,%rsp
  4089d1:	c3                   	ret
  4089d2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4089d9:	1f 84 00 00 00 00 00 

00000000004089e0 <strconv::write_bits>:
  4089e0:	48 81 ec 68 02 00 00 	sub    $0x268,%rsp
  4089e7:	4c 89 8c 24 e8 00 00 	mov    %r9,0xe8(%rsp)
  4089ee:	00 
  4089ef:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  4089f6:	00 
  4089f7:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  4089fe:	00 
  4089ff:	44 88 c0             	mov    %r8b,%al
  408a02:	88 84 24 07 01 00 00 	mov    %al,0x107(%rsp)
  408a09:	48 89 b4 24 08 01 00 	mov    %rsi,0x108(%rsp)
  408a10:	00 
  408a11:	48 89 bc 24 10 01 00 	mov    %rdi,0x110(%rsp)
  408a18:	00 
  408a19:	48 8b 84 24 88 02 00 	mov    0x288(%rsp),%rax
  408a20:	00 
  408a21:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  408a28:	00 
  408a29:	8a 84 24 80 02 00 00 	mov    0x280(%rsp),%al
  408a30:	88 84 24 27 01 00 00 	mov    %al,0x127(%rsp)
  408a37:	48 8d 84 24 70 02 00 	lea    0x270(%rsp),%rax
  408a3e:	00 
  408a3f:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  408a46:	00 
  408a47:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  408a4e:	00 
  408a4f:	8a 8c 24 27 01 00 00 	mov    0x127(%rsp),%cl
  408a56:	48 8b 94 24 28 01 00 	mov    0x128(%rsp),%rdx
  408a5d:	00 
  408a5e:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  408a65:	00 
  408a66:	40 8a bc 24 07 01 00 	mov    0x107(%rsp),%dil
  408a6d:	00 
  408a6e:	4c 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%r8
  408a75:	00 
  408a76:	4c 8b 8c 24 08 01 00 	mov    0x108(%rsp),%r9
  408a7d:	00 
  408a7e:	4c 8b 94 24 10 01 00 	mov    0x110(%rsp),%r10
  408a85:	00 
  408a86:	4c 89 94 24 58 02 00 	mov    %r10,0x258(%rsp)
  408a8d:	00 
  408a8e:	4c 89 8c 24 60 02 00 	mov    %r9,0x260(%rsp)
  408a95:	00 
  408a96:	4c 89 84 24 50 02 00 	mov    %r8,0x250(%rsp)
  408a9d:	00 
  408a9e:	48 89 84 24 48 02 00 	mov    %rax,0x248(%rsp)
  408aa5:	00 
  408aa6:	40 88 bc 24 47 02 00 	mov    %dil,0x247(%rsp)
  408aad:	00 
  408aae:	48 89 b4 24 38 02 00 	mov    %rsi,0x238(%rsp)
  408ab5:	00 
  408ab6:	48 8b 32             	mov    (%rdx),%rsi
  408ab9:	48 89 b4 24 28 02 00 	mov    %rsi,0x228(%rsp)
  408ac0:	00 
  408ac1:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  408ac5:	48 89 94 24 30 02 00 	mov    %rdx,0x230(%rsp)
  408acc:	00 
  408acd:	88 8c 24 27 02 00 00 	mov    %cl,0x227(%rsp)
  408ad4:	48 83 f8 02          	cmp    $0x2,%rax
  408ad8:	0f 9c c0             	setl   %al
  408adb:	24 01                	and    $0x1,%al
  408add:	3c 00                	cmp    $0x0,%al
  408adf:	75 15                	jne    408af6 <strconv::write_bits+0x116>
  408ae1:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  408ae8:	00 
  408ae9:	48 83 f8 20          	cmp    $0x20,%rax
  408aed:	0f 9f c0             	setg   %al
  408af0:	24 01                	and    $0x1,%al
  408af2:	3c 00                	cmp    $0x0,%al
  408af4:	74 1c                	je     408b12 <strconv::write_bits+0x132>
  408af6:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  408afd:	00 
  408afe:	bf 38 e9 40 00       	mov    $0x40e938,%edi
  408b03:	ba 70 e9 40 00       	mov    $0x40e970,%edx
  408b08:	be 2a 00 00 00       	mov    $0x2a,%esi
  408b0d:	e8 de 36 00 00       	call   40c1f0 <runtime::panic>
  408b12:	48 8d bc 24 a6 01 00 	lea    0x1a6(%rsp),%rdi
  408b19:	00 
  408b1a:	31 f6                	xor    %esi,%esi
  408b1c:	ba 81 00 00 00       	mov    $0x81,%edx
  408b21:	e8 1a 85 ff ff       	call   401040 <memset@plt>
  408b26:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  408b2d:	00 
  408b2e:	8a 84 24 07 01 00 00 	mov    0x107(%rsp),%al
  408b35:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  408b3c:	00 
  408b3d:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  408b44:	00 
  408b45:	48 c7 84 24 98 01 00 	movq   $0x81,0x198(%rsp)
  408b4c:	00 81 00 00 00 
  408b51:	48 c7 84 24 90 01 00 	movq   $0x0,0x190(%rsp)
  408b58:	00 00 00 00 00 
  408b5d:	48 8d 8c 24 90 01 00 	lea    0x190(%rsp),%rcx
  408b64:	00 
  408b65:	0f b6 f0             	movzbl %al,%esi
  408b68:	e8 d3 fc ff ff       	call   408840 <strconv::is_integer_negative>
  408b6d:	88 c1                	mov    %al,%cl
  408b6f:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  408b76:	00 
  408b77:	48 8b 94 24 90 01 00 	mov    0x190(%rsp),%rdx
  408b7e:	00 
  408b7f:	48 89 94 24 88 01 00 	mov    %rdx,0x188(%rsp)
  408b86:	00 
  408b87:	88 8c 24 87 01 00 00 	mov    %cl,0x187(%rsp)
  408b8e:	48 89 84 24 78 01 00 	mov    %rax,0x178(%rsp)
  408b95:	00 
  408b96:	48 8b 84 24 88 01 00 	mov    0x188(%rsp),%rax
  408b9d:	00 
  408b9e:	48 3b 84 24 78 01 00 	cmp    0x178(%rsp),%rax
  408ba5:	00 
  408ba6:	0f 93 c0             	setae  %al
  408ba9:	24 01                	and    $0x1,%al
  408bab:	3c 00                	cmp    $0x0,%al
  408bad:	0f 84 57 01 00 00    	je     408d0a <strconv::write_bits+0x32a>
  408bb3:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408bba:	00 
  408bbb:	48 83 e8 01          	sub    $0x1,%rax
  408bbf:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  408bc6:	00 
  408bc7:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  408bce:	00 
  408bcf:	48 8d 84 24 a6 01 00 	lea    0x1a6(%rsp),%rax
  408bd6:	00 
  408bd7:	4c 01 c0             	add    %r8,%rax
  408bda:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  408be1:	00 
  408be2:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408be7:	be 28 00 00 00       	mov    $0x28,%esi
  408bec:	ba 4b 00 00 00       	mov    $0x4b,%edx
  408bf1:	b9 0b 00 00 00       	mov    $0xb,%ecx
  408bf6:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  408bfc:	e8 4f 1a 00 00       	call   40a650 <runtime::bounds_check_error>
  408c01:	48 8b 84 24 28 02 00 	mov    0x228(%rsp),%rax
  408c08:	00 
  408c09:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  408c10:	00 
  408c11:	48 8b 84 24 30 02 00 	mov    0x230(%rsp),%rax
  408c18:	00 
  408c19:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  408c20:	00 
  408c21:	48 8b 84 24 88 01 00 	mov    0x188(%rsp),%rax
  408c28:	00 
  408c29:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  408c30:	00 
  408c31:	48 8b 84 24 78 01 00 	mov    0x178(%rsp),%rax
  408c38:	00 
  408c39:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  408c40:	00 
  408c41:	48 83 f8 00          	cmp    $0x0,%rax
  408c45:	74 1f                	je     408c66 <strconv::write_bits+0x286>
  408c47:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  408c4e:	00 
  408c4f:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  408c56:	00 
  408c57:	31 d2                	xor    %edx,%edx
  408c59:	48 f7 f1             	div    %rcx
  408c5c:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  408c63:	00 
  408c64:	eb 02                	jmp    408c68 <strconv::write_bits+0x288>
  408c66:	0f 0b                	ud2
  408c68:	4c 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%r8
  408c6f:	00 
  408c70:	4c 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%r9
  408c77:	00 
  408c78:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408c7d:	be 28 00 00 00       	mov    $0x28,%esi
  408c82:	ba 4b 00 00 00       	mov    $0x4b,%edx
  408c87:	b9 17 00 00 00       	mov    $0x17,%ecx
  408c8c:	e8 bf 19 00 00       	call   40a650 <runtime::bounds_check_error>
  408c91:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  408c98:	00 
  408c99:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  408ca0:	00 
  408ca1:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  408ca8:	00 
  408ca9:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  408cac:	88 08                	mov    %cl,(%rax)
  408cae:	48 8b 84 24 78 01 00 	mov    0x178(%rsp),%rax
  408cb5:	00 
  408cb6:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  408cbd:	00 
  408cbe:	48 8b 8c 24 88 01 00 	mov    0x188(%rsp),%rcx
  408cc5:	00 
  408cc6:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  408ccd:	00 
  408cce:	48 83 f8 00          	cmp    $0x0,%rax
  408cd2:	74 1f                	je     408cf3 <strconv::write_bits+0x313>
  408cd4:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  408cdb:	00 
  408cdc:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  408ce3:	00 
  408ce4:	31 d2                	xor    %edx,%edx
  408ce6:	48 f7 f1             	div    %rcx
  408ce9:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  408cf0:	00 
  408cf1:	eb 02                	jmp    408cf5 <strconv::write_bits+0x315>
  408cf3:	0f 0b                	ud2
  408cf5:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  408cfc:	00 
  408cfd:	48 89 84 24 88 01 00 	mov    %rax,0x188(%rsp)
  408d04:	00 
  408d05:	e9 8c fe ff ff       	jmp    408b96 <strconv::write_bits+0x1b6>
  408d0a:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408d11:	00 
  408d12:	48 83 e8 01          	sub    $0x1,%rax
  408d16:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  408d1d:	00 
  408d1e:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  408d25:	00 
  408d26:	48 8d 84 24 a6 01 00 	lea    0x1a6(%rsp),%rax
  408d2d:	00 
  408d2e:	4c 01 c0             	add    %r8,%rax
  408d31:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  408d36:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408d3b:	be 28 00 00 00       	mov    $0x28,%esi
  408d40:	ba 4e 00 00 00       	mov    $0x4e,%edx
  408d45:	b9 0a 00 00 00       	mov    $0xa,%ecx
  408d4a:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  408d50:	e8 fb 18 00 00       	call   40a650 <runtime::bounds_check_error>
  408d55:	48 8b 84 24 28 02 00 	mov    0x228(%rsp),%rax
  408d5c:	00 
  408d5d:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  408d64:	00 
  408d65:	48 8b 84 24 30 02 00 	mov    0x230(%rsp),%rax
  408d6c:	00 
  408d6d:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  408d74:	00 
  408d75:	48 8b 84 24 88 01 00 	mov    0x188(%rsp),%rax
  408d7c:	00 
  408d7d:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  408d84:	00 
  408d85:	48 8b 84 24 78 01 00 	mov    0x178(%rsp),%rax
  408d8c:	00 
  408d8d:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  408d94:	00 
  408d95:	48 83 f8 00          	cmp    $0x0,%rax
  408d99:	74 1c                	je     408db7 <strconv::write_bits+0x3d7>
  408d9b:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  408da2:	00 
  408da3:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  408daa:	00 
  408dab:	31 d2                	xor    %edx,%edx
  408dad:	48 f7 f1             	div    %rcx
  408db0:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  408db5:	eb 02                	jmp    408db9 <strconv::write_bits+0x3d9>
  408db7:	0f 0b                	ud2
  408db9:	4c 8b 44 24 70       	mov    0x70(%rsp),%r8
  408dbe:	4c 8b 8c 24 88 00 00 	mov    0x88(%rsp),%r9
  408dc5:	00 
  408dc6:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408dcb:	be 28 00 00 00       	mov    $0x28,%esi
  408dd0:	ba 4e 00 00 00       	mov    $0x4e,%edx
  408dd5:	b9 16 00 00 00       	mov    $0x16,%ecx
  408dda:	e8 71 18 00 00       	call   40a650 <runtime::bounds_check_error>
  408ddf:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  408de4:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  408deb:	00 
  408dec:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  408df1:	8a 84 24 27 01 00 00 	mov    0x127(%rsp),%al
  408df8:	8a 14 32             	mov    (%rdx,%rsi,1),%dl
  408dfb:	88 11                	mov    %dl,(%rcx)
  408dfd:	24 01                	and    $0x1,%al
  408dff:	3c 00                	cmp    $0x0,%al
  408e01:	0f 95 c0             	setne  %al
  408e04:	24 01                	and    $0x1,%al
  408e06:	3c 00                	cmp    $0x0,%al
  408e08:	0f 84 d5 01 00 00    	je     408fe3 <strconv::write_bits+0x603>
  408e0e:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  408e15:	00 
  408e16:	c6 84 24 77 01 00 00 	movb   $0x1,0x177(%rsp)
  408e1d:	01 
  408e1e:	48 83 c0 fe          	add    $0xfffffffffffffffe,%rax
  408e22:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  408e27:	48 83 e8 0e          	sub    $0xe,%rax
  408e2b:	0f 87 51 01 00 00    	ja     408f82 <strconv::write_bits+0x5a2>
  408e31:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  408e36:	48 8b 04 c5 30 e8 40 	mov    0x40e830(,%rax,8),%rax
  408e3d:	00 
  408e3e:	ff e0                	jmp    *%rax
  408e40:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408e47:	00 
  408e48:	48 83 e8 01          	sub    $0x1,%rax
  408e4c:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  408e53:	00 
  408e54:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  408e5b:	00 
  408e5c:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  408e61:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408e66:	be 28 00 00 00       	mov    $0x28,%esi
  408e6b:	ba 53 00 00 00       	mov    $0x53,%edx
  408e70:	b9 14 00 00 00       	mov    $0x14,%ecx
  408e75:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  408e7b:	e8 d0 17 00 00       	call   40a650 <runtime::bounds_check_error>
  408e80:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  408e85:	c6 84 04 a6 01 00 00 	movb   $0x62,0x1a6(%rsp,%rax,1)
  408e8c:	62 
  408e8d:	e9 f8 00 00 00       	jmp    408f8a <strconv::write_bits+0x5aa>
  408e92:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408e99:	00 
  408e9a:	48 83 e8 01          	sub    $0x1,%rax
  408e9e:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  408ea5:	00 
  408ea6:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  408ead:	00 
  408eae:	4c 89 44 24 58       	mov    %r8,0x58(%rsp)
  408eb3:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408eb8:	be 28 00 00 00       	mov    $0x28,%esi
  408ebd:	ba 54 00 00 00       	mov    $0x54,%edx
  408ec2:	b9 14 00 00 00       	mov    $0x14,%ecx
  408ec7:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  408ecd:	e8 7e 17 00 00       	call   40a650 <runtime::bounds_check_error>
  408ed2:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  408ed7:	c6 84 04 a6 01 00 00 	movb   $0x6f,0x1a6(%rsp,%rax,1)
  408ede:	6f 
  408edf:	e9 a6 00 00 00       	jmp    408f8a <strconv::write_bits+0x5aa>
  408ee4:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408eeb:	00 
  408eec:	48 83 e8 01          	sub    $0x1,%rax
  408ef0:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  408ef7:	00 
  408ef8:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  408eff:	00 
  408f00:	4c 89 44 24 50       	mov    %r8,0x50(%rsp)
  408f05:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408f0a:	be 28 00 00 00       	mov    $0x28,%esi
  408f0f:	ba 56 00 00 00       	mov    $0x56,%edx
  408f14:	b9 14 00 00 00       	mov    $0x14,%ecx
  408f19:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  408f1f:	e8 2c 17 00 00       	call   40a650 <runtime::bounds_check_error>
  408f24:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  408f29:	c6 84 04 a6 01 00 00 	movb   $0x7a,0x1a6(%rsp,%rax,1)
  408f30:	7a 
  408f31:	eb 57                	jmp    408f8a <strconv::write_bits+0x5aa>
  408f33:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408f3a:	00 
  408f3b:	48 83 e8 01          	sub    $0x1,%rax
  408f3f:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  408f46:	00 
  408f47:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  408f4e:	00 
  408f4f:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  408f54:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408f59:	be 28 00 00 00       	mov    $0x28,%esi
  408f5e:	ba 57 00 00 00       	mov    $0x57,%edx
  408f63:	b9 14 00 00 00       	mov    $0x14,%ecx
  408f68:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  408f6e:	e8 dd 16 00 00       	call   40a650 <runtime::bounds_check_error>
  408f73:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  408f78:	c6 84 04 a6 01 00 00 	movb   $0x78,0x1a6(%rsp,%rax,1)
  408f7f:	78 
  408f80:	eb 08                	jmp    408f8a <strconv::write_bits+0x5aa>
  408f82:	c6 84 24 77 01 00 00 	movb   $0x0,0x177(%rsp)
  408f89:	00 
  408f8a:	80 bc 24 77 01 00 00 	cmpb   $0x0,0x177(%rsp)
  408f91:	00 
  408f92:	74 4d                	je     408fe1 <strconv::write_bits+0x601>
  408f94:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408f9b:	00 
  408f9c:	48 83 e8 01          	sub    $0x1,%rax
  408fa0:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  408fa7:	00 
  408fa8:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  408faf:	00 
  408fb0:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  408fb5:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  408fba:	be 28 00 00 00       	mov    $0x28,%esi
  408fbf:	ba 5b 00 00 00       	mov    $0x5b,%edx
  408fc4:	b9 0c 00 00 00       	mov    $0xc,%ecx
  408fc9:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  408fcf:	e8 7c 16 00 00       	call   40a650 <runtime::bounds_check_error>
  408fd4:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  408fd9:	c6 84 04 a6 01 00 00 	movb   $0x30,0x1a6(%rsp,%rax,1)
  408fe0:	30 
  408fe1:	eb 00                	jmp    408fe3 <strconv::write_bits+0x603>
  408fe3:	b0 01                	mov    $0x1,%al
  408fe5:	3a 84 24 87 01 00 00 	cmp    0x187(%rsp),%al
  408fec:	74 19                	je     409007 <strconv::write_bits+0x627>
  408fee:	8a 84 24 27 01 00 00 	mov    0x127(%rsp),%al
  408ff5:	24 02                	and    $0x2,%al
  408ff7:	3c 00                	cmp    $0x0,%al
  408ff9:	0f 95 c1             	setne  %cl
  408ffc:	80 e1 01             	and    $0x1,%cl
  408fff:	b0 01                	mov    $0x1,%al
  409001:	38 c8                	cmp    %cl,%al
  409003:	74 53                	je     409058 <strconv::write_bits+0x678>
  409005:	eb 4f                	jmp    409056 <strconv::write_bits+0x676>
  409007:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  40900e:	00 
  40900f:	48 83 e8 01          	sub    $0x1,%rax
  409013:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  40901a:	00 
  40901b:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  409022:	00 
  409023:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  409028:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  40902d:	be 28 00 00 00       	mov    $0x28,%esi
  409032:	ba 61 00 00 00       	mov    $0x61,%edx
  409037:	b9 0b 00 00 00       	mov    $0xb,%ecx
  40903c:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  409042:	e8 09 16 00 00       	call   40a650 <runtime::bounds_check_error>
  409047:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40904c:	c6 84 04 a6 01 00 00 	movb   $0x2d,0x1a6(%rsp,%rax,1)
  409053:	2d 
  409054:	eb 4f                	jmp    4090a5 <strconv::write_bits+0x6c5>
  409056:	eb 4d                	jmp    4090a5 <strconv::write_bits+0x6c5>
  409058:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  40905f:	00 
  409060:	48 83 e8 01          	sub    $0x1,%rax
  409064:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  40906b:	00 
  40906c:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  409073:	00 
  409074:	4c 89 44 24 30       	mov    %r8,0x30(%rsp)
  409079:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  40907e:	be 28 00 00 00       	mov    $0x28,%esi
  409083:	ba 63 00 00 00       	mov    $0x63,%edx
  409088:	b9 0b 00 00 00       	mov    $0xb,%ecx
  40908d:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  409093:	e8 b8 15 00 00       	call   40a650 <runtime::bounds_check_error>
  409098:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40909d:	c6 84 04 a6 01 00 00 	movb   $0x2b,0x1a6(%rsp,%rax,1)
  4090a4:	2b 
  4090a5:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  4090ac:	00 
  4090ad:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4090b2:	48 89 e0             	mov    %rsp,%rax
  4090b5:	48 c7 00 81 00 00 00 	movq   $0x81,(%rax)
  4090bc:	bf d2 e8 40 00       	mov    $0x40e8d2,%edi
  4090c1:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4090c6:	be 28 00 00 00       	mov    $0x28,%esi
  4090cb:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4090d0:	ba 66 00 00 00       	mov    $0x66,%edx
  4090d5:	b9 0a 00 00 00       	mov    $0xa,%ecx
  4090da:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  4090e0:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4090e5:	e8 46 19 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  4090ea:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4090ef:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4090f4:	48 8b bc 24 10 01 00 	mov    0x110(%rsp),%rdi
  4090fb:	00 
  4090fc:	48 8b b4 24 08 01 00 	mov    0x108(%rsp),%rsi
  409103:	00 
  409104:	48 8d 8c 14 a6 01 00 	lea    0x1a6(%rsp,%rdx,1),%rcx
  40910b:	00 
  40910c:	48 29 d0             	sub    %rdx,%rax
  40910f:	48 89 8c 24 60 01 00 	mov    %rcx,0x160(%rsp)
  409116:	00 
  409117:	48 89 84 24 68 01 00 	mov    %rax,0x168(%rsp)
  40911e:	00 
  40911f:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  409126:	00 
  409127:	48 8b 8c 24 68 01 00 	mov    0x168(%rsp),%rcx
  40912e:	00 
  40912f:	48 89 8c 24 58 01 00 	mov    %rcx,0x158(%rsp)
  409136:	00 
  409137:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  40913e:	00 
  40913f:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  409146:	00 
  409147:	48 8b 8c 24 58 01 00 	mov    0x158(%rsp),%rcx
  40914e:	00 
  40914f:	e8 ac 2b 00 00       	call   40bd00 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  409154:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  409159:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40915e:	4c 8b 8c 24 58 01 00 	mov    0x158(%rsp),%r9
  409165:	00 
  409166:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40916b:	48 8b 8c 24 60 02 00 	mov    0x260(%rsp),%rcx
  409172:	00 
  409173:	48 89 e0             	mov    %rsp,%rax
  409176:	48 89 08             	mov    %rcx,(%rax)
  409179:	31 c0                	xor    %eax,%eax
  40917b:	41 89 c0             	mov    %eax,%r8d
  40917e:	ba 68 00 00 00       	mov    $0x68,%edx
  409183:	b9 13 00 00 00       	mov    $0x13,%ecx
  409188:	e8 a3 18 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  40918d:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  409192:	48 8b 8c 24 58 02 00 	mov    0x258(%rsp),%rcx
  409199:	00 
  40919a:	48 89 8c 24 40 01 00 	mov    %rcx,0x140(%rsp)
  4091a1:	00 
  4091a2:	48 89 84 24 48 01 00 	mov    %rax,0x148(%rsp)
  4091a9:	00 
  4091aa:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4091b1:	00 
  4091b2:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  4091b9:	00 
  4091ba:	48 89 8c 24 38 01 00 	mov    %rcx,0x138(%rsp)
  4091c1:	00 
  4091c2:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  4091c9:	00 
  4091ca:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4091d1:	00 
  4091d2:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  4091d9:	00 
  4091da:	48 81 c4 68 02 00 00 	add    $0x268,%rsp
  4091e1:	c3                   	ret
  4091e2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4091e9:	1f 84 00 00 00 00 00 

00000000004091f0 <strconv::write_int>:
  4091f0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4091f7:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  4091fc:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  409201:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  409206:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40920b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  409210:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  409215:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40921a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40921f:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  409224:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  409229:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  40922e:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  409235:	00 
  409236:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  40923b:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  409240:	49 c7 c1 58 30 41 00 	mov    $0x413058,%r9
  409247:	49 8b 01             	mov    (%r9),%rax
  40924a:	4d 8b 49 08          	mov    0x8(%r9),%r9
  40924e:	4c 89 4c 24 60       	mov    %r9,0x60(%rsp)
  409253:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  409258:	0f 10 44 24 58       	movups 0x58(%rsp),%xmm0
  40925d:	48 89 e0             	mov    %rsp,%rax
  409260:	0f 11 00             	movups %xmm0,(%rax)
  409263:	4c 89 40 18          	mov    %r8,0x18(%rax)
  409267:	c7 40 10 00 00 00 00 	movl   $0x0,0x10(%rax)
  40926e:	41 b8 01 00 00 00    	mov    $0x1,%r8d
  409274:	41 b9 40 00 00 00    	mov    $0x40,%r9d
  40927a:	e8 61 f7 ff ff       	call   4089e0 <strconv::write_bits>
  40927f:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  409284:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  409289:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40928e:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  409293:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40929a:	c3                   	ret
  40929b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004092a0 <runtime::default_random_generator_proc>:
  4092a0:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  4092a7:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4092ac:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4092b1:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4092b6:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4092bb:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4092c0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4092c5:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4092ca:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4092cf:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4092d4:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4092db:	00 
  4092dc:	48 89 b4 24 08 01 00 	mov    %rsi,0x108(%rsp)
  4092e3:	00 
  4092e4:	48 89 94 24 00 01 00 	mov    %rdx,0x100(%rsp)
  4092eb:	00 
  4092ec:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  4092f3:	00 
  4092f4:	48 c7 c2 48 fb ff ff 	mov    $0xfffffffffffffb48,%rdx
  4092fb:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  409302:	00 00 
  409304:	48 01 d1             	add    %rdx,%rcx
  409307:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  40930e:	00 
  40930f:	48 83 f8 00          	cmp    $0x0,%rax
  409313:	0f 95 c0             	setne  %al
  409316:	24 01                	and    $0x1,%al
  409318:	3c 00                	cmp    $0x0,%al
  40931a:	74 0d                	je     409329 <runtime::default_random_generator_proc+0x89>
  40931c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  409321:	48 89 84 24 f0 00 00 	mov    %rax,0xf0(%rsp)
  409328:	00 
  409329:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  409330:	00 
  409331:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  409336:	48 89 e0             	mov    %rsp,%rax
  409339:	48 c7 00 00 04 00 00 	movq   $0x400,(%rax)
  409340:	bf a0 e9 40 00       	mov    $0x40e9a0,%edi
  409345:	be 38 00 00 00       	mov    $0x38,%esi
  40934a:	ba 43 00 00 00       	mov    $0x43,%edx
  40934f:	b9 15 00 00 00       	mov    $0x15,%ecx
  409354:	41 b8 e0 03 00 00    	mov    $0x3e0,%r8d
  40935a:	41 b9 00 04 00 00    	mov    $0x400,%r9d
  409360:	e8 cb 16 00 00       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  409365:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40936a:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40936f:	48 81 c1 e0 03 00 00 	add    $0x3e0,%rcx
  409376:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  40937d:	00 
  40937e:	48 c7 84 24 e8 00 00 	movq   $0x20,0xe8(%rsp)
  409385:	00 20 00 00 00 
  40938a:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  409391:	00 
  409392:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  409399:	00 
  40939a:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  4093a1:	00 
  4093a2:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  4093a9:	00 
  4093aa:	48 85 c0             	test   %rax,%rax
  4093ad:	74 27                	je     4093d6 <runtime::default_random_generator_proc+0x136>
  4093af:	eb 00                	jmp    4093b1 <runtime::default_random_generator_proc+0x111>
  4093b1:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4093b6:	48 83 e8 01          	sub    $0x1,%rax
  4093ba:	0f 84 c5 03 00 00    	je     409785 <runtime::default_random_generator_proc+0x4e5>
  4093c0:	eb 00                	jmp    4093c2 <runtime::default_random_generator_proc+0x122>
  4093c2:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4093c7:	48 83 e8 02          	sub    $0x2,%rax
  4093cb:	0f 84 32 04 00 00    	je     409803 <runtime::default_random_generator_proc+0x563>
  4093d1:	e9 5d 04 00 00       	jmp    409833 <runtime::default_random_generator_proc+0x593>
  4093d6:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4093dd:	00 
  4093de:	80 b8 08 04 00 00 00 	cmpb   $0x0,0x408(%rax)
  4093e5:	75 37                	jne    40941e <runtime::default_random_generator_proc+0x17e>
  4093e7:	48 8b bc 24 d0 00 00 	mov    0xd0(%rsp),%rdi
  4093ee:	00 
  4093ef:	48 8b b4 24 d8 00 00 	mov    0xd8(%rsp),%rsi
  4093f6:	00 
  4093f7:	e8 24 d6 ff ff       	call   406a20 <runtime::rand_bytes>
  4093fc:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  409403:	00 
  409404:	48 c7 80 00 04 00 00 	movq   $0x3e0,0x400(%rax)
  40940b:	e0 03 00 00 
  40940f:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  409416:	00 
  409417:	c6 80 08 04 00 00 01 	movb   $0x1,0x408(%rax)
  40941e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  409423:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40942a:	00 
  40942b:	48 81 b8 00 04 00 00 	cmpq   $0x3e0,0x400(%rax)
  409432:	e0 03 00 00 
  409436:	0f 9e c0             	setle  %al
  409439:	24 01                	and    $0x1,%al
  40943b:	0f b6 f8             	movzbl %al,%edi
  40943e:	be d9 e9 40 00       	mov    $0x40e9d9,%esi
  409443:	b9 20 ea 40 00       	mov    $0x40ea20,%ecx
  409448:	ba 26 00 00 00       	mov    $0x26,%edx
  40944d:	e8 3e 2d 00 00       	call   40c190 <runtime::assert>
  409452:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  409459:	00 
  40945a:	48 81 b8 00 04 00 00 	cmpq   $0x3e0,0x400(%rax)
  409461:	e0 03 00 00 
  409465:	0f 9d c0             	setge  %al
  409468:	24 01                	and    $0x1,%al
  40946a:	3c 00                	cmp    $0x0,%al
  40946c:	74 12                	je     409480 <runtime::default_random_generator_proc+0x1e0>
  40946e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  409473:	48 8b bc 24 f0 00 00 	mov    0xf0(%rsp),%rdi
  40947a:	00 
  40947b:	e8 c0 03 00 00       	call   409840 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>
  409480:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  409485:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40948c:	00 
  40948d:	48 81 b8 00 04 00 00 	cmpq   $0x3d8,0x400(%rax)
  409494:	d8 03 00 00 
  409498:	0f 9e c0             	setle  %al
  40949b:	24 01                	and    $0x1,%al
  40949d:	0f b6 f8             	movzbl %al,%edi
  4094a0:	be 48 ea 40 00       	mov    $0x40ea48,%esi
  4094a5:	b9 80 ea 40 00       	mov    $0x40ea80,%ecx
  4094aa:	ba 36 00 00 00       	mov    $0x36,%edx
  4094af:	e8 dc 2c 00 00       	call   40c190 <runtime::assert>
  4094b4:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4094b9:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4094c0:	00 
  4094c1:	48 8b 80 00 04 00 00 	mov    0x400(%rax),%rax
  4094c8:	b9 08 00 00 00       	mov    $0x8,%ecx
  4094cd:	48 99                	cqto
  4094cf:	48 f7 f9             	idiv   %rcx
  4094d2:	48 83 fa 00          	cmp    $0x0,%rdx
  4094d6:	0f 94 c0             	sete   %al
  4094d9:	24 01                	and    $0x1,%al
  4094db:	0f b6 f8             	movzbl %al,%edi
  4094de:	be a8 ea 40 00       	mov    $0x40eaa8,%esi
  4094e3:	b9 f0 ea 40 00       	mov    $0x40eaf0,%ecx
  4094e8:	ba 3d 00 00 00       	mov    $0x3d,%edx
  4094ed:	e8 9e 2c 00 00       	call   40c190 <runtime::assert>
  4094f2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4094f7:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  4094fe:	00 
  4094ff:	48 83 bc 24 c8 00 00 	cmpq   $0x8,0xc8(%rsp)
  409506:	00 08 
  409508:	0f 94 c0             	sete   %al
  40950b:	24 01                	and    $0x1,%al
  40950d:	3c 00                	cmp    $0x0,%al
  40950f:	0f 84 86 00 00 00    	je     40959b <runtime::default_random_generator_proc+0x2fb>
  409515:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40951a:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  409521:	00 
  409522:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  409529:	00 
  40952a:	48 8b b1 00 04 00 00 	mov    0x400(%rcx),%rsi
  409531:	48 01 f2             	add    %rsi,%rdx
  409534:	b9 00 04 00 00       	mov    $0x400,%ecx
  409539:	48 29 f1             	sub    %rsi,%rcx
  40953c:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  409543:	00 
  409544:	48 89 8c 24 c0 00 00 	mov    %rcx,0xc0(%rsp)
  40954b:	00 
  40954c:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  409553:	00 
  409554:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  40955b:	00 
  40955c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  409563:	00 
  409564:	48 8b 09             	mov    (%rcx),%rcx
  409567:	48 89 08             	mov    %rcx,(%rax)
  40956a:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  409571:	00 
  409572:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  409579:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  409580:	00 
  409581:	48 8b 88 00 04 00 00 	mov    0x400(%rax),%rcx
  409588:	48 83 c1 08          	add    $0x8,%rcx
  40958c:	48 89 88 00 04 00 00 	mov    %rcx,0x400(%rax)
  409593:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40959a:	c3                   	ret
  40959b:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4095a0:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4095a5:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  4095ac:	00 
  4095ad:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4095b4:	00 
  4095b5:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  4095bc:	00 
  4095bd:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  4095c4:	00 
  4095c5:	48 83 bc 24 98 00 00 	cmpq   $0x0,0x98(%rsp)
  4095cc:	00 00 
  4095ce:	0f 9f c0             	setg   %al
  4095d1:	24 01                	and    $0x1,%al
  4095d3:	3c 00                	cmp    $0x0,%al
  4095d5:	0f 84 a5 01 00 00    	je     409780 <runtime::default_random_generator_proc+0x4e0>
  4095db:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4095e2:	00 
  4095e3:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4095ea:	00 
  4095eb:	48 8b 90 00 04 00 00 	mov    0x400(%rax),%rdx
  4095f2:	b8 e0 03 00 00       	mov    $0x3e0,%eax
  4095f7:	48 29 d0             	sub    %rdx,%rax
  4095fa:	48 89 ca             	mov    %rcx,%rdx
  4095fd:	48 29 c2             	sub    %rax,%rdx
  409600:	48 0f 4c c1          	cmovl  %rcx,%rax
  409604:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40960b:	00 
  40960c:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  409613:	00 
  409614:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  40961b:	00 
  40961c:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  409623:	00 
  409624:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  40962b:	00 
  40962c:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  409633:	00 
  409634:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  40963b:	00 
  40963c:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  409643:	00 
  409644:	48 8b 91 00 04 00 00 	mov    0x400(%rcx),%rdx
  40964b:	48 01 d1             	add    %rdx,%rcx
  40964e:	b8 00 04 00 00       	mov    $0x400,%eax
  409653:	48 29 d0             	sub    %rdx,%rax
  409656:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40965b:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  409660:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  409665:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  40966a:	e8 91 26 00 00       	call   40bd00 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  40966f:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  409676:	00 
  409677:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  40967e:	00 
  40967f:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409686:	00 
  409687:	48 01 d1             	add    %rdx,%rcx
  40968a:	48 29 d0             	sub    %rdx,%rax
  40968d:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  409692:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  409697:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40969c:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4096a1:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  4096a8:	00 
  4096a9:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4096b0:	00 
  4096b1:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  4096b8:	00 
  4096b9:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  4096c0:	00 
  4096c1:	48 29 c8             	sub    %rcx,%rax
  4096c4:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  4096cb:	00 
  4096cc:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  4096d3:	00 
  4096d4:	48 83 c0 07          	add    $0x7,%rax
  4096d8:	b9 08 00 00 00       	mov    $0x8,%ecx
  4096dd:	48 99                	cqto
  4096df:	48 f7 f9             	idiv   %rcx
  4096e2:	48 c1 e0 03          	shl    $0x3,%rax
  4096e6:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4096eb:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4096f2:	00 
  4096f3:	48 8b 80 00 04 00 00 	mov    0x400(%rax),%rax
  4096fa:	48 03 44 24 58       	add    0x58(%rsp),%rax
  4096ff:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  409704:	48 81 7c 24 50 e0 03 	cmpq   $0x3e0,0x50(%rsp)
  40970b:	00 00 
  40970d:	0f 9c c0             	setl   %al
  409710:	24 01                	and    $0x1,%al
  409712:	3c 00                	cmp    $0x0,%al
  409714:	74 53                	je     409769 <runtime::default_random_generator_proc+0x4c9>
  409716:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  40971d:	00 
  40971e:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  409725:	00 
  409726:	48 8b 90 00 04 00 00 	mov    0x400(%rax),%rdx
  40972d:	48 01 d1             	add    %rdx,%rcx
  409730:	b8 00 04 00 00       	mov    $0x400,%eax
  409735:	48 29 d0             	sub    %rdx,%rax
  409738:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40973d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  409742:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  409747:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  40974c:	31 f6                	xor    %esi,%esi
  40974e:	e8 ed 78 ff ff       	call   401040 <memset@plt>
  409753:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40975a:	00 
  40975b:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  409760:	48 89 88 00 04 00 00 	mov    %rcx,0x400(%rax)
  409767:	eb 12                	jmp    40977b <runtime::default_random_generator_proc+0x4db>
  409769:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40976e:	48 8b bc 24 f0 00 00 	mov    0xf0(%rsp),%rdi
  409775:	00 
  409776:	e8 c5 00 00 00       	call   409840 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>
  40977b:	e9 45 fe ff ff       	jmp    4095c5 <runtime::default_random_generator_proc+0x325>
  409780:	e9 ae 00 00 00       	jmp    409833 <runtime::default_random_generator_proc+0x593>
  409785:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40978a:	48 83 f8 00          	cmp    $0x0,%rax
  40978e:	0f 94 c0             	sete   %al
  409791:	24 01                	and    $0x1,%al
  409793:	3c 00                	cmp    $0x0,%al
  409795:	74 17                	je     4097ae <runtime::default_random_generator_proc+0x50e>
  409797:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40979e:	00 
  40979f:	c6 80 08 04 00 00 00 	movb   $0x0,0x408(%rax)
  4097a6:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4097ad:	c3                   	ret
  4097ae:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4097b3:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4097b8:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  4097bf:	00 
  4097c0:	0f 57 c0             	xorps  %xmm0,%xmm0
  4097c3:	0f 11 40 10          	movups %xmm0,0x10(%rax)
  4097c7:	0f 11 00             	movups %xmm0,(%rax)
  4097ca:	48 8b bc 24 d0 00 00 	mov    0xd0(%rsp),%rdi
  4097d1:	00 
  4097d2:	48 8b b4 24 d8 00 00 	mov    0xd8(%rsp),%rsi
  4097d9:	00 
  4097da:	e8 21 25 00 00       	call   40bd00 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  4097df:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4097e6:	00 
  4097e7:	c6 80 08 04 00 00 01 	movb   $0x1,0x408(%rax)
  4097ee:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4097f5:	00 
  4097f6:	48 c7 80 00 04 00 00 	movq   $0x3e0,0x400(%rax)
  4097fd:	e0 03 00 00 
  409801:	eb 30                	jmp    409833 <runtime::default_random_generator_proc+0x593>
  409803:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  409808:	48 83 f8 04          	cmp    $0x4,%rax
  40980c:	0f 95 c0             	setne  %al
  40980f:	24 01                	and    $0x1,%al
  409811:	3c 00                	cmp    $0x0,%al
  409813:	74 08                	je     40981d <runtime::default_random_generator_proc+0x57d>
  409815:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40981c:	c3                   	ret
  40981d:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  409822:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  409827:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40982c:	8b 08                	mov    (%rax),%ecx
  40982e:	83 c9 0b             	or     $0xb,%ecx
  409831:	89 08                	mov    %ecx,(%rax)
  409833:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40983a:	c3                   	ret
  40983b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000409840 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>:
  409840:	48 83 ec 18          	sub    $0x18,%rsp
  409844:	48 89 3c 24          	mov    %rdi,(%rsp)
  409848:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40984d:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  409852:	48 8b 04 24          	mov    (%rsp),%rax
  409856:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40985b:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  409860:	80 b8 08 04 00 00 01 	cmpb   $0x1,0x408(%rax)
  409867:	0f 94 c0             	sete   %al
  40986a:	24 01                	and    $0x1,%al
  40986c:	0f b6 f8             	movzbl %al,%edi
  40986f:	be 18 eb 40 00       	mov    $0x40eb18,%esi
  409874:	b9 50 eb 40 00       	mov    $0x40eb50,%ecx
  409879:	ba 20 00 00 00       	mov    $0x20,%edx
  40987e:	e8 0d 29 00 00       	call   40c190 <runtime::assert>
  409883:	48 8b 3c 24          	mov    (%rsp),%rdi
  409887:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40988c:	e8 0f e1 ff ff       	call   4079a0 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128>
  409891:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  409896:	48 c7 80 00 04 00 00 	movq   $0x0,0x400(%rax)
  40989d:	00 00 00 00 
  4098a1:	48 83 c4 18          	add    $0x18,%rsp
  4098a5:	c3                   	ret
  4098a6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4098ad:	00 00 00 

00000000004098b0 <runtime::udivmod128>:
  4098b0:	48 81 ec b8 01 00 00 	sub    $0x1b8,%rsp
  4098b7:	4c 89 84 24 a8 00 00 	mov    %r8,0xa8(%rsp)
  4098be:	00 
  4098bf:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  4098c6:	00 
  4098c7:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  4098ce:	00 
  4098cf:	48 89 b4 24 c0 00 00 	mov    %rsi,0xc0(%rsp)
  4098d6:	00 
  4098d7:	48 89 bc 24 c8 00 00 	mov    %rdi,0xc8(%rsp)
  4098de:	00 
  4098df:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  4098e6:	00 
  4098e7:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4098ee:	00 
  4098ef:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  4098f6:	00 
  4098f7:	48 8b b4 24 c0 00 00 	mov    0xc0(%rsp),%rsi
  4098fe:	00 
  4098ff:	48 8b bc 24 a8 00 00 	mov    0xa8(%rsp),%rdi
  409906:	00 
  409907:	48 89 94 24 a0 01 00 	mov    %rdx,0x1a0(%rsp)
  40990e:	00 
  40990f:	48 89 b4 24 a8 01 00 	mov    %rsi,0x1a8(%rsp)
  409916:	00 
  409917:	48 89 8c 24 98 01 00 	mov    %rcx,0x198(%rsp)
  40991e:	00 
  40991f:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  409926:	00 
  409927:	48 89 bc 24 88 01 00 	mov    %rdi,0x188(%rsp)
  40992e:	00 
  40992f:	48 89 b4 24 78 01 00 	mov    %rsi,0x178(%rsp)
  409936:	00 
  409937:	48 89 94 24 70 01 00 	mov    %rdx,0x170(%rsp)
  40993e:	00 
  40993f:	48 8b 94 24 70 01 00 	mov    0x170(%rsp),%rdx
  409946:	00 
  409947:	48 8b b4 24 78 01 00 	mov    0x178(%rsp),%rsi
  40994e:	00 
  40994f:	48 89 b4 24 68 01 00 	mov    %rsi,0x168(%rsp)
  409956:	00 
  409957:	48 89 94 24 60 01 00 	mov    %rdx,0x160(%rsp)
  40995e:	00 
  40995f:	48 89 8c 24 58 01 00 	mov    %rcx,0x158(%rsp)
  409966:	00 
  409967:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  40996e:	00 
  40996f:	48 8b 84 24 50 01 00 	mov    0x150(%rsp),%rax
  409976:	00 
  409977:	48 8b 8c 24 58 01 00 	mov    0x158(%rsp),%rcx
  40997e:	00 
  40997f:	48 89 8c 24 48 01 00 	mov    %rcx,0x148(%rsp)
  409986:	00 
  409987:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40998e:	00 
  40998f:	48 8d bc 24 30 01 00 	lea    0x130(%rsp),%rdi
  409996:	00 
  409997:	31 f6                	xor    %esi,%esi
  409999:	ba 10 00 00 00       	mov    $0x10,%edx
  40999e:	e8 9d 76 ff ff       	call   401040 <memset@plt>
  4099a3:	48 8d bc 24 20 01 00 	lea    0x120(%rsp),%rdi
  4099aa:	00 
  4099ab:	31 f6                	xor    %esi,%esi
  4099ad:	ba 10 00 00 00       	mov    $0x10,%edx
  4099b2:	e8 89 76 ff ff       	call   401040 <memset@plt>
  4099b7:	c7 84 24 1c 01 00 00 	movl   $0x0,0x11c(%rsp)
  4099be:	00 00 00 00 
  4099c2:	48 83 bc 24 68 01 00 	cmpq   $0x0,0x168(%rsp)
  4099c9:	00 00 
  4099cb:	0f 94 c0             	sete   %al
  4099ce:	24 01                	and    $0x1,%al
  4099d0:	3c 00                	cmp    $0x0,%al
  4099d2:	0f 84 31 01 00 00    	je     409b09 <runtime::udivmod128+0x259>
  4099d8:	48 83 bc 24 48 01 00 	cmpq   $0x0,0x148(%rsp)
  4099df:	00 00 
  4099e1:	0f 94 c0             	sete   %al
  4099e4:	24 01                	and    $0x1,%al
  4099e6:	3c 00                	cmp    $0x0,%al
  4099e8:	0f 84 dc 00 00 00    	je     409aca <runtime::udivmod128+0x21a>
  4099ee:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4099f5:	00 
  4099f6:	48 83 f8 00          	cmp    $0x0,%rax
  4099fa:	0f 95 c0             	setne  %al
  4099fd:	24 01                	and    $0x1,%al
  4099ff:	3c 00                	cmp    $0x0,%al
  409a01:	74 72                	je     409a75 <runtime::udivmod128+0x1c5>
  409a03:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  409a0a:	00 
  409a0b:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  409a12:	00 
  409a13:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  409a1a:	00 
  409a1b:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  409a22:	00 
  409a23:	48 83 f8 00          	cmp    $0x0,%rax
  409a27:	74 1f                	je     409a48 <runtime::udivmod128+0x198>
  409a29:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  409a30:	00 
  409a31:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  409a38:	00 
  409a39:	31 d2                	xor    %edx,%edx
  409a3b:	48 f7 f1             	div    %rcx
  409a3e:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  409a45:	00 
  409a46:	eb 02                	jmp    409a4a <runtime::udivmod128+0x19a>
  409a48:	0f 0b                	ud2
  409a4a:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409a51:	00 
  409a52:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  409a59:	00 
  409a5a:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  409a61:	00 
  409a62:	48 8b 8c 24 10 01 00 	mov    0x110(%rsp),%rcx
  409a69:	00 
  409a6a:	48 89 08             	mov    %rcx,(%rax)
  409a6d:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  409a74:	00 
  409a75:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  409a7c:	00 
  409a7d:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  409a84:	00 
  409a85:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  409a8c:	00 
  409a8d:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  409a94:	00 
  409a95:	48 83 f8 00          	cmp    $0x0,%rax
  409a99:	74 1c                	je     409ab7 <runtime::udivmod128+0x207>
  409a9b:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  409aa2:	00 
  409aa3:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  409aaa:	00 
  409aab:	31 d2                	xor    %edx,%edx
  409aad:	48 f7 f1             	div    %rcx
  409ab0:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  409ab5:	eb 02                	jmp    409ab9 <runtime::udivmod128+0x209>
  409ab7:	0f 0b                	ud2
  409ab9:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  409abe:	31 c9                	xor    %ecx,%ecx
  409ac0:	89 ca                	mov    %ecx,%edx
  409ac2:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  409ac9:	c3                   	ret
  409aca:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409ad1:	00 
  409ad2:	48 83 f8 00          	cmp    $0x0,%rax
  409ad6:	0f 95 c0             	setne  %al
  409ad9:	24 01                	and    $0x1,%al
  409adb:	3c 00                	cmp    $0x0,%al
  409add:	74 1b                	je     409afa <runtime::udivmod128+0x24a>
  409adf:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409ae6:	00 
  409ae7:	48 8b 8c 24 60 01 00 	mov    0x160(%rsp),%rcx
  409aee:	00 
  409aef:	48 89 08             	mov    %rcx,(%rax)
  409af2:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  409af9:	00 
  409afa:	31 c0                	xor    %eax,%eax
  409afc:	89 c2                	mov    %eax,%edx
  409afe:	48 89 d0             	mov    %rdx,%rax
  409b01:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  409b08:	c3                   	ret
  409b09:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  409b10:	00 00 
  409b12:	0f 94 c0             	sete   %al
  409b15:	24 01                	and    $0x1,%al
  409b17:	3c 00                	cmp    $0x0,%al
  409b19:	0f 84 df 03 00 00    	je     409efe <runtime::udivmod128+0x64e>
  409b1f:	48 83 bc 24 48 01 00 	cmpq   $0x0,0x148(%rsp)
  409b26:	00 00 
  409b28:	0f 94 c0             	sete   %al
  409b2b:	24 01                	and    $0x1,%al
  409b2d:	3c 00                	cmp    $0x0,%al
  409b2f:	0f 84 ae 00 00 00    	je     409be3 <runtime::udivmod128+0x333>
  409b35:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409b3c:	00 
  409b3d:	48 83 f8 00          	cmp    $0x0,%rax
  409b41:	0f 95 c0             	setne  %al
  409b44:	24 01                	and    $0x1,%al
  409b46:	3c 00                	cmp    $0x0,%al
  409b48:	74 50                	je     409b9a <runtime::udivmod128+0x2ea>
  409b4a:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  409b51:	00 
  409b52:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  409b57:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  409b5e:	00 
  409b5f:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  409b64:	48 83 f8 00          	cmp    $0x0,%rax
  409b68:	74 16                	je     409b80 <runtime::udivmod128+0x2d0>
  409b6a:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  409b6f:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  409b74:	31 d2                	xor    %edx,%edx
  409b76:	48 f7 f1             	div    %rcx
  409b79:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  409b7e:	eb 02                	jmp    409b82 <runtime::udivmod128+0x2d2>
  409b80:	0f 0b                	ud2
  409b82:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409b89:	00 
  409b8a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  409b8f:	48 89 08             	mov    %rcx,(%rax)
  409b92:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  409b99:	00 
  409b9a:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  409ba1:	00 
  409ba2:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  409ba7:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  409bae:	00 
  409baf:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  409bb4:	48 83 f8 00          	cmp    $0x0,%rax
  409bb8:	74 16                	je     409bd0 <runtime::udivmod128+0x320>
  409bba:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  409bbf:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  409bc4:	31 d2                	xor    %edx,%edx
  409bc6:	48 f7 f1             	div    %rcx
  409bc9:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  409bce:	eb 02                	jmp    409bd2 <runtime::udivmod128+0x322>
  409bd0:	0f 0b                	ud2
  409bd2:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  409bd7:	31 c9                	xor    %ecx,%ecx
  409bd9:	89 ca                	mov    %ecx,%edx
  409bdb:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  409be2:	c3                   	ret
  409be3:	48 83 bc 24 60 01 00 	cmpq   $0x0,0x160(%rsp)
  409bea:	00 00 
  409bec:	0f 94 c0             	sete   %al
  409bef:	24 01                	and    $0x1,%al
  409bf1:	3c 00                	cmp    $0x0,%al
  409bf3:	0f 84 e3 00 00 00    	je     409cdc <runtime::udivmod128+0x42c>
  409bf9:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409c00:	00 
  409c01:	48 83 f8 00          	cmp    $0x0,%rax
  409c05:	0f 95 c0             	setne  %al
  409c08:	24 01                	and    $0x1,%al
  409c0a:	3c 00                	cmp    $0x0,%al
  409c0c:	0f 84 81 00 00 00    	je     409c93 <runtime::udivmod128+0x3e3>
  409c12:	48 8d 84 24 20 01 00 	lea    0x120(%rsp),%rax
  409c19:	00 
  409c1a:	48 83 c0 08          	add    $0x8,%rax
  409c1e:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  409c23:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  409c2a:	00 
  409c2b:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  409c30:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  409c37:	00 
  409c38:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  409c3d:	48 83 f8 00          	cmp    $0x0,%rax
  409c41:	74 16                	je     409c59 <runtime::udivmod128+0x3a9>
  409c43:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  409c48:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  409c4d:	31 d2                	xor    %edx,%edx
  409c4f:	48 f7 f1             	div    %rcx
  409c52:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  409c57:	eb 02                	jmp    409c5b <runtime::udivmod128+0x3ab>
  409c59:	0f 0b                	ud2
  409c5b:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409c62:	00 
  409c63:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  409c68:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  409c6d:	48 89 11             	mov    %rdx,(%rcx)
  409c70:	48 c7 84 24 20 01 00 	movq   $0x0,0x120(%rsp)
  409c77:	00 00 00 00 00 
  409c7c:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  409c83:	00 
  409c84:	48 8b 94 24 28 01 00 	mov    0x128(%rsp),%rdx
  409c8b:	00 
  409c8c:	48 89 50 08          	mov    %rdx,0x8(%rax)
  409c90:	48 89 08             	mov    %rcx,(%rax)
  409c93:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  409c9a:	00 
  409c9b:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  409ca0:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  409ca7:	00 
  409ca8:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  409cad:	48 83 f8 00          	cmp    $0x0,%rax
  409cb1:	74 16                	je     409cc9 <runtime::udivmod128+0x419>
  409cb3:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  409cb8:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  409cbd:	31 d2                	xor    %edx,%edx
  409cbf:	48 f7 f1             	div    %rcx
  409cc2:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  409cc7:	eb 02                	jmp    409ccb <runtime::udivmod128+0x41b>
  409cc9:	0f 0b                	ud2
  409ccb:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  409cd0:	31 c9                	xor    %ecx,%ecx
  409cd2:	89 ca                	mov    %ecx,%edx
  409cd4:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  409cdb:	c3                   	ret
  409cdc:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  409ce3:	00 
  409ce4:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  409ceb:	00 
  409cec:	48 83 e9 01          	sub    $0x1,%rcx
  409cf0:	48 21 c8             	and    %rcx,%rax
  409cf3:	48 83 f8 00          	cmp    $0x0,%rax
  409cf7:	0f 94 c0             	sete   %al
  409cfa:	24 01                	and    $0x1,%al
  409cfc:	3c 00                	cmp    $0x0,%al
  409cfe:	0f 84 9b 00 00 00    	je     409d9f <runtime::udivmod128+0x4ef>
  409d04:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409d0b:	00 
  409d0c:	48 83 f8 00          	cmp    $0x0,%rax
  409d10:	0f 95 c0             	setne  %al
  409d13:	24 01                	and    $0x1,%al
  409d15:	3c 00                	cmp    $0x0,%al
  409d17:	74 4d                	je     409d66 <runtime::udivmod128+0x4b6>
  409d19:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409d20:	00 
  409d21:	48 8b 8c 24 60 01 00 	mov    0x160(%rsp),%rcx
  409d28:	00 
  409d29:	48 89 8c 24 20 01 00 	mov    %rcx,0x120(%rsp)
  409d30:	00 
  409d31:	48 8b 8c 24 68 01 00 	mov    0x168(%rsp),%rcx
  409d38:	00 
  409d39:	48 8b 94 24 48 01 00 	mov    0x148(%rsp),%rdx
  409d40:	00 
  409d41:	48 ff ca             	dec    %rdx
  409d44:	48 21 d1             	and    %rdx,%rcx
  409d47:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  409d4e:	00 
  409d4f:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  409d56:	00 
  409d57:	48 8b 94 24 28 01 00 	mov    0x128(%rsp),%rdx
  409d5e:	00 
  409d5f:	48 89 50 08          	mov    %rdx,0x8(%rax)
  409d63:	48 89 08             	mov    %rcx,(%rax)
  409d66:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  409d6d:	00 
  409d6e:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  409d75:	00 
  409d76:	ba 40 00 00 00       	mov    $0x40,%edx
  409d7b:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  409d80:	88 d1                	mov    %dl,%cl
  409d82:	48 d3 e8             	shr    %cl,%rax
  409d85:	48 89 c1             	mov    %rax,%rcx
  409d88:	31 c0                	xor    %eax,%eax
  409d8a:	48 83 ea 40          	sub    $0x40,%rdx
  409d8e:	89 c2                	mov    %eax,%edx
  409d90:	48 89 d0             	mov    %rdx,%rax
  409d93:	48 0f 42 c1          	cmovb  %rcx,%rax
  409d97:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  409d9e:	c3                   	ret
  409d9f:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  409da6:	00 
  409da7:	b8 7f 00 00 00       	mov    $0x7f,%eax
  409dac:	48 0f bd c1          	bsr    %rcx,%rax
  409db0:	48 83 f0 3f          	xor    $0x3f,%rax
  409db4:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  409dbb:	00 
  409dbc:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  409dc1:	48 0f bd ca          	bsr    %rdx,%rcx
  409dc5:	48 83 f1 3f          	xor    $0x3f,%rcx
  409dc9:	29 c8                	sub    %ecx,%eax
  409dcb:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  409dd2:	83 bc 24 1c 01 00 00 	cmpl   $0x3e,0x11c(%rsp)
  409dd9:	3e 
  409dda:	0f 97 c0             	seta   %al
  409ddd:	24 01                	and    $0x1,%al
  409ddf:	3c 00                	cmp    $0x0,%al
  409de1:	74 43                	je     409e26 <runtime::udivmod128+0x576>
  409de3:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409dea:	00 
  409deb:	48 83 f8 00          	cmp    $0x0,%rax
  409def:	0f 95 c0             	setne  %al
  409df2:	24 01                	and    $0x1,%al
  409df4:	3c 00                	cmp    $0x0,%al
  409df6:	74 1f                	je     409e17 <runtime::udivmod128+0x567>
  409df8:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409dff:	00 
  409e00:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  409e07:	00 
  409e08:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  409e0f:	00 
  409e10:	48 89 10             	mov    %rdx,(%rax)
  409e13:	48 89 48 08          	mov    %rcx,0x8(%rax)
  409e17:	31 c0                	xor    %eax,%eax
  409e19:	89 c2                	mov    %eax,%edx
  409e1b:	48 89 d0             	mov    %rdx,%rax
  409e1e:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  409e25:	c3                   	ret
  409e26:	8b 84 24 1c 01 00 00 	mov    0x11c(%rsp),%eax
  409e2d:	83 c0 01             	add    $0x1,%eax
  409e30:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  409e37:	48 c7 84 24 30 01 00 	movq   $0x0,0x130(%rsp)
  409e3e:	00 00 00 00 00 
  409e43:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  409e4a:	00 
  409e4b:	b9 40 00 00 00       	mov    $0x40,%ecx
  409e50:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  409e57:	89 c9                	mov    %ecx,%ecx
  409e59:	89 ca                	mov    %ecx,%edx
  409e5b:	48 89 d1             	mov    %rdx,%rcx
  409e5e:	48 d3 e0             	shl    %cl,%rax
  409e61:	48 89 c1             	mov    %rax,%rcx
  409e64:	31 c0                	xor    %eax,%eax
  409e66:	48 83 fa 40          	cmp    $0x40,%rdx
  409e6a:	48 0f 42 c1          	cmovb  %rcx,%rax
  409e6e:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  409e75:	00 
  409e76:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  409e7d:	00 
  409e7e:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  409e85:	89 ca                	mov    %ecx,%edx
  409e87:	48 89 d1             	mov    %rdx,%rcx
  409e8a:	48 d3 e8             	shr    %cl,%rax
  409e8d:	48 89 c1             	mov    %rax,%rcx
  409e90:	31 c0                	xor    %eax,%eax
  409e92:	48 83 fa 40          	cmp    $0x40,%rdx
  409e96:	48 0f 42 c1          	cmovb  %rcx,%rax
  409e9a:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  409ea1:	00 
  409ea2:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  409ea9:	00 
  409eaa:	b9 40 00 00 00       	mov    $0x40,%ecx
  409eaf:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  409eb6:	89 c9                	mov    %ecx,%ecx
  409eb8:	89 ca                	mov    %ecx,%edx
  409eba:	48 89 d1             	mov    %rdx,%rcx
  409ebd:	48 d3 e0             	shl    %cl,%rax
  409ec0:	48 89 c1             	mov    %rax,%rcx
  409ec3:	31 c0                	xor    %eax,%eax
  409ec5:	48 83 fa 40          	cmp    $0x40,%rdx
  409ec9:	48 0f 42 c1          	cmovb  %rcx,%rax
  409ecd:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  409ed4:	00 
  409ed5:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  409edc:	89 ce                	mov    %ecx,%esi
  409ede:	48 89 f1             	mov    %rsi,%rcx
  409ee1:	48 d3 ea             	shr    %cl,%rdx
  409ee4:	31 c9                	xor    %ecx,%ecx
  409ee6:	48 83 fe 40          	cmp    $0x40,%rsi
  409eea:	48 0f 42 ca          	cmovb  %rdx,%rcx
  409eee:	48 09 c8             	or     %rcx,%rax
  409ef1:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  409ef8:	00 
  409ef9:	e9 2c 05 00 00       	jmp    40a42a <runtime::udivmod128+0xb7a>
  409efe:	48 83 bc 24 48 01 00 	cmpq   $0x0,0x148(%rsp)
  409f05:	00 00 
  409f07:	0f 94 c0             	sete   %al
  409f0a:	24 01                	and    $0x1,%al
  409f0c:	3c 00                	cmp    $0x0,%al
  409f0e:	0f 84 76 03 00 00    	je     40a28a <runtime::udivmod128+0x9da>
  409f14:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  409f1b:	00 
  409f1c:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  409f23:	00 
  409f24:	48 83 e9 01          	sub    $0x1,%rcx
  409f28:	48 21 c8             	and    %rcx,%rax
  409f2b:	48 83 f8 00          	cmp    $0x0,%rax
  409f2f:	0f 94 c0             	sete   %al
  409f32:	24 01                	and    $0x1,%al
  409f34:	3c 00                	cmp    $0x0,%al
  409f36:	0f 84 14 01 00 00    	je     40a050 <runtime::udivmod128+0x7a0>
  409f3c:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409f43:	00 
  409f44:	48 83 f8 00          	cmp    $0x0,%rax
  409f48:	0f 95 c0             	setne  %al
  409f4b:	24 01                	and    $0x1,%al
  409f4d:	3c 00                	cmp    $0x0,%al
  409f4f:	74 29                	je     409f7a <runtime::udivmod128+0x6ca>
  409f51:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409f58:	00 
  409f59:	48 8b 8c 24 60 01 00 	mov    0x160(%rsp),%rcx
  409f60:	00 
  409f61:	48 8b 94 24 40 01 00 	mov    0x140(%rsp),%rdx
  409f68:	00 
  409f69:	48 ff ca             	dec    %rdx
  409f6c:	48 21 d1             	and    %rdx,%rcx
  409f6f:	48 89 08             	mov    %rcx,(%rax)
  409f72:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  409f79:	00 
  409f7a:	48 83 bc 24 40 01 00 	cmpq   $0x1,0x140(%rsp)
  409f81:	00 01 
  409f83:	0f 94 c0             	sete   %al
  409f86:	24 01                	and    $0x1,%al
  409f88:	3c 00                	cmp    $0x0,%al
  409f8a:	74 18                	je     409fa4 <runtime::udivmod128+0x6f4>
  409f8c:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  409f93:	00 
  409f94:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  409f9b:	00 
  409f9c:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  409fa3:	c3                   	ret
  409fa4:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  409fab:	00 
  409fac:	b8 40 00 00 00       	mov    $0x40,%eax
  409fb1:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  409fb6:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  409fbd:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  409fc4:	00 
  409fc5:	8b 84 24 1c 01 00 00 	mov    0x11c(%rsp),%eax
  409fcc:	89 44 24 04          	mov    %eax,0x4(%rsp)
  409fd0:	88 c1                	mov    %al,%cl
  409fd2:	48 d3 ea             	shr    %cl,%rdx
  409fd5:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  409fd9:	31 c0                	xor    %eax,%eax
  409fdb:	83 e9 40             	sub    $0x40,%ecx
  409fde:	48 89 c1             	mov    %rax,%rcx
  409fe1:	48 0f 42 ca          	cmovb  %rdx,%rcx
  409fe5:	48 89 8c 24 38 01 00 	mov    %rcx,0x138(%rsp)
  409fec:	00 
  409fed:	48 8b bc 24 60 01 00 	mov    0x160(%rsp),%rdi
  409ff4:	00 
  409ff5:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  409ffc:	00 
  409ffd:	8b b4 24 1c 01 00 00 	mov    0x11c(%rsp),%esi
  40a004:	40 88 f1             	mov    %sil,%cl
  40a007:	48 d3 ef             	shr    %cl,%rdi
  40a00a:	83 ee 40             	sub    $0x40,%esi
  40a00d:	48 89 c1             	mov    %rax,%rcx
  40a010:	48 0f 42 cf          	cmovb  %rdi,%rcx
  40a014:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40a019:	f7 de                	neg    %esi
  40a01b:	40 88 f1             	mov    %sil,%cl
  40a01e:	48 d3 e2             	shl    %cl,%rdx
  40a021:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40a026:	83 ee 40             	sub    $0x40,%esi
  40a029:	48 0f 42 c2          	cmovb  %rdx,%rax
  40a02d:	48 09 c8             	or     %rcx,%rax
  40a030:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  40a037:	00 
  40a038:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40a03f:	00 
  40a040:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  40a047:	00 
  40a048:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  40a04f:	c3                   	ret
  40a050:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  40a057:	00 
  40a058:	b8 7f 00 00 00       	mov    $0x7f,%eax
  40a05d:	48 0f bd c1          	bsr    %rcx,%rax
  40a061:	48 83 f0 3f          	xor    $0x3f,%rax
  40a065:	83 c0 41             	add    $0x41,%eax
  40a068:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  40a06f:	00 
  40a070:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40a075:	48 0f bd ca          	bsr    %rdx,%rcx
  40a079:	48 83 f1 3f          	xor    $0x3f,%rcx
  40a07d:	29 c8                	sub    %ecx,%eax
  40a07f:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  40a086:	83 bc 24 1c 01 00 00 	cmpl   $0x40,0x11c(%rsp)
  40a08d:	40 
  40a08e:	0f 94 c1             	sete   %cl
  40a091:	80 e1 01             	and    $0x1,%cl
  40a094:	b0 01                	mov    $0x1,%al
  40a096:	38 c8                	cmp    %cl,%al
  40a098:	74 16                	je     40a0b0 <runtime::udivmod128+0x800>
  40a09a:	83 bc 24 1c 01 00 00 	cmpl   $0x40,0x11c(%rsp)
  40a0a1:	40 
  40a0a2:	0f 92 c1             	setb   %cl
  40a0a5:	80 e1 01             	and    $0x1,%cl
  40a0a8:	b0 01                	mov    $0x1,%al
  40a0aa:	38 c8                	cmp    %cl,%al
  40a0ac:	74 44                	je     40a0f2 <runtime::udivmod128+0x842>
  40a0ae:	eb 3d                	jmp    40a0ed <runtime::udivmod128+0x83d>
  40a0b0:	48 c7 84 24 30 01 00 	movq   $0x0,0x130(%rsp)
  40a0b7:	00 00 00 00 00 
  40a0bc:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  40a0c3:	00 
  40a0c4:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  40a0cb:	00 
  40a0cc:	48 c7 84 24 28 01 00 	movq   $0x0,0x128(%rsp)
  40a0d3:	00 00 00 00 00 
  40a0d8:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40a0df:	00 
  40a0e0:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40a0e7:	00 
  40a0e8:	e9 98 01 00 00       	jmp    40a285 <runtime::udivmod128+0x9d5>
  40a0ed:	e9 c7 00 00 00       	jmp    40a1b9 <runtime::udivmod128+0x909>
  40a0f2:	48 c7 84 24 30 01 00 	movq   $0x0,0x130(%rsp)
  40a0f9:	00 00 00 00 00 
  40a0fe:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  40a105:	00 
  40a106:	b9 40 00 00 00       	mov    $0x40,%ecx
  40a10b:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  40a112:	89 c9                	mov    %ecx,%ecx
  40a114:	89 ca                	mov    %ecx,%edx
  40a116:	48 89 d1             	mov    %rdx,%rcx
  40a119:	48 d3 e0             	shl    %cl,%rax
  40a11c:	48 89 c1             	mov    %rax,%rcx
  40a11f:	31 c0                	xor    %eax,%eax
  40a121:	48 83 fa 40          	cmp    $0x40,%rdx
  40a125:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a129:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  40a130:	00 
  40a131:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40a138:	00 
  40a139:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  40a140:	89 ca                	mov    %ecx,%edx
  40a142:	48 89 d1             	mov    %rdx,%rcx
  40a145:	48 d3 e8             	shr    %cl,%rax
  40a148:	48 89 c1             	mov    %rax,%rcx
  40a14b:	31 c0                	xor    %eax,%eax
  40a14d:	48 83 fa 40          	cmp    $0x40,%rdx
  40a151:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a155:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  40a15c:	00 
  40a15d:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40a164:	00 
  40a165:	b9 40 00 00 00       	mov    $0x40,%ecx
  40a16a:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  40a171:	89 c9                	mov    %ecx,%ecx
  40a173:	89 ca                	mov    %ecx,%edx
  40a175:	48 89 d1             	mov    %rdx,%rcx
  40a178:	48 d3 e0             	shl    %cl,%rax
  40a17b:	48 89 c1             	mov    %rax,%rcx
  40a17e:	31 c0                	xor    %eax,%eax
  40a180:	48 83 fa 40          	cmp    $0x40,%rdx
  40a184:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a188:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  40a18f:	00 
  40a190:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  40a197:	89 ce                	mov    %ecx,%esi
  40a199:	48 89 f1             	mov    %rsi,%rcx
  40a19c:	48 d3 ea             	shr    %cl,%rdx
  40a19f:	31 c9                	xor    %ecx,%ecx
  40a1a1:	48 83 fe 40          	cmp    $0x40,%rsi
  40a1a5:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40a1a9:	48 09 c8             	or     %rcx,%rax
  40a1ac:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40a1b3:	00 
  40a1b4:	e9 cc 00 00 00       	jmp    40a285 <runtime::udivmod128+0x9d5>
  40a1b9:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  40a1c0:	00 
  40a1c1:	b9 80 00 00 00       	mov    $0x80,%ecx
  40a1c6:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  40a1cd:	89 c9                	mov    %ecx,%ecx
  40a1cf:	89 ca                	mov    %ecx,%edx
  40a1d1:	48 89 d1             	mov    %rdx,%rcx
  40a1d4:	48 d3 e0             	shl    %cl,%rax
  40a1d7:	48 89 c1             	mov    %rax,%rcx
  40a1da:	31 c0                	xor    %eax,%eax
  40a1dc:	48 83 fa 40          	cmp    $0x40,%rdx
  40a1e0:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a1e4:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  40a1eb:	00 
  40a1ec:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40a1f3:	00 
  40a1f4:	b9 80 00 00 00       	mov    $0x80,%ecx
  40a1f9:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  40a200:	89 c9                	mov    %ecx,%ecx
  40a202:	89 ca                	mov    %ecx,%edx
  40a204:	48 89 d1             	mov    %rdx,%rcx
  40a207:	48 d3 e0             	shl    %cl,%rax
  40a20a:	48 89 c1             	mov    %rax,%rcx
  40a20d:	31 c0                	xor    %eax,%eax
  40a20f:	48 83 fa 40          	cmp    $0x40,%rdx
  40a213:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a217:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  40a21e:	00 
  40a21f:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  40a226:	83 e9 40             	sub    $0x40,%ecx
  40a229:	89 c9                	mov    %ecx,%ecx
  40a22b:	89 ce                	mov    %ecx,%esi
  40a22d:	48 89 f1             	mov    %rsi,%rcx
  40a230:	48 d3 ea             	shr    %cl,%rdx
  40a233:	31 c9                	xor    %ecx,%ecx
  40a235:	48 83 fe 40          	cmp    $0x40,%rsi
  40a239:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40a23d:	48 09 c8             	or     %rcx,%rax
  40a240:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  40a247:	00 
  40a248:	48 c7 84 24 28 01 00 	movq   $0x0,0x128(%rsp)
  40a24f:	00 00 00 00 00 
  40a254:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40a25b:	00 
  40a25c:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  40a263:	83 e9 40             	sub    $0x40,%ecx
  40a266:	89 c9                	mov    %ecx,%ecx
  40a268:	89 ca                	mov    %ecx,%edx
  40a26a:	48 89 d1             	mov    %rdx,%rcx
  40a26d:	48 d3 e8             	shr    %cl,%rax
  40a270:	48 89 c1             	mov    %rax,%rcx
  40a273:	31 c0                	xor    %eax,%eax
  40a275:	48 83 fa 40          	cmp    $0x40,%rdx
  40a279:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a27d:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40a284:	00 
  40a285:	e9 9e 01 00 00       	jmp    40a428 <runtime::udivmod128+0xb78>
  40a28a:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  40a291:	00 
  40a292:	b8 7f 00 00 00       	mov    $0x7f,%eax
  40a297:	48 0f bd c1          	bsr    %rcx,%rax
  40a29b:	48 83 f0 3f          	xor    $0x3f,%rax
  40a29f:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  40a2a6:	00 
  40a2a7:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40a2ac:	48 0f bd ca          	bsr    %rdx,%rcx
  40a2b0:	48 83 f1 3f          	xor    $0x3f,%rcx
  40a2b4:	29 c8                	sub    %ecx,%eax
  40a2b6:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  40a2bd:	83 bc 24 1c 01 00 00 	cmpl   $0x3f,0x11c(%rsp)
  40a2c4:	3f 
  40a2c5:	0f 97 c0             	seta   %al
  40a2c8:	24 01                	and    $0x1,%al
  40a2ca:	3c 00                	cmp    $0x0,%al
  40a2cc:	74 43                	je     40a311 <runtime::udivmod128+0xa61>
  40a2ce:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40a2d5:	00 
  40a2d6:	48 83 f8 00          	cmp    $0x0,%rax
  40a2da:	0f 95 c0             	setne  %al
  40a2dd:	24 01                	and    $0x1,%al
  40a2df:	3c 00                	cmp    $0x0,%al
  40a2e1:	74 1f                	je     40a302 <runtime::udivmod128+0xa52>
  40a2e3:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40a2ea:	00 
  40a2eb:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  40a2f2:	00 
  40a2f3:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  40a2fa:	00 
  40a2fb:	48 89 10             	mov    %rdx,(%rax)
  40a2fe:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40a302:	31 c0                	xor    %eax,%eax
  40a304:	89 c2                	mov    %eax,%edx
  40a306:	48 89 d0             	mov    %rdx,%rax
  40a309:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  40a310:	c3                   	ret
  40a311:	8b 84 24 1c 01 00 00 	mov    0x11c(%rsp),%eax
  40a318:	83 c0 01             	add    $0x1,%eax
  40a31b:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  40a322:	48 c7 84 24 30 01 00 	movq   $0x0,0x130(%rsp)
  40a329:	00 00 00 00 00 
  40a32e:	83 bc 24 1c 01 00 00 	cmpl   $0x40,0x11c(%rsp)
  40a335:	40 
  40a336:	0f 94 c0             	sete   %al
  40a339:	24 01                	and    $0x1,%al
  40a33b:	3c 00                	cmp    $0x0,%al
  40a33d:	74 31                	je     40a370 <runtime::udivmod128+0xac0>
  40a33f:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  40a346:	00 
  40a347:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  40a34e:	00 
  40a34f:	48 c7 84 24 28 01 00 	movq   $0x0,0x128(%rsp)
  40a356:	00 00 00 00 00 
  40a35b:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40a362:	00 
  40a363:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40a36a:	00 
  40a36b:	e9 b6 00 00 00       	jmp    40a426 <runtime::udivmod128+0xb76>
  40a370:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40a377:	00 
  40a378:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  40a37f:	89 ca                	mov    %ecx,%edx
  40a381:	48 89 d1             	mov    %rdx,%rcx
  40a384:	48 d3 e8             	shr    %cl,%rax
  40a387:	48 89 c1             	mov    %rax,%rcx
  40a38a:	31 c0                	xor    %eax,%eax
  40a38c:	48 83 fa 40          	cmp    $0x40,%rdx
  40a390:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a394:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  40a39b:	00 
  40a39c:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40a3a3:	00 
  40a3a4:	b9 40 00 00 00       	mov    $0x40,%ecx
  40a3a9:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  40a3b0:	89 c9                	mov    %ecx,%ecx
  40a3b2:	89 ca                	mov    %ecx,%edx
  40a3b4:	48 89 d1             	mov    %rdx,%rcx
  40a3b7:	48 d3 e0             	shl    %cl,%rax
  40a3ba:	48 89 c1             	mov    %rax,%rcx
  40a3bd:	31 c0                	xor    %eax,%eax
  40a3bf:	48 83 fa 40          	cmp    $0x40,%rdx
  40a3c3:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a3c7:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  40a3ce:	00 
  40a3cf:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  40a3d6:	89 ce                	mov    %ecx,%esi
  40a3d8:	48 89 f1             	mov    %rsi,%rcx
  40a3db:	48 d3 ea             	shr    %cl,%rdx
  40a3de:	31 c9                	xor    %ecx,%ecx
  40a3e0:	48 83 fe 40          	cmp    $0x40,%rsi
  40a3e4:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40a3e8:	48 09 c8             	or     %rcx,%rax
  40a3eb:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40a3f2:	00 
  40a3f3:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  40a3fa:	00 
  40a3fb:	b9 40 00 00 00       	mov    $0x40,%ecx
  40a400:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  40a407:	89 c9                	mov    %ecx,%ecx
  40a409:	89 ca                	mov    %ecx,%edx
  40a40b:	48 89 d1             	mov    %rdx,%rcx
  40a40e:	48 d3 e0             	shl    %cl,%rax
  40a411:	48 89 c1             	mov    %rax,%rcx
  40a414:	31 c0                	xor    %eax,%eax
  40a416:	48 83 fa 40          	cmp    $0x40,%rdx
  40a41a:	48 0f 42 c1          	cmovb  %rcx,%rax
  40a41e:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  40a425:	00 
  40a426:	eb 00                	jmp    40a428 <runtime::udivmod128+0xb78>
  40a428:	eb 00                	jmp    40a42a <runtime::udivmod128+0xb7a>
  40a42a:	c7 84 24 0c 01 00 00 	movl   $0x0,0x10c(%rsp)
  40a431:	00 00 00 00 
  40a435:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  40a43c:	00 00 00 00 00 
  40a441:	48 c7 84 24 f0 00 00 	movq   $0x0,0xf0(%rsp)
  40a448:	00 00 00 00 00 
  40a44d:	83 bc 24 1c 01 00 00 	cmpl   $0x0,0x11c(%rsp)
  40a454:	00 
  40a455:	0f 97 c0             	seta   %al
  40a458:	24 01                	and    $0x1,%al
  40a45a:	3c 00                	cmp    $0x0,%al
  40a45c:	0f 84 57 01 00 00    	je     40a5b9 <runtime::udivmod128+0xd09>
  40a462:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  40a469:	00 
  40a46a:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  40a471:	00 
  40a472:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  40a479:	00 
  40a47a:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  40a481:	00 
  40a482:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  40a487:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  40a48e:	00 
  40a48f:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40a496:	00 
  40a497:	48 8b 8c 24 38 01 00 	mov    0x138(%rsp),%rcx
  40a49e:	00 
  40a49f:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  40a4a4:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40a4ab:	00 
  40a4ac:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  40a4b3:	00 
  40a4b4:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40a4bb:	00 
  40a4bc:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  40a4c1:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  40a4c8:	00 
  40a4c9:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40a4d0:	00 
  40a4d1:	48 01 c0             	add    %rax,%rax
  40a4d4:	8b 8c 24 0c 01 00 00 	mov    0x10c(%rsp),%ecx
  40a4db:	48 09 c8             	or     %rcx,%rax
  40a4de:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  40a4e5:	00 
  40a4e6:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40a4ed:	00 
  40a4ee:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  40a4f5:	00 
  40a4f6:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  40a4fd:	00 
  40a4fe:	48 89 84 24 f0 00 00 	mov    %rax,0xf0(%rsp)
  40a505:	00 
  40a506:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  40a50d:	00 
  40a50e:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40a515:	00 
  40a516:	48 f7 d0             	not    %rax
  40a519:	48 f7 d1             	not    %rcx
  40a51c:	48 01 f1             	add    %rsi,%rcx
  40a51f:	48 11 d0             	adc    %rdx,%rax
  40a522:	48 c1 f8 3f          	sar    $0x3f,%rax
  40a526:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  40a52d:	00 
  40a52e:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  40a535:	00 
  40a536:	8b 84 24 e0 00 00 00 	mov    0xe0(%rsp),%eax
  40a53d:	83 e0 01             	and    $0x1,%eax
  40a540:	89 84 24 0c 01 00 00 	mov    %eax,0x10c(%rsp)
  40a547:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40a54e:	00 
  40a54f:	48 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%rcx
  40a556:	00 
  40a557:	48 21 ca             	and    %rcx,%rdx
  40a55a:	48 21 c6             	and    %rax,%rsi
  40a55d:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  40a564:	00 
  40a565:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40a56c:	00 
  40a56d:	48 29 f1             	sub    %rsi,%rcx
  40a570:	48 19 d0             	sbb    %rdx,%rax
  40a573:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  40a57a:	00 
  40a57b:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  40a582:	00 
  40a583:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40a58a:	00 
  40a58b:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  40a592:	00 
  40a593:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  40a59a:	00 
  40a59b:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40a5a2:	00 
  40a5a3:	8b 84 24 1c 01 00 00 	mov    0x11c(%rsp),%eax
  40a5aa:	83 e8 01             	sub    $0x1,%eax
  40a5ad:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  40a5b4:	e9 94 fe ff ff       	jmp    40a44d <runtime::udivmod128+0xb9d>
  40a5b9:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40a5c0:	00 
  40a5c1:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  40a5c8:	00 
  40a5c9:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  40a5d0:	00 
  40a5d1:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  40a5d6:	48 01 c9             	add    %rcx,%rcx
  40a5d9:	8b b4 24 0c 01 00 00 	mov    0x10c(%rsp),%esi
  40a5e0:	48 09 f1             	or     %rsi,%rcx
  40a5e3:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  40a5ea:	00 
  40a5eb:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  40a5f2:	00 
  40a5f3:	48 83 f8 00          	cmp    $0x0,%rax
  40a5f7:	0f 95 c0             	setne  %al
  40a5fa:	24 01                	and    $0x1,%al
  40a5fc:	3c 00                	cmp    $0x0,%al
  40a5fe:	74 1f                	je     40a61f <runtime::udivmod128+0xd6f>
  40a600:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40a607:	00 
  40a608:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  40a60f:	00 
  40a610:	48 8b 94 24 f8 00 00 	mov    0xf8(%rsp),%rdx
  40a617:	00 
  40a618:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40a61c:	48 89 08             	mov    %rcx,(%rax)
  40a61f:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  40a626:	00 
  40a627:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  40a62e:	00 
  40a62f:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  40a636:	c3                   	ret
  40a637:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40a63e:	00 00 

000000000040a640 <runtime::bounds_trap>:
  40a640:	eb 00                	jmp    40a642 <runtime::bounds_trap+0x2>
  40a642:	0f 0b                	ud2
  40a644:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40a64b:	00 00 00 00 00 

000000000040a650 <runtime::bounds_check_error>:
  40a650:	48 83 ec 58          	sub    $0x58,%rsp
  40a654:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40a659:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40a65e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  40a662:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  40a666:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40a66b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40a670:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40a675:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40a67a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40a67e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  40a682:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40a687:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40a68c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  40a691:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  40a696:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40a69a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40a69e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40a6a3:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40a6a8:	48 39 c8             	cmp    %rcx,%rax
  40a6ab:	0f 92 c0             	setb   %al
  40a6ae:	24 01                	and    $0x1,%al
  40a6b0:	3c 00                	cmp    $0x0,%al
  40a6b2:	74 05                	je     40a6b9 <runtime::bounds_check_error+0x69>
  40a6b4:	48 83 c4 58          	add    $0x58,%rsp
  40a6b8:	c3                   	ret
  40a6b9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40a6be:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40a6c3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40a6c7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40a6cb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40a6d0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40a6d5:	e8 76 05 00 00       	call   40ac50 <runtime::bounds_check_error.handle_error-0>
  40a6da:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

000000000040a6e0 <runtime::slice_handle_error>:
  40a6e0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40a6e7:	4c 89 0c 24          	mov    %r9,(%rsp)
  40a6eb:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40a6f0:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  40a6f4:	89 54 24 14          	mov    %edx,0x14(%rsp)
  40a6f8:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40a6fd:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40a702:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40a709:	00 
  40a70a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40a70f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40a714:	4c 8b 04 24          	mov    (%rsp),%r8
  40a718:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40a71d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  40a721:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  40a725:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40a72a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40a72f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40a734:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40a73b:	00 
  40a73c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  40a740:	89 44 24 70          	mov    %eax,0x70(%rsp)
  40a744:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  40a749:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  40a74e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  40a753:	0f 57 c0             	xorps  %xmm0,%xmm0
  40a756:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40a75b:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40a760:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40a767:	00 00 
  40a769:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40a76e:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40a773:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40a77a:	00 00 
  40a77c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40a781:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40a786:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  40a78a:	89 44 24 44          	mov    %eax,0x44(%rsp)
  40a78e:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40a793:	e8 78 c6 ff ff       	call   406e10 <runtime::print_caller_location>
  40a798:	bf 79 eb 40 00       	mov    $0x40eb79,%edi
  40a79d:	be 17 00 00 00       	mov    $0x17,%esi
  40a7a2:	e8 a9 c2 ff ff       	call   406a50 <runtime::print_string>
  40a7a7:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40a7ac:	e8 ef c4 ff ff       	call   406ca0 <runtime::print_i64>
  40a7b1:	bf 91 eb 40 00       	mov    $0x40eb91,%edi
  40a7b6:	be 01 00 00 00       	mov    $0x1,%esi
  40a7bb:	e8 90 c2 ff ff       	call   406a50 <runtime::print_string>
  40a7c0:	48 8b 3c 24          	mov    (%rsp),%rdi
  40a7c4:	e8 d7 c4 ff ff       	call   406ca0 <runtime::print_i64>
  40a7c9:	bf 93 eb 40 00       	mov    $0x40eb93,%edi
  40a7ce:	be 15 00 00 00       	mov    $0x15,%esi
  40a7d3:	e8 78 c2 ff ff       	call   406a50 <runtime::print_string>
  40a7d8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40a7dd:	e8 be c4 ff ff       	call   406ca0 <runtime::print_i64>
  40a7e2:	bf 0a 00 00 00       	mov    $0xa,%edi
  40a7e7:	e8 d4 c2 ff ff       	call   406ac0 <runtime::print_byte>
  40a7ec:	e8 4f fe ff ff       	call   40a640 <runtime::bounds_trap>
  40a7f1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40a7f8:	0f 1f 84 00 00 00 00 
  40a7ff:	00 

000000000040a800 <runtime::multi_pointer_slice_handle_error>:
  40a800:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40a807:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40a80c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40a811:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  40a815:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  40a819:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40a81e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40a823:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40a828:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40a82d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  40a831:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40a835:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40a83a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40a83f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40a844:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40a84b:	00 
  40a84c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  40a850:	89 44 24 70          	mov    %eax,0x70(%rsp)
  40a854:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  40a859:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40a85e:	0f 57 c0             	xorps  %xmm0,%xmm0
  40a861:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40a866:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40a86b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40a872:	00 00 
  40a874:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40a879:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40a87e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40a885:	00 00 
  40a887:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40a88c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40a891:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  40a895:	89 44 24 44          	mov    %eax,0x44(%rsp)
  40a899:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40a89e:	e8 6d c5 ff ff       	call   406e10 <runtime::print_caller_location>
  40a8a3:	bf 79 eb 40 00       	mov    $0x40eb79,%edi
  40a8a8:	be 17 00 00 00       	mov    $0x17,%esi
  40a8ad:	e8 9e c1 ff ff       	call   406a50 <runtime::print_string>
  40a8b2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40a8b7:	e8 e4 c3 ff ff       	call   406ca0 <runtime::print_i64>
  40a8bc:	bf 91 eb 40 00       	mov    $0x40eb91,%edi
  40a8c1:	be 01 00 00 00       	mov    $0x1,%esi
  40a8c6:	e8 85 c1 ff ff       	call   406a50 <runtime::print_string>
  40a8cb:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40a8d0:	e8 cb c3 ff ff       	call   406ca0 <runtime::print_i64>
  40a8d5:	bf 0a 00 00 00       	mov    $0xa,%edi
  40a8da:	e8 e1 c1 ff ff       	call   406ac0 <runtime::print_byte>
  40a8df:	e8 5c fd ff ff       	call   40a640 <runtime::bounds_trap>
  40a8e4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40a8eb:	00 00 00 00 00 

000000000040a8f0 <runtime::multi_pointer_slice_expr_error>:
  40a8f0:	48 83 ec 58          	sub    $0x58,%rsp
  40a8f4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40a8f9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40a8fe:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  40a902:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  40a906:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40a90b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40a910:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40a915:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40a91a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40a91e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  40a922:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40a927:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40a92c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  40a931:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  40a936:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40a93a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40a93e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40a943:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40a948:	48 39 c8             	cmp    %rcx,%rax
  40a94b:	0f 9e c0             	setle  %al
  40a94e:	24 01                	and    $0x1,%al
  40a950:	3c 00                	cmp    $0x0,%al
  40a952:	74 05                	je     40a959 <runtime::multi_pointer_slice_expr_error+0x69>
  40a954:	48 83 c4 58          	add    $0x58,%rsp
  40a958:	c3                   	ret
  40a959:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40a95e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40a963:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40a967:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40a96b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40a970:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40a975:	e8 86 fe ff ff       	call   40a800 <runtime::multi_pointer_slice_handle_error>
  40a97a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

000000000040a980 <runtime::slice_expr_error_hi>:
  40a980:	48 83 ec 58          	sub    $0x58,%rsp
  40a984:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40a989:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40a98e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  40a992:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  40a996:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40a99b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40a9a0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40a9a5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40a9aa:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40a9ae:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  40a9b2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40a9b7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40a9bc:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  40a9c1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  40a9c6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40a9ca:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40a9ce:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40a9d3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40a9d8:	31 c0                	xor    %eax,%eax
  40a9da:	48 39 c8             	cmp    %rcx,%rax
  40a9dd:	0f 9e c0             	setle  %al
  40a9e0:	24 01                	and    $0x1,%al
  40a9e2:	3c 00                	cmp    $0x0,%al
  40a9e4:	74 1b                	je     40aa01 <runtime::slice_expr_error_hi+0x81>
  40a9e6:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40a9eb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40a9f0:	48 39 c8             	cmp    %rcx,%rax
  40a9f3:	0f 9e c0             	setle  %al
  40a9f6:	24 01                	and    $0x1,%al
  40a9f8:	3c 00                	cmp    $0x0,%al
  40a9fa:	74 05                	je     40aa01 <runtime::slice_expr_error_hi+0x81>
  40a9fc:	48 83 c4 58          	add    $0x58,%rsp
  40aa00:	c3                   	ret
  40aa01:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  40aa06:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40aa0a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40aa0e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40aa13:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40aa18:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40aa1d:	48 89 e0             	mov    %rsp,%rax
  40aa20:	4c 89 00             	mov    %r8,(%rax)
  40aa23:	31 c0                	xor    %eax,%eax
  40aa25:	41 89 c0             	mov    %eax,%r8d
  40aa28:	e8 b3 fc ff ff       	call   40a6e0 <runtime::slice_handle_error>
  40aa2d:	0f 1f 00             	nopl   (%rax)

000000000040aa30 <runtime::slice_expr_error_lo_hi>:
  40aa30:	48 83 ec 68          	sub    $0x68,%rsp
  40aa34:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40aa39:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40aa3e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  40aa42:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  40aa46:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40aa4b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40aa50:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40aa55:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40aa5a:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40aa5f:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40aa64:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40aa69:	8b 74 24 18          	mov    0x18(%rsp),%esi
  40aa6d:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  40aa71:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  40aa76:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  40aa7b:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  40aa80:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  40aa85:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  40aa89:	89 74 24 50          	mov    %esi,0x50(%rsp)
  40aa8d:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40aa92:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40aa97:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40aa9c:	31 c0                	xor    %eax,%eax
  40aa9e:	48 39 c8             	cmp    %rcx,%rax
  40aaa1:	0f 9e c0             	setle  %al
  40aaa4:	24 01                	and    $0x1,%al
  40aaa6:	3c 00                	cmp    $0x0,%al
  40aaa8:	74 47                	je     40aaf1 <runtime::slice_expr_error_lo_hi+0xc1>
  40aaaa:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40aaaf:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40aab4:	48 39 c8             	cmp    %rcx,%rax
  40aab7:	0f 9e c0             	setle  %al
  40aaba:	24 01                	and    $0x1,%al
  40aabc:	3c 00                	cmp    $0x0,%al
  40aabe:	74 31                	je     40aaf1 <runtime::slice_expr_error_lo_hi+0xc1>
  40aac0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40aac5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40aaca:	48 39 c8             	cmp    %rcx,%rax
  40aacd:	0f 9e c0             	setle  %al
  40aad0:	24 01                	and    $0x1,%al
  40aad2:	3c 00                	cmp    $0x0,%al
  40aad4:	74 1b                	je     40aaf1 <runtime::slice_expr_error_lo_hi+0xc1>
  40aad6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40aadb:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40aae0:	48 39 c8             	cmp    %rcx,%rax
  40aae3:	0f 9e c0             	setle  %al
  40aae6:	24 01                	and    $0x1,%al
  40aae8:	3c 00                	cmp    $0x0,%al
  40aaea:	74 05                	je     40aaf1 <runtime::slice_expr_error_lo_hi+0xc1>
  40aaec:	48 83 c4 68          	add    $0x68,%rsp
  40aaf0:	c3                   	ret
  40aaf1:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40aaf6:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40aafb:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40aaff:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40ab03:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40ab08:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40ab0d:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  40ab12:	48 89 e0             	mov    %rsp,%rax
  40ab15:	4c 89 10             	mov    %r10,(%rax)
  40ab18:	e8 c3 fb ff ff       	call   40a6e0 <runtime::slice_handle_error>
  40ab1d:	0f 1f 00             	nopl   (%rax)

000000000040ab20 <runtime::matrix_bounds_check_error>:
  40ab20:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40ab27:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40ab2c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40ab31:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  40ab35:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  40ab39:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  40ab3e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40ab43:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  40ab4a:	00 
  40ab4b:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40ab50:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40ab57:	00 
  40ab58:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40ab5d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40ab62:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40ab67:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40ab6c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40ab71:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  40ab75:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  40ab7a:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  40ab7f:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  40ab84:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  40ab89:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  40ab90:	00 
  40ab91:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  40ab96:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  40ab9a:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40ab9f:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  40aba4:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  40aba9:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40abae:	48 39 c8             	cmp    %rcx,%rax
  40abb1:	0f 92 c0             	setb   %al
  40abb4:	24 01                	and    $0x1,%al
  40abb6:	3c 00                	cmp    $0x0,%al
  40abb8:	74 1e                	je     40abd8 <runtime::matrix_bounds_check_error+0xb8>
  40abba:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40abbf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40abc4:	48 39 c8             	cmp    %rcx,%rax
  40abc7:	0f 92 c0             	setb   %al
  40abca:	24 01                	and    $0x1,%al
  40abcc:	3c 00                	cmp    $0x0,%al
  40abce:	74 08                	je     40abd8 <runtime::matrix_bounds_check_error+0xb8>
  40abd0:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40abd7:	c3                   	ret
  40abd8:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40abdd:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  40abe2:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  40abe6:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  40abea:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40abef:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40abf4:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  40abf9:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  40abfe:	48 89 e0             	mov    %rsp,%rax
  40ac01:	4c 89 58 08          	mov    %r11,0x8(%rax)
  40ac05:	4c 89 10             	mov    %r10,(%rax)
  40ac08:	e8 33 01 00 00       	call   40ad40 <runtime::matrix_bounds_check_error.handle_error-0>
  40ac0d:	0f 1f 00             	nopl   (%rax)

000000000040ac10 <runtime::make_slice_error_loc>:
  40ac10:	48 83 ec 18          	sub    $0x18,%rsp
  40ac14:	48 89 3c 24          	mov    %rdi,(%rsp)
  40ac18:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40ac1d:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40ac22:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40ac27:	31 c0                	xor    %eax,%eax
  40ac29:	48 39 c8             	cmp    %rcx,%rax
  40ac2c:	0f 9e c0             	setle  %al
  40ac2f:	24 01                	and    $0x1,%al
  40ac31:	3c 00                	cmp    $0x0,%al
  40ac33:	74 05                	je     40ac3a <runtime::make_slice_error_loc+0x2a>
  40ac35:	48 83 c4 18          	add    $0x18,%rsp
  40ac39:	c3                   	ret
  40ac3a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40ac3f:	48 8b 3c 24          	mov    (%rsp),%rdi
  40ac43:	e8 68 02 00 00       	call   40aeb0 <runtime::make_slice_error_loc.handle_error-0>
  40ac48:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40ac4f:	00 

000000000040ac50 <runtime::bounds_check_error.handle_error-0>:
  40ac50:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40ac57:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40ac5c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40ac61:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  40ac65:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  40ac69:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40ac6e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40ac73:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40ac78:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40ac7d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  40ac81:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40ac85:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40ac8a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40ac8f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40ac94:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40ac9b:	00 
  40ac9c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  40aca0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  40aca4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  40aca9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40acae:	0f 57 c0             	xorps  %xmm0,%xmm0
  40acb1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40acb6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40acbb:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40acc2:	00 00 
  40acc4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40acc9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40acce:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40acd5:	00 00 
  40acd7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40acdc:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40ace1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  40ace5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  40ace9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40acee:	e8 1d c1 ff ff       	call   406e10 <runtime::print_caller_location>
  40acf3:	bf a9 eb 40 00       	mov    $0x40eba9,%edi
  40acf8:	be 07 00 00 00       	mov    $0x7,%esi
  40acfd:	e8 4e bd ff ff       	call   406a50 <runtime::print_string>
  40ad02:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40ad07:	e8 94 bf ff ff       	call   406ca0 <runtime::print_i64>
  40ad0c:	bf 93 eb 40 00       	mov    $0x40eb93,%edi
  40ad11:	be 15 00 00 00       	mov    $0x15,%esi
  40ad16:	e8 35 bd ff ff       	call   406a50 <runtime::print_string>
  40ad1b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40ad20:	e8 7b bf ff ff       	call   406ca0 <runtime::print_i64>
  40ad25:	bf 0a 00 00 00       	mov    $0xa,%edi
  40ad2a:	e8 91 bd ff ff       	call   406ac0 <runtime::print_byte>
  40ad2f:	e8 0c f9 ff ff       	call   40a640 <runtime::bounds_trap>
  40ad34:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40ad3b:	00 00 00 00 00 

000000000040ad40 <runtime::matrix_bounds_check_error.handle_error-0>:
  40ad40:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  40ad47:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40ad4c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40ad51:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  40ad55:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  40ad59:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40ad5e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40ad63:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  40ad6a:	00 
  40ad6b:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40ad70:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40ad77:	00 
  40ad78:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40ad7d:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40ad82:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  40ad87:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40ad8c:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  40ad91:	8b 44 24 18          	mov    0x18(%rsp),%eax
  40ad95:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40ad99:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40ad9e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40ada3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  40adaa:	00 
  40adab:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  40adb2:	00 
  40adb3:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  40adba:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  40adc1:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  40adc8:	00 
  40adc9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  40add0:	00 
  40add1:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  40add6:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  40addb:	0f 57 c0             	xorps  %xmm0,%xmm0
  40adde:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40ade3:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40ade8:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40adef:	00 00 
  40adf1:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40adf6:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40adfb:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40ae02:	00 00 
  40ae04:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40ae09:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40ae0e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  40ae12:	89 44 24 54          	mov    %eax,0x54(%rsp)
  40ae16:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  40ae1b:	e8 f0 bf ff ff       	call   406e10 <runtime::print_caller_location>
  40ae20:	bf b1 eb 40 00       	mov    $0x40ebb1,%edi
  40ae25:	be 11 00 00 00       	mov    $0x11,%esi
  40ae2a:	e8 21 bc ff ff       	call   406a50 <runtime::print_string>
  40ae2f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40ae34:	e8 67 be ff ff       	call   406ca0 <runtime::print_i64>
  40ae39:	bf c3 eb 40 00       	mov    $0x40ebc3,%edi
  40ae3e:	be 02 00 00 00       	mov    $0x2,%esi
  40ae43:	e8 08 bc ff ff       	call   406a50 <runtime::print_string>
  40ae48:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40ae4d:	e8 4e be ff ff       	call   406ca0 <runtime::print_i64>
  40ae52:	bf c6 eb 40 00       	mov    $0x40ebc6,%edi
  40ae57:	be 16 00 00 00       	mov    $0x16,%esi
  40ae5c:	e8 ef bb ff ff       	call   406a50 <runtime::print_string>
  40ae61:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40ae66:	e8 35 be ff ff       	call   406ca0 <runtime::print_i64>
  40ae6b:	bf dd eb 40 00       	mov    $0x40ebdd,%edi
  40ae70:	be 06 00 00 00       	mov    $0x6,%esi
  40ae75:	e8 d6 bb ff ff       	call   406a50 <runtime::print_string>
  40ae7a:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40ae7f:	e8 1c be ff ff       	call   406ca0 <runtime::print_i64>
  40ae84:	bf e4 eb 40 00       	mov    $0x40ebe4,%edi
  40ae89:	be 01 00 00 00       	mov    $0x1,%esi
  40ae8e:	e8 bd bb ff ff       	call   406a50 <runtime::print_string>
  40ae93:	bf 0a 00 00 00       	mov    $0xa,%edi
  40ae98:	e8 23 bc ff ff       	call   406ac0 <runtime::print_byte>
  40ae9d:	e8 9e f7 ff ff       	call   40a640 <runtime::bounds_trap>
  40aea2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40aea9:	1f 84 00 00 00 00 00 

000000000040aeb0 <runtime::make_slice_error_loc.handle_error-0>:
  40aeb0:	48 83 ec 18          	sub    $0x18,%rsp
  40aeb4:	48 89 3c 24          	mov    %rdi,(%rsp)
  40aeb8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40aebd:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40aec2:	48 8b 3c 24          	mov    (%rsp),%rdi
  40aec6:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40aecb:	e8 40 bf ff ff       	call   406e10 <runtime::print_caller_location>
  40aed0:	bf e6 eb 40 00       	mov    $0x40ebe6,%edi
  40aed5:	be 20 00 00 00       	mov    $0x20,%esi
  40aeda:	e8 71 bb ff ff       	call   406a50 <runtime::print_string>
  40aedf:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40aee4:	e8 b7 bd ff ff       	call   406ca0 <runtime::print_i64>
  40aee9:	bf 0a 00 00 00       	mov    $0xa,%edi
  40aeee:	e8 cd bb ff ff       	call   406ac0 <runtime::print_byte>
  40aef3:	e8 48 f7 ff ff       	call   40a640 <runtime::bounds_trap>
  40aef8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40aeff:	00 

000000000040af00 <runtime::__type_info_of>:
  40af00:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  40af05:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40af0a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40af0f:	48 c7 c0 10 ec 40 00 	mov    $0x40ec10,%rax
  40af16:	48 8b 40 08          	mov    0x8(%rax),%rax
  40af1a:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40af1f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40af24:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  40af29:	48 83 f8 00          	cmp    $0x0,%rax
  40af2d:	74 16                	je     40af45 <runtime::__type_info_of+0x45>
  40af2f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  40af34:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40af39:	31 d2                	xor    %edx,%edx
  40af3b:	48 f7 f1             	div    %rcx
  40af3e:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  40af43:	eb 02                	jmp    40af47 <runtime::__type_info_of+0x47>
  40af45:	0f 0b                	ud2
  40af47:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40af4c:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40af51:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  40af58:	00 00 
  40af5a:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  40af61:	00 00 
  40af63:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40af68:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  40af6d:	0f 83 9f 00 00 00    	jae    40b012 <runtime::__type_info_of+0x112>
  40af73:	48 c7 c0 10 ec 40 00 	mov    $0x40ec10,%rax
  40af7a:	48 8b 00             	mov    (%rax),%rax
  40af7d:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40af82:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  40af86:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40af8b:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  40af91:	0f 95 c0             	setne  %al
  40af94:	24 01                	and    $0x1,%al
  40af96:	3c 00                	cmp    $0x0,%al
  40af98:	74 1d                	je     40afb7 <runtime::__type_info_of+0xb7>
  40af9a:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  40af9f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40afa4:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  40afa8:	0f 94 c0             	sete   %al
  40afab:	24 01                	and    $0x1,%al
  40afad:	3c 00                	cmp    $0x0,%al
  40afaf:	74 06                	je     40afb7 <runtime::__type_info_of+0xb7>
  40afb1:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40afb6:	c3                   	ret
  40afb7:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40afbc:	48 83 c0 01          	add    $0x1,%rax
  40afc0:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  40afc5:	0f 92 c0             	setb   %al
  40afc8:	24 01                	and    $0x1,%al
  40afca:	3c 00                	cmp    $0x0,%al
  40afcc:	74 10                	je     40afde <runtime::__type_info_of+0xde>
  40afce:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40afd3:	48 83 c0 01          	add    $0x1,%rax
  40afd7:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40afdc:	eb 09                	jmp    40afe7 <runtime::__type_info_of+0xe7>
  40afde:	31 c0                	xor    %eax,%eax
  40afe0:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40afe5:	eb 00                	jmp    40afe7 <runtime::__type_info_of+0xe7>
  40afe7:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40afec:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40aff1:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40aff6:	48 83 c0 01          	add    $0x1,%rax
  40affa:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40afff:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40b004:	48 83 c0 01          	add    $0x1,%rax
  40b008:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40b00d:	e9 51 ff ff ff       	jmp    40af63 <runtime::__type_info_of+0x63>
  40b012:	48 c7 c0 10 ec 40 00 	mov    $0x40ec10,%rax
  40b019:	48 8b 00             	mov    (%rax),%rax
  40b01c:	48 8b 00             	mov    (%rax),%rax
  40b01f:	c3                   	ret

000000000040b020 <runtime::default_logger_proc>:
  40b020:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40b025:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40b02a:	66 44 89 c0          	mov    %r8w,%ax
  40b02e:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  40b033:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  40b038:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40b03d:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  40b042:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  40b047:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40b04c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  40b051:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  40b056:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  40b05b:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  40b060:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  40b065:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40b06a:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  40b06f:	c3                   	ret

000000000040b070 <runtime::default_context>:
  40b070:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40b077:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40b07c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40b081:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  40b086:	31 f6                	xor    %esi,%esi
  40b088:	ba 70 00 00 00       	mov    $0x70,%edx
  40b08d:	e8 ae 5f ff ff       	call   401040 <memset@plt>
  40b092:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  40b097:	e8 24 00 00 00       	call   40b0c0 <runtime::[core.odin]::__init_context>
  40b09c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40b0a1:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  40b0a6:	ba 70 00 00 00       	mov    $0x70,%edx
  40b0ab:	e8 b0 5f ff ff       	call   401060 <memcpy@plt>
  40b0b0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b0b5:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40b0bc:	c3                   	ret
  40b0bd:	0f 1f 00             	nopl   (%rax)

000000000040b0c0 <runtime::[core.odin]::__init_context>:
  40b0c0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  40b0c5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40b0ca:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40b0cf:	48 83 f8 00          	cmp    $0x0,%rax
  40b0d3:	0f 94 c0             	sete   %al
  40b0d6:	24 01                	and    $0x1,%al
  40b0d8:	3c 00                	cmp    $0x0,%al
  40b0da:	74 01                	je     40b0dd <runtime::[core.odin]::__init_context+0x1d>
  40b0dc:	c3                   	ret
  40b0dd:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b0e2:	48 c7 c1 10 b3 40 00 	mov    $0x40b310,%rcx
  40b0e9:	48 89 08             	mov    %rcx,(%rax)
  40b0ec:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b0f1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40b0f8:	00 
  40b0f9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b0fe:	48 c7 c1 70 71 40 00 	mov    $0x407170,%rcx
  40b105:	48 89 48 10          	mov    %rcx,0x10(%rax)
  40b109:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b10e:	48 c7 c2 58 ff ff ff 	mov    $0xffffffffffffff58,%rdx
  40b115:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  40b11c:	00 00 
  40b11e:	48 01 d1             	add    %rdx,%rcx
  40b121:	48 89 48 18          	mov    %rcx,0x18(%rax)
  40b125:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b12a:	48 c7 c1 70 b1 40 00 	mov    $0x40b170,%rcx
  40b131:	48 89 48 20          	mov    %rcx,0x20(%rax)
  40b135:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b13a:	48 c7 c1 20 b0 40 00 	mov    $0x40b020,%rcx
  40b141:	48 89 48 28          	mov    %rcx,0x28(%rax)
  40b145:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b14a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  40b151:	00 
  40b152:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b157:	48 c7 c1 a0 92 40 00 	mov    $0x4092a0,%rcx
  40b15e:	48 89 48 48          	mov    %rcx,0x48(%rax)
  40b162:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b167:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  40b16e:	00 
  40b16f:	c3                   	ret

000000000040b170 <runtime::default_assertion_failure_proc>:
  40b170:	48 83 ec 48          	sub    $0x48,%rsp
  40b174:	4c 89 04 24          	mov    %r8,(%rsp)
  40b178:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40b17d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40b182:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40b187:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40b18c:	4c 8b 04 24          	mov    (%rsp),%r8
  40b190:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40b195:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40b19a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40b19f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40b1a4:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40b1a9:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40b1ae:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40b1b3:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40b1b8:	e8 03 00 00 00       	call   40b1c0 <runtime::default_assertion_contextless_failure_proc>
  40b1bd:	0f 1f 00             	nopl   (%rax)

000000000040b1c0 <runtime::default_assertion_contextless_failure_proc>:
  40b1c0:	48 83 ec 48          	sub    $0x48,%rsp
  40b1c4:	4c 89 04 24          	mov    %r8,(%rsp)
  40b1c8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40b1cd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40b1d2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40b1d7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40b1dc:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40b1e1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40b1e6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40b1eb:	48 8b 3c 24          	mov    (%rsp),%rdi
  40b1ef:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b1f4:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40b1f9:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40b1fe:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40b203:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40b208:	e8 03 bc ff ff       	call   406e10 <runtime::print_caller_location>
  40b20d:	bf 07 ec 40 00       	mov    $0x40ec07,%edi
  40b212:	be 01 00 00 00       	mov    $0x1,%esi
  40b217:	e8 34 b8 ff ff       	call   406a50 <runtime::print_string>
  40b21c:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40b221:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40b226:	e8 25 b8 ff ff       	call   406a50 <runtime::print_string>
  40b22b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40b230:	48 83 f8 00          	cmp    $0x0,%rax
  40b234:	0f 9f c0             	setg   %al
  40b237:	24 01                	and    $0x1,%al
  40b239:	3c 00                	cmp    $0x0,%al
  40b23b:	74 1e                	je     40b25b <runtime::default_assertion_contextless_failure_proc+0x9b>
  40b23d:	bf 09 ec 40 00       	mov    $0x40ec09,%edi
  40b242:	be 02 00 00 00       	mov    $0x2,%esi
  40b247:	e8 04 b8 ff ff       	call   406a50 <runtime::print_string>
  40b24c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40b251:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40b256:	e8 f5 b7 ff ff       	call   406a50 <runtime::print_string>
  40b25b:	bf 0a 00 00 00       	mov    $0xa,%edi
  40b260:	e8 5b b8 ff ff       	call   406ac0 <runtime::print_byte>
  40b265:	0f 0b                	ud2
  40b267:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40b26e:	00 00 

000000000040b270 <__$startup_runtime>:
  40b270:	50                   	push   %rax
  40b271:	eb 00                	jmp    40b273 <__$startup_runtime+0x3>
  40b273:	e8 e8 7c ff ff       	call   402f60 <os::[allocators.odin]::init_thread_local_cleaner>
  40b278:	e8 43 7d ff ff       	call   402fc0 <os::[file_linux.odin]::_standard_stream_init>
  40b27d:	58                   	pop    %rax
  40b27e:	c3                   	ret
  40b27f:	90                   	nop

000000000040b280 <__$equal$$struct{x:f32,y:f32}>:
  40b280:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  40b285:	48 89 74 24 f8       	mov    %rsi,-0x8(%rsp)
  40b28a:	eb 00                	jmp    40b28c <__$equal$$struct{x:f32,y:f32}+0xc>
  40b28c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40b291:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40b296:	48 39 c8             	cmp    %rcx,%rax
  40b299:	75 03                	jne    40b29e <__$equal$$struct{x:f32,y:f32}+0x1e>
  40b29b:	b0 01                	mov    $0x1,%al
  40b29d:	c3                   	ret
  40b29e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b2a3:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40b2a8:	f3 0f 10 01          	movss  (%rcx),%xmm0
  40b2ac:	0f 2e 00             	ucomiss (%rax),%xmm0
  40b2af:	75 1c                	jne    40b2cd <__$equal$$struct{x:f32,y:f32}+0x4d>
  40b2b1:	7a 1a                	jp     40b2cd <__$equal$$struct{x:f32,y:f32}+0x4d>
  40b2b3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40b2b8:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40b2bd:	f3 0f 10 41 04       	movss  0x4(%rcx),%xmm0
  40b2c2:	0f 2e 40 04          	ucomiss 0x4(%rax),%xmm0
  40b2c6:	75 05                	jne    40b2cd <__$equal$$struct{x:f32,y:f32}+0x4d>
  40b2c8:	7a 03                	jp     40b2cd <__$equal$$struct{x:f32,y:f32}+0x4d>
  40b2ca:	b0 01                	mov    $0x1,%al
  40b2cc:	c3                   	ret
  40b2cd:	31 c0                	xor    %eax,%eax
  40b2cf:	c3                   	ret

000000000040b2d0 <__$cleanup_runtime>:
  40b2d0:	50                   	push   %rax
  40b2d1:	eb 00                	jmp    40b2d3 <__$cleanup_runtime+0x3>
  40b2d3:	e8 88 7d ff ff       	call   403060 <os::[process.odin]::delete_args>
  40b2d8:	e8 13 7f ff ff       	call   4031f0 <os::[allocators.odin]::temp_allocator_fini>
  40b2dd:	e8 1e be ff ff       	call   407100 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  40b2e2:	58                   	pop    %rax
  40b2e3:	c3                   	ret
  40b2e4:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40b2eb:	00 00 00 
  40b2ee:	66 90                	xchg   %ax,%ax

000000000040b2f0 <runtime::heap_allocator>:
  40b2f0:	48 c7 c0 10 b3 40 00 	mov    $0x40b310,%rax
  40b2f7:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40b2fc:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  40b303:	00 00 
  40b305:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40b30a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40b30f:	c3                   	ret

000000000040b310 <runtime::heap_allocator_proc>:
  40b310:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  40b317:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40b31c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40b321:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40b326:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40b32b:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40b330:	40 88 f0             	mov    %sil,%al
  40b333:	88 44 24 47          	mov    %al,0x47(%rsp)
  40b337:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  40b33e:	00 
  40b33f:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40b344:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  40b34b:	00 
  40b34c:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40b351:	8a 44 24 47          	mov    0x47(%rsp),%al
  40b355:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40b35a:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40b35f:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40b364:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40b369:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  40b36e:	4c 89 84 24 b0 00 00 	mov    %r8,0xb0(%rsp)
  40b375:	00 
  40b376:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  40b37d:	48 89 bc 24 a0 00 00 	mov    %rdi,0xa0(%rsp)
  40b384:	00 
  40b385:	48 89 b4 24 98 00 00 	mov    %rsi,0x98(%rsp)
  40b38c:	00 
  40b38d:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  40b394:	00 
  40b395:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40b39c:	00 
  40b39d:	0f b6 c8             	movzbl %al,%ecx
  40b3a0:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40b3a5:	2c 07                	sub    $0x7,%al
  40b3a7:	0f 87 4a 01 00 00    	ja     40b4f7 <runtime::heap_allocator_proc+0x1e7>
  40b3ad:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b3b2:	48 8b 04 c5 c0 ee 40 	mov    0x40eec0(,%rax,8),%rax
  40b3b9:	00 
  40b3ba:	ff e0                	jmp    *%rax
  40b3bc:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40b3c1:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40b3c6:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40b3cb:	8a 44 24 47          	mov    0x47(%rsp),%al
  40b3cf:	84 c0                	test   %al,%al
  40b3d1:	0f 94 c0             	sete   %al
  40b3d4:	0f 57 c0             	xorps  %xmm0,%xmm0
  40b3d7:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40b3dc:	48 89 e1             	mov    %rsp,%rcx
  40b3df:	48 89 11             	mov    %rdx,(%rcx)
  40b3e2:	44 0f b6 c0          	movzbl %al,%r8d
  40b3e6:	31 c0                	xor    %eax,%eax
  40b3e8:	89 c1                	mov    %eax,%ecx
  40b3ea:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  40b3ef:	48 89 ca             	mov    %rcx,%rdx
  40b3f2:	e8 a9 01 00 00       	call   40b5a0 <runtime::heap_allocator_proc.aligned_alloc-0>
  40b3f7:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40b3fc:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40b401:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  40b406:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40b40a:	48 89 11             	mov    %rdx,(%rcx)
  40b40d:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40b414:	c3                   	ret
  40b415:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40b41a:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40b41f:	e8 cc 03 00 00       	call   40b7f0 <runtime::heap_allocator_proc.aligned_free-1>
  40b424:	e9 ce 00 00 00       	jmp    40b4f7 <runtime::heap_allocator_proc+0x1e7>
  40b429:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40b42e:	31 f6                	xor    %esi,%esi
  40b430:	ba 10 00 00 00       	mov    $0x10,%edx
  40b435:	e8 06 5c ff ff       	call   401040 <memset@plt>
  40b43a:	b0 04                	mov    $0x4,%al
  40b43c:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40b443:	c3                   	ret
  40b444:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40b449:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40b44e:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40b453:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40b458:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40b45d:	8a 44 24 47          	mov    0x47(%rsp),%al
  40b461:	2c 03                	sub    $0x3,%al
  40b463:	0f 94 c0             	sete   %al
  40b466:	0f 57 c0             	xorps  %xmm0,%xmm0
  40b469:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  40b46e:	49 89 e0             	mov    %rsp,%r8
  40b471:	4d 89 08             	mov    %r9,(%r8)
  40b474:	44 0f b6 c0          	movzbl %al,%r8d
  40b478:	4c 8d 4c 24 60       	lea    0x60(%rsp),%r9
  40b47d:	e8 ae 03 00 00       	call   40b830 <runtime::heap_allocator_proc.aligned_resize-2>
  40b482:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40b487:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40b48c:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  40b491:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40b495:	48 89 11             	mov    %rdx,(%rcx)
  40b498:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40b49f:	c3                   	ret
  40b4a0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40b4a5:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40b4aa:	48 83 7c 24 58 00    	cmpq   $0x0,0x58(%rsp)
  40b4b0:	0f 95 c0             	setne  %al
  40b4b3:	24 01                	and    $0x1,%al
  40b4b5:	3c 00                	cmp    $0x0,%al
  40b4b7:	74 08                	je     40b4c1 <runtime::heap_allocator_proc+0x1b1>
  40b4b9:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40b4be:	c6 00 db             	movb   $0xdb,(%rax)
  40b4c1:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40b4c6:	31 f6                	xor    %esi,%esi
  40b4c8:	ba 10 00 00 00       	mov    $0x10,%edx
  40b4cd:	e8 6e 5b ff ff       	call   401040 <memset@plt>
  40b4d2:	31 c0                	xor    %eax,%eax
  40b4d4:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40b4db:	c3                   	ret
  40b4dc:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40b4e1:	31 f6                	xor    %esi,%esi
  40b4e3:	ba 10 00 00 00       	mov    $0x10,%edx
  40b4e8:	e8 53 5b ff ff       	call   401040 <memset@plt>
  40b4ed:	b0 04                	mov    $0x4,%al
  40b4ef:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40b4f6:	c3                   	ret
  40b4f7:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40b4fc:	31 f6                	xor    %esi,%esi
  40b4fe:	ba 10 00 00 00       	mov    $0x10,%edx
  40b503:	e8 38 5b ff ff       	call   401040 <memset@plt>
  40b508:	31 c0                	xor    %eax,%eax
  40b50a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40b511:	c3                   	ret
  40b512:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40b519:	1f 84 00 00 00 00 00 

000000000040b520 <runtime::heap_alloc>:
  40b520:	48 83 ec 18          	sub    $0x18,%rsp
  40b524:	48 89 3c 24          	mov    %rdi,(%rsp)
  40b528:	40 88 f0             	mov    %sil,%al
  40b52b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40b52f:	8a 44 24 0e          	mov    0xe(%rsp),%al
  40b533:	48 8b 3c 24          	mov    (%rsp),%rdi
  40b537:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40b53c:	88 44 24 0f          	mov    %al,0xf(%rsp)
  40b540:	0f b6 f0             	movzbl %al,%esi
  40b543:	e8 48 5c ff ff       	call   401190 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  40b548:	48 83 c4 18          	add    $0x18,%rsp
  40b54c:	c3                   	ret
  40b54d:	0f 1f 00             	nopl   (%rax)

000000000040b550 <runtime::heap_resize>:
  40b550:	48 83 ec 28          	sub    $0x28,%rsp
  40b554:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40b559:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40b55e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40b563:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40b568:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40b56d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40b572:	e8 79 5c ff ff       	call   4011f0 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  40b577:	48 83 c4 28          	add    $0x28,%rsp
  40b57b:	c3                   	ret
  40b57c:	0f 1f 40 00          	nopl   0x0(%rax)

000000000040b580 <runtime::heap_free>:
  40b580:	48 83 ec 18          	sub    $0x18,%rsp
  40b584:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40b589:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40b58e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40b593:	e8 88 5c ff ff       	call   401220 <runtime::[heap_allocator_unix.odin]::_heap_free>
  40b598:	48 83 c4 18          	add    $0x18,%rsp
  40b59c:	c3                   	ret
  40b59d:	0f 1f 00             	nopl   (%rax)

000000000040b5a0 <runtime::heap_allocator_proc.aligned_alloc-0>:
  40b5a0:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  40b5a7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40b5ac:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40b5b1:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40b5b6:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40b5bb:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40b5c0:	44 88 c0             	mov    %r8b,%al
  40b5c3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  40b5c7:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40b5ce:	00 
  40b5cf:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40b5d4:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40b5d9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40b5de:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40b5e3:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  40b5e7:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40b5ec:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  40b5f3:	00 
  40b5f4:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40b5fb:	00 
  40b5fc:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40b603:	00 
  40b604:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  40b60b:	00 
  40b60c:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  40b613:	b9 08 00 00 00       	mov    $0x8,%ecx
  40b618:	48 83 fe 08          	cmp    $0x8,%rsi
  40b61c:	48 0f 4f ce          	cmovg  %rsi,%rcx
  40b620:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40b627:	00 
  40b628:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40b62f:	00 
  40b630:	48 83 e9 01          	sub    $0x1,%rcx
  40b634:	48 83 c1 08          	add    $0x8,%rcx
  40b638:	48 01 d1             	add    %rdx,%rcx
  40b63b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  40b642:	00 
  40b643:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  40b64a:	00 00 
  40b64c:	48 83 f8 00          	cmp    $0x0,%rax
  40b650:	0f 95 c1             	setne  %cl
  40b653:	80 e1 01             	and    $0x1,%cl
  40b656:	31 c0                	xor    %eax,%eax
  40b658:	80 f9 00             	cmp    $0x0,%cl
  40b65b:	88 44 24 0f          	mov    %al,0xf(%rsp)
  40b65f:	74 17                	je     40b678 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  40b661:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40b666:	48 83 f8 08          	cmp    $0x8,%rax
  40b66a:	0f 9f c0             	setg   %al
  40b66d:	24 01                	and    $0x1,%al
  40b66f:	3c 00                	cmp    $0x0,%al
  40b671:	0f 95 c0             	setne  %al
  40b674:	88 44 24 0f          	mov    %al,0xf(%rsp)
  40b678:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40b67d:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  40b681:	80 e1 01             	and    $0x1,%cl
  40b684:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  40b688:	48 83 f8 00          	cmp    $0x0,%rax
  40b68c:	0f 95 c0             	setne  %al
  40b68f:	24 01                	and    $0x1,%al
  40b691:	3c 00                	cmp    $0x0,%al
  40b693:	74 2e                	je     40b6c3 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  40b695:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40b69a:	75 27                	jne    40b6c3 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  40b69c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40b6a1:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  40b6a5:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40b6aa:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40b6af:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  40b6b6:	00 
  40b6b7:	e8 94 fe ff ff       	call   40b550 <runtime::heap_resize>
  40b6bc:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40b6c1:	eb 19                	jmp    40b6dc <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  40b6c3:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  40b6c7:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  40b6ce:	00 
  40b6cf:	0f b6 f0             	movzbl %al,%esi
  40b6d2:	e8 49 fe ff ff       	call   40b520 <runtime::heap_alloc>
  40b6d7:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40b6dc:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40b6e1:	48 83 c0 08          	add    $0x8,%rax
  40b6e5:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40b6ea:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40b6ef:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40b6f4:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40b6f9:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40b6fe:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40b703:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  40b70a:	00 
  40b70b:	48 83 e8 01          	sub    $0x1,%rax
  40b70f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40b716:	00 
  40b717:	48 83 e9 01          	sub    $0x1,%rcx
  40b71b:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  40b71f:	48 21 c8             	and    %rcx,%rax
  40b722:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40b727:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  40b72d:	0f 94 c0             	sete   %al
  40b730:	24 01                	and    $0x1,%al
  40b732:	3c 00                	cmp    $0x0,%al
  40b734:	74 39                	je     40b76f <runtime::heap_allocator_proc.aligned_alloc-0+0x1cf>
  40b736:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40b73b:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40b740:	e8 ab 00 00 00       	call   40b7f0 <runtime::heap_allocator_proc.aligned_free-1>
  40b745:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40b74a:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  40b74f:	e8 9c 00 00 00       	call   40b7f0 <runtime::heap_allocator_proc.aligned_free-1>
  40b754:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40b759:	31 f6                	xor    %esi,%esi
  40b75b:	ba 10 00 00 00       	mov    $0x10,%edx
  40b760:	e8 db 58 ff ff       	call   401040 <memset@plt>
  40b765:	b0 01                	mov    $0x1,%al
  40b767:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40b76e:	c3                   	ret
  40b76f:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40b774:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40b779:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40b77e:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  40b783:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  40b787:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40b78c:	74 2f                	je     40b7bd <runtime::heap_allocator_proc.aligned_alloc-0+0x21d>
  40b78e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40b793:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40b798:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40b79d:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  40b7a2:	48 39 d0             	cmp    %rdx,%rax
  40b7a5:	48 0f 4c d0          	cmovl  %rax,%rdx
  40b7a9:	e8 22 0c 00 00       	call   40c3d0 <runtime::mem_copy_non_overlapping>
  40b7ae:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40b7b3:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40b7b8:	e8 33 00 00 00       	call   40b7f0 <runtime::heap_allocator_proc.aligned_free-1>
  40b7bd:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40b7c2:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  40b7c7:	e8 64 0b 00 00       	call   40c330 <runtime::[internal.odin]::byte_slice>
  40b7cc:	48 89 c1             	mov    %rax,%rcx
  40b7cf:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b7d4:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40b7d8:	48 89 08             	mov    %rcx,(%rax)
  40b7db:	31 c0                	xor    %eax,%eax
  40b7dd:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40b7e4:	c3                   	ret
  40b7e5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40b7ec:	00 00 00 00 

000000000040b7f0 <runtime::heap_allocator_proc.aligned_free-1>:
  40b7f0:	48 83 ec 18          	sub    $0x18,%rsp
  40b7f4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40b7f9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40b7fe:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40b803:	48 83 f8 00          	cmp    $0x0,%rax
  40b807:	0f 95 c0             	setne  %al
  40b80a:	24 01                	and    $0x1,%al
  40b80c:	3c 00                	cmp    $0x0,%al
  40b80e:	74 0e                	je     40b81e <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  40b810:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40b815:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  40b819:	e8 62 fd ff ff       	call   40b580 <runtime::heap_free>
  40b81e:	48 83 c4 18          	add    $0x18,%rsp
  40b822:	c3                   	ret
  40b823:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40b82a:	84 00 00 00 00 00 

000000000040b830 <runtime::heap_allocator_proc.aligned_resize-2>:
  40b830:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  40b837:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  40b83c:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40b841:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40b846:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40b84b:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  40b850:	44 88 c0             	mov    %r8b,%al
  40b853:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  40b857:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40b85e:	00 
  40b85f:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40b864:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40b869:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40b86d:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40b872:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40b877:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40b87c:	48 89 bc 24 e0 00 00 	mov    %rdi,0xe0(%rsp)
  40b883:	00 
  40b884:	48 89 b4 24 d8 00 00 	mov    %rsi,0xd8(%rsp)
  40b88b:	00 
  40b88c:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  40b893:	00 
  40b894:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  40b89b:	00 
  40b89c:	88 84 24 c7 00 00 00 	mov    %al,0xc7(%rsp)
  40b8a3:	48 8d bc 24 b0 00 00 	lea    0xb0(%rsp),%rdi
  40b8aa:	00 
  40b8ab:	31 f6                	xor    %esi,%esi
  40b8ad:	ba 10 00 00 00       	mov    $0x10,%edx
  40b8b2:	e8 89 57 ff ff       	call   401040 <memset@plt>
  40b8b7:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40b8bc:	c6 84 24 af 00 00 00 	movb   $0x0,0xaf(%rsp)
  40b8c3:	00 
  40b8c4:	48 83 f8 00          	cmp    $0x0,%rax
  40b8c8:	0f 94 c0             	sete   %al
  40b8cb:	24 01                	and    $0x1,%al
  40b8cd:	3c 00                	cmp    $0x0,%al
  40b8cf:	0f 84 80 00 00 00    	je     40b955 <runtime::heap_allocator_proc.aligned_resize-2+0x125>
  40b8d5:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40b8da:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40b8df:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40b8e4:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40b8e8:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  40b8ed:	0f 57 c0             	xorps  %xmm0,%xmm0
  40b8f0:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40b8f7:	00 
  40b8f8:	48 89 e2             	mov    %rsp,%rdx
  40b8fb:	4c 89 02             	mov    %r8,(%rdx)
  40b8fe:	44 0f b6 c0          	movzbl %al,%r8d
  40b902:	31 c0                	xor    %eax,%eax
  40b904:	89 c2                	mov    %eax,%edx
  40b906:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  40b90d:	00 
  40b90e:	e8 8d fc ff ff       	call   40b5a0 <runtime::heap_allocator_proc.aligned_alloc-0>
  40b913:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40b918:	40 88 c7             	mov    %al,%dil
  40b91b:	40 88 f8             	mov    %dil,%al
  40b91e:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40b925:	00 
  40b926:	48 8b b4 24 98 00 00 	mov    0x98(%rsp),%rsi
  40b92d:	00 
  40b92e:	48 89 b4 24 b8 00 00 	mov    %rsi,0xb8(%rsp)
  40b935:	00 
  40b936:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  40b93d:	00 
  40b93e:	40 88 bc 24 af 00 00 	mov    %dil,0xaf(%rsp)
  40b945:	00 
  40b946:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40b94a:	48 89 11             	mov    %rdx,(%rcx)
  40b94d:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40b954:	c3                   	ret
  40b955:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40b95a:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40b95f:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40b964:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40b969:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40b96d:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  40b972:	0f 57 c0             	xorps  %xmm0,%xmm0
  40b975:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40b97c:	00 
  40b97d:	49 89 e0             	mov    %rsp,%r8
  40b980:	4d 89 08             	mov    %r9,(%r8)
  40b983:	44 0f b6 c0          	movzbl %al,%r8d
  40b987:	4c 8d 8c 24 80 00 00 	lea    0x80(%rsp),%r9
  40b98e:	00 
  40b98f:	e8 0c fc ff ff       	call   40b5a0 <runtime::heap_allocator_proc.aligned_alloc-0>
  40b994:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  40b998:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  40b99f:	00 
  40b9a0:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40b9a5:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40b9ac:	00 
  40b9ad:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40b9b2:	3c 00                	cmp    $0x0,%al
  40b9b4:	74 4d                	je     40ba03 <runtime::heap_allocator_proc.aligned_resize-2+0x1d3>
  40b9b6:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40b9bb:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  40b9bf:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  40b9c6:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  40b9cd:	00 
  40b9ce:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  40b9d5:	00 
  40b9d6:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  40b9dd:	48 89 b4 24 b8 00 00 	mov    %rsi,0xb8(%rsp)
  40b9e4:	00 
  40b9e5:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  40b9ec:	00 
  40b9ed:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  40b9f4:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40b9f8:	48 89 11             	mov    %rdx,(%rcx)
  40b9fb:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40ba02:	c3                   	ret
  40ba03:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40ba07:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40ba0c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40ba11:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  40ba18:	00 
  40ba19:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40ba20:	00 
  40ba21:	3c 00                	cmp    $0x0,%al
  40ba23:	0f 84 85 00 00 00    	je     40baae <runtime::heap_allocator_proc.aligned_resize-2+0x27e>
  40ba29:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40ba2e:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40ba33:	48 39 c8             	cmp    %rcx,%rax
  40ba36:	0f 9f c0             	setg   %al
  40ba39:	24 01                	and    $0x1,%al
  40ba3b:	3c 00                	cmp    $0x0,%al
  40ba3d:	74 6f                	je     40baae <runtime::heap_allocator_proc.aligned_resize-2+0x27e>
  40ba3f:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40ba44:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  40ba4b:	00 
  40ba4c:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40ba51:	48 89 e0             	mov    %rsp,%rax
  40ba54:	4c 89 08             	mov    %r9,(%rax)
  40ba57:	bf 00 ef 40 00       	mov    $0x40ef00,%edi
  40ba5c:	be 2e 00 00 00       	mov    $0x2e,%esi
  40ba61:	ba 4d 00 00 00       	mov    $0x4d,%edx
  40ba66:	b9 26 00 00 00       	mov    $0x26,%ecx
  40ba6b:	e8 c0 ef ff ff       	call   40aa30 <runtime::slice_expr_error_lo_hi>
  40ba70:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40ba75:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40ba7a:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40ba7f:	48 89 c2             	mov    %rax,%rdx
  40ba82:	48 03 94 24 b0 00 00 	add    0xb0(%rsp),%rdx
  40ba89:	00 
  40ba8a:	48 29 c1             	sub    %rax,%rcx
  40ba8d:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  40ba92:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  40ba97:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40ba9c:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  40baa1:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40baa6:	48 29 c6             	sub    %rax,%rsi
  40baa9:	e8 c2 0d 00 00       	call   40c870 <runtime::conditional_mem_zero>
  40baae:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40bab3:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  40baba:	00 
  40babb:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  40bac2:	00 
  40bac3:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  40baca:	48 89 b4 24 b8 00 00 	mov    %rsi,0xb8(%rsp)
  40bad1:	00 
  40bad2:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  40bad9:	00 
  40bada:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  40bae1:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40bae5:	48 89 11             	mov    %rdx,(%rcx)
  40bae8:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40baef:	c3                   	ret

000000000040baf0 <runtime::nil_allocator_proc>:
  40baf0:	48 83 ec 78          	sub    $0x78,%rsp
  40baf4:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40baf9:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40bafe:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40bb03:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40bb08:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40bb0d:	40 88 f0             	mov    %sil,%al
  40bb10:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  40bb14:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  40bb1b:	00 
  40bb1c:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40bb21:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  40bb25:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40bb2a:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40bb2f:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40bb34:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40bb39:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  40bb3e:	4c 89 44 24 70       	mov    %r8,0x70(%rsp)
  40bb43:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  40bb47:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40bb4c:	48 89 74 24 58       	mov    %rsi,0x58(%rsp)
  40bb51:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40bb56:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40bb5b:	0f b6 c8             	movzbl %al,%ecx
  40bb5e:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40bb63:	2c 07                	sub    $0x7,%al
  40bb65:	0f 87 c9 00 00 00    	ja     40bc34 <runtime::nil_allocator_proc+0x144>
  40bb6b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40bb70:	48 8b 04 c5 30 ef 40 	mov    0x40ef30(,%rax,8),%rax
  40bb77:	00 
  40bb78:	ff e0                	jmp    *%rax
  40bb7a:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40bb7f:	31 f6                	xor    %esi,%esi
  40bb81:	ba 10 00 00 00       	mov    $0x10,%edx
  40bb86:	e8 b5 54 ff ff       	call   401040 <memset@plt>
  40bb8b:	b0 01                	mov    $0x1,%al
  40bb8d:	48 83 c4 78          	add    $0x78,%rsp
  40bb91:	c3                   	ret
  40bb92:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40bb97:	31 f6                	xor    %esi,%esi
  40bb99:	ba 10 00 00 00       	mov    $0x10,%edx
  40bb9e:	e8 9d 54 ff ff       	call   401040 <memset@plt>
  40bba3:	31 c0                	xor    %eax,%eax
  40bba5:	48 83 c4 78          	add    $0x78,%rsp
  40bba9:	c3                   	ret
  40bbaa:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40bbaf:	31 f6                	xor    %esi,%esi
  40bbb1:	ba 10 00 00 00       	mov    $0x10,%edx
  40bbb6:	e8 85 54 ff ff       	call   401040 <memset@plt>
  40bbbb:	b0 04                	mov    $0x4,%al
  40bbbd:	48 83 c4 78          	add    $0x78,%rsp
  40bbc1:	c3                   	ret
  40bbc2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40bbc7:	48 83 f8 00          	cmp    $0x0,%rax
  40bbcb:	0f 94 c0             	sete   %al
  40bbce:	24 01                	and    $0x1,%al
  40bbd0:	3c 00                	cmp    $0x0,%al
  40bbd2:	74 18                	je     40bbec <runtime::nil_allocator_proc+0xfc>
  40bbd4:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40bbd9:	31 f6                	xor    %esi,%esi
  40bbdb:	ba 10 00 00 00       	mov    $0x10,%edx
  40bbe0:	e8 5b 54 ff ff       	call   401040 <memset@plt>
  40bbe5:	31 c0                	xor    %eax,%eax
  40bbe7:	48 83 c4 78          	add    $0x78,%rsp
  40bbeb:	c3                   	ret
  40bbec:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40bbf1:	31 f6                	xor    %esi,%esi
  40bbf3:	ba 10 00 00 00       	mov    $0x10,%edx
  40bbf8:	e8 43 54 ff ff       	call   401040 <memset@plt>
  40bbfd:	b0 01                	mov    $0x1,%al
  40bbff:	48 83 c4 78          	add    $0x78,%rsp
  40bc03:	c3                   	ret
  40bc04:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40bc09:	31 f6                	xor    %esi,%esi
  40bc0b:	ba 10 00 00 00       	mov    $0x10,%edx
  40bc10:	e8 2b 54 ff ff       	call   401040 <memset@plt>
  40bc15:	b0 04                	mov    $0x4,%al
  40bc17:	48 83 c4 78          	add    $0x78,%rsp
  40bc1b:	c3                   	ret
  40bc1c:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40bc21:	31 f6                	xor    %esi,%esi
  40bc23:	ba 10 00 00 00       	mov    $0x10,%edx
  40bc28:	e8 13 54 ff ff       	call   401040 <memset@plt>
  40bc2d:	b0 04                	mov    $0x4,%al
  40bc2f:	48 83 c4 78          	add    $0x78,%rsp
  40bc33:	c3                   	ret
  40bc34:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40bc39:	31 f6                	xor    %esi,%esi
  40bc3b:	ba 10 00 00 00       	mov    $0x10,%edx
  40bc40:	e8 fb 53 ff ff       	call   401040 <memset@plt>
  40bc45:	31 c0                	xor    %eax,%eax
  40bc47:	48 83 c4 78          	add    $0x78,%rsp
  40bc4b:	c3                   	ret
  40bc4c:	0f 1f 40 00          	nopl   0x0(%rax)

000000000040bc50 <runtime::nil_allocator>:
  40bc50:	48 c7 c0 f0 ba 40 00 	mov    $0x40baf0,%rax
  40bc57:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40bc5c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  40bc63:	00 00 
  40bc65:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40bc6a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40bc6f:	c3                   	ret

000000000040bc70 <runtime::copy_slice_raw>:
  40bc70:	48 83 ec 58          	sub    $0x58,%rsp
  40bc74:	48 89 3c 24          	mov    %rdi,(%rsp)
  40bc78:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40bc7d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40bc82:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40bc87:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40bc8c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40bc91:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40bc96:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40bc9b:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40bca0:	48 8b 3c 24          	mov    (%rsp),%rdi
  40bca4:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  40bca9:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40bcae:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40bcb3:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40bcb8:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40bcbd:	48 39 c1             	cmp    %rax,%rcx
  40bcc0:	48 0f 4c c1          	cmovl  %rcx,%rax
  40bcc4:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40bcc9:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
  40bccf:	0f 9f c0             	setg   %al
  40bcd2:	24 01                	and    $0x1,%al
  40bcd4:	3c 00                	cmp    $0x0,%al
  40bcd6:	74 19                	je     40bcf1 <runtime::copy_slice_raw+0x81>
  40bcd8:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40bcdd:	48 8b 3c 24          	mov    (%rsp),%rdi
  40bce1:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40bce6:	48 0f af 54 24 28    	imul   0x28(%rsp),%rdx
  40bcec:	e8 9f 53 ff ff       	call   401090 <memmove@plt>
  40bcf1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40bcf6:	48 83 c4 58          	add    $0x58,%rsp
  40bcfa:	c3                   	ret
  40bcfb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040bd00 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  40bd00:	48 83 ec 48          	sub    $0x48,%rsp
  40bd04:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40bd09:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40bd0e:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40bd13:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40bd18:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40bd1d:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40bd22:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40bd27:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40bd2c:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40bd31:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40bd36:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40bd3b:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40bd40:	41 b8 01 00 00 00    	mov    $0x1,%r8d
  40bd46:	e8 25 ff ff ff       	call   40bc70 <runtime::copy_slice_raw>
  40bd4b:	48 83 c4 48          	add    $0x48,%rsp
  40bd4f:	c3                   	ret

000000000040bd50 <runtime::copy_from_string:proc"contextless"(dst:[]u8,src:string)->(:int)>:
  40bd50:	48 83 ec 48          	sub    $0x48,%rsp
  40bd54:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40bd59:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40bd5e:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40bd63:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40bd68:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40bd6d:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40bd72:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40bd77:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40bd7c:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40bd81:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40bd86:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40bd8b:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40bd90:	41 b8 01 00 00 00    	mov    $0x1,%r8d
  40bd96:	e8 d5 fe ff ff       	call   40bc70 <runtime::copy_slice_raw>
  40bd9b:	48 83 c4 48          	add    $0x48,%rsp
  40bd9f:	c3                   	ret

000000000040bda0 <runtime::delete_string>:
  40bda0:	48 83 ec 68          	sub    $0x68,%rsp
  40bda4:	4c 89 0c 24          	mov    %r9,(%rsp)
  40bda8:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40bdad:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40bdb2:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  40bdb7:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40bdbc:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40bdc1:	4c 8b 0c 24          	mov    (%rsp),%r9
  40bdc5:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40bdca:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40bdcf:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40bdd4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40bdd9:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40bdde:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  40bde3:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  40bde8:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40bded:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40bdf2:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40bdf7:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40bdfc:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40be01:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40be06:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40be0b:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40be10:	e8 6b 09 00 00       	call   40c780 <runtime::mem_free_with_size>
  40be15:	48 83 c4 68          	add    $0x68,%rsp
  40be19:	c3                   	ret
  40be1a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

000000000040be20 <runtime::delete_slice:proc(array:[]u8,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>:
  40be20:	48 83 ec 68          	sub    $0x68,%rsp
  40be24:	4c 89 0c 24          	mov    %r9,(%rsp)
  40be28:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40be2d:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40be32:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  40be37:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40be3c:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40be41:	4c 8b 0c 24          	mov    (%rsp),%r9
  40be45:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40be4a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40be4f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40be54:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40be59:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40be5e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  40be63:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  40be68:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40be6d:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40be72:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40be77:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40be7c:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40be81:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40be86:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40be8b:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40be90:	e8 eb 08 00 00       	call   40c780 <runtime::mem_free_with_size>
  40be95:	48 83 c4 68          	add    $0x68,%rsp
  40be99:	c3                   	ret
  40be9a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

000000000040bea0 <runtime::delete_slice:proc(array:[]string,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>:
  40bea0:	48 83 ec 68          	sub    $0x68,%rsp
  40bea4:	4c 89 0c 24          	mov    %r9,(%rsp)
  40bea8:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40bead:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40beb2:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  40beb7:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40bebc:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40bec1:	4c 8b 0c 24          	mov    (%rsp),%r9
  40bec5:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40beca:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40becf:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40bed4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40bed9:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40bede:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  40bee3:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  40bee8:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40beed:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40bef2:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40bef7:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40befc:	48 c1 e6 04          	shl    $0x4,%rsi
  40bf00:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40bf05:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40bf0a:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40bf0f:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40bf14:	e8 67 08 00 00       	call   40c780 <runtime::mem_free_with_size>
  40bf19:	48 83 c4 68          	add    $0x68,%rsp
  40bf1d:	c3                   	ret
  40bf1e:	66 90                	xchg   %ax,%ax

000000000040bf20 <runtime::_make_aligned_type_erased>:
  40bf20:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  40bf27:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40bf2c:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40bf31:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40bf36:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40bf3b:	4c 89 4c 24 38       	mov    %r9,0x38(%rsp)
  40bf40:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  40bf45:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40bf4c:	00 
  40bf4d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40bf52:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40bf59:	00 
  40bf5a:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40bf5f:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40bf64:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40bf69:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40bf6e:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40bf73:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40bf78:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40bf7d:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  40bf82:	4c 89 8c 24 d0 00 00 	mov    %r9,0xd0(%rsp)
  40bf89:	00 
  40bf8a:	4c 89 84 24 c8 00 00 	mov    %r8,0xc8(%rsp)
  40bf91:	00 
  40bf92:	48 89 b4 24 c0 00 00 	mov    %rsi,0xc0(%rsp)
  40bf99:	00 
  40bf9a:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  40bfa1:	00 
  40bfa2:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  40bfa9:	00 
  40bfaa:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40bfb1:	00 
  40bfb2:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  40bfb9:	00 
  40bfba:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40bfbf:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40bfc6:	00 
  40bfc7:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  40bfcc:	e8 3f ec ff ff       	call   40ac10 <runtime::make_slice_error_loc>
  40bfd1:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40bfd6:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40bfdb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40bfe0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40bfe5:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40bfea:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40bfef:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40bff4:	48 0f af fa          	imul   %rdx,%rdi
  40bff8:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  40bfff:	00 
  40c000:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40c007:	00 
  40c008:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40c00f:	00 
  40c010:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40c017:	00 
  40c018:	0f 57 c0             	xorps  %xmm0,%xmm0
  40c01b:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40c022:	00 
  40c023:	48 89 e0             	mov    %rsp,%rax
  40c026:	4c 89 08             	mov    %r9,(%rax)
  40c029:	4c 8d 8c 24 80 00 00 	lea    0x80(%rsp),%r9
  40c030:	00 
  40c031:	e8 2a 04 00 00       	call   40c460 <runtime::mem_alloc_bytes>
  40c036:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  40c03d:	00 
  40c03e:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  40c045:	00 
  40c046:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40c04b:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40c050:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  40c054:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  40c05a:	0f 94 c0             	sete   %al
  40c05d:	24 01                	and    $0x1,%al
  40c05f:	3c 00                	cmp    $0x0,%al
  40c061:	74 1e                	je     40c081 <runtime::_make_aligned_type_erased+0x161>
  40c063:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40c068:	48 83 f8 00          	cmp    $0x0,%rax
  40c06c:	0f 95 c0             	setne  %al
  40c06f:	24 01                	and    $0x1,%al
  40c071:	3c 00                	cmp    $0x0,%al
  40c073:	74 0c                	je     40c081 <runtime::_make_aligned_type_erased+0x161>
  40c075:	8a 44 24 6f          	mov    0x6f(%rsp),%al
  40c079:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40c080:	c3                   	ret
  40c081:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40c086:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40c08b:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40c090:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40c095:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  40c09a:	48 89 32             	mov    %rsi,(%rdx)
  40c09d:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40c0a2:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40c0a7:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40c0ab:	8a 44 24 6f          	mov    0x6f(%rsp),%al
  40c0af:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40c0b6:	c3                   	ret
  40c0b7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40c0be:	00 00 

000000000040c0c0 <runtime::make_slice:proc(T:$[]u8,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]u8,err:runtime::Allocator_Error)>:
  40c0c0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40c0c7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40c0cc:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40c0d1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40c0d6:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40c0db:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40c0e0:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40c0e5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40c0ea:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40c0ef:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40c0f4:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40c0f9:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40c0fe:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40c105:	00 
  40c106:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  40c10b:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40c110:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c115:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  40c11a:	0f 57 c0             	xorps  %xmm0,%xmm0
  40c11d:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  40c122:	c6 44 24 5f 00       	movb   $0x0,0x5f(%rsp)
  40c127:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  40c12c:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40c131:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  40c136:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40c13b:	48 89 e0             	mov    %rsp,%rax
  40c13e:	48 89 70 08          	mov    %rsi,0x8(%rax)
  40c142:	48 89 08             	mov    %rcx,(%rax)
  40c145:	48 8d 7c 24 60       	lea    0x60(%rsp),%rdi
  40c14a:	b9 01 00 00 00       	mov    $0x1,%ecx
  40c14f:	48 89 ce             	mov    %rcx,%rsi
  40c152:	e8 c9 fd ff ff       	call   40bf20 <runtime::_make_aligned_type_erased>
  40c157:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c15c:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  40c160:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40c165:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  40c16a:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40c16e:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  40c173:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  40c178:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  40c17c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40c180:	48 89 11             	mov    %rdx,(%rcx)
  40c183:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c18a:	c3                   	ret
  40c18b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040c190 <runtime::assert>:
  40c190:	48 83 ec 48          	sub    $0x48,%rsp
  40c194:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40c199:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40c19e:	40 88 f8             	mov    %dil,%al
  40c1a1:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  40c1a5:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40c1aa:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40c1af:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  40c1b3:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40c1b8:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40c1bd:	88 44 24 47          	mov    %al,0x47(%rsp)
  40c1c1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  40c1c6:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40c1cb:	3c 00                	cmp    $0x0,%al
  40c1cd:	75 19                	jne    40c1e8 <runtime::assert+0x58>
  40c1cf:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40c1d4:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40c1d9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40c1de:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40c1e3:	e8 c8 00 00 00       	call   40c2b0 <runtime::assert.internal-0>
  40c1e8:	48 83 c4 48          	add    $0x48,%rsp
  40c1ec:	c3                   	ret
  40c1ed:	0f 1f 00             	nopl   (%rax)

000000000040c1f0 <runtime::panic>:
  40c1f0:	48 83 ec 38          	sub    $0x38,%rsp
  40c1f4:	48 89 0c 24          	mov    %rcx,(%rsp)
  40c1f8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40c1fd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40c202:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40c207:	48 8b 04 24          	mov    (%rsp),%rax
  40c20b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40c210:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40c215:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40c21a:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40c21f:	48 8b 40 20          	mov    0x20(%rax),%rax
  40c223:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c228:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  40c22e:	0f 94 c0             	sete   %al
  40c231:	24 01                	and    $0x1,%al
  40c233:	3c 00                	cmp    $0x0,%al
  40c235:	74 0c                	je     40c243 <runtime::panic+0x53>
  40c237:	48 c7 c0 70 b1 40 00 	mov    $0x40b170,%rax
  40c23e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c243:	4c 8b 0c 24          	mov    (%rsp),%r9
  40c247:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40c24c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40c251:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40c256:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40c25b:	bf 70 ef 40 00       	mov    $0x40ef70,%edi
  40c260:	be 05 00 00 00       	mov    $0x5,%esi
  40c265:	ff d0                	call   *%rax
  40c267:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40c26e:	00 00 

000000000040c270 <runtime::panic_contextless>:
  40c270:	48 83 ec 28          	sub    $0x28,%rsp
  40c274:	48 89 14 24          	mov    %rdx,(%rsp)
  40c278:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40c27d:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40c282:	4c 8b 04 24          	mov    (%rsp),%r8
  40c286:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40c28b:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40c290:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  40c295:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40c29a:	bf 70 ef 40 00       	mov    $0x40ef70,%edi
  40c29f:	be 05 00 00 00       	mov    $0x5,%esi
  40c2a4:	e8 17 ef ff ff       	call   40b1c0 <runtime::default_assertion_contextless_failure_proc>
  40c2a9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

000000000040c2b0 <runtime::assert.internal-0>:
  40c2b0:	48 83 ec 38          	sub    $0x38,%rsp
  40c2b4:	48 89 0c 24          	mov    %rcx,(%rsp)
  40c2b8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40c2bd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40c2c2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40c2c7:	48 8b 04 24          	mov    (%rsp),%rax
  40c2cb:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40c2d0:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40c2d5:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40c2da:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40c2df:	48 8b 40 20          	mov    0x20(%rax),%rax
  40c2e3:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c2e8:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  40c2ee:	0f 94 c0             	sete   %al
  40c2f1:	24 01                	and    $0x1,%al
  40c2f3:	3c 00                	cmp    $0x0,%al
  40c2f5:	74 0c                	je     40c303 <runtime::assert.internal-0+0x53>
  40c2f7:	48 c7 c0 70 b1 40 00 	mov    $0x40b170,%rax
  40c2fe:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c303:	4c 8b 0c 24          	mov    (%rsp),%r9
  40c307:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40c30c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40c311:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40c316:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40c31b:	bf 76 ef 40 00       	mov    $0x40ef76,%edi
  40c320:	be 11 00 00 00       	mov    $0x11,%esi
  40c325:	ff d0                	call   *%rax
  40c327:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40c32e:	00 00 

000000000040c330 <runtime::[internal.odin]::byte_slice>:
  40c330:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40c335:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40c33a:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  40c33f:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40c344:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40c349:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  40c34e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40c353:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40c358:	31 c0                	xor    %eax,%eax
  40c35a:	48 85 d2             	test   %rdx,%rdx
  40c35d:	48 0f 49 c2          	cmovns %rdx,%rax
  40c361:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  40c366:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40c36b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40c370:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40c375:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40c37a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40c37f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40c384:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  40c389:	c3                   	ret
  40c38a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

000000000040c390 <runtime::is_power_of_two_int>:
  40c390:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  40c395:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40c39a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40c39f:	48 83 f8 00          	cmp    $0x0,%rax
  40c3a3:	0f 9e c0             	setle  %al
  40c3a6:	24 01                	and    $0x1,%al
  40c3a8:	3c 00                	cmp    $0x0,%al
  40c3aa:	74 03                	je     40c3af <runtime::is_power_of_two_int+0x1f>
  40c3ac:	31 c0                	xor    %eax,%eax
  40c3ae:	c3                   	ret
  40c3af:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40c3b4:	48 89 c1             	mov    %rax,%rcx
  40c3b7:	48 83 e9 01          	sub    $0x1,%rcx
  40c3bb:	48 21 c8             	and    %rcx,%rax
  40c3be:	48 83 f8 00          	cmp    $0x0,%rax
  40c3c2:	0f 94 c0             	sete   %al
  40c3c5:	24 01                	and    $0x1,%al
  40c3c7:	c3                   	ret
  40c3c8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40c3cf:	00 

000000000040c3d0 <runtime::mem_copy_non_overlapping>:
  40c3d0:	48 83 ec 38          	sub    $0x38,%rsp
  40c3d4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40c3d9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40c3de:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  40c3e3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40c3e8:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c3ed:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40c3f2:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40c3f7:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40c3fc:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40c401:	48 83 f8 00          	cmp    $0x0,%rax
  40c405:	0f 95 c0             	setne  %al
  40c408:	24 01                	and    $0x1,%al
  40c40a:	3c 00                	cmp    $0x0,%al
  40c40c:	74 3c                	je     40c44a <runtime::mem_copy_non_overlapping+0x7a>
  40c40e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40c413:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40c418:	48 39 c8             	cmp    %rcx,%rax
  40c41b:	0f 95 c0             	setne  %al
  40c41e:	24 01                	and    $0x1,%al
  40c420:	3c 00                	cmp    $0x0,%al
  40c422:	74 26                	je     40c44a <runtime::mem_copy_non_overlapping+0x7a>
  40c424:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40c429:	48 83 f8 00          	cmp    $0x0,%rax
  40c42d:	0f 9f c0             	setg   %al
  40c430:	24 01                	and    $0x1,%al
  40c432:	3c 00                	cmp    $0x0,%al
  40c434:	74 14                	je     40c44a <runtime::mem_copy_non_overlapping+0x7a>
  40c436:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40c43b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40c440:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40c445:	e8 16 4c ff ff       	call   401060 <memcpy@plt>
  40c44a:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40c44f:	48 83 c4 38          	add    $0x38,%rsp
  40c453:	c3                   	ret
  40c454:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40c45b:	00 00 00 00 00 

000000000040c460 <runtime::mem_alloc_bytes>:
  40c460:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40c467:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40c46c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40c471:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40c476:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40c47b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40c480:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40c485:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40c48c:	00 
  40c48d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40c492:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40c497:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40c49c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40c4a1:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40c4a6:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40c4ad:	00 
  40c4ae:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  40c4b3:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  40c4b8:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40c4bd:	e8 ce fe ff ff       	call   40c390 <runtime::is_power_of_two_int>
  40c4c2:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40c4c7:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40c4cc:	0f b6 f8             	movzbl %al,%edi
  40c4cf:	be 88 ef 40 00       	mov    $0x40ef88,%esi
  40c4d4:	ba 20 00 00 00       	mov    $0x20,%edx
  40c4d9:	e8 b2 fc ff ff       	call   40c190 <runtime::assert>
  40c4de:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40c4e3:	48 83 f8 00          	cmp    $0x0,%rax
  40c4e7:	0f 94 c0             	sete   %al
  40c4ea:	24 01                	and    $0x1,%al
  40c4ec:	3c 00                	cmp    $0x0,%al
  40c4ee:	75 0f                	jne    40c4ff <runtime::mem_alloc_bytes+0x9f>
  40c4f0:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  40c4f6:	0f 94 c0             	sete   %al
  40c4f9:	24 01                	and    $0x1,%al
  40c4fb:	3c 00                	cmp    $0x0,%al
  40c4fd:	74 1b                	je     40c51a <runtime::mem_alloc_bytes+0xba>
  40c4ff:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40c504:	31 f6                	xor    %esi,%esi
  40c506:	ba 10 00 00 00       	mov    $0x10,%edx
  40c50b:	e8 30 4b ff ff       	call   401040 <memset@plt>
  40c510:	31 c0                	xor    %eax,%eax
  40c512:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c519:	c3                   	ret
  40c51a:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40c51f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40c524:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  40c529:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40c52e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40c533:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40c538:	0f 57 c0             	xorps  %xmm0,%xmm0
  40c53b:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40c540:	48 89 e6             	mov    %rsp,%rsi
  40c543:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  40c547:	4c 8d 4c 24 50       	lea    0x50(%rsp),%r9
  40c54c:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  40c550:	4c 89 06             	mov    %r8,(%rsi)
  40c553:	31 f6                	xor    %esi,%esi
  40c555:	41 89 f1             	mov    %esi,%r9d
  40c558:	4d 89 c8             	mov    %r9,%r8
  40c55b:	ff d0                	call   *%rax
  40c55d:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c562:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c567:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  40c56c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40c570:	48 89 11             	mov    %rdx,(%rcx)
  40c573:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c57a:	c3                   	ret
  40c57b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040c580 <runtime::mem_alloc>:
  40c580:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40c587:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40c58c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40c591:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40c596:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40c59b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40c5a0:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40c5a5:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40c5ac:	00 
  40c5ad:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40c5b2:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40c5b7:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40c5bc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40c5c1:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40c5c6:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40c5cd:	00 
  40c5ce:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  40c5d3:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  40c5d8:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40c5dd:	e8 ae fd ff ff       	call   40c390 <runtime::is_power_of_two_int>
  40c5e2:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40c5e7:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40c5ec:	0f b6 f8             	movzbl %al,%edi
  40c5ef:	be 88 ef 40 00       	mov    $0x40ef88,%esi
  40c5f4:	ba 20 00 00 00       	mov    $0x20,%edx
  40c5f9:	e8 92 fb ff ff       	call   40c190 <runtime::assert>
  40c5fe:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40c603:	48 83 f8 00          	cmp    $0x0,%rax
  40c607:	0f 94 c0             	sete   %al
  40c60a:	24 01                	and    $0x1,%al
  40c60c:	3c 00                	cmp    $0x0,%al
  40c60e:	75 0f                	jne    40c61f <runtime::mem_alloc+0x9f>
  40c610:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  40c616:	0f 94 c0             	sete   %al
  40c619:	24 01                	and    $0x1,%al
  40c61b:	3c 00                	cmp    $0x0,%al
  40c61d:	74 1b                	je     40c63a <runtime::mem_alloc+0xba>
  40c61f:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40c624:	31 f6                	xor    %esi,%esi
  40c626:	ba 10 00 00 00       	mov    $0x10,%edx
  40c62b:	e8 10 4a ff ff       	call   401040 <memset@plt>
  40c630:	31 c0                	xor    %eax,%eax
  40c632:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c639:	c3                   	ret
  40c63a:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40c63f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40c644:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  40c649:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40c64e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40c653:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40c658:	0f 57 c0             	xorps  %xmm0,%xmm0
  40c65b:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40c660:	48 89 e6             	mov    %rsp,%rsi
  40c663:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  40c667:	4c 8d 4c 24 50       	lea    0x50(%rsp),%r9
  40c66c:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  40c670:	4c 89 06             	mov    %r8,(%rsi)
  40c673:	31 f6                	xor    %esi,%esi
  40c675:	41 89 f1             	mov    %esi,%r9d
  40c678:	4d 89 c8             	mov    %r9,%r8
  40c67b:	ff d0                	call   *%rax
  40c67d:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c682:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c687:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  40c68c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40c690:	48 89 11             	mov    %rdx,(%rcx)
  40c693:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c69a:	c3                   	ret
  40c69b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040c6a0 <runtime::mem_free>:
  40c6a0:	53                   	push   %rbx
  40c6a1:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  40c6a8:	4c 89 44 24 30       	mov    %r8,0x30(%rsp)
  40c6ad:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40c6b2:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40c6b7:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40c6bc:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  40c6c1:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40c6c6:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40c6cb:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40c6d0:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  40c6d7:	00 
  40c6d8:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40c6dd:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40c6e2:	48 83 f8 00          	cmp    $0x0,%rax
  40c6e6:	0f 94 c0             	sete   %al
  40c6e9:	24 01                	and    $0x1,%al
  40c6eb:	3c 00                	cmp    $0x0,%al
  40c6ed:	75 0f                	jne    40c6fe <runtime::mem_free+0x5e>
  40c6ef:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  40c6f5:	0f 94 c0             	sete   %al
  40c6f8:	24 01                	and    $0x1,%al
  40c6fa:	3c 00                	cmp    $0x0,%al
  40c6fc:	74 0b                	je     40c709 <runtime::mem_free+0x69>
  40c6fe:	31 c0                	xor    %eax,%eax
  40c700:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40c707:	5b                   	pop    %rbx
  40c708:	c3                   	ret
  40c709:	48 8b 5c 24 38       	mov    0x38(%rsp),%rbx
  40c70e:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c713:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40c718:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40c71d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c722:	48 8d 7c 24 60       	lea    0x60(%rsp),%rdi
  40c727:	31 f6                	xor    %esi,%esi
  40c729:	ba 10 00 00 00       	mov    $0x10,%edx
  40c72e:	e8 0d 49 ff ff       	call   401040 <memset@plt>
  40c733:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40c738:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  40c73d:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  40c742:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40c747:	be 01 00 00 00       	mov    $0x1,%esi
  40c74c:	31 c9                	xor    %ecx,%ecx
  40c74e:	41 89 c9             	mov    %ecx,%r9d
  40c751:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  40c756:	4c 89 ca             	mov    %r9,%rdx
  40c759:	4c 89 c9             	mov    %r9,%rcx
  40c75c:	48 89 1c 24          	mov    %rbx,(%rsp)
  40c760:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  40c765:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  40c76a:	ff d0                	call   *%rax
  40c76c:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  40c770:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40c774:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40c77b:	5b                   	pop    %rbx
  40c77c:	c3                   	ret
  40c77d:	0f 1f 00             	nopl   (%rax)

000000000040c780 <runtime::mem_free_with_size>:
  40c780:	53                   	push   %rbx
  40c781:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  40c788:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40c78d:	4c 89 44 24 30       	mov    %r8,0x30(%rsp)
  40c792:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40c797:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40c79c:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40c7a1:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40c7a6:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40c7ab:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40c7b0:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40c7b5:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40c7ba:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  40c7c1:	00 
  40c7c2:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40c7c9:	00 
  40c7ca:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40c7cf:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40c7d4:	48 83 f8 00          	cmp    $0x0,%rax
  40c7d8:	0f 94 c0             	sete   %al
  40c7db:	24 01                	and    $0x1,%al
  40c7dd:	3c 00                	cmp    $0x0,%al
  40c7df:	75 0f                	jne    40c7f0 <runtime::mem_free_with_size+0x70>
  40c7e1:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  40c7e7:	0f 94 c0             	sete   %al
  40c7ea:	24 01                	and    $0x1,%al
  40c7ec:	3c 00                	cmp    $0x0,%al
  40c7ee:	74 0b                	je     40c7fb <runtime::mem_free_with_size+0x7b>
  40c7f0:	31 c0                	xor    %eax,%eax
  40c7f2:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40c7f9:	5b                   	pop    %rbx
  40c7fa:	c3                   	ret
  40c7fb:	48 8b 5c 24 30       	mov    0x30(%rsp),%rbx
  40c800:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c805:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c80a:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40c80f:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40c814:	48 8d 7c 24 60       	lea    0x60(%rsp),%rdi
  40c819:	31 f6                	xor    %esi,%esi
  40c81b:	ba 10 00 00 00       	mov    $0x10,%edx
  40c820:	e8 1b 48 ff ff       	call   401040 <memset@plt>
  40c825:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40c82a:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  40c82f:	4c 8b 4c 24 38       	mov    0x38(%rsp),%r9
  40c834:	4c 8b 54 24 28       	mov    0x28(%rsp),%r10
  40c839:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40c83e:	be 01 00 00 00       	mov    $0x1,%esi
  40c843:	31 c9                	xor    %ecx,%ecx
  40c845:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  40c84a:	48 89 ca             	mov    %rcx,%rdx
  40c84d:	48 89 1c 24          	mov    %rbx,(%rsp)
  40c851:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  40c856:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  40c85b:	ff d0                	call   *%rax
  40c85d:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  40c861:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40c865:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40c86c:	5b                   	pop    %rbx
  40c86d:	c3                   	ret
  40c86e:	66 90                	xchg   %ax,%ax

000000000040c870 <runtime::conditional_mem_zero>:
  40c870:	48 83 ec 30          	sub    $0x30,%rsp
  40c874:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  40c879:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  40c87e:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40c883:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40c888:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40c88d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c892:	48 83 f8 00          	cmp    $0x0,%rax
  40c896:	0f 9e c0             	setle  %al
  40c899:	24 01                	and    $0x1,%al
  40c89b:	3c 00                	cmp    $0x0,%al
  40c89d:	74 05                	je     40c8a4 <runtime::conditional_mem_zero+0x34>
  40c89f:	48 83 c4 30          	add    $0x30,%rsp
  40c8a3:	c3                   	ret
  40c8a4:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  40c8a9:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40c8ae:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40c8b3:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c8b8:	48 c1 e9 03          	shr    $0x3,%rcx
  40c8bc:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40c8c1:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  40c8c6:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40c8cb:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40c8d0:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  40c8d5:	48 89 0c 24          	mov    %rcx,(%rsp)
  40c8d9:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40c8de:	48 8b 14 24          	mov    (%rsp),%rdx
  40c8e2:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  40c8e7:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40c8ec:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40c8f1:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40c8f6:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40c8fb:	48 89 f2             	mov    %rsi,%rdx
  40c8fe:	48 c1 e2 03          	shl    $0x3,%rdx
  40c902:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40c907:	48 8d 0c f1          	lea    (%rcx,%rsi,8),%rcx
  40c90b:	48 29 d0             	sub    %rdx,%rax
  40c90e:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40c913:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40c918:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40c91d:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40c922:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  40c927:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  40c92c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40c931:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40c936:	48 c7 44 24 b0 ff ff 	movq   $0xffffffffffffffff,-0x50(%rsp)
  40c93d:	ff ff 
  40c93f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40c944:	48 83 c0 01          	add    $0x1,%rax
  40c948:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40c94d:	48 3b 44 24 b8       	cmp    -0x48(%rsp),%rax
  40c952:	7d 38                	jge    40c98c <runtime::conditional_mem_zero+0x11c>
  40c954:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40c959:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40c95e:	48 89 ce             	mov    %rcx,%rsi
  40c961:	48 c1 e6 03          	shl    $0x3,%rsi
  40c965:	48 89 c2             	mov    %rax,%rdx
  40c968:	48 01 f2             	add    %rsi,%rdx
  40c96b:	48 89 54 24 88       	mov    %rdx,-0x78(%rsp)
  40c970:	48 83 3c c8 00       	cmpq   $0x0,(%rax,%rcx,8)
  40c975:	0f 95 c0             	setne  %al
  40c978:	24 01                	and    $0x1,%al
  40c97a:	3c 00                	cmp    $0x0,%al
  40c97c:	74 0c                	je     40c98a <runtime::conditional_mem_zero+0x11a>
  40c97e:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40c983:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40c98a:	eb b3                	jmp    40c93f <runtime::conditional_mem_zero+0xcf>
  40c98c:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40c991:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40c996:	48 c7 44 24 a0 ff ff 	movq   $0xffffffffffffffff,-0x60(%rsp)
  40c99d:	ff ff 
  40c99f:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40c9a4:	48 83 c0 01          	add    $0x1,%rax
  40c9a8:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40c9ad:	48 3b 44 24 a8       	cmp    -0x58(%rsp),%rax
  40c9b2:	7d 2c                	jge    40c9e0 <runtime::conditional_mem_zero+0x170>
  40c9b4:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  40c9b9:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40c9be:	48 89 c2             	mov    %rax,%rdx
  40c9c1:	48 01 ca             	add    %rcx,%rdx
  40c9c4:	48 89 54 24 80       	mov    %rdx,-0x80(%rsp)
  40c9c9:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  40c9cd:	0f 95 c0             	setne  %al
  40c9d0:	24 01                	and    $0x1,%al
  40c9d2:	3c 00                	cmp    $0x0,%al
  40c9d4:	74 08                	je     40c9de <runtime::conditional_mem_zero+0x16e>
  40c9d6:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  40c9db:	c6 00 00             	movb   $0x0,(%rax)
  40c9de:	eb bf                	jmp    40c99f <runtime::conditional_mem_zero+0x12f>
  40c9e0:	48 83 c4 30          	add    $0x30,%rsp
  40c9e4:	c3                   	ret
  40c9e5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40c9ec:	00 00 00 00 

000000000040c9f0 <runtime::memory_equal>:
  40c9f0:	48 83 ec 28          	sub    $0x28,%rsp
  40c9f4:	48 89 7c 24 88       	mov    %rdi,-0x78(%rsp)
  40c9f9:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  40c9fe:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  40ca03:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40ca08:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40ca0d:	48 8b 54 24 88       	mov    -0x78(%rsp),%rdx
  40ca12:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40ca17:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40ca1c:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40ca21:	48 83 f8 00          	cmp    $0x0,%rax
  40ca25:	0f 94 c1             	sete   %cl
  40ca28:	80 e1 01             	and    $0x1,%cl
  40ca2b:	b0 01                	mov    $0x1,%al
  40ca2d:	38 c8                	cmp    %cl,%al
  40ca2f:	74 1b                	je     40ca4c <runtime::memory_equal+0x5c>
  40ca31:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40ca36:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40ca3b:	48 39 c8             	cmp    %rcx,%rax
  40ca3e:	0f 94 c1             	sete   %cl
  40ca41:	80 e1 01             	and    $0x1,%cl
  40ca44:	b0 01                	mov    $0x1,%al
  40ca46:	38 c8                	cmp    %cl,%al
  40ca48:	74 0b                	je     40ca55 <runtime::memory_equal+0x65>
  40ca4a:	eb 07                	jmp    40ca53 <runtime::memory_equal+0x63>
  40ca4c:	b0 01                	mov    $0x1,%al
  40ca4e:	48 83 c4 28          	add    $0x28,%rsp
  40ca52:	c3                   	ret
  40ca53:	eb 07                	jmp    40ca5c <runtime::memory_equal+0x6c>
  40ca55:	b0 01                	mov    $0x1,%al
  40ca57:	48 83 c4 28          	add    $0x28,%rsp
  40ca5b:	c3                   	ret
  40ca5c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40ca61:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40ca66:	48 8b 54 24 88       	mov    -0x78(%rsp),%rdx
  40ca6b:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40ca70:	48 89 0c 24          	mov    %rcx,(%rsp)
  40ca74:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40ca79:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  40ca80:	00 00 
  40ca82:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  40ca89:	00 00 
  40ca8b:	48 83 7c 24 f8 08    	cmpq   $0x8,-0x8(%rsp)
  40ca91:	0f 93 c0             	setae  %al
  40ca94:	24 01                	and    $0x1,%al
  40ca96:	3c 00                	cmp    $0x0,%al
  40ca98:	0f 84 43 01 00 00    	je     40cbe1 <runtime::memory_equal+0x1f1>
  40ca9e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40caa3:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  40caa8:	48 c1 e8 04          	shr    $0x4,%rax
  40caac:	48 c1 e0 04          	shl    $0x4,%rax
  40cab0:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40cab5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40caba:	48 3b 44 24 e8       	cmp    -0x18(%rsp),%rax
  40cabf:	0f 92 c0             	setb   %al
  40cac2:	24 01                	and    $0x1,%al
  40cac4:	3c 00                	cmp    $0x0,%al
  40cac6:	0f 84 9a 00 00 00    	je     40cb66 <runtime::memory_equal+0x176>
  40cacc:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40cad1:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40cad6:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40cada:	0f 29 44 24 d0       	movaps %xmm0,-0x30(%rsp)
  40cadf:	48 8b 04 24          	mov    (%rsp),%rax
  40cae3:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40cae8:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40caec:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  40caf1:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40caf6:	0f 28 4c 24 c0       	movaps -0x40(%rsp),%xmm1
  40cafb:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  40caff:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40cb03:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40cb07:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  40cb0c:	0f 28 44 24 b0       	movaps -0x50(%rsp),%xmm0
  40cb11:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40cb16:	66 0f eb c1          	por    %xmm1,%xmm0
  40cb1a:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40cb1f:	66 0f eb c1          	por    %xmm1,%xmm0
  40cb23:	0f 28 c8             	movaps %xmm0,%xmm1
  40cb26:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40cb2b:	66 0f eb c1          	por    %xmm1,%xmm0
  40cb2f:	0f 28 c8             	movaps %xmm0,%xmm1
  40cb32:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40cb37:	66 0f eb c1          	por    %xmm1,%xmm0
  40cb3b:	66 0f 7e c0          	movd   %xmm0,%eax
  40cb3f:	3c 00                	cmp    $0x0,%al
  40cb41:	0f 95 c0             	setne  %al
  40cb44:	24 01                	and    $0x1,%al
  40cb46:	3c 00                	cmp    $0x0,%al
  40cb48:	74 07                	je     40cb51 <runtime::memory_equal+0x161>
  40cb4a:	31 c0                	xor    %eax,%eax
  40cb4c:	48 83 c4 28          	add    $0x28,%rsp
  40cb50:	c3                   	ret
  40cb51:	eb 00                	jmp    40cb53 <runtime::memory_equal+0x163>
  40cb53:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cb58:	48 83 c0 10          	add    $0x10,%rax
  40cb5c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40cb61:	e9 4f ff ff ff       	jmp    40cab5 <runtime::memory_equal+0xc5>
  40cb66:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40cb6b:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  40cb70:	48 c1 e8 03          	shr    $0x3,%rax
  40cb74:	48 c1 e0 03          	shl    $0x3,%rax
  40cb78:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40cb7d:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cb82:	48 3b 44 24 e8       	cmp    -0x18(%rsp),%rax
  40cb87:	0f 92 c0             	setb   %al
  40cb8a:	24 01                	and    $0x1,%al
  40cb8c:	3c 00                	cmp    $0x0,%al
  40cb8e:	74 4f                	je     40cbdf <runtime::memory_equal+0x1ef>
  40cb90:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40cb95:	48 03 44 24 f0       	add    -0x10(%rsp),%rax
  40cb9a:	48 8b 00             	mov    (%rax),%rax
  40cb9d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40cba2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40cba7:	48 8b 0c 24          	mov    (%rsp),%rcx
  40cbab:	48 03 4c 24 f0       	add    -0x10(%rsp),%rcx
  40cbb0:	48 8b 09             	mov    (%rcx),%rcx
  40cbb3:	48 89 4c 24 a0       	mov    %rcx,-0x60(%rsp)
  40cbb8:	48 3b 44 24 a0       	cmp    -0x60(%rsp),%rax
  40cbbd:	0f 95 c0             	setne  %al
  40cbc0:	24 01                	and    $0x1,%al
  40cbc2:	3c 00                	cmp    $0x0,%al
  40cbc4:	74 07                	je     40cbcd <runtime::memory_equal+0x1dd>
  40cbc6:	31 c0                	xor    %eax,%eax
  40cbc8:	48 83 c4 28          	add    $0x28,%rsp
  40cbcc:	c3                   	ret
  40cbcd:	eb 00                	jmp    40cbcf <runtime::memory_equal+0x1df>
  40cbcf:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cbd4:	48 83 c0 08          	add    $0x8,%rax
  40cbd8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40cbdd:	eb 9e                	jmp    40cb7d <runtime::memory_equal+0x18d>
  40cbdf:	eb 00                	jmp    40cbe1 <runtime::memory_equal+0x1f1>
  40cbe1:	eb 00                	jmp    40cbe3 <runtime::memory_equal+0x1f3>
  40cbe3:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cbe8:	48 3b 44 24 f8       	cmp    -0x8(%rsp),%rax
  40cbed:	0f 92 c0             	setb   %al
  40cbf0:	24 01                	and    $0x1,%al
  40cbf2:	3c 00                	cmp    $0x0,%al
  40cbf4:	74 3b                	je     40cc31 <runtime::memory_equal+0x241>
  40cbf6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40cbfb:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40cc00:	8a 04 08             	mov    (%rax,%rcx,1),%al
  40cc03:	48 8b 0c 24          	mov    (%rsp),%rcx
  40cc07:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40cc0c:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  40cc0f:	0f 95 c0             	setne  %al
  40cc12:	24 01                	and    $0x1,%al
  40cc14:	3c 00                	cmp    $0x0,%al
  40cc16:	74 07                	je     40cc1f <runtime::memory_equal+0x22f>
  40cc18:	31 c0                	xor    %eax,%eax
  40cc1a:	48 83 c4 28          	add    $0x28,%rsp
  40cc1e:	c3                   	ret
  40cc1f:	eb 00                	jmp    40cc21 <runtime::memory_equal+0x231>
  40cc21:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cc26:	48 83 c0 01          	add    $0x1,%rax
  40cc2a:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40cc2f:	eb b2                	jmp    40cbe3 <runtime::memory_equal+0x1f3>
  40cc31:	b0 01                	mov    $0x1,%al
  40cc33:	48 83 c4 28          	add    $0x28,%rsp
  40cc37:	c3                   	ret
  40cc38:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40cc3f:	00 

000000000040cc40 <runtime::memory_compare>:
  40cc40:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
  40cc47:	48 89 7c 24 98       	mov    %rdi,-0x68(%rsp)
  40cc4c:	48 89 74 24 a0       	mov    %rsi,-0x60(%rsp)
  40cc51:	48 89 54 24 a8       	mov    %rdx,-0x58(%rsp)
  40cc56:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40cc5b:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  40cc60:	48 8b 54 24 a8       	mov    -0x58(%rsp),%rdx
  40cc65:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40cc6c:	00 
  40cc6d:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40cc74:	00 
  40cc75:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40cc7c:	00 
  40cc7d:	48 39 c8             	cmp    %rcx,%rax
  40cc80:	0f 94 c1             	sete   %cl
  40cc83:	80 e1 01             	and    $0x1,%cl
  40cc86:	b0 01                	mov    $0x1,%al
  40cc88:	38 c8                	cmp    %cl,%al
  40cc8a:	74 17                	je     40cca3 <runtime::memory_compare+0x63>
  40cc8c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40cc91:	48 83 f8 00          	cmp    $0x0,%rax
  40cc95:	0f 94 c1             	sete   %cl
  40cc98:	80 e1 01             	and    $0x1,%cl
  40cc9b:	b0 01                	mov    $0x1,%al
  40cc9d:	38 c8                	cmp    %cl,%al
  40cc9f:	74 23                	je     40ccc4 <runtime::memory_compare+0x84>
  40cca1:	eb 0a                	jmp    40ccad <runtime::memory_compare+0x6d>
  40cca3:	31 c0                	xor    %eax,%eax
  40cca5:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40ccac:	c3                   	ret
  40ccad:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40ccb2:	48 83 f8 00          	cmp    $0x0,%rax
  40ccb6:	0f 94 c1             	sete   %cl
  40ccb9:	80 e1 01             	and    $0x1,%cl
  40ccbc:	b0 01                	mov    $0x1,%al
  40ccbe:	38 c8                	cmp    %cl,%al
  40ccc0:	74 13                	je     40ccd5 <runtime::memory_compare+0x95>
  40ccc2:	eb 0f                	jmp    40ccd3 <runtime::memory_compare+0x93>
  40ccc4:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40cccb:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40ccd2:	c3                   	ret
  40ccd3:	eb 0d                	jmp    40cce2 <runtime::memory_compare+0xa2>
  40ccd5:	b8 01 00 00 00       	mov    $0x1,%eax
  40ccda:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40cce1:	c3                   	ret
  40cce2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40cce7:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  40ccec:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  40ccf1:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40ccf6:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40ccfb:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40cd00:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40cd07:	00 00 
  40cd09:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  40cd10:	00 00 
  40cd12:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40cd17:	48 2b 44 24 60       	sub    0x60(%rsp),%rax
  40cd1c:	48 c1 e8 04          	shr    $0x4,%rax
  40cd20:	48 c1 e0 04          	shl    $0x4,%rax
  40cd24:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40cd29:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40cd2e:	48 3b 44 24 58       	cmp    0x58(%rsp),%rax
  40cd33:	0f 92 c0             	setb   %al
  40cd36:	24 01                	and    $0x1,%al
  40cd38:	3c 00                	cmp    $0x0,%al
  40cd3a:	0f 84 44 01 00 00    	je     40ce84 <runtime::memory_compare+0x244>
  40cd40:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40cd45:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40cd4a:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40cd4e:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40cd53:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40cd58:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40cd5d:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40cd61:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40cd66:	0f 28 44 24 40       	movaps 0x40(%rsp),%xmm0
  40cd6b:	0f 28 4c 24 30       	movaps 0x30(%rsp),%xmm1
  40cd70:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  40cd74:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40cd78:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40cd7c:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  40cd81:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  40cd86:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40cd8b:	66 0f eb c1          	por    %xmm1,%xmm0
  40cd8f:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40cd94:	66 0f eb c1          	por    %xmm1,%xmm0
  40cd98:	0f 28 c8             	movaps %xmm0,%xmm1
  40cd9b:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40cda0:	66 0f eb c1          	por    %xmm1,%xmm0
  40cda4:	0f 28 c8             	movaps %xmm0,%xmm1
  40cda7:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40cdac:	66 0f eb c1          	por    %xmm1,%xmm0
  40cdb0:	66 0f 7e c0          	movd   %xmm0,%eax
  40cdb4:	3c 00                	cmp    $0x0,%al
  40cdb6:	0f 95 c0             	setne  %al
  40cdb9:	24 01                	and    $0x1,%al
  40cdbb:	3c 00                	cmp    $0x0,%al
  40cdbd:	0f 84 ac 00 00 00    	je     40ce6f <runtime::memory_compare+0x22f>
  40cdc3:	66 0f 76 c0          	pcmpeqd %xmm0,%xmm0
  40cdc7:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  40cdcc:	0f 28 05 2d 19 00 00 	movaps 0x192d(%rip),%xmm0        # 40e700 <_IO_stdin_used+0x700>
  40cdd3:	0f 29 04 24          	movaps %xmm0,(%rsp)
  40cdd7:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  40cddc:	0f 28 0c 24          	movaps (%rsp),%xmm1
  40cde0:	0f 28 54 24 10       	movaps 0x10(%rsp),%xmm2
  40cde5:	0f 57 db             	xorps  %xmm3,%xmm3
  40cde8:	66 0f 74 c3          	pcmpeqb %xmm3,%xmm0
  40cdec:	66 0f 38 10 ca       	pblendvb %xmm0,%xmm2,%xmm1
  40cdf1:	0f 28 c1             	movaps %xmm1,%xmm0
  40cdf4:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  40cdf9:	0f 28 44 24 f0       	movaps -0x10(%rsp),%xmm0
  40cdfe:	0f 28 c8             	movaps %xmm0,%xmm1
  40ce01:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40ce06:	66 0f da c1          	pminub %xmm1,%xmm0
  40ce0a:	66 0f 38 41 c0       	phminposuw %xmm0,%xmm0
  40ce0f:	66 0f 7e c0          	movd   %xmm0,%eax
  40ce13:	0f b6 c0             	movzbl %al,%eax
  40ce16:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40ce1b:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40ce20:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40ce25:	48 03 4c 24 e8       	add    -0x18(%rsp),%rcx
  40ce2a:	8a 04 08             	mov    (%rax,%rcx,1),%al
  40ce2d:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40ce32:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40ce37:	48 03 54 24 e8       	add    -0x18(%rsp),%rdx
  40ce3c:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  40ce3f:	0f 92 c0             	setb   %al
  40ce42:	24 01                	and    $0x1,%al
  40ce44:	3c 00                	cmp    $0x0,%al
  40ce46:	74 0e                	je     40ce56 <runtime::memory_compare+0x216>
  40ce48:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40ce4f:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40ce54:	eb 0c                	jmp    40ce62 <runtime::memory_compare+0x222>
  40ce56:	b8 01 00 00 00       	mov    $0x1,%eax
  40ce5b:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40ce60:	eb 00                	jmp    40ce62 <runtime::memory_compare+0x222>
  40ce62:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  40ce67:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40ce6e:	c3                   	ret
  40ce6f:	eb 00                	jmp    40ce71 <runtime::memory_compare+0x231>
  40ce71:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40ce76:	48 83 c0 10          	add    $0x10,%rax
  40ce7a:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40ce7f:	e9 a5 fe ff ff       	jmp    40cd29 <runtime::memory_compare+0xe9>
  40ce84:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40ce89:	48 2b 44 24 60       	sub    0x60(%rsp),%rax
  40ce8e:	48 c1 e8 03          	shr    $0x3,%rax
  40ce92:	48 c1 e0 03          	shl    $0x3,%rax
  40ce96:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40ce9b:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40cea0:	48 3b 44 24 58       	cmp    0x58(%rsp),%rax
  40cea5:	0f 92 c0             	setb   %al
  40cea8:	24 01                	and    $0x1,%al
  40ceaa:	3c 00                	cmp    $0x0,%al
  40ceac:	0f 84 59 01 00 00    	je     40d00b <runtime::memory_compare+0x3cb>
  40ceb2:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40ceb7:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40cebc:	48 8b 04 08          	mov    (%rax,%rcx,1),%rax
  40cec0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40cec5:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40ceca:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40cecf:	48 8b 04 08          	mov    (%rax,%rcx,1),%rax
  40ced3:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40ced8:	f3 0f 7e 44 24 e0    	movq   -0x20(%rsp),%xmm0
  40cede:	f3 0f 7e 4c 24 d8    	movq   -0x28(%rsp),%xmm1
  40cee4:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  40cee8:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40ceec:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40cef0:	66 0f d6 44 24 d0    	movq   %xmm0,-0x30(%rsp)
  40cef6:	f3 0f 7e 44 24 d0    	movq   -0x30(%rsp),%xmm0
  40cefc:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40cf01:	66 0f eb c1          	por    %xmm1,%xmm0
  40cf05:	0f 28 c8             	movaps %xmm0,%xmm1
  40cf08:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40cf0d:	66 0f eb c1          	por    %xmm1,%xmm0
  40cf11:	0f 28 c8             	movaps %xmm0,%xmm1
  40cf14:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40cf19:	66 0f eb c1          	por    %xmm1,%xmm0
  40cf1d:	66 0f 7e c0          	movd   %xmm0,%eax
  40cf21:	3c 00                	cmp    $0x0,%al
  40cf23:	0f 95 c0             	setne  %al
  40cf26:	24 01                	and    $0x1,%al
  40cf28:	3c 00                	cmp    $0x0,%al
  40cf2a:	0f 84 c6 00 00 00    	je     40cff6 <runtime::memory_compare+0x3b6>
  40cf30:	48 c7 44 24 c8 ff ff 	movq   $0xffffffffffffffff,-0x38(%rsp)
  40cf37:	ff ff 
  40cf39:	48 b8 00 01 02 03 04 	movabs $0x706050403020100,%rax
  40cf40:	05 06 07 
  40cf43:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  40cf48:	f3 0f 7e 44 24 d0    	movq   -0x30(%rsp),%xmm0
  40cf4e:	f3 0f 7e 4c 24 c0    	movq   -0x40(%rsp),%xmm1
  40cf54:	f3 0f 7e 54 24 c8    	movq   -0x38(%rsp),%xmm2
  40cf5a:	0f 57 db             	xorps  %xmm3,%xmm3
  40cf5d:	66 0f 74 c3          	pcmpeqb %xmm3,%xmm0
  40cf61:	66 0f 38 10 ca       	pblendvb %xmm0,%xmm2,%xmm1
  40cf66:	0f 28 c1             	movaps %xmm1,%xmm0
  40cf69:	66 0f d6 44 24 b8    	movq   %xmm0,-0x48(%rsp)
  40cf6f:	f3 0f 7e 44 24 b8    	movq   -0x48(%rsp),%xmm0
  40cf75:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40cf7a:	66 0f da c1          	pminub %xmm1,%xmm0
  40cf7e:	0f 28 c8             	movaps %xmm0,%xmm1
  40cf81:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40cf86:	66 0f da c1          	pminub %xmm1,%xmm0
  40cf8a:	0f 28 c8             	movaps %xmm0,%xmm1
  40cf8d:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40cf92:	66 0f da c1          	pminub %xmm1,%xmm0
  40cf96:	66 0f 7e c0          	movd   %xmm0,%eax
  40cf9a:	0f b6 c0             	movzbl %al,%eax
  40cf9d:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40cfa2:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40cfa7:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40cfac:	48 03 4c 24 b0       	add    -0x50(%rsp),%rcx
  40cfb1:	8a 04 08             	mov    (%rax,%rcx,1),%al
  40cfb4:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40cfb9:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40cfbe:	48 03 54 24 b0       	add    -0x50(%rsp),%rdx
  40cfc3:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  40cfc6:	0f 92 c0             	setb   %al
  40cfc9:	24 01                	and    $0x1,%al
  40cfcb:	3c 00                	cmp    $0x0,%al
  40cfcd:	74 0e                	je     40cfdd <runtime::memory_compare+0x39d>
  40cfcf:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40cfd6:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40cfdb:	eb 0c                	jmp    40cfe9 <runtime::memory_compare+0x3a9>
  40cfdd:	b8 01 00 00 00       	mov    $0x1,%eax
  40cfe2:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40cfe7:	eb 00                	jmp    40cfe9 <runtime::memory_compare+0x3a9>
  40cfe9:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40cfee:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40cff5:	c3                   	ret
  40cff6:	eb 00                	jmp    40cff8 <runtime::memory_compare+0x3b8>
  40cff8:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40cffd:	48 83 c0 08          	add    $0x8,%rax
  40d001:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40d006:	e9 90 fe ff ff       	jmp    40ce9b <runtime::memory_compare+0x25b>
  40d00b:	eb 00                	jmp    40d00d <runtime::memory_compare+0x3cd>
  40d00d:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40d012:	48 3b 44 24 68       	cmp    0x68(%rsp),%rax
  40d017:	0f 92 c0             	setb   %al
  40d01a:	24 01                	and    $0x1,%al
  40d01c:	3c 00                	cmp    $0x0,%al
  40d01e:	0f 84 8d 00 00 00    	je     40d0b1 <runtime::memory_compare+0x471>
  40d024:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40d029:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40d02e:	8a 04 08             	mov    (%rax,%rcx,1),%al
  40d031:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40d036:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40d03b:	32 04 11             	xor    (%rcx,%rdx,1),%al
  40d03e:	3c 00                	cmp    $0x0,%al
  40d040:	0f 95 c0             	setne  %al
  40d043:	24 01                	and    $0x1,%al
  40d045:	3c 00                	cmp    $0x0,%al
  40d047:	74 53                	je     40d09c <runtime::memory_compare+0x45c>
  40d049:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40d04e:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40d053:	0f b6 04 08          	movzbl (%rax,%rcx,1),%eax
  40d057:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40d05c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40d061:	0f b6 0c 11          	movzbl (%rcx,%rdx,1),%ecx
  40d065:	48 29 c8             	sub    %rcx,%rax
  40d068:	48 83 f8 00          	cmp    $0x0,%rax
  40d06c:	0f 9c c0             	setl   %al
  40d06f:	24 01                	and    $0x1,%al
  40d071:	3c 00                	cmp    $0x0,%al
  40d073:	74 0e                	je     40d083 <runtime::memory_compare+0x443>
  40d075:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40d07c:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40d081:	eb 0c                	jmp    40d08f <runtime::memory_compare+0x44f>
  40d083:	b8 01 00 00 00       	mov    $0x1,%eax
  40d088:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40d08d:	eb 00                	jmp    40d08f <runtime::memory_compare+0x44f>
  40d08f:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  40d094:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40d09b:	c3                   	ret
  40d09c:	eb 00                	jmp    40d09e <runtime::memory_compare+0x45e>
  40d09e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40d0a3:	48 83 c0 01          	add    $0x1,%rax
  40d0a7:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40d0ac:	e9 5c ff ff ff       	jmp    40d00d <runtime::memory_compare+0x3cd>
  40d0b1:	31 c0                	xor    %eax,%eax
  40d0b3:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40d0ba:	c3                   	ret
  40d0bb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040d0c0 <runtime::memory_compare_zero>:
  40d0c0:	50                   	push   %rax
  40d0c1:	48 89 7c 24 88       	mov    %rdi,-0x78(%rsp)
  40d0c6:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  40d0cb:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40d0d0:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40d0d5:	48 89 04 24          	mov    %rax,(%rsp)
  40d0d9:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d0de:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40d0e3:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  40d0ea:	00 00 
  40d0ec:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  40d0f3:	00 00 
  40d0f5:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40d0fa:	48 83 7c 24 f0 08    	cmpq   $0x8,-0x10(%rsp)
  40d100:	0f 93 c0             	setae  %al
  40d103:	24 01                	and    $0x1,%al
  40d105:	3c 00                	cmp    $0x0,%al
  40d107:	0f 84 24 01 00 00    	je     40d231 <runtime::memory_compare_zero+0x171>
  40d10d:	0f 57 c0             	xorps  %xmm0,%xmm0
  40d110:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  40d115:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40d11a:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  40d11f:	48 c1 e8 04          	shr    $0x4,%rax
  40d123:	48 c1 e0 04          	shl    $0x4,%rax
  40d127:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40d12c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d131:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  40d136:	0f 92 c0             	setb   %al
  40d139:	24 01                	and    $0x1,%al
  40d13b:	3c 00                	cmp    $0x0,%al
  40d13d:	0f 84 88 00 00 00    	je     40d1cb <runtime::memory_compare_zero+0x10b>
  40d143:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d148:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40d14d:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40d151:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  40d156:	0f 28 44 24 c0       	movaps -0x40(%rsp),%xmm0
  40d15b:	0f 28 4c 24 b0       	movaps -0x50(%rsp),%xmm1
  40d160:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  40d164:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40d168:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40d16c:	0f 29 44 24 a0       	movaps %xmm0,-0x60(%rsp)
  40d171:	0f 28 44 24 a0       	movaps -0x60(%rsp),%xmm0
  40d176:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40d17b:	66 0f eb c1          	por    %xmm1,%xmm0
  40d17f:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40d184:	66 0f eb c1          	por    %xmm1,%xmm0
  40d188:	0f 28 c8             	movaps %xmm0,%xmm1
  40d18b:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40d190:	66 0f eb c1          	por    %xmm1,%xmm0
  40d194:	0f 28 c8             	movaps %xmm0,%xmm1
  40d197:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40d19c:	66 0f eb c1          	por    %xmm1,%xmm0
  40d1a0:	66 0f 7e c0          	movd   %xmm0,%eax
  40d1a4:	3c 00                	cmp    $0x0,%al
  40d1a6:	0f 95 c0             	setne  %al
  40d1a9:	24 01                	and    $0x1,%al
  40d1ab:	3c 00                	cmp    $0x0,%al
  40d1ad:	74 07                	je     40d1b6 <runtime::memory_compare_zero+0xf6>
  40d1af:	b8 01 00 00 00       	mov    $0x1,%eax
  40d1b4:	59                   	pop    %rcx
  40d1b5:	c3                   	ret
  40d1b6:	eb 00                	jmp    40d1b8 <runtime::memory_compare_zero+0xf8>
  40d1b8:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d1bd:	48 83 c0 10          	add    $0x10,%rax
  40d1c1:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d1c6:	e9 61 ff ff ff       	jmp    40d12c <runtime::memory_compare_zero+0x6c>
  40d1cb:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40d1d0:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  40d1d5:	48 c1 e8 03          	shr    $0x3,%rax
  40d1d9:	48 c1 e0 03          	shl    $0x3,%rax
  40d1dd:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40d1e2:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d1e7:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  40d1ec:	0f 92 c0             	setb   %al
  40d1ef:	24 01                	and    $0x1,%al
  40d1f1:	3c 00                	cmp    $0x0,%al
  40d1f3:	74 3a                	je     40d22f <runtime::memory_compare_zero+0x16f>
  40d1f5:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d1fa:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  40d1ff:	48 8b 00             	mov    (%rax),%rax
  40d202:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40d207:	48 83 7c 24 98 00    	cmpq   $0x0,-0x68(%rsp)
  40d20d:	0f 95 c0             	setne  %al
  40d210:	24 01                	and    $0x1,%al
  40d212:	3c 00                	cmp    $0x0,%al
  40d214:	74 07                	je     40d21d <runtime::memory_compare_zero+0x15d>
  40d216:	b8 01 00 00 00       	mov    $0x1,%eax
  40d21b:	59                   	pop    %rcx
  40d21c:	c3                   	ret
  40d21d:	eb 00                	jmp    40d21f <runtime::memory_compare_zero+0x15f>
  40d21f:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d224:	48 83 c0 08          	add    $0x8,%rax
  40d228:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d22d:	eb b3                	jmp    40d1e2 <runtime::memory_compare_zero+0x122>
  40d22f:	eb 00                	jmp    40d231 <runtime::memory_compare_zero+0x171>
  40d231:	eb 00                	jmp    40d233 <runtime::memory_compare_zero+0x173>
  40d233:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d238:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  40d23d:	0f 92 c0             	setb   %al
  40d240:	24 01                	and    $0x1,%al
  40d242:	3c 00                	cmp    $0x0,%al
  40d244:	74 30                	je     40d276 <runtime::memory_compare_zero+0x1b6>
  40d246:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d24b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40d250:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  40d254:	0f 95 c0             	setne  %al
  40d257:	24 01                	and    $0x1,%al
  40d259:	3c 00                	cmp    $0x0,%al
  40d25b:	74 07                	je     40d264 <runtime::memory_compare_zero+0x1a4>
  40d25d:	b8 01 00 00 00       	mov    $0x1,%eax
  40d262:	59                   	pop    %rcx
  40d263:	c3                   	ret
  40d264:	eb 00                	jmp    40d266 <runtime::memory_compare_zero+0x1a6>
  40d266:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d26b:	48 83 c0 01          	add    $0x1,%rax
  40d26f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d274:	eb bd                	jmp    40d233 <runtime::memory_compare_zero+0x173>
  40d276:	31 c0                	xor    %eax,%eax
  40d278:	59                   	pop    %rcx
  40d279:	c3                   	ret
  40d27a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

000000000040d280 <__truncsfhf2>:
  40d280:	48 83 ec 18          	sub    $0x18,%rsp
  40d284:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  40d28a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  40d290:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  40d296:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40d29d:	00 
  40d29e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  40d2a5:	00 
  40d2a6:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40d2ad:	00 
  40d2ae:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  40d2b5:	00 
  40d2b6:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  40d2bd:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  40d2c3:	8b 44 24 10          	mov    0x10(%rsp),%eax
  40d2c7:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  40d2cb:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40d2cf:	c1 f9 10             	sar    $0x10,%ecx
  40d2d2:	b2 01                	mov    $0x1,%dl
  40d2d4:	31 c0                	xor    %eax,%eax
  40d2d6:	f6 c2 01             	test   $0x1,%dl
  40d2d9:	0f 45 c1             	cmovne %ecx,%eax
  40d2dc:	25 00 80 00 00       	and    $0x8000,%eax
  40d2e1:	89 44 24 08          	mov    %eax,0x8(%rsp)
  40d2e5:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40d2e9:	c1 f9 17             	sar    $0x17,%ecx
  40d2ec:	b2 01                	mov    $0x1,%dl
  40d2ee:	31 c0                	xor    %eax,%eax
  40d2f0:	f6 c2 01             	test   $0x1,%dl
  40d2f3:	0f 45 c1             	cmovne %ecx,%eax
  40d2f6:	25 ff 00 00 00       	and    $0xff,%eax
  40d2fb:	83 e8 70             	sub    $0x70,%eax
  40d2fe:	89 44 24 04          	mov    %eax,0x4(%rsp)
  40d302:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  40d306:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  40d30b:	89 04 24             	mov    %eax,(%rsp)
  40d30e:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  40d313:	0f 9e c0             	setle  %al
  40d316:	24 01                	and    $0x1,%al
  40d318:	3c 00                	cmp    $0x0,%al
  40d31a:	0f 84 82 00 00 00    	je     40d3a2 <__truncsfhf2+0x122>
  40d320:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  40d325:	0f 9c c0             	setl   %al
  40d328:	24 01                	and    $0x1,%al
  40d32a:	3c 00                	cmp    $0x0,%al
  40d32c:	74 16                	je     40d344 <__truncsfhf2+0xc4>
  40d32e:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  40d333:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  40d338:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  40d33f:	48 83 c4 18          	add    $0x18,%rsp
  40d343:	c3                   	ret
  40d344:	8b 04 24             	mov    (%rsp),%eax
  40d347:	0d 00 00 80 00       	or     $0x800000,%eax
  40d34c:	ba 01 00 00 00       	mov    $0x1,%edx
  40d351:	2b 54 24 04          	sub    0x4(%rsp),%edx
  40d355:	89 d1                	mov    %edx,%ecx
  40d357:	d3 f8                	sar    %cl,%eax
  40d359:	89 c1                	mov    %eax,%ecx
  40d35b:	31 c0                	xor    %eax,%eax
  40d35d:	83 fa 20             	cmp    $0x20,%edx
  40d360:	0f 42 c1             	cmovb  %ecx,%eax
  40d363:	89 04 24             	mov    %eax,(%rsp)
  40d366:	8b 04 24             	mov    (%rsp),%eax
  40d369:	25 00 10 00 00       	and    $0x1000,%eax
  40d36e:	83 f8 00             	cmp    $0x0,%eax
  40d371:	0f 95 c0             	setne  %al
  40d374:	24 01                	and    $0x1,%al
  40d376:	3c 00                	cmp    $0x0,%al
  40d378:	74 0b                	je     40d385 <__truncsfhf2+0x105>
  40d37a:	8b 04 24             	mov    (%rsp),%eax
  40d37d:	05 00 20 00 00       	add    $0x2000,%eax
  40d382:	89 04 24             	mov    %eax,(%rsp)
  40d385:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d389:	8b 0c 24             	mov    (%rsp),%ecx
  40d38c:	c1 e9 0d             	shr    $0xd,%ecx
  40d38f:	09 c8                	or     %ecx,%eax
  40d391:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  40d396:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40d39d:	48 83 c4 18          	add    $0x18,%rsp
  40d3a1:	c3                   	ret
  40d3a2:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  40d3a9:	00 
  40d3aa:	0f 94 c0             	sete   %al
  40d3ad:	24 01                	and    $0x1,%al
  40d3af:	3c 00                	cmp    $0x0,%al
  40d3b1:	74 59                	je     40d40c <__truncsfhf2+0x18c>
  40d3b3:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  40d3b7:	0f 94 c0             	sete   %al
  40d3ba:	24 01                	and    $0x1,%al
  40d3bc:	3c 00                	cmp    $0x0,%al
  40d3be:	74 1a                	je     40d3da <__truncsfhf2+0x15a>
  40d3c0:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d3c4:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40d3c9:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  40d3ce:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  40d3d5:	48 83 c4 18          	add    $0x18,%rsp
  40d3d9:	c3                   	ret
  40d3da:	8b 04 24             	mov    (%rsp),%eax
  40d3dd:	c1 f8 0d             	sar    $0xd,%eax
  40d3e0:	89 04 24             	mov    %eax,(%rsp)
  40d3e3:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d3e7:	8b 0c 24             	mov    (%rsp),%ecx
  40d3ea:	09 c8                	or     %ecx,%eax
  40d3ec:	85 c9                	test   %ecx,%ecx
  40d3ee:	0f 94 c1             	sete   %cl
  40d3f1:	0f b6 c9             	movzbl %cl,%ecx
  40d3f4:	09 c8                	or     %ecx,%eax
  40d3f6:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40d3fb:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  40d400:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  40d407:	48 83 c4 18          	add    $0x18,%rsp
  40d40b:	c3                   	ret
  40d40c:	8b 04 24             	mov    (%rsp),%eax
  40d40f:	25 00 10 00 00       	and    $0x1000,%eax
  40d414:	83 f8 00             	cmp    $0x0,%eax
  40d417:	0f 95 c0             	setne  %al
  40d41a:	24 01                	and    $0x1,%al
  40d41c:	3c 00                	cmp    $0x0,%al
  40d41e:	74 33                	je     40d453 <__truncsfhf2+0x1d3>
  40d420:	8b 04 24             	mov    (%rsp),%eax
  40d423:	05 00 20 00 00       	add    $0x2000,%eax
  40d428:	89 04 24             	mov    %eax,(%rsp)
  40d42b:	8b 04 24             	mov    (%rsp),%eax
  40d42e:	25 00 00 80 00       	and    $0x800000,%eax
  40d433:	83 f8 00             	cmp    $0x0,%eax
  40d436:	0f 95 c0             	setne  %al
  40d439:	24 01                	and    $0x1,%al
  40d43b:	3c 00                	cmp    $0x0,%al
  40d43d:	74 12                	je     40d451 <__truncsfhf2+0x1d1>
  40d43f:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  40d446:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40d44a:	83 c0 01             	add    $0x1,%eax
  40d44d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  40d451:	eb 00                	jmp    40d453 <__truncsfhf2+0x1d3>
  40d453:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  40d458:	0f 9f c0             	setg   %al
  40d45b:	24 01                	and    $0x1,%al
  40d45d:	3c 00                	cmp    $0x0,%al
  40d45f:	74 75                	je     40d4d6 <__truncsfhf2+0x256>
  40d461:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  40d468:	00 00 00 
  40d46b:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40d470:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  40d477:	00 00 
  40d479:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  40d47f:	0f 9c c0             	setl   %al
  40d482:	24 01                	and    $0x1,%al
  40d484:	3c 00                	cmp    $0x0,%al
  40d486:	74 34                	je     40d4bc <__truncsfhf2+0x23c>
  40d488:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40d48d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40d492:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40d497:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  40d49d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40d4a2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40d4a7:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40d4ac:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40d4b1:	48 83 c0 01          	add    $0x1,%rax
  40d4b5:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40d4ba:	eb bd                	jmp    40d479 <__truncsfhf2+0x1f9>
  40d4bc:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d4c0:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40d4c5:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  40d4ca:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  40d4d1:	48 83 c4 18          	add    $0x18,%rsp
  40d4d5:	c3                   	ret
  40d4d6:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d4da:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  40d4de:	c1 e1 0a             	shl    $0xa,%ecx
  40d4e1:	09 c8                	or     %ecx,%eax
  40d4e3:	8b 0c 24             	mov    (%rsp),%ecx
  40d4e6:	c1 e9 0d             	shr    $0xd,%ecx
  40d4e9:	09 c8                	or     %ecx,%eax
  40d4eb:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  40d4f0:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  40d4f7:	48 83 c4 18          	add    $0x18,%rsp
  40d4fb:	c3                   	ret
  40d4fc:	0f 1f 40 00          	nopl   0x0(%rax)

000000000040d500 <__truncdfhf2>:
  40d500:	48 83 ec 18          	sub    $0x18,%rsp
  40d504:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  40d50a:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  40d510:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  40d516:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  40d51a:	e8 61 fd ff ff       	call   40d280 <__truncsfhf2>
  40d51f:	48 83 c4 18          	add    $0x18,%rsp
  40d523:	c3                   	ret
  40d524:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40d52b:	00 00 00 00 00 

000000000040d530 <__gnu_h2f_ieee>:
  40d530:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  40d536:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  40d53c:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  40d543:	00 
  40d544:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  40d54b:	00 
  40d54c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  40d551:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  40d556:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  40d55d:	00 
  40d55e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  40d565:	00 
  40d566:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  40d56d:	00 
  40d56e:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  40d575:	77 
  40d576:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  40d57d:	47 
  40d57e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  40d583:	66 25 ff 7f          	and    $0x7fff,%ax
  40d587:	0f b7 c8             	movzwl %ax,%ecx
  40d58a:	c1 e1 0d             	shl    $0xd,%ecx
  40d58d:	b2 01                	mov    $0x1,%dl
  40d58f:	31 c0                	xor    %eax,%eax
  40d591:	f6 c2 01             	test   $0x1,%dl
  40d594:	0f 45 c1             	cmovne %ecx,%eax
  40d597:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40d59b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  40d5a1:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  40d5a7:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  40d5ad:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  40d5b3:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  40d5b8:	0f 93 c0             	setae  %al
  40d5bb:	24 01                	and    $0x1,%al
  40d5bd:	3c 00                	cmp    $0x0,%al
  40d5bf:	74 0d                	je     40d5ce <__gnu_h2f_ieee+0x9e>
  40d5c1:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  40d5c5:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  40d5ca:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40d5ce:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  40d5d3:	66 25 00 80          	and    $0x8000,%ax
  40d5d7:	0f b7 c8             	movzwl %ax,%ecx
  40d5da:	c1 e1 10             	shl    $0x10,%ecx
  40d5dd:	b2 01                	mov    $0x1,%dl
  40d5df:	31 c0                	xor    %eax,%eax
  40d5e1:	f6 c2 01             	test   $0x1,%dl
  40d5e4:	0f 45 c1             	cmovne %ecx,%eax
  40d5e7:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  40d5eb:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40d5ef:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  40d5f5:	c3                   	ret
  40d5f6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40d5fd:	00 00 00 

000000000040d600 <__gnu_f2h_ieee>:
  40d600:	50                   	push   %rax
  40d601:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  40d606:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  40d60b:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  40d611:	e8 6a fc ff ff       	call   40d280 <__truncsfhf2>
  40d616:	58                   	pop    %rax
  40d617:	c3                   	ret
  40d618:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40d61f:	00 

000000000040d620 <__extendhfsf2>:
  40d620:	50                   	push   %rax
  40d621:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  40d627:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  40d62d:	0f 28 c8             	movaps %xmm0,%xmm1
  40d630:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  40d637:	00 
  40d638:	e8 f3 fe ff ff       	call   40d530 <__gnu_h2f_ieee>
  40d63d:	58                   	pop    %rax
  40d63e:	c3                   	ret
  40d63f:	90                   	nop

000000000040d640 <__floattidf>:
  40d640:	53                   	push   %rbx
  40d641:	48 83 ec 10          	sub    $0x10,%rsp
  40d645:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40d64a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40d64f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40d654:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40d659:	48 89 04 24          	mov    %rax,(%rsp)
  40d65d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40d662:	48 09 c8             	or     %rcx,%rax
  40d665:	0f 94 c0             	sete   %al
  40d668:	24 01                	and    $0x1,%al
  40d66a:	3c 00                	cmp    $0x0,%al
  40d66c:	74 09                	je     40d677 <__floattidf+0x37>
  40d66e:	0f 57 c0             	xorps  %xmm0,%xmm0
  40d671:	48 83 c4 10          	add    $0x10,%rsp
  40d675:	5b                   	pop    %rbx
  40d676:	c3                   	ret
  40d677:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40d67c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40d681:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40d686:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40d68b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40d690:	48 c1 f8 3f          	sar    $0x3f,%rax
  40d694:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d699:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40d69e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40d6a3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40d6a8:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40d6ad:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40d6b2:	48 31 d0             	xor    %rdx,%rax
  40d6b5:	48 31 f1             	xor    %rsi,%rcx
  40d6b8:	48 29 f1             	sub    %rsi,%rcx
  40d6bb:	48 19 d0             	sbb    %rdx,%rax
  40d6be:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40d6c3:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40d6c8:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  40d6cd:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40d6d2:	48 0f bd c2          	bsr    %rdx,%rax
  40d6d6:	48 83 f0 3f          	xor    $0x3f,%rax
  40d6da:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40d6df:	48 0f bd ce          	bsr    %rsi,%rcx
  40d6e3:	48 83 f1 3f          	xor    $0x3f,%rcx
  40d6e7:	48 83 c1 40          	add    $0x40,%rcx
  40d6eb:	48 85 d2             	test   %rdx,%rdx
  40d6ee:	48 0f 45 c8          	cmovne %rax,%rcx
  40d6f2:	31 c0                	xor    %eax,%eax
  40d6f4:	ba 80 00 00 00       	mov    $0x80,%edx
  40d6f9:	48 29 ca             	sub    %rcx,%rdx
  40d6fc:	48 89 c1             	mov    %rax,%rcx
  40d6ff:	48 19 c9             	sbb    %rcx,%rcx
  40d702:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40d707:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  40d70c:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  40d710:	ff c9                	dec    %ecx
  40d712:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  40d716:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  40d71b:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40d720:	ba 35 00 00 00       	mov    $0x35,%edx
  40d725:	48 29 f2             	sub    %rsi,%rdx
  40d728:	48 19 c8             	sbb    %rcx,%rax
  40d72b:	0f 9c c0             	setl   %al
  40d72e:	24 01                	and    $0x1,%al
  40d730:	3c 00                	cmp    $0x0,%al
  40d732:	0f 84 c0 01 00 00    	je     40d8f8 <__floattidf+0x2b8>
  40d738:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40d73d:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40d742:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d747:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40d74c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40d751:	0f 28 0d b8 0f 00 00 	movaps 0xfb8(%rip),%xmm1        # 40e710 <_IO_stdin_used+0x710>
  40d758:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40d75c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  40d761:	74 17                	je     40d77a <__floattidf+0x13a>
  40d763:	eb 00                	jmp    40d765 <__floattidf+0x125>
  40d765:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  40d76a:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40d76f:	48 83 f0 37          	xor    $0x37,%rax
  40d773:	48 09 c8             	or     %rcx,%rax
  40d776:	74 26                	je     40d79e <__floattidf+0x15e>
  40d778:	eb 29                	jmp    40d7a3 <__floattidf+0x163>
  40d77a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40d77f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40d784:	48 89 d0             	mov    %rdx,%rax
  40d787:	48 01 c0             	add    %rax,%rax
  40d78a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40d78f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d794:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d799:	e9 d6 00 00 00       	jmp    40d874 <__floattidf+0x234>
  40d79e:	e9 d1 00 00 00       	jmp    40d874 <__floattidf+0x234>
  40d7a3:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40d7a8:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  40d7ad:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  40d7b2:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d7b7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40d7bc:	49 89 fb             	mov    %rdi,%r11
  40d7bf:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  40d7c3:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  40d7c7:	44 88 db             	mov    %r11b,%bl
  40d7ca:	88 d9                	mov    %bl,%cl
  40d7cc:	49 89 f2             	mov    %rsi,%r10
  40d7cf:	49 d3 ea             	shr    %cl,%r10
  40d7d2:	88 d9                	mov    %bl,%cl
  40d7d4:	49 89 d1             	mov    %rdx,%r9
  40d7d7:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40d7db:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40d7e0:	45 31 c0             	xor    %r8d,%r8d
  40d7e3:	f6 c3 40             	test   $0x40,%bl
  40d7e6:	4d 0f 45 ca          	cmovne %r10,%r9
  40d7ea:	4d 0f 45 d0          	cmovne %r8,%r10
  40d7ee:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  40d7f5:	48 83 d8 00          	sbb    $0x0,%rax
  40d7f9:	4c 89 c0             	mov    %r8,%rax
  40d7fc:	49 0f 42 c2          	cmovb  %r10,%rax
  40d800:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40d805:	4c 89 c0             	mov    %r8,%rax
  40d808:	49 0f 42 c1          	cmovb  %r9,%rax
  40d80c:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  40d812:	49 29 fb             	sub    %rdi,%r11
  40d815:	4c 89 c7             	mov    %r8,%rdi
  40d818:	48 19 cf             	sbb    %rcx,%rdi
  40d81b:	45 88 d9             	mov    %r11b,%r9b
  40d81e:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  40d825:	44 88 c9             	mov    %r9b,%cl
  40d828:	4c 89 d3             	mov    %r10,%rbx
  40d82b:	48 d3 eb             	shr    %cl,%rbx
  40d82e:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40d833:	41 f6 c1 40          	test   $0x40,%r9b
  40d837:	49 89 d9             	mov    %rbx,%r9
  40d83a:	4d 0f 45 c8          	cmovne %r8,%r9
  40d83e:	4c 0f 45 d3          	cmovne %rbx,%r10
  40d842:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  40d849:	48 83 df 00          	sbb    $0x0,%rdi
  40d84d:	4c 89 c7             	mov    %r8,%rdi
  40d850:	49 0f 42 fa          	cmovb  %r10,%rdi
  40d854:	4d 0f 42 c1          	cmovb  %r9,%r8
  40d858:	4c 21 c6             	and    %r8,%rsi
  40d85b:	48 21 fa             	and    %rdi,%rdx
  40d85e:	48 09 f2             	or     %rsi,%rdx
  40d861:	0f 95 c2             	setne  %dl
  40d864:	0f b6 d2             	movzbl %dl,%edx
  40d867:	48 09 d0             	or     %rdx,%rax
  40d86a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d86f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d874:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40d879:	89 c1                	mov    %eax,%ecx
  40d87b:	83 e1 04             	and    $0x4,%ecx
  40d87e:	c1 e9 02             	shr    $0x2,%ecx
  40d881:	48 09 c8             	or     %rcx,%rax
  40d884:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d889:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40d88e:	48 83 c0 01          	add    $0x1,%rax
  40d892:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  40d898:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d89d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40d8a2:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40d8a7:	48 89 c8             	mov    %rcx,%rax
  40d8aa:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40d8af:	48 c1 f9 02          	sar    $0x2,%rcx
  40d8b3:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d8b8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d8bd:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  40d8c1:	24 20                	and    $0x20,%al
  40d8c3:	c0 e8 05             	shr    $0x5,%al
  40d8c6:	24 01                	and    $0x1,%al
  40d8c8:	3c 00                	cmp    $0x0,%al
  40d8ca:	74 2a                	je     40d8f6 <__floattidf+0x2b6>
  40d8cc:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40d8d1:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40d8d6:	48 89 c8             	mov    %rcx,%rax
  40d8d9:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40d8de:	48 d1 f9             	sar    $1,%rcx
  40d8e1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d8e6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d8eb:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40d8ef:	83 c0 01             	add    $0x1,%eax
  40d8f2:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  40d8f6:	eb 5c                	jmp    40d954 <__floattidf+0x314>
  40d8f8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  40d8fc:	b9 35 00 00 00       	mov    $0x35,%ecx
  40d901:	29 c1                	sub    %eax,%ecx
  40d903:	83 e1 7f             	and    $0x7f,%ecx
  40d906:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  40d90a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40d90f:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  40d914:	40 88 cf             	mov    %cl,%dil
  40d917:	40 88 f9             	mov    %dil,%cl
  40d91a:	48 89 c2             	mov    %rax,%rdx
  40d91d:	48 d3 e2             	shl    %cl,%rdx
  40d920:	40 88 f9             	mov    %dil,%cl
  40d923:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  40d927:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  40d92b:	31 c0                	xor    %eax,%eax
  40d92d:	40 f6 c7 40          	test   $0x40,%dil
  40d931:	48 0f 45 f2          	cmovne %rdx,%rsi
  40d935:	48 0f 45 d0          	cmovne %rax,%rdx
  40d939:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  40d93f:	48 89 c1             	mov    %rax,%rcx
  40d942:	48 0f 42 ce          	cmovb  %rsi,%rcx
  40d946:	48 0f 42 c2          	cmovb  %rdx,%rax
  40d94a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d94f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d954:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  40d95b:	00 00 
  40d95d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  40d961:	25 00 00 00 80       	and    $0x80000000,%eax
  40d966:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  40d96a:	c1 e1 14             	shl    $0x14,%ecx
  40d96d:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  40d973:	09 c8                	or     %ecx,%eax
  40d975:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  40d979:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  40d97f:	09 c8                	or     %ecx,%eax
  40d981:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  40d985:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  40d989:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40d98d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  40d993:	48 83 c4 10          	add    $0x10,%rsp
  40d997:	5b                   	pop    %rbx
  40d998:	c3                   	ret
  40d999:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

000000000040d9a0 <__floattidf_unsigned>:
  40d9a0:	53                   	push   %rbx
  40d9a1:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40d9a6:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40d9ab:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40d9b0:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40d9b5:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d9ba:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d9bf:	48 09 c8             	or     %rcx,%rax
  40d9c2:	0f 94 c0             	sete   %al
  40d9c5:	24 01                	and    $0x1,%al
  40d9c7:	3c 00                	cmp    $0x0,%al
  40d9c9:	74 05                	je     40d9d0 <__floattidf_unsigned+0x30>
  40d9cb:	0f 57 c0             	xorps  %xmm0,%xmm0
  40d9ce:	5b                   	pop    %rbx
  40d9cf:	c3                   	ret
  40d9d0:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40d9d5:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40d9da:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40d9df:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d9e4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40d9e9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40d9ee:	48 0f bd c2          	bsr    %rdx,%rax
  40d9f2:	48 83 f0 3f          	xor    $0x3f,%rax
  40d9f6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40d9fb:	48 0f bd ce          	bsr    %rsi,%rcx
  40d9ff:	48 83 f1 3f          	xor    $0x3f,%rcx
  40da03:	48 83 c1 40          	add    $0x40,%rcx
  40da07:	48 85 d2             	test   %rdx,%rdx
  40da0a:	48 0f 45 c8          	cmovne %rax,%rcx
  40da0e:	31 c0                	xor    %eax,%eax
  40da10:	ba 80 00 00 00       	mov    $0x80,%edx
  40da15:	48 29 ca             	sub    %rcx,%rdx
  40da18:	48 89 c1             	mov    %rax,%rcx
  40da1b:	48 19 c9             	sbb    %rcx,%rcx
  40da1e:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40da23:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  40da28:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  40da2c:	ff c9                	dec    %ecx
  40da2e:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  40da32:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  40da37:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40da3c:	ba 35 00 00 00       	mov    $0x35,%edx
  40da41:	48 29 f2             	sub    %rsi,%rdx
  40da44:	48 19 c8             	sbb    %rcx,%rax
  40da47:	0f 92 c0             	setb   %al
  40da4a:	24 01                	and    $0x1,%al
  40da4c:	3c 00                	cmp    $0x0,%al
  40da4e:	0f 84 c0 01 00 00    	je     40dc14 <__floattidf_unsigned+0x274>
  40da54:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40da59:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40da5e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40da63:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40da68:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40da6d:	0f 28 0d 9c 0c 00 00 	movaps 0xc9c(%rip),%xmm1        # 40e710 <_IO_stdin_used+0x710>
  40da74:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40da78:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  40da7d:	74 17                	je     40da96 <__floattidf_unsigned+0xf6>
  40da7f:	eb 00                	jmp    40da81 <__floattidf_unsigned+0xe1>
  40da81:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  40da86:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40da8b:	48 83 f0 37          	xor    $0x37,%rax
  40da8f:	48 09 c8             	or     %rcx,%rax
  40da92:	74 26                	je     40daba <__floattidf_unsigned+0x11a>
  40da94:	eb 29                	jmp    40dabf <__floattidf_unsigned+0x11f>
  40da96:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40da9b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40daa0:	48 89 d0             	mov    %rdx,%rax
  40daa3:	48 01 c0             	add    %rax,%rax
  40daa6:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40daab:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40dab0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dab5:	e9 d6 00 00 00       	jmp    40db90 <__floattidf_unsigned+0x1f0>
  40daba:	e9 d1 00 00 00       	jmp    40db90 <__floattidf_unsigned+0x1f0>
  40dabf:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40dac4:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  40dac9:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  40dace:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40dad3:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40dad8:	49 89 fb             	mov    %rdi,%r11
  40dadb:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  40dadf:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  40dae3:	44 88 db             	mov    %r11b,%bl
  40dae6:	88 d9                	mov    %bl,%cl
  40dae8:	49 89 f2             	mov    %rsi,%r10
  40daeb:	49 d3 ea             	shr    %cl,%r10
  40daee:	88 d9                	mov    %bl,%cl
  40daf0:	49 89 d1             	mov    %rdx,%r9
  40daf3:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40daf7:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40dafc:	45 31 c0             	xor    %r8d,%r8d
  40daff:	f6 c3 40             	test   $0x40,%bl
  40db02:	4d 0f 45 ca          	cmovne %r10,%r9
  40db06:	4d 0f 45 d0          	cmovne %r8,%r10
  40db0a:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  40db11:	48 83 d8 00          	sbb    $0x0,%rax
  40db15:	4c 89 c0             	mov    %r8,%rax
  40db18:	49 0f 42 c2          	cmovb  %r10,%rax
  40db1c:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40db21:	4c 89 c0             	mov    %r8,%rax
  40db24:	49 0f 42 c1          	cmovb  %r9,%rax
  40db28:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  40db2e:	49 29 fb             	sub    %rdi,%r11
  40db31:	4c 89 c7             	mov    %r8,%rdi
  40db34:	48 19 cf             	sbb    %rcx,%rdi
  40db37:	45 88 d9             	mov    %r11b,%r9b
  40db3a:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  40db41:	44 88 c9             	mov    %r9b,%cl
  40db44:	4c 89 d3             	mov    %r10,%rbx
  40db47:	48 d3 eb             	shr    %cl,%rbx
  40db4a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40db4f:	41 f6 c1 40          	test   $0x40,%r9b
  40db53:	49 89 d9             	mov    %rbx,%r9
  40db56:	4d 0f 45 c8          	cmovne %r8,%r9
  40db5a:	4c 0f 45 d3          	cmovne %rbx,%r10
  40db5e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  40db65:	48 83 df 00          	sbb    $0x0,%rdi
  40db69:	4c 89 c7             	mov    %r8,%rdi
  40db6c:	49 0f 42 fa          	cmovb  %r10,%rdi
  40db70:	4d 0f 42 c1          	cmovb  %r9,%r8
  40db74:	4c 21 c6             	and    %r8,%rsi
  40db77:	48 21 fa             	and    %rdi,%rdx
  40db7a:	48 09 f2             	or     %rsi,%rdx
  40db7d:	0f 95 c2             	setne  %dl
  40db80:	0f b6 d2             	movzbl %dl,%edx
  40db83:	48 09 d0             	or     %rdx,%rax
  40db86:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40db8b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40db90:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40db95:	89 c1                	mov    %eax,%ecx
  40db97:	83 e1 04             	and    $0x4,%ecx
  40db9a:	c1 e9 02             	shr    $0x2,%ecx
  40db9d:	48 09 c8             	or     %rcx,%rax
  40dba0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dba5:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40dbaa:	48 83 c0 01          	add    $0x1,%rax
  40dbae:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  40dbb4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dbb9:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40dbbe:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40dbc3:	48 89 c8             	mov    %rcx,%rax
  40dbc6:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40dbcb:	48 c1 e9 02          	shr    $0x2,%rcx
  40dbcf:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40dbd4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dbd9:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  40dbdd:	24 20                	and    $0x20,%al
  40dbdf:	c0 e8 05             	shr    $0x5,%al
  40dbe2:	24 01                	and    $0x1,%al
  40dbe4:	3c 00                	cmp    $0x0,%al
  40dbe6:	74 2a                	je     40dc12 <__floattidf_unsigned+0x272>
  40dbe8:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40dbed:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40dbf2:	48 89 c8             	mov    %rcx,%rax
  40dbf5:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40dbfa:	48 d1 e9             	shr    $1,%rcx
  40dbfd:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40dc02:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dc07:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40dc0b:	83 c0 01             	add    $0x1,%eax
  40dc0e:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  40dc12:	eb 6a                	jmp    40dc7e <__floattidf_unsigned+0x2de>
  40dc14:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  40dc19:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40dc1e:	31 c0                	xor    %eax,%eax
  40dc20:	bf 35 00 00 00       	mov    $0x35,%edi
  40dc25:	48 29 d7             	sub    %rdx,%rdi
  40dc28:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40dc2d:	48 19 c8             	sbb    %rcx,%rax
  40dc30:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  40dc35:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40dc3a:	41 88 f8             	mov    %dil,%r8b
  40dc3d:	44 88 c1             	mov    %r8b,%cl
  40dc40:	4c 89 ce             	mov    %r9,%rsi
  40dc43:	48 d3 e6             	shl    %cl,%rsi
  40dc46:	44 88 c1             	mov    %r8b,%cl
  40dc49:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  40dc4d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  40dc52:	41 f6 c0 40          	test   $0x40,%r8b
  40dc56:	48 0f 45 d6          	cmovne %rsi,%rdx
  40dc5a:	48 0f 45 f1          	cmovne %rcx,%rsi
  40dc5e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  40dc65:	48 83 d8 00          	sbb    $0x0,%rax
  40dc69:	48 89 c8             	mov    %rcx,%rax
  40dc6c:	48 0f 42 c6          	cmovb  %rsi,%rax
  40dc70:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40dc74:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40dc79:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dc7e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  40dc85:	00 00 
  40dc87:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  40dc8b:	c1 e2 14             	shl    $0x14,%edx
  40dc8e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  40dc92:	25 ff ff 0f 00       	and    $0xfffff,%eax
  40dc97:	89 c1                	mov    %eax,%ecx
  40dc99:	89 d0                	mov    %edx,%eax
  40dc9b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  40dca2:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  40dca6:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  40dcaa:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40dcae:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  40dcb4:	5b                   	pop    %rbx
  40dcb5:	c3                   	ret
  40dcb6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40dcbd:	00 00 00 

000000000040dcc0 <__umodti3>:
  40dcc0:	48 83 ec 58          	sub    $0x58,%rsp
  40dcc4:	48 89 0c 24          	mov    %rcx,(%rsp)
  40dcc8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40dccd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40dcd2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40dcd7:	48 8b 0c 24          	mov    (%rsp),%rcx
  40dcdb:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40dce0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40dce5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40dcea:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40dcef:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40dcf4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40dcf9:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40dcfe:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  40dd03:	e8 a8 bb ff ff       	call   4098b0 <runtime::udivmod128>
  40dd08:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40dd0d:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40dd12:	48 83 c4 58          	add    $0x58,%rsp
  40dd16:	c3                   	ret
  40dd17:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40dd1e:	00 00 

000000000040dd20 <__udivmodti4>:
  40dd20:	48 83 ec 58          	sub    $0x58,%rsp
  40dd24:	4c 89 04 24          	mov    %r8,(%rsp)
  40dd28:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40dd2d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40dd32:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40dd37:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40dd3c:	4c 8b 04 24          	mov    (%rsp),%r8
  40dd40:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40dd45:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40dd4a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40dd4f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40dd54:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40dd59:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40dd5e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40dd63:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40dd68:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40dd6d:	e8 3e bb ff ff       	call   4098b0 <runtime::udivmod128>
  40dd72:	48 83 c4 58          	add    $0x58,%rsp
  40dd76:	c3                   	ret
  40dd77:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40dd7e:	00 00 

000000000040dd80 <__udivti3>:
  40dd80:	48 83 ec 48          	sub    $0x48,%rsp
  40dd84:	48 89 0c 24          	mov    %rcx,(%rsp)
  40dd88:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40dd8d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40dd92:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40dd97:	48 8b 0c 24          	mov    (%rsp),%rcx
  40dd9b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40dda0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40dda5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40ddaa:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40ddaf:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40ddb4:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40ddb9:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40ddbe:	31 c0                	xor    %eax,%eax
  40ddc0:	41 89 c0             	mov    %eax,%r8d
  40ddc3:	e8 58 ff ff ff       	call   40dd20 <__udivmodti4>
  40ddc8:	48 83 c4 48          	add    $0x48,%rsp
  40ddcc:	c3                   	ret
  40ddcd:	0f 1f 00             	nopl   (%rax)

000000000040ddd0 <__modti3>:
  40ddd0:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
  40ddd7:	48 89 0c 24          	mov    %rcx,(%rsp)
  40dddb:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40dde0:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40dde5:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40ddea:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40ddef:	48 8b 04 24          	mov    (%rsp),%rax
  40ddf3:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40ddf8:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40ddfd:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40de04:	00 
  40de05:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  40de0c:	00 
  40de0d:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40de12:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40de17:	48 89 d7             	mov    %rdx,%rdi
  40de1a:	48 c1 ff 3f          	sar    $0x3f,%rdi
  40de1e:	48 89 7c 24 68       	mov    %rdi,0x68(%rsp)
  40de23:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40de28:	48 89 c7             	mov    %rax,%rdi
  40de2b:	48 c1 ff 3f          	sar    $0x3f,%rdi
  40de2f:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  40de34:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  40de39:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  40de3e:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40de43:	48 31 fa             	xor    %rdi,%rdx
  40de46:	4c 31 c6             	xor    %r8,%rsi
  40de49:	4c 29 c6             	sub    %r8,%rsi
  40de4c:	48 19 fa             	sbb    %rdi,%rdx
  40de4f:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40de54:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40de59:	48 8b 74 24 50       	mov    0x50(%rsp),%rsi
  40de5e:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  40de63:	48 31 d0             	xor    %rdx,%rax
  40de66:	48 31 f1             	xor    %rsi,%rcx
  40de69:	48 29 f1             	sub    %rsi,%rcx
  40de6c:	48 19 d0             	sbb    %rdx,%rax
  40de6f:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40de74:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40de79:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40de7e:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40de83:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40de88:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40de8d:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  40de92:	e8 19 ba ff ff       	call   4098b0 <runtime::udivmod128>
  40de97:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40de9c:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40dea1:	48 8b 74 24 60       	mov    0x60(%rsp),%rsi
  40dea6:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40deab:	48 31 ca             	xor    %rcx,%rdx
  40deae:	48 31 f0             	xor    %rsi,%rax
  40deb1:	48 29 f0             	sub    %rsi,%rax
  40deb4:	48 19 ca             	sbb    %rcx,%rdx
  40deb7:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40debe:	c3                   	ret

Disassembly of section .fini:

000000000040dec0 <_fini>:
  40dec0:	f3 0f 1e fa          	endbr64
  40dec4:	48 83 ec 08          	sub    $0x8,%rsp
  40dec8:	48 83 c4 08          	add    $0x8,%rsp
  40decc:	c3                   	ret
