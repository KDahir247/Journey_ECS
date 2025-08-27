
/home/khalid/Documents/GitHub/Journey_ECS/main.bin:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 c1 8f 00 00 	mov    0x8fc1(%rip),%rax        # 409fd0 <__gmon_start__@Base>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 8f 00 00    	push   0x8fca(%rip)        # 409ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 8f 00 00    	jmp    *0x8fcc(%rip)        # 409ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 8f 00 00    	jmp    *0x8fca(%rip)        # 40a000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <memset@plt>:
  401040:	ff 25 c2 8f 00 00    	jmp    *0x8fc2(%rip)        # 40a008 <memset@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <calloc@plt>:
  401050:	ff 25 ba 8f 00 00    	jmp    *0x8fba(%rip)        # 40a010 <calloc@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <memcpy@plt>:
  401060:	ff 25 b2 8f 00 00    	jmp    *0x8fb2(%rip)        # 40a018 <memcpy@GLIBC_2.14>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <malloc@plt>:
  401070:	ff 25 aa 8f 00 00    	jmp    *0x8faa(%rip)        # 40a020 <malloc@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <realloc@plt>:
  401080:	ff 25 a2 8f 00 00    	jmp    *0x8fa2(%rip)        # 40a028 <realloc@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <memmove@plt>:
  401090:	ff 25 9a 8f 00 00    	jmp    *0x8f9a(%rip)        # 40a030 <memmove@GLIBC_2.2.5>
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
  4010b8:	48 c7 c7 90 29 40 00 	mov    $0x402990,%rdi
  4010bf:	ff 15 fb 8e 00 00    	call   *0x8efb(%rip)        # 409fc0 <__libc_start_main@GLIBC_2.34>
  4010c5:	f4                   	hlt
  4010c6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4010cd:	00 00 00 

00000000004010d0 <_dl_relocate_static_pie>:
  4010d0:	f3 0f 1e fa          	endbr64
  4010d4:	c3                   	ret
  4010d5:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4010dc:	00 00 00 
  4010df:	90                   	nop
  4010e0:	b8 58 a0 40 00       	mov    $0x40a058,%eax
  4010e5:	48 3d 58 a0 40 00    	cmp    $0x40a058,%rax
  4010eb:	74 13                	je     401100 <_dl_relocate_static_pie+0x30>
  4010ed:	48 8b 05 d4 8e 00 00 	mov    0x8ed4(%rip),%rax        # 409fc8 <_ITM_deregisterTMCloneTable@Base>
  4010f4:	48 85 c0             	test   %rax,%rax
  4010f7:	74 07                	je     401100 <_dl_relocate_static_pie+0x30>
  4010f9:	bf 58 a0 40 00       	mov    $0x40a058,%edi
  4010fe:	ff e0                	jmp    *%rax
  401100:	c3                   	ret
  401101:	0f 1f 40 00          	nopl   0x0(%rax)
  401105:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40110c:	00 00 00 00 
  401110:	be 58 a0 40 00       	mov    $0x40a058,%esi
  401115:	48 81 ee 58 a0 40 00 	sub    $0x40a058,%rsi
  40111c:	48 89 f0             	mov    %rsi,%rax
  40111f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401123:	48 c1 f8 03          	sar    $0x3,%rax
  401127:	48 01 c6             	add    %rax,%rsi
  40112a:	48 d1 fe             	sar    $1,%rsi
  40112d:	74 19                	je     401148 <_dl_relocate_static_pie+0x78>
  40112f:	48 8b 05 a2 8e 00 00 	mov    0x8ea2(%rip),%rax        # 409fd8 <_ITM_registerTMCloneTable@Base>
  401136:	48 85 c0             	test   %rax,%rax
  401139:	74 0d                	je     401148 <_dl_relocate_static_pie+0x78>
  40113b:	bf 58 a0 40 00       	mov    $0x40a058,%edi
  401140:	ff e0                	jmp    *%rax
  401142:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401148:	c3                   	ret
  401149:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
  401150:	f3 0f 1e fa          	endbr64
  401154:	80 3d fd 8e 00 00 00 	cmpb   $0x0,0x8efd(%rip)        # 40a058 <__TMC_END__>
  40115b:	75 13                	jne    401170 <_dl_relocate_static_pie+0xa0>
  40115d:	55                   	push   %rbp
  40115e:	48 89 e5             	mov    %rsp,%rbp
  401161:	e8 7a ff ff ff       	call   4010e0 <_dl_relocate_static_pie+0x10>
  401166:	c6 05 eb 8e 00 00 01 	movb   $0x1,0x8eeb(%rip)        # 40a058 <__TMC_END__>
  40116d:	5d                   	pop    %rbp
  40116e:	c3                   	ret
  40116f:	90                   	nop
  401170:	c3                   	ret
  401171:	0f 1f 40 00          	nopl   0x0(%rax)
  401175:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40117c:	00 00 00 00 
  401180:	f3 0f 1e fa          	endbr64
  401184:	eb 8a                	jmp    401110 <_dl_relocate_static_pie+0x40>
  401186:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40118d:	00 00 00 

0000000000401190 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  401190:	50                   	push   %rax
  401191:	48 89 3c 24          	mov    %rdi,(%rsp)
  401195:	eb 00                	jmp    401197 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x7>
  401197:	48 8b 34 24          	mov    (%rsp),%rsi
  40119b:	48 c7 c0 c0 ff ff ff 	mov    $0xffffffffffffffc0,%rax
  4011a2:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  4011a9:	00 00 
  4011ab:	48 01 c7             	add    %rax,%rdi
  4011ae:	e8 8d 11 00 00       	call   402340 <runtime::default_temp_allocator_destroy>
  4011b3:	58                   	pop    %rax
  4011b4:	c3                   	ret
  4011b5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4011bc:	00 00 00 00 

00000000004011c0 <runtime::bounds_trap>:
  4011c0:	eb 00                	jmp    4011c2 <runtime::bounds_trap+0x2>
  4011c2:	0f 0b                	ud2
  4011c4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4011cb:	00 00 00 00 00 

00000000004011d0 <runtime::heap_allocator>:
  4011d0:	48 c7 c0 70 1c 40 00 	mov    $0x401c70,%rax
  4011d7:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4011dc:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  4011e3:	00 00 
  4011e5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4011ea:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4011ef:	c3                   	ret

00000000004011f0 <runtime::udivmod128>:
  4011f0:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  4011f7:	4c 89 44 24 a8       	mov    %r8,-0x58(%rsp)
  4011fc:	48 89 4c 24 b0       	mov    %rcx,-0x50(%rsp)
  401201:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401206:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40120b:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  401210:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401215:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40121a:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40121f:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  401224:	48 8b 7c 24 a8       	mov    -0x58(%rsp),%rdi
  401229:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  401230:	00 
  401231:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  401238:	00 
  401239:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  401240:	00 
  401241:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  401248:	00 
  401249:	48 89 bc 24 88 00 00 	mov    %rdi,0x88(%rsp)
  401250:	00 
  401251:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  401256:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  40125b:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  401260:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  401265:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  40126a:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  40126f:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  401274:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401279:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40127e:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  401283:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  401288:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40128d:	0f 57 c0             	xorps  %xmm0,%xmm0
  401290:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401295:	0f 57 c0             	xorps  %xmm0,%xmm0
  401298:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  40129d:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  4012a4:	00 
  4012a5:	48 83 7c 24 68 00    	cmpq   $0x0,0x68(%rsp)
  4012ab:	0f 94 c0             	sete   %al
  4012ae:	24 01                	and    $0x1,%al
  4012b0:	3c 00                	cmp    $0x0,%al
  4012b2:	0f 84 a1 00 00 00    	je     401359 <runtime::udivmod128+0x169>
  4012b8:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  4012be:	0f 94 c0             	sete   %al
  4012c1:	24 01                	and    $0x1,%al
  4012c3:	3c 00                	cmp    $0x0,%al
  4012c5:	74 5c                	je     401323 <runtime::udivmod128+0x133>
  4012c7:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4012cc:	48 83 f8 00          	cmp    $0x0,%rax
  4012d0:	0f 95 c0             	setne  %al
  4012d3:	24 01                	and    $0x1,%al
  4012d5:	3c 00                	cmp    $0x0,%al
  4012d7:	74 29                	je     401302 <runtime::udivmod128+0x112>
  4012d9:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4012de:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4012e3:	31 d2                	xor    %edx,%edx
  4012e5:	48 f7 f1             	div    %rcx
  4012e8:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4012ed:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4012f2:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4012f7:	48 89 08             	mov    %rcx,(%rax)
  4012fa:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401301:	00 
  401302:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401307:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40130c:	31 d2                	xor    %edx,%edx
  40130e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  401313:	48 f7 f1             	div    %rcx
  401316:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  40131b:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401322:	c3                   	ret
  401323:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401328:	48 83 f8 00          	cmp    $0x0,%rax
  40132c:	0f 95 c0             	setne  %al
  40132f:	24 01                	and    $0x1,%al
  401331:	3c 00                	cmp    $0x0,%al
  401333:	74 15                	je     40134a <runtime::udivmod128+0x15a>
  401335:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40133a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40133f:	48 89 08             	mov    %rcx,(%rax)
  401342:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401349:	00 
  40134a:	31 c0                	xor    %eax,%eax
  40134c:	89 c2                	mov    %eax,%edx
  40134e:	48 89 d0             	mov    %rdx,%rax
  401351:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401358:	c3                   	ret
  401359:	48 83 7c 24 40 00    	cmpq   $0x0,0x40(%rsp)
  40135f:	0f 94 c0             	sete   %al
  401362:	24 01                	and    $0x1,%al
  401364:	3c 00                	cmp    $0x0,%al
  401366:	0f 84 8b 02 00 00    	je     4015f7 <runtime::udivmod128+0x407>
  40136c:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401372:	0f 94 c0             	sete   %al
  401375:	24 01                	and    $0x1,%al
  401377:	3c 00                	cmp    $0x0,%al
  401379:	74 52                	je     4013cd <runtime::udivmod128+0x1dd>
  40137b:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401380:	48 83 f8 00          	cmp    $0x0,%rax
  401384:	0f 95 c0             	setne  %al
  401387:	24 01                	and    $0x1,%al
  401389:	3c 00                	cmp    $0x0,%al
  40138b:	74 1f                	je     4013ac <runtime::udivmod128+0x1bc>
  40138d:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401392:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401397:	31 d2                	xor    %edx,%edx
  401399:	48 f7 f1             	div    %rcx
  40139c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4013a1:	48 89 10             	mov    %rdx,(%rax)
  4013a4:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4013ab:	00 
  4013ac:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4013b1:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4013b6:	31 d2                	xor    %edx,%edx
  4013b8:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  4013bd:	48 f7 f1             	div    %rcx
  4013c0:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  4013c5:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4013cc:	c3                   	ret
  4013cd:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  4013d3:	0f 94 c0             	sete   %al
  4013d6:	24 01                	and    $0x1,%al
  4013d8:	3c 00                	cmp    $0x0,%al
  4013da:	74 66                	je     401442 <runtime::udivmod128+0x252>
  4013dc:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4013e1:	48 83 f8 00          	cmp    $0x0,%rax
  4013e5:	0f 95 c0             	setne  %al
  4013e8:	24 01                	and    $0x1,%al
  4013ea:	3c 00                	cmp    $0x0,%al
  4013ec:	74 33                	je     401421 <runtime::udivmod128+0x231>
  4013ee:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4013f3:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4013f8:	31 d2                	xor    %edx,%edx
  4013fa:	48 f7 f1             	div    %rcx
  4013fd:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401402:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401407:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  40140e:	00 00 
  401410:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401415:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40141a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40141e:	48 89 08             	mov    %rcx,(%rax)
  401421:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401426:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40142b:	31 d2                	xor    %edx,%edx
  40142d:	48 89 54 24 90       	mov    %rdx,-0x70(%rsp)
  401432:	48 f7 f1             	div    %rcx
  401435:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  40143a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401441:	c3                   	ret
  401442:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401447:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40144c:	48 83 e9 01          	sub    $0x1,%rcx
  401450:	48 21 c8             	and    %rcx,%rax
  401453:	48 83 f8 00          	cmp    $0x0,%rax
  401457:	0f 94 c0             	sete   %al
  40145a:	24 01                	and    $0x1,%al
  40145c:	3c 00                	cmp    $0x0,%al
  40145e:	74 7a                	je     4014da <runtime::udivmod128+0x2ea>
  401460:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401465:	48 83 f8 00          	cmp    $0x0,%rax
  401469:	0f 95 c0             	setne  %al
  40146c:	24 01                	and    $0x1,%al
  40146e:	3c 00                	cmp    $0x0,%al
  401470:	74 35                	je     4014a7 <runtime::udivmod128+0x2b7>
  401472:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401477:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40147c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401481:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  401486:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40148b:	48 ff ca             	dec    %rdx
  40148e:	48 21 d1             	and    %rdx,%rcx
  401491:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401496:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40149b:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4014a0:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4014a4:	48 89 08             	mov    %rcx,(%rax)
  4014a7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4014ac:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4014b1:	ba 40 00 00 00       	mov    $0x40,%edx
  4014b6:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  4014bb:	88 d1                	mov    %dl,%cl
  4014bd:	48 d3 e8             	shr    %cl,%rax
  4014c0:	48 89 c1             	mov    %rax,%rcx
  4014c3:	31 c0                	xor    %eax,%eax
  4014c5:	48 83 ea 40          	sub    $0x40,%rdx
  4014c9:	89 c2                	mov    %eax,%edx
  4014cb:	48 89 d0             	mov    %rdx,%rax
  4014ce:	48 0f 42 c1          	cmovb  %rcx,%rax
  4014d2:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4014d9:	c3                   	ret
  4014da:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4014df:	b8 7f 00 00 00       	mov    $0x7f,%eax
  4014e4:	48 0f bd c1          	bsr    %rcx,%rax
  4014e8:	48 83 f0 3f          	xor    $0x3f,%rax
  4014ec:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  4014f1:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  4014f6:	48 0f bd ca          	bsr    %rdx,%rcx
  4014fa:	48 83 f1 3f          	xor    $0x3f,%rcx
  4014fe:	29 c8                	sub    %ecx,%eax
  401500:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401504:	83 7c 24 1c 3e       	cmpl   $0x3e,0x1c(%rsp)
  401509:	0f 97 c0             	seta   %al
  40150c:	24 01                	and    $0x1,%al
  40150e:	3c 00                	cmp    $0x0,%al
  401510:	74 37                	je     401549 <runtime::udivmod128+0x359>
  401512:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401517:	48 83 f8 00          	cmp    $0x0,%rax
  40151b:	0f 95 c0             	setne  %al
  40151e:	24 01                	and    $0x1,%al
  401520:	3c 00                	cmp    $0x0,%al
  401522:	74 16                	je     40153a <runtime::udivmod128+0x34a>
  401524:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401529:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  40152e:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401533:	48 89 10             	mov    %rdx,(%rax)
  401536:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40153a:	31 c0                	xor    %eax,%eax
  40153c:	89 c2                	mov    %eax,%edx
  40153e:	48 89 d0             	mov    %rdx,%rax
  401541:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401548:	c3                   	ret
  401549:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  40154d:	83 c0 01             	add    $0x1,%eax
  401550:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401554:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40155b:	00 00 
  40155d:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401562:	b9 40 00 00 00       	mov    $0x40,%ecx
  401567:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40156b:	89 c9                	mov    %ecx,%ecx
  40156d:	89 ca                	mov    %ecx,%edx
  40156f:	48 89 d1             	mov    %rdx,%rcx
  401572:	48 d3 e0             	shl    %cl,%rax
  401575:	48 89 c1             	mov    %rax,%rcx
  401578:	31 c0                	xor    %eax,%eax
  40157a:	48 83 fa 40          	cmp    $0x40,%rdx
  40157e:	48 0f 42 c1          	cmovb  %rcx,%rax
  401582:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401587:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40158c:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401590:	89 ca                	mov    %ecx,%edx
  401592:	48 89 d1             	mov    %rdx,%rcx
  401595:	48 d3 e8             	shr    %cl,%rax
  401598:	48 89 c1             	mov    %rax,%rcx
  40159b:	31 c0                	xor    %eax,%eax
  40159d:	48 83 fa 40          	cmp    $0x40,%rdx
  4015a1:	48 0f 42 c1          	cmovb  %rcx,%rax
  4015a5:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4015aa:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4015af:	b9 40 00 00 00       	mov    $0x40,%ecx
  4015b4:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4015b8:	89 c9                	mov    %ecx,%ecx
  4015ba:	89 ca                	mov    %ecx,%edx
  4015bc:	48 89 d1             	mov    %rdx,%rcx
  4015bf:	48 d3 e0             	shl    %cl,%rax
  4015c2:	48 89 c1             	mov    %rax,%rcx
  4015c5:	31 c0                	xor    %eax,%eax
  4015c7:	48 83 fa 40          	cmp    $0x40,%rdx
  4015cb:	48 0f 42 c1          	cmovb  %rcx,%rax
  4015cf:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4015d4:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4015d8:	89 ce                	mov    %ecx,%esi
  4015da:	48 89 f1             	mov    %rsi,%rcx
  4015dd:	48 d3 ea             	shr    %cl,%rdx
  4015e0:	31 c9                	xor    %ecx,%ecx
  4015e2:	48 83 fe 40          	cmp    $0x40,%rsi
  4015e6:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4015ea:	48 09 c8             	or     %rcx,%rax
  4015ed:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4015f2:	e9 30 04 00 00       	jmp    401a27 <runtime::udivmod128+0x837>
  4015f7:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  4015fd:	0f 94 c0             	sete   %al
  401600:	24 01                	and    $0x1,%al
  401602:	3c 00                	cmp    $0x0,%al
  401604:	0f 84 d1 02 00 00    	je     4018db <runtime::udivmod128+0x6eb>
  40160a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40160f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401614:	48 83 e9 01          	sub    $0x1,%rcx
  401618:	48 21 c8             	and    %rcx,%rax
  40161b:	48 83 f8 00          	cmp    $0x0,%rax
  40161f:	0f 94 c0             	sete   %al
  401622:	24 01                	and    $0x1,%al
  401624:	3c 00                	cmp    $0x0,%al
  401626:	0f 84 de 00 00 00    	je     40170a <runtime::udivmod128+0x51a>
  40162c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401631:	48 83 f8 00          	cmp    $0x0,%rax
  401635:	0f 95 c0             	setne  %al
  401638:	24 01                	and    $0x1,%al
  40163a:	3c 00                	cmp    $0x0,%al
  40163c:	74 20                	je     40165e <runtime::udivmod128+0x46e>
  40163e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401643:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401648:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40164d:	48 ff ca             	dec    %rdx
  401650:	48 21 d1             	and    %rdx,%rcx
  401653:	48 89 08             	mov    %rcx,(%rax)
  401656:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40165d:	00 
  40165e:	48 83 7c 24 40 01    	cmpq   $0x1,0x40(%rsp)
  401664:	0f 94 c0             	sete   %al
  401667:	24 01                	and    $0x1,%al
  401669:	3c 00                	cmp    $0x0,%al
  40166b:	74 12                	je     40167f <runtime::udivmod128+0x48f>
  40166d:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401672:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401677:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40167e:	c3                   	ret
  40167f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401684:	b8 40 00 00 00       	mov    $0x40,%eax
  401689:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  40168e:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401692:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401697:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  40169b:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  40169f:	88 c1                	mov    %al,%cl
  4016a1:	48 d3 ea             	shr    %cl,%rdx
  4016a4:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  4016a8:	31 c0                	xor    %eax,%eax
  4016aa:	83 e9 40             	sub    $0x40,%ecx
  4016ad:	48 89 c1             	mov    %rax,%rcx
  4016b0:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4016b4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4016b9:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4016be:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  4016c3:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  4016c7:	40 88 f1             	mov    %sil,%cl
  4016ca:	48 d3 ef             	shr    %cl,%rdi
  4016cd:	83 ee 40             	sub    $0x40,%esi
  4016d0:	48 89 c1             	mov    %rax,%rcx
  4016d3:	48 0f 42 cf          	cmovb  %rdi,%rcx
  4016d7:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  4016dc:	f7 de                	neg    %esi
  4016de:	40 88 f1             	mov    %sil,%cl
  4016e1:	48 d3 e2             	shl    %cl,%rdx
  4016e4:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  4016e9:	83 ee 40             	sub    $0x40,%esi
  4016ec:	48 0f 42 c2          	cmovb  %rdx,%rax
  4016f0:	48 09 c8             	or     %rcx,%rax
  4016f3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4016f8:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4016fd:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401702:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401709:	c3                   	ret
  40170a:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40170f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401714:	48 0f bd c1          	bsr    %rcx,%rax
  401718:	48 83 f0 3f          	xor    $0x3f,%rax
  40171c:	83 c0 41             	add    $0x41,%eax
  40171f:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401724:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401729:	48 0f bd ca          	bsr    %rdx,%rcx
  40172d:	48 83 f1 3f          	xor    $0x3f,%rcx
  401731:	29 c8                	sub    %ecx,%eax
  401733:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401737:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  40173c:	0f 94 c1             	sete   %cl
  40173f:	80 e1 01             	and    $0x1,%cl
  401742:	b0 01                	mov    $0x1,%al
  401744:	38 c8                	cmp    %cl,%al
  401746:	74 13                	je     40175b <runtime::udivmod128+0x56b>
  401748:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  40174d:	0f 92 c1             	setb   %cl
  401750:	80 e1 01             	and    $0x1,%cl
  401753:	b0 01                	mov    $0x1,%al
  401755:	38 c8                	cmp    %cl,%al
  401757:	74 32                	je     40178b <runtime::udivmod128+0x59b>
  401759:	eb 2b                	jmp    401786 <runtime::udivmod128+0x596>
  40175b:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401762:	00 00 
  401764:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401769:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40176e:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401775:	00 00 
  401777:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40177c:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401781:	e9 50 01 00 00       	jmp    4018d6 <runtime::udivmod128+0x6e6>
  401786:	e9 a3 00 00 00       	jmp    40182e <runtime::udivmod128+0x63e>
  40178b:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401792:	00 00 
  401794:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401799:	b9 40 00 00 00       	mov    $0x40,%ecx
  40179e:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4017a2:	89 c9                	mov    %ecx,%ecx
  4017a4:	89 ca                	mov    %ecx,%edx
  4017a6:	48 89 d1             	mov    %rdx,%rcx
  4017a9:	48 d3 e0             	shl    %cl,%rax
  4017ac:	48 89 c1             	mov    %rax,%rcx
  4017af:	31 c0                	xor    %eax,%eax
  4017b1:	48 83 fa 40          	cmp    $0x40,%rdx
  4017b5:	48 0f 42 c1          	cmovb  %rcx,%rax
  4017b9:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4017be:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4017c3:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4017c7:	89 ca                	mov    %ecx,%edx
  4017c9:	48 89 d1             	mov    %rdx,%rcx
  4017cc:	48 d3 e8             	shr    %cl,%rax
  4017cf:	48 89 c1             	mov    %rax,%rcx
  4017d2:	31 c0                	xor    %eax,%eax
  4017d4:	48 83 fa 40          	cmp    $0x40,%rdx
  4017d8:	48 0f 42 c1          	cmovb  %rcx,%rax
  4017dc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4017e1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4017e6:	b9 40 00 00 00       	mov    $0x40,%ecx
  4017eb:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4017ef:	89 c9                	mov    %ecx,%ecx
  4017f1:	89 ca                	mov    %ecx,%edx
  4017f3:	48 89 d1             	mov    %rdx,%rcx
  4017f6:	48 d3 e0             	shl    %cl,%rax
  4017f9:	48 89 c1             	mov    %rax,%rcx
  4017fc:	31 c0                	xor    %eax,%eax
  4017fe:	48 83 fa 40          	cmp    $0x40,%rdx
  401802:	48 0f 42 c1          	cmovb  %rcx,%rax
  401806:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40180b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40180f:	89 ce                	mov    %ecx,%esi
  401811:	48 89 f1             	mov    %rsi,%rcx
  401814:	48 d3 ea             	shr    %cl,%rdx
  401817:	31 c9                	xor    %ecx,%ecx
  401819:	48 83 fe 40          	cmp    $0x40,%rsi
  40181d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401821:	48 09 c8             	or     %rcx,%rax
  401824:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401829:	e9 a8 00 00 00       	jmp    4018d6 <runtime::udivmod128+0x6e6>
  40182e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401833:	b9 80 00 00 00       	mov    $0x80,%ecx
  401838:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40183c:	89 c9                	mov    %ecx,%ecx
  40183e:	89 ca                	mov    %ecx,%edx
  401840:	48 89 d1             	mov    %rdx,%rcx
  401843:	48 d3 e0             	shl    %cl,%rax
  401846:	48 89 c1             	mov    %rax,%rcx
  401849:	31 c0                	xor    %eax,%eax
  40184b:	48 83 fa 40          	cmp    $0x40,%rdx
  40184f:	48 0f 42 c1          	cmovb  %rcx,%rax
  401853:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401858:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40185d:	b9 80 00 00 00       	mov    $0x80,%ecx
  401862:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401866:	89 c9                	mov    %ecx,%ecx
  401868:	89 ca                	mov    %ecx,%edx
  40186a:	48 89 d1             	mov    %rdx,%rcx
  40186d:	48 d3 e0             	shl    %cl,%rax
  401870:	48 89 c1             	mov    %rax,%rcx
  401873:	31 c0                	xor    %eax,%eax
  401875:	48 83 fa 40          	cmp    $0x40,%rdx
  401879:	48 0f 42 c1          	cmovb  %rcx,%rax
  40187d:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401882:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401886:	83 e9 40             	sub    $0x40,%ecx
  401889:	89 c9                	mov    %ecx,%ecx
  40188b:	89 ce                	mov    %ecx,%esi
  40188d:	48 89 f1             	mov    %rsi,%rcx
  401890:	48 d3 ea             	shr    %cl,%rdx
  401893:	31 c9                	xor    %ecx,%ecx
  401895:	48 83 fe 40          	cmp    $0x40,%rsi
  401899:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40189d:	48 09 c8             	or     %rcx,%rax
  4018a0:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4018a5:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4018ac:	00 00 
  4018ae:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4018b3:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4018b7:	83 e9 40             	sub    $0x40,%ecx
  4018ba:	89 c9                	mov    %ecx,%ecx
  4018bc:	89 ca                	mov    %ecx,%edx
  4018be:	48 89 d1             	mov    %rdx,%rcx
  4018c1:	48 d3 e8             	shr    %cl,%rax
  4018c4:	48 89 c1             	mov    %rax,%rcx
  4018c7:	31 c0                	xor    %eax,%eax
  4018c9:	48 83 fa 40          	cmp    $0x40,%rdx
  4018cd:	48 0f 42 c1          	cmovb  %rcx,%rax
  4018d1:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4018d6:	e9 4a 01 00 00       	jmp    401a25 <runtime::udivmod128+0x835>
  4018db:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4018e0:	b8 7f 00 00 00       	mov    $0x7f,%eax
  4018e5:	48 0f bd c1          	bsr    %rcx,%rax
  4018e9:	48 83 f0 3f          	xor    $0x3f,%rax
  4018ed:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  4018f2:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  4018f7:	48 0f bd ca          	bsr    %rdx,%rcx
  4018fb:	48 83 f1 3f          	xor    $0x3f,%rcx
  4018ff:	29 c8                	sub    %ecx,%eax
  401901:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401905:	83 7c 24 1c 3f       	cmpl   $0x3f,0x1c(%rsp)
  40190a:	0f 97 c0             	seta   %al
  40190d:	24 01                	and    $0x1,%al
  40190f:	3c 00                	cmp    $0x0,%al
  401911:	74 37                	je     40194a <runtime::udivmod128+0x75a>
  401913:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401918:	48 83 f8 00          	cmp    $0x0,%rax
  40191c:	0f 95 c0             	setne  %al
  40191f:	24 01                	and    $0x1,%al
  401921:	3c 00                	cmp    $0x0,%al
  401923:	74 16                	je     40193b <runtime::udivmod128+0x74b>
  401925:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40192a:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  40192f:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401934:	48 89 10             	mov    %rdx,(%rax)
  401937:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40193b:	31 c0                	xor    %eax,%eax
  40193d:	89 c2                	mov    %eax,%edx
  40193f:	48 89 d0             	mov    %rdx,%rax
  401942:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401949:	c3                   	ret
  40194a:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  40194e:	83 c0 01             	add    $0x1,%eax
  401951:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401955:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40195c:	00 00 
  40195e:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401963:	0f 94 c0             	sete   %al
  401966:	24 01                	and    $0x1,%al
  401968:	3c 00                	cmp    $0x0,%al
  40196a:	74 22                	je     40198e <runtime::udivmod128+0x79e>
  40196c:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401971:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401976:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  40197d:	00 00 
  40197f:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401984:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401989:	e9 95 00 00 00       	jmp    401a23 <runtime::udivmod128+0x833>
  40198e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401993:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401997:	89 ca                	mov    %ecx,%edx
  401999:	48 89 d1             	mov    %rdx,%rcx
  40199c:	48 d3 e8             	shr    %cl,%rax
  40199f:	48 89 c1             	mov    %rax,%rcx
  4019a2:	31 c0                	xor    %eax,%eax
  4019a4:	48 83 fa 40          	cmp    $0x40,%rdx
  4019a8:	48 0f 42 c1          	cmovb  %rcx,%rax
  4019ac:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4019b1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4019b6:	b9 40 00 00 00       	mov    $0x40,%ecx
  4019bb:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4019bf:	89 c9                	mov    %ecx,%ecx
  4019c1:	89 ca                	mov    %ecx,%edx
  4019c3:	48 89 d1             	mov    %rdx,%rcx
  4019c6:	48 d3 e0             	shl    %cl,%rax
  4019c9:	48 89 c1             	mov    %rax,%rcx
  4019cc:	31 c0                	xor    %eax,%eax
  4019ce:	48 83 fa 40          	cmp    $0x40,%rdx
  4019d2:	48 0f 42 c1          	cmovb  %rcx,%rax
  4019d6:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4019db:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4019df:	89 ce                	mov    %ecx,%esi
  4019e1:	48 89 f1             	mov    %rsi,%rcx
  4019e4:	48 d3 ea             	shr    %cl,%rdx
  4019e7:	31 c9                	xor    %ecx,%ecx
  4019e9:	48 83 fe 40          	cmp    $0x40,%rsi
  4019ed:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4019f1:	48 09 c8             	or     %rcx,%rax
  4019f4:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4019f9:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4019fe:	b9 40 00 00 00       	mov    $0x40,%ecx
  401a03:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401a07:	89 c9                	mov    %ecx,%ecx
  401a09:	89 ca                	mov    %ecx,%edx
  401a0b:	48 89 d1             	mov    %rdx,%rcx
  401a0e:	48 d3 e0             	shl    %cl,%rax
  401a11:	48 89 c1             	mov    %rax,%rcx
  401a14:	31 c0                	xor    %eax,%eax
  401a16:	48 83 fa 40          	cmp    $0x40,%rdx
  401a1a:	48 0f 42 c1          	cmovb  %rcx,%rax
  401a1e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401a23:	eb 00                	jmp    401a25 <runtime::udivmod128+0x835>
  401a25:	eb 00                	jmp    401a27 <runtime::udivmod128+0x837>
  401a27:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  401a2e:	00 
  401a2f:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401a36:	00 00 
  401a38:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  401a3f:	00 00 
  401a41:	83 7c 24 1c 00       	cmpl   $0x0,0x1c(%rsp)
  401a46:	0f 97 c0             	seta   %al
  401a49:	24 01                	and    $0x1,%al
  401a4b:	3c 00                	cmp    $0x0,%al
  401a4d:	0f 84 eb 00 00 00    	je     401b3e <runtime::udivmod128+0x94e>
  401a53:	48 8b 74 24 b8       	mov    -0x48(%rsp),%rsi
  401a58:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  401a5d:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401a62:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401a67:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401a6c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401a71:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401a76:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  401a7b:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401a80:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401a85:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401a8a:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  401a8f:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401a94:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401a99:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401a9e:	48 01 c0             	add    %rax,%rax
  401aa1:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  401aa5:	48 09 c8             	or     %rcx,%rax
  401aa8:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401aad:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401ab2:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  401ab7:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  401abc:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401ac1:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401ac6:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401acb:	48 f7 d0             	not    %rax
  401ace:	48 f7 d1             	not    %rcx
  401ad1:	48 01 f1             	add    %rsi,%rcx
  401ad4:	48 11 d0             	adc    %rdx,%rax
  401ad7:	48 c1 f8 3f          	sar    $0x3f,%rax
  401adb:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401ae0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401ae5:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401ae9:	83 e0 01             	and    $0x1,%eax
  401aec:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  401af0:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401af5:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401afa:	48 21 ca             	and    %rcx,%rdx
  401afd:	48 21 c6             	and    %rax,%rsi
  401b00:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401b05:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401b0a:	48 29 f1             	sub    %rsi,%rcx
  401b0d:	48 19 d0             	sbb    %rdx,%rax
  401b10:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401b15:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401b1a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401b1f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401b24:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401b29:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401b2e:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401b32:	83 e8 01             	sub    $0x1,%eax
  401b35:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401b39:	e9 03 ff ff ff       	jmp    401a41 <runtime::udivmod128+0x851>
  401b3e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b43:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401b48:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401b4d:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  401b52:	48 01 c9             	add    %rcx,%rcx
  401b55:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  401b59:	48 09 f1             	or     %rsi,%rcx
  401b5c:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  401b61:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  401b66:	48 83 f8 00          	cmp    $0x0,%rax
  401b6a:	0f 95 c0             	setne  %al
  401b6d:	24 01                	and    $0x1,%al
  401b6f:	3c 00                	cmp    $0x0,%al
  401b71:	74 16                	je     401b89 <runtime::udivmod128+0x999>
  401b73:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b78:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401b7d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  401b82:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401b86:	48 89 08             	mov    %rcx,(%rax)
  401b89:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401b8e:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  401b93:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401b9a:	c3                   	ret
  401b9b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401ba0 <runtime::stderr_write>:
  401ba0:	48 83 ec 48          	sub    $0x48,%rsp
  401ba4:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  401ba9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  401bae:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  401bb3:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401bb8:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401bbd:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  401bc2:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  401bc7:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401bce:	00 00 
  401bd0:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  401bd5:	e8 16 00 00 00       	call   401bf0 <runtime::[os_specific_linux.odin]::_stderr_write>
  401bda:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401bdf:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  401be4:	48 89 11             	mov    %rdx,(%rcx)
  401be7:	48 83 c4 48          	add    $0x48,%rsp
  401beb:	c3                   	ret
  401bec:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401bf0 <runtime::[os_specific_linux.odin]::_stderr_write>:
  401bf0:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  401bf5:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  401bfa:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  401bff:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  401c04:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  401c09:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  401c0e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  401c13:	b8 01 00 00 00       	mov    $0x1,%eax
  401c18:	bf 02 00 00 00       	mov    $0x2,%edi
  401c1d:	0f 05                	syscall
  401c1f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401c24:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  401c2a:	0f 9c c0             	setl   %al
  401c2d:	24 01                	and    $0x1,%al
  401c2f:	3c 00                	cmp    $0x0,%al
  401c31:	74 26                	je     401c59 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  401c33:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  401c3a:	ff ff 
  401c3c:	0f 9f c0             	setg   %al
  401c3f:	24 01                	and    $0x1,%al
  401c41:	3c 00                	cmp    $0x0,%al
  401c43:	74 14                	je     401c59 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  401c45:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  401c4a:	31 c0                	xor    %eax,%eax
  401c4c:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  401c51:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  401c58:	c3                   	ret
  401c59:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401c5e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401c63:	48 89 08             	mov    %rcx,(%rax)
  401c66:	31 c0                	xor    %eax,%eax
  401c68:	c3                   	ret
  401c69:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401c70 <runtime::heap_allocator_proc>:
  401c70:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  401c77:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  401c7c:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  401c81:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401c86:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401c8b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  401c90:	40 88 f0             	mov    %sil,%al
  401c93:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  401c97:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401c9e:	00 
  401c9f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401ca4:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  401cab:	00 
  401cac:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  401cb1:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401cb5:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401cba:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401cbf:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401cc4:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401cc9:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  401cce:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  401cd5:	00 
  401cd6:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401cdd:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  401ce4:	00 
  401ce5:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  401cec:	00 
  401ced:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  401cf4:	00 
  401cf5:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  401cfc:	00 
  401cfd:	0f b6 c8             	movzbl %al,%ecx
  401d00:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  401d05:	2c 07                	sub    $0x7,%al
  401d07:	0f 87 5f 01 00 00    	ja     401e6c <runtime::heap_allocator_proc+0x1fc>
  401d0d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401d12:	48 8b 04 c5 10 70 40 	mov    0x407010(,%rax,8),%rax
  401d19:	00 
  401d1a:	ff e0                	jmp    *%rax
  401d1c:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401d21:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401d26:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401d2b:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401d2f:	84 c0                	test   %al,%al
  401d31:	0f 94 c0             	sete   %al
  401d34:	0f 57 c0             	xorps  %xmm0,%xmm0
  401d37:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  401d3e:	00 
  401d3f:	48 89 e1             	mov    %rsp,%rcx
  401d42:	48 89 11             	mov    %rdx,(%rcx)
  401d45:	44 0f b6 c0          	movzbl %al,%r8d
  401d49:	31 c0                	xor    %eax,%eax
  401d4b:	89 c1                	mov    %eax,%ecx
  401d4d:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  401d54:	00 
  401d55:	48 89 ca             	mov    %rcx,%rdx
  401d58:	e8 43 3f 00 00       	call   405ca0 <runtime::heap_allocator_proc.aligned_alloc-0>
  401d5d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401d62:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  401d69:	00 
  401d6a:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  401d71:	00 
  401d72:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401d76:	48 89 11             	mov    %rdx,(%rcx)
  401d79:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401d80:	c3                   	ret
  401d81:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  401d86:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401d8b:	e8 60 41 00 00       	call   405ef0 <runtime::heap_allocator_proc.aligned_free-1>
  401d90:	e9 d7 00 00 00       	jmp    401e6c <runtime::heap_allocator_proc+0x1fc>
  401d95:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401d9a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401da1:	00 
  401da2:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401da9:	b0 04                	mov    $0x4,%al
  401dab:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401db2:	c3                   	ret
  401db3:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401db8:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401dbd:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401dc2:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401dc7:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  401dcc:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401dd0:	2c 03                	sub    $0x3,%al
  401dd2:	0f 94 c0             	sete   %al
  401dd5:	0f 57 c0             	xorps  %xmm0,%xmm0
  401dd8:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  401ddd:	49 89 e0             	mov    %rsp,%r8
  401de0:	4d 89 08             	mov    %r9,(%r8)
  401de3:	44 0f b6 c0          	movzbl %al,%r8d
  401de7:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  401dec:	e8 3f 41 00 00       	call   405f30 <runtime::heap_allocator_proc.aligned_resize-2>
  401df1:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401df6:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  401dfb:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  401e00:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401e04:	48 89 11             	mov    %rdx,(%rcx)
  401e07:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401e0e:	c3                   	ret
  401e0f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401e14:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401e19:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  401e1f:	0f 95 c0             	setne  %al
  401e22:	24 01                	and    $0x1,%al
  401e24:	3c 00                	cmp    $0x0,%al
  401e26:	74 08                	je     401e30 <runtime::heap_allocator_proc+0x1c0>
  401e28:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  401e2d:	c6 00 db             	movb   $0xdb,(%rax)
  401e30:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401e35:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401e3c:	00 
  401e3d:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401e44:	31 c0                	xor    %eax,%eax
  401e46:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401e4d:	c3                   	ret
  401e4e:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401e53:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401e5a:	00 
  401e5b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401e62:	b0 04                	mov    $0x4,%al
  401e64:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401e6b:	c3                   	ret
  401e6c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401e71:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401e78:	00 
  401e79:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401e80:	31 c0                	xor    %eax,%eax
  401e82:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401e89:	c3                   	ret
  401e8a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401e90 <runtime::[internal.odin]::byte_slice>:
  401e90:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  401e95:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  401e9a:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401e9f:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401ea4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401ea9:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  401eae:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401eb3:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401eb8:	31 c0                	xor    %eax,%eax
  401eba:	48 85 d2             	test   %rdx,%rdx
  401ebd:	48 0f 49 c2          	cmovns %rdx,%rax
  401ec1:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  401ec6:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401ecb:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401ed0:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401ed5:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  401eda:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  401edf:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401ee4:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  401ee9:	c3                   	ret
  401eea:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401ef0 <runtime::bounds_check_error>:
  401ef0:	48 83 ec 58          	sub    $0x58,%rsp
  401ef4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  401ef9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  401efe:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  401f02:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  401f06:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  401f0b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  401f10:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401f15:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401f1a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  401f1e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  401f22:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  401f27:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  401f2c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  401f31:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  401f36:	89 74 24 44          	mov    %esi,0x44(%rsp)
  401f3a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  401f3e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401f43:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  401f48:	48 39 c8             	cmp    %rcx,%rax
  401f4b:	0f 92 c0             	setb   %al
  401f4e:	24 01                	and    $0x1,%al
  401f50:	3c 00                	cmp    $0x0,%al
  401f52:	74 05                	je     401f59 <runtime::bounds_check_error+0x69>
  401f54:	48 83 c4 58          	add    $0x58,%rsp
  401f58:	c3                   	ret
  401f59:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  401f5e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  401f63:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  401f67:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  401f6b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401f70:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401f75:	e8 76 42 00 00       	call   4061f0 <runtime::bounds_check_error.handle_error-0>
  401f7a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401f80 <runtime::is_power_of_two_int>:
  401f80:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  401f85:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401f8a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401f8f:	48 83 f8 00          	cmp    $0x0,%rax
  401f93:	0f 9e c0             	setle  %al
  401f96:	24 01                	and    $0x1,%al
  401f98:	3c 00                	cmp    $0x0,%al
  401f9a:	74 03                	je     401f9f <runtime::is_power_of_two_int+0x1f>
  401f9c:	31 c0                	xor    %eax,%eax
  401f9e:	c3                   	ret
  401f9f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401fa4:	48 89 c1             	mov    %rax,%rcx
  401fa7:	48 83 e9 01          	sub    $0x1,%rcx
  401fab:	48 21 c8             	and    %rcx,%rax
  401fae:	48 83 f8 00          	cmp    $0x0,%rax
  401fb2:	0f 94 c0             	sete   %al
  401fb5:	24 01                	and    $0x1,%al
  401fb7:	c3                   	ret
  401fb8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  401fbf:	00 

0000000000401fc0 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  401fc0:	48 83 ec 18          	sub    $0x18,%rsp
  401fc4:	48 89 3c 24          	mov    %rdi,(%rsp)
  401fc8:	40 88 f0             	mov    %sil,%al
  401fcb:	88 44 24 0e          	mov    %al,0xe(%rsp)
  401fcf:	48 8b 04 24          	mov    (%rsp),%rax
  401fd3:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  401fd7:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  401fdc:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  401fe0:	48 83 f8 00          	cmp    $0x0,%rax
  401fe4:	0f 9e c0             	setle  %al
  401fe7:	24 01                	and    $0x1,%al
  401fe9:	3c 00                	cmp    $0x0,%al
  401feb:	74 07                	je     401ff4 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  401fed:	31 c0                	xor    %eax,%eax
  401fef:	48 83 c4 18          	add    $0x18,%rsp
  401ff3:	c3                   	ret
  401ff4:	8a 44 24 0e          	mov    0xe(%rsp),%al
  401ff8:	3c 00                	cmp    $0x0,%al
  401ffa:	74 13                	je     40200f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  401ffc:	48 8b 34 24          	mov    (%rsp),%rsi
  402000:	bf 01 00 00 00       	mov    $0x1,%edi
  402005:	e8 46 f0 ff ff       	call   401050 <calloc@plt>
  40200a:	48 83 c4 18          	add    $0x18,%rsp
  40200e:	c3                   	ret
  40200f:	48 8b 3c 24          	mov    (%rsp),%rdi
  402013:	e8 58 f0 ff ff       	call   401070 <malloc@plt>
  402018:	48 83 c4 18          	add    $0x18,%rsp
  40201c:	c3                   	ret
  40201d:	0f 1f 00             	nopl   (%rax)

0000000000402020 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  402020:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  402025:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  40202a:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  40202f:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  402034:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  402039:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40203e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  402043:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  402048:	48 01 c2             	add    %rax,%rdx
  40204b:	0f 92 c0             	setb   %al
  40204e:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  402053:	24 01                	and    $0x1,%al
  402055:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  402059:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40205e:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  402063:	0f 94 c0             	sete   %al
  402066:	24 01                	and    $0x1,%al
  402068:	48 89 11             	mov    %rdx,(%rcx)
  40206b:	c3                   	ret
  40206c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402070 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  402070:	48 83 ec 28          	sub    $0x28,%rsp
  402074:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402079:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40207e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402083:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402088:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40208d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402092:	e8 e9 ef ff ff       	call   401080 <realloc@plt>
  402097:	48 83 c4 28          	add    $0x28,%rsp
  40209b:	c3                   	ret
  40209c:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004020a0 <runtime::memory_block_alloc>:
  4020a0:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  4020a7:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  4020ac:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  4020b1:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4020b6:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4020bb:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  4020c0:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  4020c5:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  4020cc:	00 
  4020cd:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4020d2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  4020d7:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  4020dc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4020e1:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4020e6:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  4020eb:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  4020f0:	48 89 b4 24 f0 00 00 	mov    %rsi,0xf0(%rsp)
  4020f7:	00 
  4020f8:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  4020ff:	00 
  402100:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  402107:	00 
  402108:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40210d:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  402114:	00 
  402115:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40211a:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  402121:	00 
  402122:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  402129:	00 
  40212a:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  402131:	00 00 00 00 00 
  402136:	c6 84 24 d7 00 00 00 	movb   $0x0,0xd7(%rsp)
  40213d:	00 
  40213e:	48 89 c1             	mov    %rax,%rcx
  402141:	48 83 e9 31          	sub    $0x31,%rcx
  402145:	b9 30 00 00 00       	mov    $0x30,%ecx
  40214a:	48 0f 43 c8          	cmovae %rax,%rcx
  40214e:	48 01 ca             	add    %rcx,%rdx
  402151:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  402158:	00 
  402159:	48 89 8c 24 c0 00 00 	mov    %rcx,0xc0(%rsp)
  402160:	00 
  402161:	48 89 c1             	mov    %rax,%rcx
  402164:	48 83 e9 10          	sub    $0x10,%rcx
  402168:	b9 10 00 00 00       	mov    $0x10,%ecx
  40216d:	48 0f 4c c1          	cmovl  %rcx,%rax
  402171:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  402178:	00 
  402179:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  402180:	00 
  402181:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  402188:	00 
  402189:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  402190:	00 
  402191:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  402198:	00 
  402199:	0f 57 c0             	xorps  %xmm0,%xmm0
  40219c:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4021a3:	00 
  4021a4:	48 89 e0             	mov    %rsp,%rax
  4021a7:	4c 89 08             	mov    %r9,(%rax)
  4021aa:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  4021b1:	00 
  4021b2:	e8 f9 13 00 00       	call   4035b0 <runtime::mem_alloc>
  4021b7:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  4021bb:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4021c2:	00 
  4021c3:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4021c8:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  4021cf:	00 
  4021d0:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4021d5:	3c 00                	cmp    $0x0,%al
  4021d7:	74 39                	je     402212 <runtime::memory_block_alloc+0x172>
  4021d9:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4021de:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  4021e2:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  4021e9:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  4021f0:	00 
  4021f1:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  4021f8:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  4021ff:	00 
  402200:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402207:	48 89 11             	mov    %rdx,(%rcx)
  40220a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402211:	c3                   	ret
  402212:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  402217:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40221c:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402221:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402226:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40222b:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  402230:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402237:	00 
  402238:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40223d:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  402244:	00 
  402245:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40224a:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  402251:	00 
  402252:	48 01 f0             	add    %rsi,%rax
  402255:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40225a:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40225f:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  402264:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40226b:	00 
  40226c:	48 89 50 10          	mov    %rdx,0x10(%rax)
  402270:	48 89 48 08          	mov    %rcx,0x8(%rax)
  402274:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40227b:	00 
  40227c:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  402283:	00 
  402284:	48 03 8c 24 c0 00 00 	add    0xc0(%rsp),%rcx
  40228b:	00 
  40228c:	48 89 48 18          	mov    %rcx,0x18(%rax)
  402290:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402297:	00 
  402298:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40229d:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  4022a4:	00 
  4022a5:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  4022a9:	48 29 d1             	sub    %rdx,%rcx
  4022ac:	48 89 48 28          	mov    %rcx,0x28(%rax)
  4022b0:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4022b7:	00 
  4022b8:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  4022bd:	0f 94 c0             	sete   %al
  4022c0:	24 01                	and    $0x1,%al
  4022c2:	0f b6 f8             	movzbl %al,%edi
  4022c5:	be 90 70 40 00       	mov    $0x407090,%esi
  4022ca:	b9 00 71 40 00       	mov    $0x407100,%ecx
  4022cf:	ba 0f 00 00 00       	mov    $0xf,%edx
  4022d4:	e8 67 39 00 00       	call   405c40 <runtime::assert>
  4022d9:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  4022de:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4022e5:	00 
  4022e6:	48 83 38 00          	cmpq   $0x0,(%rax)
  4022ea:	0f 94 c0             	sete   %al
  4022ed:	24 01                	and    $0x1,%al
  4022ef:	0f b6 f8             	movzbl %al,%edi
  4022f2:	be 28 71 40 00       	mov    $0x407128,%esi
  4022f7:	b9 40 71 40 00       	mov    $0x407140,%ecx
  4022fc:	ba 11 00 00 00       	mov    $0x11,%edx
  402301:	e8 3a 39 00 00       	call   405c40 <runtime::assert>
  402306:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40230b:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402312:	00 
  402313:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  40231a:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  402321:	00 
  402322:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402329:	48 89 11             	mov    %rdx,(%rcx)
  40232c:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402333:	c3                   	ret
  402334:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40233b:	00 00 00 00 00 

0000000000402340 <runtime::default_temp_allocator_destroy>:
  402340:	48 83 ec 18          	sub    $0x18,%rsp
  402344:	48 89 3c 24          	mov    %rdi,(%rsp)
  402348:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40234d:	48 8b 04 24          	mov    (%rsp),%rax
  402351:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402356:	48 83 f8 00          	cmp    $0x0,%rax
  40235a:	0f 95 c0             	setne  %al
  40235d:	24 01                	and    $0x1,%al
  40235f:	3c 00                	cmp    $0x0,%al
  402361:	74 29                	je     40238c <runtime::default_temp_allocator_destroy+0x4c>
  402363:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402368:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40236d:	48 be d0 71 40 00 00 	movabs $0x4071d0,%rsi
  402374:	00 00 00 
  402377:	e8 14 1b 00 00       	call   403e90 <runtime::arena_destroy>
  40237c:	48 8b 3c 24          	mov    (%rsp),%rdi
  402380:	31 f6                	xor    %esi,%esi
  402382:	ba 38 00 00 00       	mov    $0x38,%edx
  402387:	e8 b4 ec ff ff       	call   401040 <memset@plt>
  40238c:	48 83 c4 18          	add    $0x18,%rsp
  402390:	c3                   	ret
  402391:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402398:	0f 1f 84 00 00 00 00 
  40239f:	00 

00000000004023a0 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  4023a0:	48 83 ec 18          	sub    $0x18,%rsp
  4023a4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4023a9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4023ae:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4023b3:	e8 78 ec ff ff       	call   401030 <free@plt>
  4023b8:	48 83 c4 18          	add    $0x18,%rsp
  4023bc:	c3                   	ret
  4023bd:	0f 1f 00             	nopl   (%rax)

00000000004023c0 <runtime::default_random_generator_proc>:
  4023c0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4023c7:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4023cc:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4023d1:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4023d6:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4023db:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4023e0:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4023e5:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4023ea:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4023ef:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4023f6:	00 
  4023f7:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4023fc:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  402401:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  402406:	48 83 f8 00          	cmp    $0x0,%rax
  40240a:	0f 94 c0             	sete   %al
  40240d:	24 01                	and    $0x1,%al
  40240f:	3c 00                	cmp    $0x0,%al
  402411:	74 1a                	je     40242d <runtime::default_random_generator_proc+0x6d>
  402413:	48 c7 c1 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rcx
  40241a:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402421:	00 00 
  402423:	48 01 c8             	add    %rcx,%rax
  402426:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40242b:	eb 0a                	jmp    402437 <runtime::default_random_generator_proc+0x77>
  40242d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402432:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402437:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40243c:	48 85 c0             	test   %rax,%rax
  40243f:	74 27                	je     402468 <runtime::default_random_generator_proc+0xa8>
  402441:	eb 00                	jmp    402443 <runtime::default_random_generator_proc+0x83>
  402443:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402448:	48 83 e8 01          	sub    $0x1,%rax
  40244c:	0f 84 17 01 00 00    	je     402569 <runtime::default_random_generator_proc+0x1a9>
  402452:	eb 00                	jmp    402454 <runtime::default_random_generator_proc+0x94>
  402454:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402459:	48 83 e8 02          	sub    $0x2,%rax
  40245d:	0f 84 40 01 00 00    	je     4025a3 <runtime::default_random_generator_proc+0x1e3>
  402463:	e9 6b 01 00 00       	jmp    4025d3 <runtime::default_random_generator_proc+0x213>
  402468:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40246d:	48 83 38 00          	cmpq   $0x0,(%rax)
  402471:	0f 94 c0             	sete   %al
  402474:	24 01                	and    $0x1,%al
  402476:	3c 00                	cmp    $0x0,%al
  402478:	74 21                	je     40249b <runtime::default_random_generator_proc+0xdb>
  40247a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40247f:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  402484:	0f 94 c0             	sete   %al
  402487:	24 01                	and    $0x1,%al
  402489:	3c 00                	cmp    $0x0,%al
  40248b:	74 0e                	je     40249b <runtime::default_random_generator_proc+0xdb>
  40248d:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402492:	31 c0                	xor    %eax,%eax
  402494:	89 c6                	mov    %eax,%esi
  402496:	e8 15 3f 00 00       	call   4063b0 <runtime::default_random_generator_proc.init-1>
  40249b:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4024a0:	48 83 e8 08          	sub    $0x8,%rax
  4024a4:	75 26                	jne    4024cc <runtime::default_random_generator_proc+0x10c>
  4024a6:	eb 00                	jmp    4024a8 <runtime::default_random_generator_proc+0xe8>
  4024a8:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4024ad:	e8 2e 3e 00 00       	call   4062e0 <runtime::default_random_generator_proc.read_u64-0>
  4024b2:	48 89 c1             	mov    %rax,%rcx
  4024b5:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4024ba:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4024bf:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4024c4:	48 89 08             	mov    %rcx,(%rax)
  4024c7:	e9 9b 00 00 00       	jmp    402567 <runtime::default_random_generator_proc+0x1a7>
  4024cc:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4024d1:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  4024d6:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  4024dd:	00 00 
  4024df:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4024e4:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  4024eb:	ff ff 
  4024ed:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4024f2:	48 83 c0 01          	add    $0x1,%rax
  4024f6:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4024fb:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  402500:	7d 63                	jge    402565 <runtime::default_random_generator_proc+0x1a5>
  402502:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402507:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40250c:	48 01 c8             	add    %rcx,%rax
  40250f:	48 89 04 24          	mov    %rax,(%rsp)
  402513:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  402518:	0f 94 c0             	sete   %al
  40251b:	24 01                	and    $0x1,%al
  40251d:	3c 00                	cmp    $0x0,%al
  40251f:	74 14                	je     402535 <runtime::default_random_generator_proc+0x175>
  402521:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402526:	e8 b5 3d 00 00       	call   4062e0 <runtime::default_random_generator_proc.read_u64-0>
  40252b:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402530:	c6 44 24 57 07       	movb   $0x7,0x57(%rsp)
  402535:	48 8b 04 24          	mov    (%rsp),%rax
  402539:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40253e:	88 08                	mov    %cl,(%rax)
  402540:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402545:	48 c1 e9 08          	shr    $0x8,%rcx
  402549:	b2 01                	mov    $0x1,%dl
  40254b:	31 c0                	xor    %eax,%eax
  40254d:	f6 c2 01             	test   $0x1,%dl
  402550:	48 0f 45 c1          	cmovne %rcx,%rax
  402554:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402559:	8a 44 24 57          	mov    0x57(%rsp),%al
  40255d:	2c 01                	sub    $0x1,%al
  40255f:	88 44 24 57          	mov    %al,0x57(%rsp)
  402563:	eb 88                	jmp    4024ed <runtime::default_random_generator_proc+0x12d>
  402565:	eb 00                	jmp    402567 <runtime::default_random_generator_proc+0x1a7>
  402567:	eb 6a                	jmp    4025d3 <runtime::default_random_generator_proc+0x213>
  402569:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40256e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402573:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40257a:	00 00 
  40257c:	b8 08 00 00 00       	mov    $0x8,%eax
  402581:	48 39 d0             	cmp    %rdx,%rax
  402584:	48 0f 4c d0          	cmovl  %rax,%rdx
  402588:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40258d:	e8 5e 09 00 00       	call   402ef0 <runtime::mem_copy_non_overlapping>
  402592:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402597:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40259c:	e8 0f 3e 00 00       	call   4063b0 <runtime::default_random_generator_proc.init-1>
  4025a1:	eb 30                	jmp    4025d3 <runtime::default_random_generator_proc+0x213>
  4025a3:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4025a8:	48 83 f8 04          	cmp    $0x4,%rax
  4025ac:	0f 95 c0             	setne  %al
  4025af:	24 01                	and    $0x1,%al
  4025b1:	3c 00                	cmp    $0x0,%al
  4025b3:	74 08                	je     4025bd <runtime::default_random_generator_proc+0x1fd>
  4025b5:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4025bc:	c3                   	ret
  4025bd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4025c2:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4025c7:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4025cc:	8b 08                	mov    (%rax),%ecx
  4025ce:	83 c9 0a             	or     $0xa,%ecx
  4025d1:	89 08                	mov    %ecx,(%rax)
  4025d3:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4025da:	c3                   	ret
  4025db:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004025e0 <runtime::slice_handle_error>:
  4025e0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4025e7:	4c 89 0c 24          	mov    %r9,(%rsp)
  4025eb:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  4025f0:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  4025f4:	89 54 24 14          	mov    %edx,0x14(%rsp)
  4025f8:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4025fd:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  402602:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  402609:	00 
  40260a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40260f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402614:	4c 8b 04 24          	mov    (%rsp),%r8
  402618:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40261d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  402621:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  402625:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40262a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40262f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402634:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40263b:	00 
  40263c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402640:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402644:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  402649:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  40264e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402653:	0f 57 c0             	xorps  %xmm0,%xmm0
  402656:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40265b:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402660:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402667:	00 00 
  402669:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40266e:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402673:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40267a:	00 00 
  40267c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  402681:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402686:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  40268a:	89 44 24 44          	mov    %eax,0x44(%rsp)
  40268e:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402693:	e8 c8 16 00 00       	call   403d60 <runtime::print_caller_location>
  402698:	bf f9 71 40 00       	mov    $0x4071f9,%edi
  40269d:	be 17 00 00 00       	mov    $0x17,%esi
  4026a2:	e8 99 0e 00 00       	call   403540 <runtime::print_string>
  4026a7:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4026ac:	e8 af 14 00 00       	call   403b60 <runtime::print_i64>
  4026b1:	bf 11 72 40 00       	mov    $0x407211,%edi
  4026b6:	be 01 00 00 00       	mov    $0x1,%esi
  4026bb:	e8 80 0e 00 00       	call   403540 <runtime::print_string>
  4026c0:	48 8b 3c 24          	mov    (%rsp),%rdi
  4026c4:	e8 97 14 00 00       	call   403b60 <runtime::print_i64>
  4026c9:	bf 13 72 40 00       	mov    $0x407213,%edi
  4026ce:	be 15 00 00 00       	mov    $0x15,%esi
  4026d3:	e8 68 0e 00 00       	call   403540 <runtime::print_string>
  4026d8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4026dd:	e8 7e 14 00 00       	call   403b60 <runtime::print_i64>
  4026e2:	bf 0a 00 00 00       	mov    $0xa,%edi
  4026e7:	e8 84 10 00 00       	call   403770 <runtime::print_byte>
  4026ec:	e8 cf ea ff ff       	call   4011c0 <runtime::bounds_trap>
  4026f1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4026f8:	0f 1f 84 00 00 00 00 
  4026ff:	00 

0000000000402700 <runtime::default_temp_allocator_proc>:
  402700:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  402707:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  40270c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  402711:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402716:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  40271b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  402720:	40 88 f0             	mov    %sil,%al
  402723:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  402727:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40272e:	00 
  40272f:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402734:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40273b:	00 
  40273c:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  402741:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  402748:	00 
  402749:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40274e:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  402753:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402758:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40275d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  402762:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  402766:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  40276b:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  402770:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  402775:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  40277c:	00 
  40277d:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  402784:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  40278b:	00 
  40278c:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  402793:	00 
  402794:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  40279b:	00 
  40279c:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  4027a3:	00 
  4027a4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4027a7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4027ae:	00 
  4027af:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  4027b6:	00 
  4027b7:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  4027be:	00 
  4027bf:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  4027c6:	00 
  4027c7:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  4027ce:	00 
  4027cf:	48 89 e6             	mov    %rsp,%rsi
  4027d2:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  4027d6:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  4027dd:	00 
  4027de:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  4027e2:	4c 89 16             	mov    %r10,(%rsi)
  4027e5:	0f b6 f0             	movzbl %al,%esi
  4027e8:	e8 43 17 00 00       	call   403f30 <runtime::arena_allocator_proc>
  4027ed:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4027f2:	40 88 c7             	mov    %al,%dil
  4027f5:	40 88 f8             	mov    %dil,%al
  4027f8:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  4027ff:	00 
  402800:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  402807:	00 
  402808:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40280f:	00 
  402810:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402817:	00 
  402818:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  40281f:	00 
  402820:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402824:	48 89 11             	mov    %rdx,(%rcx)
  402827:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40282e:	c3                   	ret
  40282f:	90                   	nop

0000000000402830 <runtime::multi_pointer_slice_handle_error>:
  402830:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402837:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40283c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402841:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402845:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402849:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40284e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402853:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402858:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40285d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  402861:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402865:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40286a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40286f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402874:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40287b:	00 
  40287c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402880:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402884:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  402889:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40288e:	0f 57 c0             	xorps  %xmm0,%xmm0
  402891:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402896:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40289b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4028a2:	00 00 
  4028a4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4028a9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4028ae:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4028b5:	00 00 
  4028b7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4028bc:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4028c1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4028c5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4028c9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4028ce:	e8 8d 14 00 00       	call   403d60 <runtime::print_caller_location>
  4028d3:	bf f9 71 40 00       	mov    $0x4071f9,%edi
  4028d8:	be 17 00 00 00       	mov    $0x17,%esi
  4028dd:	e8 5e 0c 00 00       	call   403540 <runtime::print_string>
  4028e2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4028e7:	e8 74 12 00 00       	call   403b60 <runtime::print_i64>
  4028ec:	bf 11 72 40 00       	mov    $0x407211,%edi
  4028f1:	be 01 00 00 00       	mov    $0x1,%esi
  4028f6:	e8 45 0c 00 00       	call   403540 <runtime::print_string>
  4028fb:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402900:	e8 5b 12 00 00       	call   403b60 <runtime::print_i64>
  402905:	bf 0a 00 00 00       	mov    $0xa,%edi
  40290a:	e8 61 0e 00 00       	call   403770 <runtime::print_byte>
  40290f:	e8 ac e8 ff ff       	call   4011c0 <runtime::bounds_trap>
  402914:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40291b:	00 00 00 00 00 

0000000000402920 <runtime::memory_block_dealloc>:
  402920:	48 83 ec 38          	sub    $0x38,%rsp
  402924:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402929:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40292e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402933:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402938:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40293d:	48 83 f8 00          	cmp    $0x0,%rax
  402941:	0f 95 c0             	setne  %al
  402944:	24 01                	and    $0x1,%al
  402946:	3c 00                	cmp    $0x0,%al
  402948:	74 35                	je     40297f <runtime::memory_block_dealloc+0x5f>
  40294a:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  40294f:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402954:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402959:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40295e:	48 8b 42 08          	mov    0x8(%rdx),%rax
  402962:	48 8b 52 10          	mov    0x10(%rdx),%rdx
  402966:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40296b:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402970:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402975:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40297a:	e8 f1 0f 00 00       	call   403970 <runtime::mem_free>
  40297f:	48 83 c4 38          	add    $0x38,%rsp
  402983:	c3                   	ret
  402984:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40298b:	00 00 00 00 00 

0000000000402990 <main>:
  402990:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  402997:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  40299b:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4029a0:	8b 44 24 14          	mov    0x14(%rsp),%eax
  4029a4:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4029a9:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  4029b0:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  4029b7:	00 
  4029b8:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  4029bf:	00 
  4029c0:	48 89 0c 24          	mov    %rcx,(%rsp)
  4029c4:	4c 63 c8             	movslq %eax,%r9
  4029c7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4029cc:	bf 29 72 40 00       	mov    $0x407229,%edi
  4029d1:	31 c0                	xor    %eax,%eax
  4029d3:	41 89 c0             	mov    %eax,%r8d
  4029d6:	be 2c 00 00 00       	mov    $0x2c,%esi
  4029db:	ba 36 00 00 00       	mov    $0x36,%edx
  4029e0:	b9 11 00 00 00       	mov    $0x11,%ecx
  4029e5:	e8 c6 03 00 00       	call   402db0 <runtime::multi_pointer_slice_expr_error>
  4029ea:	48 8b 0c 24          	mov    (%rsp),%rcx
  4029ee:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4029f3:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  4029fa:	00 
  4029fb:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  402a02:	00 
  402a03:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  402a0a:	00 
  402a0b:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  402a12:	00 
  402a13:	48 c7 c0 60 a0 40 00 	mov    $0x40a060,%rax
  402a1a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402a1e:	48 89 08             	mov    %rcx,(%rax)
  402a21:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402a28:	00 
  402a29:	31 f6                	xor    %esi,%esi
  402a2b:	ba 70 00 00 00       	mov    $0x70,%edx
  402a30:	e8 0b e6 ff ff       	call   401040 <memset@plt>
  402a35:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402a3c:	00 
  402a3d:	e8 fe 24 00 00       	call   404f40 <runtime::[core.odin]::__init_context>
  402a42:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  402a47:	31 f6                	xor    %esi,%esi
  402a49:	ba 70 00 00 00       	mov    $0x70,%edx
  402a4e:	e8 ed e5 ff ff       	call   401040 <memset@plt>
  402a53:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  402a58:	e8 93 24 00 00       	call   404ef0 <runtime::default_context>
  402a5d:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  402a62:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  402a67:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  402a6c:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  402a71:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  402a76:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  402a7b:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  402a82:	00 
  402a83:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  402a8a:	00 
  402a8b:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  402a92:	00 
  402a93:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  402a9a:	00 
  402a9b:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  402aa2:	00 
  402aa3:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  402aaa:	00 
  402aab:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  402ab2:	00 
  402ab3:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  402aba:	00 
  402abb:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402ac2:	00 
  402ac3:	e8 b8 40 00 00       	call   406b80 <__$startup_runtime>
  402ac8:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402acf:	00 
  402ad0:	e8 5b 3c 00 00       	call   406730 <journey::main>
  402ad5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402adc:	00 
  402add:	e8 ae 40 00 00       	call   406b90 <__$cleanup_runtime>
  402ae2:	31 c0                	xor    %eax,%eax
  402ae4:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  402aeb:	c3                   	ret
  402aec:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402af0 <runtime::alloc_from_memory_block>:
  402af0:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  402af7:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402afc:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402b01:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402b06:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402b0b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402b10:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402b15:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402b1a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  402b21:	00 
  402b22:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  402b29:	00 
  402b2a:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  402b31:	00 
  402b32:	0f 57 c0             	xorps  %xmm0,%xmm0
  402b35:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402b3c:	00 
  402b3d:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  402b44:	00 
  402b45:	48 83 f8 00          	cmp    $0x0,%rax
  402b49:	0f 94 c0             	sete   %al
  402b4c:	24 01                	and    $0x1,%al
  402b4e:	3c 00                	cmp    $0x0,%al
  402b50:	74 3e                	je     402b90 <runtime::alloc_from_memory_block+0xa0>
  402b52:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402b57:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  402b5e:	00 00 00 00 00 
  402b63:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  402b6a:	00 00 00 00 00 
  402b6f:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402b76:	01 
  402b77:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402b7e:	00 
  402b7f:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402b86:	b0 01                	mov    $0x1,%al
  402b88:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402b8f:	c3                   	ret
  402b90:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  402b95:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402b9a:	e8 b1 38 00 00       	call   406450 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  402b9f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402ba4:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  402bab:	00 
  402bac:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  402bb3:	00 
  402bb4:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  402bbb:	00 00 00 00 00 
  402bc0:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  402bc7:	00 
  402bc8:	e8 53 f4 ff ff       	call   402020 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  402bcd:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  402bd4:	00 
  402bd5:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  402bda:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  402bde:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  402be3:	75 4a                	jne    402c2f <runtime::alloc_from_memory_block+0x13f>
  402be5:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402bea:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402bf1:	01 
  402bf2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402bf9:	00 
  402bfa:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402c01:	00 
  402c02:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402c09:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402c10:	00 
  402c11:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402c18:	00 
  402c19:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402c20:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402c24:	48 89 11             	mov    %rdx,(%rcx)
  402c27:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402c2e:	c3                   	ret
  402c2f:	eb 00                	jmp    402c31 <runtime::alloc_from_memory_block+0x141>
  402c31:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  402c38:	00 
  402c39:	48 8b 78 20          	mov    0x20(%rax),%rdi
  402c3d:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  402c42:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  402c49:	00 00 
  402c4b:	48 8d 54 24 60       	lea    0x60(%rsp),%rdx
  402c50:	e8 cb f3 ff ff       	call   402020 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  402c55:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  402c5a:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  402c5f:	88 44 24 47          	mov    %al,0x47(%rsp)
  402c63:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  402c68:	74 1a                	je     402c84 <runtime::alloc_from_memory_block+0x194>
  402c6a:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402c6f:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402c76:	00 
  402c77:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  402c7b:	0f 97 c0             	seta   %al
  402c7e:	24 01                	and    $0x1,%al
  402c80:	3c 00                	cmp    $0x0,%al
  402c82:	74 4a                	je     402cce <runtime::alloc_from_memory_block+0x1de>
  402c84:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402c89:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402c90:	01 
  402c91:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402c98:	00 
  402c99:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402ca0:	00 
  402ca1:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402ca8:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402caf:	00 
  402cb0:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402cb7:	00 
  402cb8:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402cbf:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402cc3:	48 89 11             	mov    %rdx,(%rcx)
  402cc6:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402ccd:	c3                   	ret
  402cce:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  402cd3:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402cda:	00 
  402cdb:	48 8b 41 18          	mov    0x18(%rcx),%rax
  402cdf:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  402ce3:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  402cea:	00 
  402ceb:	48 01 d1             	add    %rdx,%rcx
  402cee:	48 01 c8             	add    %rcx,%rax
  402cf1:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402cf6:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402cfb:	48 89 04 24          	mov    %rax,(%rsp)
  402cff:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  402d04:	31 c0                	xor    %eax,%eax
  402d06:	41 89 c0             	mov    %eax,%r8d
  402d09:	be 3e 00 00 00       	mov    $0x3e,%esi
  402d0e:	ba 55 00 00 00       	mov    $0x55,%edx
  402d13:	b9 31 00 00 00       	mov    $0x31,%ecx
  402d18:	e8 93 00 00 00       	call   402db0 <runtime::multi_pointer_slice_expr_error>
  402d1d:	48 8b 14 24          	mov    (%rsp),%rdx
  402d21:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402d26:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402d2b:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402d30:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402d35:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402d3a:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402d3f:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  402d46:	00 
  402d47:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402d4e:	00 
  402d4f:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  402d56:	00 
  402d57:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  402d5c:	48 8b 50 20          	mov    0x20(%rax),%rdx
  402d60:	48 01 f2             	add    %rsi,%rdx
  402d63:	48 89 50 20          	mov    %rdx,0x20(%rax)
  402d67:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402d6e:	00 
  402d6f:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402d76:	00 
  402d77:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402d7e:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402d85:	00 
  402d86:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402d8d:	00 
  402d8e:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402d95:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402d99:	48 89 11             	mov    %rdx,(%rcx)
  402d9c:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402da3:	c3                   	ret
  402da4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402dab:	00 00 00 00 00 

0000000000402db0 <runtime::multi_pointer_slice_expr_error>:
  402db0:	48 83 ec 58          	sub    $0x58,%rsp
  402db4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402db9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402dbe:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402dc2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402dc6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402dcb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402dd0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402dd5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  402dda:	8b 54 24 18          	mov    0x18(%rsp),%edx
  402dde:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402de2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402de7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402dec:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402df1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402df6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  402dfa:	89 54 24 40          	mov    %edx,0x40(%rsp)
  402dfe:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402e03:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402e08:	48 39 c8             	cmp    %rcx,%rax
  402e0b:	0f 9e c0             	setle  %al
  402e0e:	24 01                	and    $0x1,%al
  402e10:	3c 00                	cmp    $0x0,%al
  402e12:	74 05                	je     402e19 <runtime::multi_pointer_slice_expr_error+0x69>
  402e14:	48 83 c4 58          	add    $0x58,%rsp
  402e18:	c3                   	ret
  402e19:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  402e1e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402e23:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  402e27:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  402e2b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402e30:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402e35:	e8 f6 f9 ff ff       	call   402830 <runtime::multi_pointer_slice_handle_error>
  402e3a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402e40 <runtime::slice_expr_error_hi>:
  402e40:	48 83 ec 58          	sub    $0x58,%rsp
  402e44:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402e49:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402e4e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402e52:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402e56:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402e5b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402e60:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402e65:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402e6a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  402e6e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402e72:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402e77:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402e7c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402e81:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402e86:	89 74 24 44          	mov    %esi,0x44(%rsp)
  402e8a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  402e8e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  402e93:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402e98:	31 c0                	xor    %eax,%eax
  402e9a:	48 39 c8             	cmp    %rcx,%rax
  402e9d:	0f 9e c0             	setle  %al
  402ea0:	24 01                	and    $0x1,%al
  402ea2:	3c 00                	cmp    $0x0,%al
  402ea4:	74 1b                	je     402ec1 <runtime::slice_expr_error_hi+0x81>
  402ea6:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402eab:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  402eb0:	48 39 c8             	cmp    %rcx,%rax
  402eb3:	0f 9e c0             	setle  %al
  402eb6:	24 01                	and    $0x1,%al
  402eb8:	3c 00                	cmp    $0x0,%al
  402eba:	74 05                	je     402ec1 <runtime::slice_expr_error_hi+0x81>
  402ebc:	48 83 c4 58          	add    $0x58,%rsp
  402ec0:	c3                   	ret
  402ec1:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  402ec6:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  402eca:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  402ece:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402ed3:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402ed8:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  402edd:	48 89 e0             	mov    %rsp,%rax
  402ee0:	4c 89 00             	mov    %r8,(%rax)
  402ee3:	31 c0                	xor    %eax,%eax
  402ee5:	41 89 c0             	mov    %eax,%r8d
  402ee8:	e8 f3 f6 ff ff       	call   4025e0 <runtime::slice_handle_error>
  402eed:	0f 1f 00             	nopl   (%rax)

0000000000402ef0 <runtime::mem_copy_non_overlapping>:
  402ef0:	48 83 ec 38          	sub    $0x38,%rsp
  402ef4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402ef9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402efe:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402f03:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402f08:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402f0d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402f12:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402f17:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402f1c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402f21:	48 83 f8 00          	cmp    $0x0,%rax
  402f25:	0f 95 c0             	setne  %al
  402f28:	24 01                	and    $0x1,%al
  402f2a:	3c 00                	cmp    $0x0,%al
  402f2c:	74 3c                	je     402f6a <runtime::mem_copy_non_overlapping+0x7a>
  402f2e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402f33:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402f38:	48 39 c8             	cmp    %rcx,%rax
  402f3b:	0f 95 c0             	setne  %al
  402f3e:	24 01                	and    $0x1,%al
  402f40:	3c 00                	cmp    $0x0,%al
  402f42:	74 26                	je     402f6a <runtime::mem_copy_non_overlapping+0x7a>
  402f44:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402f49:	48 83 f8 00          	cmp    $0x0,%rax
  402f4d:	0f 9f c0             	setg   %al
  402f50:	24 01                	and    $0x1,%al
  402f52:	3c 00                	cmp    $0x0,%al
  402f54:	74 14                	je     402f6a <runtime::mem_copy_non_overlapping+0x7a>
  402f56:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402f5b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402f60:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402f65:	e8 f6 e0 ff ff       	call   401060 <memcpy@plt>
  402f6a:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402f6f:	48 83 c4 38          	add    $0x38,%rsp
  402f73:	c3                   	ret
  402f74:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402f7b:	00 00 00 00 00 

0000000000402f80 <runtime::slice_expr_error_lo_hi>:
  402f80:	48 83 ec 68          	sub    $0x68,%rsp
  402f84:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402f89:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402f8e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402f92:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402f96:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402f9b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402fa0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  402fa5:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402faa:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402faf:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  402fb4:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402fb9:	8b 74 24 18          	mov    0x18(%rsp),%esi
  402fbd:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  402fc1:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  402fc6:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  402fcb:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  402fd0:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  402fd5:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  402fd9:	89 74 24 50          	mov    %esi,0x50(%rsp)
  402fdd:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  402fe2:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  402fe7:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402fec:	31 c0                	xor    %eax,%eax
  402fee:	48 39 c8             	cmp    %rcx,%rax
  402ff1:	0f 9e c0             	setle  %al
  402ff4:	24 01                	and    $0x1,%al
  402ff6:	3c 00                	cmp    $0x0,%al
  402ff8:	74 47                	je     403041 <runtime::slice_expr_error_lo_hi+0xc1>
  402ffa:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402fff:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403004:	48 39 c8             	cmp    %rcx,%rax
  403007:	0f 9e c0             	setle  %al
  40300a:	24 01                	and    $0x1,%al
  40300c:	3c 00                	cmp    $0x0,%al
  40300e:	74 31                	je     403041 <runtime::slice_expr_error_lo_hi+0xc1>
  403010:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403015:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40301a:	48 39 c8             	cmp    %rcx,%rax
  40301d:	0f 9e c0             	setle  %al
  403020:	24 01                	and    $0x1,%al
  403022:	3c 00                	cmp    $0x0,%al
  403024:	74 1b                	je     403041 <runtime::slice_expr_error_lo_hi+0xc1>
  403026:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40302b:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403030:	48 39 c8             	cmp    %rcx,%rax
  403033:	0f 9e c0             	setle  %al
  403036:	24 01                	and    $0x1,%al
  403038:	3c 00                	cmp    $0x0,%al
  40303a:	74 05                	je     403041 <runtime::slice_expr_error_lo_hi+0xc1>
  40303c:	48 83 c4 68          	add    $0x68,%rsp
  403040:	c3                   	ret
  403041:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403046:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40304b:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40304f:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  403053:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403058:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40305d:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  403062:	48 89 e0             	mov    %rsp,%rax
  403065:	4c 89 10             	mov    %r10,(%rax)
  403068:	e8 73 f5 ff ff       	call   4025e0 <runtime::slice_handle_error>
  40306d:	0f 1f 00             	nopl   (%rax)

0000000000403070 <runtime::memset>:
  403070:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  403075:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  403079:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  40307e:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  403083:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  403088:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  40308c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  403091:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  403095:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40309a:	48 83 f8 00          	cmp    $0x0,%rax
  40309e:	0f 95 c0             	setne  %al
  4030a1:	24 01                	and    $0x1,%al
  4030a3:	3c 00                	cmp    $0x0,%al
  4030a5:	74 63                	je     40310a <runtime::memset+0x9a>
  4030a7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4030ac:	48 83 f8 00          	cmp    $0x0,%rax
  4030b0:	0f 95 c0             	setne  %al
  4030b3:	24 01                	and    $0x1,%al
  4030b5:	3c 00                	cmp    $0x0,%al
  4030b7:	74 51                	je     40310a <runtime::memset+0x9a>
  4030b9:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4030be:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  4030c2:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  4030c6:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4030cb:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  4030d2:	00 00 
  4030d4:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4030d9:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  4030de:	0f 9c c0             	setl   %al
  4030e1:	24 01                	and    $0x1,%al
  4030e3:	3c 00                	cmp    $0x0,%al
  4030e5:	74 21                	je     403108 <runtime::memset+0x98>
  4030e7:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4030ec:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  4030f1:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  4030f5:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  4030f8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4030fd:	48 83 c0 01          	add    $0x1,%rax
  403101:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  403106:	eb cc                	jmp    4030d4 <runtime::memset+0x64>
  403108:	eb 00                	jmp    40310a <runtime::memset+0x9a>
  40310a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40310f:	c3                   	ret

0000000000403110 <runtime::arena_alloc>:
  403110:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  403117:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40311c:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403121:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  403126:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40312b:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403130:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  403135:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40313a:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40313f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403144:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403149:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40314e:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  403155:	00 
  403156:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  40315d:	00 
  40315e:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  403165:	00 
  403166:	0f 57 c0             	xorps  %xmm0,%xmm0
  403169:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  403170:	00 
  403171:	c6 84 24 0f 01 00 00 	movb   $0x0,0x10f(%rsp)
  403178:	00 
  403179:	48 89 c2             	mov    %rax,%rdx
  40317c:	48 83 ea 01          	sub    $0x1,%rdx
  403180:	48 21 d0             	and    %rdx,%rax
  403183:	48 83 f8 00          	cmp    $0x0,%rax
  403187:	0f 94 c0             	sete   %al
  40318a:	24 01                	and    $0x1,%al
  40318c:	0f b6 f8             	movzbl %al,%edi
  40318f:	be 56 72 40 00       	mov    $0x407256,%esi
  403194:	ba 1a 00 00 00       	mov    $0x1a,%edx
  403199:	e8 a2 2a 00 00       	call   405c40 <runtime::assert>
  40319e:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4031a3:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  4031aa:	00 
  4031ab:	48 83 bc 24 00 01 00 	cmpq   $0x0,0x100(%rsp)
  4031b2:	00 00 
  4031b4:	0f 94 c0             	sete   %al
  4031b7:	24 01                	and    $0x1,%al
  4031b9:	3c 00                	cmp    $0x0,%al
  4031bb:	74 42                	je     4031ff <runtime::arena_alloc+0xef>
  4031bd:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4031c2:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  4031c9:	00 
  4031ca:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  4031d1:	00 
  4031d2:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  4031d9:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  4031e0:	00 
  4031e1:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  4031e8:	00 
  4031e9:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4031f0:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4031f4:	48 89 11             	mov    %rdx,(%rcx)
  4031f7:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  4031fe:	c3                   	ret
  4031ff:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403206:	00 
  403207:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  40320c:	0f 94 c0             	sete   %al
  40320f:	24 01                	and    $0x1,%al
  403211:	3c 00                	cmp    $0x0,%al
  403213:	74 09                	je     40321e <runtime::arena_alloc+0x10e>
  403215:	31 c0                	xor    %eax,%eax
  403217:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40321c:	eb 15                	jmp    403233 <runtime::arena_alloc+0x123>
  40321e:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403225:	00 
  403226:	48 8b 40 10          	mov    0x10(%rax),%rax
  40322a:	48 8b 40 20          	mov    0x20(%rax),%rax
  40322e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403233:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403238:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40323d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403242:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  403249:	00 
  40324a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403251:	00 
  403252:	48 8b 78 10          	mov    0x10(%rax),%rdi
  403256:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  40325d:	00 
  40325e:	0f 57 c0             	xorps  %xmm0,%xmm0
  403261:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  403268:	00 
  403269:	48 8d 8c 24 e0 00 00 	lea    0xe0(%rsp),%rcx
  403270:	00 
  403271:	e8 7a f8 ff ff       	call   402af0 <runtime::alloc_from_memory_block>
  403276:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  40327d:	00 
  40327e:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  403285:	00 
  403286:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  40328d:	00 
  40328e:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403295:	00 
  403296:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  40329d:	80 bc 24 0f 01 00 00 	cmpb   $0x1,0x10f(%rsp)
  4032a4:	01 
  4032a5:	0f 94 c0             	sete   %al
  4032a8:	24 01                	and    $0x1,%al
  4032aa:	3c 00                	cmp    $0x0,%al
  4032ac:	0f 84 19 02 00 00    	je     4034cb <runtime::arena_alloc+0x3bb>
  4032b2:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4032b9:	00 
  4032ba:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  4032bf:	0f 94 c0             	sete   %al
  4032c2:	24 01                	and    $0x1,%al
  4032c4:	3c 00                	cmp    $0x0,%al
  4032c6:	74 10                	je     4032d8 <runtime::arena_alloc+0x1c8>
  4032c8:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4032cf:	00 
  4032d0:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  4032d7:	00 
  4032d8:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4032dd:	48 8b bc 24 00 01 00 	mov    0x100(%rsp),%rdi
  4032e4:	00 
  4032e5:	e8 f6 31 00 00       	call   4064e0 <runtime::arena_alloc.align_forward_uint-0>
  4032ea:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4032f1:	00 
  4032f2:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4032f9:	00 
  4032fa:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403301:	00 
  403302:	48 8b 40 28          	mov    0x28(%rax),%rax
  403306:	48 39 c1             	cmp    %rax,%rcx
  403309:	48 0f 47 c1          	cmova  %rcx,%rax
  40330d:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  403314:	00 
  403315:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40331c:	00 
  40331d:	48 83 38 00          	cmpq   $0x0,(%rax)
  403321:	0f 94 c0             	sete   %al
  403324:	24 01                	and    $0x1,%al
  403326:	3c 00                	cmp    $0x0,%al
  403328:	74 46                	je     403370 <runtime::arena_alloc+0x260>
  40332a:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40332f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403336:	00 
  403337:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40333c:	e8 8f de ff ff       	call   4011d0 <runtime::heap_allocator>
  403341:	48 89 c1             	mov    %rax,%rcx
  403344:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403349:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  403350:	00 
  403351:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  403358:	00 
  403359:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403360:	00 
  403361:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  403368:	00 
  403369:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40336d:	48 89 08             	mov    %rcx,(%rax)
  403370:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  403375:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40337a:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  40337f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403386:	00 
  403387:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  40338e:	00 
  40338f:	48 8b 38             	mov    (%rax),%rdi
  403392:	48 8b 70 08          	mov    0x8(%rax),%rsi
  403396:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  40339d:	00 00 00 00 00 
  4033a2:	48 89 e0             	mov    %rsp,%rax
  4033a5:	4c 89 08             	mov    %r9,(%rax)
  4033a8:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  4033af:	00 
  4033b0:	e8 eb ec ff ff       	call   4020a0 <runtime::memory_block_alloc>
  4033b5:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4033b9:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4033c0:	00 
  4033c1:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4033c6:	3c 00                	cmp    $0x0,%al
  4033c8:	74 4d                	je     403417 <runtime::arena_alloc+0x307>
  4033ca:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4033cf:	8a 44 24 0f          	mov    0xf(%rsp),%al
  4033d3:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4033da:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  4033e1:	00 
  4033e2:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  4033e9:	00 
  4033ea:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  4033f1:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  4033f8:	00 
  4033f9:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403400:	00 
  403401:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403408:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40340c:	48 89 11             	mov    %rdx,(%rcx)
  40340f:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403416:	c3                   	ret
  403417:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40341c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403421:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403426:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40342d:	00 
  40342e:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403435:	00 
  403436:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  40343d:	00 
  40343e:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  403442:	48 89 08             	mov    %rcx,(%rax)
  403445:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40344c:	00 
  40344d:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403454:	00 
  403455:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403459:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403460:	00 
  403461:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403468:	00 
  403469:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  40346d:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403471:	48 01 f1             	add    %rsi,%rcx
  403474:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403478:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  40347f:	00 00 00 00 00 
  403484:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40348b:	00 
  40348c:	48 8b 78 10          	mov    0x10(%rax),%rdi
  403490:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  403497:	00 
  403498:	0f 57 c0             	xorps  %xmm0,%xmm0
  40349b:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4034a0:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  4034a5:	e8 46 f6 ff ff       	call   402af0 <runtime::alloc_from_memory_block>
  4034aa:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4034af:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  4034b4:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  4034bb:	00 
  4034bc:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  4034c3:	00 
  4034c4:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4034cb:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4034d0:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4034d7:	00 
  4034d8:	48 8b 70 10          	mov    0x10(%rax),%rsi
  4034dc:	48 8b 50 18          	mov    0x18(%rax),%rdx
  4034e0:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  4034e4:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  4034eb:	00 
  4034ec:	48 29 fe             	sub    %rdi,%rsi
  4034ef:	48 01 f2             	add    %rsi,%rdx
  4034f2:	48 89 50 18          	mov    %rdx,0x18(%rax)
  4034f6:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  4034fd:	00 
  4034fe:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403505:	00 
  403506:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  40350d:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403514:	00 
  403515:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  40351c:	00 
  40351d:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403524:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403528:	48 89 11             	mov    %rdx,(%rcx)
  40352b:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403532:	c3                   	ret
  403533:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40353a:	84 00 00 00 00 00 

0000000000403540 <runtime::print_string>:
  403540:	48 83 ec 58          	sub    $0x58,%rsp
  403544:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403549:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40354e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403553:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403558:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40355d:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  403562:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  403569:	00 00 
  40356b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403570:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403575:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40357a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40357f:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403586:	00 00 
  403588:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  40358d:	e8 0e e6 ff ff       	call   401ba0 <runtime::stderr_write>
  403592:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403597:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40359c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4035a1:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4035a6:	48 83 c4 58          	add    $0x58,%rsp
  4035aa:	c3                   	ret
  4035ab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004035b0 <runtime::mem_alloc>:
  4035b0:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  4035b7:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  4035bc:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  4035c1:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  4035c6:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4035cb:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4035d0:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4035d5:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4035dc:	00 
  4035dd:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4035e2:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4035e7:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4035ec:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4035f1:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4035f6:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4035fd:	00 
  4035fe:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  403605:	00 
  403606:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40360d:	00 
  40360e:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403615:	00 
  403616:	e8 65 e9 ff ff       	call   401f80 <runtime::is_power_of_two_int>
  40361b:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403620:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403625:	0f b6 f8             	movzbl %al,%edi
  403628:	be 71 72 40 00       	mov    $0x407271,%esi
  40362d:	ba 20 00 00 00       	mov    $0x20,%edx
  403632:	e8 09 26 00 00       	call   405c40 <runtime::assert>
  403637:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40363c:	48 83 f8 00          	cmp    $0x0,%rax
  403640:	0f 94 c0             	sete   %al
  403643:	24 01                	and    $0x1,%al
  403645:	3c 00                	cmp    $0x0,%al
  403647:	75 12                	jne    40365b <runtime::mem_alloc+0xab>
  403649:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  403650:	00 00 
  403652:	0f 94 c0             	sete   %al
  403655:	24 01                	and    $0x1,%al
  403657:	3c 00                	cmp    $0x0,%al
  403659:	74 1e                	je     403679 <runtime::mem_alloc+0xc9>
  40365b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403660:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  403667:	00 
  403668:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40366f:	31 c0                	xor    %eax,%eax
  403671:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403678:	c3                   	ret
  403679:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40367e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403683:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403688:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  40368d:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403694:	00 
  403695:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  40369c:	00 
  40369d:	0f 57 c0             	xorps  %xmm0,%xmm0
  4036a0:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4036a5:	48 89 e6             	mov    %rsp,%rsi
  4036a8:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  4036ac:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  4036b1:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  4036b5:	4c 89 06             	mov    %r8,(%rsi)
  4036b8:	31 f6                	xor    %esi,%esi
  4036ba:	41 89 f1             	mov    %esi,%r9d
  4036bd:	4d 89 c8             	mov    %r9,%r8
  4036c0:	ff d0                	call   *%rax
  4036c2:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4036c7:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4036cc:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4036d1:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4036d5:	48 89 11             	mov    %rdx,(%rcx)
  4036d8:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  4036df:	c3                   	ret

00000000004036e0 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  4036e0:	48 83 ec 48          	sub    $0x48,%rsp
  4036e4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4036e8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4036ed:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4036f2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4036f7:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4036fc:	48 8b 04 24          	mov    (%rsp),%rax
  403700:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403705:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40370a:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40370f:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403714:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403719:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40371e:	48 39 c1             	cmp    %rax,%rcx
  403721:	48 0f 4c c1          	cmovl  %rcx,%rax
  403725:	31 c9                	xor    %ecx,%ecx
  403727:	48 39 c1             	cmp    %rax,%rcx
  40372a:	48 0f 4f c1          	cmovg  %rcx,%rax
  40372e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403733:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  403739:	0f 9f c0             	setg   %al
  40373c:	24 01                	and    $0x1,%al
  40373e:	3c 00                	cmp    $0x0,%al
  403740:	74 18                	je     40375a <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)+0x7a>
  403742:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403747:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40374c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  403751:	48 c1 e2 00          	shl    $0x0,%rdx
  403755:	e8 36 d9 ff ff       	call   401090 <memmove@plt>
  40375a:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40375f:	48 83 c4 48          	add    $0x48,%rsp
  403763:	c3                   	ret
  403764:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40376b:	00 00 00 00 00 

0000000000403770 <runtime::print_byte>:
  403770:	48 83 ec 68          	sub    $0x68,%rsp
  403774:	40 88 f8             	mov    %dil,%al
  403777:	88 44 24 07          	mov    %al,0x7(%rsp)
  40377b:	8a 54 24 07          	mov    0x7(%rsp),%dl
  40377f:	88 54 24 67          	mov    %dl,0x67(%rsp)
  403783:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  40378a:	00 00 
  40378c:	0f 57 c0             	xorps  %xmm0,%xmm0
  40378f:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403794:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  403799:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  40379e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4037a3:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  4037aa:	00 00 
  4037ac:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4037b1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4037b6:	88 11                	mov    %dl,(%rcx)
  4037b8:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4037bd:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4037c2:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4037c7:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4037cc:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  4037d3:	00 00 
  4037d5:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  4037da:	e8 c1 e3 ff ff       	call   401ba0 <runtime::stderr_write>
  4037df:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4037e4:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4037e9:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4037ee:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4037f3:	48 83 c4 68          	add    $0x68,%rsp
  4037f7:	c3                   	ret
  4037f8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4037ff:	00 

0000000000403800 <runtime::matrix_bounds_check_error>:
  403800:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403807:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40380c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403811:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  403815:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  403819:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  40381e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403823:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  40382a:	00 
  40382b:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403830:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403837:	00 
  403838:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40383d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403842:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403847:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40384c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403851:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  403855:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  40385a:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  40385f:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  403864:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  403869:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  403870:	00 
  403871:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  403876:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  40387a:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40387f:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  403884:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  403889:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40388e:	48 39 c8             	cmp    %rcx,%rax
  403891:	0f 92 c0             	setb   %al
  403894:	24 01                	and    $0x1,%al
  403896:	3c 00                	cmp    $0x0,%al
  403898:	74 1e                	je     4038b8 <runtime::matrix_bounds_check_error+0xb8>
  40389a:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40389f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4038a4:	48 39 c8             	cmp    %rcx,%rax
  4038a7:	0f 92 c0             	setb   %al
  4038aa:	24 01                	and    $0x1,%al
  4038ac:	3c 00                	cmp    $0x0,%al
  4038ae:	74 08                	je     4038b8 <runtime::matrix_bounds_check_error+0xb8>
  4038b0:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4038b7:	c3                   	ret
  4038b8:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  4038bd:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  4038c2:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  4038c6:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  4038ca:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4038cf:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4038d4:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  4038d9:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  4038de:	48 89 e0             	mov    %rsp,%rax
  4038e1:	4c 89 58 08          	mov    %r11,0x8(%rax)
  4038e5:	4c 89 10             	mov    %r10,(%rax)
  4038e8:	e8 53 2c 00 00       	call   406540 <runtime::matrix_bounds_check_error.handle_error-0>
  4038ed:	0f 1f 00             	nopl   (%rax)

00000000004038f0 <runtime::heap_alloc>:
  4038f0:	48 83 ec 18          	sub    $0x18,%rsp
  4038f4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4038f8:	40 88 f0             	mov    %sil,%al
  4038fb:	88 44 24 0e          	mov    %al,0xe(%rsp)
  4038ff:	8a 44 24 0e          	mov    0xe(%rsp),%al
  403903:	48 8b 3c 24          	mov    (%rsp),%rdi
  403907:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40390c:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403910:	0f b6 f0             	movzbl %al,%esi
  403913:	e8 a8 e6 ff ff       	call   401fc0 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  403918:	48 83 c4 18          	add    $0x18,%rsp
  40391c:	c3                   	ret
  40391d:	0f 1f 00             	nopl   (%rax)

0000000000403920 <runtime::heap_resize>:
  403920:	48 83 ec 28          	sub    $0x28,%rsp
  403924:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403929:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40392e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403933:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403938:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40393d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403942:	e8 29 e7 ff ff       	call   402070 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  403947:	48 83 c4 28          	add    $0x28,%rsp
  40394b:	c3                   	ret
  40394c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403950 <runtime::heap_free>:
  403950:	48 83 ec 18          	sub    $0x18,%rsp
  403954:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403959:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40395e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403963:	e8 38 ea ff ff       	call   4023a0 <runtime::[heap_allocator_unix.odin]::_heap_free>
  403968:	48 83 c4 18          	add    $0x18,%rsp
  40396c:	c3                   	ret
  40396d:	0f 1f 00             	nopl   (%rax)

0000000000403970 <runtime::mem_free>:
  403970:	53                   	push   %rbx
  403971:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  403978:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40397d:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  403982:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403987:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40398c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403991:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403996:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40399b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4039a0:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  4039a7:	00 
  4039a8:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4039ad:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4039b2:	48 83 f8 00          	cmp    $0x0,%rax
  4039b6:	0f 94 c0             	sete   %al
  4039b9:	24 01                	and    $0x1,%al
  4039bb:	3c 00                	cmp    $0x0,%al
  4039bd:	75 0f                	jne    4039ce <runtime::mem_free+0x5e>
  4039bf:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  4039c5:	0f 94 c0             	sete   %al
  4039c8:	24 01                	and    $0x1,%al
  4039ca:	3c 00                	cmp    $0x0,%al
  4039cc:	74 0b                	je     4039d9 <runtime::mem_free+0x69>
  4039ce:	31 c0                	xor    %eax,%eax
  4039d0:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  4039d7:	5b                   	pop    %rbx
  4039d8:	c3                   	ret
  4039d9:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  4039de:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  4039e3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4039e8:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4039ed:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  4039f2:	0f 57 c0             	xorps  %xmm0,%xmm0
  4039f5:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  4039fa:	be 01 00 00 00       	mov    $0x1,%esi
  4039ff:	31 c9                	xor    %ecx,%ecx
  403a01:	41 89 c9             	mov    %ecx,%r9d
  403a04:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  403a09:	4c 89 ca             	mov    %r9,%rdx
  403a0c:	4c 89 c9             	mov    %r9,%rcx
  403a0f:	48 89 1c 24          	mov    %rbx,(%rsp)
  403a13:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  403a18:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  403a1d:	ff d0                	call   *%rax
  403a1f:	88 44 24 47          	mov    %al,0x47(%rsp)
  403a23:	8a 44 24 47          	mov    0x47(%rsp),%al
  403a27:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  403a2e:	5b                   	pop    %rbx
  403a2f:	c3                   	ret

0000000000403a30 <runtime::print_u64>:
  403a30:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403a37:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403a3c:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403a41:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403a48:	00 
  403a49:	48 8d 7c 24 5f       	lea    0x5f(%rsp),%rdi
  403a4e:	31 f6                	xor    %esi,%esi
  403a50:	ba 81 00 00 00       	mov    $0x81,%edx
  403a55:	e8 e6 d5 ff ff       	call   401040 <memset@plt>
  403a5a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403a5f:	48 c7 44 24 50 81 00 	movq   $0x81,0x50(%rsp)
  403a66:	00 00 
  403a68:	48 c7 44 24 48 0a 00 	movq   $0xa,0x48(%rsp)
  403a6f:	00 00 
  403a71:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403a76:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403a7b:	48 3b 44 24 48       	cmp    0x48(%rsp),%rax
  403a80:	0f 93 c0             	setae  %al
  403a83:	24 01                	and    $0x1,%al
  403a85:	3c 00                	cmp    $0x0,%al
  403a87:	74 50                	je     403ad9 <runtime::print_u64+0xa9>
  403a89:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403a8e:	48 83 e8 01          	sub    $0x1,%rax
  403a92:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403a97:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403a9c:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403aa1:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403aa8:	48 8b 08             	mov    (%rax),%rcx
  403aab:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403ab0:	31 d2                	xor    %edx,%edx
  403ab2:	48 f7 74 24 48       	divq   0x48(%rsp)
  403ab7:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403abc:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403abf:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  403ac3:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403ac8:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403acd:	31 d2                	xor    %edx,%edx
  403acf:	48 f7 f1             	div    %rcx
  403ad2:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403ad7:	eb 9d                	jmp    403a76 <runtime::print_u64+0x46>
  403ad9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403ade:	48 ff c8             	dec    %rax
  403ae1:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403ae6:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403aeb:	48 89 04 24          	mov    %rax,(%rsp)
  403aef:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403af6:	48 8b 08             	mov    (%rax),%rcx
  403af9:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403afe:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403b03:	31 d2                	xor    %edx,%edx
  403b05:	48 f7 f6             	div    %rsi
  403b08:	48 8b 04 24          	mov    (%rsp),%rax
  403b0c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403b0f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  403b13:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  403b18:	48 8d 4c 14 5f       	lea    0x5f(%rsp,%rdx,1),%rcx
  403b1d:	b8 81 00 00 00       	mov    $0x81,%eax
  403b22:	48 29 d0             	sub    %rdx,%rax
  403b25:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403b2a:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403b2f:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403b34:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403b39:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403b40:	00 00 
  403b42:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403b47:	e8 54 e0 ff ff       	call   401ba0 <runtime::stderr_write>
  403b4c:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  403b53:	c3                   	ret
  403b54:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403b5b:	00 00 00 00 00 

0000000000403b60 <runtime::print_i64>:
  403b60:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403b67:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403b6c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403b71:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403b78:	00 
  403b79:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403b80:	00 
  403b81:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  403b88:	00 00 
  403b8a:	0f 9c c0             	setl   %al
  403b8d:	24 01                	and    $0x1,%al
  403b8f:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  403b96:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403b9d:	00 
  403b9e:	31 c9                	xor    %ecx,%ecx
  403ba0:	48 29 c1             	sub    %rax,%rcx
  403ba3:	48 83 f8 00          	cmp    $0x0,%rax
  403ba7:	48 0f 4c c1          	cmovl  %rcx,%rax
  403bab:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403bb2:	00 
  403bb3:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  403bb8:	31 f6                	xor    %esi,%esi
  403bba:	ba 81 00 00 00       	mov    $0x81,%edx
  403bbf:	e8 7c d4 ff ff       	call   401040 <memset@plt>
  403bc4:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  403bcb:	00 00 
  403bcd:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  403bd4:	00 0a 
  403bd6:	0f 9d c0             	setge  %al
  403bd9:	24 01                	and    $0x1,%al
  403bdb:	3c 00                	cmp    $0x0,%al
  403bdd:	74 5c                	je     403c3b <runtime::print_i64+0xdb>
  403bdf:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403be4:	48 83 e8 01          	sub    $0x1,%rax
  403be8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403bed:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403bf2:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  403bf7:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403bfe:	48 8b 08             	mov    (%rax),%rcx
  403c01:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403c08:	00 
  403c09:	be 0a 00 00 00       	mov    $0xa,%esi
  403c0e:	48 99                	cqto
  403c10:	48 f7 fe             	idiv   %rsi
  403c13:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403c18:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403c1b:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  403c1f:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403c26:	00 
  403c27:	b9 0a 00 00 00       	mov    $0xa,%ecx
  403c2c:	48 99                	cqto
  403c2e:	48 f7 f9             	idiv   %rcx
  403c31:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403c38:	00 
  403c39:	eb 92                	jmp    403bcd <runtime::print_i64+0x6d>
  403c3b:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403c40:	48 83 e8 01          	sub    $0x1,%rax
  403c44:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403c49:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403c4e:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403c53:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403c5a:	48 8b 08             	mov    (%rax),%rcx
  403c5d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403c64:	00 
  403c65:	be 0a 00 00 00       	mov    $0xa,%esi
  403c6a:	48 99                	cqto
  403c6c:	48 f7 fe             	idiv   %rsi
  403c6f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403c74:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403c77:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  403c7b:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  403c82:	00 
  403c83:	74 18                	je     403c9d <runtime::print_i64+0x13d>
  403c85:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403c8a:	48 83 e8 01          	sub    $0x1,%rax
  403c8e:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403c93:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403c98:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  403c9d:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  403ca2:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  403ca7:	b8 81 00 00 00       	mov    $0x81,%eax
  403cac:	48 29 d0             	sub    %rdx,%rax
  403caf:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403cb4:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403cb9:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403cbe:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  403cc3:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  403cca:	00 00 
  403ccc:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  403cd1:	e8 ca de ff ff       	call   401ba0 <runtime::stderr_write>
  403cd6:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  403cdd:	c3                   	ret
  403cde:	66 90                	xchg   %ax,%ax

0000000000403ce0 <runtime::arena_free_last_memory_block>:
  403ce0:	48 83 ec 28          	sub    $0x28,%rsp
  403ce4:	48 89 3c 24          	mov    %rdi,(%rsp)
  403ce8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403ced:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  403cf2:	48 8b 04 24          	mov    (%rsp),%rax
  403cf6:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403cfb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403d00:	48 8b 40 10          	mov    0x10(%rax),%rax
  403d04:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403d09:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  403d0f:	0f 95 c0             	setne  %al
  403d12:	24 01                	and    $0x1,%al
  403d14:	3c 00                	cmp    $0x0,%al
  403d16:	74 3e                	je     403d56 <runtime::arena_free_last_memory_block+0x76>
  403d18:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  403d1d:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403d22:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403d27:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403d2c:	48 8b 09             	mov    (%rcx),%rcx
  403d2f:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403d33:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403d38:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403d3d:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  403d41:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403d45:	48 29 f9             	sub    %rdi,%rcx
  403d48:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403d4c:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403d51:	e8 ca eb ff ff       	call   402920 <runtime::memory_block_dealloc>
  403d56:	48 83 c4 28          	add    $0x28,%rsp
  403d5a:	c3                   	ret
  403d5b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403d60 <runtime::print_caller_location>:
  403d60:	50                   	push   %rax
  403d61:	48 89 3c 24          	mov    %rdi,(%rsp)
  403d65:	eb 00                	jmp    403d67 <runtime::print_caller_location+0x7>
  403d67:	48 8b 04 24          	mov    (%rsp),%rax
  403d6b:	48 8b 38             	mov    (%rax),%rdi
  403d6e:	48 8b 70 08          	mov    0x8(%rax),%rsi
  403d72:	e8 c9 f7 ff ff       	call   403540 <runtime::print_string>
  403d77:	bf 28 00 00 00       	mov    $0x28,%edi
  403d7c:	e8 ef f9 ff ff       	call   403770 <runtime::print_byte>
  403d81:	48 8b 04 24          	mov    (%rsp),%rax
  403d85:	48 63 78 10          	movslq 0x10(%rax),%rdi
  403d89:	e8 a2 fc ff ff       	call   403a30 <runtime::print_u64>
  403d8e:	48 8b 04 24          	mov    (%rsp),%rax
  403d92:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  403d96:	0f 95 c0             	setne  %al
  403d99:	24 01                	and    $0x1,%al
  403d9b:	3c 00                	cmp    $0x0,%al
  403d9d:	74 17                	je     403db6 <runtime::print_caller_location+0x56>
  403d9f:	bf 3a 00 00 00       	mov    $0x3a,%edi
  403da4:	e8 c7 f9 ff ff       	call   403770 <runtime::print_byte>
  403da9:	48 8b 04 24          	mov    (%rsp),%rax
  403dad:	48 63 78 14          	movslq 0x14(%rax),%rdi
  403db1:	e8 7a fc ff ff       	call   403a30 <runtime::print_u64>
  403db6:	bf 29 00 00 00       	mov    $0x29,%edi
  403dbb:	e8 b0 f9 ff ff       	call   403770 <runtime::print_byte>
  403dc0:	58                   	pop    %rax
  403dc1:	c3                   	ret
  403dc2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403dc9:	1f 84 00 00 00 00 00 

0000000000403dd0 <runtime::arena_free_all>:
  403dd0:	48 83 ec 28          	sub    $0x28,%rsp
  403dd4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403dd9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403dde:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403de3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403de8:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403ded:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403df2:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403df7:	0f 95 c0             	setne  %al
  403dfa:	24 01                	and    $0x1,%al
  403dfc:	3c 00                	cmp    $0x0,%al
  403dfe:	74 2c                	je     403e2c <runtime::arena_free_all+0x5c>
  403e00:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e05:	48 8b 40 10          	mov    0x10(%rax),%rax
  403e09:	48 83 38 00          	cmpq   $0x0,(%rax)
  403e0d:	0f 95 c0             	setne  %al
  403e10:	24 01                	and    $0x1,%al
  403e12:	3c 00                	cmp    $0x0,%al
  403e14:	74 16                	je     403e2c <runtime::arena_free_all+0x5c>
  403e16:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  403e1b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403e20:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403e25:	e8 b6 fe ff ff       	call   403ce0 <runtime::arena_free_last_memory_block>
  403e2a:	eb c1                	jmp    403ded <runtime::arena_free_all+0x1d>
  403e2c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e31:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403e36:	0f 95 c0             	setne  %al
  403e39:	24 01                	and    $0x1,%al
  403e3b:	3c 00                	cmp    $0x0,%al
  403e3d:	74 32                	je     403e71 <runtime::arena_free_all+0xa1>
  403e3f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e44:	48 8b 40 10          	mov    0x10(%rax),%rax
  403e48:	48 8b 78 18          	mov    0x18(%rax),%rdi
  403e4c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e51:	48 8b 40 10          	mov    0x10(%rax),%rax
  403e55:	48 8b 50 20          	mov    0x20(%rax),%rdx
  403e59:	31 f6                	xor    %esi,%esi
  403e5b:	e8 e0 d1 ff ff       	call   401040 <memset@plt>
  403e60:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e65:	48 8b 40 10          	mov    0x10(%rax),%rax
  403e69:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  403e70:	00 
  403e71:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e76:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  403e7d:	00 
  403e7e:	48 83 c4 28          	add    $0x28,%rsp
  403e82:	c3                   	ret
  403e83:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403e8a:	84 00 00 00 00 00 

0000000000403e90 <runtime::arena_destroy>:
  403e90:	48 83 ec 28          	sub    $0x28,%rsp
  403e94:	48 89 3c 24          	mov    %rdi,(%rsp)
  403e98:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403e9d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  403ea2:	48 8b 04 24          	mov    (%rsp),%rax
  403ea6:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403eab:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403eb0:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403eb5:	0f 95 c0             	setne  %al
  403eb8:	24 01                	and    $0x1,%al
  403eba:	3c 00                	cmp    $0x0,%al
  403ebc:	74 4e                	je     403f0c <runtime::arena_destroy+0x7c>
  403ebe:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  403ec3:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403ec8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403ecd:	48 8b 40 10          	mov    0x10(%rax),%rax
  403ed1:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403ed6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403edb:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403ee0:	48 8b 09             	mov    (%rcx),%rcx
  403ee3:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403ee7:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403eec:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403ef1:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  403ef5:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403ef9:	48 29 f9             	sub    %rdi,%rcx
  403efc:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403f00:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403f05:	e8 16 ea ff ff       	call   402920 <runtime::memory_block_dealloc>
  403f0a:	eb 9f                	jmp    403eab <runtime::arena_destroy+0x1b>
  403f0c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f11:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  403f18:	00 
  403f19:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f1e:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  403f25:	00 
  403f26:	48 83 c4 28          	add    $0x28,%rsp
  403f2a:	c3                   	ret
  403f2b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403f30 <runtime::arena_allocator_proc>:
  403f30:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  403f37:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  403f3c:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  403f43:	00 
  403f44:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  403f4b:	00 
  403f4c:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  403f53:	00 
  403f54:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  403f5b:	00 
  403f5c:	40 88 f0             	mov    %sil,%al
  403f5f:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  403f66:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  403f6d:	00 
  403f6e:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  403f75:	00 
  403f76:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  403f7d:	00 
  403f7e:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  403f85:	00 
  403f86:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  403f8d:	00 
  403f8e:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  403f95:	00 
  403f96:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  403f9d:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  403fa2:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  403fa9:	00 
  403faa:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  403fb1:	00 
  403fb2:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  403fb9:	00 
  403fba:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  403fc1:	00 
  403fc2:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  403fc9:	00 
  403fca:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  403fd1:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  403fd8:	00 
  403fd9:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  403fe0:	00 
  403fe1:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  403fe8:	00 
  403fe9:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  403ff0:	00 
  403ff1:	0f 57 c0             	xorps  %xmm0,%xmm0
  403ff4:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  403ffb:	00 
  403ffc:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404003:	00 
  404004:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  40400b:	00 
  40400c:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  404013:	00 
  404014:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  40401b:	00 
  40401c:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  404023:	00 
  404024:	0f b6 c8             	movzbl %al,%ecx
  404027:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40402c:	2c 07                	sub    $0x7,%al
  40402e:	0f 87 9a 07 00 00    	ja     4047ce <runtime::arena_allocator_proc+0x89e>
  404034:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  404039:	48 8b 04 c5 50 70 40 	mov    0x407050(,%rax,8),%rax
  404040:	00 
  404041:	ff e0                	jmp    *%rax
  404043:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  40404a:	00 
  40404b:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404052:	00 
  404053:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  40405a:	00 
  40405b:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404062:	00 
  404063:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  40406a:	00 
  40406b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40406e:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  404075:	00 
  404076:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  40407d:	00 
  40407e:	e8 8d f0 ff ff       	call   403110 <runtime::arena_alloc>
  404083:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40408a:	00 
  40408b:	40 88 c7             	mov    %al,%dil
  40408e:	40 88 f8             	mov    %dil,%al
  404091:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  404098:	00 
  404099:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4040a0:	00 
  4040a1:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4040a8:	00 
  4040a9:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4040b0:	00 
  4040b1:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  4040b8:	00 
  4040b9:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4040bd:	48 89 11             	mov    %rdx,(%rcx)
  4040c0:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4040c7:	c3                   	ret
  4040c8:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  4040cf:	04 
  4040d0:	e9 f9 06 00 00       	jmp    4047ce <runtime::arena_allocator_proc+0x89e>
  4040d5:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  4040dc:	00 
  4040dd:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  4040e4:	00 
  4040e5:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4040ec:	00 
  4040ed:	e8 de fc ff ff       	call   403dd0 <runtime::arena_free_all>
  4040f2:	e9 d7 06 00 00       	jmp    4047ce <runtime::arena_allocator_proc+0x89e>
  4040f7:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4040fe:	00 
  4040ff:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  404106:	00 
  404107:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  40410e:	00 00 
  404110:	0f 94 c1             	sete   %cl
  404113:	80 e1 01             	and    $0x1,%cl
  404116:	b0 01                	mov    $0x1,%al
  404118:	38 c8                	cmp    %cl,%al
  40411a:	74 25                	je     404141 <runtime::arena_allocator_proc+0x211>
  40411c:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  404123:	00 
  404124:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  40412b:	00 
  40412c:	0f 94 c1             	sete   %cl
  40412f:	80 e1 01             	and    $0x1,%cl
  404132:	b0 01                	mov    $0x1,%al
  404134:	38 c8                	cmp    %cl,%al
  404136:	0f 84 a8 00 00 00    	je     4041e4 <runtime::arena_allocator_proc+0x2b4>
  40413c:	e9 85 00 00 00       	jmp    4041c6 <runtime::arena_allocator_proc+0x296>
  404141:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404148:	00 
  404149:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404150:	00 
  404151:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404158:	00 
  404159:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404160:	00 
  404161:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404168:	00 
  404169:	0f 57 c0             	xorps  %xmm0,%xmm0
  40416c:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  404173:	00 
  404174:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  40417b:	00 
  40417c:	e8 8f ef ff ff       	call   403110 <runtime::arena_alloc>
  404181:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404188:	00 
  404189:	40 88 c7             	mov    %al,%dil
  40418c:	40 88 f8             	mov    %dil,%al
  40418f:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  404196:	00 
  404197:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  40419e:	00 
  40419f:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4041a6:	00 
  4041a7:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4041ae:	00 
  4041af:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  4041b6:	00 
  4041b7:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4041bb:	48 89 11             	mov    %rdx,(%rcx)
  4041be:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4041c5:	c3                   	ret
  4041c6:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  4041cd:	00 00 
  4041cf:	0f 94 c1             	sete   %cl
  4041d2:	80 e1 01             	and    $0x1,%cl
  4041d5:	b0 01                	mov    $0x1,%al
  4041d7:	38 c8                	cmp    %cl,%al
  4041d9:	0f 84 e5 00 00 00    	je     4042c4 <runtime::arena_allocator_proc+0x394>
  4041df:	e9 b7 00 00 00       	jmp    40429b <runtime::arena_allocator_proc+0x36b>
  4041e4:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4041eb:	00 
  4041ec:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4041f1:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  4041f8:	00 
  4041f9:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  4041fe:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404203:	31 c0                	xor    %eax,%eax
  404205:	41 89 c0             	mov    %eax,%r8d
  404208:	be 3e 00 00 00       	mov    $0x3e,%esi
  40420d:	ba d1 00 00 00       	mov    $0xd1,%edx
  404212:	b9 13 00 00 00       	mov    $0x13,%ecx
  404217:	e8 94 eb ff ff       	call   402db0 <runtime::multi_pointer_slice_expr_error>
  40421c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  404221:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  404226:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40422d:	00 
  40422e:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  404235:	00 
  404236:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  40423d:	00 
  40423e:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  404245:	00 
  404246:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  40424d:	00 
  40424e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404255:	00 
  404256:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  40425d:	00 
  40425e:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404265:	00 
  404266:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40426d:	00 
  40426e:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404275:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40427c:	00 
  40427d:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404284:	00 
  404285:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40428c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404290:	48 89 11             	mov    %rdx,(%rcx)
  404293:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40429a:	c3                   	ret
  40429b:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4042a2:	00 
  4042a3:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  4042aa:	00 
  4042ab:	48 83 e9 01          	sub    $0x1,%rcx
  4042af:	48 21 c8             	and    %rcx,%rax
  4042b2:	48 83 f8 00          	cmp    $0x0,%rax
  4042b6:	0f 94 c1             	sete   %cl
  4042b9:	80 e1 01             	and    $0x1,%cl
  4042bc:	b0 01                	mov    $0x1,%al
  4042be:	38 c8                	cmp    %cl,%al
  4042c0:	74 54                	je     404316 <runtime::arena_allocator_proc+0x3e6>
  4042c2:	eb 4d                	jmp    404311 <runtime::arena_allocator_proc+0x3e1>
  4042c4:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4042cb:	00 
  4042cc:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  4042d3:	04 
  4042d4:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4042db:	00 
  4042dc:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4042e3:	00 
  4042e4:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4042eb:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4042f2:	00 
  4042f3:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4042fa:	00 
  4042fb:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404302:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404306:	48 89 11             	mov    %rdx,(%rcx)
  404309:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404310:	c3                   	ret
  404311:	e9 94 02 00 00       	jmp    4045aa <runtime::arena_allocator_proc+0x67a>
  404316:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  40431d:	00 
  40431e:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  404325:	00 
  404326:	0f 92 c0             	setb   %al
  404329:	24 01                	and    $0x1,%al
  40432b:	3c 00                	cmp    $0x0,%al
  40432d:	0f 84 b7 00 00 00    	je     4043ea <runtime::arena_allocator_proc+0x4ba>
  404333:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40433a:	00 
  40433b:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404340:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404347:	00 
  404348:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  40434d:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404352:	31 c0                	xor    %eax,%eax
  404354:	41 89 c0             	mov    %eax,%r8d
  404357:	be 3e 00 00 00       	mov    $0x3e,%esi
  40435c:	ba d9 00 00 00       	mov    $0xd9,%edx
  404361:	b9 14 00 00 00       	mov    $0x14,%ecx
  404366:	e8 45 ea ff ff       	call   402db0 <runtime::multi_pointer_slice_expr_error>
  40436b:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  404370:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  404375:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40437c:	00 
  40437d:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  404384:	00 
  404385:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  40438c:	00 
  40438d:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  404394:	00 
  404395:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  40439c:	00 
  40439d:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4043a4:	00 
  4043a5:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4043ac:	00 
  4043ad:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4043b4:	00 
  4043b5:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4043bc:	00 
  4043bd:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4043c4:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4043cb:	00 
  4043cc:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4043d3:	00 
  4043d4:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4043db:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4043df:	48 89 11             	mov    %rdx,(%rcx)
  4043e2:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4043e9:	c3                   	ret
  4043ea:	eb 00                	jmp    4043ec <runtime::arena_allocator_proc+0x4bc>
  4043ec:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  4043f3:	00 
  4043f4:	48 8b 40 10          	mov    0x10(%rax),%rax
  4043f8:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  4043ff:	00 
  404400:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  404407:	00 00 
  404409:	0f 95 c0             	setne  %al
  40440c:	24 01                	and    $0x1,%al
  40440e:	3c 00                	cmp    $0x0,%al
  404410:	0f 84 92 01 00 00    	je     4045a8 <runtime::arena_allocator_proc+0x678>
  404416:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40441d:	00 
  40441e:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404425:	00 
  404426:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  40442a:	48 29 c8             	sub    %rcx,%rax
  40442d:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  404434:	00 
  404435:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40443c:	00 
  40443d:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  404444:	00 
  404445:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  40444c:	00 
  40444d:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404454:	00 
  404455:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  40445c:	00 
  40445d:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  404464:	00 
  404465:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40446c:	00 
  40446d:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  404474:	00 
  404475:	0f 92 c0             	setb   %al
  404478:	24 01                	and    $0x1,%al
  40447a:	3c 00                	cmp    $0x0,%al
  40447c:	0f 84 24 01 00 00    	je     4045a6 <runtime::arena_allocator_proc+0x676>
  404482:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  404489:	00 
  40448a:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404491:	00 
  404492:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  404496:	0f 94 c0             	sete   %al
  404499:	24 01                	and    $0x1,%al
  40449b:	3c 00                	cmp    $0x0,%al
  40449d:	0f 84 03 01 00 00    	je     4045a6 <runtime::arena_allocator_proc+0x676>
  4044a3:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  4044aa:	00 
  4044ab:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  4044b2:	00 
  4044b3:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  4044b7:	0f 96 c0             	setbe  %al
  4044ba:	24 01                	and    $0x1,%al
  4044bc:	3c 00                	cmp    $0x0,%al
  4044be:	0f 84 e2 00 00 00    	je     4045a6 <runtime::arena_allocator_proc+0x676>
  4044c4:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4044cb:	00 
  4044cc:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  4044d3:	00 
  4044d4:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4044d8:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4044df:	00 
  4044e0:	48 8b 40 18          	mov    0x18(%rax),%rax
  4044e4:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4044e9:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  4044f0:	00 
  4044f1:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4044f6:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  4044fd:	00 
  4044fe:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  404503:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404508:	be 3e 00 00 00       	mov    $0x3e,%esi
  40450d:	ba e4 00 00 00       	mov    $0xe4,%edx
  404512:	b9 17 00 00 00       	mov    $0x17,%ecx
  404517:	e8 94 e8 ff ff       	call   402db0 <runtime::multi_pointer_slice_expr_error>
  40451c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404521:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404526:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40452b:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404532:	00 
  404533:	48 01 f2             	add    %rsi,%rdx
  404536:	48 29 f0             	sub    %rsi,%rax
  404539:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  404540:	00 
  404541:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  404548:	00 
  404549:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  404550:	00 
  404551:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  404558:	00 
  404559:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404560:	00 
  404561:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404568:	00 
  404569:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404570:	00 
  404571:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404578:	00 
  404579:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404580:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404587:	00 
  404588:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40458f:	00 
  404590:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404597:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40459b:	48 89 11             	mov    %rdx,(%rcx)
  40459e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4045a5:	c3                   	ret
  4045a6:	eb 00                	jmp    4045a8 <runtime::arena_allocator_proc+0x678>
  4045a8:	eb 00                	jmp    4045aa <runtime::arena_allocator_proc+0x67a>
  4045aa:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  4045b1:	00 
  4045b2:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4045b9:	00 
  4045ba:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4045c1:	00 
  4045c2:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4045c9:	00 
  4045ca:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4045d1:	00 
  4045d2:	0f 57 c0             	xorps  %xmm0,%xmm0
  4045d5:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  4045dc:	00 
  4045dd:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  4045e4:	00 
  4045e5:	e8 26 eb ff ff       	call   403110 <runtime::arena_alloc>
  4045ea:	88 44 24 27          	mov    %al,0x27(%rsp)
  4045ee:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  4045f5:	00 
  4045f6:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4045fb:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  404602:	00 
  404603:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404608:	3c 00                	cmp    $0x0,%al
  40460a:	74 50                	je     40465c <runtime::arena_allocator_proc+0x72c>
  40460c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404613:	00 
  404614:	8a 44 24 27          	mov    0x27(%rsp),%al
  404618:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40461f:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404626:	00 
  404627:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40462e:	00 
  40462f:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404636:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40463d:	00 
  40463e:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404645:	00 
  404646:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40464d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404651:	48 89 11             	mov    %rdx,(%rcx)
  404654:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40465b:	c3                   	ret
  40465c:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404661:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  404666:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  40466d:	00 
  40466e:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404675:	00 
  404676:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  40467d:	00 00 
  40467f:	0f 94 c0             	sete   %al
  404682:	24 01                	and    $0x1,%al
  404684:	3c 00                	cmp    $0x0,%al
  404686:	74 45                	je     4046cd <runtime::arena_allocator_proc+0x79d>
  404688:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40468f:	00 
  404690:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404697:	00 
  404698:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40469f:	00 
  4046a0:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4046a7:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4046ae:	00 
  4046af:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4046b6:	00 
  4046b7:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4046be:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4046c2:	48 89 11             	mov    %rdx,(%rcx)
  4046c5:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4046cc:	c3                   	ret
  4046cd:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4046d4:	00 
  4046d5:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4046da:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4046e1:	00 
  4046e2:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4046e7:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4046ee:	00 
  4046ef:	48 89 04 24          	mov    %rax,(%rsp)
  4046f3:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  4046fa:	00 
  4046fb:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  404700:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404705:	31 c0                	xor    %eax,%eax
  404707:	41 89 c0             	mov    %eax,%r8d
  40470a:	be 3e 00 00 00       	mov    $0x3e,%esi
  40470f:	ba ee 00 00 00       	mov    $0xee,%edx
  404714:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  404719:	e8 92 e6 ff ff       	call   402db0 <runtime::multi_pointer_slice_expr_error>
  40471e:	48 8b 0c 24          	mov    (%rsp),%rcx
  404722:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404727:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40472c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404731:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  404738:	00 
  404739:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404740:	00 
  404741:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  404748:	00 
  404749:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  404750:	00 
  404751:	e8 8a ef ff ff       	call   4036e0 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  404756:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40475d:	00 
  40475e:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  404765:	00 
  404766:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40476d:	00 
  40476e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404775:	00 
  404776:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  40477d:	00 
  40477e:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404785:	00 
  404786:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40478a:	48 89 08             	mov    %rcx,(%rax)
  40478d:	31 c0                	xor    %eax,%eax
  40478f:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404796:	c3                   	ret
  404797:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40479e:	00 
  40479f:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4047a6:	00 
  4047a7:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  4047ae:	00 00 
  4047b0:	0f 95 c0             	setne  %al
  4047b3:	24 01                	and    $0x1,%al
  4047b5:	3c 00                	cmp    $0x0,%al
  4047b7:	74 0b                	je     4047c4 <runtime::arena_allocator_proc+0x894>
  4047b9:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4047c0:	00 
  4047c1:	c6 00 5d             	movb   $0x5d,(%rax)
  4047c4:	eb 08                	jmp    4047ce <runtime::arena_allocator_proc+0x89e>
  4047c6:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  4047cd:	04 
  4047ce:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4047d5:	00 
  4047d6:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4047dd:	00 
  4047de:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4047e5:	00 
  4047e6:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4047ed:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4047f4:	00 
  4047f5:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4047fc:	00 
  4047fd:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404804:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404808:	48 89 11             	mov    %rdx,(%rcx)
  40480b:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404812:	c3                   	ret
  404813:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40481a:	84 00 00 00 00 00 

0000000000404820 <runtime::memory_equal>:
  404820:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  404825:	48 89 74 24 b8       	mov    %rsi,-0x48(%rsp)
  40482a:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  40482f:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404834:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404839:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  40483e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404843:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404848:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40484d:	48 83 f8 00          	cmp    $0x0,%rax
  404851:	0f 94 c1             	sete   %cl
  404854:	80 e1 01             	and    $0x1,%cl
  404857:	b0 01                	mov    $0x1,%al
  404859:	38 c8                	cmp    %cl,%al
  40485b:	74 1b                	je     404878 <runtime::memory_equal+0x58>
  40485d:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404862:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404867:	48 39 c8             	cmp    %rcx,%rax
  40486a:	0f 94 c1             	sete   %cl
  40486d:	80 e1 01             	and    $0x1,%cl
  404870:	b0 01                	mov    $0x1,%al
  404872:	38 c8                	cmp    %cl,%al
  404874:	74 07                	je     40487d <runtime::memory_equal+0x5d>
  404876:	eb 03                	jmp    40487b <runtime::memory_equal+0x5b>
  404878:	b0 01                	mov    $0x1,%al
  40487a:	c3                   	ret
  40487b:	eb 03                	jmp    404880 <runtime::memory_equal+0x60>
  40487d:	b0 01                	mov    $0x1,%al
  40487f:	c3                   	ret
  404880:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404885:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40488a:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  40488f:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  404894:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  404899:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40489e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  4048a5:	00 00 
  4048a7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4048ac:	48 3b 44 24 d0       	cmp    -0x30(%rsp),%rax
  4048b1:	0f 92 c0             	setb   %al
  4048b4:	24 01                	and    $0x1,%al
  4048b6:	3c 00                	cmp    $0x0,%al
  4048b8:	74 38                	je     4048f2 <runtime::memory_equal+0xd2>
  4048ba:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4048bf:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4048c4:	8a 04 08             	mov    (%rax,%rcx,1),%al
  4048c7:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4048cc:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4048d1:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  4048d4:	0f 95 c0             	setne  %al
  4048d7:	24 01                	and    $0x1,%al
  4048d9:	3c 00                	cmp    $0x0,%al
  4048db:	74 03                	je     4048e0 <runtime::memory_equal+0xc0>
  4048dd:	31 c0                	xor    %eax,%eax
  4048df:	c3                   	ret
  4048e0:	eb 00                	jmp    4048e2 <runtime::memory_equal+0xc2>
  4048e2:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4048e7:	48 83 c0 01          	add    $0x1,%rax
  4048eb:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4048f0:	eb b5                	jmp    4048a7 <runtime::memory_equal+0x87>
  4048f2:	b0 01                	mov    $0x1,%al
  4048f4:	c3                   	ret
  4048f5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4048fc:	00 00 00 00 

0000000000404900 <runtime::memory_compare>:
  404900:	48 83 ec 10          	sub    $0x10,%rsp
  404904:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  404909:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  40490e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  404913:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404918:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40491d:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  404922:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404927:	48 89 0c 24          	mov    %rcx,(%rsp)
  40492b:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404930:	48 39 c8             	cmp    %rcx,%rax
  404933:	0f 94 c1             	sete   %cl
  404936:	80 e1 01             	and    $0x1,%cl
  404939:	b0 01                	mov    $0x1,%al
  40493b:	38 c8                	cmp    %cl,%al
  40493d:	74 17                	je     404956 <runtime::memory_compare+0x56>
  40493f:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404944:	48 83 f8 00          	cmp    $0x0,%rax
  404948:	0f 94 c1             	sete   %cl
  40494b:	80 e1 01             	and    $0x1,%cl
  40494e:	b0 01                	mov    $0x1,%al
  404950:	38 c8                	cmp    %cl,%al
  404952:	74 20                	je     404974 <runtime::memory_compare+0x74>
  404954:	eb 07                	jmp    40495d <runtime::memory_compare+0x5d>
  404956:	31 c0                	xor    %eax,%eax
  404958:	48 83 c4 10          	add    $0x10,%rsp
  40495c:	c3                   	ret
  40495d:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  404962:	48 83 f8 00          	cmp    $0x0,%rax
  404966:	0f 94 c1             	sete   %cl
  404969:	80 e1 01             	and    $0x1,%cl
  40496c:	b0 01                	mov    $0x1,%al
  40496e:	38 c8                	cmp    %cl,%al
  404970:	74 10                	je     404982 <runtime::memory_compare+0x82>
  404972:	eb 0c                	jmp    404980 <runtime::memory_compare+0x80>
  404974:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40497b:	48 83 c4 10          	add    $0x10,%rsp
  40497f:	c3                   	ret
  404980:	eb 0a                	jmp    40498c <runtime::memory_compare+0x8c>
  404982:	b8 01 00 00 00       	mov    $0x1,%eax
  404987:	48 83 c4 10          	add    $0x10,%rsp
  40498b:	c3                   	ret
  40498c:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  404991:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  404996:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  40499b:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  4049a0:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4049a5:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4049aa:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4049af:	48 c1 e8 03          	shr    $0x3,%rax
  4049b3:	48 83 c0 01          	add    $0x1,%rax
  4049b7:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4049bc:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4049c1:	48 83 e8 01          	sub    $0x1,%rax
  4049c5:	48 c1 e0 03          	shl    $0x3,%rax
  4049c9:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4049ce:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  4049d5:	00 00 
  4049d7:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  4049dd:	0f 92 c0             	setb   %al
  4049e0:	24 01                	and    $0x1,%al
  4049e2:	3c 00                	cmp    $0x0,%al
  4049e4:	74 09                	je     4049ef <runtime::memory_compare+0xef>
  4049e6:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4049ed:	00 00 
  4049ef:	eb 00                	jmp    4049f1 <runtime::memory_compare+0xf1>
  4049f1:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4049f6:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  4049fb:	0f 92 c0             	setb   %al
  4049fe:	24 01                	and    $0x1,%al
  404a00:	3c 00                	cmp    $0x0,%al
  404a02:	0f 84 11 01 00 00    	je     404b19 <runtime::memory_compare+0x219>
  404a08:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404a0d:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404a12:	48 c1 e1 03          	shl    $0x3,%rcx
  404a16:	48 01 c8             	add    %rcx,%rax
  404a19:	48 8b 00             	mov    (%rax),%rax
  404a1c:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404a21:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404a26:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404a2b:	48 c1 e1 03          	shl    $0x3,%rcx
  404a2f:	48 01 c8             	add    %rcx,%rax
  404a32:	48 8b 00             	mov    (%rax),%rax
  404a35:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404a3a:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404a3f:	48 33 44 24 b8       	xor    -0x48(%rsp),%rax
  404a44:	48 83 f8 00          	cmp    $0x0,%rax
  404a48:	0f 95 c0             	setne  %al
  404a4b:	24 01                	and    $0x1,%al
  404a4d:	3c 00                	cmp    $0x0,%al
  404a4f:	0f 84 af 00 00 00    	je     404b04 <runtime::memory_compare+0x204>
  404a55:	eb 00                	jmp    404a57 <runtime::memory_compare+0x157>
  404a57:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404a5c:	48 c1 e0 03          	shl    $0x3,%rax
  404a60:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  404a65:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404a6a:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404a6f:	0f 92 c0             	setb   %al
  404a72:	24 01                	and    $0x1,%al
  404a74:	3c 00                	cmp    $0x0,%al
  404a76:	0f 84 86 00 00 00    	je     404b02 <runtime::memory_compare+0x202>
  404a7c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404a81:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  404a86:	8a 00                	mov    (%rax),%al
  404a88:	88 44 24 af          	mov    %al,-0x51(%rsp)
  404a8c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404a91:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  404a96:	8a 00                	mov    (%rax),%al
  404a98:	88 44 24 ae          	mov    %al,-0x52(%rsp)
  404a9c:	8a 44 24 af          	mov    -0x51(%rsp),%al
  404aa0:	32 44 24 ae          	xor    -0x52(%rsp),%al
  404aa4:	3c 00                	cmp    $0x0,%al
  404aa6:	0f 95 c0             	setne  %al
  404aa9:	24 01                	and    $0x1,%al
  404aab:	3c 00                	cmp    $0x0,%al
  404aad:	74 3e                	je     404aed <runtime::memory_compare+0x1ed>
  404aaf:	0f b6 44 24 af       	movzbl -0x51(%rsp),%eax
  404ab4:	0f b6 4c 24 ae       	movzbl -0x52(%rsp),%ecx
  404ab9:	48 29 c8             	sub    %rcx,%rax
  404abc:	48 83 f8 00          	cmp    $0x0,%rax
  404ac0:	0f 9c c0             	setl   %al
  404ac3:	24 01                	and    $0x1,%al
  404ac5:	3c 00                	cmp    $0x0,%al
  404ac7:	74 0e                	je     404ad7 <runtime::memory_compare+0x1d7>
  404ac9:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404ad0:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  404ad5:	eb 0c                	jmp    404ae3 <runtime::memory_compare+0x1e3>
  404ad7:	b8 01 00 00 00       	mov    $0x1,%eax
  404adc:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  404ae1:	eb 00                	jmp    404ae3 <runtime::memory_compare+0x1e3>
  404ae3:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  404ae8:	48 83 c4 10          	add    $0x10,%rsp
  404aec:	c3                   	ret
  404aed:	eb 00                	jmp    404aef <runtime::memory_compare+0x1ef>
  404aef:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404af4:	48 83 c0 01          	add    $0x1,%rax
  404af8:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  404afd:	e9 63 ff ff ff       	jmp    404a65 <runtime::memory_compare+0x165>
  404b02:	eb 00                	jmp    404b04 <runtime::memory_compare+0x204>
  404b04:	eb 00                	jmp    404b06 <runtime::memory_compare+0x206>
  404b06:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404b0b:	48 83 c0 01          	add    $0x1,%rax
  404b0f:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404b14:	e9 d8 fe ff ff       	jmp    4049f1 <runtime::memory_compare+0xf1>
  404b19:	eb 00                	jmp    404b1b <runtime::memory_compare+0x21b>
  404b1b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404b20:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404b25:	0f 92 c0             	setb   %al
  404b28:	24 01                	and    $0x1,%al
  404b2a:	3c 00                	cmp    $0x0,%al
  404b2c:	0f 84 86 00 00 00    	je     404bb8 <runtime::memory_compare+0x2b8>
  404b32:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404b37:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404b3c:	8a 00                	mov    (%rax),%al
  404b3e:	88 44 24 ad          	mov    %al,-0x53(%rsp)
  404b42:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404b47:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404b4c:	8a 00                	mov    (%rax),%al
  404b4e:	88 44 24 ac          	mov    %al,-0x54(%rsp)
  404b52:	8a 44 24 ad          	mov    -0x53(%rsp),%al
  404b56:	32 44 24 ac          	xor    -0x54(%rsp),%al
  404b5a:	3c 00                	cmp    $0x0,%al
  404b5c:	0f 95 c0             	setne  %al
  404b5f:	24 01                	and    $0x1,%al
  404b61:	3c 00                	cmp    $0x0,%al
  404b63:	74 3e                	je     404ba3 <runtime::memory_compare+0x2a3>
  404b65:	0f b6 44 24 ad       	movzbl -0x53(%rsp),%eax
  404b6a:	0f b6 4c 24 ac       	movzbl -0x54(%rsp),%ecx
  404b6f:	48 29 c8             	sub    %rcx,%rax
  404b72:	48 83 f8 00          	cmp    $0x0,%rax
  404b76:	0f 9c c0             	setl   %al
  404b79:	24 01                	and    $0x1,%al
  404b7b:	3c 00                	cmp    $0x0,%al
  404b7d:	74 0e                	je     404b8d <runtime::memory_compare+0x28d>
  404b7f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404b86:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  404b8b:	eb 0c                	jmp    404b99 <runtime::memory_compare+0x299>
  404b8d:	b8 01 00 00 00       	mov    $0x1,%eax
  404b92:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  404b97:	eb 00                	jmp    404b99 <runtime::memory_compare+0x299>
  404b99:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  404b9e:	48 83 c4 10          	add    $0x10,%rsp
  404ba2:	c3                   	ret
  404ba3:	eb 00                	jmp    404ba5 <runtime::memory_compare+0x2a5>
  404ba5:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404baa:	48 83 c0 01          	add    $0x1,%rax
  404bae:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404bb3:	e9 63 ff ff ff       	jmp    404b1b <runtime::memory_compare+0x21b>
  404bb8:	31 c0                	xor    %eax,%eax
  404bba:	48 83 c4 10          	add    $0x10,%rsp
  404bbe:	c3                   	ret
  404bbf:	90                   	nop

0000000000404bc0 <runtime::memory_compare_zero>:
  404bc0:	48 89 7c 24 a0       	mov    %rdi,-0x60(%rsp)
  404bc5:	48 89 74 24 a8       	mov    %rsi,-0x58(%rsp)
  404bca:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  404bcf:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  404bd4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404bd9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  404bde:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  404be3:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404be8:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404bed:	48 c1 e8 03          	shr    $0x3,%rax
  404bf1:	48 83 c0 01          	add    $0x1,%rax
  404bf5:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404bfa:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404bff:	48 83 e8 01          	sub    $0x1,%rax
  404c03:	48 c1 e0 03          	shl    $0x3,%rax
  404c07:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404c0c:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  404c13:	00 00 
  404c15:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  404c1b:	0f 92 c0             	setb   %al
  404c1e:	24 01                	and    $0x1,%al
  404c20:	3c 00                	cmp    $0x0,%al
  404c22:	74 09                	je     404c2d <runtime::memory_compare_zero+0x6d>
  404c24:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404c2b:	00 00 
  404c2d:	eb 00                	jmp    404c2f <runtime::memory_compare_zero+0x6f>
  404c2f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404c34:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  404c39:	0f 92 c0             	setb   %al
  404c3c:	24 01                	and    $0x1,%al
  404c3e:	3c 00                	cmp    $0x0,%al
  404c40:	0f 84 d2 00 00 00    	je     404d18 <runtime::memory_compare_zero+0x158>
  404c46:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404c4b:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404c50:	48 c1 e1 03          	shl    $0x3,%rcx
  404c54:	48 01 c8             	add    %rcx,%rax
  404c57:	48 8b 00             	mov    (%rax),%rax
  404c5a:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404c5f:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404c64:	48 83 f0 00          	xor    $0x0,%rax
  404c68:	48 83 f8 00          	cmp    $0x0,%rax
  404c6c:	0f 95 c0             	setne  %al
  404c6f:	24 01                	and    $0x1,%al
  404c71:	3c 00                	cmp    $0x0,%al
  404c73:	0f 84 8a 00 00 00    	je     404d03 <runtime::memory_compare_zero+0x143>
  404c79:	eb 00                	jmp    404c7b <runtime::memory_compare_zero+0xbb>
  404c7b:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404c80:	48 c1 e0 03          	shl    $0x3,%rax
  404c84:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404c89:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  404c8e:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404c93:	0f 92 c0             	setb   %al
  404c96:	24 01                	and    $0x1,%al
  404c98:	3c 00                	cmp    $0x0,%al
  404c9a:	74 65                	je     404d01 <runtime::memory_compare_zero+0x141>
  404c9c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404ca1:	48 03 44 24 b8       	add    -0x48(%rsp),%rax
  404ca6:	8a 00                	mov    (%rax),%al
  404ca8:	88 44 24 b7          	mov    %al,-0x49(%rsp)
  404cac:	8a 44 24 b7          	mov    -0x49(%rsp),%al
  404cb0:	34 00                	xor    $0x0,%al
  404cb2:	3c 00                	cmp    $0x0,%al
  404cb4:	0f 95 c0             	setne  %al
  404cb7:	24 01                	and    $0x1,%al
  404cb9:	3c 00                	cmp    $0x0,%al
  404cbb:	74 32                	je     404cef <runtime::memory_compare_zero+0x12f>
  404cbd:	0f b6 44 24 b7       	movzbl -0x49(%rsp),%eax
  404cc2:	48 83 f8 00          	cmp    $0x0,%rax
  404cc6:	0f 9c c0             	setl   %al
  404cc9:	24 01                	and    $0x1,%al
  404ccb:	3c 00                	cmp    $0x0,%al
  404ccd:	74 0e                	je     404cdd <runtime::memory_compare_zero+0x11d>
  404ccf:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404cd6:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  404cdb:	eb 0c                	jmp    404ce9 <runtime::memory_compare_zero+0x129>
  404cdd:	b8 01 00 00 00       	mov    $0x1,%eax
  404ce2:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  404ce7:	eb 00                	jmp    404ce9 <runtime::memory_compare_zero+0x129>
  404ce9:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  404cee:	c3                   	ret
  404cef:	eb 00                	jmp    404cf1 <runtime::memory_compare_zero+0x131>
  404cf1:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  404cf6:	48 83 c0 01          	add    $0x1,%rax
  404cfa:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404cff:	eb 88                	jmp    404c89 <runtime::memory_compare_zero+0xc9>
  404d01:	eb 00                	jmp    404d03 <runtime::memory_compare_zero+0x143>
  404d03:	eb 00                	jmp    404d05 <runtime::memory_compare_zero+0x145>
  404d05:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404d0a:	48 83 c0 01          	add    $0x1,%rax
  404d0e:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404d13:	e9 17 ff ff ff       	jmp    404c2f <runtime::memory_compare_zero+0x6f>
  404d18:	eb 00                	jmp    404d1a <runtime::memory_compare_zero+0x15a>
  404d1a:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404d1f:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404d24:	0f 92 c0             	setb   %al
  404d27:	24 01                	and    $0x1,%al
  404d29:	3c 00                	cmp    $0x0,%al
  404d2b:	74 65                	je     404d92 <runtime::memory_compare_zero+0x1d2>
  404d2d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404d32:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404d37:	8a 00                	mov    (%rax),%al
  404d39:	88 44 24 b6          	mov    %al,-0x4a(%rsp)
  404d3d:	8a 44 24 b6          	mov    -0x4a(%rsp),%al
  404d41:	34 00                	xor    $0x0,%al
  404d43:	3c 00                	cmp    $0x0,%al
  404d45:	0f 95 c0             	setne  %al
  404d48:	24 01                	and    $0x1,%al
  404d4a:	3c 00                	cmp    $0x0,%al
  404d4c:	74 32                	je     404d80 <runtime::memory_compare_zero+0x1c0>
  404d4e:	0f b6 44 24 b6       	movzbl -0x4a(%rsp),%eax
  404d53:	48 83 f8 00          	cmp    $0x0,%rax
  404d57:	0f 9c c0             	setl   %al
  404d5a:	24 01                	and    $0x1,%al
  404d5c:	3c 00                	cmp    $0x0,%al
  404d5e:	74 0e                	je     404d6e <runtime::memory_compare_zero+0x1ae>
  404d60:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404d67:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  404d6c:	eb 0c                	jmp    404d7a <runtime::memory_compare_zero+0x1ba>
  404d6e:	b8 01 00 00 00       	mov    $0x1,%eax
  404d73:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  404d78:	eb 00                	jmp    404d7a <runtime::memory_compare_zero+0x1ba>
  404d7a:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404d7f:	c3                   	ret
  404d80:	eb 00                	jmp    404d82 <runtime::memory_compare_zero+0x1c2>
  404d82:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404d87:	48 83 c0 01          	add    $0x1,%rax
  404d8b:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404d90:	eb 88                	jmp    404d1a <runtime::memory_compare_zero+0x15a>
  404d92:	31 c0                	xor    %eax,%eax
  404d94:	c3                   	ret
  404d95:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  404d9c:	00 00 00 00 

0000000000404da0 <runtime::__type_info_of>:
  404da0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  404da5:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404daa:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404daf:	48 c7 c1 a0 74 40 00 	mov    $0x4074a0,%rcx
  404db6:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  404dba:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404dbf:	31 c9                	xor    %ecx,%ecx
  404dc1:	89 ca                	mov    %ecx,%edx
  404dc3:	48 f7 74 24 f0       	divq   -0x10(%rsp)
  404dc8:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  404dcd:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  404dd4:	00 00 
  404dd6:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404ddd:	00 00 
  404ddf:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404de4:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  404de9:	0f 83 9f 00 00 00    	jae    404e8e <runtime::__type_info_of+0xee>
  404def:	48 c7 c0 a0 74 40 00 	mov    $0x4074a0,%rax
  404df6:	48 8b 00             	mov    (%rax),%rax
  404df9:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  404dfe:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  404e02:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404e07:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  404e0d:	0f 95 c0             	setne  %al
  404e10:	24 01                	and    $0x1,%al
  404e12:	3c 00                	cmp    $0x0,%al
  404e14:	74 1d                	je     404e33 <runtime::__type_info_of+0x93>
  404e16:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404e1b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404e20:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  404e24:	0f 94 c0             	sete   %al
  404e27:	24 01                	and    $0x1,%al
  404e29:	3c 00                	cmp    $0x0,%al
  404e2b:	74 06                	je     404e33 <runtime::__type_info_of+0x93>
  404e2d:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404e32:	c3                   	ret
  404e33:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404e38:	48 83 c0 01          	add    $0x1,%rax
  404e3c:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  404e41:	0f 92 c0             	setb   %al
  404e44:	24 01                	and    $0x1,%al
  404e46:	3c 00                	cmp    $0x0,%al
  404e48:	74 10                	je     404e5a <runtime::__type_info_of+0xba>
  404e4a:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404e4f:	48 83 c0 01          	add    $0x1,%rax
  404e53:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404e58:	eb 09                	jmp    404e63 <runtime::__type_info_of+0xc3>
  404e5a:	31 c0                	xor    %eax,%eax
  404e5c:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404e61:	eb 00                	jmp    404e63 <runtime::__type_info_of+0xc3>
  404e63:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404e68:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  404e6d:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404e72:	48 83 c0 01          	add    $0x1,%rax
  404e76:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404e7b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404e80:	48 83 c0 01          	add    $0x1,%rax
  404e84:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404e89:	e9 51 ff ff ff       	jmp    404ddf <runtime::__type_info_of+0x3f>
  404e8e:	48 c7 c0 a0 74 40 00 	mov    $0x4074a0,%rax
  404e95:	48 8b 00             	mov    (%rax),%rax
  404e98:	48 8b 00             	mov    (%rax),%rax
  404e9b:	c3                   	ret
  404e9c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000404ea0 <runtime::default_logger_proc>:
  404ea0:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  404ea5:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  404eaa:	66 44 89 c0          	mov    %r8w,%ax
  404eae:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  404eb3:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  404eb8:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  404ebd:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  404ec2:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  404ec7:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  404ecc:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  404ed1:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  404ed6:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  404edb:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  404ee0:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  404ee5:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  404eea:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  404eef:	c3                   	ret

0000000000404ef0 <runtime::default_context>:
  404ef0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  404ef7:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404efc:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  404f01:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  404f06:	31 f6                	xor    %esi,%esi
  404f08:	ba 70 00 00 00       	mov    $0x70,%edx
  404f0d:	e8 2e c1 ff ff       	call   401040 <memset@plt>
  404f12:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  404f17:	e8 24 00 00 00       	call   404f40 <runtime::[core.odin]::__init_context>
  404f1c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  404f21:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  404f26:	ba 70 00 00 00       	mov    $0x70,%edx
  404f2b:	e8 30 c1 ff ff       	call   401060 <memcpy@plt>
  404f30:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404f35:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  404f3c:	c3                   	ret
  404f3d:	0f 1f 00             	nopl   (%rax)

0000000000404f40 <runtime::[core.odin]::__init_context>:
  404f40:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  404f45:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404f4a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404f4f:	48 83 f8 00          	cmp    $0x0,%rax
  404f53:	0f 94 c0             	sete   %al
  404f56:	24 01                	and    $0x1,%al
  404f58:	3c 00                	cmp    $0x0,%al
  404f5a:	74 01                	je     404f5d <runtime::[core.odin]::__init_context+0x1d>
  404f5c:	c3                   	ret
  404f5d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404f62:	48 c7 c1 70 1c 40 00 	mov    $0x401c70,%rcx
  404f69:	48 89 08             	mov    %rcx,(%rax)
  404f6c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404f71:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  404f78:	00 
  404f79:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404f7e:	48 c7 c1 00 27 40 00 	mov    $0x402700,%rcx
  404f85:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404f89:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404f8e:	48 c7 c2 c0 ff ff ff 	mov    $0xffffffffffffffc0,%rdx
  404f95:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  404f9c:	00 00 
  404f9e:	48 01 d1             	add    %rdx,%rcx
  404fa1:	48 89 48 18          	mov    %rcx,0x18(%rax)
  404fa5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404faa:	48 c7 c1 f0 4f 40 00 	mov    $0x404ff0,%rcx
  404fb1:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404fb5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404fba:	48 c7 c1 a0 4e 40 00 	mov    $0x404ea0,%rcx
  404fc1:	48 89 48 28          	mov    %rcx,0x28(%rax)
  404fc5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404fca:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  404fd1:	00 
  404fd2:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404fd7:	48 c7 c1 c0 23 40 00 	mov    $0x4023c0,%rcx
  404fde:	48 89 48 48          	mov    %rcx,0x48(%rax)
  404fe2:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404fe7:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  404fee:	00 
  404fef:	c3                   	ret

0000000000404ff0 <runtime::default_assertion_failure_proc>:
  404ff0:	48 83 ec 48          	sub    $0x48,%rsp
  404ff4:	4c 89 04 24          	mov    %r8,(%rsp)
  404ff8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  404ffd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405002:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405007:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40500c:	4c 8b 04 24          	mov    (%rsp),%r8
  405010:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405015:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40501a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40501f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405024:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405029:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40502e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405033:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  405038:	e8 03 00 00 00       	call   405040 <runtime::default_assertion_contextless_failure_proc>
  40503d:	0f 1f 00             	nopl   (%rax)

0000000000405040 <runtime::default_assertion_contextless_failure_proc>:
  405040:	48 83 ec 48          	sub    $0x48,%rsp
  405044:	4c 89 04 24          	mov    %r8,(%rsp)
  405048:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40504d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405052:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405057:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40505c:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405061:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  405066:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40506b:	48 8b 3c 24          	mov    (%rsp),%rdi
  40506f:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405074:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405079:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40507e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405083:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  405088:	e8 d3 ec ff ff       	call   403d60 <runtime::print_caller_location>
  40508d:	bf 92 72 40 00       	mov    $0x407292,%edi
  405092:	be 01 00 00 00       	mov    $0x1,%esi
  405097:	e8 a4 e4 ff ff       	call   403540 <runtime::print_string>
  40509c:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4050a1:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4050a6:	e8 95 e4 ff ff       	call   403540 <runtime::print_string>
  4050ab:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4050b0:	48 83 f8 00          	cmp    $0x0,%rax
  4050b4:	0f 9f c0             	setg   %al
  4050b7:	24 01                	and    $0x1,%al
  4050b9:	3c 00                	cmp    $0x0,%al
  4050bb:	74 1e                	je     4050db <runtime::default_assertion_contextless_failure_proc+0x9b>
  4050bd:	bf 94 72 40 00       	mov    $0x407294,%edi
  4050c2:	be 02 00 00 00       	mov    $0x2,%esi
  4050c7:	e8 74 e4 ff ff       	call   403540 <runtime::print_string>
  4050cc:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4050d1:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4050d6:	e8 65 e4 ff ff       	call   403540 <runtime::print_string>
  4050db:	bf 0a 00 00 00       	mov    $0xa,%edi
  4050e0:	e8 8b e6 ff ff       	call   403770 <runtime::print_byte>
  4050e5:	0f 0b                	ud2
  4050e7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4050ee:	00 00 

00000000004050f0 <__truncsfhf2>:
  4050f0:	48 83 ec 18          	sub    $0x18,%rsp
  4050f4:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  4050fa:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  405100:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  405106:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40510d:	00 
  40510e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  405115:	00 
  405116:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40511d:	00 
  40511e:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  405125:	00 
  405126:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  40512d:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  405133:	8b 44 24 10          	mov    0x10(%rsp),%eax
  405137:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  40513b:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40513f:	c1 f9 10             	sar    $0x10,%ecx
  405142:	b2 01                	mov    $0x1,%dl
  405144:	31 c0                	xor    %eax,%eax
  405146:	f6 c2 01             	test   $0x1,%dl
  405149:	0f 45 c1             	cmovne %ecx,%eax
  40514c:	25 00 80 00 00       	and    $0x8000,%eax
  405151:	89 44 24 08          	mov    %eax,0x8(%rsp)
  405155:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  405159:	c1 f9 17             	sar    $0x17,%ecx
  40515c:	b2 01                	mov    $0x1,%dl
  40515e:	31 c0                	xor    %eax,%eax
  405160:	f6 c2 01             	test   $0x1,%dl
  405163:	0f 45 c1             	cmovne %ecx,%eax
  405166:	25 ff 00 00 00       	and    $0xff,%eax
  40516b:	83 e8 70             	sub    $0x70,%eax
  40516e:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405172:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  405176:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  40517b:	89 04 24             	mov    %eax,(%rsp)
  40517e:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  405183:	0f 9e c0             	setle  %al
  405186:	24 01                	and    $0x1,%al
  405188:	3c 00                	cmp    $0x0,%al
  40518a:	0f 84 82 00 00 00    	je     405212 <__truncsfhf2+0x122>
  405190:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  405195:	0f 9c c0             	setl   %al
  405198:	24 01                	and    $0x1,%al
  40519a:	3c 00                	cmp    $0x0,%al
  40519c:	74 16                	je     4051b4 <__truncsfhf2+0xc4>
  40519e:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  4051a3:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  4051a8:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  4051af:	48 83 c4 18          	add    $0x18,%rsp
  4051b3:	c3                   	ret
  4051b4:	8b 04 24             	mov    (%rsp),%eax
  4051b7:	0d 00 00 80 00       	or     $0x800000,%eax
  4051bc:	ba 01 00 00 00       	mov    $0x1,%edx
  4051c1:	2b 54 24 04          	sub    0x4(%rsp),%edx
  4051c5:	89 d1                	mov    %edx,%ecx
  4051c7:	d3 f8                	sar    %cl,%eax
  4051c9:	89 c1                	mov    %eax,%ecx
  4051cb:	31 c0                	xor    %eax,%eax
  4051cd:	83 fa 20             	cmp    $0x20,%edx
  4051d0:	0f 42 c1             	cmovb  %ecx,%eax
  4051d3:	89 04 24             	mov    %eax,(%rsp)
  4051d6:	8b 04 24             	mov    (%rsp),%eax
  4051d9:	25 00 10 00 00       	and    $0x1000,%eax
  4051de:	83 f8 00             	cmp    $0x0,%eax
  4051e1:	0f 95 c0             	setne  %al
  4051e4:	24 01                	and    $0x1,%al
  4051e6:	3c 00                	cmp    $0x0,%al
  4051e8:	74 0b                	je     4051f5 <__truncsfhf2+0x105>
  4051ea:	8b 04 24             	mov    (%rsp),%eax
  4051ed:	05 00 20 00 00       	add    $0x2000,%eax
  4051f2:	89 04 24             	mov    %eax,(%rsp)
  4051f5:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4051f9:	8b 0c 24             	mov    (%rsp),%ecx
  4051fc:	c1 e9 0d             	shr    $0xd,%ecx
  4051ff:	09 c8                	or     %ecx,%eax
  405201:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  405206:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40520d:	48 83 c4 18          	add    $0x18,%rsp
  405211:	c3                   	ret
  405212:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  405219:	00 
  40521a:	0f 94 c0             	sete   %al
  40521d:	24 01                	and    $0x1,%al
  40521f:	3c 00                	cmp    $0x0,%al
  405221:	74 59                	je     40527c <__truncsfhf2+0x18c>
  405223:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  405227:	0f 94 c0             	sete   %al
  40522a:	24 01                	and    $0x1,%al
  40522c:	3c 00                	cmp    $0x0,%al
  40522e:	74 1a                	je     40524a <__truncsfhf2+0x15a>
  405230:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405234:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405239:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  40523e:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  405245:	48 83 c4 18          	add    $0x18,%rsp
  405249:	c3                   	ret
  40524a:	8b 04 24             	mov    (%rsp),%eax
  40524d:	c1 f8 0d             	sar    $0xd,%eax
  405250:	89 04 24             	mov    %eax,(%rsp)
  405253:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405257:	8b 0c 24             	mov    (%rsp),%ecx
  40525a:	09 c8                	or     %ecx,%eax
  40525c:	85 c9                	test   %ecx,%ecx
  40525e:	0f 94 c1             	sete   %cl
  405261:	0f b6 c9             	movzbl %cl,%ecx
  405264:	09 c8                	or     %ecx,%eax
  405266:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40526b:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  405270:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  405277:	48 83 c4 18          	add    $0x18,%rsp
  40527b:	c3                   	ret
  40527c:	8b 04 24             	mov    (%rsp),%eax
  40527f:	25 00 10 00 00       	and    $0x1000,%eax
  405284:	83 f8 00             	cmp    $0x0,%eax
  405287:	0f 95 c0             	setne  %al
  40528a:	24 01                	and    $0x1,%al
  40528c:	3c 00                	cmp    $0x0,%al
  40528e:	74 33                	je     4052c3 <__truncsfhf2+0x1d3>
  405290:	8b 04 24             	mov    (%rsp),%eax
  405293:	05 00 20 00 00       	add    $0x2000,%eax
  405298:	89 04 24             	mov    %eax,(%rsp)
  40529b:	8b 04 24             	mov    (%rsp),%eax
  40529e:	25 00 00 80 00       	and    $0x800000,%eax
  4052a3:	83 f8 00             	cmp    $0x0,%eax
  4052a6:	0f 95 c0             	setne  %al
  4052a9:	24 01                	and    $0x1,%al
  4052ab:	3c 00                	cmp    $0x0,%al
  4052ad:	74 12                	je     4052c1 <__truncsfhf2+0x1d1>
  4052af:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  4052b6:	8b 44 24 04          	mov    0x4(%rsp),%eax
  4052ba:	83 c0 01             	add    $0x1,%eax
  4052bd:	89 44 24 04          	mov    %eax,0x4(%rsp)
  4052c1:	eb 00                	jmp    4052c3 <__truncsfhf2+0x1d3>
  4052c3:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  4052c8:	0f 9f c0             	setg   %al
  4052cb:	24 01                	and    $0x1,%al
  4052cd:	3c 00                	cmp    $0x0,%al
  4052cf:	74 75                	je     405346 <__truncsfhf2+0x256>
  4052d1:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  4052d8:	00 00 00 
  4052db:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  4052e0:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  4052e7:	00 00 
  4052e9:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  4052ef:	0f 9c c0             	setl   %al
  4052f2:	24 01                	and    $0x1,%al
  4052f4:	3c 00                	cmp    $0x0,%al
  4052f6:	74 34                	je     40532c <__truncsfhf2+0x23c>
  4052f8:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4052fd:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405302:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405307:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  40530d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405312:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405317:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40531c:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405321:	48 83 c0 01          	add    $0x1,%rax
  405325:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40532a:	eb bd                	jmp    4052e9 <__truncsfhf2+0x1f9>
  40532c:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405330:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405335:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  40533a:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  405341:	48 83 c4 18          	add    $0x18,%rsp
  405345:	c3                   	ret
  405346:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40534a:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  40534e:	c1 e1 0a             	shl    $0xa,%ecx
  405351:	09 c8                	or     %ecx,%eax
  405353:	8b 0c 24             	mov    (%rsp),%ecx
  405356:	c1 e9 0d             	shr    $0xd,%ecx
  405359:	09 c8                	or     %ecx,%eax
  40535b:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  405360:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  405367:	48 83 c4 18          	add    $0x18,%rsp
  40536b:	c3                   	ret
  40536c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405370 <__truncdfhf2>:
  405370:	48 83 ec 18          	sub    $0x18,%rsp
  405374:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  40537a:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  405380:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  405386:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  40538a:	e8 61 fd ff ff       	call   4050f0 <__truncsfhf2>
  40538f:	48 83 c4 18          	add    $0x18,%rsp
  405393:	c3                   	ret
  405394:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40539b:	00 00 00 00 00 

00000000004053a0 <__gnu_h2f_ieee>:
  4053a0:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  4053a6:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  4053ac:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  4053b3:	00 
  4053b4:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  4053bb:	00 
  4053bc:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  4053c1:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  4053c6:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  4053cd:	00 
  4053ce:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  4053d5:	00 
  4053d6:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  4053dd:	00 
  4053de:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  4053e5:	77 
  4053e6:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  4053ed:	47 
  4053ee:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  4053f3:	66 25 ff 7f          	and    $0x7fff,%ax
  4053f7:	0f b7 c8             	movzwl %ax,%ecx
  4053fa:	c1 e1 0d             	shl    $0xd,%ecx
  4053fd:	b2 01                	mov    $0x1,%dl
  4053ff:	31 c0                	xor    %eax,%eax
  405401:	f6 c2 01             	test   $0x1,%dl
  405404:	0f 45 c1             	cmovne %ecx,%eax
  405407:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40540b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  405411:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  405417:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  40541d:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405423:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  405428:	0f 93 c0             	setae  %al
  40542b:	24 01                	and    $0x1,%al
  40542d:	3c 00                	cmp    $0x0,%al
  40542f:	74 0d                	je     40543e <__gnu_h2f_ieee+0x9e>
  405431:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405435:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  40543a:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40543e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405443:	66 25 00 80          	and    $0x8000,%ax
  405447:	0f b7 c8             	movzwl %ax,%ecx
  40544a:	c1 e1 10             	shl    $0x10,%ecx
  40544d:	b2 01                	mov    $0x1,%dl
  40544f:	31 c0                	xor    %eax,%eax
  405451:	f6 c2 01             	test   $0x1,%dl
  405454:	0f 45 c1             	cmovne %ecx,%eax
  405457:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  40545b:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40545f:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405465:	c3                   	ret
  405466:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40546d:	00 00 00 

0000000000405470 <__gnu_f2h_ieee>:
  405470:	50                   	push   %rax
  405471:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  405476:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  40547b:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  405481:	e8 6a fc ff ff       	call   4050f0 <__truncsfhf2>
  405486:	58                   	pop    %rax
  405487:	c3                   	ret
  405488:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40548f:	00 

0000000000405490 <__extendhfsf2>:
  405490:	50                   	push   %rax
  405491:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  405497:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  40549d:	0f 28 c8             	movaps %xmm0,%xmm1
  4054a0:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  4054a7:	00 
  4054a8:	e8 f3 fe ff ff       	call   4053a0 <__gnu_h2f_ieee>
  4054ad:	58                   	pop    %rax
  4054ae:	c3                   	ret
  4054af:	90                   	nop

00000000004054b0 <__floattidf>:
  4054b0:	53                   	push   %rbx
  4054b1:	48 83 ec 10          	sub    $0x10,%rsp
  4054b5:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  4054ba:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4054bf:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  4054c4:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4054c9:	48 89 04 24          	mov    %rax,(%rsp)
  4054cd:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4054d2:	48 09 c8             	or     %rcx,%rax
  4054d5:	0f 94 c0             	sete   %al
  4054d8:	24 01                	and    $0x1,%al
  4054da:	3c 00                	cmp    $0x0,%al
  4054dc:	74 09                	je     4054e7 <__floattidf+0x37>
  4054de:	0f 57 c0             	xorps  %xmm0,%xmm0
  4054e1:	48 83 c4 10          	add    $0x10,%rsp
  4054e5:	5b                   	pop    %rbx
  4054e6:	c3                   	ret
  4054e7:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4054ec:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  4054f1:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4054f6:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4054fb:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405500:	48 c1 f8 3f          	sar    $0x3f,%rax
  405504:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405509:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40550e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405513:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405518:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40551d:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405522:	48 31 d0             	xor    %rdx,%rax
  405525:	48 31 f1             	xor    %rsi,%rcx
  405528:	48 29 f1             	sub    %rsi,%rcx
  40552b:	48 19 d0             	sbb    %rdx,%rax
  40552e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405533:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405538:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  40553d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  405542:	48 0f bd c2          	bsr    %rdx,%rax
  405546:	48 83 f0 3f          	xor    $0x3f,%rax
  40554a:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40554f:	48 0f bd ce          	bsr    %rsi,%rcx
  405553:	48 83 f1 3f          	xor    $0x3f,%rcx
  405557:	48 83 c1 40          	add    $0x40,%rcx
  40555b:	48 85 d2             	test   %rdx,%rdx
  40555e:	48 0f 45 c8          	cmovne %rax,%rcx
  405562:	31 c0                	xor    %eax,%eax
  405564:	ba 80 00 00 00       	mov    $0x80,%edx
  405569:	48 29 ca             	sub    %rcx,%rdx
  40556c:	48 89 c1             	mov    %rax,%rcx
  40556f:	48 19 c9             	sbb    %rcx,%rcx
  405572:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405577:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  40557c:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  405580:	ff c9                	dec    %ecx
  405582:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  405586:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  40558b:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405590:	ba 35 00 00 00       	mov    $0x35,%edx
  405595:	48 29 f2             	sub    %rsi,%rdx
  405598:	48 19 c8             	sbb    %rcx,%rax
  40559b:	0f 9c c0             	setl   %al
  40559e:	24 01                	and    $0x1,%al
  4055a0:	3c 00                	cmp    $0x0,%al
  4055a2:	0f 84 c0 01 00 00    	je     405768 <__floattidf+0x2b8>
  4055a8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4055ad:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  4055b2:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4055b7:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  4055bc:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  4055c1:	0f 28 0d 58 1d 00 00 	movaps 0x1d58(%rip),%xmm1        # 407320 <_IO_stdin_used+0x320>
  4055c8:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4055cc:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  4055d1:	74 17                	je     4055ea <__floattidf+0x13a>
  4055d3:	eb 00                	jmp    4055d5 <__floattidf+0x125>
  4055d5:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  4055da:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  4055df:	48 83 f0 37          	xor    $0x37,%rax
  4055e3:	48 09 c8             	or     %rcx,%rax
  4055e6:	74 26                	je     40560e <__floattidf+0x15e>
  4055e8:	eb 29                	jmp    405613 <__floattidf+0x163>
  4055ea:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  4055ef:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4055f4:	48 89 d0             	mov    %rdx,%rax
  4055f7:	48 01 c0             	add    %rax,%rax
  4055fa:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  4055ff:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405604:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405609:	e9 d6 00 00 00       	jmp    4056e4 <__floattidf+0x234>
  40560e:	e9 d1 00 00 00       	jmp    4056e4 <__floattidf+0x234>
  405613:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405618:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  40561d:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405622:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405627:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40562c:	49 89 fb             	mov    %rdi,%r11
  40562f:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405633:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405637:	44 88 db             	mov    %r11b,%bl
  40563a:	88 d9                	mov    %bl,%cl
  40563c:	49 89 f2             	mov    %rsi,%r10
  40563f:	49 d3 ea             	shr    %cl,%r10
  405642:	88 d9                	mov    %bl,%cl
  405644:	49 89 d1             	mov    %rdx,%r9
  405647:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40564b:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  405650:	45 31 c0             	xor    %r8d,%r8d
  405653:	f6 c3 40             	test   $0x40,%bl
  405656:	4d 0f 45 ca          	cmovne %r10,%r9
  40565a:	4d 0f 45 d0          	cmovne %r8,%r10
  40565e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405665:	48 83 d8 00          	sbb    $0x0,%rax
  405669:	4c 89 c0             	mov    %r8,%rax
  40566c:	49 0f 42 c2          	cmovb  %r10,%rax
  405670:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405675:	4c 89 c0             	mov    %r8,%rax
  405678:	49 0f 42 c1          	cmovb  %r9,%rax
  40567c:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  405682:	49 29 fb             	sub    %rdi,%r11
  405685:	4c 89 c7             	mov    %r8,%rdi
  405688:	48 19 cf             	sbb    %rcx,%rdi
  40568b:	45 88 d9             	mov    %r11b,%r9b
  40568e:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  405695:	44 88 c9             	mov    %r9b,%cl
  405698:	4c 89 d3             	mov    %r10,%rbx
  40569b:	48 d3 eb             	shr    %cl,%rbx
  40569e:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  4056a3:	41 f6 c1 40          	test   $0x40,%r9b
  4056a7:	49 89 d9             	mov    %rbx,%r9
  4056aa:	4d 0f 45 c8          	cmovne %r8,%r9
  4056ae:	4c 0f 45 d3          	cmovne %rbx,%r10
  4056b2:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  4056b9:	48 83 df 00          	sbb    $0x0,%rdi
  4056bd:	4c 89 c7             	mov    %r8,%rdi
  4056c0:	49 0f 42 fa          	cmovb  %r10,%rdi
  4056c4:	4d 0f 42 c1          	cmovb  %r9,%r8
  4056c8:	4c 21 c6             	and    %r8,%rsi
  4056cb:	48 21 fa             	and    %rdi,%rdx
  4056ce:	48 09 f2             	or     %rsi,%rdx
  4056d1:	0f 95 c2             	setne  %dl
  4056d4:	0f b6 d2             	movzbl %dl,%edx
  4056d7:	48 09 d0             	or     %rdx,%rax
  4056da:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4056df:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4056e4:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4056e9:	89 c1                	mov    %eax,%ecx
  4056eb:	83 e1 04             	and    $0x4,%ecx
  4056ee:	c1 e9 02             	shr    $0x2,%ecx
  4056f1:	48 09 c8             	or     %rcx,%rax
  4056f4:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4056f9:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4056fe:	48 83 c0 01          	add    $0x1,%rax
  405702:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  405708:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40570d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405712:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405717:	48 89 c8             	mov    %rcx,%rax
  40571a:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40571f:	48 c1 f9 02          	sar    $0x2,%rcx
  405723:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405728:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40572d:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  405731:	24 20                	and    $0x20,%al
  405733:	c0 e8 05             	shr    $0x5,%al
  405736:	24 01                	and    $0x1,%al
  405738:	3c 00                	cmp    $0x0,%al
  40573a:	74 2a                	je     405766 <__floattidf+0x2b6>
  40573c:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405741:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405746:	48 89 c8             	mov    %rcx,%rax
  405749:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40574e:	48 d1 f9             	sar    $1,%rcx
  405751:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405756:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40575b:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40575f:	83 c0 01             	add    $0x1,%eax
  405762:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405766:	eb 5c                	jmp    4057c4 <__floattidf+0x314>
  405768:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  40576c:	b9 35 00 00 00       	mov    $0x35,%ecx
  405771:	29 c1                	sub    %eax,%ecx
  405773:	83 e1 7f             	and    $0x7f,%ecx
  405776:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  40577a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40577f:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  405784:	40 88 cf             	mov    %cl,%dil
  405787:	40 88 f9             	mov    %dil,%cl
  40578a:	48 89 c2             	mov    %rax,%rdx
  40578d:	48 d3 e2             	shl    %cl,%rdx
  405790:	40 88 f9             	mov    %dil,%cl
  405793:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  405797:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  40579b:	31 c0                	xor    %eax,%eax
  40579d:	40 f6 c7 40          	test   $0x40,%dil
  4057a1:	48 0f 45 f2          	cmovne %rdx,%rsi
  4057a5:	48 0f 45 d0          	cmovne %rax,%rdx
  4057a9:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  4057af:	48 89 c1             	mov    %rax,%rcx
  4057b2:	48 0f 42 ce          	cmovb  %rsi,%rcx
  4057b6:	48 0f 42 c2          	cmovb  %rdx,%rax
  4057ba:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4057bf:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4057c4:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  4057cb:	00 00 
  4057cd:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4057d1:	25 00 00 00 80       	and    $0x80000000,%eax
  4057d6:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  4057da:	c1 e1 14             	shl    $0x14,%ecx
  4057dd:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  4057e3:	09 c8                	or     %ecx,%eax
  4057e5:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  4057e9:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  4057ef:	09 c8                	or     %ecx,%eax
  4057f1:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  4057f5:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4057f9:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  4057fd:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405803:	48 83 c4 10          	add    $0x10,%rsp
  405807:	5b                   	pop    %rbx
  405808:	c3                   	ret
  405809:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000405810 <__floattidf_unsigned>:
  405810:	53                   	push   %rbx
  405811:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405816:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40581b:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405820:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405825:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40582a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40582f:	48 09 c8             	or     %rcx,%rax
  405832:	0f 94 c0             	sete   %al
  405835:	24 01                	and    $0x1,%al
  405837:	3c 00                	cmp    $0x0,%al
  405839:	74 05                	je     405840 <__floattidf_unsigned+0x30>
  40583b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40583e:	5b                   	pop    %rbx
  40583f:	c3                   	ret
  405840:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405845:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40584a:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40584f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405854:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  405859:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40585e:	48 0f bd c2          	bsr    %rdx,%rax
  405862:	48 83 f0 3f          	xor    $0x3f,%rax
  405866:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40586b:	48 0f bd ce          	bsr    %rsi,%rcx
  40586f:	48 83 f1 3f          	xor    $0x3f,%rcx
  405873:	48 83 c1 40          	add    $0x40,%rcx
  405877:	48 85 d2             	test   %rdx,%rdx
  40587a:	48 0f 45 c8          	cmovne %rax,%rcx
  40587e:	31 c0                	xor    %eax,%eax
  405880:	ba 80 00 00 00       	mov    $0x80,%edx
  405885:	48 29 ca             	sub    %rcx,%rdx
  405888:	48 89 c1             	mov    %rax,%rcx
  40588b:	48 19 c9             	sbb    %rcx,%rcx
  40588e:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405893:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405898:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  40589c:	ff c9                	dec    %ecx
  40589e:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  4058a2:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4058a7:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4058ac:	ba 35 00 00 00       	mov    $0x35,%edx
  4058b1:	48 29 f2             	sub    %rsi,%rdx
  4058b4:	48 19 c8             	sbb    %rcx,%rax
  4058b7:	0f 92 c0             	setb   %al
  4058ba:	24 01                	and    $0x1,%al
  4058bc:	3c 00                	cmp    $0x0,%al
  4058be:	0f 84 c0 01 00 00    	je     405a84 <__floattidf_unsigned+0x274>
  4058c4:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4058c9:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  4058ce:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4058d3:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  4058d8:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  4058dd:	0f 28 0d 3c 1a 00 00 	movaps 0x1a3c(%rip),%xmm1        # 407320 <_IO_stdin_used+0x320>
  4058e4:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4058e8:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  4058ed:	74 17                	je     405906 <__floattidf_unsigned+0xf6>
  4058ef:	eb 00                	jmp    4058f1 <__floattidf_unsigned+0xe1>
  4058f1:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  4058f6:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  4058fb:	48 83 f0 37          	xor    $0x37,%rax
  4058ff:	48 09 c8             	or     %rcx,%rax
  405902:	74 26                	je     40592a <__floattidf_unsigned+0x11a>
  405904:	eb 29                	jmp    40592f <__floattidf_unsigned+0x11f>
  405906:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40590b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405910:	48 89 d0             	mov    %rdx,%rax
  405913:	48 01 c0             	add    %rax,%rax
  405916:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40591b:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405920:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405925:	e9 d6 00 00 00       	jmp    405a00 <__floattidf_unsigned+0x1f0>
  40592a:	e9 d1 00 00 00       	jmp    405a00 <__floattidf_unsigned+0x1f0>
  40592f:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405934:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  405939:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  40593e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405943:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405948:	49 89 fb             	mov    %rdi,%r11
  40594b:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  40594f:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405953:	44 88 db             	mov    %r11b,%bl
  405956:	88 d9                	mov    %bl,%cl
  405958:	49 89 f2             	mov    %rsi,%r10
  40595b:	49 d3 ea             	shr    %cl,%r10
  40595e:	88 d9                	mov    %bl,%cl
  405960:	49 89 d1             	mov    %rdx,%r9
  405963:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  405967:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40596c:	45 31 c0             	xor    %r8d,%r8d
  40596f:	f6 c3 40             	test   $0x40,%bl
  405972:	4d 0f 45 ca          	cmovne %r10,%r9
  405976:	4d 0f 45 d0          	cmovne %r8,%r10
  40597a:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405981:	48 83 d8 00          	sbb    $0x0,%rax
  405985:	4c 89 c0             	mov    %r8,%rax
  405988:	49 0f 42 c2          	cmovb  %r10,%rax
  40598c:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405991:	4c 89 c0             	mov    %r8,%rax
  405994:	49 0f 42 c1          	cmovb  %r9,%rax
  405998:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  40599e:	49 29 fb             	sub    %rdi,%r11
  4059a1:	4c 89 c7             	mov    %r8,%rdi
  4059a4:	48 19 cf             	sbb    %rcx,%rdi
  4059a7:	45 88 d9             	mov    %r11b,%r9b
  4059aa:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  4059b1:	44 88 c9             	mov    %r9b,%cl
  4059b4:	4c 89 d3             	mov    %r10,%rbx
  4059b7:	48 d3 eb             	shr    %cl,%rbx
  4059ba:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  4059bf:	41 f6 c1 40          	test   $0x40,%r9b
  4059c3:	49 89 d9             	mov    %rbx,%r9
  4059c6:	4d 0f 45 c8          	cmovne %r8,%r9
  4059ca:	4c 0f 45 d3          	cmovne %rbx,%r10
  4059ce:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  4059d5:	48 83 df 00          	sbb    $0x0,%rdi
  4059d9:	4c 89 c7             	mov    %r8,%rdi
  4059dc:	49 0f 42 fa          	cmovb  %r10,%rdi
  4059e0:	4d 0f 42 c1          	cmovb  %r9,%r8
  4059e4:	4c 21 c6             	and    %r8,%rsi
  4059e7:	48 21 fa             	and    %rdi,%rdx
  4059ea:	48 09 f2             	or     %rsi,%rdx
  4059ed:	0f 95 c2             	setne  %dl
  4059f0:	0f b6 d2             	movzbl %dl,%edx
  4059f3:	48 09 d0             	or     %rdx,%rax
  4059f6:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4059fb:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405a00:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405a05:	89 c1                	mov    %eax,%ecx
  405a07:	83 e1 04             	and    $0x4,%ecx
  405a0a:	c1 e9 02             	shr    $0x2,%ecx
  405a0d:	48 09 c8             	or     %rcx,%rax
  405a10:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405a15:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405a1a:	48 83 c0 01          	add    $0x1,%rax
  405a1e:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  405a24:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405a29:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405a2e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405a33:	48 89 c8             	mov    %rcx,%rax
  405a36:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  405a3b:	48 c1 e9 02          	shr    $0x2,%rcx
  405a3f:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405a44:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405a49:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  405a4d:	24 20                	and    $0x20,%al
  405a4f:	c0 e8 05             	shr    $0x5,%al
  405a52:	24 01                	and    $0x1,%al
  405a54:	3c 00                	cmp    $0x0,%al
  405a56:	74 2a                	je     405a82 <__floattidf_unsigned+0x272>
  405a58:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405a5d:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405a62:	48 89 c8             	mov    %rcx,%rax
  405a65:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  405a6a:	48 d1 e9             	shr    $1,%rcx
  405a6d:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405a72:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405a77:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  405a7b:	83 c0 01             	add    $0x1,%eax
  405a7e:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405a82:	eb 6a                	jmp    405aee <__floattidf_unsigned+0x2de>
  405a84:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  405a89:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405a8e:	31 c0                	xor    %eax,%eax
  405a90:	bf 35 00 00 00       	mov    $0x35,%edi
  405a95:	48 29 d7             	sub    %rdx,%rdi
  405a98:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  405a9d:	48 19 c8             	sbb    %rcx,%rax
  405aa0:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  405aa5:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405aaa:	41 88 f8             	mov    %dil,%r8b
  405aad:	44 88 c1             	mov    %r8b,%cl
  405ab0:	4c 89 ce             	mov    %r9,%rsi
  405ab3:	48 d3 e6             	shl    %cl,%rsi
  405ab6:	44 88 c1             	mov    %r8b,%cl
  405ab9:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  405abd:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  405ac2:	41 f6 c0 40          	test   $0x40,%r8b
  405ac6:	48 0f 45 d6          	cmovne %rsi,%rdx
  405aca:	48 0f 45 f1          	cmovne %rcx,%rsi
  405ace:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  405ad5:	48 83 d8 00          	sbb    $0x0,%rax
  405ad9:	48 89 c8             	mov    %rcx,%rax
  405adc:	48 0f 42 c6          	cmovb  %rsi,%rax
  405ae0:	48 0f 42 ca          	cmovb  %rdx,%rcx
  405ae4:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405ae9:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405aee:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  405af5:	00 00 
  405af7:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  405afb:	c1 e2 14             	shl    $0x14,%edx
  405afe:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  405b02:	25 ff ff 0f 00       	and    $0xfffff,%eax
  405b07:	89 c1                	mov    %eax,%ecx
  405b09:	89 d0                	mov    %edx,%eax
  405b0b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  405b12:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405b16:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  405b1a:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  405b1e:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405b24:	5b                   	pop    %rbx
  405b25:	c3                   	ret
  405b26:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  405b2d:	00 00 00 

0000000000405b30 <__umodti3>:
  405b30:	48 83 ec 58          	sub    $0x58,%rsp
  405b34:	48 89 0c 24          	mov    %rcx,(%rsp)
  405b38:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405b3d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405b42:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405b47:	48 8b 0c 24          	mov    (%rsp),%rcx
  405b4b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  405b50:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405b55:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405b5a:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  405b5f:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  405b64:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  405b69:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405b6e:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  405b73:	e8 78 b6 ff ff       	call   4011f0 <runtime::udivmod128>
  405b78:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405b7d:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  405b82:	48 83 c4 58          	add    $0x58,%rsp
  405b86:	c3                   	ret
  405b87:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405b8e:	00 00 

0000000000405b90 <__udivmodti4>:
  405b90:	48 83 ec 58          	sub    $0x58,%rsp
  405b94:	4c 89 04 24          	mov    %r8,(%rsp)
  405b98:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405b9d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405ba2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405ba7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  405bac:	4c 8b 04 24          	mov    (%rsp),%r8
  405bb0:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405bb5:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405bba:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405bbf:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405bc4:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  405bc9:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  405bce:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  405bd3:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405bd8:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  405bdd:	e8 0e b6 ff ff       	call   4011f0 <runtime::udivmod128>
  405be2:	48 83 c4 58          	add    $0x58,%rsp
  405be6:	c3                   	ret
  405be7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405bee:	00 00 

0000000000405bf0 <__udivti3>:
  405bf0:	48 83 ec 48          	sub    $0x48,%rsp
  405bf4:	48 89 0c 24          	mov    %rcx,(%rsp)
  405bf8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405bfd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405c02:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405c07:	48 8b 0c 24          	mov    (%rsp),%rcx
  405c0b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  405c10:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405c15:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405c1a:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  405c1f:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405c24:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  405c29:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405c2e:	31 c0                	xor    %eax,%eax
  405c30:	41 89 c0             	mov    %eax,%r8d
  405c33:	e8 58 ff ff ff       	call   405b90 <__udivmodti4>
  405c38:	48 83 c4 48          	add    $0x48,%rsp
  405c3c:	c3                   	ret
  405c3d:	0f 1f 00             	nopl   (%rax)

0000000000405c40 <runtime::assert>:
  405c40:	48 83 ec 48          	sub    $0x48,%rsp
  405c44:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  405c49:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  405c4e:	40 88 f8             	mov    %dil,%al
  405c51:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  405c55:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405c5a:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  405c5f:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  405c63:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  405c68:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  405c6d:	88 44 24 47          	mov    %al,0x47(%rsp)
  405c71:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  405c76:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405c7b:	3c 00                	cmp    $0x0,%al
  405c7d:	75 19                	jne    405c98 <runtime::assert+0x58>
  405c7f:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405c84:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405c89:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405c8e:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  405c93:	e8 18 0a 00 00       	call   4066b0 <runtime::assert.internal-0>
  405c98:	48 83 c4 48          	add    $0x48,%rsp
  405c9c:	c3                   	ret
  405c9d:	0f 1f 00             	nopl   (%rax)

0000000000405ca0 <runtime::heap_allocator_proc.aligned_alloc-0>:
  405ca0:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  405ca7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  405cac:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  405cb1:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405cb6:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  405cbb:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  405cc0:	44 88 c0             	mov    %r8b,%al
  405cc3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  405cc7:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  405cce:	00 
  405ccf:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  405cd4:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405cd9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405cde:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  405ce3:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  405ce7:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405cec:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  405cf3:	00 
  405cf4:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  405cfb:	00 
  405cfc:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  405d03:	00 
  405d04:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  405d0b:	00 
  405d0c:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  405d13:	b9 08 00 00 00       	mov    $0x8,%ecx
  405d18:	48 83 fe 08          	cmp    $0x8,%rsi
  405d1c:	48 0f 4f ce          	cmovg  %rsi,%rcx
  405d20:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  405d27:	00 
  405d28:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405d2f:	00 
  405d30:	48 83 e9 01          	sub    $0x1,%rcx
  405d34:	48 83 c1 08          	add    $0x8,%rcx
  405d38:	48 01 d1             	add    %rdx,%rcx
  405d3b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  405d42:	00 
  405d43:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  405d4a:	00 00 
  405d4c:	48 83 f8 00          	cmp    $0x0,%rax
  405d50:	0f 95 c1             	setne  %cl
  405d53:	80 e1 01             	and    $0x1,%cl
  405d56:	31 c0                	xor    %eax,%eax
  405d58:	80 f9 00             	cmp    $0x0,%cl
  405d5b:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405d5f:	74 17                	je     405d78 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  405d61:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  405d66:	48 83 f8 08          	cmp    $0x8,%rax
  405d6a:	0f 9f c0             	setg   %al
  405d6d:	24 01                	and    $0x1,%al
  405d6f:	3c 00                	cmp    $0x0,%al
  405d71:	0f 95 c0             	setne  %al
  405d74:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405d78:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405d7d:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  405d81:	80 e1 01             	and    $0x1,%cl
  405d84:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  405d88:	48 83 f8 00          	cmp    $0x0,%rax
  405d8c:	0f 95 c0             	setne  %al
  405d8f:	24 01                	and    $0x1,%al
  405d91:	3c 00                	cmp    $0x0,%al
  405d93:	74 2e                	je     405dc3 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  405d95:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  405d9a:	75 27                	jne    405dc3 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  405d9c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405da1:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  405da5:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  405daa:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  405daf:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  405db6:	00 
  405db7:	e8 64 db ff ff       	call   403920 <runtime::heap_resize>
  405dbc:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  405dc1:	eb 19                	jmp    405ddc <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  405dc3:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  405dc7:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  405dce:	00 
  405dcf:	0f b6 f0             	movzbl %al,%esi
  405dd2:	e8 19 db ff ff       	call   4038f0 <runtime::heap_alloc>
  405dd7:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  405ddc:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  405de1:	48 83 c0 08          	add    $0x8,%rax
  405de5:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  405dea:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  405def:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405df4:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  405df9:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  405dfe:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  405e03:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  405e0a:	00 
  405e0b:	48 83 e8 01          	sub    $0x1,%rax
  405e0f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405e16:	00 
  405e17:	48 83 e9 01          	sub    $0x1,%rcx
  405e1b:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  405e1f:	48 21 c8             	and    %rcx,%rax
  405e22:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  405e27:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  405e2d:	0f 94 c0             	sete   %al
  405e30:	24 01                	and    $0x1,%al
  405e32:	3c 00                	cmp    $0x0,%al
  405e34:	74 3c                	je     405e72 <runtime::heap_allocator_proc.aligned_alloc-0+0x1d2>
  405e36:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405e3b:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405e40:	e8 ab 00 00 00       	call   405ef0 <runtime::heap_allocator_proc.aligned_free-1>
  405e45:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405e4a:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  405e4f:	e8 9c 00 00 00       	call   405ef0 <runtime::heap_allocator_proc.aligned_free-1>
  405e54:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405e59:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405e60:	00 
  405e61:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  405e68:	b0 01                	mov    $0x1,%al
  405e6a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  405e71:	c3                   	ret
  405e72:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405e77:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405e7c:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  405e81:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  405e86:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  405e8a:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  405e8f:	74 2f                	je     405ec0 <runtime::heap_allocator_proc.aligned_alloc-0+0x220>
  405e91:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405e96:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  405e9b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405ea0:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  405ea5:	48 39 d0             	cmp    %rdx,%rax
  405ea8:	48 0f 4c d0          	cmovl  %rax,%rdx
  405eac:	e8 3f d0 ff ff       	call   402ef0 <runtime::mem_copy_non_overlapping>
  405eb1:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405eb6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405ebb:	e8 30 00 00 00       	call   405ef0 <runtime::heap_allocator_proc.aligned_free-1>
  405ec0:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  405ec5:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  405eca:	e8 c1 bf ff ff       	call   401e90 <runtime::[internal.odin]::byte_slice>
  405ecf:	48 89 c1             	mov    %rax,%rcx
  405ed2:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405ed7:	48 89 50 08          	mov    %rdx,0x8(%rax)
  405edb:	48 89 08             	mov    %rcx,(%rax)
  405ede:	31 c0                	xor    %eax,%eax
  405ee0:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  405ee7:	c3                   	ret
  405ee8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  405eef:	00 

0000000000405ef0 <runtime::heap_allocator_proc.aligned_free-1>:
  405ef0:	48 83 ec 18          	sub    $0x18,%rsp
  405ef4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  405ef9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405efe:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  405f03:	48 83 f8 00          	cmp    $0x0,%rax
  405f07:	0f 95 c0             	setne  %al
  405f0a:	24 01                	and    $0x1,%al
  405f0c:	3c 00                	cmp    $0x0,%al
  405f0e:	74 0e                	je     405f1e <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  405f10:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405f15:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  405f19:	e8 32 da ff ff       	call   403950 <runtime::heap_free>
  405f1e:	48 83 c4 18          	add    $0x18,%rsp
  405f22:	c3                   	ret
  405f23:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  405f2a:	84 00 00 00 00 00 

0000000000405f30 <runtime::heap_allocator_proc.aligned_resize-2>:
  405f30:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  405f37:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  405f3c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405f41:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  405f46:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  405f4b:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  405f50:	44 88 c0             	mov    %r8b,%al
  405f53:	88 44 24 57          	mov    %al,0x57(%rsp)
  405f57:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  405f5e:	00 
  405f5f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405f64:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405f69:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  405f6d:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405f72:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  405f77:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  405f7c:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  405f83:	00 
  405f84:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  405f8b:	00 
  405f8c:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  405f93:	00 
  405f94:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  405f9b:	00 
  405f9c:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  405fa3:	0f 57 c0             	xorps  %xmm0,%xmm0
  405fa6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  405fad:	00 
  405fae:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  405fb5:	00 
  405fb6:	48 83 f8 00          	cmp    $0x0,%rax
  405fba:	0f 94 c0             	sete   %al
  405fbd:	24 01                	and    $0x1,%al
  405fbf:	3c 00                	cmp    $0x0,%al
  405fc1:	0f 84 80 00 00 00    	je     406047 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  405fc7:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  405fcc:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  405fd1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  405fd6:	8a 44 24 57          	mov    0x57(%rsp),%al
  405fda:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  405fdf:	0f 57 c0             	xorps  %xmm0,%xmm0
  405fe2:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  405fe9:	00 
  405fea:	48 89 e2             	mov    %rsp,%rdx
  405fed:	4c 89 02             	mov    %r8,(%rdx)
  405ff0:	44 0f b6 c0          	movzbl %al,%r8d
  405ff4:	31 c0                	xor    %eax,%eax
  405ff6:	89 c2                	mov    %eax,%edx
  405ff8:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  405fff:	00 
  406000:	e8 9b fc ff ff       	call   405ca0 <runtime::heap_allocator_proc.aligned_alloc-0>
  406005:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40600a:	40 88 c7             	mov    %al,%dil
  40600d:	40 88 f8             	mov    %dil,%al
  406010:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  406017:	00 
  406018:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  40601f:	00 
  406020:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406027:	00 
  406028:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40602f:	00 
  406030:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  406037:	00 
  406038:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40603c:	48 89 11             	mov    %rdx,(%rcx)
  40603f:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406046:	c3                   	ret
  406047:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40604c:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  406051:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406056:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40605b:	8a 44 24 57          	mov    0x57(%rsp),%al
  40605f:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  406064:	0f 57 c0             	xorps  %xmm0,%xmm0
  406067:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40606e:	00 
  40606f:	49 89 e0             	mov    %rsp,%r8
  406072:	4d 89 08             	mov    %r9,(%r8)
  406075:	44 0f b6 c0          	movzbl %al,%r8d
  406079:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  406080:	00 
  406081:	e8 1a fc ff ff       	call   405ca0 <runtime::heap_allocator_proc.aligned_alloc-0>
  406086:	88 44 24 17          	mov    %al,0x17(%rsp)
  40608a:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  406091:	00 
  406092:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406097:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40609e:	00 
  40609f:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4060a4:	3c 00                	cmp    $0x0,%al
  4060a6:	74 4d                	je     4060f5 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  4060a8:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4060ad:	8a 44 24 17          	mov    0x17(%rsp),%al
  4060b1:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4060b8:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4060bf:	00 
  4060c0:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  4060c7:	00 
  4060c8:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  4060cf:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  4060d6:	00 
  4060d7:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4060de:	00 
  4060df:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4060e6:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4060ea:	48 89 11             	mov    %rdx,(%rcx)
  4060ed:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4060f4:	c3                   	ret
  4060f5:	8a 44 24 57          	mov    0x57(%rsp),%al
  4060f9:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4060fe:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406103:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40610a:	00 
  40610b:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  406112:	00 
  406113:	3c 00                	cmp    $0x0,%al
  406115:	0f 84 87 00 00 00    	je     4061a2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  40611b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406120:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  406125:	48 39 c8             	cmp    %rcx,%rax
  406128:	0f 9f c0             	setg   %al
  40612b:	24 01                	and    $0x1,%al
  40612d:	3c 00                	cmp    $0x0,%al
  40612f:	74 71                	je     4061a2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  406131:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  406136:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  40613d:	00 
  40613e:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406143:	48 89 e0             	mov    %rsp,%rax
  406146:	4c 89 08             	mov    %r9,(%rax)
  406149:	bf 97 72 40 00       	mov    $0x407297,%edi
  40614e:	be 30 00 00 00       	mov    $0x30,%esi
  406153:	ba 4b 00 00 00       	mov    $0x4b,%edx
  406158:	b9 25 00 00 00       	mov    $0x25,%ecx
  40615d:	e8 1e ce ff ff       	call   402f80 <runtime::slice_expr_error_lo_hi>
  406162:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406167:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40616c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  406171:	48 89 c6             	mov    %rax,%rsi
  406174:	48 03 b4 24 e0 00 00 	add    0xe0(%rsp),%rsi
  40617b:	00 
  40617c:	48 29 c1             	sub    %rax,%rcx
  40617f:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  406184:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  406189:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40618e:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  406193:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  406198:	48 29 c2             	sub    %rax,%rdx
  40619b:	31 f6                	xor    %esi,%esi
  40619d:	e8 9e ae ff ff       	call   401040 <memset@plt>
  4061a2:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4061a7:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4061ae:	00 
  4061af:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  4061b6:	00 
  4061b7:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  4061be:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  4061c5:	00 
  4061c6:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4061cd:	00 
  4061ce:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4061d5:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4061d9:	48 89 11             	mov    %rdx,(%rcx)
  4061dc:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4061e3:	c3                   	ret
  4061e4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4061eb:	00 00 00 00 00 

00000000004061f0 <runtime::bounds_check_error.handle_error-0>:
  4061f0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4061f7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4061fc:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406201:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406205:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406209:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40620e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406213:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406218:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40621d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406221:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406225:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40622a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40622f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  406234:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40623b:	00 
  40623c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  406240:	89 44 24 70          	mov    %eax,0x70(%rsp)
  406244:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  406249:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40624e:	0f 57 c0             	xorps  %xmm0,%xmm0
  406251:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406256:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40625b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  406262:	00 00 
  406264:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406269:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40626e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  406275:	00 00 
  406277:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40627c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406281:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  406285:	89 44 24 44          	mov    %eax,0x44(%rsp)
  406289:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40628e:	e8 cd da ff ff       	call   403d60 <runtime::print_caller_location>
  406293:	bf c8 72 40 00       	mov    $0x4072c8,%edi
  406298:	be 07 00 00 00       	mov    $0x7,%esi
  40629d:	e8 9e d2 ff ff       	call   403540 <runtime::print_string>
  4062a2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4062a7:	e8 b4 d8 ff ff       	call   403b60 <runtime::print_i64>
  4062ac:	bf 13 72 40 00       	mov    $0x407213,%edi
  4062b1:	be 15 00 00 00       	mov    $0x15,%esi
  4062b6:	e8 85 d2 ff ff       	call   403540 <runtime::print_string>
  4062bb:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4062c0:	e8 9b d8 ff ff       	call   403b60 <runtime::print_i64>
  4062c5:	bf 0a 00 00 00       	mov    $0xa,%edi
  4062ca:	e8 a1 d4 ff ff       	call   403770 <runtime::print_byte>
  4062cf:	e8 ec ae ff ff       	call   4011c0 <runtime::bounds_trap>
  4062d4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4062db:	00 00 00 00 00 

00000000004062e0 <runtime::default_random_generator_proc.read_u64-0>:
  4062e0:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  4062e5:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4062ea:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4062ef:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4062f4:	48 8b 00             	mov    (%rax),%rax
  4062f7:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4062fc:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406301:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  406308:	f4 51 58 
  40630b:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  406311:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406316:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  40631a:	48 83 ca 01          	or     $0x1,%rdx
  40631e:	48 01 d1             	add    %rdx,%rcx
  406321:	48 89 08             	mov    %rcx,(%rax)
  406324:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406329:	48 c1 e9 3b          	shr    $0x3b,%rcx
  40632d:	b2 01                	mov    $0x1,%dl
  40632f:	31 c0                	xor    %eax,%eax
  406331:	f6 c2 01             	test   $0x1,%dl
  406334:	48 0f 45 c1          	cmovne %rcx,%rax
  406338:	48 83 c0 05          	add    $0x5,%rax
  40633c:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  406341:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  406348:	75 f1 ae 
  40634b:	48 0f af c1          	imul   %rcx,%rax
  40634f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406354:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406359:	48 c1 e9 3b          	shr    $0x3b,%rcx
  40635d:	b2 01                	mov    $0x1,%dl
  40635f:	31 c0                	xor    %eax,%eax
  406361:	f6 c2 01             	test   $0x1,%dl
  406364:	48 0f 45 c1          	cmovne %rcx,%rax
  406368:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40636d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406372:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  406377:	48 89 d1             	mov    %rdx,%rcx
  40637a:	48 d3 e8             	shr    %cl,%rax
  40637d:	48 89 c1             	mov    %rax,%rcx
  406380:	31 c0                	xor    %eax,%eax
  406382:	48 83 fa 40          	cmp    $0x40,%rdx
  406386:	48 0f 42 c1          	cmovb  %rcx,%rax
  40638a:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40638f:	31 c9                	xor    %ecx,%ecx
  406391:	89 ce                	mov    %ecx,%esi
  406393:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  406398:	48 83 e6 3f          	and    $0x3f,%rsi
  40639c:	48 89 f1             	mov    %rsi,%rcx
  40639f:	48 d3 e2             	shl    %cl,%rdx
  4063a2:	31 c9                	xor    %ecx,%ecx
  4063a4:	48 83 fe 40          	cmp    $0x40,%rsi
  4063a8:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4063ac:	48 09 c8             	or     %rcx,%rax
  4063af:	c3                   	ret

00000000004063b0 <runtime::default_random_generator_proc.init-1>:
  4063b0:	48 83 ec 28          	sub    $0x28,%rsp
  4063b4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4063b8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4063bd:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4063c2:	48 8b 0c 24          	mov    (%rsp),%rcx
  4063c6:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4063cb:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4063d0:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4063d5:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  4063db:	0f 94 c0             	sete   %al
  4063de:	24 01                	and    $0x1,%al
  4063e0:	3c 00                	cmp    $0x0,%al
  4063e2:	74 0e                	je     4063f2 <runtime::default_random_generator_proc.init-1+0x42>
  4063e4:	0f 31                	rdtsc
  4063e6:	48 c1 e2 20          	shl    $0x20,%rdx
  4063ea:	48 09 d0             	or     %rdx,%rax
  4063ed:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4063f2:	48 8b 3c 24          	mov    (%rsp),%rdi
  4063f6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4063fb:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  406402:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406407:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40640c:	48 d1 e2             	shl    $1,%rdx
  40640f:	40 b6 01             	mov    $0x1,%sil
  406412:	31 c9                	xor    %ecx,%ecx
  406414:	40 f6 c6 01          	test   $0x1,%sil
  406418:	48 0f 45 ca          	cmovne %rdx,%rcx
  40641c:	48 83 c9 01          	or     $0x1,%rcx
  406420:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406424:	e8 b7 fe ff ff       	call   4062e0 <runtime::default_random_generator_proc.read_u64-0>
  406429:	48 8b 3c 24          	mov    (%rsp),%rdi
  40642d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406432:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406437:	48 03 08             	add    (%rax),%rcx
  40643a:	48 89 08             	mov    %rcx,(%rax)
  40643d:	e8 9e fe ff ff       	call   4062e0 <runtime::default_random_generator_proc.read_u64-0>
  406442:	48 83 c4 28          	add    $0x28,%rsp
  406446:	c3                   	ret
  406447:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40644e:	00 00 

0000000000406450 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  406450:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  406455:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  40645a:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40645f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  406464:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  406469:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40646e:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  406475:	00 00 
  406477:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40647c:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  406480:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406485:	48 03 4a 20          	add    0x20(%rdx),%rcx
  406489:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40648e:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406493:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406498:	48 83 e8 01          	sub    $0x1,%rax
  40649c:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4064a1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4064a6:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  4064ab:	48 83 f8 00          	cmp    $0x0,%rax
  4064af:	0f 95 c0             	setne  %al
  4064b2:	24 01                	and    $0x1,%al
  4064b4:	3c 00                	cmp    $0x0,%al
  4064b6:	74 17                	je     4064cf <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  4064b8:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4064bd:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4064c2:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  4064c7:	48 29 c8             	sub    %rcx,%rax
  4064ca:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4064cf:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4064d4:	c3                   	ret
  4064d5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4064dc:	00 00 00 00 

00000000004064e0 <runtime::arena_alloc.align_forward_uint-0>:
  4064e0:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  4064e5:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  4064ea:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4064ef:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4064f4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4064f9:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4064fe:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406503:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406508:	48 83 e9 01          	sub    $0x1,%rcx
  40650c:	48 21 c8             	and    %rcx,%rax
  40650f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406514:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  40651a:	0f 95 c0             	setne  %al
  40651d:	24 01                	and    $0x1,%al
  40651f:	3c 00                	cmp    $0x0,%al
  406521:	74 14                	je     406537 <runtime::arena_alloc.align_forward_uint-0+0x57>
  406523:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406528:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  40652d:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  406532:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406537:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40653c:	c3                   	ret
  40653d:	0f 1f 00             	nopl   (%rax)

0000000000406540 <runtime::matrix_bounds_check_error.handle_error-0>:
  406540:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  406547:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40654c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406551:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406555:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406559:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40655e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406563:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  40656a:	00 
  40656b:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406570:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  406577:	00 
  406578:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40657d:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406582:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  406587:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40658c:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  406591:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406595:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406599:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40659e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4065a3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  4065aa:	00 
  4065ab:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  4065b2:	00 
  4065b3:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  4065ba:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  4065c1:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  4065c8:	00 
  4065c9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  4065d0:	00 
  4065d1:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  4065d6:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  4065db:	0f 57 c0             	xorps  %xmm0,%xmm0
  4065de:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  4065e3:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4065e8:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  4065ef:	00 00 
  4065f1:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  4065f6:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4065fb:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406602:	00 00 
  406604:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  406609:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40660e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  406612:	89 44 24 54          	mov    %eax,0x54(%rsp)
  406616:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  40661b:	e8 40 d7 ff ff       	call   403d60 <runtime::print_caller_location>
  406620:	bf d0 72 40 00       	mov    $0x4072d0,%edi
  406625:	be 11 00 00 00       	mov    $0x11,%esi
  40662a:	e8 11 cf ff ff       	call   403540 <runtime::print_string>
  40662f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406634:	e8 27 d5 ff ff       	call   403b60 <runtime::print_i64>
  406639:	bf e2 72 40 00       	mov    $0x4072e2,%edi
  40663e:	be 02 00 00 00       	mov    $0x2,%esi
  406643:	e8 f8 ce ff ff       	call   403540 <runtime::print_string>
  406648:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40664d:	e8 0e d5 ff ff       	call   403b60 <runtime::print_i64>
  406652:	bf e5 72 40 00       	mov    $0x4072e5,%edi
  406657:	be 16 00 00 00       	mov    $0x16,%esi
  40665c:	e8 df ce ff ff       	call   403540 <runtime::print_string>
  406661:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406666:	e8 f5 d4 ff ff       	call   403b60 <runtime::print_i64>
  40666b:	bf fc 72 40 00       	mov    $0x4072fc,%edi
  406670:	be 06 00 00 00       	mov    $0x6,%esi
  406675:	e8 c6 ce ff ff       	call   403540 <runtime::print_string>
  40667a:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40667f:	e8 dc d4 ff ff       	call   403b60 <runtime::print_i64>
  406684:	bf 03 73 40 00       	mov    $0x407303,%edi
  406689:	be 01 00 00 00       	mov    $0x1,%esi
  40668e:	e8 ad ce ff ff       	call   403540 <runtime::print_string>
  406693:	bf 0a 00 00 00       	mov    $0xa,%edi
  406698:	e8 d3 d0 ff ff       	call   403770 <runtime::print_byte>
  40669d:	e8 1e ab ff ff       	call   4011c0 <runtime::bounds_trap>
  4066a2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4066a9:	1f 84 00 00 00 00 00 

00000000004066b0 <runtime::assert.internal-0>:
  4066b0:	48 83 ec 38          	sub    $0x38,%rsp
  4066b4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4066b8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4066bd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4066c2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4066c7:	48 8b 04 24          	mov    (%rsp),%rax
  4066cb:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4066d0:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4066d5:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4066da:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4066df:	48 8b 40 20          	mov    0x20(%rax),%rax
  4066e3:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4066e8:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  4066ee:	0f 94 c0             	sete   %al
  4066f1:	24 01                	and    $0x1,%al
  4066f3:	3c 00                	cmp    $0x0,%al
  4066f5:	74 0c                	je     406703 <runtime::assert.internal-0+0x53>
  4066f7:	48 c7 c0 f0 4f 40 00 	mov    $0x404ff0,%rax
  4066fe:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406703:	4c 8b 0c 24          	mov    (%rsp),%r9
  406707:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40670c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406711:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406716:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40671b:	bf 05 73 40 00       	mov    $0x407305,%edi
  406720:	be 11 00 00 00       	mov    $0x11,%esi
  406725:	ff d0                	call   *%rax
  406727:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40672e:	00 00 

0000000000406730 <journey::main>:
  406730:	48 83 ec 58          	sub    $0x58,%rsp
  406734:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406739:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40673e:	e8 a3 00 00 00       	call   4067e6 <journey::create_world:proc(indice_bit_capacity:$$900,unique_data_capacity:$$30)->(:journey::World)>
  406743:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  406748:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  40674d:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406752:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  406757:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40675c:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  406761:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  406766:	48 8d 7c 24 48       	lea    0x48(%rsp),%rdi
  40676b:	48 89 3c 24          	mov    %rdi,(%rsp)
  40676f:	e8 11 01 00 00       	call   406885 <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Health::$1,data_storage_index:$$0,indices_capacity:$$100)>
  406774:	48 8b 3c 24          	mov    (%rsp),%rdi
  406778:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40677d:	e8 78 01 00 00       	call   4068fa <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)>
  406782:	48 8b 3c 24          	mov    (%rsp),%rdi
  406786:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40678b:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  406790:	e8 3f 02 00 00       	call   4069d4 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)>
  406795:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40679a:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40679f:	0f 57 c0             	xorps  %xmm0,%xmm0
  4067a2:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  4067a7:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  4067ae:	00 00 
  4067b0:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  4067b7:	00 00 
  4067b9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4067be:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4067c3:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4067c8:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4067cd:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4067d2:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4067d7:	48 8d 7c 24 48       	lea    0x48(%rsp),%rdi
  4067dc:	e8 cd 02 00 00       	call   406aae <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)>
  4067e1:	48 83 c4 58          	add    $0x58,%rsp
  4067e5:	c3                   	ret

00000000004067e6 <journey::create_world:proc(indice_bit_capacity:$$900,unique_data_capacity:$$30)->(:journey::World)>:
  4067e6:	31 c0                	xor    %eax,%eax
  4067e8:	41 89 c1             	mov    %eax,%r9d
  4067eb:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  4067f0:	b8 09 00 00 00       	mov    $0x9,%eax
  4067f5:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4067fa:	be 00 10 00 00       	mov    $0x1000,%esi
  4067ff:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  406804:	ba 03 00 00 00       	mov    $0x3,%edx
  406809:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  40680e:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  406814:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  406819:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  406820:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  406825:	4c 89 cf             	mov    %r9,%rdi
  406828:	0f 05                	syscall
  40682a:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  40682f:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  406834:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  406839:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40683e:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  406843:	48 89 c1             	mov    %rax,%rcx
  406846:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40684b:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  406850:	4c 89 cf             	mov    %r9,%rdi
  406853:	0f 05                	syscall
  406855:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40685a:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40685f:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  406866:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40686b:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  406870:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  406875:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40687a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40687f:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  406884:	c3                   	ret

0000000000406885 <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Health::$1,data_storage_index:$$0,indices_capacity:$$100)>:
  406885:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  40688a:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40688f:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406894:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406899:	48 8b 00             	mov    (%rax),%rax
  40689c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4068a1:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4068a6:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4068ab:	31 c0                	xor    %eax,%eax
  4068ad:	41 89 c1             	mov    %eax,%r9d
  4068b0:	b8 09 00 00 00       	mov    $0x9,%eax
  4068b5:	be 00 10 00 00       	mov    $0x1000,%esi
  4068ba:	ba 03 00 00 00       	mov    $0x3,%edx
  4068bf:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4068c5:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4068cc:	4c 89 cf             	mov    %r9,%rdi
  4068cf:	0f 05                	syscall
  4068d1:	48 89 c1             	mov    %rax,%rcx
  4068d4:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4068d9:	48 89 08             	mov    %rcx,(%rax)
  4068dc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4068e1:	48 c7 40 18 08 00 00 	movq   $0x8,0x18(%rax)
  4068e8:	00 
  4068e9:	48 c7 40 10 c8 00 00 	movq   $0xc8,0x10(%rax)
  4068f0:	00 
  4068f1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4068f8:	00 
  4068f9:	c3                   	ret

00000000004068fa <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)>:
  4068fa:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  4068ff:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406904:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406909:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40690e:	48 8b 40 08          	mov    0x8(%rax),%rax
  406912:	48 8b 00             	mov    (%rax),%rax
  406915:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40691a:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40691f:	48 8b 40 08          	mov    0x8(%rax),%rax
  406923:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406928:	48 c1 e9 06          	shr    $0x6,%rcx
  40692c:	48 c1 e1 03          	shl    $0x3,%rcx
  406930:	48 01 c8             	add    %rcx,%rax
  406933:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  406938:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40693d:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406942:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406947:	48 83 c1 14          	add    $0x14,%rcx
  40694b:	48 83 e1 3f          	and    $0x3f,%rcx
  40694f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  406956:	c4 e2 f0 f5 c0       	bzhi   %rcx,%rax,%rax
  40695b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406960:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406965:	48 c7 00 ff ff ff ff 	movq   $0xffffffffffffffff,(%rax)
  40696c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406971:	48 83 c0 14          	add    $0x14,%rax
  406975:	48 c1 e8 06          	shr    $0x6,%rax
  406979:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40697e:	48 83 c1 00          	add    $0x0,%rcx
  406982:	48 c1 e9 06          	shr    $0x6,%rcx
  406986:	48 29 c8             	sub    %rcx,%rax
  406989:	48 83 f8 00          	cmp    $0x0,%rax
  40698d:	75 0d                	jne    40699c <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)+0xa2>
  40698f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406994:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406999:	48 89 08             	mov    %rcx,(%rax)
  40699c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4069a1:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4069a6:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4069aa:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4069af:	48 8b 40 08          	mov    0x8(%rax),%rax
  4069b3:	48 8b 08             	mov    (%rax),%rcx
  4069b6:	48 83 c1 14          	add    $0x14,%rcx
  4069ba:	48 89 08             	mov    %rcx,(%rax)
  4069bd:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4069c2:	48 83 e8 40          	sub    $0x40,%rax
  4069c6:	48 b9 00 00 00 00 14 	movabs $0x1400000000,%rcx
  4069cd:	00 00 00 
  4069d0:	48 09 c8             	or     %rcx,%rax
  4069d3:	c3                   	ret

00000000004069d4 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)>:
  4069d4:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  4069d9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4069de:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4069e3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4069e8:	48 8b 40 08          	mov    0x8(%rax),%rax
  4069ec:	48 8b 00             	mov    (%rax),%rax
  4069ef:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4069f4:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4069f9:	48 8b 40 08          	mov    0x8(%rax),%rax
  4069fd:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406a02:	48 c1 e9 06          	shr    $0x6,%rcx
  406a06:	48 c1 e1 03          	shl    $0x3,%rcx
  406a0a:	48 01 c8             	add    %rcx,%rax
  406a0d:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  406a12:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406a17:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406a1c:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406a21:	48 83 c1 32          	add    $0x32,%rcx
  406a25:	48 83 e1 3f          	and    $0x3f,%rcx
  406a29:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  406a30:	c4 e2 f0 f5 c0       	bzhi   %rcx,%rax,%rax
  406a35:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406a3a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406a3f:	48 c7 00 ff ff ff ff 	movq   $0xffffffffffffffff,(%rax)
  406a46:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406a4b:	48 83 c0 32          	add    $0x32,%rax
  406a4f:	48 c1 e8 06          	shr    $0x6,%rax
  406a53:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406a58:	48 83 c1 00          	add    $0x0,%rcx
  406a5c:	48 c1 e9 06          	shr    $0x6,%rcx
  406a60:	48 29 c8             	sub    %rcx,%rax
  406a63:	48 83 f8 00          	cmp    $0x0,%rax
  406a67:	75 0d                	jne    406a76 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)+0xa2>
  406a69:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406a6e:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406a73:	48 89 08             	mov    %rcx,(%rax)
  406a76:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406a7b:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406a80:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406a84:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406a89:	48 8b 40 08          	mov    0x8(%rax),%rax
  406a8d:	48 8b 08             	mov    (%rax),%rcx
  406a90:	48 83 c1 32          	add    $0x32,%rcx
  406a94:	48 89 08             	mov    %rcx,(%rax)
  406a97:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406a9c:	48 83 e8 40          	sub    $0x40,%rax
  406aa0:	48 b9 00 00 00 00 32 	movabs $0x3200000000,%rcx
  406aa7:	00 00 00 
  406aaa:	48 09 c8             	or     %rcx,%rax
  406aad:	c3                   	ret

0000000000406aae <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)>:
  406aae:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  406ab3:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  406ab8:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  406abd:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  406ac2:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  406ac7:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  406acc:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  406ad1:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  406ad6:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406adb:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406ae0:	48 8b 00             	mov    (%rax),%rax
  406ae3:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406ae8:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  406aed:	48 8b 00             	mov    (%rax),%rax
  406af0:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406af5:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  406af9:	48 c1 e1 04          	shl    $0x4,%rcx
  406afd:	48 01 c8             	add    %rcx,%rax
  406b00:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  406b05:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  406b0c:	00 00 
  406b0e:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406b13:	48 83 e8 02          	sub    $0x2,%rax
  406b17:	48 83 f8 00          	cmp    $0x0,%rax
  406b1b:	74 4e                	je     406b6b <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)+0xbd>
  406b1d:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406b22:	48 8b 44 c4 e8       	mov    -0x18(%rsp,%rax,8),%rax
  406b27:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  406b2c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406b31:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  406b36:	89 c9                	mov    %ecx,%ecx
  406b38:	48 89 08             	mov    %rcx,(%rax)
  406b3b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406b40:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  406b45:	48 c1 e9 20          	shr    $0x20,%rcx
  406b49:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406b4d:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406b52:	48 83 c0 08          	add    $0x8,%rax
  406b56:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  406b5b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406b60:	48 83 c0 01          	add    $0x1,%rax
  406b64:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  406b69:	eb a3                	jmp    406b0e <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)+0x60>
  406b6b:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  406b70:	48 8b 48 08          	mov    0x8(%rax),%rcx
  406b74:	48 83 c1 02          	add    $0x2,%rcx
  406b78:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406b7c:	c3                   	ret
  406b7d:	0f 1f 00             	nopl   (%rax)

0000000000406b80 <__$startup_runtime>:
  406b80:	eb 00                	jmp    406b82 <__$startup_runtime+0x2>
  406b82:	c3                   	ret
  406b83:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  406b8a:	84 00 00 00 00 00 

0000000000406b90 <__$cleanup_runtime>:
  406b90:	50                   	push   %rax
  406b91:	48 89 3c 24          	mov    %rdi,(%rsp)
  406b95:	eb 00                	jmp    406b97 <__$cleanup_runtime+0x7>
  406b97:	48 8b 3c 24          	mov    (%rsp),%rdi
  406b9b:	e8 f0 a5 ff ff       	call   401190 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  406ba0:	58                   	pop    %rax
  406ba1:	c3                   	ret

Disassembly of section .fini:

0000000000406ba4 <_fini>:
  406ba4:	f3 0f 1e fa          	endbr64
  406ba8:	48 83 ec 08          	sub    $0x8,%rsp
  406bac:	48 83 c4 08          	add    $0x8,%rsp
  406bb0:	c3                   	ret
