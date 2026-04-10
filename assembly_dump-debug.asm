
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
  4010b8:	48 c7 c7 30 51 40 00 	mov    $0x405130,%rdi
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

0000000000401190 <runtime::udivmod128>:
  401190:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  401197:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40119c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4011a1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4011a6:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4011ab:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  4011b0:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4011b5:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4011ba:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4011bf:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4011c4:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4011c9:	48 89 94 24 20 01 00 	mov    %rdx,0x120(%rsp)
  4011d0:	00 
  4011d1:	48 89 b4 24 28 01 00 	mov    %rsi,0x128(%rsp)
  4011d8:	00 
  4011d9:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  4011e0:	00 
  4011e1:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4011e8:	00 
  4011e9:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  4011f0:	00 
  4011f1:	48 89 b4 24 f8 00 00 	mov    %rsi,0xf8(%rsp)
  4011f8:	00 
  4011f9:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  401200:	00 
  401201:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  401208:	00 
  401209:	48 8b b4 24 f8 00 00 	mov    0xf8(%rsp),%rsi
  401210:	00 
  401211:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  401218:	00 
  401219:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  401220:	00 
  401221:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  401228:	00 
  401229:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  401230:	00 
  401231:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  401238:	00 
  401239:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  401240:	00 
  401241:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  401248:	00 
  401249:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  401250:	00 
  401251:	0f 57 c0             	xorps  %xmm0,%xmm0
  401254:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  40125b:	00 
  40125c:	0f 57 c0             	xorps  %xmm0,%xmm0
  40125f:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  401266:	00 
  401267:	c7 84 24 9c 00 00 00 	movl   $0x0,0x9c(%rsp)
  40126e:	00 00 00 00 
  401272:	48 83 bc 24 e8 00 00 	cmpq   $0x0,0xe8(%rsp)
  401279:	00 00 
  40127b:	0f 94 c0             	sete   %al
  40127e:	24 01                	and    $0x1,%al
  401280:	3c 00                	cmp    $0x0,%al
  401282:	0f 84 05 01 00 00    	je     40138d <runtime::udivmod128+0x1fd>
  401288:	48 83 bc 24 c8 00 00 	cmpq   $0x0,0xc8(%rsp)
  40128f:	00 00 
  401291:	0f 94 c0             	sete   %al
  401294:	24 01                	and    $0x1,%al
  401296:	3c 00                	cmp    $0x0,%al
  401298:	0f 84 b6 00 00 00    	je     401354 <runtime::udivmod128+0x1c4>
  40129e:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4012a3:	48 83 f8 00          	cmp    $0x0,%rax
  4012a7:	0f 95 c0             	setne  %al
  4012aa:	24 01                	and    $0x1,%al
  4012ac:	3c 00                	cmp    $0x0,%al
  4012ae:	74 5d                	je     40130d <runtime::udivmod128+0x17d>
  4012b0:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4012b7:	00 
  4012b8:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4012bd:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4012c4:	00 
  4012c5:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4012ca:	48 83 f8 00          	cmp    $0x0,%rax
  4012ce:	74 16                	je     4012e6 <runtime::udivmod128+0x156>
  4012d0:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4012d5:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4012da:	31 d2                	xor    %edx,%edx
  4012dc:	48 f7 f1             	div    %rcx
  4012df:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4012e4:	eb 02                	jmp    4012e8 <runtime::udivmod128+0x158>
  4012e6:	0f 0b                	ud2
  4012e8:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4012ed:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4012f2:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  4012f9:	00 
  4012fa:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  401301:	00 
  401302:	48 89 08             	mov    %rcx,(%rax)
  401305:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40130c:	00 
  40130d:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401314:	00 
  401315:	48 89 04 24          	mov    %rax,(%rsp)
  401319:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  401320:	00 
  401321:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  401326:	48 83 f8 00          	cmp    $0x0,%rax
  40132a:	74 15                	je     401341 <runtime::udivmod128+0x1b1>
  40132c:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401331:	48 8b 04 24          	mov    (%rsp),%rax
  401335:	31 d2                	xor    %edx,%edx
  401337:	48 f7 f1             	div    %rcx
  40133a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40133f:	eb 02                	jmp    401343 <runtime::udivmod128+0x1b3>
  401341:	0f 0b                	ud2
  401343:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401348:	31 c9                	xor    %ecx,%ecx
  40134a:	89 ca                	mov    %ecx,%edx
  40134c:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401353:	c3                   	ret
  401354:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401359:	48 83 f8 00          	cmp    $0x0,%rax
  40135d:	0f 95 c0             	setne  %al
  401360:	24 01                	and    $0x1,%al
  401362:	3c 00                	cmp    $0x0,%al
  401364:	74 18                	je     40137e <runtime::udivmod128+0x1ee>
  401366:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40136b:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  401372:	00 
  401373:	48 89 08             	mov    %rcx,(%rax)
  401376:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40137d:	00 
  40137e:	31 c0                	xor    %eax,%eax
  401380:	89 c2                	mov    %eax,%edx
  401382:	48 89 d0             	mov    %rdx,%rax
  401385:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40138c:	c3                   	ret
  40138d:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  401394:	00 00 
  401396:	0f 94 c0             	sete   %al
  401399:	24 01                	and    $0x1,%al
  40139b:	3c 00                	cmp    $0x0,%al
  40139d:	0f 84 bd 03 00 00    	je     401760 <runtime::udivmod128+0x5d0>
  4013a3:	48 83 bc 24 c8 00 00 	cmpq   $0x0,0xc8(%rsp)
  4013aa:	00 00 
  4013ac:	0f 94 c0             	sete   %al
  4013af:	24 01                	and    $0x1,%al
  4013b1:	3c 00                	cmp    $0x0,%al
  4013b3:	0f 84 a8 00 00 00    	je     401461 <runtime::udivmod128+0x2d1>
  4013b9:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4013be:	48 83 f8 00          	cmp    $0x0,%rax
  4013c2:	0f 95 c0             	setne  %al
  4013c5:	24 01                	and    $0x1,%al
  4013c7:	3c 00                	cmp    $0x0,%al
  4013c9:	74 4d                	je     401418 <runtime::udivmod128+0x288>
  4013cb:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4013d2:	00 
  4013d3:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4013d8:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4013df:	00 
  4013e0:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4013e5:	48 83 f8 00          	cmp    $0x0,%rax
  4013e9:	74 16                	je     401401 <runtime::udivmod128+0x271>
  4013eb:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4013f0:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4013f5:	31 d2                	xor    %edx,%edx
  4013f7:	48 f7 f1             	div    %rcx
  4013fa:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  4013ff:	eb 02                	jmp    401403 <runtime::udivmod128+0x273>
  401401:	0f 0b                	ud2
  401403:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401408:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40140d:	48 89 08             	mov    %rcx,(%rax)
  401410:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401417:	00 
  401418:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40141f:	00 
  401420:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  401425:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40142c:	00 
  40142d:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401432:	48 83 f8 00          	cmp    $0x0,%rax
  401436:	74 16                	je     40144e <runtime::udivmod128+0x2be>
  401438:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40143d:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401442:	31 d2                	xor    %edx,%edx
  401444:	48 f7 f1             	div    %rcx
  401447:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40144c:	eb 02                	jmp    401450 <runtime::udivmod128+0x2c0>
  40144e:	0f 0b                	ud2
  401450:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401455:	31 c9                	xor    %ecx,%ecx
  401457:	89 ca                	mov    %ecx,%edx
  401459:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401460:	c3                   	ret
  401461:	48 83 bc 24 e0 00 00 	cmpq   $0x0,0xe0(%rsp)
  401468:	00 00 
  40146a:	0f 94 c0             	sete   %al
  40146d:	24 01                	and    $0x1,%al
  40146f:	3c 00                	cmp    $0x0,%al
  401471:	0f 84 d9 00 00 00    	je     401550 <runtime::udivmod128+0x3c0>
  401477:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40147c:	48 83 f8 00          	cmp    $0x0,%rax
  401480:	0f 95 c0             	setne  %al
  401483:	24 01                	and    $0x1,%al
  401485:	3c 00                	cmp    $0x0,%al
  401487:	74 7e                	je     401507 <runtime::udivmod128+0x377>
  401489:	48 8d 84 24 a0 00 00 	lea    0xa0(%rsp),%rax
  401490:	00 
  401491:	48 83 c0 08          	add    $0x8,%rax
  401495:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40149a:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4014a1:	00 
  4014a2:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  4014a7:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  4014ae:	00 
  4014af:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4014b4:	48 83 f8 00          	cmp    $0x0,%rax
  4014b8:	74 16                	je     4014d0 <runtime::udivmod128+0x340>
  4014ba:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4014bf:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4014c4:	31 d2                	xor    %edx,%edx
  4014c6:	48 f7 f1             	div    %rcx
  4014c9:	48 89 54 24 a8       	mov    %rdx,-0x58(%rsp)
  4014ce:	eb 02                	jmp    4014d2 <runtime::udivmod128+0x342>
  4014d0:	0f 0b                	ud2
  4014d2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4014d7:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  4014dc:	48 8b 54 24 a8       	mov    -0x58(%rsp),%rdx
  4014e1:	48 89 11             	mov    %rdx,(%rcx)
  4014e4:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  4014eb:	00 00 00 00 00 
  4014f0:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4014f7:	00 
  4014f8:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  4014ff:	00 
  401500:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401504:	48 89 08             	mov    %rcx,(%rax)
  401507:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40150e:	00 
  40150f:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  401514:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  40151b:	00 
  40151c:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  401521:	48 83 f8 00          	cmp    $0x0,%rax
  401525:	74 16                	je     40153d <runtime::udivmod128+0x3ad>
  401527:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  40152c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  401531:	31 d2                	xor    %edx,%edx
  401533:	48 f7 f1             	div    %rcx
  401536:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40153b:	eb 02                	jmp    40153f <runtime::udivmod128+0x3af>
  40153d:	0f 0b                	ud2
  40153f:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  401544:	31 c9                	xor    %ecx,%ecx
  401546:	89 ca                	mov    %ecx,%edx
  401548:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40154f:	c3                   	ret
  401550:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  401557:	00 
  401558:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  40155f:	00 
  401560:	48 83 e9 01          	sub    $0x1,%rcx
  401564:	48 21 c8             	and    %rcx,%rax
  401567:	48 83 f8 00          	cmp    $0x0,%rax
  40156b:	0f 94 c0             	sete   %al
  40156e:	24 01                	and    $0x1,%al
  401570:	3c 00                	cmp    $0x0,%al
  401572:	0f 84 95 00 00 00    	je     40160d <runtime::udivmod128+0x47d>
  401578:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40157d:	48 83 f8 00          	cmp    $0x0,%rax
  401581:	0f 95 c0             	setne  %al
  401584:	24 01                	and    $0x1,%al
  401586:	3c 00                	cmp    $0x0,%al
  401588:	74 4a                	je     4015d4 <runtime::udivmod128+0x444>
  40158a:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40158f:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  401596:	00 
  401597:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  40159e:	00 
  40159f:	48 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%rcx
  4015a6:	00 
  4015a7:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  4015ae:	00 
  4015af:	48 ff ca             	dec    %rdx
  4015b2:	48 21 d1             	and    %rdx,%rcx
  4015b5:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  4015bc:	00 
  4015bd:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4015c4:	00 
  4015c5:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  4015cc:	00 
  4015cd:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4015d1:	48 89 08             	mov    %rcx,(%rax)
  4015d4:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4015db:	00 
  4015dc:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  4015e3:	00 
  4015e4:	ba 40 00 00 00       	mov    $0x40,%edx
  4015e9:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  4015ee:	88 d1                	mov    %dl,%cl
  4015f0:	48 d3 e8             	shr    %cl,%rax
  4015f3:	48 89 c1             	mov    %rax,%rcx
  4015f6:	31 c0                	xor    %eax,%eax
  4015f8:	48 83 ea 40          	sub    $0x40,%rdx
  4015fc:	89 c2                	mov    %eax,%edx
  4015fe:	48 89 d0             	mov    %rdx,%rax
  401601:	48 0f 42 c1          	cmovb  %rcx,%rax
  401605:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40160c:	c3                   	ret
  40160d:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  401614:	00 
  401615:	b8 7f 00 00 00       	mov    $0x7f,%eax
  40161a:	48 0f bd c1          	bsr    %rcx,%rax
  40161e:	48 83 f0 3f          	xor    $0x3f,%rax
  401622:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  401629:	00 
  40162a:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40162f:	48 0f bd ca          	bsr    %rdx,%rcx
  401633:	48 83 f1 3f          	xor    $0x3f,%rcx
  401637:	29 c8                	sub    %ecx,%eax
  401639:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  401640:	83 bc 24 9c 00 00 00 	cmpl   $0x3e,0x9c(%rsp)
  401647:	3e 
  401648:	0f 97 c0             	seta   %al
  40164b:	24 01                	and    $0x1,%al
  40164d:	3c 00                	cmp    $0x0,%al
  40164f:	74 37                	je     401688 <runtime::udivmod128+0x4f8>
  401651:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401656:	48 83 f8 00          	cmp    $0x0,%rax
  40165a:	0f 95 c0             	setne  %al
  40165d:	24 01                	and    $0x1,%al
  40165f:	3c 00                	cmp    $0x0,%al
  401661:	74 16                	je     401679 <runtime::udivmod128+0x4e9>
  401663:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401668:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40166d:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  401672:	48 89 10             	mov    %rdx,(%rax)
  401675:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401679:	31 c0                	xor    %eax,%eax
  40167b:	89 c2                	mov    %eax,%edx
  40167d:	48 89 d0             	mov    %rdx,%rax
  401680:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401687:	c3                   	ret
  401688:	8b 84 24 9c 00 00 00 	mov    0x9c(%rsp),%eax
  40168f:	83 c0 01             	add    $0x1,%eax
  401692:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  401699:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  4016a0:	00 00 00 00 00 
  4016a5:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4016ac:	00 
  4016ad:	b9 40 00 00 00       	mov    $0x40,%ecx
  4016b2:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  4016b9:	89 c9                	mov    %ecx,%ecx
  4016bb:	89 ca                	mov    %ecx,%edx
  4016bd:	48 89 d1             	mov    %rdx,%rcx
  4016c0:	48 d3 e0             	shl    %cl,%rax
  4016c3:	48 89 c1             	mov    %rax,%rcx
  4016c6:	31 c0                	xor    %eax,%eax
  4016c8:	48 83 fa 40          	cmp    $0x40,%rdx
  4016cc:	48 0f 42 c1          	cmovb  %rcx,%rax
  4016d0:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4016d7:	00 
  4016d8:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4016df:	00 
  4016e0:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  4016e7:	89 ca                	mov    %ecx,%edx
  4016e9:	48 89 d1             	mov    %rdx,%rcx
  4016ec:	48 d3 e8             	shr    %cl,%rax
  4016ef:	48 89 c1             	mov    %rax,%rcx
  4016f2:	31 c0                	xor    %eax,%eax
  4016f4:	48 83 fa 40          	cmp    $0x40,%rdx
  4016f8:	48 0f 42 c1          	cmovb  %rcx,%rax
  4016fc:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401703:	00 
  401704:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40170b:	00 
  40170c:	b9 40 00 00 00       	mov    $0x40,%ecx
  401711:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  401718:	89 c9                	mov    %ecx,%ecx
  40171a:	89 ca                	mov    %ecx,%edx
  40171c:	48 89 d1             	mov    %rdx,%rcx
  40171f:	48 d3 e0             	shl    %cl,%rax
  401722:	48 89 c1             	mov    %rax,%rcx
  401725:	31 c0                	xor    %eax,%eax
  401727:	48 83 fa 40          	cmp    $0x40,%rdx
  40172b:	48 0f 42 c1          	cmovb  %rcx,%rax
  40172f:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  401736:	00 
  401737:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  40173e:	89 ce                	mov    %ecx,%esi
  401740:	48 89 f1             	mov    %rsi,%rcx
  401743:	48 d3 ea             	shr    %cl,%rdx
  401746:	31 c9                	xor    %ecx,%ecx
  401748:	48 83 fe 40          	cmp    $0x40,%rsi
  40174c:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401750:	48 09 c8             	or     %rcx,%rax
  401753:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40175a:	00 
  40175b:	e9 14 05 00 00       	jmp    401c74 <runtime::udivmod128+0xae4>
  401760:	48 83 bc 24 c8 00 00 	cmpq   $0x0,0xc8(%rsp)
  401767:	00 00 
  401769:	0f 94 c0             	sete   %al
  40176c:	24 01                	and    $0x1,%al
  40176e:	3c 00                	cmp    $0x0,%al
  401770:	0f 84 6a 03 00 00    	je     401ae0 <runtime::udivmod128+0x950>
  401776:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40177d:	00 
  40177e:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  401785:	00 
  401786:	48 83 e9 01          	sub    $0x1,%rcx
  40178a:	48 21 c8             	and    %rcx,%rax
  40178d:	48 83 f8 00          	cmp    $0x0,%rax
  401791:	0f 94 c0             	sete   %al
  401794:	24 01                	and    $0x1,%al
  401796:	3c 00                	cmp    $0x0,%al
  401798:	0f 84 08 01 00 00    	je     4018a6 <runtime::udivmod128+0x716>
  40179e:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4017a3:	48 83 f8 00          	cmp    $0x0,%rax
  4017a7:	0f 95 c0             	setne  %al
  4017aa:	24 01                	and    $0x1,%al
  4017ac:	3c 00                	cmp    $0x0,%al
  4017ae:	74 26                	je     4017d6 <runtime::udivmod128+0x646>
  4017b0:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4017b5:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  4017bc:	00 
  4017bd:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  4017c4:	00 
  4017c5:	48 ff ca             	dec    %rdx
  4017c8:	48 21 d1             	and    %rdx,%rcx
  4017cb:	48 89 08             	mov    %rcx,(%rax)
  4017ce:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4017d5:	00 
  4017d6:	48 83 bc 24 c0 00 00 	cmpq   $0x1,0xc0(%rsp)
  4017dd:	00 01 
  4017df:	0f 94 c0             	sete   %al
  4017e2:	24 01                	and    $0x1,%al
  4017e4:	3c 00                	cmp    $0x0,%al
  4017e6:	74 12                	je     4017fa <runtime::udivmod128+0x66a>
  4017e8:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  4017ed:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4017f2:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  4017f9:	c3                   	ret
  4017fa:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  401801:	00 
  401802:	b8 40 00 00 00       	mov    $0x40,%eax
  401807:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  40180c:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  401813:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  40181a:	00 
  40181b:	8b 84 24 9c 00 00 00 	mov    0x9c(%rsp),%eax
  401822:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  401826:	88 c1                	mov    %al,%cl
  401828:	48 d3 ea             	shr    %cl,%rdx
  40182b:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  40182f:	31 c0                	xor    %eax,%eax
  401831:	83 e9 40             	sub    $0x40,%ecx
  401834:	48 89 c1             	mov    %rax,%rcx
  401837:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40183b:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  401842:	00 
  401843:	48 8b bc 24 e0 00 00 	mov    0xe0(%rsp),%rdi
  40184a:	00 
  40184b:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  401852:	00 
  401853:	8b b4 24 9c 00 00 00 	mov    0x9c(%rsp),%esi
  40185a:	40 88 f1             	mov    %sil,%cl
  40185d:	48 d3 ef             	shr    %cl,%rdi
  401860:	83 ee 40             	sub    $0x40,%esi
  401863:	48 89 c1             	mov    %rax,%rcx
  401866:	48 0f 42 cf          	cmovb  %rdi,%rcx
  40186a:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  40186f:	f7 de                	neg    %esi
  401871:	40 88 f1             	mov    %sil,%cl
  401874:	48 d3 e2             	shl    %cl,%rdx
  401877:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  40187c:	83 ee 40             	sub    $0x40,%esi
  40187f:	48 0f 42 c2          	cmovb  %rdx,%rax
  401883:	48 09 c8             	or     %rcx,%rax
  401886:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  40188d:	00 
  40188e:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  401895:	00 
  401896:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  40189d:	00 
  40189e:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  4018a5:	c3                   	ret
  4018a6:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4018ad:	00 
  4018ae:	b8 7f 00 00 00       	mov    $0x7f,%eax
  4018b3:	48 0f bd c1          	bsr    %rcx,%rax
  4018b7:	48 83 f0 3f          	xor    $0x3f,%rax
  4018bb:	83 c0 41             	add    $0x41,%eax
  4018be:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4018c5:	00 
  4018c6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  4018cb:	48 0f bd ca          	bsr    %rdx,%rcx
  4018cf:	48 83 f1 3f          	xor    $0x3f,%rcx
  4018d3:	29 c8                	sub    %ecx,%eax
  4018d5:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  4018dc:	83 bc 24 9c 00 00 00 	cmpl   $0x40,0x9c(%rsp)
  4018e3:	40 
  4018e4:	0f 94 c1             	sete   %cl
  4018e7:	80 e1 01             	and    $0x1,%cl
  4018ea:	b0 01                	mov    $0x1,%al
  4018ec:	38 c8                	cmp    %cl,%al
  4018ee:	74 16                	je     401906 <runtime::udivmod128+0x776>
  4018f0:	83 bc 24 9c 00 00 00 	cmpl   $0x40,0x9c(%rsp)
  4018f7:	40 
  4018f8:	0f 92 c1             	setb   %cl
  4018fb:	80 e1 01             	and    $0x1,%cl
  4018fe:	b0 01                	mov    $0x1,%al
  401900:	38 c8                	cmp    %cl,%al
  401902:	74 44                	je     401948 <runtime::udivmod128+0x7b8>
  401904:	eb 3d                	jmp    401943 <runtime::udivmod128+0x7b3>
  401906:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  40190d:	00 00 00 00 00 
  401912:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401919:	00 
  40191a:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  401921:	00 
  401922:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  401929:	00 00 00 00 00 
  40192e:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401935:	00 
  401936:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40193d:	00 
  40193e:	e9 98 01 00 00       	jmp    401adb <runtime::udivmod128+0x94b>
  401943:	e9 c7 00 00 00       	jmp    401a0f <runtime::udivmod128+0x87f>
  401948:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  40194f:	00 00 00 00 00 
  401954:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40195b:	00 
  40195c:	b9 40 00 00 00       	mov    $0x40,%ecx
  401961:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  401968:	89 c9                	mov    %ecx,%ecx
  40196a:	89 ca                	mov    %ecx,%edx
  40196c:	48 89 d1             	mov    %rdx,%rcx
  40196f:	48 d3 e0             	shl    %cl,%rax
  401972:	48 89 c1             	mov    %rax,%rcx
  401975:	31 c0                	xor    %eax,%eax
  401977:	48 83 fa 40          	cmp    $0x40,%rdx
  40197b:	48 0f 42 c1          	cmovb  %rcx,%rax
  40197f:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  401986:	00 
  401987:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40198e:	00 
  40198f:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  401996:	89 ca                	mov    %ecx,%edx
  401998:	48 89 d1             	mov    %rdx,%rcx
  40199b:	48 d3 e8             	shr    %cl,%rax
  40199e:	48 89 c1             	mov    %rax,%rcx
  4019a1:	31 c0                	xor    %eax,%eax
  4019a3:	48 83 fa 40          	cmp    $0x40,%rdx
  4019a7:	48 0f 42 c1          	cmovb  %rcx,%rax
  4019ab:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4019b2:	00 
  4019b3:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4019ba:	00 
  4019bb:	b9 40 00 00 00       	mov    $0x40,%ecx
  4019c0:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  4019c7:	89 c9                	mov    %ecx,%ecx
  4019c9:	89 ca                	mov    %ecx,%edx
  4019cb:	48 89 d1             	mov    %rdx,%rcx
  4019ce:	48 d3 e0             	shl    %cl,%rax
  4019d1:	48 89 c1             	mov    %rax,%rcx
  4019d4:	31 c0                	xor    %eax,%eax
  4019d6:	48 83 fa 40          	cmp    $0x40,%rdx
  4019da:	48 0f 42 c1          	cmovb  %rcx,%rax
  4019de:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4019e5:	00 
  4019e6:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  4019ed:	89 ce                	mov    %ecx,%esi
  4019ef:	48 89 f1             	mov    %rsi,%rcx
  4019f2:	48 d3 ea             	shr    %cl,%rdx
  4019f5:	31 c9                	xor    %ecx,%ecx
  4019f7:	48 83 fe 40          	cmp    $0x40,%rsi
  4019fb:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4019ff:	48 09 c8             	or     %rcx,%rax
  401a02:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401a09:	00 
  401a0a:	e9 cc 00 00 00       	jmp    401adb <runtime::udivmod128+0x94b>
  401a0f:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401a16:	00 
  401a17:	b9 80 00 00 00       	mov    $0x80,%ecx
  401a1c:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  401a23:	89 c9                	mov    %ecx,%ecx
  401a25:	89 ca                	mov    %ecx,%edx
  401a27:	48 89 d1             	mov    %rdx,%rcx
  401a2a:	48 d3 e0             	shl    %cl,%rax
  401a2d:	48 89 c1             	mov    %rax,%rcx
  401a30:	31 c0                	xor    %eax,%eax
  401a32:	48 83 fa 40          	cmp    $0x40,%rdx
  401a36:	48 0f 42 c1          	cmovb  %rcx,%rax
  401a3a:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  401a41:	00 
  401a42:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401a49:	00 
  401a4a:	b9 80 00 00 00       	mov    $0x80,%ecx
  401a4f:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  401a56:	89 c9                	mov    %ecx,%ecx
  401a58:	89 ca                	mov    %ecx,%edx
  401a5a:	48 89 d1             	mov    %rdx,%rcx
  401a5d:	48 d3 e0             	shl    %cl,%rax
  401a60:	48 89 c1             	mov    %rax,%rcx
  401a63:	31 c0                	xor    %eax,%eax
  401a65:	48 83 fa 40          	cmp    $0x40,%rdx
  401a69:	48 0f 42 c1          	cmovb  %rcx,%rax
  401a6d:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  401a74:	00 
  401a75:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  401a7c:	83 e9 40             	sub    $0x40,%ecx
  401a7f:	89 c9                	mov    %ecx,%ecx
  401a81:	89 ce                	mov    %ecx,%esi
  401a83:	48 89 f1             	mov    %rsi,%rcx
  401a86:	48 d3 ea             	shr    %cl,%rdx
  401a89:	31 c9                	xor    %ecx,%ecx
  401a8b:	48 83 fe 40          	cmp    $0x40,%rsi
  401a8f:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401a93:	48 09 c8             	or     %rcx,%rax
  401a96:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  401a9d:	00 
  401a9e:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  401aa5:	00 00 00 00 00 
  401aaa:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401ab1:	00 
  401ab2:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  401ab9:	83 e9 40             	sub    $0x40,%ecx
  401abc:	89 c9                	mov    %ecx,%ecx
  401abe:	89 ca                	mov    %ecx,%edx
  401ac0:	48 89 d1             	mov    %rdx,%rcx
  401ac3:	48 d3 e8             	shr    %cl,%rax
  401ac6:	48 89 c1             	mov    %rax,%rcx
  401ac9:	31 c0                	xor    %eax,%eax
  401acb:	48 83 fa 40          	cmp    $0x40,%rdx
  401acf:	48 0f 42 c1          	cmovb  %rcx,%rax
  401ad3:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401ada:	00 
  401adb:	e9 92 01 00 00       	jmp    401c72 <runtime::udivmod128+0xae2>
  401ae0:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  401ae7:	00 
  401ae8:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401aed:	48 0f bd c1          	bsr    %rcx,%rax
  401af1:	48 83 f0 3f          	xor    $0x3f,%rax
  401af5:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  401afc:	00 
  401afd:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401b02:	48 0f bd ca          	bsr    %rdx,%rcx
  401b06:	48 83 f1 3f          	xor    $0x3f,%rcx
  401b0a:	29 c8                	sub    %ecx,%eax
  401b0c:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  401b13:	83 bc 24 9c 00 00 00 	cmpl   $0x3f,0x9c(%rsp)
  401b1a:	3f 
  401b1b:	0f 97 c0             	seta   %al
  401b1e:	24 01                	and    $0x1,%al
  401b20:	3c 00                	cmp    $0x0,%al
  401b22:	74 37                	je     401b5b <runtime::udivmod128+0x9cb>
  401b24:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401b29:	48 83 f8 00          	cmp    $0x0,%rax
  401b2d:	0f 95 c0             	setne  %al
  401b30:	24 01                	and    $0x1,%al
  401b32:	3c 00                	cmp    $0x0,%al
  401b34:	74 16                	je     401b4c <runtime::udivmod128+0x9bc>
  401b36:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401b3b:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401b40:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  401b45:	48 89 10             	mov    %rdx,(%rax)
  401b48:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401b4c:	31 c0                	xor    %eax,%eax
  401b4e:	89 c2                	mov    %eax,%edx
  401b50:	48 89 d0             	mov    %rdx,%rax
  401b53:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401b5a:	c3                   	ret
  401b5b:	8b 84 24 9c 00 00 00 	mov    0x9c(%rsp),%eax
  401b62:	83 c0 01             	add    $0x1,%eax
  401b65:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  401b6c:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  401b73:	00 00 00 00 00 
  401b78:	83 bc 24 9c 00 00 00 	cmpl   $0x40,0x9c(%rsp)
  401b7f:	40 
  401b80:	0f 94 c0             	sete   %al
  401b83:	24 01                	and    $0x1,%al
  401b85:	3c 00                	cmp    $0x0,%al
  401b87:	74 31                	je     401bba <runtime::udivmod128+0xa2a>
  401b89:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401b90:	00 
  401b91:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  401b98:	00 
  401b99:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  401ba0:	00 00 00 00 00 
  401ba5:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401bac:	00 
  401bad:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401bb4:	00 
  401bb5:	e9 b6 00 00 00       	jmp    401c70 <runtime::udivmod128+0xae0>
  401bba:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401bc1:	00 
  401bc2:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  401bc9:	89 ca                	mov    %ecx,%edx
  401bcb:	48 89 d1             	mov    %rdx,%rcx
  401bce:	48 d3 e8             	shr    %cl,%rax
  401bd1:	48 89 c1             	mov    %rax,%rcx
  401bd4:	31 c0                	xor    %eax,%eax
  401bd6:	48 83 fa 40          	cmp    $0x40,%rdx
  401bda:	48 0f 42 c1          	cmovb  %rcx,%rax
  401bde:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401be5:	00 
  401be6:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  401bed:	00 
  401bee:	b9 40 00 00 00       	mov    $0x40,%ecx
  401bf3:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  401bfa:	89 c9                	mov    %ecx,%ecx
  401bfc:	89 ca                	mov    %ecx,%edx
  401bfe:	48 89 d1             	mov    %rdx,%rcx
  401c01:	48 d3 e0             	shl    %cl,%rax
  401c04:	48 89 c1             	mov    %rax,%rcx
  401c07:	31 c0                	xor    %eax,%eax
  401c09:	48 83 fa 40          	cmp    $0x40,%rdx
  401c0d:	48 0f 42 c1          	cmovb  %rcx,%rax
  401c11:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  401c18:	00 
  401c19:	8b 8c 24 9c 00 00 00 	mov    0x9c(%rsp),%ecx
  401c20:	89 ce                	mov    %ecx,%esi
  401c22:	48 89 f1             	mov    %rsi,%rcx
  401c25:	48 d3 ea             	shr    %cl,%rdx
  401c28:	31 c9                	xor    %ecx,%ecx
  401c2a:	48 83 fe 40          	cmp    $0x40,%rsi
  401c2e:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401c32:	48 09 c8             	or     %rcx,%rax
  401c35:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401c3c:	00 
  401c3d:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401c44:	00 
  401c45:	b9 40 00 00 00       	mov    $0x40,%ecx
  401c4a:	2b 8c 24 9c 00 00 00 	sub    0x9c(%rsp),%ecx
  401c51:	89 c9                	mov    %ecx,%ecx
  401c53:	89 ca                	mov    %ecx,%edx
  401c55:	48 89 d1             	mov    %rdx,%rcx
  401c58:	48 d3 e0             	shl    %cl,%rax
  401c5b:	48 89 c1             	mov    %rax,%rcx
  401c5e:	31 c0                	xor    %eax,%eax
  401c60:	48 83 fa 40          	cmp    $0x40,%rdx
  401c64:	48 0f 42 c1          	cmovb  %rcx,%rax
  401c68:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  401c6f:	00 
  401c70:	eb 00                	jmp    401c72 <runtime::udivmod128+0xae2>
  401c72:	eb 00                	jmp    401c74 <runtime::udivmod128+0xae4>
  401c74:	c7 84 24 8c 00 00 00 	movl   $0x0,0x8c(%rsp)
  401c7b:	00 00 00 00 
  401c7f:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  401c86:	00 00 
  401c88:	48 c7 44 24 70 00 00 	movq   $0x0,0x70(%rsp)
  401c8f:	00 00 
  401c91:	83 bc 24 9c 00 00 00 	cmpl   $0x0,0x9c(%rsp)
  401c98:	00 
  401c99:	0f 97 c0             	seta   %al
  401c9c:	24 01                	and    $0x1,%al
  401c9e:	3c 00                	cmp    $0x0,%al
  401ca0:	0f 84 24 01 00 00    	je     401dca <runtime::udivmod128+0xc3a>
  401ca6:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  401cab:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  401cb0:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  401cb7:	00 
  401cb8:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  401cbf:	00 
  401cc0:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401cc5:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401ccc:	00 
  401ccd:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  401cd4:	00 
  401cd5:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  401cdc:	00 
  401cdd:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401ce2:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401ce9:	00 
  401cea:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  401cf1:	00 
  401cf2:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  401cf9:	00 
  401cfa:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401cff:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  401d06:	00 
  401d07:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  401d0e:	00 
  401d0f:	48 01 c0             	add    %rax,%rax
  401d12:	8b 8c 24 8c 00 00 00 	mov    0x8c(%rsp),%ecx
  401d19:	48 09 c8             	or     %rcx,%rax
  401d1c:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  401d23:	00 
  401d24:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  401d2b:	00 
  401d2c:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  401d33:	00 
  401d34:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  401d39:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  401d3e:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  401d43:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  401d48:	48 f7 d0             	not    %rax
  401d4b:	48 f7 d1             	not    %rcx
  401d4e:	48 01 f1             	add    %rsi,%rcx
  401d51:	48 11 d0             	adc    %rdx,%rax
  401d54:	48 c1 f8 3f          	sar    $0x3f,%rax
  401d58:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  401d5d:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  401d62:	8b 44 24 60          	mov    0x60(%rsp),%eax
  401d66:	83 e0 01             	and    $0x1,%eax
  401d69:	89 84 24 8c 00 00 00 	mov    %eax,0x8c(%rsp)
  401d70:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401d75:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  401d7a:	48 21 ca             	and    %rcx,%rdx
  401d7d:	48 21 c6             	and    %rax,%rsi
  401d80:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  401d85:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  401d8a:	48 29 f1             	sub    %rsi,%rcx
  401d8d:	48 19 d0             	sbb    %rdx,%rax
  401d90:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  401d95:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  401d9a:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  401d9f:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  401da4:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  401dab:	00 
  401dac:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  401db3:	00 
  401db4:	8b 84 24 9c 00 00 00 	mov    0x9c(%rsp),%eax
  401dbb:	83 e8 01             	sub    $0x1,%eax
  401dbe:	89 84 24 9c 00 00 00 	mov    %eax,0x9c(%rsp)
  401dc5:	e9 c7 fe ff ff       	jmp    401c91 <runtime::udivmod128+0xb01>
  401dca:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401dcf:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  401dd6:	00 
  401dd7:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  401dde:	00 
  401ddf:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  401de4:	48 01 c9             	add    %rcx,%rcx
  401de7:	8b b4 24 8c 00 00 00 	mov    0x8c(%rsp),%esi
  401dee:	48 09 f1             	or     %rsi,%rcx
  401df1:	48 89 54 24 58       	mov    %rdx,0x58(%rsp)
  401df6:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  401dfb:	48 83 f8 00          	cmp    $0x0,%rax
  401dff:	0f 95 c0             	setne  %al
  401e02:	24 01                	and    $0x1,%al
  401e04:	3c 00                	cmp    $0x0,%al
  401e06:	74 16                	je     401e1e <runtime::udivmod128+0xc8e>
  401e08:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401e0d:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  401e12:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  401e17:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401e1b:	48 89 08             	mov    %rcx,(%rax)
  401e1e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  401e23:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  401e28:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  401e2f:	c3                   	ret

0000000000401e30 <runtime::memset>:
  401e30:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  401e35:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  401e39:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  401e3e:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401e43:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  401e48:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  401e4c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401e51:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  401e55:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  401e5a:	48 83 f8 00          	cmp    $0x0,%rax
  401e5e:	0f 95 c0             	setne  %al
  401e61:	24 01                	and    $0x1,%al
  401e63:	3c 00                	cmp    $0x0,%al
  401e65:	74 63                	je     401eca <runtime::memset+0x9a>
  401e67:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401e6c:	48 83 f8 00          	cmp    $0x0,%rax
  401e70:	0f 95 c0             	setne  %al
  401e73:	24 01                	and    $0x1,%al
  401e75:	3c 00                	cmp    $0x0,%al
  401e77:	74 51                	je     401eca <runtime::memset+0x9a>
  401e79:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401e7e:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  401e82:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  401e86:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401e8b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  401e92:	00 00 
  401e94:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401e99:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  401e9e:	0f 9c c0             	setl   %al
  401ea1:	24 01                	and    $0x1,%al
  401ea3:	3c 00                	cmp    $0x0,%al
  401ea5:	74 21                	je     401ec8 <runtime::memset+0x98>
  401ea7:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401eac:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  401eb1:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  401eb5:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  401eb8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401ebd:	48 83 c0 01          	add    $0x1,%rax
  401ec1:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  401ec6:	eb cc                	jmp    401e94 <runtime::memset+0x64>
  401ec8:	eb 00                	jmp    401eca <runtime::memset+0x9a>
  401eca:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401ecf:	c3                   	ret

0000000000401ed0 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  401ed0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  401ed5:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  401eda:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  401edf:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  401ee4:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401ee9:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401eee:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  401ef3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401ef8:	48 01 c2             	add    %rax,%rdx
  401efb:	0f 92 c0             	setb   %al
  401efe:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  401f03:	24 01                	and    $0x1,%al
  401f05:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  401f09:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  401f0e:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  401f13:	0f 94 c0             	sete   %al
  401f16:	24 01                	and    $0x1,%al
  401f18:	48 89 11             	mov    %rdx,(%rcx)
  401f1b:	c3                   	ret
  401f1c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401f20 <runtime::memory_block_alloc>:
  401f20:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  401f27:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  401f2c:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  401f31:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  401f36:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  401f3b:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  401f40:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  401f45:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  401f4c:	00 
  401f4d:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  401f52:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  401f57:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  401f5c:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401f61:	48 8b 7c 24 48       	mov    0x48(%rsp),%rdi
  401f66:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  401f6b:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  401f70:	48 89 8c 24 00 01 00 	mov    %rcx,0x100(%rsp)
  401f77:	00 
  401f78:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  401f7f:	00 
  401f80:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401f87:	00 
  401f88:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  401f8d:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  401f94:	00 
  401f95:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  401f9a:	48 89 bc 24 f8 00 00 	mov    %rdi,0xf8(%rsp)
  401fa1:	00 
  401fa2:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  401fa9:	00 
  401faa:	48 c7 84 24 e8 00 00 	movq   $0x0,0xe8(%rsp)
  401fb1:	00 00 00 00 00 
  401fb6:	c6 84 24 e7 00 00 00 	movb   $0x0,0xe7(%rsp)
  401fbd:	00 
  401fbe:	48 89 d6             	mov    %rdx,%rsi
  401fc1:	48 83 ee 31          	sub    $0x31,%rsi
  401fc5:	be 30 00 00 00       	mov    $0x30,%esi
  401fca:	48 0f 43 f2          	cmovae %rdx,%rsi
  401fce:	48 01 f7             	add    %rsi,%rdi
  401fd1:	48 89 bc 24 d8 00 00 	mov    %rdi,0xd8(%rsp)
  401fd8:	00 
  401fd9:	48 89 b4 24 d0 00 00 	mov    %rsi,0xd0(%rsp)
  401fe0:	00 
  401fe1:	48 89 d6             	mov    %rdx,%rsi
  401fe4:	48 83 ee 10          	sub    $0x10,%rsi
  401fe8:	be 10 00 00 00       	mov    $0x10,%esi
  401fed:	48 0f 4c d6          	cmovl  %rsi,%rdx
  401ff1:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  401ff8:	00 
  401ff9:	48 8b bc 24 d8 00 00 	mov    0xd8(%rsp),%rdi
  402000:	00 
  402001:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  402008:	00 
  402009:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  402010:	00 
  402011:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  402018:	00 
  402019:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  402020:	00 
  402021:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  402028:	00 
  402029:	0f 57 c0             	xorps  %xmm0,%xmm0
  40202c:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402033:	00 
  402034:	48 89 e0             	mov    %rsp,%rax
  402037:	4c 89 08             	mov    %r9,(%rax)
  40203a:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  402041:	00 
  402042:	e8 09 3d 00 00       	call   405d50 <runtime::mem_alloc>
  402047:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  40204b:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  402052:	00 
  402053:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402058:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  40205f:	00 
  402060:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  402065:	3c 00                	cmp    $0x0,%al
  402067:	74 39                	je     4020a2 <runtime::memory_block_alloc+0x182>
  402069:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40206e:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  402072:	88 84 24 e7 00 00 00 	mov    %al,0xe7(%rsp)
  402079:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  402080:	00 
  402081:	8a 84 24 e7 00 00 00 	mov    0xe7(%rsp),%al
  402088:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  40208f:	00 
  402090:	88 84 24 e7 00 00 00 	mov    %al,0xe7(%rsp)
  402097:	48 89 11             	mov    %rdx,(%rcx)
  40209a:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4020a1:	c3                   	ret
  4020a2:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  4020a7:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4020ac:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4020b1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4020b6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4020bb:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4020c0:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4020c7:	00 
  4020c8:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4020cd:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  4020d4:	00 
  4020d5:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4020da:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  4020e1:	00 
  4020e2:	48 01 f0             	add    %rsi,%rax
  4020e5:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4020ea:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4020ef:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4020f4:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4020fb:	00 
  4020fc:	48 89 50 10          	mov    %rdx,0x10(%rax)
  402100:	48 89 48 08          	mov    %rcx,0x8(%rax)
  402104:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40210b:	00 
  40210c:	48 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%rcx
  402113:	00 
  402114:	48 03 8c 24 d0 00 00 	add    0xd0(%rsp),%rcx
  40211b:	00 
  40211c:	48 89 48 18          	mov    %rcx,0x18(%rax)
  402120:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  402127:	00 
  402128:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40212d:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  402134:	00 
  402135:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  402139:	48 29 d1             	sub    %rdx,%rcx
  40213c:	48 89 48 28          	mov    %rcx,0x28(%rax)
  402140:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  402147:	00 
  402148:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  40214d:	0f 94 c0             	sete   %al
  402150:	24 01                	and    $0x1,%al
  402152:	0f b6 f8             	movzbl %al,%edi
  402155:	be 50 90 40 00       	mov    $0x409050,%esi
  40215a:	b9 b0 90 40 00       	mov    $0x4090b0,%ecx
  40215f:	ba 0f 00 00 00       	mov    $0xf,%edx
  402164:	e8 87 5d 00 00       	call   407ef0 <runtime::assert>
  402169:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  40216e:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  402175:	00 
  402176:	48 83 38 00          	cmpq   $0x0,(%rax)
  40217a:	0f 94 c0             	sete   %al
  40217d:	24 01                	and    $0x1,%al
  40217f:	0f b6 f8             	movzbl %al,%edi
  402182:	be d8 90 40 00       	mov    $0x4090d8,%esi
  402187:	b9 f0 90 40 00       	mov    $0x4090f0,%ecx
  40218c:	ba 11 00 00 00       	mov    $0x11,%edx
  402191:	e8 5a 5d 00 00       	call   407ef0 <runtime::assert>
  402196:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40219b:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4021a2:	00 
  4021a3:	8a 84 24 e7 00 00 00 	mov    0xe7(%rsp),%al
  4021aa:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  4021b1:	00 
  4021b2:	88 84 24 e7 00 00 00 	mov    %al,0xe7(%rsp)
  4021b9:	48 89 11             	mov    %rdx,(%rcx)
  4021bc:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4021c3:	c3                   	ret
  4021c4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4021cb:	00 00 00 00 00 

00000000004021d0 <runtime::memory_block_dealloc>:
  4021d0:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  4021d7:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4021dc:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4021e1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4021e6:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4021ed:	00 
  4021ee:	48 83 f8 00          	cmp    $0x0,%rax
  4021f2:	0f 95 c0             	setne  %al
  4021f5:	24 01                	and    $0x1,%al
  4021f7:	3c 00                	cmp    $0x0,%al
  4021f9:	0f 84 5f 01 00 00    	je     40235e <runtime::memory_block_dealloc+0x18e>
  4021ff:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  402206:	00 
  402207:	48 8b 41 08          	mov    0x8(%rcx),%rax
  40220b:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  40220f:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  402216:	00 
  402217:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40221e:	00 
  40221f:	0f 57 c0             	xorps  %xmm0,%xmm0
  402222:	0f 29 04 24          	movaps %xmm0,(%rsp)
  402226:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40222d:	00 
  40222e:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  402235:	00 
  402236:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  40223d:	00 
  40223e:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  402245:	00 
  402246:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  40224d:	00 
  40224e:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  402255:	00 
  402256:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40225d:	00 
  40225e:	48 8d bc 24 a0 00 00 	lea    0xa0(%rsp),%rdi
  402265:	00 
  402266:	e8 95 15 00 00       	call   403800 <runtime::[core.odin]::__init_context>
  40226b:	0f 28 04 24          	movaps (%rsp),%xmm0
  40226f:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  402276:	00 
  402277:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40227e:	00 
  40227f:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  402284:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  402289:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40228e:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402293:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402298:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40229d:	e8 0e 15 00 00       	call   4037b0 <runtime::default_context>
  4022a2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4022a7:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4022ac:	0f 28 44 24 30       	movaps 0x30(%rsp),%xmm0
  4022b1:	0f 28 4c 24 40       	movaps 0x40(%rsp),%xmm1
  4022b6:	0f 28 54 24 50       	movaps 0x50(%rsp),%xmm2
  4022bb:	0f 28 5c 24 60       	movaps 0x60(%rsp),%xmm3
  4022c0:	0f 28 64 24 70       	movaps 0x70(%rsp),%xmm4
  4022c5:	0f 28 ac 24 80 00 00 	movaps 0x80(%rsp),%xmm5
  4022cc:	00 
  4022cd:	0f 28 b4 24 90 00 00 	movaps 0x90(%rsp),%xmm6
  4022d4:	00 
  4022d5:	0f 29 b4 24 00 01 00 	movaps %xmm6,0x100(%rsp)
  4022dc:	00 
  4022dd:	0f 29 ac 24 f0 00 00 	movaps %xmm5,0xf0(%rsp)
  4022e4:	00 
  4022e5:	0f 29 a4 24 e0 00 00 	movaps %xmm4,0xe0(%rsp)
  4022ec:	00 
  4022ed:	0f 29 9c 24 d0 00 00 	movaps %xmm3,0xd0(%rsp)
  4022f4:	00 
  4022f5:	0f 29 94 24 c0 00 00 	movaps %xmm2,0xc0(%rsp)
  4022fc:	00 
  4022fd:	0f 29 8c 24 b0 00 00 	movaps %xmm1,0xb0(%rsp)
  402304:	00 
  402305:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40230c:	00 
  40230d:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  402314:	00 
  402315:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  40231c:	00 
  40231d:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  402324:	00 
  402325:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40232c:	00 
  40232d:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  402334:	00 
  402335:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  40233c:	00 
  40233d:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402342:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402347:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40234c:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402351:	4c 8d 84 24 a0 00 00 	lea    0xa0(%rsp),%r8
  402358:	00 
  402359:	e8 22 3b 00 00       	call   405e80 <runtime::mem_free>
  40235e:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  402365:	c3                   	ret
  402366:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40236d:	00 00 00 

0000000000402370 <runtime::alloc_from_memory_block>:
  402370:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  402377:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40237c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402381:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402386:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40238b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402390:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402395:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40239a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4023a1:	00 
  4023a2:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  4023a9:	00 
  4023aa:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  4023b1:	00 
  4023b2:	0f 57 c0             	xorps  %xmm0,%xmm0
  4023b5:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4023bc:	00 
  4023bd:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  4023c4:	00 
  4023c5:	48 83 f8 00          	cmp    $0x0,%rax
  4023c9:	0f 94 c0             	sete   %al
  4023cc:	24 01                	and    $0x1,%al
  4023ce:	3c 00                	cmp    $0x0,%al
  4023d0:	74 3e                	je     402410 <runtime::alloc_from_memory_block+0xa0>
  4023d2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4023d7:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  4023de:	00 00 00 00 00 
  4023e3:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  4023ea:	00 00 00 00 00 
  4023ef:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  4023f6:	01 
  4023f7:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4023fe:	00 
  4023ff:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402406:	b0 01                	mov    $0x1,%al
  402408:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40240f:	c3                   	ret
  402410:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  402415:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40241a:	e8 31 11 00 00       	call   403550 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  40241f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402424:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40242b:	00 
  40242c:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  402433:	00 
  402434:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40243b:	00 00 00 00 00 
  402440:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  402447:	00 
  402448:	e8 83 fa ff ff       	call   401ed0 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  40244d:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  402454:	00 
  402455:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40245a:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  40245e:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  402463:	75 4a                	jne    4024af <runtime::alloc_from_memory_block+0x13f>
  402465:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40246a:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402471:	01 
  402472:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402479:	00 
  40247a:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402481:	00 
  402482:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402489:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402490:	00 
  402491:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402498:	00 
  402499:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  4024a0:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4024a4:	48 89 11             	mov    %rdx,(%rcx)
  4024a7:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  4024ae:	c3                   	ret
  4024af:	eb 00                	jmp    4024b1 <runtime::alloc_from_memory_block+0x141>
  4024b1:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4024b8:	00 
  4024b9:	48 8b 78 20          	mov    0x20(%rax),%rdi
  4024bd:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4024c2:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  4024c9:	00 00 
  4024cb:	48 8d 54 24 60       	lea    0x60(%rsp),%rdx
  4024d0:	e8 fb f9 ff ff       	call   401ed0 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  4024d5:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4024da:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4024df:	88 44 24 47          	mov    %al,0x47(%rsp)
  4024e3:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  4024e8:	74 1a                	je     402504 <runtime::alloc_from_memory_block+0x194>
  4024ea:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4024ef:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4024f6:	00 
  4024f7:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  4024fb:	0f 97 c0             	seta   %al
  4024fe:	24 01                	and    $0x1,%al
  402500:	3c 00                	cmp    $0x0,%al
  402502:	74 4a                	je     40254e <runtime::alloc_from_memory_block+0x1de>
  402504:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402509:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402510:	01 
  402511:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402518:	00 
  402519:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402520:	00 
  402521:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402528:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40252f:	00 
  402530:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402537:	00 
  402538:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  40253f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402543:	48 89 11             	mov    %rdx,(%rcx)
  402546:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40254d:	c3                   	ret
  40254e:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  402553:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  40255a:	00 
  40255b:	48 8b 41 18          	mov    0x18(%rcx),%rax
  40255f:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  402563:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40256a:	00 
  40256b:	48 01 d1             	add    %rdx,%rcx
  40256e:	48 01 c8             	add    %rcx,%rax
  402571:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402576:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40257b:	48 89 04 24          	mov    %rax,(%rsp)
  40257f:	bf 60 90 40 00       	mov    $0x409060,%edi
  402584:	31 c0                	xor    %eax,%eax
  402586:	41 89 c0             	mov    %eax,%r8d
  402589:	be 3c 00 00 00       	mov    $0x3c,%esi
  40258e:	ba 5c 00 00 00       	mov    $0x5c,%edx
  402593:	b9 31 00 00 00       	mov    $0x31,%ecx
  402598:	e8 a3 19 00 00       	call   403f40 <runtime::multi_pointer_slice_expr_error>
  40259d:	48 8b 14 24          	mov    (%rsp),%rdx
  4025a1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4025a6:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4025ab:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4025b0:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4025b5:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4025ba:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4025bf:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  4025c6:	00 
  4025c7:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4025ce:	00 
  4025cf:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4025d6:	00 
  4025d7:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4025dc:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4025e0:	48 01 f2             	add    %rsi,%rdx
  4025e3:	48 89 50 20          	mov    %rdx,0x20(%rax)
  4025e7:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4025ee:	00 
  4025ef:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4025f6:	00 
  4025f7:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  4025fe:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402605:	00 
  402606:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40260d:	00 
  40260e:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402615:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402619:	48 89 11             	mov    %rdx,(%rcx)
  40261c:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402623:	c3                   	ret
  402624:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40262b:	00 00 00 00 00 

0000000000402630 <runtime::arena_alloc>:
  402630:	48 81 ec 48 01 00 00 	sub    $0x148,%rsp
  402637:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40263c:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  402641:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  402646:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40264b:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402650:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  402655:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40265a:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40265f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  402664:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402669:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40266e:	48 89 b4 24 40 01 00 	mov    %rsi,0x140(%rsp)
  402675:	00 
  402676:	48 89 94 24 38 01 00 	mov    %rdx,0x138(%rsp)
  40267d:	00 
  40267e:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  402685:	00 
  402686:	0f 57 c0             	xorps  %xmm0,%xmm0
  402689:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  402690:	00 
  402691:	c6 84 24 1f 01 00 00 	movb   $0x0,0x11f(%rsp)
  402698:	00 
  402699:	48 89 c2             	mov    %rax,%rdx
  40269c:	48 83 ea 01          	sub    $0x1,%rdx
  4026a0:	48 21 d0             	and    %rdx,%rax
  4026a3:	48 83 f8 00          	cmp    $0x0,%rax
  4026a7:	0f 94 c0             	sete   %al
  4026aa:	24 01                	and    $0x1,%al
  4026ac:	0f b6 f8             	movzbl %al,%edi
  4026af:	be 18 91 40 00       	mov    $0x409118,%esi
  4026b4:	ba 1a 00 00 00       	mov    $0x1a,%edx
  4026b9:	e8 32 58 00 00       	call   407ef0 <runtime::assert>
  4026be:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4026c3:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4026ca:	00 
  4026cb:	48 83 bc 24 10 01 00 	cmpq   $0x0,0x110(%rsp)
  4026d2:	00 00 
  4026d4:	0f 94 c0             	sete   %al
  4026d7:	24 01                	and    $0x1,%al
  4026d9:	3c 00                	cmp    $0x0,%al
  4026db:	74 42                	je     40271f <runtime::arena_alloc+0xef>
  4026dd:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4026e2:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  4026e9:	00 
  4026ea:	48 8b b4 24 28 01 00 	mov    0x128(%rsp),%rsi
  4026f1:	00 
  4026f2:	8a 84 24 1f 01 00 00 	mov    0x11f(%rsp),%al
  4026f9:	48 89 b4 24 28 01 00 	mov    %rsi,0x128(%rsp)
  402700:	00 
  402701:	48 89 94 24 20 01 00 	mov    %rdx,0x120(%rsp)
  402708:	00 
  402709:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  402710:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402714:	48 89 11             	mov    %rdx,(%rcx)
  402717:	48 81 c4 48 01 00 00 	add    $0x148,%rsp
  40271e:	c3                   	ret
  40271f:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  402726:	00 
  402727:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  40272c:	0f 94 c0             	sete   %al
  40272f:	24 01                	and    $0x1,%al
  402731:	3c 00                	cmp    $0x0,%al
  402733:	74 09                	je     40273e <runtime::arena_alloc+0x10e>
  402735:	31 c0                	xor    %eax,%eax
  402737:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40273c:	eb 15                	jmp    402753 <runtime::arena_alloc+0x123>
  40273e:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  402745:	00 
  402746:	48 8b 40 10          	mov    0x10(%rax),%rax
  40274a:	48 8b 40 20          	mov    0x20(%rax),%rax
  40274e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402753:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  402758:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40275d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402762:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  402769:	00 
  40276a:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  402771:	00 
  402772:	48 8b 78 10          	mov    0x10(%rax),%rdi
  402776:	48 8b b4 24 10 01 00 	mov    0x110(%rsp),%rsi
  40277d:	00 
  40277e:	0f 57 c0             	xorps  %xmm0,%xmm0
  402781:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  402788:	00 
  402789:	48 8d 8c 24 f0 00 00 	lea    0xf0(%rsp),%rcx
  402790:	00 
  402791:	e8 da fb ff ff       	call   402370 <runtime::alloc_from_memory_block>
  402796:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  40279d:	00 
  40279e:	48 8b 94 24 f8 00 00 	mov    0xf8(%rsp),%rdx
  4027a5:	00 
  4027a6:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  4027ad:	00 
  4027ae:	48 89 8c 24 20 01 00 	mov    %rcx,0x120(%rsp)
  4027b5:	00 
  4027b6:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  4027bd:	80 bc 24 1f 01 00 00 	cmpb   $0x1,0x11f(%rsp)
  4027c4:	01 
  4027c5:	0f 94 c0             	sete   %al
  4027c8:	24 01                	and    $0x1,%al
  4027ca:	3c 00                	cmp    $0x0,%al
  4027cc:	0f 84 39 02 00 00    	je     402a0b <runtime::arena_alloc+0x3db>
  4027d2:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4027d9:	00 
  4027da:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  4027df:	0f 94 c0             	sete   %al
  4027e2:	24 01                	and    $0x1,%al
  4027e4:	3c 00                	cmp    $0x0,%al
  4027e6:	74 10                	je     4027f8 <runtime::arena_alloc+0x1c8>
  4027e8:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4027ef:	00 
  4027f0:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  4027f7:	00 
  4027f8:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4027fd:	48 8b bc 24 10 01 00 	mov    0x110(%rsp),%rdi
  402804:	00 
  402805:	e8 d6 0d 00 00       	call   4035e0 <runtime::arena_alloc.align_forward_uint-0>
  40280a:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  402811:	00 
  402812:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  402819:	00 
  40281a:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  402821:	00 
  402822:	48 8b 40 28          	mov    0x28(%rax),%rax
  402826:	48 39 c1             	cmp    %rax,%rcx
  402829:	48 0f 47 c1          	cmova  %rcx,%rax
  40282d:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  402834:	00 
  402835:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40283c:	00 
  40283d:	48 83 38 00          	cmpq   $0x0,(%rax)
  402841:	0f 94 c0             	sete   %al
  402844:	24 01                	and    $0x1,%al
  402846:	3c 00                	cmp    $0x0,%al
  402848:	74 46                	je     402890 <runtime::arena_alloc+0x260>
  40284a:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40284f:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  402856:	00 
  402857:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40285c:	e8 7f 2a 00 00       	call   4052e0 <runtime::heap_allocator>
  402861:	48 89 c1             	mov    %rax,%rcx
  402864:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402869:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  402870:	00 
  402871:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  402878:	00 
  402879:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  402880:	00 
  402881:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  402888:	00 
  402889:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40288d:	48 89 08             	mov    %rcx,(%rax)
  402890:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  402895:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40289a:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  40289f:	48 8b 94 24 40 01 00 	mov    0x140(%rsp),%rdx
  4028a6:	00 
  4028a7:	48 8b 02             	mov    (%rdx),%rax
  4028aa:	48 8b 72 08          	mov    0x8(%rdx),%rsi
  4028ae:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  4028b5:	00 
  4028b6:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4028bd:	00 
  4028be:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4028c5:	00 
  4028c6:	48 8b bc 24 a0 00 00 	mov    0xa0(%rsp),%rdi
  4028cd:	00 
  4028ce:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4028d5:	00 
  4028d6:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  4028dd:	00 00 00 00 00 
  4028e2:	48 89 e0             	mov    %rsp,%rax
  4028e5:	4c 89 08             	mov    %r9,(%rax)
  4028e8:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  4028ef:	00 
  4028f0:	e8 2b f6 ff ff       	call   401f20 <runtime::memory_block_alloc>
  4028f5:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4028f9:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  402900:	00 
  402901:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  402906:	3c 00                	cmp    $0x0,%al
  402908:	74 4d                	je     402957 <runtime::arena_alloc+0x327>
  40290a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40290f:	8a 44 24 0f          	mov    0xf(%rsp),%al
  402913:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  40291a:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  402921:	00 
  402922:	48 8b b4 24 28 01 00 	mov    0x128(%rsp),%rsi
  402929:	00 
  40292a:	8a 84 24 1f 01 00 00 	mov    0x11f(%rsp),%al
  402931:	48 89 b4 24 28 01 00 	mov    %rsi,0x128(%rsp)
  402938:	00 
  402939:	48 89 94 24 20 01 00 	mov    %rdx,0x120(%rsp)
  402940:	00 
  402941:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  402948:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40294c:	48 89 11             	mov    %rdx,(%rcx)
  40294f:	48 81 c4 48 01 00 00 	add    $0x148,%rsp
  402956:	c3                   	ret
  402957:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40295c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  402961:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402966:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40296d:	00 
  40296e:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  402975:	00 
  402976:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  40297d:	00 
  40297e:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  402982:	48 89 08             	mov    %rcx,(%rax)
  402985:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40298c:	00 
  40298d:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  402994:	00 
  402995:	48 89 48 10          	mov    %rcx,0x10(%rax)
  402999:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4029a0:	00 
  4029a1:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  4029a8:	00 
  4029a9:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  4029ad:	48 8b 48 20          	mov    0x20(%rax),%rcx
  4029b1:	48 01 f1             	add    %rsi,%rcx
  4029b4:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4029b8:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  4029bf:	00 00 00 00 00 
  4029c4:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4029cb:	00 
  4029cc:	48 8b 78 10          	mov    0x10(%rax),%rdi
  4029d0:	48 8b b4 24 10 01 00 	mov    0x110(%rsp),%rsi
  4029d7:	00 
  4029d8:	0f 57 c0             	xorps  %xmm0,%xmm0
  4029db:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4029e0:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  4029e5:	e8 86 f9 ff ff       	call   402370 <runtime::alloc_from_memory_block>
  4029ea:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4029ef:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  4029f4:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  4029fb:	00 
  4029fc:	48 89 8c 24 20 01 00 	mov    %rcx,0x120(%rsp)
  402a03:	00 
  402a04:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  402a0b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402a10:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  402a17:	00 
  402a18:	48 8b 70 10          	mov    0x10(%rax),%rsi
  402a1c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  402a20:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  402a24:	48 8b bc 24 08 01 00 	mov    0x108(%rsp),%rdi
  402a2b:	00 
  402a2c:	48 29 fe             	sub    %rdi,%rsi
  402a2f:	48 01 f2             	add    %rsi,%rdx
  402a32:	48 89 50 18          	mov    %rdx,0x18(%rax)
  402a36:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  402a3d:	00 
  402a3e:	48 8b b4 24 28 01 00 	mov    0x128(%rsp),%rsi
  402a45:	00 
  402a46:	8a 84 24 1f 01 00 00 	mov    0x11f(%rsp),%al
  402a4d:	48 89 b4 24 28 01 00 	mov    %rsi,0x128(%rsp)
  402a54:	00 
  402a55:	48 89 94 24 20 01 00 	mov    %rdx,0x120(%rsp)
  402a5c:	00 
  402a5d:	88 84 24 1f 01 00 00 	mov    %al,0x11f(%rsp)
  402a64:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402a68:	48 89 11             	mov    %rdx,(%rcx)
  402a6b:	48 81 c4 48 01 00 00 	add    $0x148,%rsp
  402a72:	c3                   	ret
  402a73:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402a7a:	84 00 00 00 00 00 

0000000000402a80 <runtime::arena_free_last_memory_block>:
  402a80:	48 83 ec 28          	sub    $0x28,%rsp
  402a84:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402a89:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402a8e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402a93:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402a98:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402a9d:	48 8b 40 10          	mov    0x10(%rax),%rax
  402aa1:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  402aa6:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  402aac:	0f 95 c0             	setne  %al
  402aaf:	24 01                	and    $0x1,%al
  402ab1:	3c 00                	cmp    $0x0,%al
  402ab3:	74 39                	je     402aee <runtime::arena_free_last_memory_block+0x6e>
  402ab5:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402aba:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402abf:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402ac4:	48 8b 09             	mov    (%rcx),%rcx
  402ac7:	48 89 48 10          	mov    %rcx,0x10(%rax)
  402acb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402ad0:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402ad5:	48 8b 51 28          	mov    0x28(%rcx),%rdx
  402ad9:	48 8b 48 20          	mov    0x20(%rax),%rcx
  402add:	48 29 d1             	sub    %rdx,%rcx
  402ae0:	48 89 48 20          	mov    %rcx,0x20(%rax)
  402ae4:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  402ae9:	e8 e2 f6 ff ff       	call   4021d0 <runtime::memory_block_dealloc>
  402aee:	48 83 c4 28          	add    $0x28,%rsp
  402af2:	c3                   	ret
  402af3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402afa:	84 00 00 00 00 00 

0000000000402b00 <runtime::arena_free_all>:
  402b00:	48 83 ec 28          	sub    $0x28,%rsp
  402b04:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402b09:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402b0e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402b13:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402b18:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402b1d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402b22:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  402b27:	0f 95 c0             	setne  %al
  402b2a:	24 01                	and    $0x1,%al
  402b2c:	3c 00                	cmp    $0x0,%al
  402b2e:	74 2c                	je     402b5c <runtime::arena_free_all+0x5c>
  402b30:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402b35:	48 8b 40 10          	mov    0x10(%rax),%rax
  402b39:	48 83 38 00          	cmpq   $0x0,(%rax)
  402b3d:	0f 95 c0             	setne  %al
  402b40:	24 01                	and    $0x1,%al
  402b42:	3c 00                	cmp    $0x0,%al
  402b44:	74 16                	je     402b5c <runtime::arena_free_all+0x5c>
  402b46:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402b4b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402b50:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402b55:	e8 26 ff ff ff       	call   402a80 <runtime::arena_free_last_memory_block>
  402b5a:	eb c1                	jmp    402b1d <runtime::arena_free_all+0x1d>
  402b5c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402b61:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  402b66:	0f 95 c0             	setne  %al
  402b69:	24 01                	and    $0x1,%al
  402b6b:	3c 00                	cmp    $0x0,%al
  402b6d:	74 32                	je     402ba1 <runtime::arena_free_all+0xa1>
  402b6f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402b74:	48 8b 40 10          	mov    0x10(%rax),%rax
  402b78:	48 8b 78 18          	mov    0x18(%rax),%rdi
  402b7c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402b81:	48 8b 40 10          	mov    0x10(%rax),%rax
  402b85:	48 8b 50 20          	mov    0x20(%rax),%rdx
  402b89:	31 f6                	xor    %esi,%esi
  402b8b:	e8 b0 e4 ff ff       	call   401040 <memset@plt>
  402b90:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402b95:	48 8b 40 10          	mov    0x10(%rax),%rax
  402b99:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  402ba0:	00 
  402ba1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402ba6:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  402bad:	00 
  402bae:	48 83 c4 28          	add    $0x28,%rsp
  402bb2:	c3                   	ret
  402bb3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402bba:	84 00 00 00 00 00 

0000000000402bc0 <runtime::arena_destroy>:
  402bc0:	48 83 ec 28          	sub    $0x28,%rsp
  402bc4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402bc9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402bce:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402bd3:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402bd8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402bdd:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  402be2:	0f 95 c0             	setne  %al
  402be5:	24 01                	and    $0x1,%al
  402be7:	3c 00                	cmp    $0x0,%al
  402be9:	74 49                	je     402c34 <runtime::arena_destroy+0x74>
  402beb:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402bf0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402bf5:	48 8b 40 10          	mov    0x10(%rax),%rax
  402bf9:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  402bfe:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c03:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402c08:	48 8b 09             	mov    (%rcx),%rcx
  402c0b:	48 89 48 10          	mov    %rcx,0x10(%rax)
  402c0f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c14:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402c19:	48 8b 51 28          	mov    0x28(%rcx),%rdx
  402c1d:	48 8b 48 20          	mov    0x20(%rax),%rcx
  402c21:	48 29 d1             	sub    %rdx,%rcx
  402c24:	48 89 48 20          	mov    %rcx,0x20(%rax)
  402c28:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  402c2d:	e8 9e f5 ff ff       	call   4021d0 <runtime::memory_block_dealloc>
  402c32:	eb a4                	jmp    402bd8 <runtime::arena_destroy+0x18>
  402c34:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c39:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  402c40:	00 
  402c41:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c46:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  402c4d:	00 
  402c4e:	48 83 c4 28          	add    $0x28,%rsp
  402c52:	c3                   	ret
  402c53:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402c5a:	84 00 00 00 00 00 

0000000000402c60 <runtime::arena_allocator_proc>:
  402c60:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  402c67:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  402c6c:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  402c73:	00 
  402c74:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  402c7b:	00 
  402c7c:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  402c83:	00 
  402c84:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  402c8b:	00 
  402c8c:	40 88 f0             	mov    %sil,%al
  402c8f:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  402c96:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  402c9d:	00 
  402c9e:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  402ca5:	00 
  402ca6:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  402cad:	00 
  402cae:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  402cb5:	00 
  402cb6:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  402cbd:	00 
  402cbe:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  402cc5:	00 
  402cc6:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  402ccd:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  402cd2:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  402cd9:	00 
  402cda:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  402ce1:	00 
  402ce2:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  402ce9:	00 
  402cea:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  402cf1:	00 
  402cf2:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  402cf9:	00 
  402cfa:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  402d01:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  402d08:	00 
  402d09:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  402d10:	00 
  402d11:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  402d18:	00 
  402d19:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  402d20:	00 
  402d21:	0f 57 c0             	xorps  %xmm0,%xmm0
  402d24:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  402d2b:	00 
  402d2c:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  402d33:	00 
  402d34:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  402d3b:	00 
  402d3c:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  402d43:	00 
  402d44:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  402d4b:	00 
  402d4c:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  402d53:	00 
  402d54:	0f b6 c8             	movzbl %al,%ecx
  402d57:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  402d5c:	2c 07                	sub    $0x7,%al
  402d5e:	0f 87 9a 07 00 00    	ja     4034fe <runtime::arena_allocator_proc+0x89e>
  402d64:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  402d69:	48 8b 04 c5 10 90 40 	mov    0x409010(,%rax,8),%rax
  402d70:	00 
  402d71:	ff e0                	jmp    *%rax
  402d73:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  402d7a:	00 
  402d7b:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  402d82:	00 
  402d83:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  402d8a:	00 
  402d8b:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  402d92:	00 
  402d93:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  402d9a:	00 
  402d9b:	0f 57 c0             	xorps  %xmm0,%xmm0
  402d9e:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  402da5:	00 
  402da6:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  402dad:	00 
  402dae:	e8 7d f8 ff ff       	call   402630 <runtime::arena_alloc>
  402db3:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  402dba:	00 
  402dbb:	40 88 c7             	mov    %al,%dil
  402dbe:	40 88 f8             	mov    %dil,%al
  402dc1:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  402dc8:	00 
  402dc9:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  402dd0:	00 
  402dd1:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  402dd8:	00 
  402dd9:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  402de0:	00 
  402de1:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  402de8:	00 
  402de9:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402ded:	48 89 11             	mov    %rdx,(%rcx)
  402df0:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  402df7:	c3                   	ret
  402df8:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  402dff:	04 
  402e00:	e9 f9 06 00 00       	jmp    4034fe <runtime::arena_allocator_proc+0x89e>
  402e05:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  402e0c:	00 
  402e0d:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  402e14:	00 
  402e15:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  402e1c:	00 
  402e1d:	e8 de fc ff ff       	call   402b00 <runtime::arena_free_all>
  402e22:	e9 d7 06 00 00       	jmp    4034fe <runtime::arena_allocator_proc+0x89e>
  402e27:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  402e2e:	00 
  402e2f:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  402e36:	00 
  402e37:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  402e3e:	00 00 
  402e40:	0f 94 c1             	sete   %cl
  402e43:	80 e1 01             	and    $0x1,%cl
  402e46:	b0 01                	mov    $0x1,%al
  402e48:	38 c8                	cmp    %cl,%al
  402e4a:	74 25                	je     402e71 <runtime::arena_allocator_proc+0x211>
  402e4c:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  402e53:	00 
  402e54:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  402e5b:	00 
  402e5c:	0f 94 c1             	sete   %cl
  402e5f:	80 e1 01             	and    $0x1,%cl
  402e62:	b0 01                	mov    $0x1,%al
  402e64:	38 c8                	cmp    %cl,%al
  402e66:	0f 84 a8 00 00 00    	je     402f14 <runtime::arena_allocator_proc+0x2b4>
  402e6c:	e9 85 00 00 00       	jmp    402ef6 <runtime::arena_allocator_proc+0x296>
  402e71:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  402e78:	00 
  402e79:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  402e80:	00 
  402e81:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  402e88:	00 
  402e89:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  402e90:	00 
  402e91:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  402e98:	00 
  402e99:	0f 57 c0             	xorps  %xmm0,%xmm0
  402e9c:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  402ea3:	00 
  402ea4:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  402eab:	00 
  402eac:	e8 7f f7 ff ff       	call   402630 <runtime::arena_alloc>
  402eb1:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  402eb8:	00 
  402eb9:	40 88 c7             	mov    %al,%dil
  402ebc:	40 88 f8             	mov    %dil,%al
  402ebf:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  402ec6:	00 
  402ec7:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  402ece:	00 
  402ecf:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  402ed6:	00 
  402ed7:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  402ede:	00 
  402edf:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  402ee6:	00 
  402ee7:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402eeb:	48 89 11             	mov    %rdx,(%rcx)
  402eee:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  402ef5:	c3                   	ret
  402ef6:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  402efd:	00 00 
  402eff:	0f 94 c1             	sete   %cl
  402f02:	80 e1 01             	and    $0x1,%cl
  402f05:	b0 01                	mov    $0x1,%al
  402f07:	38 c8                	cmp    %cl,%al
  402f09:	0f 84 e5 00 00 00    	je     402ff4 <runtime::arena_allocator_proc+0x394>
  402f0f:	e9 b7 00 00 00       	jmp    402fcb <runtime::arena_allocator_proc+0x36b>
  402f14:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  402f1b:	00 
  402f1c:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402f21:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  402f28:	00 
  402f29:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  402f2e:	bf 60 90 40 00       	mov    $0x409060,%edi
  402f33:	31 c0                	xor    %eax,%eax
  402f35:	41 89 c0             	mov    %eax,%r8d
  402f38:	be 3c 00 00 00       	mov    $0x3c,%esi
  402f3d:	ba db 00 00 00       	mov    $0xdb,%edx
  402f42:	b9 13 00 00 00       	mov    $0x13,%ecx
  402f47:	e8 f4 0f 00 00       	call   403f40 <runtime::multi_pointer_slice_expr_error>
  402f4c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  402f51:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402f56:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  402f5d:	00 
  402f5e:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  402f65:	00 
  402f66:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  402f6d:	00 
  402f6e:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  402f75:	00 
  402f76:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  402f7d:	00 
  402f7e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  402f85:	00 
  402f86:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  402f8d:	00 
  402f8e:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  402f95:	00 
  402f96:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  402f9d:	00 
  402f9e:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  402fa5:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  402fac:	00 
  402fad:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  402fb4:	00 
  402fb5:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  402fbc:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402fc0:	48 89 11             	mov    %rdx,(%rcx)
  402fc3:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  402fca:	c3                   	ret
  402fcb:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  402fd2:	00 
  402fd3:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  402fda:	00 
  402fdb:	48 83 e9 01          	sub    $0x1,%rcx
  402fdf:	48 21 c8             	and    %rcx,%rax
  402fe2:	48 83 f8 00          	cmp    $0x0,%rax
  402fe6:	0f 94 c1             	sete   %cl
  402fe9:	80 e1 01             	and    $0x1,%cl
  402fec:	b0 01                	mov    $0x1,%al
  402fee:	38 c8                	cmp    %cl,%al
  402ff0:	74 54                	je     403046 <runtime::arena_allocator_proc+0x3e6>
  402ff2:	eb 4d                	jmp    403041 <runtime::arena_allocator_proc+0x3e1>
  402ff4:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  402ffb:	00 
  402ffc:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  403003:	04 
  403004:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  40300b:	00 
  40300c:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  403013:	00 
  403014:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40301b:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  403022:	00 
  403023:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40302a:	00 
  40302b:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  403032:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403036:	48 89 11             	mov    %rdx,(%rcx)
  403039:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  403040:	c3                   	ret
  403041:	e9 94 02 00 00       	jmp    4032da <runtime::arena_allocator_proc+0x67a>
  403046:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  40304d:	00 
  40304e:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  403055:	00 
  403056:	0f 92 c0             	setb   %al
  403059:	24 01                	and    $0x1,%al
  40305b:	3c 00                	cmp    $0x0,%al
  40305d:	0f 84 b7 00 00 00    	je     40311a <runtime::arena_allocator_proc+0x4ba>
  403063:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40306a:	00 
  40306b:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403070:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  403077:	00 
  403078:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  40307d:	bf 60 90 40 00       	mov    $0x409060,%edi
  403082:	31 c0                	xor    %eax,%eax
  403084:	41 89 c0             	mov    %eax,%r8d
  403087:	be 3c 00 00 00       	mov    $0x3c,%esi
  40308c:	ba e3 00 00 00       	mov    $0xe3,%edx
  403091:	b9 14 00 00 00       	mov    $0x14,%ecx
  403096:	e8 a5 0e 00 00       	call   403f40 <runtime::multi_pointer_slice_expr_error>
  40309b:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4030a0:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4030a5:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4030ac:	00 
  4030ad:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  4030b4:	00 
  4030b5:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  4030bc:	00 
  4030bd:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  4030c4:	00 
  4030c5:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  4030cc:	00 
  4030cd:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4030d4:	00 
  4030d5:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4030dc:	00 
  4030dd:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4030e4:	00 
  4030e5:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4030ec:	00 
  4030ed:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4030f4:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4030fb:	00 
  4030fc:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  403103:	00 
  403104:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40310b:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40310f:	48 89 11             	mov    %rdx,(%rcx)
  403112:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  403119:	c3                   	ret
  40311a:	eb 00                	jmp    40311c <runtime::arena_allocator_proc+0x4bc>
  40311c:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  403123:	00 
  403124:	48 8b 40 10          	mov    0x10(%rax),%rax
  403128:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40312f:	00 
  403130:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  403137:	00 00 
  403139:	0f 95 c0             	setne  %al
  40313c:	24 01                	and    $0x1,%al
  40313e:	3c 00                	cmp    $0x0,%al
  403140:	0f 84 92 01 00 00    	je     4032d8 <runtime::arena_allocator_proc+0x678>
  403146:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40314d:	00 
  40314e:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  403155:	00 
  403156:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  40315a:	48 29 c8             	sub    %rcx,%rax
  40315d:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  403164:	00 
  403165:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40316c:	00 
  40316d:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  403174:	00 
  403175:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  40317c:	00 
  40317d:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  403184:	00 
  403185:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  40318c:	00 
  40318d:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  403194:	00 
  403195:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40319c:	00 
  40319d:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  4031a4:	00 
  4031a5:	0f 92 c0             	setb   %al
  4031a8:	24 01                	and    $0x1,%al
  4031aa:	3c 00                	cmp    $0x0,%al
  4031ac:	0f 84 24 01 00 00    	je     4032d6 <runtime::arena_allocator_proc+0x676>
  4031b2:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4031b9:	00 
  4031ba:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  4031c1:	00 
  4031c2:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  4031c6:	0f 94 c0             	sete   %al
  4031c9:	24 01                	and    $0x1,%al
  4031cb:	3c 00                	cmp    $0x0,%al
  4031cd:	0f 84 03 01 00 00    	je     4032d6 <runtime::arena_allocator_proc+0x676>
  4031d3:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  4031da:	00 
  4031db:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  4031e2:	00 
  4031e3:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  4031e7:	0f 96 c0             	setbe  %al
  4031ea:	24 01                	and    $0x1,%al
  4031ec:	3c 00                	cmp    $0x0,%al
  4031ee:	0f 84 e2 00 00 00    	je     4032d6 <runtime::arena_allocator_proc+0x676>
  4031f4:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4031fb:	00 
  4031fc:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  403203:	00 
  403204:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403208:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40320f:	00 
  403210:	48 8b 40 18          	mov    0x18(%rax),%rax
  403214:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403219:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  403220:	00 
  403221:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403226:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  40322d:	00 
  40322e:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  403233:	bf 60 90 40 00       	mov    $0x409060,%edi
  403238:	be 3c 00 00 00       	mov    $0x3c,%esi
  40323d:	ba ee 00 00 00       	mov    $0xee,%edx
  403242:	b9 17 00 00 00       	mov    $0x17,%ecx
  403247:	e8 f4 0c 00 00       	call   403f40 <runtime::multi_pointer_slice_expr_error>
  40324c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403251:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403256:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40325b:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  403262:	00 
  403263:	48 01 f2             	add    %rsi,%rdx
  403266:	48 29 f0             	sub    %rsi,%rax
  403269:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  403270:	00 
  403271:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  403278:	00 
  403279:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  403280:	00 
  403281:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  403288:	00 
  403289:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  403290:	00 
  403291:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  403298:	00 
  403299:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4032a0:	00 
  4032a1:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4032a8:	00 
  4032a9:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4032b0:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4032b7:	00 
  4032b8:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4032bf:	00 
  4032c0:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4032c7:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4032cb:	48 89 11             	mov    %rdx,(%rcx)
  4032ce:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4032d5:	c3                   	ret
  4032d6:	eb 00                	jmp    4032d8 <runtime::arena_allocator_proc+0x678>
  4032d8:	eb 00                	jmp    4032da <runtime::arena_allocator_proc+0x67a>
  4032da:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  4032e1:	00 
  4032e2:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4032e9:	00 
  4032ea:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4032f1:	00 
  4032f2:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4032f9:	00 
  4032fa:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  403301:	00 
  403302:	0f 57 c0             	xorps  %xmm0,%xmm0
  403305:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40330c:	00 
  40330d:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  403314:	00 
  403315:	e8 16 f3 ff ff       	call   402630 <runtime::arena_alloc>
  40331a:	88 44 24 27          	mov    %al,0x27(%rsp)
  40331e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  403325:	00 
  403326:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40332b:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  403332:	00 
  403333:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403338:	3c 00                	cmp    $0x0,%al
  40333a:	74 50                	je     40338c <runtime::arena_allocator_proc+0x72c>
  40333c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  403343:	00 
  403344:	8a 44 24 27          	mov    0x27(%rsp),%al
  403348:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40334f:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  403356:	00 
  403357:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40335e:	00 
  40335f:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  403366:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40336d:	00 
  40336e:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  403375:	00 
  403376:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40337d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403381:	48 89 11             	mov    %rdx,(%rcx)
  403384:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40338b:	c3                   	ret
  40338c:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403391:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403396:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  40339d:	00 
  40339e:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4033a5:	00 
  4033a6:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  4033ad:	00 00 
  4033af:	0f 94 c0             	sete   %al
  4033b2:	24 01                	and    $0x1,%al
  4033b4:	3c 00                	cmp    $0x0,%al
  4033b6:	74 45                	je     4033fd <runtime::arena_allocator_proc+0x79d>
  4033b8:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4033bf:	00 
  4033c0:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4033c7:	00 
  4033c8:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4033cf:	00 
  4033d0:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4033d7:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4033de:	00 
  4033df:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4033e6:	00 
  4033e7:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4033ee:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4033f2:	48 89 11             	mov    %rdx,(%rcx)
  4033f5:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4033fc:	c3                   	ret
  4033fd:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403404:	00 
  403405:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40340a:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  403411:	00 
  403412:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403417:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40341e:	00 
  40341f:	48 89 04 24          	mov    %rax,(%rsp)
  403423:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  40342a:	00 
  40342b:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403430:	bf 60 90 40 00       	mov    $0x409060,%edi
  403435:	31 c0                	xor    %eax,%eax
  403437:	41 89 c0             	mov    %eax,%r8d
  40343a:	be 3c 00 00 00       	mov    $0x3c,%esi
  40343f:	ba f9 00 00 00       	mov    $0xf9,%edx
  403444:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  403449:	e8 f2 0a 00 00       	call   403f40 <runtime::multi_pointer_slice_expr_error>
  40344e:	48 8b 0c 24          	mov    (%rsp),%rcx
  403452:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403457:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40345c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403461:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  403468:	00 
  403469:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  403470:	00 
  403471:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  403478:	00 
  403479:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  403480:	00 
  403481:	e8 2a 47 00 00       	call   407bb0 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  403486:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40348d:	00 
  40348e:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  403495:	00 
  403496:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40349d:	00 
  40349e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4034a5:	00 
  4034a6:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  4034ad:	00 
  4034ae:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  4034b5:	00 
  4034b6:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4034ba:	48 89 08             	mov    %rcx,(%rax)
  4034bd:	31 c0                	xor    %eax,%eax
  4034bf:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4034c6:	c3                   	ret
  4034c7:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4034ce:	00 
  4034cf:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4034d6:	00 
  4034d7:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  4034de:	00 00 
  4034e0:	0f 95 c0             	setne  %al
  4034e3:	24 01                	and    $0x1,%al
  4034e5:	3c 00                	cmp    $0x0,%al
  4034e7:	74 0b                	je     4034f4 <runtime::arena_allocator_proc+0x894>
  4034e9:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4034f0:	00 
  4034f1:	c6 00 5d             	movb   $0x5d,(%rax)
  4034f4:	eb 08                	jmp    4034fe <runtime::arena_allocator_proc+0x89e>
  4034f6:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  4034fd:	04 
  4034fe:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  403505:	00 
  403506:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  40350d:	00 
  40350e:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  403515:	00 
  403516:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40351d:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  403524:	00 
  403525:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40352c:	00 
  40352d:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  403534:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403538:	48 89 11             	mov    %rdx,(%rcx)
  40353b:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  403542:	c3                   	ret
  403543:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40354a:	84 00 00 00 00 00 

0000000000403550 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  403550:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  403555:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  40355a:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40355f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  403564:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  403569:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40356e:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  403575:	00 00 
  403577:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40357c:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  403580:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  403585:	48 03 4a 20          	add    0x20(%rdx),%rcx
  403589:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40358e:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  403593:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  403598:	48 83 e8 01          	sub    $0x1,%rax
  40359c:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4035a1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4035a6:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  4035ab:	48 83 f8 00          	cmp    $0x0,%rax
  4035af:	0f 95 c0             	setne  %al
  4035b2:	24 01                	and    $0x1,%al
  4035b4:	3c 00                	cmp    $0x0,%al
  4035b6:	74 17                	je     4035cf <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  4035b8:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4035bd:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4035c2:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  4035c7:	48 29 c8             	sub    %rcx,%rax
  4035ca:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4035cf:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4035d4:	c3                   	ret
  4035d5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4035dc:	00 00 00 00 

00000000004035e0 <runtime::arena_alloc.align_forward_uint-0>:
  4035e0:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  4035e5:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  4035ea:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4035ef:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4035f4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4035f9:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4035fe:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  403603:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  403608:	48 83 e9 01          	sub    $0x1,%rcx
  40360c:	48 21 c8             	and    %rcx,%rax
  40360f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  403614:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  40361a:	0f 95 c0             	setne  %al
  40361d:	24 01                	and    $0x1,%al
  40361f:	3c 00                	cmp    $0x0,%al
  403621:	74 14                	je     403637 <runtime::arena_alloc.align_forward_uint-0+0x57>
  403623:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  403628:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  40362d:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  403632:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  403637:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40363c:	c3                   	ret
  40363d:	0f 1f 00             	nopl   (%rax)

0000000000403640 <runtime::__type_info_of>:
  403640:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  403645:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40364a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40364f:	48 c7 c0 d0 91 40 00 	mov    $0x4091d0,%rax
  403656:	48 8b 40 08          	mov    0x8(%rax),%rax
  40365a:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40365f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  403664:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  403669:	48 83 f8 00          	cmp    $0x0,%rax
  40366d:	74 16                	je     403685 <runtime::__type_info_of+0x45>
  40366f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  403674:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  403679:	31 d2                	xor    %edx,%edx
  40367b:	48 f7 f1             	div    %rcx
  40367e:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  403683:	eb 02                	jmp    403687 <runtime::__type_info_of+0x47>
  403685:	0f 0b                	ud2
  403687:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40368c:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  403691:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  403698:	00 00 
  40369a:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4036a1:	00 00 
  4036a3:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4036a8:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  4036ad:	0f 83 9f 00 00 00    	jae    403752 <runtime::__type_info_of+0x112>
  4036b3:	48 c7 c0 d0 91 40 00 	mov    $0x4091d0,%rax
  4036ba:	48 8b 00             	mov    (%rax),%rax
  4036bd:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4036c2:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  4036c6:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4036cb:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  4036d1:	0f 95 c0             	setne  %al
  4036d4:	24 01                	and    $0x1,%al
  4036d6:	3c 00                	cmp    $0x0,%al
  4036d8:	74 1d                	je     4036f7 <runtime::__type_info_of+0xb7>
  4036da:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4036df:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4036e4:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  4036e8:	0f 94 c0             	sete   %al
  4036eb:	24 01                	and    $0x1,%al
  4036ed:	3c 00                	cmp    $0x0,%al
  4036ef:	74 06                	je     4036f7 <runtime::__type_info_of+0xb7>
  4036f1:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4036f6:	c3                   	ret
  4036f7:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4036fc:	48 83 c0 01          	add    $0x1,%rax
  403700:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  403705:	0f 92 c0             	setb   %al
  403708:	24 01                	and    $0x1,%al
  40370a:	3c 00                	cmp    $0x0,%al
  40370c:	74 10                	je     40371e <runtime::__type_info_of+0xde>
  40370e:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  403713:	48 83 c0 01          	add    $0x1,%rax
  403717:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40371c:	eb 09                	jmp    403727 <runtime::__type_info_of+0xe7>
  40371e:	31 c0                	xor    %eax,%eax
  403720:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  403725:	eb 00                	jmp    403727 <runtime::__type_info_of+0xe7>
  403727:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40372c:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  403731:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  403736:	48 83 c0 01          	add    $0x1,%rax
  40373a:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40373f:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  403744:	48 83 c0 01          	add    $0x1,%rax
  403748:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40374d:	e9 51 ff ff ff       	jmp    4036a3 <runtime::__type_info_of+0x63>
  403752:	48 c7 c0 d0 91 40 00 	mov    $0x4091d0,%rax
  403759:	48 8b 00             	mov    (%rax),%rax
  40375c:	48 8b 00             	mov    (%rax),%rax
  40375f:	c3                   	ret

0000000000403760 <runtime::default_logger_proc>:
  403760:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  403765:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40376a:	66 44 89 c0          	mov    %r8w,%ax
  40376e:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  403773:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  403778:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40377d:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  403782:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  403787:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40378c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  403791:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  403796:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  40379b:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  4037a0:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  4037a5:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  4037aa:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  4037af:	c3                   	ret

00000000004037b0 <runtime::default_context>:
  4037b0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4037b7:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4037bc:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4037c1:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  4037c6:	31 f6                	xor    %esi,%esi
  4037c8:	ba 70 00 00 00       	mov    $0x70,%edx
  4037cd:	e8 6e d8 ff ff       	call   401040 <memset@plt>
  4037d2:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  4037d7:	e8 24 00 00 00       	call   403800 <runtime::[core.odin]::__init_context>
  4037dc:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4037e1:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  4037e6:	ba 70 00 00 00       	mov    $0x70,%edx
  4037eb:	e8 70 d8 ff ff       	call   401060 <memcpy@plt>
  4037f0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4037f5:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4037fc:	c3                   	ret
  4037fd:	0f 1f 00             	nopl   (%rax)

0000000000403800 <runtime::[core.odin]::__init_context>:
  403800:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  403805:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40380a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40380f:	48 83 f8 00          	cmp    $0x0,%rax
  403813:	0f 94 c0             	sete   %al
  403816:	24 01                	and    $0x1,%al
  403818:	3c 00                	cmp    $0x0,%al
  40381a:	74 01                	je     40381d <runtime::[core.odin]::__init_context+0x1d>
  40381c:	c3                   	ret
  40381d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403822:	48 c7 c1 00 53 40 00 	mov    $0x405300,%rcx
  403829:	48 89 08             	mov    %rcx,(%rax)
  40382c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403831:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  403838:	00 
  403839:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40383e:	48 c7 c1 20 3a 40 00 	mov    $0x403a20,%rcx
  403845:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403849:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40384e:	48 c7 c2 b8 ff ff ff 	mov    $0xffffffffffffffb8,%rdx
  403855:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  40385c:	00 00 
  40385e:	48 01 d1             	add    %rdx,%rcx
  403861:	48 89 48 18          	mov    %rcx,0x18(%rax)
  403865:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40386a:	48 c7 c1 b0 38 40 00 	mov    $0x4038b0,%rcx
  403871:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403875:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40387a:	48 c7 c1 60 37 40 00 	mov    $0x403760,%rcx
  403881:	48 89 48 28          	mov    %rcx,0x28(%rax)
  403885:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40388a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  403891:	00 
  403892:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403897:	48 c7 c1 90 77 40 00 	mov    $0x407790,%rcx
  40389e:	48 89 48 48          	mov    %rcx,0x48(%rax)
  4038a2:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4038a7:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  4038ae:	00 
  4038af:	c3                   	ret

00000000004038b0 <runtime::default_assertion_failure_proc>:
  4038b0:	48 83 ec 48          	sub    $0x48,%rsp
  4038b4:	4c 89 04 24          	mov    %r8,(%rsp)
  4038b8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4038bd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4038c2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4038c7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4038cc:	4c 8b 04 24          	mov    (%rsp),%r8
  4038d0:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4038d5:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4038da:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4038df:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4038e4:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4038e9:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4038ee:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4038f3:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4038f8:	e8 03 00 00 00       	call   403900 <runtime::default_assertion_contextless_failure_proc>
  4038fd:	0f 1f 00             	nopl   (%rax)

0000000000403900 <runtime::default_assertion_contextless_failure_proc>:
  403900:	48 83 ec 48          	sub    $0x48,%rsp
  403904:	4c 89 04 24          	mov    %r8,(%rsp)
  403908:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40390d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  403912:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403917:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40391c:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403921:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  403926:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40392b:	48 8b 3c 24          	mov    (%rsp),%rdi
  40392f:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403934:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403939:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40393e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403943:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  403948:	e8 23 4b 00 00       	call   408470 <runtime::print_caller_location>
  40394d:	bf 33 91 40 00       	mov    $0x409133,%edi
  403952:	be 01 00 00 00       	mov    $0x1,%esi
  403957:	e8 74 46 00 00       	call   407fd0 <runtime::print_string>
  40395c:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403961:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403966:	e8 65 46 00 00       	call   407fd0 <runtime::print_string>
  40396b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403970:	48 83 f8 00          	cmp    $0x0,%rax
  403974:	0f 9f c0             	setg   %al
  403977:	24 01                	and    $0x1,%al
  403979:	3c 00                	cmp    $0x0,%al
  40397b:	74 1e                	je     40399b <runtime::default_assertion_contextless_failure_proc+0x9b>
  40397d:	bf 35 91 40 00       	mov    $0x409135,%edi
  403982:	be 02 00 00 00       	mov    $0x2,%esi
  403987:	e8 44 46 00 00       	call   407fd0 <runtime::print_string>
  40398c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403991:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403996:	e8 35 46 00 00       	call   407fd0 <runtime::print_string>
  40399b:	bf 0a 00 00 00       	mov    $0xa,%edi
  4039a0:	e8 9b 46 00 00       	call   408040 <runtime::print_byte>
  4039a5:	0f 0b                	ud2
  4039a7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4039ae:	00 00 

00000000004039b0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  4039b0:	50                   	push   %rax
  4039b1:	eb 00                	jmp    4039b3 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x3>
  4039b3:	48 c7 c0 b8 ff ff ff 	mov    $0xffffffffffffffb8,%rax
  4039ba:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  4039c1:	00 00 
  4039c3:	48 01 c7             	add    %rax,%rdi
  4039c6:	e8 05 00 00 00       	call   4039d0 <runtime::default_temp_allocator_destroy>
  4039cb:	58                   	pop    %rax
  4039cc:	c3                   	ret
  4039cd:	0f 1f 00             	nopl   (%rax)

00000000004039d0 <runtime::default_temp_allocator_destroy>:
  4039d0:	48 83 ec 18          	sub    $0x18,%rsp
  4039d4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4039d9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4039de:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4039e3:	48 83 f8 00          	cmp    $0x0,%rax
  4039e7:	0f 95 c0             	setne  %al
  4039ea:	24 01                	and    $0x1,%al
  4039ec:	3c 00                	cmp    $0x0,%al
  4039ee:	74 25                	je     403a15 <runtime::default_temp_allocator_destroy+0x45>
  4039f0:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4039f5:	48 be a0 91 40 00 00 	movabs $0x4091a0,%rsi
  4039fc:	00 00 00 
  4039ff:	e8 bc f1 ff ff       	call   402bc0 <runtime::arena_destroy>
  403a04:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403a09:	31 f6                	xor    %esi,%esi
  403a0b:	ba 38 00 00 00       	mov    $0x38,%edx
  403a10:	e8 2b d6 ff ff       	call   401040 <memset@plt>
  403a15:	48 83 c4 18          	add    $0x18,%rsp
  403a19:	c3                   	ret
  403a1a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000403a20 <runtime::default_temp_allocator_proc>:
  403a20:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403a27:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  403a2c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  403a31:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403a36:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  403a3b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  403a40:	40 88 f0             	mov    %sil,%al
  403a43:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  403a47:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  403a4e:	00 
  403a4f:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403a54:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  403a5b:	00 
  403a5c:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403a61:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403a68:	00 
  403a69:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  403a6e:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  403a73:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403a78:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403a7d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403a82:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  403a86:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  403a8b:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  403a90:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  403a95:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  403a9c:	00 
  403a9d:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  403aa4:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  403aab:	00 
  403aac:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  403ab3:	00 
  403ab4:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  403abb:	00 
  403abc:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  403ac3:	00 
  403ac4:	0f 57 c0             	xorps  %xmm0,%xmm0
  403ac7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  403ace:	00 
  403acf:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  403ad6:	00 
  403ad7:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  403ade:	00 
  403adf:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  403ae6:	00 
  403ae7:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  403aee:	00 
  403aef:	48 89 e6             	mov    %rsp,%rsi
  403af2:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  403af6:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  403afd:	00 
  403afe:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  403b02:	4c 89 16             	mov    %r10,(%rsi)
  403b05:	0f b6 f0             	movzbl %al,%esi
  403b08:	e8 53 f1 ff ff       	call   402c60 <runtime::arena_allocator_proc>
  403b0d:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  403b12:	40 88 c7             	mov    %al,%dil
  403b15:	40 88 f8             	mov    %dil,%al
  403b18:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  403b1f:	00 
  403b20:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  403b27:	00 
  403b28:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  403b2f:	00 
  403b30:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403b37:	00 
  403b38:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  403b3f:	00 
  403b40:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403b44:	48 89 11             	mov    %rdx,(%rcx)
  403b47:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  403b4e:	c3                   	ret
  403b4f:	90                   	nop

0000000000403b50 <runtime::[os_specific_linux.odin]::_stderr_write>:
  403b50:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  403b55:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  403b5a:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  403b5f:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  403b64:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  403b69:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  403b6e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  403b73:	b8 01 00 00 00       	mov    $0x1,%eax
  403b78:	bf 02 00 00 00       	mov    $0x2,%edi
  403b7d:	0f 05                	syscall
  403b7f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  403b84:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  403b8a:	0f 9c c0             	setl   %al
  403b8d:	24 01                	and    $0x1,%al
  403b8f:	3c 00                	cmp    $0x0,%al
  403b91:	74 26                	je     403bb9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  403b93:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  403b9a:	ff ff 
  403b9c:	0f 9f c0             	setg   %al
  403b9f:	24 01                	and    $0x1,%al
  403ba1:	3c 00                	cmp    $0x0,%al
  403ba3:	74 14                	je     403bb9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  403ba5:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  403baa:	31 c0                	xor    %eax,%eax
  403bac:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  403bb1:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  403bb8:	c3                   	ret
  403bb9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  403bbe:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  403bc3:	48 89 08             	mov    %rcx,(%rax)
  403bc6:	31 c0                	xor    %eax,%eax
  403bc8:	c3                   	ret
  403bc9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000403bd0 <__$startup_runtime>:
  403bd0:	50                   	push   %rax
  403bd1:	48 89 3c 24          	mov    %rdi,(%rsp)
  403bd5:	eb 00                	jmp    403bd7 <__$startup_runtime+0x7>
  403bd7:	48 8b 3c 24          	mov    (%rsp),%rdi
  403bdb:	e8 80 00 00 00       	call   403c60 <__$startup$os::args>
  403be0:	58                   	pop    %rax
  403be1:	c3                   	ret
  403be2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403be9:	1f 84 00 00 00 00 00 

0000000000403bf0 <__$equal-17266089877833985010>:
  403bf0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  403bf5:	48 89 74 24 f8       	mov    %rsi,-0x8(%rsp)
  403bfa:	eb 00                	jmp    403bfc <__$equal-17266089877833985010+0xc>
  403bfc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  403c01:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  403c06:	48 39 c8             	cmp    %rcx,%rax
  403c09:	75 03                	jne    403c0e <__$equal-17266089877833985010+0x1e>
  403c0b:	b0 01                	mov    $0x1,%al
  403c0d:	c3                   	ret
  403c0e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403c13:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  403c18:	f3 0f 10 01          	movss  (%rcx),%xmm0
  403c1c:	0f 2e 00             	ucomiss (%rax),%xmm0
  403c1f:	75 33                	jne    403c54 <__$equal-17266089877833985010+0x64>
  403c21:	7a 31                	jp     403c54 <__$equal-17266089877833985010+0x64>
  403c23:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403c28:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  403c2d:	f3 0f 10 41 04       	movss  0x4(%rcx),%xmm0
  403c32:	0f 2e 40 04          	ucomiss 0x4(%rax),%xmm0
  403c36:	75 1c                	jne    403c54 <__$equal-17266089877833985010+0x64>
  403c38:	7a 1a                	jp     403c54 <__$equal-17266089877833985010+0x64>
  403c3a:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403c3f:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  403c44:	f3 0f 10 41 08       	movss  0x8(%rcx),%xmm0
  403c49:	0f 2e 40 08          	ucomiss 0x8(%rax),%xmm0
  403c4d:	75 05                	jne    403c54 <__$equal-17266089877833985010+0x64>
  403c4f:	7a 03                	jp     403c54 <__$equal-17266089877833985010+0x64>
  403c51:	b0 01                	mov    $0x1,%al
  403c53:	c3                   	ret
  403c54:	31 c0                	xor    %eax,%eax
  403c56:	c3                   	ret
  403c57:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  403c5e:	00 00 

0000000000403c60 <__$startup$os::args>:
  403c60:	50                   	push   %rax
  403c61:	eb 00                	jmp    403c63 <__$startup$os::args+0x3>
  403c63:	e8 98 09 00 00       	call   404600 <os::[os_linux.odin]::_alloc_command_line_arguments>
  403c68:	48 89 15 09 84 00 00 	mov    %rdx,0x8409(%rip)        # 40c078 <os::args+0x8>
  403c6f:	48 89 05 fa 83 00 00 	mov    %rax,0x83fa(%rip)        # 40c070 <os::args>
  403c76:	58                   	pop    %rax
  403c77:	c3                   	ret
  403c78:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  403c7f:	00 

0000000000403c80 <__$cleanup_runtime>:
  403c80:	50                   	push   %rax
  403c81:	eb 00                	jmp    403c83 <__$cleanup_runtime+0x3>
  403c83:	e8 58 0c 00 00       	call   4048e0 <os::[os_linux.odin]::_delete_command_line_arguments>
  403c88:	e8 23 fd ff ff       	call   4039b0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  403c8d:	58                   	pop    %rax
  403c8e:	c3                   	ret
  403c8f:	90                   	nop

0000000000403c90 <runtime::bounds_trap>:
  403c90:	eb 00                	jmp    403c92 <runtime::bounds_trap+0x2>
  403c92:	0f 0b                	ud2
  403c94:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403c9b:	00 00 00 00 00 

0000000000403ca0 <runtime::bounds_check_error>:
  403ca0:	48 83 ec 58          	sub    $0x58,%rsp
  403ca4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403ca9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403cae:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403cb2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403cb6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  403cbb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403cc0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403cc5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403cca:	8b 54 24 18          	mov    0x18(%rsp),%edx
  403cce:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403cd2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403cd7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403cdc:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403ce1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403ce6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  403cea:	89 54 24 40          	mov    %edx,0x40(%rsp)
  403cee:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403cf3:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403cf8:	48 39 c8             	cmp    %rcx,%rax
  403cfb:	0f 92 c0             	setb   %al
  403cfe:	24 01                	and    $0x1,%al
  403d00:	3c 00                	cmp    $0x0,%al
  403d02:	74 05                	je     403d09 <runtime::bounds_check_error+0x69>
  403d04:	48 83 c4 58          	add    $0x58,%rsp
  403d08:	c3                   	ret
  403d09:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403d0e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  403d13:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  403d17:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  403d1b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403d20:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403d25:	e8 76 05 00 00       	call   4042a0 <runtime::bounds_check_error.handle_error-0>
  403d2a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000403d30 <runtime::slice_handle_error>:
  403d30:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403d37:	4c 89 0c 24          	mov    %r9,(%rsp)
  403d3b:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  403d40:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  403d44:	89 54 24 14          	mov    %edx,0x14(%rsp)
  403d48:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403d4d:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  403d52:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403d59:	00 
  403d5a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  403d5f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403d64:	4c 8b 04 24          	mov    (%rsp),%r8
  403d68:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403d6d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  403d71:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  403d75:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  403d7a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403d7f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403d84:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  403d8b:	00 
  403d8c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  403d90:	89 44 24 70          	mov    %eax,0x70(%rsp)
  403d94:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  403d99:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  403d9e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  403da3:	0f 57 c0             	xorps  %xmm0,%xmm0
  403da6:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403dab:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  403db0:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403db7:	00 00 
  403db9:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403dbe:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  403dc3:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403dca:	00 00 
  403dcc:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403dd1:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403dd6:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  403dda:	89 44 24 44          	mov    %eax,0x44(%rsp)
  403dde:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  403de3:	e8 88 46 00 00       	call   408470 <runtime::print_caller_location>
  403de8:	bf 92 92 40 00       	mov    $0x409292,%edi
  403ded:	be 17 00 00 00       	mov    $0x17,%esi
  403df2:	e8 d9 41 00 00       	call   407fd0 <runtime::print_string>
  403df7:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403dfc:	e8 ef 44 00 00       	call   4082f0 <runtime::print_i64>
  403e01:	bf aa 92 40 00       	mov    $0x4092aa,%edi
  403e06:	be 01 00 00 00       	mov    $0x1,%esi
  403e0b:	e8 c0 41 00 00       	call   407fd0 <runtime::print_string>
  403e10:	48 8b 3c 24          	mov    (%rsp),%rdi
  403e14:	e8 d7 44 00 00       	call   4082f0 <runtime::print_i64>
  403e19:	bf ac 92 40 00       	mov    $0x4092ac,%edi
  403e1e:	be 15 00 00 00       	mov    $0x15,%esi
  403e23:	e8 a8 41 00 00       	call   407fd0 <runtime::print_string>
  403e28:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403e2d:	e8 be 44 00 00       	call   4082f0 <runtime::print_i64>
  403e32:	bf 0a 00 00 00       	mov    $0xa,%edi
  403e37:	e8 04 42 00 00       	call   408040 <runtime::print_byte>
  403e3c:	e8 4f fe ff ff       	call   403c90 <runtime::bounds_trap>
  403e41:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403e48:	0f 1f 84 00 00 00 00 
  403e4f:	00 

0000000000403e50 <runtime::multi_pointer_slice_handle_error>:
  403e50:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403e57:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403e5c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403e61:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403e65:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403e69:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  403e6e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403e73:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403e78:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  403e7d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  403e81:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  403e85:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  403e8a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403e8f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403e94:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  403e9b:	00 
  403e9c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  403ea0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  403ea4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  403ea9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  403eae:	0f 57 c0             	xorps  %xmm0,%xmm0
  403eb1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403eb6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  403ebb:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403ec2:	00 00 
  403ec4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403ec9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  403ece:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403ed5:	00 00 
  403ed7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403edc:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403ee1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  403ee5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  403ee9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  403eee:	e8 7d 45 00 00       	call   408470 <runtime::print_caller_location>
  403ef3:	bf 92 92 40 00       	mov    $0x409292,%edi
  403ef8:	be 17 00 00 00       	mov    $0x17,%esi
  403efd:	e8 ce 40 00 00       	call   407fd0 <runtime::print_string>
  403f02:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403f07:	e8 e4 43 00 00       	call   4082f0 <runtime::print_i64>
  403f0c:	bf aa 92 40 00       	mov    $0x4092aa,%edi
  403f11:	be 01 00 00 00       	mov    $0x1,%esi
  403f16:	e8 b5 40 00 00       	call   407fd0 <runtime::print_string>
  403f1b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403f20:	e8 cb 43 00 00       	call   4082f0 <runtime::print_i64>
  403f25:	bf 0a 00 00 00       	mov    $0xa,%edi
  403f2a:	e8 11 41 00 00       	call   408040 <runtime::print_byte>
  403f2f:	e8 5c fd ff ff       	call   403c90 <runtime::bounds_trap>
  403f34:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403f3b:	00 00 00 00 00 

0000000000403f40 <runtime::multi_pointer_slice_expr_error>:
  403f40:	48 83 ec 58          	sub    $0x58,%rsp
  403f44:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403f49:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403f4e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403f52:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403f56:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  403f5b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403f60:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403f65:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403f6a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  403f6e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403f72:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403f77:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403f7c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403f81:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403f86:	89 74 24 44          	mov    %esi,0x44(%rsp)
  403f8a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  403f8e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403f93:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403f98:	48 39 c8             	cmp    %rcx,%rax
  403f9b:	0f 9e c0             	setle  %al
  403f9e:	24 01                	and    $0x1,%al
  403fa0:	3c 00                	cmp    $0x0,%al
  403fa2:	74 05                	je     403fa9 <runtime::multi_pointer_slice_expr_error+0x69>
  403fa4:	48 83 c4 58          	add    $0x58,%rsp
  403fa8:	c3                   	ret
  403fa9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403fae:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  403fb3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  403fb7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  403fbb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403fc0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403fc5:	e8 86 fe ff ff       	call   403e50 <runtime::multi_pointer_slice_handle_error>
  403fca:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000403fd0 <runtime::slice_expr_error_hi>:
  403fd0:	48 83 ec 58          	sub    $0x58,%rsp
  403fd4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403fd9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403fde:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403fe2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403fe6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  403feb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403ff0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  403ff5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403ffa:	8b 54 24 18          	mov    0x18(%rsp),%edx
  403ffe:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  404002:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  404007:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40400c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  404011:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  404016:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40401a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40401e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  404023:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  404028:	31 c0                	xor    %eax,%eax
  40402a:	48 39 c8             	cmp    %rcx,%rax
  40402d:	0f 9e c0             	setle  %al
  404030:	24 01                	and    $0x1,%al
  404032:	3c 00                	cmp    $0x0,%al
  404034:	74 1b                	je     404051 <runtime::slice_expr_error_hi+0x81>
  404036:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40403b:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  404040:	48 39 c8             	cmp    %rcx,%rax
  404043:	0f 9e c0             	setle  %al
  404046:	24 01                	and    $0x1,%al
  404048:	3c 00                	cmp    $0x0,%al
  40404a:	74 05                	je     404051 <runtime::slice_expr_error_hi+0x81>
  40404c:	48 83 c4 58          	add    $0x58,%rsp
  404050:	c3                   	ret
  404051:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  404056:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40405a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40405e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  404063:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  404068:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40406d:	48 89 e0             	mov    %rsp,%rax
  404070:	4c 89 00             	mov    %r8,(%rax)
  404073:	31 c0                	xor    %eax,%eax
  404075:	41 89 c0             	mov    %eax,%r8d
  404078:	e8 b3 fc ff ff       	call   403d30 <runtime::slice_handle_error>
  40407d:	0f 1f 00             	nopl   (%rax)

0000000000404080 <runtime::slice_expr_error_lo_hi>:
  404080:	48 83 ec 68          	sub    $0x68,%rsp
  404084:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  404089:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40408e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  404092:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  404096:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40409b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4040a0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4040a5:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4040aa:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4040af:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4040b4:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4040b9:	8b 74 24 18          	mov    0x18(%rsp),%esi
  4040bd:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  4040c1:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  4040c6:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  4040cb:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  4040d0:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  4040d5:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  4040d9:	89 74 24 50          	mov    %esi,0x50(%rsp)
  4040dd:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4040e2:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4040e7:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4040ec:	31 c0                	xor    %eax,%eax
  4040ee:	48 39 c8             	cmp    %rcx,%rax
  4040f1:	0f 9e c0             	setle  %al
  4040f4:	24 01                	and    $0x1,%al
  4040f6:	3c 00                	cmp    $0x0,%al
  4040f8:	74 47                	je     404141 <runtime::slice_expr_error_lo_hi+0xc1>
  4040fa:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4040ff:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  404104:	48 39 c8             	cmp    %rcx,%rax
  404107:	0f 9e c0             	setle  %al
  40410a:	24 01                	and    $0x1,%al
  40410c:	3c 00                	cmp    $0x0,%al
  40410e:	74 31                	je     404141 <runtime::slice_expr_error_lo_hi+0xc1>
  404110:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404115:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40411a:	48 39 c8             	cmp    %rcx,%rax
  40411d:	0f 9e c0             	setle  %al
  404120:	24 01                	and    $0x1,%al
  404122:	3c 00                	cmp    $0x0,%al
  404124:	74 1b                	je     404141 <runtime::slice_expr_error_lo_hi+0xc1>
  404126:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40412b:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  404130:	48 39 c8             	cmp    %rcx,%rax
  404133:	0f 9e c0             	setle  %al
  404136:	24 01                	and    $0x1,%al
  404138:	3c 00                	cmp    $0x0,%al
  40413a:	74 05                	je     404141 <runtime::slice_expr_error_lo_hi+0xc1>
  40413c:	48 83 c4 68          	add    $0x68,%rsp
  404140:	c3                   	ret
  404141:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  404146:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40414b:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40414f:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  404153:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  404158:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40415d:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  404162:	48 89 e0             	mov    %rsp,%rax
  404165:	4c 89 10             	mov    %r10,(%rax)
  404168:	e8 c3 fb ff ff       	call   403d30 <runtime::slice_handle_error>
  40416d:	0f 1f 00             	nopl   (%rax)

0000000000404170 <runtime::matrix_bounds_check_error>:
  404170:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  404177:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40417c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  404181:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  404185:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  404189:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  40418e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  404193:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  40419a:	00 
  40419b:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4041a0:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  4041a7:	00 
  4041a8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4041ad:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4041b2:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4041b7:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  4041bc:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4041c1:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  4041c5:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  4041ca:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  4041cf:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  4041d4:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  4041d9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  4041e0:	00 
  4041e1:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  4041e6:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  4041ea:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4041ef:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  4041f4:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4041f9:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  4041fe:	48 39 c8             	cmp    %rcx,%rax
  404201:	0f 92 c0             	setb   %al
  404204:	24 01                	and    $0x1,%al
  404206:	3c 00                	cmp    $0x0,%al
  404208:	74 1e                	je     404228 <runtime::matrix_bounds_check_error+0xb8>
  40420a:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40420f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  404214:	48 39 c8             	cmp    %rcx,%rax
  404217:	0f 92 c0             	setb   %al
  40421a:	24 01                	and    $0x1,%al
  40421c:	3c 00                	cmp    $0x0,%al
  40421e:	74 08                	je     404228 <runtime::matrix_bounds_check_error+0xb8>
  404220:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  404227:	c3                   	ret
  404228:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40422d:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  404232:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  404236:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  40423a:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40423f:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  404244:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  404249:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  40424e:	48 89 e0             	mov    %rsp,%rax
  404251:	4c 89 58 08          	mov    %r11,0x8(%rax)
  404255:	4c 89 10             	mov    %r10,(%rax)
  404258:	e8 33 01 00 00       	call   404390 <runtime::matrix_bounds_check_error.handle_error-0>
  40425d:	0f 1f 00             	nopl   (%rax)

0000000000404260 <runtime::make_slice_error_loc>:
  404260:	48 83 ec 18          	sub    $0x18,%rsp
  404264:	48 89 3c 24          	mov    %rdi,(%rsp)
  404268:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40426d:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  404272:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  404277:	31 c0                	xor    %eax,%eax
  404279:	48 39 c8             	cmp    %rcx,%rax
  40427c:	0f 9e c0             	setle  %al
  40427f:	24 01                	and    $0x1,%al
  404281:	3c 00                	cmp    $0x0,%al
  404283:	74 05                	je     40428a <runtime::make_slice_error_loc+0x2a>
  404285:	48 83 c4 18          	add    $0x18,%rsp
  404289:	c3                   	ret
  40428a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40428f:	48 8b 3c 24          	mov    (%rsp),%rdi
  404293:	e8 68 02 00 00       	call   404500 <runtime::make_slice_error_loc.handle_error-0>
  404298:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40429f:	00 

00000000004042a0 <runtime::bounds_check_error.handle_error-0>:
  4042a0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4042a7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4042ac:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4042b1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4042b5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4042b9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4042be:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4042c3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4042c8:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4042cd:	8b 44 24 18          	mov    0x18(%rsp),%eax
  4042d1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4042d5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4042da:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4042df:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4042e4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  4042eb:	00 
  4042ec:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  4042f0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  4042f4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  4042f9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  4042fe:	0f 57 c0             	xorps  %xmm0,%xmm0
  404301:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  404306:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40430b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  404312:	00 00 
  404314:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  404319:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40431e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  404325:	00 00 
  404327:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40432c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  404331:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  404335:	89 44 24 44          	mov    %eax,0x44(%rsp)
  404339:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40433e:	e8 2d 41 00 00       	call   408470 <runtime::print_caller_location>
  404343:	bf c2 92 40 00       	mov    $0x4092c2,%edi
  404348:	be 07 00 00 00       	mov    $0x7,%esi
  40434d:	e8 7e 3c 00 00       	call   407fd0 <runtime::print_string>
  404352:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  404357:	e8 94 3f 00 00       	call   4082f0 <runtime::print_i64>
  40435c:	bf ac 92 40 00       	mov    $0x4092ac,%edi
  404361:	be 15 00 00 00       	mov    $0x15,%esi
  404366:	e8 65 3c 00 00       	call   407fd0 <runtime::print_string>
  40436b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  404370:	e8 7b 3f 00 00       	call   4082f0 <runtime::print_i64>
  404375:	bf 0a 00 00 00       	mov    $0xa,%edi
  40437a:	e8 c1 3c 00 00       	call   408040 <runtime::print_byte>
  40437f:	e8 0c f9 ff ff       	call   403c90 <runtime::bounds_trap>
  404384:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40438b:	00 00 00 00 00 

0000000000404390 <runtime::matrix_bounds_check_error.handle_error-0>:
  404390:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  404397:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40439c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4043a1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4043a5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4043a9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4043ae:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4043b3:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  4043ba:	00 
  4043bb:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4043c0:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4043c7:	00 
  4043c8:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4043cd:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4043d2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  4043d7:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4043dc:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  4043e1:	8b 44 24 18          	mov    0x18(%rsp),%eax
  4043e5:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4043e9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4043ee:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4043f3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  4043fa:	00 
  4043fb:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  404402:	00 
  404403:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  40440a:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  404411:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  404418:	00 
  404419:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  404420:	00 
  404421:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  404426:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  40442b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40442e:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  404433:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  404438:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40443f:	00 00 
  404441:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  404446:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40444b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  404452:	00 00 
  404454:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  404459:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40445e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  404462:	89 44 24 54          	mov    %eax,0x54(%rsp)
  404466:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  40446b:	e8 00 40 00 00       	call   408470 <runtime::print_caller_location>
  404470:	bf ca 92 40 00       	mov    $0x4092ca,%edi
  404475:	be 11 00 00 00       	mov    $0x11,%esi
  40447a:	e8 51 3b 00 00       	call   407fd0 <runtime::print_string>
  40447f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  404484:	e8 67 3e 00 00       	call   4082f0 <runtime::print_i64>
  404489:	bf dc 92 40 00       	mov    $0x4092dc,%edi
  40448e:	be 02 00 00 00       	mov    $0x2,%esi
  404493:	e8 38 3b 00 00       	call   407fd0 <runtime::print_string>
  404498:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40449d:	e8 4e 3e 00 00       	call   4082f0 <runtime::print_i64>
  4044a2:	bf df 92 40 00       	mov    $0x4092df,%edi
  4044a7:	be 16 00 00 00       	mov    $0x16,%esi
  4044ac:	e8 1f 3b 00 00       	call   407fd0 <runtime::print_string>
  4044b1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4044b6:	e8 35 3e 00 00       	call   4082f0 <runtime::print_i64>
  4044bb:	bf f6 92 40 00       	mov    $0x4092f6,%edi
  4044c0:	be 06 00 00 00       	mov    $0x6,%esi
  4044c5:	e8 06 3b 00 00       	call   407fd0 <runtime::print_string>
  4044ca:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4044cf:	e8 1c 3e 00 00       	call   4082f0 <runtime::print_i64>
  4044d4:	bf fd 92 40 00       	mov    $0x4092fd,%edi
  4044d9:	be 01 00 00 00       	mov    $0x1,%esi
  4044de:	e8 ed 3a 00 00       	call   407fd0 <runtime::print_string>
  4044e3:	bf 0a 00 00 00       	mov    $0xa,%edi
  4044e8:	e8 53 3b 00 00       	call   408040 <runtime::print_byte>
  4044ed:	e8 9e f7 ff ff       	call   403c90 <runtime::bounds_trap>
  4044f2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4044f9:	1f 84 00 00 00 00 00 

0000000000404500 <runtime::make_slice_error_loc.handle_error-0>:
  404500:	48 83 ec 18          	sub    $0x18,%rsp
  404504:	48 89 3c 24          	mov    %rdi,(%rsp)
  404508:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40450d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404512:	48 8b 3c 24          	mov    (%rsp),%rdi
  404516:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40451b:	e8 50 3f 00 00       	call   408470 <runtime::print_caller_location>
  404520:	bf ff 92 40 00       	mov    $0x4092ff,%edi
  404525:	be 20 00 00 00       	mov    $0x20,%esi
  40452a:	e8 a1 3a 00 00       	call   407fd0 <runtime::print_string>
  40452f:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  404534:	e8 b7 3d 00 00       	call   4082f0 <runtime::print_i64>
  404539:	bf 0a 00 00 00       	mov    $0xa,%edi
  40453e:	e8 fd 3a 00 00       	call   408040 <runtime::print_byte>
  404543:	e8 48 f7 ff ff       	call   403c90 <runtime::bounds_trap>
  404548:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40454f:	00 

0000000000404550 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  404550:	48 83 ec 18          	sub    $0x18,%rsp
  404554:	48 89 3c 24          	mov    %rdi,(%rsp)
  404558:	40 88 f0             	mov    %sil,%al
  40455b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40455f:	48 8b 04 24          	mov    (%rsp),%rax
  404563:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  404567:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40456c:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  404570:	48 83 f8 00          	cmp    $0x0,%rax
  404574:	0f 9e c0             	setle  %al
  404577:	24 01                	and    $0x1,%al
  404579:	3c 00                	cmp    $0x0,%al
  40457b:	74 07                	je     404584 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  40457d:	31 c0                	xor    %eax,%eax
  40457f:	48 83 c4 18          	add    $0x18,%rsp
  404583:	c3                   	ret
  404584:	8a 44 24 0e          	mov    0xe(%rsp),%al
  404588:	3c 00                	cmp    $0x0,%al
  40458a:	74 13                	je     40459f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  40458c:	48 8b 34 24          	mov    (%rsp),%rsi
  404590:	bf 01 00 00 00       	mov    $0x1,%edi
  404595:	e8 b6 ca ff ff       	call   401050 <calloc@plt>
  40459a:	48 83 c4 18          	add    $0x18,%rsp
  40459e:	c3                   	ret
  40459f:	48 8b 3c 24          	mov    (%rsp),%rdi
  4045a3:	e8 c8 ca ff ff       	call   401070 <malloc@plt>
  4045a8:	48 83 c4 18          	add    $0x18,%rsp
  4045ac:	c3                   	ret
  4045ad:	0f 1f 00             	nopl   (%rax)

00000000004045b0 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  4045b0:	48 83 ec 28          	sub    $0x28,%rsp
  4045b4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4045b9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4045be:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4045c3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4045c8:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4045cd:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4045d2:	e8 a9 ca ff ff       	call   401080 <realloc@plt>
  4045d7:	48 83 c4 28          	add    $0x28,%rsp
  4045db:	c3                   	ret
  4045dc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004045e0 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  4045e0:	48 83 ec 18          	sub    $0x18,%rsp
  4045e4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4045e9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4045ee:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4045f3:	e8 38 ca ff ff       	call   401030 <free@plt>
  4045f8:	48 83 c4 18          	add    $0x18,%rsp
  4045fc:	c3                   	ret
  4045fd:	0f 1f 00             	nopl   (%rax)

0000000000404600 <os::[os_linux.odin]::_alloc_command_line_arguments>:
  404600:	48 81 ec c8 01 00 00 	sub    $0x1c8,%rsp
  404607:	0f 57 c0             	xorps  %xmm0,%xmm0
  40460a:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40460f:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  404616:	00 
  404617:	0f 29 84 24 a0 01 00 	movaps %xmm0,0x1a0(%rsp)
  40461e:	00 
  40461f:	0f 29 84 24 90 01 00 	movaps %xmm0,0x190(%rsp)
  404626:	00 
  404627:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  40462e:	00 
  40462f:	0f 29 84 24 70 01 00 	movaps %xmm0,0x170(%rsp)
  404636:	00 
  404637:	0f 29 84 24 60 01 00 	movaps %xmm0,0x160(%rsp)
  40463e:	00 
  40463f:	0f 29 84 24 50 01 00 	movaps %xmm0,0x150(%rsp)
  404646:	00 
  404647:	48 8d bc 24 50 01 00 	lea    0x150(%rsp),%rdi
  40464e:	00 
  40464f:	48 89 7c 24 68       	mov    %rdi,0x68(%rsp)
  404654:	e8 a7 f1 ff ff       	call   403800 <runtime::[core.odin]::__init_context>
  404659:	0f 28 44 24 50       	movaps 0x50(%rsp),%xmm0
  40465e:	0f 29 84 24 40 01 00 	movaps %xmm0,0x140(%rsp)
  404665:	00 
  404666:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  40466d:	00 
  40466e:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  404675:	00 
  404676:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  40467d:	00 
  40467e:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  404685:	00 
  404686:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  40468d:	00 
  40468e:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  404695:	00 
  404696:	48 8d bc 24 e0 00 00 	lea    0xe0(%rsp),%rdi
  40469d:	00 
  40469e:	e8 0d f1 ff ff       	call   4037b0 <runtime::default_context>
  4046a3:	0f 28 44 24 50       	movaps 0x50(%rsp),%xmm0
  4046a8:	4c 8b 4c 24 68       	mov    0x68(%rsp),%r9
  4046ad:	0f 28 8c 24 e0 00 00 	movaps 0xe0(%rsp),%xmm1
  4046b4:	00 
  4046b5:	0f 28 94 24 f0 00 00 	movaps 0xf0(%rsp),%xmm2
  4046bc:	00 
  4046bd:	0f 28 9c 24 00 01 00 	movaps 0x100(%rsp),%xmm3
  4046c4:	00 
  4046c5:	0f 28 a4 24 10 01 00 	movaps 0x110(%rsp),%xmm4
  4046cc:	00 
  4046cd:	0f 28 ac 24 20 01 00 	movaps 0x120(%rsp),%xmm5
  4046d4:	00 
  4046d5:	0f 28 b4 24 30 01 00 	movaps 0x130(%rsp),%xmm6
  4046dc:	00 
  4046dd:	0f 28 bc 24 40 01 00 	movaps 0x140(%rsp),%xmm7
  4046e4:	00 
  4046e5:	0f 29 bc 24 b0 01 00 	movaps %xmm7,0x1b0(%rsp)
  4046ec:	00 
  4046ed:	0f 29 b4 24 a0 01 00 	movaps %xmm6,0x1a0(%rsp)
  4046f4:	00 
  4046f5:	0f 29 ac 24 90 01 00 	movaps %xmm5,0x190(%rsp)
  4046fc:	00 
  4046fd:	0f 29 a4 24 80 01 00 	movaps %xmm4,0x180(%rsp)
  404704:	00 
  404705:	0f 29 9c 24 70 01 00 	movaps %xmm3,0x170(%rsp)
  40470c:	00 
  40470d:	0f 29 94 24 60 01 00 	movaps %xmm2,0x160(%rsp)
  404714:	00 
  404715:	0f 29 8c 24 50 01 00 	movaps %xmm1,0x150(%rsp)
  40471c:	00 
  40471d:	48 c7 c0 60 c0 40 00 	mov    $0x40c060,%rax
  404724:	48 8b 78 08          	mov    0x8(%rax),%rdi
  404728:	48 8b 84 24 50 01 00 	mov    0x150(%rsp),%rax
  40472f:	00 
  404730:	48 8b 8c 24 58 01 00 	mov    0x158(%rsp),%rcx
  404737:	00 
  404738:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  40473f:	00 
  404740:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404747:	00 
  404748:	48 8b b4 24 d0 00 00 	mov    0xd0(%rsp),%rsi
  40474f:	00 
  404750:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  404757:	00 
  404758:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  40475f:	00 
  404760:	b9 70 93 40 00       	mov    $0x409370,%ecx
  404765:	4c 8d 84 24 c0 00 00 	lea    0xc0(%rsp),%r8
  40476c:	00 
  40476d:	e8 2e 36 00 00       	call   407da0 <runtime::make_slice:proc(T:$[]string,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]string,err:runtime::Allocator_Error)>
  404772:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404779:	00 
  40477a:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  404781:	00 
  404782:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  404789:	00 
  40478a:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  404791:	00 
  404792:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  404799:	00 
  40479a:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4047a1:	00 
  4047a2:	48 c7 84 24 88 00 00 	movq   $0xffffffffffffffff,0x88(%rsp)
  4047a9:	00 ff ff ff ff 
  4047ae:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  4047b5:	00 
  4047b6:	48 83 c0 01          	add    $0x1,%rax
  4047ba:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  4047c1:	00 
  4047c2:	48 3b 84 24 90 00 00 	cmp    0x90(%rsp),%rax
  4047c9:	00 
  4047ca:	0f 8d da 00 00 00    	jge    4048aa <os::[os_linux.odin]::_alloc_command_line_arguments+0x2aa>
  4047d0:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  4047d7:	00 
  4047d8:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4047df:	00 
  4047e0:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  4047e7:	00 
  4047e8:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  4047ed:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  4047f4:	00 
  4047f5:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4047fa:	4c 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%r9
  404801:	00 
  404802:	bf 20 93 40 00       	mov    $0x409320,%edi
  404807:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40480c:	be 23 00 00 00       	mov    $0x23,%esi
  404811:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  404816:	ba 51 04 00 00       	mov    $0x451,%edx
  40481b:	89 54 24 24          	mov    %edx,0x24(%rsp)
  40481f:	b9 07 00 00 00       	mov    $0x7,%ecx
  404824:	e8 77 f4 ff ff       	call   403ca0 <runtime::bounds_check_error>
  404829:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40482e:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  404833:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404838:	8b 54 24 24          	mov    0x24(%rsp),%edx
  40483c:	48 c1 e0 04          	shl    $0x4,%rax
  404840:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404845:	48 c7 c0 60 c0 40 00 	mov    $0x40c060,%rax
  40484c:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  404853:	00 
  404854:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  404859:	48 8b 08             	mov    (%rax),%rcx
  40485c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404861:	4c 8b 48 08          	mov    0x8(%rax),%r9
  404865:	b9 22 00 00 00       	mov    $0x22,%ecx
  40486a:	e8 31 f4 ff ff       	call   403ca0 <runtime::bounds_check_error>
  40486f:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  404874:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404879:	48 8b 3c c8          	mov    (%rax,%rcx,8),%rdi
  40487d:	e8 0e 22 00 00       	call   406a90 <runtime::cstring_to_string>
  404882:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  404887:	48 89 c6             	mov    %rax,%rsi
  40488a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40488f:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  404894:	48 89 d6             	mov    %rdx,%rsi
  404897:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40489c:	48 89 74 08 08       	mov    %rsi,0x8(%rax,%rcx,1)
  4048a1:	48 89 14 08          	mov    %rdx,(%rax,%rcx,1)
  4048a5:	e9 04 ff ff ff       	jmp    4047ae <os::[os_linux.odin]::_alloc_command_line_arguments+0x1ae>
  4048aa:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  4048b1:	00 
  4048b2:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4048b9:	00 
  4048ba:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  4048bf:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4048c4:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4048c9:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  4048ce:	48 81 c4 c8 01 00 00 	add    $0x1c8,%rsp
  4048d5:	c3                   	ret
  4048d6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4048dd:	00 00 00 

00000000004048e0 <os::[os_linux.odin]::_delete_command_line_arguments>:
  4048e0:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  4048e7:	0f 57 c0             	xorps  %xmm0,%xmm0
  4048ea:	0f 29 04 24          	movaps %xmm0,(%rsp)
  4048ee:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  4048f5:	00 
  4048f6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4048fd:	00 
  4048fe:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  404905:	00 
  404906:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  40490d:	00 
  40490e:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  404915:	00 
  404916:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40491d:	00 
  40491e:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  404925:	00 
  404926:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40492d:	00 
  40492e:	e8 cd ee ff ff       	call   403800 <runtime::[core.odin]::__init_context>
  404933:	0f 28 04 24          	movaps (%rsp),%xmm0
  404937:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40493e:	00 
  40493f:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  404944:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  404949:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40494e:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  404953:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  404958:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  40495d:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  404962:	e8 49 ee ff ff       	call   4037b0 <runtime::default_context>
  404967:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  40496c:	0f 28 4c 24 30       	movaps 0x30(%rsp),%xmm1
  404971:	0f 28 54 24 40       	movaps 0x40(%rsp),%xmm2
  404976:	0f 28 5c 24 50       	movaps 0x50(%rsp),%xmm3
  40497b:	0f 28 64 24 60       	movaps 0x60(%rsp),%xmm4
  404980:	0f 28 6c 24 70       	movaps 0x70(%rsp),%xmm5
  404985:	0f 28 b4 24 80 00 00 	movaps 0x80(%rsp),%xmm6
  40498c:	00 
  40498d:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  404994:	00 
  404995:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  40499c:	00 
  40499d:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  4049a4:	00 
  4049a5:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  4049ac:	00 
  4049ad:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  4049b4:	00 
  4049b5:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  4049bc:	00 
  4049bd:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4049c4:	00 
  4049c5:	48 c7 c0 70 c0 40 00 	mov    $0x40c070,%rax
  4049cc:	48 8b 38             	mov    (%rax),%rdi
  4049cf:	48 8b 70 08          	mov    0x8(%rax),%rsi
  4049d3:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  4049da:	00 
  4049db:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4049e2:	00 
  4049e3:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4049e8:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4049ed:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4049f2:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4049f7:	41 b8 c0 93 40 00    	mov    $0x4093c0,%r8d
  4049fd:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  404a04:	00 
  404a05:	e8 66 34 00 00       	call   407e70 <runtime::delete_slice:proc(array:[]string,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  404a0a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  404a11:	c3                   	ret
  404a12:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  404a19:	00 00 00 
  404a1c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000404a20 <journey::main>:
  404a20:	55                   	push   %rbp
  404a21:	48 89 e5             	mov    %rsp,%rbp
  404a24:	41 57                	push   %r15
  404a26:	41 56                	push   %r14
  404a28:	41 54                	push   %r12
  404a2a:	53                   	push   %rbx
  404a2b:	48 83 e4 c0          	and    $0xffffffffffffffc0,%rsp
  404a2f:	48 81 ec 40 02 00 00 	sub    $0x240,%rsp
  404a36:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  404a3b:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404a40:	c7 84 24 0c 02 00 00 	movl   $0x40000000,0x20c(%rsp)
  404a47:	00 00 00 40 
  404a4b:	c7 84 24 08 02 00 00 	movl   $0x40e00000,0x208(%rsp)
  404a52:	00 00 e0 40 
  404a56:	c7 84 24 04 02 00 00 	movl   $0x40a00000,0x204(%rsp)
  404a5d:	00 00 a0 40 
  404a61:	48 8d 84 24 08 02 00 	lea    0x208(%rsp),%rax
  404a68:	00 
  404a69:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404a6e:	c7 84 24 00 02 00 00 	movl   $0x42c80000,0x200(%rsp)
  404a75:	00 00 c8 42 
  404a79:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  404a80:	00 
  404a81:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  404a86:	e8 1b 02 00 00       	call   404ca6 <journey::init_world:proc(world:^journey::World(THREAD_COUNT:$$4),unique_data_capacity:$$28)>
  404a8b:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  404a90:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404a95:	e8 f6 02 00 00       	call   404d90 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)>
  404a9a:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  404a9f:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404aa4:	e8 a7 03 00 00       	call   404e50 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$2,start:$$0,end:$$1)>
  404aa9:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404aae:	48 8d bc 24 04 02 00 	lea    0x204(%rsp),%rdi
  404ab5:	00 
  404ab6:	e8 05 04 00 00       	call   404ec0 <journey::build_sym_predicate:proc(data_typeid:$journey::main::Position::$1,field_name:$$"x",cmp_op:$$2,val:^f32,comb_op:$$2)->(:journey::SystemPredicate)>
  404abb:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  404ac0:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404ac5:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  404acc:	00 
  404acd:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  404ad4:	00 
  404ad5:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  404adc:	00 
  404add:	66 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%cx
  404ae4:	00 
  404ae5:	66 8b 94 24 ba 00 00 	mov    0xba(%rsp),%dx
  404aec:	00 
  404aed:	66 44 8b 84 24 bc 00 	mov    0xbc(%rsp),%r8w
  404af4:	00 00 
  404af6:	44 8a 8c 24 be 00 00 	mov    0xbe(%rsp),%r9b
  404afd:	00 
  404afe:	44 8a 94 24 bf 00 00 	mov    0xbf(%rsp),%r10b
  404b05:	00 
  404b06:	44 88 94 24 af 00 00 	mov    %r10b,0xaf(%rsp)
  404b0d:	00 
  404b0e:	44 88 8c 24 ae 00 00 	mov    %r9b,0xae(%rsp)
  404b15:	00 
  404b16:	66 44 89 84 24 ac 00 	mov    %r8w,0xac(%rsp)
  404b1d:	00 00 
  404b1f:	66 89 94 24 aa 00 00 	mov    %dx,0xaa(%rsp)
  404b26:	00 
  404b27:	66 89 8c 24 a8 00 00 	mov    %cx,0xa8(%rsp)
  404b2e:	00 
  404b2f:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  404b36:	00 
  404b37:	e8 34 04 00 00       	call   404f70 <journey::build_sym_predicate:proc(data_typeid:$journey::main::Position::$1,field_name:$$"y",cmp_op:$$2,val:^f32,comb_op:$$0)->(:journey::SystemPredicate)>
  404b3c:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  404b41:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  404b48:	00 
  404b49:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  404b50:	00 
  404b51:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  404b58:	00 
  404b59:	66 8b 8c 24 98 00 00 	mov    0x98(%rsp),%cx
  404b60:	00 
  404b61:	66 8b 94 24 9a 00 00 	mov    0x9a(%rsp),%dx
  404b68:	00 
  404b69:	66 8b b4 24 9c 00 00 	mov    0x9c(%rsp),%si
  404b70:	00 
  404b71:	40 8a bc 24 9e 00 00 	mov    0x9e(%rsp),%dil
  404b78:	00 
  404b79:	44 8a 84 24 9f 00 00 	mov    0x9f(%rsp),%r8b
  404b80:	00 
  404b81:	44 88 84 24 8f 00 00 	mov    %r8b,0x8f(%rsp)
  404b88:	00 
  404b89:	40 88 bc 24 8e 00 00 	mov    %dil,0x8e(%rsp)
  404b90:	00 
  404b91:	66 89 b4 24 8c 00 00 	mov    %si,0x8c(%rsp)
  404b98:	00 
  404b99:	66 89 94 24 8a 00 00 	mov    %dx,0x8a(%rsp)
  404ba0:	00 
  404ba1:	66 89 8c 24 88 00 00 	mov    %cx,0x88(%rsp)
  404ba8:	00 
  404ba9:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  404bb0:	00 
  404bb1:	4c 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%r10
  404bb8:	00 
  404bb9:	66 44 8b 9c 24 a8 00 	mov    0xa8(%rsp),%r11w
  404bc0:	00 00 
  404bc2:	66 8b 9c 24 aa 00 00 	mov    0xaa(%rsp),%bx
  404bc9:	00 
  404bca:	66 44 8b b4 24 ac 00 	mov    0xac(%rsp),%r14w
  404bd1:	00 00 
  404bd3:	44 8a bc 24 ae 00 00 	mov    0xae(%rsp),%r15b
  404bda:	00 
  404bdb:	44 8a a4 24 af 00 00 	mov    0xaf(%rsp),%r12b
  404be2:	00 
  404be3:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404bea:	00 
  404beb:	66 8b 8c 24 88 00 00 	mov    0x88(%rsp),%cx
  404bf2:	00 
  404bf3:	66 8b 94 24 8a 00 00 	mov    0x8a(%rsp),%dx
  404bfa:	00 
  404bfb:	66 8b b4 24 8c 00 00 	mov    0x8c(%rsp),%si
  404c02:	00 
  404c03:	40 8a bc 24 8e 00 00 	mov    0x8e(%rsp),%dil
  404c0a:	00 
  404c0b:	44 8a 84 24 8f 00 00 	mov    0x8f(%rsp),%r8b
  404c12:	00 
  404c13:	0f 57 c0             	xorps  %xmm0,%xmm0
  404c16:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  404c1b:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  404c20:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  404c25:	44 88 64 24 5f       	mov    %r12b,0x5f(%rsp)
  404c2a:	44 88 7c 24 5e       	mov    %r15b,0x5e(%rsp)
  404c2f:	66 44 89 74 24 5c    	mov    %r14w,0x5c(%rsp)
  404c35:	66 89 5c 24 5a       	mov    %bx,0x5a(%rsp)
  404c3a:	66 44 89 5c 24 58    	mov    %r11w,0x58(%rsp)
  404c40:	4c 89 54 24 50       	mov    %r10,0x50(%rsp)
  404c45:	44 88 44 24 6f       	mov    %r8b,0x6f(%rsp)
  404c4a:	40 88 7c 24 6e       	mov    %dil,0x6e(%rsp)
  404c4f:	66 89 74 24 6c       	mov    %si,0x6c(%rsp)
  404c54:	66 89 54 24 6a       	mov    %dx,0x6a(%rsp)
  404c59:	66 89 4c 24 68       	mov    %cx,0x68(%rsp)
  404c5e:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  404c63:	48 8d 44 24 50       	lea    0x50(%rsp),%rax
  404c68:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  404c6d:	48 c7 44 24 78 02 00 	movq   $0x2,0x78(%rsp)
  404c74:	00 00 
  404c76:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  404c7b:	4c 8b 44 24 78       	mov    0x78(%rsp),%r8
  404c80:	48 c7 c2 a0 50 40 00 	mov    $0x4050a0,%rdx
  404c87:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  404c8e:	00 
  404c8f:	be 01 00 00 00       	mov    $0x1,%esi
  404c94:	e8 7a 03 00 00       	call   405013 <journey::run_0:proc(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>
  404c99:	48 8d 65 e0          	lea    -0x20(%rbp),%rsp
  404c9d:	5b                   	pop    %rbx
  404c9e:	41 5c                	pop    %r12
  404ca0:	41 5e                	pop    %r14
  404ca2:	41 5f                	pop    %r15
  404ca4:	5d                   	pop    %rbp
  404ca5:	c3                   	ret

0000000000404ca6 <journey::init_world:proc(world:^journey::World(THREAD_COUNT:$$4),unique_data_capacity:$$28)>:
  404ca6:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  404cab:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404cb0:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404cb5:	31 c0                	xor    %eax,%eax
  404cb7:	41 89 c1             	mov    %eax,%r9d
  404cba:	b8 09 00 00 00       	mov    $0x9,%eax
  404cbf:	be 00 e0 01 00       	mov    $0x1e000,%esi
  404cc4:	ba 03 00 00 00       	mov    $0x3,%edx
  404cc9:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  404ccf:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  404cd6:	4c 89 cf             	mov    %r9,%rdi
  404cd9:	0f 05                	syscall
  404cdb:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  404ce0:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404ce5:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  404cea:	48 89 08             	mov    %rcx,(%rax)
  404ced:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404cf2:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  404cf7:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  404cfe:	48 89 48 08          	mov    %rcx,0x8(%rax)
  404d02:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404d07:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  404d0c:	48 81 c1 00 20 00 00 	add    $0x2000,%rcx
  404d13:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404d17:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404d1c:	48 c7 40 18 1c 00 00 	movq   $0x1c,0x18(%rax)
  404d23:	00 
  404d24:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404d29:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404d2e:	b8 ba 00 00 00       	mov    $0xba,%eax
  404d33:	0f 05                	syscall
  404d35:	48 89 c1             	mov    %rax,%rcx
  404d38:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404d3d:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404d41:	31 c0                	xor    %eax,%eax
  404d43:	41 89 c1             	mov    %eax,%r9d
  404d46:	b8 09 00 00 00       	mov    $0x9,%eax
  404d4b:	be 00 10 02 00       	mov    $0x21000,%esi
  404d50:	ba 03 00 00 00       	mov    $0x3,%edx
  404d55:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  404d5b:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  404d62:	4c 89 cf             	mov    %r9,%rdi
  404d65:	0f 05                	syscall
  404d67:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  404d6c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404d71:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  404d76:	48 89 48 28          	mov    %rcx,0x28(%rax)
  404d7a:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404d7f:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  404d84:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  404d8b:	48 89 48 30          	mov    %rcx,0x30(%rax)
  404d8f:	c3                   	ret

0000000000404d90 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)>:
  404d90:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  404d95:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404d9a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404d9f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404da4:	48 8b 00             	mov    (%rax),%rax
  404da7:	c7 40 1c 11 33 00 00 	movl   $0x3311,0x1c(%rax)
  404dae:	c7 40 18 3f 01 00 00 	movl   $0x13f,0x18(%rax)
  404db5:	c7 40 14 15 00 00 00 	movl   $0x15,0x14(%rax)
  404dbc:	c7 40 10 0c 00 00 00 	movl   $0xc,0x10(%rax)
  404dc3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404dc8:	48 8b 40 08          	mov    0x8(%rax),%rax
  404dcc:	c6 40 03 ef          	movb   $0xef,0x3(%rax)
  404dd0:	c6 40 02 0f          	movb   $0xf,0x2(%rax)
  404dd4:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404dd9:	48 8b 40 10          	mov    0x10(%rax),%rax
  404ddd:	48 05 00 10 00 00    	add    $0x1000,%rax
  404de3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  404de8:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  404def:	00 00 
  404df1:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  404df8:	00 00 
  404dfa:	48 83 7c 24 e8 04    	cmpq   $0x4,-0x18(%rsp)
  404e00:	73 3a                	jae    404e3c <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)+0xac>
  404e02:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404e07:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404e0c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404e11:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  404e16:	48 c7 04 c8 ff ff ff 	movq   $0xffffffffffffffff,(%rax,%rcx,8)
  404e1d:	ff 
  404e1e:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404e23:	48 83 c0 01          	add    $0x1,%rax
  404e27:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  404e2c:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404e31:	48 83 c0 01          	add    $0x1,%rax
  404e35:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404e3a:	eb be                	jmp    404dfa <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)+0x6a>
  404e3c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404e41:	48 b9 ff ff ff ff ff 	movabs $0x3ffffffffff,%rcx
  404e48:	03 00 00 
  404e4b:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404e4f:	c3                   	ret

0000000000404e50 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$2,start:$$0,end:$$1)>:
  404e50:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  404e55:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404e5a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404e5f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404e64:	48 8b 00             	mov    (%rax),%rax
  404e67:	c7 40 2c 11 33 00 00 	movl   $0x3311,0x2c(%rax)
  404e6e:	c7 40 28 15 00 00 00 	movl   $0x15,0x28(%rax)
  404e75:	c7 40 24 00 00 00 00 	movl   $0x0,0x24(%rax)
  404e7c:	c7 40 20 0c 00 00 00 	movl   $0xc,0x20(%rax)
  404e83:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404e88:	48 8b 40 08          	mov    0x8(%rax),%rax
  404e8c:	c6 40 05 fe          	movb   $0xfe,0x5(%rax)
  404e90:	c6 40 04 00          	movb   $0x0,0x4(%rax)
  404e94:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404e99:	48 8b 40 10          	mov    0x10(%rax),%rax
  404e9d:	48 05 00 20 00 00    	add    $0x2000,%rax
  404ea3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  404ea8:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404ead:	48 c7 00 ff ff 1f 00 	movq   $0x1fffff,(%rax)
  404eb4:	c3                   	ret
  404eb5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  404ebc:	00 00 00 00 

0000000000404ec0 <journey::build_sym_predicate:proc(data_typeid:$journey::main::Position::$1,field_name:$$"x",cmp_op:$$2,val:^f32,comb_op:$$2)->(:journey::SystemPredicate)>:
  404ec0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  404ec5:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404eca:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404ecf:	66 c7 44 24 f6 00 00 	movw   $0x0,-0xa(%rsp)
  404ed6:	66 c7 44 24 f6 00 00 	movw   $0x0,-0xa(%rsp)
  404edd:	0f 57 c0             	xorps  %xmm0,%xmm0
  404ee0:	0f 29 44 24 d8       	movaps %xmm0,-0x28(%rsp)
  404ee5:	c6 44 24 e7 02       	movb   $0x2,-0x19(%rsp)
  404eea:	c6 44 24 e6 02       	movb   $0x2,-0x1a(%rsp)
  404eef:	66 c7 44 24 e4 00 00 	movw   $0x0,-0x1c(%rsp)
  404ef6:	66 c7 44 24 e2 40 00 	movw   $0x40,-0x1e(%rsp)
  404efd:	66 c7 44 24 e0 00 00 	movw   $0x0,-0x20(%rsp)
  404f04:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404f0b:	00 00 
  404f0d:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404f12:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  404f17:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  404f1c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404f21:	66 8b 4c 24 e0       	mov    -0x20(%rsp),%cx
  404f26:	66 8b 54 24 e2       	mov    -0x1e(%rsp),%dx
  404f2b:	66 8b 74 24 e4       	mov    -0x1c(%rsp),%si
  404f30:	40 8a 7c 24 e6       	mov    -0x1a(%rsp),%dil
  404f35:	44 8a 44 24 e7       	mov    -0x19(%rsp),%r8b
  404f3a:	44 88 44 24 d7       	mov    %r8b,-0x29(%rsp)
  404f3f:	40 88 7c 24 d6       	mov    %dil,-0x2a(%rsp)
  404f44:	66 89 74 24 d4       	mov    %si,-0x2c(%rsp)
  404f49:	66 89 54 24 d2       	mov    %dx,-0x2e(%rsp)
  404f4e:	66 89 4c 24 d0       	mov    %cx,-0x30(%rsp)
  404f53:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404f58:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404f5d:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  404f62:	c3                   	ret
  404f63:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404f6a:	84 00 00 00 00 00 

0000000000404f70 <journey::build_sym_predicate:proc(data_typeid:$journey::main::Position::$1,field_name:$$"y",cmp_op:$$2,val:^f32,comb_op:$$0)->(:journey::SystemPredicate)>:
  404f70:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  404f75:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404f7a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404f7f:	66 c7 44 24 f6 00 00 	movw   $0x0,-0xa(%rsp)
  404f86:	66 c7 44 24 f6 20 00 	movw   $0x20,-0xa(%rsp)
  404f8d:	0f 57 c0             	xorps  %xmm0,%xmm0
  404f90:	0f 29 44 24 d8       	movaps %xmm0,-0x28(%rsp)
  404f95:	c6 44 24 e7 00       	movb   $0x0,-0x19(%rsp)
  404f9a:	c6 44 24 e6 02       	movb   $0x2,-0x1a(%rsp)
  404f9f:	66 c7 44 24 e4 00 00 	movw   $0x0,-0x1c(%rsp)
  404fa6:	66 c7 44 24 e2 40 00 	movw   $0x40,-0x1e(%rsp)
  404fad:	66 c7 44 24 e0 00 00 	movw   $0x0,-0x20(%rsp)
  404fb4:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404fbb:	00 00 
  404fbd:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404fc2:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  404fc7:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  404fcc:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404fd1:	66 8b 4c 24 e0       	mov    -0x20(%rsp),%cx
  404fd6:	66 8b 54 24 e2       	mov    -0x1e(%rsp),%dx
  404fdb:	66 8b 74 24 e4       	mov    -0x1c(%rsp),%si
  404fe0:	40 8a 7c 24 e6       	mov    -0x1a(%rsp),%dil
  404fe5:	44 8a 44 24 e7       	mov    -0x19(%rsp),%r8b
  404fea:	44 88 44 24 d7       	mov    %r8b,-0x29(%rsp)
  404fef:	40 88 7c 24 d6       	mov    %dil,-0x2a(%rsp)
  404ff4:	66 89 74 24 d4       	mov    %si,-0x2c(%rsp)
  404ff9:	66 89 54 24 d2       	mov    %dx,-0x2e(%rsp)
  404ffe:	66 89 4c 24 d0       	mov    %cx,-0x30(%rsp)
  405003:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  405008:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40500d:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  405012:	c3                   	ret

0000000000405013 <journey::run_0:proc(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>:
  405013:	48 83 ec 78          	sub    $0x78,%rsp
  405017:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40501c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405021:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405026:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40502b:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  405030:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405035:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40503a:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40503f:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405044:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405049:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  40504e:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  405053:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  405058:	48 89 54 24 58       	mov    %rdx,0x58(%rsp)
  40505d:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  405062:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  405067:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  40506b:	48 81 c1 40 10 00 00 	add    $0x1040,%rcx
  405072:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  405077:	48 c7 44 24 48 c0 0f 	movq   $0xfc0,0x48(%rsp)
  40507e:	00 00 
  405080:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  405085:	31 c9                	xor    %ecx,%ecx
  405087:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  40508b:	89 4c 24 30          	mov    %ecx,0x30(%rsp)
  40508f:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  405094:	ff d0                	call   *%rax
  405096:	48 83 c4 78          	add    $0x78,%rsp
  40509a:	c3                   	ret
  40509b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004050a0 <journey::main.readjust_npc_position-0>:
  4050a0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4050a5:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  4050aa:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4050af:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4050b4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4050b9:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4050be:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4050c3:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4050ca:	00 00 
  4050cc:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  4050d3:	00 00 
  4050d5:	48 83 7c 24 d8 01    	cmpq   $0x1,-0x28(%rsp)
  4050db:	7d 4a                	jge    405127 <journey::main.readjust_npc_position-0+0x87>
  4050dd:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4050e2:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4050e7:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4050ec:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4050f1:	48 8d 0c 49          	lea    (%rcx,%rcx,2),%rcx
  4050f5:	48 c1 e1 05          	shl    $0x5,%rcx
  4050f9:	0f 28 05 f0 42 00 00 	movaps 0x42f0(%rip),%xmm0        # 4093f0 <runtime::type_table+0x220>
  405100:	0f 29 44 08 10       	movaps %xmm0,0x10(%rax,%rcx,1)
  405105:	0f 29 04 08          	movaps %xmm0,(%rax,%rcx,1)
  405109:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40510e:	48 83 c0 01          	add    $0x1,%rax
  405112:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  405117:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40511c:	48 83 c0 01          	add    $0x1,%rax
  405120:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405125:	eb ae                	jmp    4050d5 <journey::main.readjust_npc_position-0+0x35>
  405127:	c3                   	ret
  405128:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40512f:	00 

0000000000405130 <main>:
  405130:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  405137:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  40513b:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405140:	8b 44 24 14          	mov    0x14(%rsp),%eax
  405144:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  405149:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  405150:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  405157:	00 
  405158:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  40515f:	00 
  405160:	48 89 0c 24          	mov    %rcx,(%rsp)
  405164:	4c 63 c8             	movslq %eax,%r9
  405167:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40516c:	bf 24 94 40 00       	mov    $0x409424,%edi
  405171:	31 c0                	xor    %eax,%eax
  405173:	41 89 c0             	mov    %eax,%r8d
  405176:	be 2a 00 00 00       	mov    $0x2a,%esi
  40517b:	ba 36 00 00 00       	mov    $0x36,%edx
  405180:	b9 11 00 00 00       	mov    $0x11,%ecx
  405185:	e8 b6 ed ff ff       	call   403f40 <runtime::multi_pointer_slice_expr_error>
  40518a:	48 8b 0c 24          	mov    (%rsp),%rcx
  40518e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405193:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  40519a:	00 
  40519b:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4051a2:	00 
  4051a3:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  4051aa:	00 
  4051ab:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  4051b2:	00 
  4051b3:	48 c7 c0 60 c0 40 00 	mov    $0x40c060,%rax
  4051ba:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4051be:	48 89 08             	mov    %rcx,(%rax)
  4051c1:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4051c8:	00 
  4051c9:	31 f6                	xor    %esi,%esi
  4051cb:	ba 70 00 00 00       	mov    $0x70,%edx
  4051d0:	e8 6b be ff ff       	call   401040 <memset@plt>
  4051d5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4051dc:	00 
  4051dd:	e8 1e e6 ff ff       	call   403800 <runtime::[core.odin]::__init_context>
  4051e2:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  4051e7:	31 f6                	xor    %esi,%esi
  4051e9:	ba 70 00 00 00       	mov    $0x70,%edx
  4051ee:	e8 4d be ff ff       	call   401040 <memset@plt>
  4051f3:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  4051f8:	e8 b3 e5 ff ff       	call   4037b0 <runtime::default_context>
  4051fd:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  405202:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  405207:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  40520c:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  405211:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  405216:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  40521b:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  405222:	00 
  405223:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  40522a:	00 
  40522b:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  405232:	00 
  405233:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  40523a:	00 
  40523b:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  405242:	00 
  405243:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  40524a:	00 
  40524b:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  405252:	00 
  405253:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40525a:	00 
  40525b:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  405262:	00 
  405263:	e8 68 e9 ff ff       	call   403bd0 <__$startup_runtime>
  405268:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40526f:	00 
  405270:	e8 ab f7 ff ff       	call   404a20 <journey::main>
  405275:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40527c:	00 
  40527d:	e8 fe e9 ff ff       	call   403c80 <__$cleanup_runtime>
  405282:	31 c0                	xor    %eax,%eax
  405284:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  40528b:	c3                   	ret
  40528c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405290 <runtime::stderr_write>:
  405290:	48 83 ec 48          	sub    $0x48,%rsp
  405294:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405299:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40529e:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4052a3:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4052a8:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4052ad:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4052b2:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4052b7:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4052be:	00 00 
  4052c0:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  4052c5:	e8 86 e8 ff ff       	call   403b50 <runtime::[os_specific_linux.odin]::_stderr_write>
  4052ca:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4052cf:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4052d4:	48 89 11             	mov    %rdx,(%rcx)
  4052d7:	48 83 c4 48          	add    $0x48,%rsp
  4052db:	c3                   	ret
  4052dc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004052e0 <runtime::heap_allocator>:
  4052e0:	48 c7 c0 00 53 40 00 	mov    $0x405300,%rax
  4052e7:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4052ec:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  4052f3:	00 00 
  4052f5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4052fa:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4052ff:	c3                   	ret

0000000000405300 <runtime::heap_allocator_proc>:
  405300:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  405307:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40530c:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  405311:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  405316:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40531b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  405320:	40 88 f0             	mov    %sil,%al
  405323:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  405327:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40532e:	00 
  40532f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  405334:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40533b:	00 
  40533c:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  405341:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  405345:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40534a:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40534f:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405354:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  405359:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  40535e:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  405365:	00 
  405366:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  40536d:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  405374:	00 
  405375:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  40537c:	00 
  40537d:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  405384:	00 
  405385:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40538c:	00 
  40538d:	0f b6 c8             	movzbl %al,%ecx
  405390:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405395:	2c 07                	sub    $0x7,%al
  405397:	0f 87 5f 01 00 00    	ja     4054fc <runtime::heap_allocator_proc+0x1fc>
  40539d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4053a2:	48 8b 04 c5 50 94 40 	mov    0x409450(,%rax,8),%rax
  4053a9:	00 
  4053aa:	ff e0                	jmp    *%rax
  4053ac:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4053b1:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4053b6:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  4053bb:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  4053bf:	84 c0                	test   %al,%al
  4053c1:	0f 94 c0             	sete   %al
  4053c4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4053c7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4053ce:	00 
  4053cf:	48 89 e1             	mov    %rsp,%rcx
  4053d2:	48 89 11             	mov    %rdx,(%rcx)
  4053d5:	44 0f b6 c0          	movzbl %al,%r8d
  4053d9:	31 c0                	xor    %eax,%eax
  4053db:	89 c1                	mov    %eax,%ecx
  4053dd:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  4053e4:	00 
  4053e5:	48 89 ca             	mov    %rcx,%rdx
  4053e8:	e8 b3 01 00 00       	call   4055a0 <runtime::heap_allocator_proc.aligned_alloc-0>
  4053ed:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4053f2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4053f9:	00 
  4053fa:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  405401:	00 
  405402:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405406:	48 89 11             	mov    %rdx,(%rcx)
  405409:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  405410:	c3                   	ret
  405411:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405416:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40541b:	e8 d0 03 00 00       	call   4057f0 <runtime::heap_allocator_proc.aligned_free-1>
  405420:	e9 d7 00 00 00       	jmp    4054fc <runtime::heap_allocator_proc+0x1fc>
  405425:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40542a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405431:	00 
  405432:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  405439:	b0 04                	mov    $0x4,%al
  40543b:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  405442:	c3                   	ret
  405443:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  405448:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40544d:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405452:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405457:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  40545c:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  405460:	2c 03                	sub    $0x3,%al
  405462:	0f 94 c0             	sete   %al
  405465:	0f 57 c0             	xorps  %xmm0,%xmm0
  405468:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40546d:	49 89 e0             	mov    %rsp,%r8
  405470:	4d 89 08             	mov    %r9,(%r8)
  405473:	44 0f b6 c0          	movzbl %al,%r8d
  405477:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  40547c:	e8 af 03 00 00       	call   405830 <runtime::heap_allocator_proc.aligned_resize-2>
  405481:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  405486:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40548b:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  405490:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405494:	48 89 11             	mov    %rdx,(%rcx)
  405497:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40549e:	c3                   	ret
  40549f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4054a4:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4054a9:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  4054af:	0f 95 c0             	setne  %al
  4054b2:	24 01                	and    $0x1,%al
  4054b4:	3c 00                	cmp    $0x0,%al
  4054b6:	74 08                	je     4054c0 <runtime::heap_allocator_proc+0x1c0>
  4054b8:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4054bd:	c6 00 db             	movb   $0xdb,(%rax)
  4054c0:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4054c5:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4054cc:	00 
  4054cd:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4054d4:	31 c0                	xor    %eax,%eax
  4054d6:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4054dd:	c3                   	ret
  4054de:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4054e3:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4054ea:	00 
  4054eb:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4054f2:	b0 04                	mov    $0x4,%al
  4054f4:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4054fb:	c3                   	ret
  4054fc:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405501:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405508:	00 
  405509:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  405510:	31 c0                	xor    %eax,%eax
  405512:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  405519:	c3                   	ret
  40551a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000405520 <runtime::heap_alloc>:
  405520:	48 83 ec 18          	sub    $0x18,%rsp
  405524:	48 89 3c 24          	mov    %rdi,(%rsp)
  405528:	40 88 f0             	mov    %sil,%al
  40552b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40552f:	8a 44 24 0e          	mov    0xe(%rsp),%al
  405533:	48 8b 3c 24          	mov    (%rsp),%rdi
  405537:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40553c:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405540:	0f b6 f0             	movzbl %al,%esi
  405543:	e8 08 f0 ff ff       	call   404550 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  405548:	48 83 c4 18          	add    $0x18,%rsp
  40554c:	c3                   	ret
  40554d:	0f 1f 00             	nopl   (%rax)

0000000000405550 <runtime::heap_resize>:
  405550:	48 83 ec 28          	sub    $0x28,%rsp
  405554:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  405559:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40555e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405563:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  405568:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40556d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405572:	e8 39 f0 ff ff       	call   4045b0 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  405577:	48 83 c4 28          	add    $0x28,%rsp
  40557b:	c3                   	ret
  40557c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405580 <runtime::heap_free>:
  405580:	48 83 ec 18          	sub    $0x18,%rsp
  405584:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  405589:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40558e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  405593:	e8 48 f0 ff ff       	call   4045e0 <runtime::[heap_allocator_unix.odin]::_heap_free>
  405598:	48 83 c4 18          	add    $0x18,%rsp
  40559c:	c3                   	ret
  40559d:	0f 1f 00             	nopl   (%rax)

00000000004055a0 <runtime::heap_allocator_proc.aligned_alloc-0>:
  4055a0:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  4055a7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  4055ac:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4055b1:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4055b6:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  4055bb:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  4055c0:	44 88 c0             	mov    %r8b,%al
  4055c3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  4055c7:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4055ce:	00 
  4055cf:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4055d4:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4055d9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4055de:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  4055e3:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  4055e7:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4055ec:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  4055f3:	00 
  4055f4:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4055fb:	00 
  4055fc:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  405603:	00 
  405604:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  40560b:	00 
  40560c:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  405613:	b9 08 00 00 00       	mov    $0x8,%ecx
  405618:	48 83 fe 08          	cmp    $0x8,%rsi
  40561c:	48 0f 4f ce          	cmovg  %rsi,%rcx
  405620:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  405627:	00 
  405628:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40562f:	00 
  405630:	48 83 e9 01          	sub    $0x1,%rcx
  405634:	48 83 c1 08          	add    $0x8,%rcx
  405638:	48 01 d1             	add    %rdx,%rcx
  40563b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  405642:	00 
  405643:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  40564a:	00 00 
  40564c:	48 83 f8 00          	cmp    $0x0,%rax
  405650:	0f 95 c1             	setne  %cl
  405653:	80 e1 01             	and    $0x1,%cl
  405656:	31 c0                	xor    %eax,%eax
  405658:	80 f9 00             	cmp    $0x0,%cl
  40565b:	88 44 24 0f          	mov    %al,0xf(%rsp)
  40565f:	74 17                	je     405678 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  405661:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  405666:	48 83 f8 08          	cmp    $0x8,%rax
  40566a:	0f 9f c0             	setg   %al
  40566d:	24 01                	and    $0x1,%al
  40566f:	3c 00                	cmp    $0x0,%al
  405671:	0f 95 c0             	setne  %al
  405674:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405678:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40567d:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  405681:	80 e1 01             	and    $0x1,%cl
  405684:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  405688:	48 83 f8 00          	cmp    $0x0,%rax
  40568c:	0f 95 c0             	setne  %al
  40568f:	24 01                	and    $0x1,%al
  405691:	3c 00                	cmp    $0x0,%al
  405693:	74 2e                	je     4056c3 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  405695:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40569a:	75 27                	jne    4056c3 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  40569c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4056a1:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  4056a5:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4056aa:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  4056af:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  4056b6:	00 
  4056b7:	e8 94 fe ff ff       	call   405550 <runtime::heap_resize>
  4056bc:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  4056c1:	eb 19                	jmp    4056dc <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  4056c3:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  4056c7:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  4056ce:	00 
  4056cf:	0f b6 f0             	movzbl %al,%esi
  4056d2:	e8 49 fe ff ff       	call   405520 <runtime::heap_alloc>
  4056d7:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  4056dc:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4056e1:	48 83 c0 08          	add    $0x8,%rax
  4056e5:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4056ea:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4056ef:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4056f4:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4056f9:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4056fe:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  405703:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  40570a:	00 
  40570b:	48 83 e8 01          	sub    $0x1,%rax
  40570f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405716:	00 
  405717:	48 83 e9 01          	sub    $0x1,%rcx
  40571b:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  40571f:	48 21 c8             	and    %rcx,%rax
  405722:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  405727:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  40572d:	0f 94 c0             	sete   %al
  405730:	24 01                	and    $0x1,%al
  405732:	3c 00                	cmp    $0x0,%al
  405734:	74 3c                	je     405772 <runtime::heap_allocator_proc.aligned_alloc-0+0x1d2>
  405736:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40573b:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405740:	e8 ab 00 00 00       	call   4057f0 <runtime::heap_allocator_proc.aligned_free-1>
  405745:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40574a:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  40574f:	e8 9c 00 00 00       	call   4057f0 <runtime::heap_allocator_proc.aligned_free-1>
  405754:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405759:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405760:	00 
  405761:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  405768:	b0 01                	mov    $0x1,%al
  40576a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  405771:	c3                   	ret
  405772:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405777:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40577c:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  405781:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  405786:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  40578a:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40578f:	74 2f                	je     4057c0 <runtime::heap_allocator_proc.aligned_alloc-0+0x220>
  405791:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405796:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40579b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4057a0:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  4057a5:	48 39 d0             	cmp    %rdx,%rax
  4057a8:	48 0f 4c d0          	cmovl  %rax,%rdx
  4057ac:	e8 df 03 00 00       	call   405b90 <runtime::mem_copy_non_overlapping>
  4057b1:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4057b6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4057bb:	e8 30 00 00 00       	call   4057f0 <runtime::heap_allocator_proc.aligned_free-1>
  4057c0:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4057c5:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  4057ca:	e8 21 03 00 00       	call   405af0 <runtime::[internal.odin]::byte_slice>
  4057cf:	48 89 c1             	mov    %rax,%rcx
  4057d2:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4057d7:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4057db:	48 89 08             	mov    %rcx,(%rax)
  4057de:	31 c0                	xor    %eax,%eax
  4057e0:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4057e7:	c3                   	ret
  4057e8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4057ef:	00 

00000000004057f0 <runtime::heap_allocator_proc.aligned_free-1>:
  4057f0:	48 83 ec 18          	sub    $0x18,%rsp
  4057f4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4057f9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4057fe:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  405803:	48 83 f8 00          	cmp    $0x0,%rax
  405807:	0f 95 c0             	setne  %al
  40580a:	24 01                	and    $0x1,%al
  40580c:	3c 00                	cmp    $0x0,%al
  40580e:	74 0e                	je     40581e <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  405810:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405815:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  405819:	e8 62 fd ff ff       	call   405580 <runtime::heap_free>
  40581e:	48 83 c4 18          	add    $0x18,%rsp
  405822:	c3                   	ret
  405823:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40582a:	84 00 00 00 00 00 

0000000000405830 <runtime::heap_allocator_proc.aligned_resize-2>:
  405830:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  405837:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40583c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405841:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  405846:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40584b:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  405850:	44 88 c0             	mov    %r8b,%al
  405853:	88 44 24 57          	mov    %al,0x57(%rsp)
  405857:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40585e:	00 
  40585f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405864:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405869:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  40586d:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405872:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  405877:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40587c:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  405883:	00 
  405884:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  40588b:	00 
  40588c:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  405893:	00 
  405894:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  40589b:	00 
  40589c:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  4058a3:	0f 57 c0             	xorps  %xmm0,%xmm0
  4058a6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4058ad:	00 
  4058ae:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  4058b5:	00 
  4058b6:	48 83 f8 00          	cmp    $0x0,%rax
  4058ba:	0f 94 c0             	sete   %al
  4058bd:	24 01                	and    $0x1,%al
  4058bf:	3c 00                	cmp    $0x0,%al
  4058c1:	0f 84 80 00 00 00    	je     405947 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  4058c7:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4058cc:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4058d1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4058d6:	8a 44 24 57          	mov    0x57(%rsp),%al
  4058da:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  4058df:	0f 57 c0             	xorps  %xmm0,%xmm0
  4058e2:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  4058e9:	00 
  4058ea:	48 89 e2             	mov    %rsp,%rdx
  4058ed:	4c 89 02             	mov    %r8,(%rdx)
  4058f0:	44 0f b6 c0          	movzbl %al,%r8d
  4058f4:	31 c0                	xor    %eax,%eax
  4058f6:	89 c2                	mov    %eax,%edx
  4058f8:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  4058ff:	00 
  405900:	e8 9b fc ff ff       	call   4055a0 <runtime::heap_allocator_proc.aligned_alloc-0>
  405905:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40590a:	40 88 c7             	mov    %al,%dil
  40590d:	40 88 f8             	mov    %dil,%al
  405910:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  405917:	00 
  405918:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  40591f:	00 
  405920:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  405927:	00 
  405928:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40592f:	00 
  405930:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  405937:	00 
  405938:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40593c:	48 89 11             	mov    %rdx,(%rcx)
  40593f:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  405946:	c3                   	ret
  405947:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40594c:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  405951:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  405956:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40595b:	8a 44 24 57          	mov    0x57(%rsp),%al
  40595f:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  405964:	0f 57 c0             	xorps  %xmm0,%xmm0
  405967:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40596e:	00 
  40596f:	49 89 e0             	mov    %rsp,%r8
  405972:	4d 89 08             	mov    %r9,(%r8)
  405975:	44 0f b6 c0          	movzbl %al,%r8d
  405979:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  405980:	00 
  405981:	e8 1a fc ff ff       	call   4055a0 <runtime::heap_allocator_proc.aligned_alloc-0>
  405986:	88 44 24 17          	mov    %al,0x17(%rsp)
  40598a:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  405991:	00 
  405992:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  405997:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40599e:	00 
  40599f:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4059a4:	3c 00                	cmp    $0x0,%al
  4059a6:	74 4d                	je     4059f5 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  4059a8:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4059ad:	8a 44 24 17          	mov    0x17(%rsp),%al
  4059b1:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4059b8:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4059bf:	00 
  4059c0:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  4059c7:	00 
  4059c8:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  4059cf:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  4059d6:	00 
  4059d7:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4059de:	00 
  4059df:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4059e6:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4059ea:	48 89 11             	mov    %rdx,(%rcx)
  4059ed:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4059f4:	c3                   	ret
  4059f5:	8a 44 24 57          	mov    0x57(%rsp),%al
  4059f9:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4059fe:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  405a03:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  405a0a:	00 
  405a0b:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  405a12:	00 
  405a13:	3c 00                	cmp    $0x0,%al
  405a15:	0f 84 85 00 00 00    	je     405aa0 <runtime::heap_allocator_proc.aligned_resize-2+0x270>
  405a1b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  405a20:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  405a25:	48 39 c8             	cmp    %rcx,%rax
  405a28:	0f 9f c0             	setg   %al
  405a2b:	24 01                	and    $0x1,%al
  405a2d:	3c 00                	cmp    $0x0,%al
  405a2f:	74 6f                	je     405aa0 <runtime::heap_allocator_proc.aligned_resize-2+0x270>
  405a31:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  405a36:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  405a3d:	00 
  405a3e:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  405a43:	48 89 e0             	mov    %rsp,%rax
  405a46:	4c 89 08             	mov    %r9,(%rax)
  405a49:	bf 90 94 40 00       	mov    $0x409490,%edi
  405a4e:	be 2e 00 00 00       	mov    $0x2e,%esi
  405a53:	ba 4d 00 00 00       	mov    $0x4d,%edx
  405a58:	b9 26 00 00 00       	mov    $0x26,%ecx
  405a5d:	e8 1e e6 ff ff       	call   404080 <runtime::slice_expr_error_lo_hi>
  405a62:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405a67:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  405a6c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  405a71:	48 89 c2             	mov    %rax,%rdx
  405a74:	48 03 94 24 e0 00 00 	add    0xe0(%rsp),%rdx
  405a7b:	00 
  405a7c:	48 29 c1             	sub    %rax,%rcx
  405a7f:	48 89 54 24 68       	mov    %rdx,0x68(%rsp)
  405a84:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  405a89:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  405a8e:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  405a93:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  405a98:	48 29 c6             	sub    %rax,%rsi
  405a9b:	e8 80 05 00 00       	call   406020 <runtime::conditional_mem_zero>
  405aa0:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  405aa5:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  405aac:	00 
  405aad:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  405ab4:	00 
  405ab5:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  405abc:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  405ac3:	00 
  405ac4:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  405acb:	00 
  405acc:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  405ad3:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405ad7:	48 89 11             	mov    %rdx,(%rcx)
  405ada:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  405ae1:	c3                   	ret
  405ae2:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  405ae9:	00 00 00 
  405aec:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405af0 <runtime::[internal.odin]::byte_slice>:
  405af0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  405af5:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  405afa:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  405aff:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405b04:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405b09:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  405b0e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405b13:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405b18:	31 c0                	xor    %eax,%eax
  405b1a:	48 85 d2             	test   %rdx,%rdx
  405b1d:	48 0f 49 c2          	cmovns %rdx,%rax
  405b21:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405b26:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b2b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405b30:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  405b35:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  405b3a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  405b3f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405b44:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  405b49:	c3                   	ret
  405b4a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000405b50 <runtime::is_power_of_two_int>:
  405b50:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  405b55:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405b5a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405b5f:	48 83 f8 00          	cmp    $0x0,%rax
  405b63:	0f 9e c0             	setle  %al
  405b66:	24 01                	and    $0x1,%al
  405b68:	3c 00                	cmp    $0x0,%al
  405b6a:	74 03                	je     405b6f <runtime::is_power_of_two_int+0x1f>
  405b6c:	31 c0                	xor    %eax,%eax
  405b6e:	c3                   	ret
  405b6f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405b74:	48 89 c1             	mov    %rax,%rcx
  405b77:	48 83 e9 01          	sub    $0x1,%rcx
  405b7b:	48 21 c8             	and    %rcx,%rax
  405b7e:	48 83 f8 00          	cmp    $0x0,%rax
  405b82:	0f 94 c0             	sete   %al
  405b85:	24 01                	and    $0x1,%al
  405b87:	c3                   	ret
  405b88:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  405b8f:	00 

0000000000405b90 <runtime::mem_copy_non_overlapping>:
  405b90:	48 83 ec 38          	sub    $0x38,%rsp
  405b94:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  405b99:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405b9e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  405ba3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405ba8:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  405bad:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  405bb2:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405bb7:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  405bbc:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  405bc1:	48 83 f8 00          	cmp    $0x0,%rax
  405bc5:	0f 95 c0             	setne  %al
  405bc8:	24 01                	and    $0x1,%al
  405bca:	3c 00                	cmp    $0x0,%al
  405bcc:	74 3c                	je     405c0a <runtime::mem_copy_non_overlapping+0x7a>
  405bce:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405bd3:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  405bd8:	48 39 c8             	cmp    %rcx,%rax
  405bdb:	0f 95 c0             	setne  %al
  405bde:	24 01                	and    $0x1,%al
  405be0:	3c 00                	cmp    $0x0,%al
  405be2:	74 26                	je     405c0a <runtime::mem_copy_non_overlapping+0x7a>
  405be4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  405be9:	48 83 f8 00          	cmp    $0x0,%rax
  405bed:	0f 9f c0             	setg   %al
  405bf0:	24 01                	and    $0x1,%al
  405bf2:	3c 00                	cmp    $0x0,%al
  405bf4:	74 14                	je     405c0a <runtime::mem_copy_non_overlapping+0x7a>
  405bf6:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  405bfb:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405c00:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  405c05:	e8 56 b4 ff ff       	call   401060 <memcpy@plt>
  405c0a:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405c0f:	48 83 c4 38          	add    $0x38,%rsp
  405c13:	c3                   	ret
  405c14:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  405c1b:	00 00 00 00 00 

0000000000405c20 <runtime::mem_alloc_bytes>:
  405c20:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  405c27:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  405c2c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  405c31:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  405c36:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405c3b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  405c40:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  405c45:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  405c4c:	00 
  405c4d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  405c52:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  405c57:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  405c5c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405c61:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  405c66:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  405c6d:	00 
  405c6e:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  405c75:	00 
  405c76:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  405c7d:	00 
  405c7e:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  405c85:	00 
  405c86:	e8 c5 fe ff ff       	call   405b50 <runtime::is_power_of_two_int>
  405c8b:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  405c90:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  405c95:	0f b6 f8             	movzbl %al,%edi
  405c98:	be bf 94 40 00       	mov    $0x4094bf,%esi
  405c9d:	ba 20 00 00 00       	mov    $0x20,%edx
  405ca2:	e8 49 22 00 00       	call   407ef0 <runtime::assert>
  405ca7:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  405cac:	48 83 f8 00          	cmp    $0x0,%rax
  405cb0:	0f 94 c0             	sete   %al
  405cb3:	24 01                	and    $0x1,%al
  405cb5:	3c 00                	cmp    $0x0,%al
  405cb7:	75 12                	jne    405ccb <runtime::mem_alloc_bytes+0xab>
  405cb9:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  405cc0:	00 00 
  405cc2:	0f 94 c0             	sete   %al
  405cc5:	24 01                	and    $0x1,%al
  405cc7:	3c 00                	cmp    $0x0,%al
  405cc9:	74 1e                	je     405ce9 <runtime::mem_alloc_bytes+0xc9>
  405ccb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405cd0:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405cd7:	00 
  405cd8:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  405cdf:	31 c0                	xor    %eax,%eax
  405ce1:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  405ce8:	c3                   	ret
  405ce9:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  405cee:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  405cf3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  405cf8:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  405cfd:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  405d04:	00 
  405d05:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  405d0c:	00 
  405d0d:	0f 57 c0             	xorps  %xmm0,%xmm0
  405d10:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  405d15:	48 89 e6             	mov    %rsp,%rsi
  405d18:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  405d1c:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  405d21:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  405d25:	4c 89 06             	mov    %r8,(%rsi)
  405d28:	31 f6                	xor    %esi,%esi
  405d2a:	41 89 f1             	mov    %esi,%r9d
  405d2d:	4d 89 c8             	mov    %r9,%r8
  405d30:	ff d0                	call   *%rax
  405d32:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  405d37:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  405d3c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  405d41:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405d45:	48 89 11             	mov    %rdx,(%rcx)
  405d48:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  405d4f:	c3                   	ret

0000000000405d50 <runtime::mem_alloc>:
  405d50:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  405d57:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  405d5c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  405d61:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  405d66:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405d6b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  405d70:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  405d75:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  405d7c:	00 
  405d7d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  405d82:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  405d87:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  405d8c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405d91:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  405d96:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  405d9d:	00 
  405d9e:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  405da5:	00 
  405da6:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  405dad:	00 
  405dae:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  405db5:	00 
  405db6:	e8 95 fd ff ff       	call   405b50 <runtime::is_power_of_two_int>
  405dbb:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  405dc0:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  405dc5:	0f b6 f8             	movzbl %al,%edi
  405dc8:	be bf 94 40 00       	mov    $0x4094bf,%esi
  405dcd:	ba 20 00 00 00       	mov    $0x20,%edx
  405dd2:	e8 19 21 00 00       	call   407ef0 <runtime::assert>
  405dd7:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  405ddc:	48 83 f8 00          	cmp    $0x0,%rax
  405de0:	0f 94 c0             	sete   %al
  405de3:	24 01                	and    $0x1,%al
  405de5:	3c 00                	cmp    $0x0,%al
  405de7:	75 12                	jne    405dfb <runtime::mem_alloc+0xab>
  405de9:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  405df0:	00 00 
  405df2:	0f 94 c0             	sete   %al
  405df5:	24 01                	and    $0x1,%al
  405df7:	3c 00                	cmp    $0x0,%al
  405df9:	74 1e                	je     405e19 <runtime::mem_alloc+0xc9>
  405dfb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405e00:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405e07:	00 
  405e08:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  405e0f:	31 c0                	xor    %eax,%eax
  405e11:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  405e18:	c3                   	ret
  405e19:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  405e1e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  405e23:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  405e28:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  405e2d:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  405e34:	00 
  405e35:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  405e3c:	00 
  405e3d:	0f 57 c0             	xorps  %xmm0,%xmm0
  405e40:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  405e45:	48 89 e6             	mov    %rsp,%rsi
  405e48:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  405e4c:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  405e51:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  405e55:	4c 89 06             	mov    %r8,(%rsi)
  405e58:	31 f6                	xor    %esi,%esi
  405e5a:	41 89 f1             	mov    %esi,%r9d
  405e5d:	4d 89 c8             	mov    %r9,%r8
  405e60:	ff d0                	call   *%rax
  405e62:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  405e67:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  405e6c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  405e71:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405e75:	48 89 11             	mov    %rdx,(%rcx)
  405e78:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  405e7f:	c3                   	ret

0000000000405e80 <runtime::mem_free>:
  405e80:	53                   	push   %rbx
  405e81:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  405e88:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  405e8d:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  405e92:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  405e97:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405e9c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405ea1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  405ea6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  405eab:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405eb0:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  405eb7:	00 
  405eb8:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  405ebd:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  405ec2:	48 83 f8 00          	cmp    $0x0,%rax
  405ec6:	0f 94 c0             	sete   %al
  405ec9:	24 01                	and    $0x1,%al
  405ecb:	3c 00                	cmp    $0x0,%al
  405ecd:	75 0f                	jne    405ede <runtime::mem_free+0x5e>
  405ecf:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  405ed5:	0f 94 c0             	sete   %al
  405ed8:	24 01                	and    $0x1,%al
  405eda:	3c 00                	cmp    $0x0,%al
  405edc:	74 0b                	je     405ee9 <runtime::mem_free+0x69>
  405ede:	31 c0                	xor    %eax,%eax
  405ee0:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  405ee7:	5b                   	pop    %rbx
  405ee8:	c3                   	ret
  405ee9:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  405eee:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  405ef3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  405ef8:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  405efd:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  405f02:	0f 57 c0             	xorps  %xmm0,%xmm0
  405f05:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  405f0a:	be 01 00 00 00       	mov    $0x1,%esi
  405f0f:	31 c9                	xor    %ecx,%ecx
  405f11:	41 89 c9             	mov    %ecx,%r9d
  405f14:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  405f19:	4c 89 ca             	mov    %r9,%rdx
  405f1c:	4c 89 c9             	mov    %r9,%rcx
  405f1f:	48 89 1c 24          	mov    %rbx,(%rsp)
  405f23:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  405f28:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  405f2d:	ff d0                	call   *%rax
  405f2f:	88 44 24 47          	mov    %al,0x47(%rsp)
  405f33:	8a 44 24 47          	mov    0x47(%rsp),%al
  405f37:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  405f3e:	5b                   	pop    %rbx
  405f3f:	c3                   	ret

0000000000405f40 <runtime::mem_free_with_size>:
  405f40:	53                   	push   %rbx
  405f41:	48 81 ec a0 00 00 00 	sub    $0xa0,%rsp
  405f48:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  405f4d:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  405f52:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  405f57:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405f5c:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  405f61:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  405f66:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  405f6b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  405f70:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  405f75:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  405f7a:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  405f81:	00 
  405f82:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  405f89:	00 
  405f8a:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  405f91:	00 
  405f92:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  405f99:	00 
  405f9a:	48 83 f8 00          	cmp    $0x0,%rax
  405f9e:	0f 94 c0             	sete   %al
  405fa1:	24 01                	and    $0x1,%al
  405fa3:	3c 00                	cmp    $0x0,%al
  405fa5:	75 12                	jne    405fb9 <runtime::mem_free_with_size+0x79>
  405fa7:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  405fae:	00 00 
  405fb0:	0f 94 c0             	sete   %al
  405fb3:	24 01                	and    $0x1,%al
  405fb5:	3c 00                	cmp    $0x0,%al
  405fb7:	74 0b                	je     405fc4 <runtime::mem_free_with_size+0x84>
  405fb9:	31 c0                	xor    %eax,%eax
  405fbb:	48 81 c4 a0 00 00 00 	add    $0xa0,%rsp
  405fc2:	5b                   	pop    %rbx
  405fc3:	c3                   	ret
  405fc4:	4c 8b 54 24 20       	mov    0x20(%rsp),%r10
  405fc9:	48 8b 5c 24 28       	mov    0x28(%rsp),%rbx
  405fce:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  405fd3:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  405fd8:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  405fdf:	00 
  405fe0:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  405fe7:	00 
  405fe8:	0f 57 c0             	xorps  %xmm0,%xmm0
  405feb:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  405ff0:	be 01 00 00 00       	mov    $0x1,%esi
  405ff5:	31 c9                	xor    %ecx,%ecx
  405ff7:	4c 8d 5c 24 70       	lea    0x70(%rsp),%r11
  405ffc:	48 89 ca             	mov    %rcx,%rdx
  405fff:	48 89 1c 24          	mov    %rbx,(%rsp)
  406003:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  406008:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  40600d:	ff d0                	call   *%rax
  40600f:	88 44 24 57          	mov    %al,0x57(%rsp)
  406013:	8a 44 24 57          	mov    0x57(%rsp),%al
  406017:	48 81 c4 a0 00 00 00 	add    $0xa0,%rsp
  40601e:	5b                   	pop    %rbx
  40601f:	c3                   	ret

0000000000406020 <runtime::conditional_mem_zero>:
  406020:	48 83 ec 30          	sub    $0x30,%rsp
  406024:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  406029:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  40602e:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  406033:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  406038:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40603d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406042:	48 83 f8 00          	cmp    $0x0,%rax
  406046:	0f 9e c0             	setle  %al
  406049:	24 01                	and    $0x1,%al
  40604b:	3c 00                	cmp    $0x0,%al
  40604d:	74 05                	je     406054 <runtime::conditional_mem_zero+0x34>
  40604f:	48 83 c4 30          	add    $0x30,%rsp
  406053:	c3                   	ret
  406054:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  406059:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40605e:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406063:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  406068:	48 c1 e9 03          	shr    $0x3,%rcx
  40606c:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  406071:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  406076:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40607b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406080:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  406085:	48 89 0c 24          	mov    %rcx,(%rsp)
  406089:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40608e:	48 8b 14 24          	mov    (%rsp),%rdx
  406092:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  406097:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40609c:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4060a1:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4060a6:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4060ab:	48 89 f2             	mov    %rsi,%rdx
  4060ae:	48 c1 e2 03          	shl    $0x3,%rdx
  4060b2:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4060b7:	48 8d 0c f1          	lea    (%rcx,%rsi,8),%rcx
  4060bb:	48 29 d0             	sub    %rdx,%rax
  4060be:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  4060c3:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4060c8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4060cd:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4060d2:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  4060d7:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4060dc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4060e1:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  4060e6:	48 c7 44 24 b0 ff ff 	movq   $0xffffffffffffffff,-0x50(%rsp)
  4060ed:	ff ff 
  4060ef:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4060f4:	48 83 c0 01          	add    $0x1,%rax
  4060f8:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4060fd:	48 3b 44 24 b8       	cmp    -0x48(%rsp),%rax
  406102:	7d 38                	jge    40613c <runtime::conditional_mem_zero+0x11c>
  406104:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  406109:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40610e:	48 89 ce             	mov    %rcx,%rsi
  406111:	48 c1 e6 03          	shl    $0x3,%rsi
  406115:	48 89 c2             	mov    %rax,%rdx
  406118:	48 01 f2             	add    %rsi,%rdx
  40611b:	48 89 54 24 88       	mov    %rdx,-0x78(%rsp)
  406120:	48 83 3c c8 00       	cmpq   $0x0,(%rax,%rcx,8)
  406125:	0f 95 c0             	setne  %al
  406128:	24 01                	and    $0x1,%al
  40612a:	3c 00                	cmp    $0x0,%al
  40612c:	74 0c                	je     40613a <runtime::conditional_mem_zero+0x11a>
  40612e:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  406133:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40613a:	eb b3                	jmp    4060ef <runtime::conditional_mem_zero+0xcf>
  40613c:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406141:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  406146:	48 c7 44 24 a0 ff ff 	movq   $0xffffffffffffffff,-0x60(%rsp)
  40614d:	ff ff 
  40614f:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  406154:	48 83 c0 01          	add    $0x1,%rax
  406158:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40615d:	48 3b 44 24 a8       	cmp    -0x58(%rsp),%rax
  406162:	7d 2c                	jge    406190 <runtime::conditional_mem_zero+0x170>
  406164:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  406169:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40616e:	48 89 c2             	mov    %rax,%rdx
  406171:	48 01 ca             	add    %rcx,%rdx
  406174:	48 89 54 24 80       	mov    %rdx,-0x80(%rsp)
  406179:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  40617d:	0f 95 c0             	setne  %al
  406180:	24 01                	and    $0x1,%al
  406182:	3c 00                	cmp    $0x0,%al
  406184:	74 08                	je     40618e <runtime::conditional_mem_zero+0x16e>
  406186:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  40618b:	c6 00 00             	movb   $0x0,(%rax)
  40618e:	eb bf                	jmp    40614f <runtime::conditional_mem_zero+0x12f>
  406190:	48 83 c4 30          	add    $0x30,%rsp
  406194:	c3                   	ret
  406195:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40619c:	00 00 00 00 

00000000004061a0 <runtime::memory_equal>:
  4061a0:	48 83 ec 28          	sub    $0x28,%rsp
  4061a4:	48 89 7c 24 88       	mov    %rdi,-0x78(%rsp)
  4061a9:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  4061ae:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  4061b3:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  4061b8:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  4061bd:	48 8b 54 24 88       	mov    -0x78(%rsp),%rdx
  4061c2:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4061c7:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4061cc:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4061d1:	48 83 f8 00          	cmp    $0x0,%rax
  4061d5:	0f 94 c1             	sete   %cl
  4061d8:	80 e1 01             	and    $0x1,%cl
  4061db:	b0 01                	mov    $0x1,%al
  4061dd:	38 c8                	cmp    %cl,%al
  4061df:	74 1b                	je     4061fc <runtime::memory_equal+0x5c>
  4061e1:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  4061e6:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  4061eb:	48 39 c8             	cmp    %rcx,%rax
  4061ee:	0f 94 c1             	sete   %cl
  4061f1:	80 e1 01             	and    $0x1,%cl
  4061f4:	b0 01                	mov    $0x1,%al
  4061f6:	38 c8                	cmp    %cl,%al
  4061f8:	74 0b                	je     406205 <runtime::memory_equal+0x65>
  4061fa:	eb 07                	jmp    406203 <runtime::memory_equal+0x63>
  4061fc:	b0 01                	mov    $0x1,%al
  4061fe:	48 83 c4 28          	add    $0x28,%rsp
  406202:	c3                   	ret
  406203:	eb 07                	jmp    40620c <runtime::memory_equal+0x6c>
  406205:	b0 01                	mov    $0x1,%al
  406207:	48 83 c4 28          	add    $0x28,%rsp
  40620b:	c3                   	ret
  40620c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  406211:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  406216:	48 8b 54 24 88       	mov    -0x78(%rsp),%rdx
  40621b:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  406220:	48 89 0c 24          	mov    %rcx,(%rsp)
  406224:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406229:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  406230:	00 00 
  406232:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  406239:	00 00 
  40623b:	48 83 7c 24 f8 08    	cmpq   $0x8,-0x8(%rsp)
  406241:	0f 93 c0             	setae  %al
  406244:	24 01                	and    $0x1,%al
  406246:	3c 00                	cmp    $0x0,%al
  406248:	0f 84 43 01 00 00    	je     406391 <runtime::memory_equal+0x1f1>
  40624e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406253:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  406258:	48 c1 e8 04          	shr    $0x4,%rax
  40625c:	48 c1 e0 04          	shl    $0x4,%rax
  406260:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406265:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40626a:	48 3b 44 24 e8       	cmp    -0x18(%rsp),%rax
  40626f:	0f 92 c0             	setb   %al
  406272:	24 01                	and    $0x1,%al
  406274:	3c 00                	cmp    $0x0,%al
  406276:	0f 84 9a 00 00 00    	je     406316 <runtime::memory_equal+0x176>
  40627c:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406281:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406286:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40628a:	0f 29 44 24 d0       	movaps %xmm0,-0x30(%rsp)
  40628f:	48 8b 04 24          	mov    (%rsp),%rax
  406293:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406298:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40629c:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  4062a1:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  4062a6:	0f 28 4c 24 c0       	movaps -0x40(%rsp),%xmm1
  4062ab:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  4062af:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  4062b3:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4062b7:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  4062bc:	0f 28 44 24 b0       	movaps -0x50(%rsp),%xmm0
  4062c1:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  4062c6:	66 0f eb c1          	por    %xmm1,%xmm0
  4062ca:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  4062cf:	66 0f eb c1          	por    %xmm1,%xmm0
  4062d3:	0f 28 c8             	movaps %xmm0,%xmm1
  4062d6:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  4062db:	66 0f eb c1          	por    %xmm1,%xmm0
  4062df:	0f 28 c8             	movaps %xmm0,%xmm1
  4062e2:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  4062e7:	66 0f eb c1          	por    %xmm1,%xmm0
  4062eb:	66 0f 7e c0          	movd   %xmm0,%eax
  4062ef:	3c 00                	cmp    $0x0,%al
  4062f1:	0f 95 c0             	setne  %al
  4062f4:	24 01                	and    $0x1,%al
  4062f6:	3c 00                	cmp    $0x0,%al
  4062f8:	74 07                	je     406301 <runtime::memory_equal+0x161>
  4062fa:	31 c0                	xor    %eax,%eax
  4062fc:	48 83 c4 28          	add    $0x28,%rsp
  406300:	c3                   	ret
  406301:	eb 00                	jmp    406303 <runtime::memory_equal+0x163>
  406303:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406308:	48 83 c0 10          	add    $0x10,%rax
  40630c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406311:	e9 4f ff ff ff       	jmp    406265 <runtime::memory_equal+0xc5>
  406316:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40631b:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  406320:	48 c1 e8 03          	shr    $0x3,%rax
  406324:	48 c1 e0 03          	shl    $0x3,%rax
  406328:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40632d:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406332:	48 3b 44 24 e8       	cmp    -0x18(%rsp),%rax
  406337:	0f 92 c0             	setb   %al
  40633a:	24 01                	and    $0x1,%al
  40633c:	3c 00                	cmp    $0x0,%al
  40633e:	74 4f                	je     40638f <runtime::memory_equal+0x1ef>
  406340:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406345:	48 03 44 24 f0       	add    -0x10(%rsp),%rax
  40634a:	48 8b 00             	mov    (%rax),%rax
  40634d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  406352:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  406357:	48 8b 0c 24          	mov    (%rsp),%rcx
  40635b:	48 03 4c 24 f0       	add    -0x10(%rsp),%rcx
  406360:	48 8b 09             	mov    (%rcx),%rcx
  406363:	48 89 4c 24 a0       	mov    %rcx,-0x60(%rsp)
  406368:	48 3b 44 24 a0       	cmp    -0x60(%rsp),%rax
  40636d:	0f 95 c0             	setne  %al
  406370:	24 01                	and    $0x1,%al
  406372:	3c 00                	cmp    $0x0,%al
  406374:	74 07                	je     40637d <runtime::memory_equal+0x1dd>
  406376:	31 c0                	xor    %eax,%eax
  406378:	48 83 c4 28          	add    $0x28,%rsp
  40637c:	c3                   	ret
  40637d:	eb 00                	jmp    40637f <runtime::memory_equal+0x1df>
  40637f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406384:	48 83 c0 08          	add    $0x8,%rax
  406388:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40638d:	eb 9e                	jmp    40632d <runtime::memory_equal+0x18d>
  40638f:	eb 00                	jmp    406391 <runtime::memory_equal+0x1f1>
  406391:	eb 00                	jmp    406393 <runtime::memory_equal+0x1f3>
  406393:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406398:	48 3b 44 24 f8       	cmp    -0x8(%rsp),%rax
  40639d:	0f 92 c0             	setb   %al
  4063a0:	24 01                	and    $0x1,%al
  4063a2:	3c 00                	cmp    $0x0,%al
  4063a4:	74 3b                	je     4063e1 <runtime::memory_equal+0x241>
  4063a6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4063ab:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4063b0:	8a 04 08             	mov    (%rax,%rcx,1),%al
  4063b3:	48 8b 0c 24          	mov    (%rsp),%rcx
  4063b7:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  4063bc:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  4063bf:	0f 95 c0             	setne  %al
  4063c2:	24 01                	and    $0x1,%al
  4063c4:	3c 00                	cmp    $0x0,%al
  4063c6:	74 07                	je     4063cf <runtime::memory_equal+0x22f>
  4063c8:	31 c0                	xor    %eax,%eax
  4063ca:	48 83 c4 28          	add    $0x28,%rsp
  4063ce:	c3                   	ret
  4063cf:	eb 00                	jmp    4063d1 <runtime::memory_equal+0x231>
  4063d1:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4063d6:	48 83 c0 01          	add    $0x1,%rax
  4063da:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4063df:	eb b2                	jmp    406393 <runtime::memory_equal+0x1f3>
  4063e1:	b0 01                	mov    $0x1,%al
  4063e3:	48 83 c4 28          	add    $0x28,%rsp
  4063e7:	c3                   	ret
  4063e8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4063ef:	00 

00000000004063f0 <runtime::memory_compare>:
  4063f0:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
  4063f7:	48 89 7c 24 98       	mov    %rdi,-0x68(%rsp)
  4063fc:	48 89 74 24 a0       	mov    %rsi,-0x60(%rsp)
  406401:	48 89 54 24 a8       	mov    %rdx,-0x58(%rsp)
  406406:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40640b:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  406410:	48 8b 54 24 a8       	mov    -0x58(%rsp),%rdx
  406415:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40641c:	00 
  40641d:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  406424:	00 
  406425:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40642c:	00 
  40642d:	48 39 c8             	cmp    %rcx,%rax
  406430:	0f 94 c1             	sete   %cl
  406433:	80 e1 01             	and    $0x1,%cl
  406436:	b0 01                	mov    $0x1,%al
  406438:	38 c8                	cmp    %cl,%al
  40643a:	74 17                	je     406453 <runtime::memory_compare+0x63>
  40643c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  406441:	48 83 f8 00          	cmp    $0x0,%rax
  406445:	0f 94 c1             	sete   %cl
  406448:	80 e1 01             	and    $0x1,%cl
  40644b:	b0 01                	mov    $0x1,%al
  40644d:	38 c8                	cmp    %cl,%al
  40644f:	74 23                	je     406474 <runtime::memory_compare+0x84>
  406451:	eb 0a                	jmp    40645d <runtime::memory_compare+0x6d>
  406453:	31 c0                	xor    %eax,%eax
  406455:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40645c:	c3                   	ret
  40645d:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  406462:	48 83 f8 00          	cmp    $0x0,%rax
  406466:	0f 94 c1             	sete   %cl
  406469:	80 e1 01             	and    $0x1,%cl
  40646c:	b0 01                	mov    $0x1,%al
  40646e:	38 c8                	cmp    %cl,%al
  406470:	74 13                	je     406485 <runtime::memory_compare+0x95>
  406472:	eb 0f                	jmp    406483 <runtime::memory_compare+0x93>
  406474:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40647b:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  406482:	c3                   	ret
  406483:	eb 0d                	jmp    406492 <runtime::memory_compare+0xa2>
  406485:	b8 01 00 00 00       	mov    $0x1,%eax
  40648a:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  406491:	c3                   	ret
  406492:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  406497:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  40649c:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  4064a1:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4064a6:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4064ab:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4064b0:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  4064b7:	00 00 
  4064b9:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  4064c0:	00 00 
  4064c2:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4064c7:	48 2b 44 24 60       	sub    0x60(%rsp),%rax
  4064cc:	48 c1 e8 04          	shr    $0x4,%rax
  4064d0:	48 c1 e0 04          	shl    $0x4,%rax
  4064d4:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4064d9:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4064de:	48 3b 44 24 58       	cmp    0x58(%rsp),%rax
  4064e3:	0f 92 c0             	setb   %al
  4064e6:	24 01                	and    $0x1,%al
  4064e8:	3c 00                	cmp    $0x0,%al
  4064ea:	0f 84 44 01 00 00    	je     406634 <runtime::memory_compare+0x244>
  4064f0:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4064f5:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4064fa:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  4064fe:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406503:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  406508:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40650d:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  406511:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  406516:	0f 28 44 24 40       	movaps 0x40(%rsp),%xmm0
  40651b:	0f 28 4c 24 30       	movaps 0x30(%rsp),%xmm1
  406520:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  406524:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  406528:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40652c:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  406531:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  406536:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40653b:	66 0f eb c1          	por    %xmm1,%xmm0
  40653f:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  406544:	66 0f eb c1          	por    %xmm1,%xmm0
  406548:	0f 28 c8             	movaps %xmm0,%xmm1
  40654b:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  406550:	66 0f eb c1          	por    %xmm1,%xmm0
  406554:	0f 28 c8             	movaps %xmm0,%xmm1
  406557:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40655c:	66 0f eb c1          	por    %xmm1,%xmm0
  406560:	66 0f 7e c0          	movd   %xmm0,%eax
  406564:	3c 00                	cmp    $0x0,%al
  406566:	0f 95 c0             	setne  %al
  406569:	24 01                	and    $0x1,%al
  40656b:	3c 00                	cmp    $0x0,%al
  40656d:	0f 84 ac 00 00 00    	je     40661f <runtime::memory_compare+0x22f>
  406573:	66 0f 76 c0          	pcmpeqd %xmm0,%xmm0
  406577:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  40657c:	0f 28 05 7d 2e 00 00 	movaps 0x2e7d(%rip),%xmm0        # 409400 <runtime::type_table+0x230>
  406583:	0f 29 04 24          	movaps %xmm0,(%rsp)
  406587:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  40658c:	0f 28 0c 24          	movaps (%rsp),%xmm1
  406590:	0f 28 54 24 10       	movaps 0x10(%rsp),%xmm2
  406595:	0f 57 db             	xorps  %xmm3,%xmm3
  406598:	66 0f 74 c3          	pcmpeqb %xmm3,%xmm0
  40659c:	66 0f 38 10 ca       	pblendvb %xmm0,%xmm2,%xmm1
  4065a1:	0f 28 c1             	movaps %xmm1,%xmm0
  4065a4:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  4065a9:	0f 28 44 24 f0       	movaps -0x10(%rsp),%xmm0
  4065ae:	0f 28 c8             	movaps %xmm0,%xmm1
  4065b1:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  4065b6:	66 0f da c1          	pminub %xmm1,%xmm0
  4065ba:	66 0f 38 41 c0       	phminposuw %xmm0,%xmm0
  4065bf:	66 0f 7e c0          	movd   %xmm0,%eax
  4065c3:	0f b6 c0             	movzbl %al,%eax
  4065c6:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4065cb:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4065d0:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4065d5:	48 03 4c 24 e8       	add    -0x18(%rsp),%rcx
  4065da:	8a 04 08             	mov    (%rax,%rcx,1),%al
  4065dd:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4065e2:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4065e7:	48 03 54 24 e8       	add    -0x18(%rsp),%rdx
  4065ec:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  4065ef:	0f 92 c0             	setb   %al
  4065f2:	24 01                	and    $0x1,%al
  4065f4:	3c 00                	cmp    $0x0,%al
  4065f6:	74 0e                	je     406606 <runtime::memory_compare+0x216>
  4065f8:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4065ff:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  406604:	eb 0c                	jmp    406612 <runtime::memory_compare+0x222>
  406606:	b8 01 00 00 00       	mov    $0x1,%eax
  40660b:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  406610:	eb 00                	jmp    406612 <runtime::memory_compare+0x222>
  406612:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  406617:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40661e:	c3                   	ret
  40661f:	eb 00                	jmp    406621 <runtime::memory_compare+0x231>
  406621:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  406626:	48 83 c0 10          	add    $0x10,%rax
  40662a:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40662f:	e9 a5 fe ff ff       	jmp    4064d9 <runtime::memory_compare+0xe9>
  406634:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  406639:	48 2b 44 24 60       	sub    0x60(%rsp),%rax
  40663e:	48 c1 e8 03          	shr    $0x3,%rax
  406642:	48 c1 e0 03          	shl    $0x3,%rax
  406646:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40664b:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  406650:	48 3b 44 24 58       	cmp    0x58(%rsp),%rax
  406655:	0f 92 c0             	setb   %al
  406658:	24 01                	and    $0x1,%al
  40665a:	3c 00                	cmp    $0x0,%al
  40665c:	0f 84 59 01 00 00    	je     4067bb <runtime::memory_compare+0x3cb>
  406662:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  406667:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40666c:	48 8b 04 08          	mov    (%rax,%rcx,1),%rax
  406670:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406675:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40667a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40667f:	48 8b 04 08          	mov    (%rax,%rcx,1),%rax
  406683:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  406688:	f3 0f 7e 44 24 e0    	movq   -0x20(%rsp),%xmm0
  40668e:	f3 0f 7e 4c 24 d8    	movq   -0x28(%rsp),%xmm1
  406694:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  406698:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40669c:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4066a0:	66 0f d6 44 24 d0    	movq   %xmm0,-0x30(%rsp)
  4066a6:	f3 0f 7e 44 24 d0    	movq   -0x30(%rsp),%xmm0
  4066ac:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  4066b1:	66 0f eb c1          	por    %xmm1,%xmm0
  4066b5:	0f 28 c8             	movaps %xmm0,%xmm1
  4066b8:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  4066bd:	66 0f eb c1          	por    %xmm1,%xmm0
  4066c1:	0f 28 c8             	movaps %xmm0,%xmm1
  4066c4:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  4066c9:	66 0f eb c1          	por    %xmm1,%xmm0
  4066cd:	66 0f 7e c0          	movd   %xmm0,%eax
  4066d1:	3c 00                	cmp    $0x0,%al
  4066d3:	0f 95 c0             	setne  %al
  4066d6:	24 01                	and    $0x1,%al
  4066d8:	3c 00                	cmp    $0x0,%al
  4066da:	0f 84 c6 00 00 00    	je     4067a6 <runtime::memory_compare+0x3b6>
  4066e0:	48 c7 44 24 c8 ff ff 	movq   $0xffffffffffffffff,-0x38(%rsp)
  4066e7:	ff ff 
  4066e9:	48 b8 00 01 02 03 04 	movabs $0x706050403020100,%rax
  4066f0:	05 06 07 
  4066f3:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4066f8:	f3 0f 7e 44 24 d0    	movq   -0x30(%rsp),%xmm0
  4066fe:	f3 0f 7e 4c 24 c0    	movq   -0x40(%rsp),%xmm1
  406704:	f3 0f 7e 54 24 c8    	movq   -0x38(%rsp),%xmm2
  40670a:	0f 57 db             	xorps  %xmm3,%xmm3
  40670d:	66 0f 74 c3          	pcmpeqb %xmm3,%xmm0
  406711:	66 0f 38 10 ca       	pblendvb %xmm0,%xmm2,%xmm1
  406716:	0f 28 c1             	movaps %xmm1,%xmm0
  406719:	66 0f d6 44 24 b8    	movq   %xmm0,-0x48(%rsp)
  40671f:	f3 0f 7e 44 24 b8    	movq   -0x48(%rsp),%xmm0
  406725:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40672a:	66 0f da c1          	pminub %xmm1,%xmm0
  40672e:	0f 28 c8             	movaps %xmm0,%xmm1
  406731:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  406736:	66 0f da c1          	pminub %xmm1,%xmm0
  40673a:	0f 28 c8             	movaps %xmm0,%xmm1
  40673d:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  406742:	66 0f da c1          	pminub %xmm1,%xmm0
  406746:	66 0f 7e c0          	movd   %xmm0,%eax
  40674a:	0f b6 c0             	movzbl %al,%eax
  40674d:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  406752:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  406757:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40675c:	48 03 4c 24 b0       	add    -0x50(%rsp),%rcx
  406761:	8a 04 08             	mov    (%rax,%rcx,1),%al
  406764:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  406769:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40676e:	48 03 54 24 b0       	add    -0x50(%rsp),%rdx
  406773:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  406776:	0f 92 c0             	setb   %al
  406779:	24 01                	and    $0x1,%al
  40677b:	3c 00                	cmp    $0x0,%al
  40677d:	74 0e                	je     40678d <runtime::memory_compare+0x39d>
  40677f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  406786:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40678b:	eb 0c                	jmp    406799 <runtime::memory_compare+0x3a9>
  40678d:	b8 01 00 00 00       	mov    $0x1,%eax
  406792:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  406797:	eb 00                	jmp    406799 <runtime::memory_compare+0x3a9>
  406799:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40679e:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  4067a5:	c3                   	ret
  4067a6:	eb 00                	jmp    4067a8 <runtime::memory_compare+0x3b8>
  4067a8:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4067ad:	48 83 c0 08          	add    $0x8,%rax
  4067b1:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4067b6:	e9 90 fe ff ff       	jmp    40664b <runtime::memory_compare+0x25b>
  4067bb:	eb 00                	jmp    4067bd <runtime::memory_compare+0x3cd>
  4067bd:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4067c2:	48 3b 44 24 68       	cmp    0x68(%rsp),%rax
  4067c7:	0f 92 c0             	setb   %al
  4067ca:	24 01                	and    $0x1,%al
  4067cc:	3c 00                	cmp    $0x0,%al
  4067ce:	0f 84 8d 00 00 00    	je     406861 <runtime::memory_compare+0x471>
  4067d4:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4067d9:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4067de:	8a 04 08             	mov    (%rax,%rcx,1),%al
  4067e1:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4067e6:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4067eb:	32 04 11             	xor    (%rcx,%rdx,1),%al
  4067ee:	3c 00                	cmp    $0x0,%al
  4067f0:	0f 95 c0             	setne  %al
  4067f3:	24 01                	and    $0x1,%al
  4067f5:	3c 00                	cmp    $0x0,%al
  4067f7:	74 53                	je     40684c <runtime::memory_compare+0x45c>
  4067f9:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4067fe:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  406803:	0f b6 04 08          	movzbl (%rax,%rcx,1),%eax
  406807:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40680c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  406811:	0f b6 0c 11          	movzbl (%rcx,%rdx,1),%ecx
  406815:	48 29 c8             	sub    %rcx,%rax
  406818:	48 83 f8 00          	cmp    $0x0,%rax
  40681c:	0f 9c c0             	setl   %al
  40681f:	24 01                	and    $0x1,%al
  406821:	3c 00                	cmp    $0x0,%al
  406823:	74 0e                	je     406833 <runtime::memory_compare+0x443>
  406825:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40682c:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  406831:	eb 0c                	jmp    40683f <runtime::memory_compare+0x44f>
  406833:	b8 01 00 00 00       	mov    $0x1,%eax
  406838:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40683d:	eb 00                	jmp    40683f <runtime::memory_compare+0x44f>
  40683f:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  406844:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40684b:	c3                   	ret
  40684c:	eb 00                	jmp    40684e <runtime::memory_compare+0x45e>
  40684e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  406853:	48 83 c0 01          	add    $0x1,%rax
  406857:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40685c:	e9 5c ff ff ff       	jmp    4067bd <runtime::memory_compare+0x3cd>
  406861:	31 c0                	xor    %eax,%eax
  406863:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40686a:	c3                   	ret
  40686b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000406870 <runtime::memory_compare_zero>:
  406870:	50                   	push   %rax
  406871:	48 89 7c 24 88       	mov    %rdi,-0x78(%rsp)
  406876:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  40687b:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  406880:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  406885:	48 89 04 24          	mov    %rax,(%rsp)
  406889:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40688e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  406893:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  40689a:	00 00 
  40689c:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  4068a3:	00 00 
  4068a5:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4068aa:	48 83 7c 24 f0 08    	cmpq   $0x8,-0x10(%rsp)
  4068b0:	0f 93 c0             	setae  %al
  4068b3:	24 01                	and    $0x1,%al
  4068b5:	3c 00                	cmp    $0x0,%al
  4068b7:	0f 84 24 01 00 00    	je     4069e1 <runtime::memory_compare_zero+0x171>
  4068bd:	0f 57 c0             	xorps  %xmm0,%xmm0
  4068c0:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  4068c5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4068ca:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  4068cf:	48 c1 e8 04          	shr    $0x4,%rax
  4068d3:	48 c1 e0 04          	shl    $0x4,%rax
  4068d7:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4068dc:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4068e1:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4068e6:	0f 92 c0             	setb   %al
  4068e9:	24 01                	and    $0x1,%al
  4068eb:	3c 00                	cmp    $0x0,%al
  4068ed:	0f 84 88 00 00 00    	je     40697b <runtime::memory_compare_zero+0x10b>
  4068f3:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4068f8:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4068fd:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  406901:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  406906:	0f 28 44 24 c0       	movaps -0x40(%rsp),%xmm0
  40690b:	0f 28 4c 24 b0       	movaps -0x50(%rsp),%xmm1
  406910:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  406914:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  406918:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40691c:	0f 29 44 24 a0       	movaps %xmm0,-0x60(%rsp)
  406921:	0f 28 44 24 a0       	movaps -0x60(%rsp),%xmm0
  406926:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40692b:	66 0f eb c1          	por    %xmm1,%xmm0
  40692f:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  406934:	66 0f eb c1          	por    %xmm1,%xmm0
  406938:	0f 28 c8             	movaps %xmm0,%xmm1
  40693b:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  406940:	66 0f eb c1          	por    %xmm1,%xmm0
  406944:	0f 28 c8             	movaps %xmm0,%xmm1
  406947:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40694c:	66 0f eb c1          	por    %xmm1,%xmm0
  406950:	66 0f 7e c0          	movd   %xmm0,%eax
  406954:	3c 00                	cmp    $0x0,%al
  406956:	0f 95 c0             	setne  %al
  406959:	24 01                	and    $0x1,%al
  40695b:	3c 00                	cmp    $0x0,%al
  40695d:	74 07                	je     406966 <runtime::memory_compare_zero+0xf6>
  40695f:	b8 01 00 00 00       	mov    $0x1,%eax
  406964:	59                   	pop    %rcx
  406965:	c3                   	ret
  406966:	eb 00                	jmp    406968 <runtime::memory_compare_zero+0xf8>
  406968:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40696d:	48 83 c0 10          	add    $0x10,%rax
  406971:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406976:	e9 61 ff ff ff       	jmp    4068dc <runtime::memory_compare_zero+0x6c>
  40697b:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406980:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  406985:	48 c1 e8 03          	shr    $0x3,%rax
  406989:	48 c1 e0 03          	shl    $0x3,%rax
  40698d:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406992:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406997:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  40699c:	0f 92 c0             	setb   %al
  40699f:	24 01                	and    $0x1,%al
  4069a1:	3c 00                	cmp    $0x0,%al
  4069a3:	74 3a                	je     4069df <runtime::memory_compare_zero+0x16f>
  4069a5:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4069aa:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  4069af:	48 8b 00             	mov    (%rax),%rax
  4069b2:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  4069b7:	48 83 7c 24 98 00    	cmpq   $0x0,-0x68(%rsp)
  4069bd:	0f 95 c0             	setne  %al
  4069c0:	24 01                	and    $0x1,%al
  4069c2:	3c 00                	cmp    $0x0,%al
  4069c4:	74 07                	je     4069cd <runtime::memory_compare_zero+0x15d>
  4069c6:	b8 01 00 00 00       	mov    $0x1,%eax
  4069cb:	59                   	pop    %rcx
  4069cc:	c3                   	ret
  4069cd:	eb 00                	jmp    4069cf <runtime::memory_compare_zero+0x15f>
  4069cf:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4069d4:	48 83 c0 08          	add    $0x8,%rax
  4069d8:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4069dd:	eb b3                	jmp    406992 <runtime::memory_compare_zero+0x122>
  4069df:	eb 00                	jmp    4069e1 <runtime::memory_compare_zero+0x171>
  4069e1:	eb 00                	jmp    4069e3 <runtime::memory_compare_zero+0x173>
  4069e3:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4069e8:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  4069ed:	0f 92 c0             	setb   %al
  4069f0:	24 01                	and    $0x1,%al
  4069f2:	3c 00                	cmp    $0x0,%al
  4069f4:	74 30                	je     406a26 <runtime::memory_compare_zero+0x1b6>
  4069f6:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4069fb:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406a00:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  406a04:	0f 95 c0             	setne  %al
  406a07:	24 01                	and    $0x1,%al
  406a09:	3c 00                	cmp    $0x0,%al
  406a0b:	74 07                	je     406a14 <runtime::memory_compare_zero+0x1a4>
  406a0d:	b8 01 00 00 00       	mov    $0x1,%eax
  406a12:	59                   	pop    %rcx
  406a13:	c3                   	ret
  406a14:	eb 00                	jmp    406a16 <runtime::memory_compare_zero+0x1a6>
  406a16:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406a1b:	48 83 c0 01          	add    $0x1,%rax
  406a1f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406a24:	eb bd                	jmp    4069e3 <runtime::memory_compare_zero+0x173>
  406a26:	31 c0                	xor    %eax,%eax
  406a28:	59                   	pop    %rcx
  406a29:	c3                   	ret
  406a2a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000406a30 <runtime::cstring_len>:
  406a30:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  406a35:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  406a3a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406a3f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406a44:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  406a49:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406a4e:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  406a54:	0f 95 c0             	setne  %al
  406a57:	24 01                	and    $0x1,%al
  406a59:	3c 00                	cmp    $0x0,%al
  406a5b:	74 21                	je     406a7e <runtime::cstring_len+0x4e>
  406a5d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406a62:	80 38 00             	cmpb   $0x0,(%rax)
  406a65:	0f 95 c0             	setne  %al
  406a68:	24 01                	and    $0x1,%al
  406a6a:	3c 00                	cmp    $0x0,%al
  406a6c:	74 10                	je     406a7e <runtime::cstring_len+0x4e>
  406a6e:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406a73:	48 83 c0 01          	add    $0x1,%rax
  406a77:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406a7c:	eb d0                	jmp    406a4e <runtime::cstring_len+0x1e>
  406a7e:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406a83:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  406a88:	c3                   	ret
  406a89:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000406a90 <runtime::cstring_to_string>:
  406a90:	48 83 ec 48          	sub    $0x48,%rsp
  406a94:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406a99:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406a9e:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  406aa3:	48 83 f8 00          	cmp    $0x0,%rax
  406aa7:	0f 94 c0             	sete   %al
  406aaa:	24 01                	and    $0x1,%al
  406aac:	3c 00                	cmp    $0x0,%al
  406aae:	74 21                	je     406ad1 <runtime::cstring_to_string+0x41>
  406ab0:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  406ab7:	00 00 
  406ab9:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  406ac0:	00 00 
  406ac2:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  406ac7:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406acc:	48 83 c4 48          	add    $0x48,%rsp
  406ad0:	c3                   	ret
  406ad1:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406ad6:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406adb:	e8 50 ff ff ff       	call   406a30 <runtime::cstring_len>
  406ae0:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406ae5:	0f 57 c0             	xorps  %xmm0,%xmm0
  406ae8:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  406aed:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  406af4:	00 00 
  406af6:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  406afd:	00 00 
  406aff:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  406b04:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406b09:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406b0e:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  406b13:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406b18:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  406b1d:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  406b22:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406b27:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  406b2c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406b31:	48 83 c4 48          	add    $0x48,%rsp
  406b35:	c3                   	ret
  406b36:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  406b3d:	00 00 00 

0000000000406b40 <runtime::cstring_eq>:
  406b40:	48 83 ec 48          	sub    $0x48,%rsp
  406b44:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406b49:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406b4e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406b53:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406b58:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  406b5d:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406b62:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406b67:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  406b6c:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  406b71:	48 3b 44 24 28       	cmp    0x28(%rsp),%rax
  406b76:	0f 94 c0             	sete   %al
  406b79:	24 01                	and    $0x1,%al
  406b7b:	3c 00                	cmp    $0x0,%al
  406b7d:	74 07                	je     406b86 <runtime::cstring_eq+0x46>
  406b7f:	b0 01                	mov    $0x1,%al
  406b81:	48 83 c4 48          	add    $0x48,%rsp
  406b85:	c3                   	ret
  406b86:	48 83 7c 24 30 00    	cmpq   $0x0,0x30(%rsp)
  406b8c:	0f 94 c0             	sete   %al
  406b8f:	24 01                	and    $0x1,%al
  406b91:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
  406b97:	0f 94 c1             	sete   %cl
  406b9a:	80 e1 01             	and    $0x1,%cl
  406b9d:	30 c8                	xor    %cl,%al
  406b9f:	3c 00                	cmp    $0x0,%al
  406ba1:	74 07                	je     406baa <runtime::cstring_eq+0x6a>
  406ba3:	31 c0                	xor    %eax,%eax
  406ba5:	48 83 c4 48          	add    $0x48,%rsp
  406ba9:	c3                   	ret
  406baa:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406baf:	e8 7c fe ff ff       	call   406a30 <runtime::cstring_len>
  406bb4:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406bb9:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406bbe:	e8 6d fe ff ff       	call   406a30 <runtime::cstring_len>
  406bc3:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  406bc8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406bcd:	48 3b 44 24 18       	cmp    0x18(%rsp),%rax
  406bd2:	0f 95 c0             	setne  %al
  406bd5:	24 01                	and    $0x1,%al
  406bd7:	3c 00                	cmp    $0x0,%al
  406bd9:	74 07                	je     406be2 <runtime::cstring_eq+0xa2>
  406bdb:	31 c0                	xor    %eax,%eax
  406bdd:	48 83 c4 48          	add    $0x48,%rsp
  406be1:	c3                   	ret
  406be2:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406be7:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  406bec:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  406bf1:	e8 aa f5 ff ff       	call   4061a0 <runtime::memory_equal>
  406bf6:	48 83 c4 48          	add    $0x48,%rsp
  406bfa:	c3                   	ret
  406bfb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000406c00 <runtime::cstring_ne>:
  406c00:	48 83 ec 28          	sub    $0x28,%rsp
  406c04:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406c09:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406c0e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  406c13:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406c18:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  406c1d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  406c22:	e8 19 ff ff ff       	call   406b40 <runtime::cstring_eq>
  406c27:	3c 00                	cmp    $0x0,%al
  406c29:	0f 94 c0             	sete   %al
  406c2c:	24 01                	and    $0x1,%al
  406c2e:	48 83 c4 28          	add    $0x28,%rsp
  406c32:	c3                   	ret
  406c33:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  406c3a:	84 00 00 00 00 00 

0000000000406c40 <__truncsfhf2>:
  406c40:	48 83 ec 18          	sub    $0x18,%rsp
  406c44:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  406c4a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  406c50:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  406c56:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  406c5d:	00 
  406c5e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  406c65:	00 
  406c66:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  406c6d:	00 
  406c6e:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  406c75:	00 
  406c76:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  406c7d:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  406c83:	8b 44 24 10          	mov    0x10(%rsp),%eax
  406c87:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  406c8b:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  406c8f:	c1 f9 10             	sar    $0x10,%ecx
  406c92:	b2 01                	mov    $0x1,%dl
  406c94:	31 c0                	xor    %eax,%eax
  406c96:	f6 c2 01             	test   $0x1,%dl
  406c99:	0f 45 c1             	cmovne %ecx,%eax
  406c9c:	25 00 80 00 00       	and    $0x8000,%eax
  406ca1:	89 44 24 08          	mov    %eax,0x8(%rsp)
  406ca5:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  406ca9:	c1 f9 17             	sar    $0x17,%ecx
  406cac:	b2 01                	mov    $0x1,%dl
  406cae:	31 c0                	xor    %eax,%eax
  406cb0:	f6 c2 01             	test   $0x1,%dl
  406cb3:	0f 45 c1             	cmovne %ecx,%eax
  406cb6:	25 ff 00 00 00       	and    $0xff,%eax
  406cbb:	83 e8 70             	sub    $0x70,%eax
  406cbe:	89 44 24 04          	mov    %eax,0x4(%rsp)
  406cc2:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  406cc6:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  406ccb:	89 04 24             	mov    %eax,(%rsp)
  406cce:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  406cd3:	0f 9e c0             	setle  %al
  406cd6:	24 01                	and    $0x1,%al
  406cd8:	3c 00                	cmp    $0x0,%al
  406cda:	0f 84 82 00 00 00    	je     406d62 <__truncsfhf2+0x122>
  406ce0:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  406ce5:	0f 9c c0             	setl   %al
  406ce8:	24 01                	and    $0x1,%al
  406cea:	3c 00                	cmp    $0x0,%al
  406cec:	74 16                	je     406d04 <__truncsfhf2+0xc4>
  406cee:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  406cf3:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  406cf8:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  406cff:	48 83 c4 18          	add    $0x18,%rsp
  406d03:	c3                   	ret
  406d04:	8b 04 24             	mov    (%rsp),%eax
  406d07:	0d 00 00 80 00       	or     $0x800000,%eax
  406d0c:	ba 01 00 00 00       	mov    $0x1,%edx
  406d11:	2b 54 24 04          	sub    0x4(%rsp),%edx
  406d15:	89 d1                	mov    %edx,%ecx
  406d17:	d3 f8                	sar    %cl,%eax
  406d19:	89 c1                	mov    %eax,%ecx
  406d1b:	31 c0                	xor    %eax,%eax
  406d1d:	83 fa 20             	cmp    $0x20,%edx
  406d20:	0f 42 c1             	cmovb  %ecx,%eax
  406d23:	89 04 24             	mov    %eax,(%rsp)
  406d26:	8b 04 24             	mov    (%rsp),%eax
  406d29:	25 00 10 00 00       	and    $0x1000,%eax
  406d2e:	83 f8 00             	cmp    $0x0,%eax
  406d31:	0f 95 c0             	setne  %al
  406d34:	24 01                	and    $0x1,%al
  406d36:	3c 00                	cmp    $0x0,%al
  406d38:	74 0b                	je     406d45 <__truncsfhf2+0x105>
  406d3a:	8b 04 24             	mov    (%rsp),%eax
  406d3d:	05 00 20 00 00       	add    $0x2000,%eax
  406d42:	89 04 24             	mov    %eax,(%rsp)
  406d45:	8b 44 24 08          	mov    0x8(%rsp),%eax
  406d49:	8b 0c 24             	mov    (%rsp),%ecx
  406d4c:	c1 e9 0d             	shr    $0xd,%ecx
  406d4f:	09 c8                	or     %ecx,%eax
  406d51:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  406d56:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  406d5d:	48 83 c4 18          	add    $0x18,%rsp
  406d61:	c3                   	ret
  406d62:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  406d69:	00 
  406d6a:	0f 94 c0             	sete   %al
  406d6d:	24 01                	and    $0x1,%al
  406d6f:	3c 00                	cmp    $0x0,%al
  406d71:	74 59                	je     406dcc <__truncsfhf2+0x18c>
  406d73:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  406d77:	0f 94 c0             	sete   %al
  406d7a:	24 01                	and    $0x1,%al
  406d7c:	3c 00                	cmp    $0x0,%al
  406d7e:	74 1a                	je     406d9a <__truncsfhf2+0x15a>
  406d80:	8b 44 24 08          	mov    0x8(%rsp),%eax
  406d84:	0d 00 7c 00 00       	or     $0x7c00,%eax
  406d89:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  406d8e:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  406d95:	48 83 c4 18          	add    $0x18,%rsp
  406d99:	c3                   	ret
  406d9a:	8b 04 24             	mov    (%rsp),%eax
  406d9d:	c1 f8 0d             	sar    $0xd,%eax
  406da0:	89 04 24             	mov    %eax,(%rsp)
  406da3:	8b 44 24 08          	mov    0x8(%rsp),%eax
  406da7:	8b 0c 24             	mov    (%rsp),%ecx
  406daa:	09 c8                	or     %ecx,%eax
  406dac:	85 c9                	test   %ecx,%ecx
  406dae:	0f 94 c1             	sete   %cl
  406db1:	0f b6 c9             	movzbl %cl,%ecx
  406db4:	09 c8                	or     %ecx,%eax
  406db6:	0d 00 7c 00 00       	or     $0x7c00,%eax
  406dbb:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  406dc0:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  406dc7:	48 83 c4 18          	add    $0x18,%rsp
  406dcb:	c3                   	ret
  406dcc:	8b 04 24             	mov    (%rsp),%eax
  406dcf:	25 00 10 00 00       	and    $0x1000,%eax
  406dd4:	83 f8 00             	cmp    $0x0,%eax
  406dd7:	0f 95 c0             	setne  %al
  406dda:	24 01                	and    $0x1,%al
  406ddc:	3c 00                	cmp    $0x0,%al
  406dde:	74 33                	je     406e13 <__truncsfhf2+0x1d3>
  406de0:	8b 04 24             	mov    (%rsp),%eax
  406de3:	05 00 20 00 00       	add    $0x2000,%eax
  406de8:	89 04 24             	mov    %eax,(%rsp)
  406deb:	8b 04 24             	mov    (%rsp),%eax
  406dee:	25 00 00 80 00       	and    $0x800000,%eax
  406df3:	83 f8 00             	cmp    $0x0,%eax
  406df6:	0f 95 c0             	setne  %al
  406df9:	24 01                	and    $0x1,%al
  406dfb:	3c 00                	cmp    $0x0,%al
  406dfd:	74 12                	je     406e11 <__truncsfhf2+0x1d1>
  406dff:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  406e06:	8b 44 24 04          	mov    0x4(%rsp),%eax
  406e0a:	83 c0 01             	add    $0x1,%eax
  406e0d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  406e11:	eb 00                	jmp    406e13 <__truncsfhf2+0x1d3>
  406e13:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  406e18:	0f 9f c0             	setg   %al
  406e1b:	24 01                	and    $0x1,%al
  406e1d:	3c 00                	cmp    $0x0,%al
  406e1f:	74 75                	je     406e96 <__truncsfhf2+0x256>
  406e21:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  406e28:	00 00 00 
  406e2b:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  406e30:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  406e37:	00 00 
  406e39:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  406e3f:	0f 9c c0             	setl   %al
  406e42:	24 01                	and    $0x1,%al
  406e44:	3c 00                	cmp    $0x0,%al
  406e46:	74 34                	je     406e7c <__truncsfhf2+0x23c>
  406e48:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  406e4d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  406e52:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  406e57:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  406e5d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  406e62:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  406e67:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  406e6c:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  406e71:	48 83 c0 01          	add    $0x1,%rax
  406e75:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  406e7a:	eb bd                	jmp    406e39 <__truncsfhf2+0x1f9>
  406e7c:	8b 44 24 08          	mov    0x8(%rsp),%eax
  406e80:	0d 00 7c 00 00       	or     $0x7c00,%eax
  406e85:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  406e8a:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  406e91:	48 83 c4 18          	add    $0x18,%rsp
  406e95:	c3                   	ret
  406e96:	8b 44 24 08          	mov    0x8(%rsp),%eax
  406e9a:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  406e9e:	c1 e1 0a             	shl    $0xa,%ecx
  406ea1:	09 c8                	or     %ecx,%eax
  406ea3:	8b 0c 24             	mov    (%rsp),%ecx
  406ea6:	c1 e9 0d             	shr    $0xd,%ecx
  406ea9:	09 c8                	or     %ecx,%eax
  406eab:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  406eb0:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  406eb7:	48 83 c4 18          	add    $0x18,%rsp
  406ebb:	c3                   	ret
  406ebc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000406ec0 <__truncdfhf2>:
  406ec0:	48 83 ec 18          	sub    $0x18,%rsp
  406ec4:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  406eca:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  406ed0:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  406ed6:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  406eda:	e8 61 fd ff ff       	call   406c40 <__truncsfhf2>
  406edf:	48 83 c4 18          	add    $0x18,%rsp
  406ee3:	c3                   	ret
  406ee4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  406eeb:	00 00 00 00 00 

0000000000406ef0 <__gnu_h2f_ieee>:
  406ef0:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  406ef6:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  406efc:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  406f03:	00 
  406f04:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  406f0b:	00 
  406f0c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  406f11:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  406f16:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  406f1d:	00 
  406f1e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  406f25:	00 
  406f26:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  406f2d:	00 
  406f2e:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  406f35:	77 
  406f36:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  406f3d:	47 
  406f3e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  406f43:	66 25 ff 7f          	and    $0x7fff,%ax
  406f47:	0f b7 c8             	movzwl %ax,%ecx
  406f4a:	c1 e1 0d             	shl    $0xd,%ecx
  406f4d:	b2 01                	mov    $0x1,%dl
  406f4f:	31 c0                	xor    %eax,%eax
  406f51:	f6 c2 01             	test   $0x1,%dl
  406f54:	0f 45 c1             	cmovne %ecx,%eax
  406f57:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  406f5b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  406f61:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  406f67:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  406f6d:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  406f73:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  406f78:	0f 93 c0             	setae  %al
  406f7b:	24 01                	and    $0x1,%al
  406f7d:	3c 00                	cmp    $0x0,%al
  406f7f:	74 0d                	je     406f8e <__gnu_h2f_ieee+0x9e>
  406f81:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  406f85:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  406f8a:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  406f8e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  406f93:	66 25 00 80          	and    $0x8000,%ax
  406f97:	0f b7 c8             	movzwl %ax,%ecx
  406f9a:	c1 e1 10             	shl    $0x10,%ecx
  406f9d:	b2 01                	mov    $0x1,%dl
  406f9f:	31 c0                	xor    %eax,%eax
  406fa1:	f6 c2 01             	test   $0x1,%dl
  406fa4:	0f 45 c1             	cmovne %ecx,%eax
  406fa7:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  406fab:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  406faf:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  406fb5:	c3                   	ret
  406fb6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  406fbd:	00 00 00 

0000000000406fc0 <__gnu_f2h_ieee>:
  406fc0:	50                   	push   %rax
  406fc1:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  406fc6:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  406fcb:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  406fd1:	e8 6a fc ff ff       	call   406c40 <__truncsfhf2>
  406fd6:	58                   	pop    %rax
  406fd7:	c3                   	ret
  406fd8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  406fdf:	00 

0000000000406fe0 <__extendhfsf2>:
  406fe0:	50                   	push   %rax
  406fe1:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  406fe7:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  406fed:	0f 28 c8             	movaps %xmm0,%xmm1
  406ff0:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  406ff7:	00 
  406ff8:	e8 f3 fe ff ff       	call   406ef0 <__gnu_h2f_ieee>
  406ffd:	58                   	pop    %rax
  406ffe:	c3                   	ret
  406fff:	90                   	nop

0000000000407000 <__floattidf>:
  407000:	53                   	push   %rbx
  407001:	48 83 ec 10          	sub    $0x10,%rsp
  407005:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40700a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40700f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  407014:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  407019:	48 89 04 24          	mov    %rax,(%rsp)
  40701d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  407022:	48 09 c8             	or     %rcx,%rax
  407025:	0f 94 c0             	sete   %al
  407028:	24 01                	and    $0x1,%al
  40702a:	3c 00                	cmp    $0x0,%al
  40702c:	74 09                	je     407037 <__floattidf+0x37>
  40702e:	0f 57 c0             	xorps  %xmm0,%xmm0
  407031:	48 83 c4 10          	add    $0x10,%rsp
  407035:	5b                   	pop    %rbx
  407036:	c3                   	ret
  407037:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40703c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  407041:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  407046:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40704b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  407050:	48 c1 f8 3f          	sar    $0x3f,%rax
  407054:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  407059:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40705e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  407063:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  407068:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40706d:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  407072:	48 31 d0             	xor    %rdx,%rax
  407075:	48 31 f1             	xor    %rsi,%rcx
  407078:	48 29 f1             	sub    %rsi,%rcx
  40707b:	48 19 d0             	sbb    %rdx,%rax
  40707e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  407083:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  407088:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  40708d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  407092:	48 0f bd c2          	bsr    %rdx,%rax
  407096:	48 83 f0 3f          	xor    $0x3f,%rax
  40709a:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40709f:	48 0f bd ce          	bsr    %rsi,%rcx
  4070a3:	48 83 f1 3f          	xor    $0x3f,%rcx
  4070a7:	48 83 c1 40          	add    $0x40,%rcx
  4070ab:	48 85 d2             	test   %rdx,%rdx
  4070ae:	48 0f 45 c8          	cmovne %rax,%rcx
  4070b2:	31 c0                	xor    %eax,%eax
  4070b4:	ba 80 00 00 00       	mov    $0x80,%edx
  4070b9:	48 29 ca             	sub    %rcx,%rdx
  4070bc:	48 89 c1             	mov    %rax,%rcx
  4070bf:	48 19 c9             	sbb    %rcx,%rcx
  4070c2:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4070c7:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4070cc:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  4070d0:	ff c9                	dec    %ecx
  4070d2:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  4070d6:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4070db:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4070e0:	ba 35 00 00 00       	mov    $0x35,%edx
  4070e5:	48 29 f2             	sub    %rsi,%rdx
  4070e8:	48 19 c8             	sbb    %rcx,%rax
  4070eb:	0f 9c c0             	setl   %al
  4070ee:	24 01                	and    $0x1,%al
  4070f0:	3c 00                	cmp    $0x0,%al
  4070f2:	0f 84 c0 01 00 00    	je     4072b8 <__floattidf+0x2b8>
  4070f8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4070fd:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  407102:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407107:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40710c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  407111:	0f 28 0d f8 22 00 00 	movaps 0x22f8(%rip),%xmm1        # 409410 <runtime::type_table+0x240>
  407118:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40711c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  407121:	74 17                	je     40713a <__floattidf+0x13a>
  407123:	eb 00                	jmp    407125 <__floattidf+0x125>
  407125:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  40712a:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40712f:	48 83 f0 37          	xor    $0x37,%rax
  407133:	48 09 c8             	or     %rcx,%rax
  407136:	74 26                	je     40715e <__floattidf+0x15e>
  407138:	eb 29                	jmp    407163 <__floattidf+0x163>
  40713a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40713f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  407144:	48 89 d0             	mov    %rdx,%rax
  407147:	48 01 c0             	add    %rax,%rax
  40714a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40714f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  407154:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407159:	e9 d6 00 00 00       	jmp    407234 <__floattidf+0x234>
  40715e:	e9 d1 00 00 00       	jmp    407234 <__floattidf+0x234>
  407163:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  407168:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  40716d:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  407172:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407177:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40717c:	49 89 fb             	mov    %rdi,%r11
  40717f:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  407183:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  407187:	44 88 db             	mov    %r11b,%bl
  40718a:	88 d9                	mov    %bl,%cl
  40718c:	49 89 f2             	mov    %rsi,%r10
  40718f:	49 d3 ea             	shr    %cl,%r10
  407192:	88 d9                	mov    %bl,%cl
  407194:	49 89 d1             	mov    %rdx,%r9
  407197:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40719b:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  4071a0:	45 31 c0             	xor    %r8d,%r8d
  4071a3:	f6 c3 40             	test   $0x40,%bl
  4071a6:	4d 0f 45 ca          	cmovne %r10,%r9
  4071aa:	4d 0f 45 d0          	cmovne %r8,%r10
  4071ae:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  4071b5:	48 83 d8 00          	sbb    $0x0,%rax
  4071b9:	4c 89 c0             	mov    %r8,%rax
  4071bc:	49 0f 42 c2          	cmovb  %r10,%rax
  4071c0:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  4071c5:	4c 89 c0             	mov    %r8,%rax
  4071c8:	49 0f 42 c1          	cmovb  %r9,%rax
  4071cc:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  4071d2:	49 29 fb             	sub    %rdi,%r11
  4071d5:	4c 89 c7             	mov    %r8,%rdi
  4071d8:	48 19 cf             	sbb    %rcx,%rdi
  4071db:	45 88 d9             	mov    %r11b,%r9b
  4071de:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  4071e5:	44 88 c9             	mov    %r9b,%cl
  4071e8:	4c 89 d3             	mov    %r10,%rbx
  4071eb:	48 d3 eb             	shr    %cl,%rbx
  4071ee:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  4071f3:	41 f6 c1 40          	test   $0x40,%r9b
  4071f7:	49 89 d9             	mov    %rbx,%r9
  4071fa:	4d 0f 45 c8          	cmovne %r8,%r9
  4071fe:	4c 0f 45 d3          	cmovne %rbx,%r10
  407202:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  407209:	48 83 df 00          	sbb    $0x0,%rdi
  40720d:	4c 89 c7             	mov    %r8,%rdi
  407210:	49 0f 42 fa          	cmovb  %r10,%rdi
  407214:	4d 0f 42 c1          	cmovb  %r9,%r8
  407218:	4c 21 c6             	and    %r8,%rsi
  40721b:	48 21 fa             	and    %rdi,%rdx
  40721e:	48 09 f2             	or     %rsi,%rdx
  407221:	0f 95 c2             	setne  %dl
  407224:	0f b6 d2             	movzbl %dl,%edx
  407227:	48 09 d0             	or     %rdx,%rax
  40722a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40722f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407234:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  407239:	89 c1                	mov    %eax,%ecx
  40723b:	83 e1 04             	and    $0x4,%ecx
  40723e:	c1 e9 02             	shr    $0x2,%ecx
  407241:	48 09 c8             	or     %rcx,%rax
  407244:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407249:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40724e:	48 83 c0 01          	add    $0x1,%rax
  407252:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  407258:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40725d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  407262:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  407267:	48 89 c8             	mov    %rcx,%rax
  40726a:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40726f:	48 c1 f9 02          	sar    $0x2,%rcx
  407273:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  407278:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40727d:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  407281:	24 20                	and    $0x20,%al
  407283:	c0 e8 05             	shr    $0x5,%al
  407286:	24 01                	and    $0x1,%al
  407288:	3c 00                	cmp    $0x0,%al
  40728a:	74 2a                	je     4072b6 <__floattidf+0x2b6>
  40728c:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  407291:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  407296:	48 89 c8             	mov    %rcx,%rax
  407299:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40729e:	48 d1 f9             	sar    $1,%rcx
  4072a1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4072a6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4072ab:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  4072af:	83 c0 01             	add    $0x1,%eax
  4072b2:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  4072b6:	eb 5c                	jmp    407314 <__floattidf+0x314>
  4072b8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  4072bc:	b9 35 00 00 00       	mov    $0x35,%ecx
  4072c1:	29 c1                	sub    %eax,%ecx
  4072c3:	83 e1 7f             	and    $0x7f,%ecx
  4072c6:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  4072ca:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4072cf:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  4072d4:	40 88 cf             	mov    %cl,%dil
  4072d7:	40 88 f9             	mov    %dil,%cl
  4072da:	48 89 c2             	mov    %rax,%rdx
  4072dd:	48 d3 e2             	shl    %cl,%rdx
  4072e0:	40 88 f9             	mov    %dil,%cl
  4072e3:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  4072e7:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  4072eb:	31 c0                	xor    %eax,%eax
  4072ed:	40 f6 c7 40          	test   $0x40,%dil
  4072f1:	48 0f 45 f2          	cmovne %rdx,%rsi
  4072f5:	48 0f 45 d0          	cmovne %rax,%rdx
  4072f9:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  4072ff:	48 89 c1             	mov    %rax,%rcx
  407302:	48 0f 42 ce          	cmovb  %rsi,%rcx
  407306:	48 0f 42 c2          	cmovb  %rdx,%rax
  40730a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40730f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  407314:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  40731b:	00 00 
  40731d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  407321:	25 00 00 00 80       	and    $0x80000000,%eax
  407326:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  40732a:	c1 e1 14             	shl    $0x14,%ecx
  40732d:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  407333:	09 c8                	or     %ecx,%eax
  407335:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  407339:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  40733f:	09 c8                	or     %ecx,%eax
  407341:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  407345:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  407349:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40734d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  407353:	48 83 c4 10          	add    $0x10,%rsp
  407357:	5b                   	pop    %rbx
  407358:	c3                   	ret
  407359:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000407360 <__floattidf_unsigned>:
  407360:	53                   	push   %rbx
  407361:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  407366:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40736b:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  407370:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  407375:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40737a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40737f:	48 09 c8             	or     %rcx,%rax
  407382:	0f 94 c0             	sete   %al
  407385:	24 01                	and    $0x1,%al
  407387:	3c 00                	cmp    $0x0,%al
  407389:	74 05                	je     407390 <__floattidf_unsigned+0x30>
  40738b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40738e:	5b                   	pop    %rbx
  40738f:	c3                   	ret
  407390:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  407395:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40739a:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40739f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4073a4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  4073a9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4073ae:	48 0f bd c2          	bsr    %rdx,%rax
  4073b2:	48 83 f0 3f          	xor    $0x3f,%rax
  4073b6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  4073bb:	48 0f bd ce          	bsr    %rsi,%rcx
  4073bf:	48 83 f1 3f          	xor    $0x3f,%rcx
  4073c3:	48 83 c1 40          	add    $0x40,%rcx
  4073c7:	48 85 d2             	test   %rdx,%rdx
  4073ca:	48 0f 45 c8          	cmovne %rax,%rcx
  4073ce:	31 c0                	xor    %eax,%eax
  4073d0:	ba 80 00 00 00       	mov    $0x80,%edx
  4073d5:	48 29 ca             	sub    %rcx,%rdx
  4073d8:	48 89 c1             	mov    %rax,%rcx
  4073db:	48 19 c9             	sbb    %rcx,%rcx
  4073de:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4073e3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4073e8:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  4073ec:	ff c9                	dec    %ecx
  4073ee:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  4073f2:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4073f7:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4073fc:	ba 35 00 00 00       	mov    $0x35,%edx
  407401:	48 29 f2             	sub    %rsi,%rdx
  407404:	48 19 c8             	sbb    %rcx,%rax
  407407:	0f 92 c0             	setb   %al
  40740a:	24 01                	and    $0x1,%al
  40740c:	3c 00                	cmp    $0x0,%al
  40740e:	0f 84 c0 01 00 00    	je     4075d4 <__floattidf_unsigned+0x274>
  407414:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  407419:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40741e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407423:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  407428:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40742d:	0f 28 0d dc 1f 00 00 	movaps 0x1fdc(%rip),%xmm1        # 409410 <runtime::type_table+0x240>
  407434:	66 0f ef c1          	pxor   %xmm1,%xmm0
  407438:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  40743d:	74 17                	je     407456 <__floattidf_unsigned+0xf6>
  40743f:	eb 00                	jmp    407441 <__floattidf_unsigned+0xe1>
  407441:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  407446:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40744b:	48 83 f0 37          	xor    $0x37,%rax
  40744f:	48 09 c8             	or     %rcx,%rax
  407452:	74 26                	je     40747a <__floattidf_unsigned+0x11a>
  407454:	eb 29                	jmp    40747f <__floattidf_unsigned+0x11f>
  407456:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40745b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  407460:	48 89 d0             	mov    %rdx,%rax
  407463:	48 01 c0             	add    %rax,%rax
  407466:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40746b:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  407470:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407475:	e9 d6 00 00 00       	jmp    407550 <__floattidf_unsigned+0x1f0>
  40747a:	e9 d1 00 00 00       	jmp    407550 <__floattidf_unsigned+0x1f0>
  40747f:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  407484:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  407489:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  40748e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  407493:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  407498:	49 89 fb             	mov    %rdi,%r11
  40749b:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  40749f:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  4074a3:	44 88 db             	mov    %r11b,%bl
  4074a6:	88 d9                	mov    %bl,%cl
  4074a8:	49 89 f2             	mov    %rsi,%r10
  4074ab:	49 d3 ea             	shr    %cl,%r10
  4074ae:	88 d9                	mov    %bl,%cl
  4074b0:	49 89 d1             	mov    %rdx,%r9
  4074b3:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  4074b7:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  4074bc:	45 31 c0             	xor    %r8d,%r8d
  4074bf:	f6 c3 40             	test   $0x40,%bl
  4074c2:	4d 0f 45 ca          	cmovne %r10,%r9
  4074c6:	4d 0f 45 d0          	cmovne %r8,%r10
  4074ca:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  4074d1:	48 83 d8 00          	sbb    $0x0,%rax
  4074d5:	4c 89 c0             	mov    %r8,%rax
  4074d8:	49 0f 42 c2          	cmovb  %r10,%rax
  4074dc:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  4074e1:	4c 89 c0             	mov    %r8,%rax
  4074e4:	49 0f 42 c1          	cmovb  %r9,%rax
  4074e8:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  4074ee:	49 29 fb             	sub    %rdi,%r11
  4074f1:	4c 89 c7             	mov    %r8,%rdi
  4074f4:	48 19 cf             	sbb    %rcx,%rdi
  4074f7:	45 88 d9             	mov    %r11b,%r9b
  4074fa:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  407501:	44 88 c9             	mov    %r9b,%cl
  407504:	4c 89 d3             	mov    %r10,%rbx
  407507:	48 d3 eb             	shr    %cl,%rbx
  40750a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40750f:	41 f6 c1 40          	test   $0x40,%r9b
  407513:	49 89 d9             	mov    %rbx,%r9
  407516:	4d 0f 45 c8          	cmovne %r8,%r9
  40751a:	4c 0f 45 d3          	cmovne %rbx,%r10
  40751e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  407525:	48 83 df 00          	sbb    $0x0,%rdi
  407529:	4c 89 c7             	mov    %r8,%rdi
  40752c:	49 0f 42 fa          	cmovb  %r10,%rdi
  407530:	4d 0f 42 c1          	cmovb  %r9,%r8
  407534:	4c 21 c6             	and    %r8,%rsi
  407537:	48 21 fa             	and    %rdi,%rdx
  40753a:	48 09 f2             	or     %rsi,%rdx
  40753d:	0f 95 c2             	setne  %dl
  407540:	0f b6 d2             	movzbl %dl,%edx
  407543:	48 09 d0             	or     %rdx,%rax
  407546:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40754b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407550:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  407555:	89 c1                	mov    %eax,%ecx
  407557:	83 e1 04             	and    $0x4,%ecx
  40755a:	c1 e9 02             	shr    $0x2,%ecx
  40755d:	48 09 c8             	or     %rcx,%rax
  407560:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407565:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40756a:	48 83 c0 01          	add    $0x1,%rax
  40756e:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  407574:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407579:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40757e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  407583:	48 89 c8             	mov    %rcx,%rax
  407586:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40758b:	48 c1 e9 02          	shr    $0x2,%rcx
  40758f:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  407594:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407599:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  40759d:	24 20                	and    $0x20,%al
  40759f:	c0 e8 05             	shr    $0x5,%al
  4075a2:	24 01                	and    $0x1,%al
  4075a4:	3c 00                	cmp    $0x0,%al
  4075a6:	74 2a                	je     4075d2 <__floattidf_unsigned+0x272>
  4075a8:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4075ad:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4075b2:	48 89 c8             	mov    %rcx,%rax
  4075b5:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  4075ba:	48 d1 e9             	shr    $1,%rcx
  4075bd:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4075c2:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4075c7:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  4075cb:	83 c0 01             	add    $0x1,%eax
  4075ce:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  4075d2:	eb 6a                	jmp    40763e <__floattidf_unsigned+0x2de>
  4075d4:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  4075d9:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4075de:	31 c0                	xor    %eax,%eax
  4075e0:	bf 35 00 00 00       	mov    $0x35,%edi
  4075e5:	48 29 d7             	sub    %rdx,%rdi
  4075e8:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  4075ed:	48 19 c8             	sbb    %rcx,%rax
  4075f0:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  4075f5:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4075fa:	41 88 f8             	mov    %dil,%r8b
  4075fd:	44 88 c1             	mov    %r8b,%cl
  407600:	4c 89 ce             	mov    %r9,%rsi
  407603:	48 d3 e6             	shl    %cl,%rsi
  407606:	44 88 c1             	mov    %r8b,%cl
  407609:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  40760d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  407612:	41 f6 c0 40          	test   $0x40,%r8b
  407616:	48 0f 45 d6          	cmovne %rsi,%rdx
  40761a:	48 0f 45 f1          	cmovne %rcx,%rsi
  40761e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  407625:	48 83 d8 00          	sbb    $0x0,%rax
  407629:	48 89 c8             	mov    %rcx,%rax
  40762c:	48 0f 42 c6          	cmovb  %rsi,%rax
  407630:	48 0f 42 ca          	cmovb  %rdx,%rcx
  407634:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  407639:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40763e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  407645:	00 00 
  407647:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  40764b:	c1 e2 14             	shl    $0x14,%edx
  40764e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  407652:	25 ff ff 0f 00       	and    $0xfffff,%eax
  407657:	89 c1                	mov    %eax,%ecx
  407659:	89 d0                	mov    %edx,%eax
  40765b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  407662:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  407666:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  40766a:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40766e:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  407674:	5b                   	pop    %rbx
  407675:	c3                   	ret
  407676:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40767d:	00 00 00 

0000000000407680 <__umodti3>:
  407680:	48 83 ec 58          	sub    $0x58,%rsp
  407684:	48 89 0c 24          	mov    %rcx,(%rsp)
  407688:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40768d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407692:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  407697:	48 8b 0c 24          	mov    (%rsp),%rcx
  40769b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4076a0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4076a5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4076aa:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  4076af:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  4076b4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4076b9:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4076be:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  4076c3:	e8 c8 9a ff ff       	call   401190 <runtime::udivmod128>
  4076c8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4076cd:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4076d2:	48 83 c4 58          	add    $0x58,%rsp
  4076d6:	c3                   	ret
  4076d7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4076de:	00 00 

00000000004076e0 <__udivmodti4>:
  4076e0:	48 83 ec 58          	sub    $0x58,%rsp
  4076e4:	4c 89 04 24          	mov    %r8,(%rsp)
  4076e8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4076ed:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4076f2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4076f7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4076fc:	4c 8b 04 24          	mov    (%rsp),%r8
  407700:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  407705:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40770a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40770f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  407714:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  407719:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40771e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  407723:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  407728:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40772d:	e8 5e 9a ff ff       	call   401190 <runtime::udivmod128>
  407732:	48 83 c4 58          	add    $0x58,%rsp
  407736:	c3                   	ret
  407737:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40773e:	00 00 

0000000000407740 <__udivti3>:
  407740:	48 83 ec 48          	sub    $0x48,%rsp
  407744:	48 89 0c 24          	mov    %rcx,(%rsp)
  407748:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40774d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407752:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  407757:	48 8b 0c 24          	mov    (%rsp),%rcx
  40775b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  407760:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  407765:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40776a:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40776f:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  407774:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  407779:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40777e:	31 c0                	xor    %eax,%eax
  407780:	41 89 c0             	mov    %eax,%r8d
  407783:	e8 58 ff ff ff       	call   4076e0 <__udivmodti4>
  407788:	48 83 c4 48          	add    $0x48,%rsp
  40778c:	c3                   	ret
  40778d:	0f 1f 00             	nopl   (%rax)

0000000000407790 <runtime::default_random_generator_proc>:
  407790:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  407797:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40779c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4077a1:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4077a6:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4077ab:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4077b0:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4077b5:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4077ba:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4077bf:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4077c6:	00 
  4077c7:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4077cc:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  4077d1:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  4077d6:	48 83 f8 00          	cmp    $0x0,%rax
  4077da:	0f 94 c0             	sete   %al
  4077dd:	24 01                	and    $0x1,%al
  4077df:	3c 00                	cmp    $0x0,%al
  4077e1:	74 1a                	je     4077fd <runtime::default_random_generator_proc+0x6d>
  4077e3:	48 c7 c1 f0 ff ff ff 	mov    $0xfffffffffffffff0,%rcx
  4077ea:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  4077f1:	00 00 
  4077f3:	48 01 c8             	add    %rcx,%rax
  4077f6:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4077fb:	eb 0a                	jmp    407807 <runtime::default_random_generator_proc+0x77>
  4077fd:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  407802:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  407807:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40780c:	48 85 c0             	test   %rax,%rax
  40780f:	74 27                	je     407838 <runtime::default_random_generator_proc+0xa8>
  407811:	eb 00                	jmp    407813 <runtime::default_random_generator_proc+0x83>
  407813:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  407818:	48 83 e8 01          	sub    $0x1,%rax
  40781c:	0f 84 17 01 00 00    	je     407939 <runtime::default_random_generator_proc+0x1a9>
  407822:	eb 00                	jmp    407824 <runtime::default_random_generator_proc+0x94>
  407824:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  407829:	48 83 e8 02          	sub    $0x2,%rax
  40782d:	0f 84 40 01 00 00    	je     407973 <runtime::default_random_generator_proc+0x1e3>
  407833:	e9 6b 01 00 00       	jmp    4079a3 <runtime::default_random_generator_proc+0x213>
  407838:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40783d:	48 83 38 00          	cmpq   $0x0,(%rax)
  407841:	0f 94 c0             	sete   %al
  407844:	24 01                	and    $0x1,%al
  407846:	3c 00                	cmp    $0x0,%al
  407848:	74 21                	je     40786b <runtime::default_random_generator_proc+0xdb>
  40784a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40784f:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  407854:	0f 94 c0             	sete   %al
  407857:	24 01                	and    $0x1,%al
  407859:	3c 00                	cmp    $0x0,%al
  40785b:	74 0e                	je     40786b <runtime::default_random_generator_proc+0xdb>
  40785d:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  407862:	31 c0                	xor    %eax,%eax
  407864:	89 c6                	mov    %eax,%esi
  407866:	e8 15 02 00 00       	call   407a80 <runtime::default_random_generator_proc.init-1>
  40786b:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  407870:	48 83 e8 08          	sub    $0x8,%rax
  407874:	75 26                	jne    40789c <runtime::default_random_generator_proc+0x10c>
  407876:	eb 00                	jmp    407878 <runtime::default_random_generator_proc+0xe8>
  407878:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  40787d:	e8 2e 01 00 00       	call   4079b0 <runtime::default_random_generator_proc.read_u64-0>
  407882:	48 89 c1             	mov    %rax,%rcx
  407885:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40788a:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  40788f:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  407894:	48 89 08             	mov    %rcx,(%rax)
  407897:	e9 9b 00 00 00       	jmp    407937 <runtime::default_random_generator_proc+0x1a7>
  40789c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4078a1:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  4078a6:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  4078ad:	00 00 
  4078af:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4078b4:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  4078bb:	ff ff 
  4078bd:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4078c2:	48 83 c0 01          	add    $0x1,%rax
  4078c6:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4078cb:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  4078d0:	7d 63                	jge    407935 <runtime::default_random_generator_proc+0x1a5>
  4078d2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4078d7:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4078dc:	48 01 c8             	add    %rcx,%rax
  4078df:	48 89 04 24          	mov    %rax,(%rsp)
  4078e3:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  4078e8:	0f 94 c0             	sete   %al
  4078eb:	24 01                	and    $0x1,%al
  4078ed:	3c 00                	cmp    $0x0,%al
  4078ef:	74 14                	je     407905 <runtime::default_random_generator_proc+0x175>
  4078f1:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4078f6:	e8 b5 00 00 00       	call   4079b0 <runtime::default_random_generator_proc.read_u64-0>
  4078fb:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  407900:	c6 44 24 57 08       	movb   $0x8,0x57(%rsp)
  407905:	48 8b 04 24          	mov    (%rsp),%rax
  407909:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40790e:	88 08                	mov    %cl,(%rax)
  407910:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  407915:	48 c1 e9 08          	shr    $0x8,%rcx
  407919:	b2 01                	mov    $0x1,%dl
  40791b:	31 c0                	xor    %eax,%eax
  40791d:	f6 c2 01             	test   $0x1,%dl
  407920:	48 0f 45 c1          	cmovne %rcx,%rax
  407924:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  407929:	8a 44 24 57          	mov    0x57(%rsp),%al
  40792d:	2c 01                	sub    $0x1,%al
  40792f:	88 44 24 57          	mov    %al,0x57(%rsp)
  407933:	eb 88                	jmp    4078bd <runtime::default_random_generator_proc+0x12d>
  407935:	eb 00                	jmp    407937 <runtime::default_random_generator_proc+0x1a7>
  407937:	eb 6a                	jmp    4079a3 <runtime::default_random_generator_proc+0x213>
  407939:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40793e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  407943:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40794a:	00 00 
  40794c:	b8 08 00 00 00       	mov    $0x8,%eax
  407951:	48 39 d0             	cmp    %rdx,%rax
  407954:	48 0f 4c d0          	cmovl  %rax,%rdx
  407958:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40795d:	e8 2e e2 ff ff       	call   405b90 <runtime::mem_copy_non_overlapping>
  407962:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  407967:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40796c:	e8 0f 01 00 00       	call   407a80 <runtime::default_random_generator_proc.init-1>
  407971:	eb 30                	jmp    4079a3 <runtime::default_random_generator_proc+0x213>
  407973:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  407978:	48 83 f8 04          	cmp    $0x4,%rax
  40797c:	0f 95 c0             	setne  %al
  40797f:	24 01                	and    $0x1,%al
  407981:	3c 00                	cmp    $0x0,%al
  407983:	74 08                	je     40798d <runtime::default_random_generator_proc+0x1fd>
  407985:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40798c:	c3                   	ret
  40798d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407992:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  407997:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40799c:	8b 08                	mov    (%rax),%ecx
  40799e:	83 c9 0a             	or     $0xa,%ecx
  4079a1:	89 08                	mov    %ecx,(%rax)
  4079a3:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4079aa:	c3                   	ret
  4079ab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004079b0 <runtime::default_random_generator_proc.read_u64-0>:
  4079b0:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  4079b5:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4079ba:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4079bf:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4079c4:	48 8b 00             	mov    (%rax),%rax
  4079c7:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4079cc:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4079d1:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  4079d8:	f4 51 58 
  4079db:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  4079e1:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4079e6:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  4079ea:	48 83 ca 01          	or     $0x1,%rdx
  4079ee:	48 01 d1             	add    %rdx,%rcx
  4079f1:	48 89 08             	mov    %rcx,(%rax)
  4079f4:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4079f9:	48 c1 e9 3b          	shr    $0x3b,%rcx
  4079fd:	b2 01                	mov    $0x1,%dl
  4079ff:	31 c0                	xor    %eax,%eax
  407a01:	f6 c2 01             	test   $0x1,%dl
  407a04:	48 0f 45 c1          	cmovne %rcx,%rax
  407a08:	48 83 c0 05          	add    $0x5,%rax
  407a0c:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  407a11:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  407a18:	75 f1 ae 
  407a1b:	48 0f af c1          	imul   %rcx,%rax
  407a1f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  407a24:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  407a29:	48 c1 e9 3b          	shr    $0x3b,%rcx
  407a2d:	b2 01                	mov    $0x1,%dl
  407a2f:	31 c0                	xor    %eax,%eax
  407a31:	f6 c2 01             	test   $0x1,%dl
  407a34:	48 0f 45 c1          	cmovne %rcx,%rax
  407a38:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  407a3d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  407a42:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  407a47:	48 89 d1             	mov    %rdx,%rcx
  407a4a:	48 d3 e8             	shr    %cl,%rax
  407a4d:	48 89 c1             	mov    %rax,%rcx
  407a50:	31 c0                	xor    %eax,%eax
  407a52:	48 83 fa 40          	cmp    $0x40,%rdx
  407a56:	48 0f 42 c1          	cmovb  %rcx,%rax
  407a5a:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  407a5f:	31 c9                	xor    %ecx,%ecx
  407a61:	89 ce                	mov    %ecx,%esi
  407a63:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  407a68:	48 83 e6 3f          	and    $0x3f,%rsi
  407a6c:	48 89 f1             	mov    %rsi,%rcx
  407a6f:	48 d3 e2             	shl    %cl,%rdx
  407a72:	31 c9                	xor    %ecx,%ecx
  407a74:	48 83 fe 40          	cmp    $0x40,%rsi
  407a78:	48 0f 42 ca          	cmovb  %rdx,%rcx
  407a7c:	48 09 c8             	or     %rcx,%rax
  407a7f:	c3                   	ret

0000000000407a80 <runtime::default_random_generator_proc.init-1>:
  407a80:	48 83 ec 28          	sub    $0x28,%rsp
  407a84:	48 89 3c 24          	mov    %rdi,(%rsp)
  407a88:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  407a8d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  407a92:	48 8b 0c 24          	mov    (%rsp),%rcx
  407a96:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  407a9b:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  407aa0:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  407aa5:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  407aab:	0f 94 c0             	sete   %al
  407aae:	24 01                	and    $0x1,%al
  407ab0:	3c 00                	cmp    $0x0,%al
  407ab2:	74 0e                	je     407ac2 <runtime::default_random_generator_proc.init-1+0x42>
  407ab4:	0f 31                	rdtsc
  407ab6:	48 c1 e2 20          	shl    $0x20,%rdx
  407aba:	48 09 d0             	or     %rdx,%rax
  407abd:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  407ac2:	48 8b 3c 24          	mov    (%rsp),%rdi
  407ac6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407acb:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  407ad2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407ad7:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  407adc:	48 d1 e2             	shl    $1,%rdx
  407adf:	40 b6 01             	mov    $0x1,%sil
  407ae2:	31 c9                	xor    %ecx,%ecx
  407ae4:	40 f6 c6 01          	test   $0x1,%sil
  407ae8:	48 0f 45 ca          	cmovne %rdx,%rcx
  407aec:	48 83 c9 01          	or     $0x1,%rcx
  407af0:	48 89 48 08          	mov    %rcx,0x8(%rax)
  407af4:	e8 b7 fe ff ff       	call   4079b0 <runtime::default_random_generator_proc.read_u64-0>
  407af9:	48 8b 3c 24          	mov    (%rsp),%rdi
  407afd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407b02:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  407b07:	48 03 08             	add    (%rax),%rcx
  407b0a:	48 89 08             	mov    %rcx,(%rax)
  407b0d:	e8 9e fe ff ff       	call   4079b0 <runtime::default_random_generator_proc.read_u64-0>
  407b12:	48 83 c4 28          	add    $0x28,%rsp
  407b16:	c3                   	ret
  407b17:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  407b1e:	00 00 

0000000000407b20 <runtime::copy_slice_raw>:
  407b20:	48 83 ec 58          	sub    $0x58,%rsp
  407b24:	48 89 3c 24          	mov    %rdi,(%rsp)
  407b28:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  407b2d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  407b32:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  407b37:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  407b3c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  407b41:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  407b46:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  407b4b:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  407b50:	48 8b 3c 24          	mov    (%rsp),%rdi
  407b54:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  407b59:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  407b5e:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  407b63:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  407b68:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  407b6d:	48 39 c1             	cmp    %rax,%rcx
  407b70:	48 0f 4c c1          	cmovl  %rcx,%rax
  407b74:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  407b79:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
  407b7f:	0f 9f c0             	setg   %al
  407b82:	24 01                	and    $0x1,%al
  407b84:	3c 00                	cmp    $0x0,%al
  407b86:	74 19                	je     407ba1 <runtime::copy_slice_raw+0x81>
  407b88:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  407b8d:	48 8b 3c 24          	mov    (%rsp),%rdi
  407b91:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  407b96:	48 0f af 54 24 28    	imul   0x28(%rsp),%rdx
  407b9c:	e8 ef 94 ff ff       	call   401090 <memmove@plt>
  407ba1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  407ba6:	48 83 c4 58          	add    $0x58,%rsp
  407baa:	c3                   	ret
  407bab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000407bb0 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  407bb0:	48 83 ec 48          	sub    $0x48,%rsp
  407bb4:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  407bb9:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  407bbe:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  407bc3:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  407bc8:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  407bcd:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  407bd2:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  407bd7:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  407bdc:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  407be1:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  407be6:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  407beb:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  407bf0:	41 b8 01 00 00 00    	mov    $0x1,%r8d
  407bf6:	e8 25 ff ff ff       	call   407b20 <runtime::copy_slice_raw>
  407bfb:	48 83 c4 48          	add    $0x48,%rsp
  407bff:	c3                   	ret

0000000000407c00 <runtime::_make_aligned_type_erased>:
  407c00:	48 81 ec f8 00 00 00 	sub    $0xf8,%rsp
  407c07:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  407c0c:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  407c11:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  407c16:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  407c1b:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  407c20:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  407c25:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  407c2c:	00 
  407c2d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  407c32:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407c39:	00 
  407c3a:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  407c3f:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  407c44:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  407c49:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  407c4e:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  407c53:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  407c58:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  407c5d:	4c 8b 4c 24 38       	mov    0x38(%rsp),%r9
  407c62:	4c 89 8c 24 f0 00 00 	mov    %r9,0xf0(%rsp)
  407c69:	00 
  407c6a:	4c 89 84 24 e8 00 00 	mov    %r8,0xe8(%rsp)
  407c71:	00 
  407c72:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  407c79:	00 
  407c7a:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  407c81:	00 
  407c82:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  407c89:	00 
  407c8a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  407c91:	00 
  407c92:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  407c99:	00 
  407c9a:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  407c9f:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  407ca6:	00 
  407ca7:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  407cac:	e8 af c5 ff ff       	call   404260 <runtime::make_slice_error_loc>
  407cb1:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  407cb6:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  407cbb:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  407cc0:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  407cc5:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  407cca:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  407ccf:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  407cd4:	48 0f af fa          	imul   %rdx,%rdi
  407cd8:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  407cdf:	00 
  407ce0:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  407ce7:	00 
  407ce8:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  407cef:	00 
  407cf0:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  407cf7:	00 
  407cf8:	0f 57 c0             	xorps  %xmm0,%xmm0
  407cfb:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  407d02:	00 
  407d03:	48 89 e0             	mov    %rsp,%rax
  407d06:	4c 89 08             	mov    %r9,(%rax)
  407d09:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  407d10:	00 
  407d11:	e8 0a df ff ff       	call   405c20 <runtime::mem_alloc_bytes>
  407d16:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  407d1d:	00 
  407d1e:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  407d25:	00 
  407d26:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  407d2d:	00 
  407d2e:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  407d33:	88 44 24 77          	mov    %al,0x77(%rsp)
  407d37:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  407d3d:	0f 94 c0             	sete   %al
  407d40:	24 01                	and    $0x1,%al
  407d42:	3c 00                	cmp    $0x0,%al
  407d44:	74 1e                	je     407d64 <runtime::_make_aligned_type_erased+0x164>
  407d46:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  407d4b:	48 83 f8 00          	cmp    $0x0,%rax
  407d4f:	0f 95 c0             	setne  %al
  407d52:	24 01                	and    $0x1,%al
  407d54:	3c 00                	cmp    $0x0,%al
  407d56:	74 0c                	je     407d64 <runtime::_make_aligned_type_erased+0x164>
  407d58:	8a 44 24 77          	mov    0x77(%rsp),%al
  407d5c:	48 81 c4 f8 00 00 00 	add    $0xf8,%rsp
  407d63:	c3                   	ret
  407d64:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  407d69:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  407d6e:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  407d73:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  407d78:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  407d7d:	48 89 32             	mov    %rsi,(%rdx)
  407d80:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  407d85:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  407d8a:	48 89 48 08          	mov    %rcx,0x8(%rax)
  407d8e:	8a 44 24 77          	mov    0x77(%rsp),%al
  407d92:	48 81 c4 f8 00 00 00 	add    $0xf8,%rsp
  407d99:	c3                   	ret
  407d9a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000407da0 <runtime::make_slice:proc(T:$[]string,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]string,err:runtime::Allocator_Error)>:
  407da0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  407da7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  407dac:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  407db1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  407db6:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  407dbb:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  407dc0:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  407dc5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  407dca:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  407dcf:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  407dd4:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  407dd9:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  407dde:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  407de5:	00 
  407de6:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  407deb:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  407df0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  407df5:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  407dfa:	0f 57 c0             	xorps  %xmm0,%xmm0
  407dfd:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  407e02:	c6 44 24 5f 00       	movb   $0x0,0x5f(%rsp)
  407e07:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  407e0c:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  407e11:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  407e16:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  407e1b:	48 89 e0             	mov    %rsp,%rax
  407e1e:	48 89 70 08          	mov    %rsi,0x8(%rax)
  407e22:	48 89 08             	mov    %rcx,(%rax)
  407e25:	48 8d 7c 24 60       	lea    0x60(%rsp),%rdi
  407e2a:	be 10 00 00 00       	mov    $0x10,%esi
  407e2f:	b9 08 00 00 00       	mov    $0x8,%ecx
  407e34:	e8 c7 fd ff ff       	call   407c00 <runtime::_make_aligned_type_erased>
  407e39:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  407e3e:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  407e42:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  407e47:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  407e4c:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  407e50:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  407e55:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  407e5a:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  407e5e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  407e62:	48 89 11             	mov    %rdx,(%rcx)
  407e65:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  407e6c:	c3                   	ret
  407e6d:	0f 1f 00             	nopl   (%rax)

0000000000407e70 <runtime::delete_slice:proc(array:[]string,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>:
  407e70:	48 83 ec 68          	sub    $0x68,%rsp
  407e74:	4c 89 0c 24          	mov    %r9,(%rsp)
  407e78:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  407e7d:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  407e82:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  407e87:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  407e8c:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  407e91:	4c 8b 0c 24          	mov    (%rsp),%r9
  407e95:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  407e9a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  407e9f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  407ea4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  407ea9:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  407eae:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  407eb3:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  407eb8:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  407ebd:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  407ec2:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  407ec7:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  407ecc:	48 c1 e6 04          	shl    $0x4,%rsi
  407ed0:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  407ed5:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  407eda:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  407edf:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  407ee4:	e8 57 e0 ff ff       	call   405f40 <runtime::mem_free_with_size>
  407ee9:	48 83 c4 68          	add    $0x68,%rsp
  407eed:	c3                   	ret
  407eee:	66 90                	xchg   %ax,%ax

0000000000407ef0 <runtime::assert>:
  407ef0:	48 83 ec 48          	sub    $0x48,%rsp
  407ef4:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  407ef9:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  407efe:	40 88 f8             	mov    %dil,%al
  407f01:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  407f05:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  407f0a:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  407f0f:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  407f13:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  407f18:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  407f1d:	88 44 24 47          	mov    %al,0x47(%rsp)
  407f21:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  407f26:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  407f2b:	3c 00                	cmp    $0x0,%al
  407f2d:	75 19                	jne    407f48 <runtime::assert+0x58>
  407f2f:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  407f34:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  407f39:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  407f3e:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  407f43:	e8 08 00 00 00       	call   407f50 <runtime::assert.internal-0>
  407f48:	48 83 c4 48          	add    $0x48,%rsp
  407f4c:	c3                   	ret
  407f4d:	0f 1f 00             	nopl   (%rax)

0000000000407f50 <runtime::assert.internal-0>:
  407f50:	48 83 ec 38          	sub    $0x38,%rsp
  407f54:	48 89 0c 24          	mov    %rcx,(%rsp)
  407f58:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  407f5d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407f62:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  407f67:	48 8b 04 24          	mov    (%rsp),%rax
  407f6b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  407f70:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  407f75:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  407f7a:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  407f7f:	48 8b 40 20          	mov    0x20(%rax),%rax
  407f83:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407f88:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  407f8e:	0f 94 c0             	sete   %al
  407f91:	24 01                	and    $0x1,%al
  407f93:	3c 00                	cmp    $0x0,%al
  407f95:	74 0c                	je     407fa3 <runtime::assert.internal-0+0x53>
  407f97:	48 c7 c0 b0 38 40 00 	mov    $0x4038b0,%rax
  407f9e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407fa3:	4c 8b 0c 24          	mov    (%rsp),%r9
  407fa7:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  407fac:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  407fb1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  407fb6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407fbb:	bf e1 94 40 00       	mov    $0x4094e1,%edi
  407fc0:	be 11 00 00 00       	mov    $0x11,%esi
  407fc5:	ff d0                	call   *%rax
  407fc7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  407fce:	00 00 

0000000000407fd0 <runtime::print_string>:
  407fd0:	48 83 ec 58          	sub    $0x58,%rsp
  407fd4:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  407fd9:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  407fde:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  407fe3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  407fe8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  407fed:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  407ff2:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  407ff9:	00 00 
  407ffb:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  408000:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  408005:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40800a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40800f:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  408016:	00 00 
  408018:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  40801d:	e8 6e d2 ff ff       	call   405290 <runtime::stderr_write>
  408022:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  408027:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40802c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  408031:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  408036:	48 83 c4 58          	add    $0x58,%rsp
  40803a:	c3                   	ret
  40803b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000408040 <runtime::print_byte>:
  408040:	48 83 ec 68          	sub    $0x68,%rsp
  408044:	40 88 f8             	mov    %dil,%al
  408047:	88 44 24 07          	mov    %al,0x7(%rsp)
  40804b:	8a 54 24 07          	mov    0x7(%rsp),%dl
  40804f:	88 54 24 67          	mov    %dl,0x67(%rsp)
  408053:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  40805a:	00 00 
  40805c:	0f 57 c0             	xorps  %xmm0,%xmm0
  40805f:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  408064:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  408069:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  40806e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  408073:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  40807a:	00 00 
  40807c:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  408081:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  408086:	88 11                	mov    %dl,(%rcx)
  408088:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40808d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  408092:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  408097:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40809c:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  4080a3:	00 00 
  4080a5:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  4080aa:	e8 e1 d1 ff ff       	call   405290 <runtime::stderr_write>
  4080af:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4080b4:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4080b9:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4080be:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4080c3:	48 83 c4 68          	add    $0x68,%rsp
  4080c7:	c3                   	ret
  4080c8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4080cf:	00 

00000000004080d0 <runtime::print_u64>:
  4080d0:	48 81 ec 48 01 00 00 	sub    $0x148,%rsp
  4080d7:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  4080dc:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4080e1:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  4080e8:	00 
  4080e9:	48 8d bc 24 bf 00 00 	lea    0xbf(%rsp),%rdi
  4080f0:	00 
  4080f1:	31 f6                	xor    %esi,%esi
  4080f3:	ba 81 00 00 00       	mov    $0x81,%edx
  4080f8:	e8 43 8f ff ff       	call   401040 <memset@plt>
  4080fd:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  408102:	48 c7 84 24 b0 00 00 	movq   $0x81,0xb0(%rsp)
  408109:	00 81 00 00 00 
  40810e:	48 c7 84 24 a8 00 00 	movq   $0xa,0xa8(%rsp)
  408115:	00 0a 00 00 00 
  40811a:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  408121:	00 
  408122:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  408129:	00 
  40812a:	48 3b 84 24 a8 00 00 	cmp    0xa8(%rsp),%rax
  408131:	00 
  408132:	0f 93 c0             	setae  %al
  408135:	24 01                	and    $0x1,%al
  408137:	3c 00                	cmp    $0x0,%al
  408139:	0f 84 ce 00 00 00    	je     40820d <runtime::print_u64+0x13d>
  40813f:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  408146:	00 
  408147:	48 83 e8 01          	sub    $0x1,%rax
  40814b:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  408152:	00 
  408153:	48 8d 84 24 bf 00 00 	lea    0xbf(%rsp),%rax
  40815a:	00 
  40815b:	48 03 84 24 b0 00 00 	add    0xb0(%rsp),%rax
  408162:	00 
  408163:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  408168:	48 c7 c0 48 c0 40 00 	mov    $0x40c048,%rax
  40816f:	48 8b 00             	mov    (%rax),%rax
  408172:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  408177:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  40817e:	00 
  40817f:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  408184:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40818b:	00 
  40818c:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  408191:	48 83 f8 00          	cmp    $0x0,%rax
  408195:	74 16                	je     4081ad <runtime::print_u64+0xdd>
  408197:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40819c:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4081a1:	31 d2                	xor    %edx,%edx
  4081a3:	48 f7 f1             	div    %rcx
  4081a6:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4081ab:	eb 02                	jmp    4081af <runtime::print_u64+0xdf>
  4081ad:	0f 0b                	ud2
  4081af:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4081b4:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4081b9:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4081be:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  4081c1:	88 08                	mov    %cl,(%rax)
  4081c3:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4081ca:	00 
  4081cb:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4081d0:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4081d7:	00 
  4081d8:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4081dd:	48 83 f8 00          	cmp    $0x0,%rax
  4081e1:	74 16                	je     4081f9 <runtime::print_u64+0x129>
  4081e3:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4081e8:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4081ed:	31 d2                	xor    %edx,%edx
  4081ef:	48 f7 f1             	div    %rcx
  4081f2:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4081f7:	eb 02                	jmp    4081fb <runtime::print_u64+0x12b>
  4081f9:	0f 0b                	ud2
  4081fb:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  408200:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  408207:	00 
  408208:	e9 15 ff ff ff       	jmp    408122 <runtime::print_u64+0x52>
  40820d:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  408214:	00 
  408215:	48 83 e8 01          	sub    $0x1,%rax
  408219:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  408220:	00 
  408221:	48 8d 84 24 bf 00 00 	lea    0xbf(%rsp),%rax
  408228:	00 
  408229:	48 03 84 24 b0 00 00 	add    0xb0(%rsp),%rax
  408230:	00 
  408231:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  408236:	48 c7 c0 48 c0 40 00 	mov    $0x40c048,%rax
  40823d:	48 8b 00             	mov    (%rax),%rax
  408240:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  408245:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  40824c:	00 
  40824d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  408252:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  408259:	00 
  40825a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40825f:	48 83 f8 00          	cmp    $0x0,%rax
  408263:	74 16                	je     40827b <runtime::print_u64+0x1ab>
  408265:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40826a:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40826f:	31 d2                	xor    %edx,%edx
  408271:	48 f7 f1             	div    %rcx
  408274:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  408279:	eb 02                	jmp    40827d <runtime::print_u64+0x1ad>
  40827b:	0f 0b                	ud2
  40827d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  408282:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  408287:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40828c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  40828f:	88 08                	mov    %cl,(%rax)
  408291:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  408298:	00 
  408299:	48 8d 8c 14 bf 00 00 	lea    0xbf(%rsp,%rdx,1),%rcx
  4082a0:	00 
  4082a1:	b8 81 00 00 00       	mov    $0x81,%eax
  4082a6:	48 29 d0             	sub    %rdx,%rax
  4082a9:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  4082b0:	00 
  4082b1:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  4082b8:	00 
  4082b9:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  4082c0:	00 
  4082c1:	48 8b b4 24 98 00 00 	mov    0x98(%rsp),%rsi
  4082c8:	00 
  4082c9:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  4082d0:	00 00 00 00 00 
  4082d5:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  4082dc:	00 
  4082dd:	e8 ae cf ff ff       	call   405290 <runtime::stderr_write>
  4082e2:	48 81 c4 48 01 00 00 	add    $0x148,%rsp
  4082e9:	c3                   	ret
  4082ea:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004082f0 <runtime::print_i64>:
  4082f0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4082f7:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4082fc:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  408301:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  408308:	00 
  408309:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  408310:	00 
  408311:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  408318:	00 00 
  40831a:	0f 9c c0             	setl   %al
  40831d:	24 01                	and    $0x1,%al
  40831f:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  408326:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40832d:	00 
  40832e:	31 c9                	xor    %ecx,%ecx
  408330:	48 29 c1             	sub    %rax,%rcx
  408333:	48 83 f8 00          	cmp    $0x0,%rax
  408337:	48 0f 4c c1          	cmovl  %rcx,%rax
  40833b:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  408342:	00 
  408343:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  408348:	31 f6                	xor    %esi,%esi
  40834a:	ba 81 00 00 00       	mov    $0x81,%edx
  40834f:	e8 ec 8c ff ff       	call   401040 <memset@plt>
  408354:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  40835b:	00 00 
  40835d:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  408364:	00 0a 
  408366:	0f 9d c0             	setge  %al
  408369:	24 01                	and    $0x1,%al
  40836b:	3c 00                	cmp    $0x0,%al
  40836d:	74 5c                	je     4083cb <runtime::print_i64+0xdb>
  40836f:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  408374:	48 83 e8 01          	sub    $0x1,%rax
  408378:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40837d:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  408382:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  408387:	48 c7 c0 48 c0 40 00 	mov    $0x40c048,%rax
  40838e:	48 8b 08             	mov    (%rax),%rcx
  408391:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  408398:	00 
  408399:	be 0a 00 00 00       	mov    $0xa,%esi
  40839e:	48 99                	cqto
  4083a0:	48 f7 fe             	idiv   %rsi
  4083a3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4083a8:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  4083ab:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  4083af:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4083b6:	00 
  4083b7:	b9 0a 00 00 00       	mov    $0xa,%ecx
  4083bc:	48 99                	cqto
  4083be:	48 f7 f9             	idiv   %rcx
  4083c1:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4083c8:	00 
  4083c9:	eb 92                	jmp    40835d <runtime::print_i64+0x6d>
  4083cb:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4083d0:	48 83 e8 01          	sub    $0x1,%rax
  4083d4:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4083d9:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4083de:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4083e3:	48 c7 c0 48 c0 40 00 	mov    $0x40c048,%rax
  4083ea:	48 8b 08             	mov    (%rax),%rcx
  4083ed:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4083f4:	00 
  4083f5:	be 0a 00 00 00       	mov    $0xa,%esi
  4083fa:	48 99                	cqto
  4083fc:	48 f7 fe             	idiv   %rsi
  4083ff:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  408404:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  408407:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  40840b:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  408412:	00 
  408413:	74 18                	je     40842d <runtime::print_i64+0x13d>
  408415:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40841a:	48 83 e8 01          	sub    $0x1,%rax
  40841e:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  408423:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  408428:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  40842d:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  408432:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  408437:	b8 81 00 00 00       	mov    $0x81,%eax
  40843c:	48 29 d0             	sub    %rdx,%rax
  40843f:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  408444:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  408449:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40844e:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  408453:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40845a:	00 00 
  40845c:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  408461:	e8 2a ce ff ff       	call   405290 <runtime::stderr_write>
  408466:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40846d:	c3                   	ret
  40846e:	66 90                	xchg   %ax,%ax

0000000000408470 <runtime::print_caller_location>:
  408470:	50                   	push   %rax
  408471:	48 89 3c 24          	mov    %rdi,(%rsp)
  408475:	eb 00                	jmp    408477 <runtime::print_caller_location+0x7>
  408477:	48 8b 04 24          	mov    (%rsp),%rax
  40847b:	48 8b 38             	mov    (%rax),%rdi
  40847e:	48 8b 70 08          	mov    0x8(%rax),%rsi
  408482:	e8 49 fb ff ff       	call   407fd0 <runtime::print_string>
  408487:	bf 28 00 00 00       	mov    $0x28,%edi
  40848c:	e8 af fb ff ff       	call   408040 <runtime::print_byte>
  408491:	48 8b 04 24          	mov    (%rsp),%rax
  408495:	48 63 78 10          	movslq 0x10(%rax),%rdi
  408499:	e8 32 fc ff ff       	call   4080d0 <runtime::print_u64>
  40849e:	48 8b 04 24          	mov    (%rsp),%rax
  4084a2:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  4084a6:	0f 95 c0             	setne  %al
  4084a9:	24 01                	and    $0x1,%al
  4084ab:	3c 00                	cmp    $0x0,%al
  4084ad:	74 17                	je     4084c6 <runtime::print_caller_location+0x56>
  4084af:	bf 3a 00 00 00       	mov    $0x3a,%edi
  4084b4:	e8 87 fb ff ff       	call   408040 <runtime::print_byte>
  4084b9:	48 8b 04 24          	mov    (%rsp),%rax
  4084bd:	48 63 78 14          	movslq 0x14(%rax),%rdi
  4084c1:	e8 0a fc ff ff       	call   4080d0 <runtime::print_u64>
  4084c6:	bf 29 00 00 00       	mov    $0x29,%edi
  4084cb:	e8 70 fb ff ff       	call   408040 <runtime::print_byte>
  4084d0:	58                   	pop    %rax
  4084d1:	c3                   	ret

Disassembly of section .fini:

00000000004084d4 <_fini>:
  4084d4:	f3 0f 1e fa          	endbr64
  4084d8:	48 83 ec 08          	sub    $0x8,%rsp
  4084dc:	48 83 c4 08          	add    $0x8,%rsp
  4084e0:	c3                   	ret
