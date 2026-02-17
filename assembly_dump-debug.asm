
/home/khalid/Documents/GitHub/Journey_ECS/main-debug.bin:     file format elf64-x86-64


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
  4010b8:	48 c7 c7 e0 2a 40 00 	mov    $0x402ae0,%rdi
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

00000000004010e0 <deregister_tm_clones>:
  4010e0:	b8 58 a0 40 00       	mov    $0x40a058,%eax
  4010e5:	48 3d 58 a0 40 00    	cmp    $0x40a058,%rax
  4010eb:	74 13                	je     401100 <deregister_tm_clones+0x20>
  4010ed:	48 8b 05 d4 8e 00 00 	mov    0x8ed4(%rip),%rax        # 409fc8 <_ITM_deregisterTMCloneTable@Base>
  4010f4:	48 85 c0             	test   %rax,%rax
  4010f7:	74 07                	je     401100 <deregister_tm_clones+0x20>
  4010f9:	bf 58 a0 40 00       	mov    $0x40a058,%edi
  4010fe:	ff e0                	jmp    *%rax
  401100:	c3                   	ret
  401101:	0f 1f 40 00          	nopl   0x0(%rax)
  401105:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40110c:	00 00 00 00 

0000000000401110 <register_tm_clones>:
  401110:	be 58 a0 40 00       	mov    $0x40a058,%esi
  401115:	48 81 ee 58 a0 40 00 	sub    $0x40a058,%rsi
  40111c:	48 89 f0             	mov    %rsi,%rax
  40111f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401123:	48 c1 f8 03          	sar    $0x3,%rax
  401127:	48 01 c6             	add    %rax,%rsi
  40112a:	48 d1 fe             	sar    $1,%rsi
  40112d:	74 19                	je     401148 <register_tm_clones+0x38>
  40112f:	48 8b 05 a2 8e 00 00 	mov    0x8ea2(%rip),%rax        # 409fd8 <_ITM_registerTMCloneTable@Base>
  401136:	48 85 c0             	test   %rax,%rax
  401139:	74 0d                	je     401148 <register_tm_clones+0x38>
  40113b:	bf 58 a0 40 00       	mov    $0x40a058,%edi
  401140:	ff e0                	jmp    *%rax
  401142:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401148:	c3                   	ret
  401149:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401150 <__do_global_dtors_aux>:
  401150:	f3 0f 1e fa          	endbr64
  401154:	80 3d fd 8e 00 00 00 	cmpb   $0x0,0x8efd(%rip)        # 40a058 <__TMC_END__>
  40115b:	75 13                	jne    401170 <__do_global_dtors_aux+0x20>
  40115d:	55                   	push   %rbp
  40115e:	48 89 e5             	mov    %rsp,%rbp
  401161:	e8 7a ff ff ff       	call   4010e0 <deregister_tm_clones>
  401166:	c6 05 eb 8e 00 00 01 	movb   $0x1,0x8eeb(%rip)        # 40a058 <__TMC_END__>
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

0000000000401190 <__$startup_runtime>:
  401190:	eb 00                	jmp    401192 <__$startup_runtime+0x2>
  401192:	c3                   	ret
  401193:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40119a:	84 00 00 00 00 00 

00000000004011a0 <__$cleanup_runtime>:
  4011a0:	50                   	push   %rax
  4011a1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4011a5:	eb 00                	jmp    4011a7 <__$cleanup_runtime+0x7>
  4011a7:	48 8b 3c 24          	mov    (%rsp),%rdi
  4011ab:	e8 30 01 00 00       	call   4012e0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  4011b0:	58                   	pop    %rax
  4011b1:	c3                   	ret
  4011b2:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4011b9:	00 00 00 
  4011bc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004011c0 <journey::main>:
  4011c0:	55                   	push   %rbp
  4011c1:	48 89 e5             	mov    %rsp,%rbp
  4011c4:	48 83 e4 c0          	and    $0xffffffffffffffc0,%rsp
  4011c8:	48 81 ec c0 01 00 00 	sub    $0x1c0,%rsp
  4011cf:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4011d4:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4011d9:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  4011de:	e8 05 00 00 00       	call   4011e8 <journey::init_world:proc(world:^journey::World($PAGE_COUNT=5, $THREAD_COUNT=4),unique_data_capacity:$$1)>
  4011e3:	48 89 ec             	mov    %rbp,%rsp
  4011e6:	5d                   	pop    %rbp
  4011e7:	c3                   	ret

00000000004011e8 <journey::init_world:proc(world:^journey::World($PAGE_COUNT=5, $THREAD_COUNT=4),unique_data_capacity:$$1)>:
  4011e8:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  4011ed:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4011f2:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4011f7:	31 c0                	xor    %eax,%eax
  4011f9:	41 89 c1             	mov    %eax,%r9d
  4011fc:	b8 09 00 00 00       	mov    $0x9,%eax
  401201:	be 00 60 00 00       	mov    $0x6000,%esi
  401206:	ba 03 00 00 00       	mov    $0x3,%edx
  40120b:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401211:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401218:	4c 89 cf             	mov    %r9,%rdi
  40121b:	0f 05                	syscall
  40121d:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401222:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401227:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40122c:	48 89 08             	mov    %rcx,(%rax)
  40122f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401234:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401239:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  401240:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401244:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401249:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40124e:	b8 ba 00 00 00       	mov    $0xba,%eax
  401253:	0f 05                	syscall
  401255:	48 89 c1             	mov    %rax,%rcx
  401258:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40125d:	48 89 48 10          	mov    %rcx,0x10(%rax)
  401261:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401266:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40126b:	31 c0                	xor    %eax,%eax
  40126d:	41 89 c1             	mov    %eax,%r9d
  401270:	b8 09 00 00 00       	mov    $0x9,%eax
  401275:	be 00 10 00 00       	mov    $0x1000,%esi
  40127a:	ba 03 00 00 00       	mov    $0x3,%edx
  40127f:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401285:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  40128c:	4c 89 cf             	mov    %r9,%rdi
  40128f:	0f 05                	syscall
  401291:	48 89 c1             	mov    %rax,%rcx
  401294:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401299:	48 89 48 18          	mov    %rcx,0x18(%rax)
  40129d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4012a2:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4012a7:	31 c0                	xor    %eax,%eax
  4012a9:	41 89 c1             	mov    %eax,%r9d
  4012ac:	b8 09 00 00 00       	mov    $0x9,%eax
  4012b1:	be 00 00 02 00       	mov    $0x20000,%esi
  4012b6:	ba 03 00 00 00       	mov    $0x3,%edx
  4012bb:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4012c1:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4012c8:	4c 89 cf             	mov    %r9,%rdi
  4012cb:	0f 05                	syscall
  4012cd:	48 89 c1             	mov    %rax,%rcx
  4012d0:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4012d5:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4012d9:	c3                   	ret
  4012da:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004012e0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  4012e0:	50                   	push   %rax
  4012e1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4012e5:	eb 00                	jmp    4012e7 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x7>
  4012e7:	48 8b 34 24          	mov    (%rsp),%rsi
  4012eb:	48 c7 c0 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rax
  4012f2:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  4012f9:	00 00 
  4012fb:	48 01 c7             	add    %rax,%rdi
  4012fe:	e8 8d 11 00 00       	call   402490 <runtime::default_temp_allocator_destroy>
  401303:	58                   	pop    %rax
  401304:	c3                   	ret
  401305:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40130c:	00 00 00 00 

0000000000401310 <runtime::bounds_trap>:
  401310:	eb 00                	jmp    401312 <runtime::bounds_trap+0x2>
  401312:	0f 0b                	ud2
  401314:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40131b:	00 00 00 00 00 

0000000000401320 <runtime::heap_allocator>:
  401320:	48 c7 c0 c0 1d 40 00 	mov    $0x401dc0,%rax
  401327:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40132c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401333:	00 00 
  401335:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40133a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40133f:	c3                   	ret

0000000000401340 <runtime::udivmod128>:
  401340:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  401347:	4c 89 44 24 a8       	mov    %r8,-0x58(%rsp)
  40134c:	48 89 4c 24 b0       	mov    %rcx,-0x50(%rsp)
  401351:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401356:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40135b:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  401360:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401365:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40136a:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40136f:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  401374:	48 8b 7c 24 a8       	mov    -0x58(%rsp),%rdi
  401379:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  401380:	00 
  401381:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  401388:	00 
  401389:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  401390:	00 
  401391:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  401398:	00 
  401399:	48 89 bc 24 88 00 00 	mov    %rdi,0x88(%rsp)
  4013a0:	00 
  4013a1:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4013a6:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  4013ab:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4013b0:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4013b5:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4013ba:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  4013bf:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4013c4:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4013c9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4013ce:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4013d3:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4013d8:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4013dd:	0f 57 c0             	xorps  %xmm0,%xmm0
  4013e0:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4013e5:	0f 57 c0             	xorps  %xmm0,%xmm0
  4013e8:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  4013ed:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  4013f4:	00 
  4013f5:	48 83 7c 24 68 00    	cmpq   $0x0,0x68(%rsp)
  4013fb:	0f 94 c0             	sete   %al
  4013fe:	24 01                	and    $0x1,%al
  401400:	3c 00                	cmp    $0x0,%al
  401402:	0f 84 a1 00 00 00    	je     4014a9 <runtime::udivmod128+0x169>
  401408:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  40140e:	0f 94 c0             	sete   %al
  401411:	24 01                	and    $0x1,%al
  401413:	3c 00                	cmp    $0x0,%al
  401415:	74 5c                	je     401473 <runtime::udivmod128+0x133>
  401417:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40141c:	48 83 f8 00          	cmp    $0x0,%rax
  401420:	0f 95 c0             	setne  %al
  401423:	24 01                	and    $0x1,%al
  401425:	3c 00                	cmp    $0x0,%al
  401427:	74 29                	je     401452 <runtime::udivmod128+0x112>
  401429:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40142e:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401433:	31 d2                	xor    %edx,%edx
  401435:	48 f7 f1             	div    %rcx
  401438:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40143d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  401442:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401447:	48 89 08             	mov    %rcx,(%rax)
  40144a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401451:	00 
  401452:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401457:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40145c:	31 d2                	xor    %edx,%edx
  40145e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  401463:	48 f7 f1             	div    %rcx
  401466:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  40146b:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401472:	c3                   	ret
  401473:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401478:	48 83 f8 00          	cmp    $0x0,%rax
  40147c:	0f 95 c0             	setne  %al
  40147f:	24 01                	and    $0x1,%al
  401481:	3c 00                	cmp    $0x0,%al
  401483:	74 15                	je     40149a <runtime::udivmod128+0x15a>
  401485:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40148a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40148f:	48 89 08             	mov    %rcx,(%rax)
  401492:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401499:	00 
  40149a:	31 c0                	xor    %eax,%eax
  40149c:	89 c2                	mov    %eax,%edx
  40149e:	48 89 d0             	mov    %rdx,%rax
  4014a1:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4014a8:	c3                   	ret
  4014a9:	48 83 7c 24 40 00    	cmpq   $0x0,0x40(%rsp)
  4014af:	0f 94 c0             	sete   %al
  4014b2:	24 01                	and    $0x1,%al
  4014b4:	3c 00                	cmp    $0x0,%al
  4014b6:	0f 84 8b 02 00 00    	je     401747 <runtime::udivmod128+0x407>
  4014bc:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  4014c2:	0f 94 c0             	sete   %al
  4014c5:	24 01                	and    $0x1,%al
  4014c7:	3c 00                	cmp    $0x0,%al
  4014c9:	74 52                	je     40151d <runtime::udivmod128+0x1dd>
  4014cb:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4014d0:	48 83 f8 00          	cmp    $0x0,%rax
  4014d4:	0f 95 c0             	setne  %al
  4014d7:	24 01                	and    $0x1,%al
  4014d9:	3c 00                	cmp    $0x0,%al
  4014db:	74 1f                	je     4014fc <runtime::udivmod128+0x1bc>
  4014dd:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4014e2:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4014e7:	31 d2                	xor    %edx,%edx
  4014e9:	48 f7 f1             	div    %rcx
  4014ec:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4014f1:	48 89 10             	mov    %rdx,(%rax)
  4014f4:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4014fb:	00 
  4014fc:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401501:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401506:	31 d2                	xor    %edx,%edx
  401508:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  40150d:	48 f7 f1             	div    %rcx
  401510:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  401515:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40151c:	c3                   	ret
  40151d:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  401523:	0f 94 c0             	sete   %al
  401526:	24 01                	and    $0x1,%al
  401528:	3c 00                	cmp    $0x0,%al
  40152a:	74 66                	je     401592 <runtime::udivmod128+0x252>
  40152c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401531:	48 83 f8 00          	cmp    $0x0,%rax
  401535:	0f 95 c0             	setne  %al
  401538:	24 01                	and    $0x1,%al
  40153a:	3c 00                	cmp    $0x0,%al
  40153c:	74 33                	je     401571 <runtime::udivmod128+0x231>
  40153e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401543:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401548:	31 d2                	xor    %edx,%edx
  40154a:	48 f7 f1             	div    %rcx
  40154d:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401552:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401557:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  40155e:	00 00 
  401560:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401565:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40156a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40156e:	48 89 08             	mov    %rcx,(%rax)
  401571:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401576:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40157b:	31 d2                	xor    %edx,%edx
  40157d:	48 89 54 24 90       	mov    %rdx,-0x70(%rsp)
  401582:	48 f7 f1             	div    %rcx
  401585:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  40158a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401591:	c3                   	ret
  401592:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401597:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40159c:	48 83 e9 01          	sub    $0x1,%rcx
  4015a0:	48 21 c8             	and    %rcx,%rax
  4015a3:	48 83 f8 00          	cmp    $0x0,%rax
  4015a7:	0f 94 c0             	sete   %al
  4015aa:	24 01                	and    $0x1,%al
  4015ac:	3c 00                	cmp    $0x0,%al
  4015ae:	74 7a                	je     40162a <runtime::udivmod128+0x2ea>
  4015b0:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4015b5:	48 83 f8 00          	cmp    $0x0,%rax
  4015b9:	0f 95 c0             	setne  %al
  4015bc:	24 01                	and    $0x1,%al
  4015be:	3c 00                	cmp    $0x0,%al
  4015c0:	74 35                	je     4015f7 <runtime::udivmod128+0x2b7>
  4015c2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4015c7:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4015cc:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4015d1:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4015d6:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4015db:	48 ff ca             	dec    %rdx
  4015de:	48 21 d1             	and    %rdx,%rcx
  4015e1:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4015e6:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4015eb:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4015f0:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4015f4:	48 89 08             	mov    %rcx,(%rax)
  4015f7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4015fc:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401601:	ba 40 00 00 00       	mov    $0x40,%edx
  401606:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  40160b:	88 d1                	mov    %dl,%cl
  40160d:	48 d3 e8             	shr    %cl,%rax
  401610:	48 89 c1             	mov    %rax,%rcx
  401613:	31 c0                	xor    %eax,%eax
  401615:	48 83 ea 40          	sub    $0x40,%rdx
  401619:	89 c2                	mov    %eax,%edx
  40161b:	48 89 d0             	mov    %rdx,%rax
  40161e:	48 0f 42 c1          	cmovb  %rcx,%rax
  401622:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401629:	c3                   	ret
  40162a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40162f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401634:	48 0f bd c1          	bsr    %rcx,%rax
  401638:	48 83 f0 3f          	xor    $0x3f,%rax
  40163c:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401641:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401646:	48 0f bd ca          	bsr    %rdx,%rcx
  40164a:	48 83 f1 3f          	xor    $0x3f,%rcx
  40164e:	29 c8                	sub    %ecx,%eax
  401650:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401654:	83 7c 24 1c 3e       	cmpl   $0x3e,0x1c(%rsp)
  401659:	0f 97 c0             	seta   %al
  40165c:	24 01                	and    $0x1,%al
  40165e:	3c 00                	cmp    $0x0,%al
  401660:	74 37                	je     401699 <runtime::udivmod128+0x359>
  401662:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401667:	48 83 f8 00          	cmp    $0x0,%rax
  40166b:	0f 95 c0             	setne  %al
  40166e:	24 01                	and    $0x1,%al
  401670:	3c 00                	cmp    $0x0,%al
  401672:	74 16                	je     40168a <runtime::udivmod128+0x34a>
  401674:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401679:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  40167e:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401683:	48 89 10             	mov    %rdx,(%rax)
  401686:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40168a:	31 c0                	xor    %eax,%eax
  40168c:	89 c2                	mov    %eax,%edx
  40168e:	48 89 d0             	mov    %rdx,%rax
  401691:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401698:	c3                   	ret
  401699:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  40169d:	83 c0 01             	add    $0x1,%eax
  4016a0:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4016a4:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4016ab:	00 00 
  4016ad:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4016b2:	b9 40 00 00 00       	mov    $0x40,%ecx
  4016b7:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4016bb:	89 c9                	mov    %ecx,%ecx
  4016bd:	89 ca                	mov    %ecx,%edx
  4016bf:	48 89 d1             	mov    %rdx,%rcx
  4016c2:	48 d3 e0             	shl    %cl,%rax
  4016c5:	48 89 c1             	mov    %rax,%rcx
  4016c8:	31 c0                	xor    %eax,%eax
  4016ca:	48 83 fa 40          	cmp    $0x40,%rdx
  4016ce:	48 0f 42 c1          	cmovb  %rcx,%rax
  4016d2:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4016d7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4016dc:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4016e0:	89 ca                	mov    %ecx,%edx
  4016e2:	48 89 d1             	mov    %rdx,%rcx
  4016e5:	48 d3 e8             	shr    %cl,%rax
  4016e8:	48 89 c1             	mov    %rax,%rcx
  4016eb:	31 c0                	xor    %eax,%eax
  4016ed:	48 83 fa 40          	cmp    $0x40,%rdx
  4016f1:	48 0f 42 c1          	cmovb  %rcx,%rax
  4016f5:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4016fa:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4016ff:	b9 40 00 00 00       	mov    $0x40,%ecx
  401704:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401708:	89 c9                	mov    %ecx,%ecx
  40170a:	89 ca                	mov    %ecx,%edx
  40170c:	48 89 d1             	mov    %rdx,%rcx
  40170f:	48 d3 e0             	shl    %cl,%rax
  401712:	48 89 c1             	mov    %rax,%rcx
  401715:	31 c0                	xor    %eax,%eax
  401717:	48 83 fa 40          	cmp    $0x40,%rdx
  40171b:	48 0f 42 c1          	cmovb  %rcx,%rax
  40171f:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401724:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401728:	89 ce                	mov    %ecx,%esi
  40172a:	48 89 f1             	mov    %rsi,%rcx
  40172d:	48 d3 ea             	shr    %cl,%rdx
  401730:	31 c9                	xor    %ecx,%ecx
  401732:	48 83 fe 40          	cmp    $0x40,%rsi
  401736:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40173a:	48 09 c8             	or     %rcx,%rax
  40173d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401742:	e9 30 04 00 00       	jmp    401b77 <runtime::udivmod128+0x837>
  401747:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  40174d:	0f 94 c0             	sete   %al
  401750:	24 01                	and    $0x1,%al
  401752:	3c 00                	cmp    $0x0,%al
  401754:	0f 84 d1 02 00 00    	je     401a2b <runtime::udivmod128+0x6eb>
  40175a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40175f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401764:	48 83 e9 01          	sub    $0x1,%rcx
  401768:	48 21 c8             	and    %rcx,%rax
  40176b:	48 83 f8 00          	cmp    $0x0,%rax
  40176f:	0f 94 c0             	sete   %al
  401772:	24 01                	and    $0x1,%al
  401774:	3c 00                	cmp    $0x0,%al
  401776:	0f 84 de 00 00 00    	je     40185a <runtime::udivmod128+0x51a>
  40177c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401781:	48 83 f8 00          	cmp    $0x0,%rax
  401785:	0f 95 c0             	setne  %al
  401788:	24 01                	and    $0x1,%al
  40178a:	3c 00                	cmp    $0x0,%al
  40178c:	74 20                	je     4017ae <runtime::udivmod128+0x46e>
  40178e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401793:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401798:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40179d:	48 ff ca             	dec    %rdx
  4017a0:	48 21 d1             	and    %rdx,%rcx
  4017a3:	48 89 08             	mov    %rcx,(%rax)
  4017a6:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4017ad:	00 
  4017ae:	48 83 7c 24 40 01    	cmpq   $0x1,0x40(%rsp)
  4017b4:	0f 94 c0             	sete   %al
  4017b7:	24 01                	and    $0x1,%al
  4017b9:	3c 00                	cmp    $0x0,%al
  4017bb:	74 12                	je     4017cf <runtime::udivmod128+0x48f>
  4017bd:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  4017c2:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4017c7:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4017ce:	c3                   	ret
  4017cf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4017d4:	b8 40 00 00 00       	mov    $0x40,%eax
  4017d9:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  4017de:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4017e2:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  4017e7:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  4017eb:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  4017ef:	88 c1                	mov    %al,%cl
  4017f1:	48 d3 ea             	shr    %cl,%rdx
  4017f4:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  4017f8:	31 c0                	xor    %eax,%eax
  4017fa:	83 e9 40             	sub    $0x40,%ecx
  4017fd:	48 89 c1             	mov    %rax,%rcx
  401800:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401804:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  401809:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  40180e:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401813:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  401817:	40 88 f1             	mov    %sil,%cl
  40181a:	48 d3 ef             	shr    %cl,%rdi
  40181d:	83 ee 40             	sub    $0x40,%esi
  401820:	48 89 c1             	mov    %rax,%rcx
  401823:	48 0f 42 cf          	cmovb  %rdi,%rcx
  401827:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  40182c:	f7 de                	neg    %esi
  40182e:	40 88 f1             	mov    %sil,%cl
  401831:	48 d3 e2             	shl    %cl,%rdx
  401834:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  401839:	83 ee 40             	sub    $0x40,%esi
  40183c:	48 0f 42 c2          	cmovb  %rdx,%rax
  401840:	48 09 c8             	or     %rcx,%rax
  401843:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401848:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40184d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401852:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401859:	c3                   	ret
  40185a:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40185f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401864:	48 0f bd c1          	bsr    %rcx,%rax
  401868:	48 83 f0 3f          	xor    $0x3f,%rax
  40186c:	83 c0 41             	add    $0x41,%eax
  40186f:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401874:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401879:	48 0f bd ca          	bsr    %rdx,%rcx
  40187d:	48 83 f1 3f          	xor    $0x3f,%rcx
  401881:	29 c8                	sub    %ecx,%eax
  401883:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401887:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  40188c:	0f 94 c1             	sete   %cl
  40188f:	80 e1 01             	and    $0x1,%cl
  401892:	b0 01                	mov    $0x1,%al
  401894:	38 c8                	cmp    %cl,%al
  401896:	74 13                	je     4018ab <runtime::udivmod128+0x56b>
  401898:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  40189d:	0f 92 c1             	setb   %cl
  4018a0:	80 e1 01             	and    $0x1,%cl
  4018a3:	b0 01                	mov    $0x1,%al
  4018a5:	38 c8                	cmp    %cl,%al
  4018a7:	74 32                	je     4018db <runtime::udivmod128+0x59b>
  4018a9:	eb 2b                	jmp    4018d6 <runtime::udivmod128+0x596>
  4018ab:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4018b2:	00 00 
  4018b4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4018b9:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4018be:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4018c5:	00 00 
  4018c7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4018cc:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4018d1:	e9 50 01 00 00       	jmp    401a26 <runtime::udivmod128+0x6e6>
  4018d6:	e9 a3 00 00 00       	jmp    40197e <runtime::udivmod128+0x63e>
  4018db:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4018e2:	00 00 
  4018e4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4018e9:	b9 40 00 00 00       	mov    $0x40,%ecx
  4018ee:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4018f2:	89 c9                	mov    %ecx,%ecx
  4018f4:	89 ca                	mov    %ecx,%edx
  4018f6:	48 89 d1             	mov    %rdx,%rcx
  4018f9:	48 d3 e0             	shl    %cl,%rax
  4018fc:	48 89 c1             	mov    %rax,%rcx
  4018ff:	31 c0                	xor    %eax,%eax
  401901:	48 83 fa 40          	cmp    $0x40,%rdx
  401905:	48 0f 42 c1          	cmovb  %rcx,%rax
  401909:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40190e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401913:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401917:	89 ca                	mov    %ecx,%edx
  401919:	48 89 d1             	mov    %rdx,%rcx
  40191c:	48 d3 e8             	shr    %cl,%rax
  40191f:	48 89 c1             	mov    %rax,%rcx
  401922:	31 c0                	xor    %eax,%eax
  401924:	48 83 fa 40          	cmp    $0x40,%rdx
  401928:	48 0f 42 c1          	cmovb  %rcx,%rax
  40192c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401931:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401936:	b9 40 00 00 00       	mov    $0x40,%ecx
  40193b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40193f:	89 c9                	mov    %ecx,%ecx
  401941:	89 ca                	mov    %ecx,%edx
  401943:	48 89 d1             	mov    %rdx,%rcx
  401946:	48 d3 e0             	shl    %cl,%rax
  401949:	48 89 c1             	mov    %rax,%rcx
  40194c:	31 c0                	xor    %eax,%eax
  40194e:	48 83 fa 40          	cmp    $0x40,%rdx
  401952:	48 0f 42 c1          	cmovb  %rcx,%rax
  401956:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40195b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40195f:	89 ce                	mov    %ecx,%esi
  401961:	48 89 f1             	mov    %rsi,%rcx
  401964:	48 d3 ea             	shr    %cl,%rdx
  401967:	31 c9                	xor    %ecx,%ecx
  401969:	48 83 fe 40          	cmp    $0x40,%rsi
  40196d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401971:	48 09 c8             	or     %rcx,%rax
  401974:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401979:	e9 a8 00 00 00       	jmp    401a26 <runtime::udivmod128+0x6e6>
  40197e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401983:	b9 80 00 00 00       	mov    $0x80,%ecx
  401988:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40198c:	89 c9                	mov    %ecx,%ecx
  40198e:	89 ca                	mov    %ecx,%edx
  401990:	48 89 d1             	mov    %rdx,%rcx
  401993:	48 d3 e0             	shl    %cl,%rax
  401996:	48 89 c1             	mov    %rax,%rcx
  401999:	31 c0                	xor    %eax,%eax
  40199b:	48 83 fa 40          	cmp    $0x40,%rdx
  40199f:	48 0f 42 c1          	cmovb  %rcx,%rax
  4019a3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4019a8:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4019ad:	b9 80 00 00 00       	mov    $0x80,%ecx
  4019b2:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4019b6:	89 c9                	mov    %ecx,%ecx
  4019b8:	89 ca                	mov    %ecx,%edx
  4019ba:	48 89 d1             	mov    %rdx,%rcx
  4019bd:	48 d3 e0             	shl    %cl,%rax
  4019c0:	48 89 c1             	mov    %rax,%rcx
  4019c3:	31 c0                	xor    %eax,%eax
  4019c5:	48 83 fa 40          	cmp    $0x40,%rdx
  4019c9:	48 0f 42 c1          	cmovb  %rcx,%rax
  4019cd:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4019d2:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4019d6:	83 e9 40             	sub    $0x40,%ecx
  4019d9:	89 c9                	mov    %ecx,%ecx
  4019db:	89 ce                	mov    %ecx,%esi
  4019dd:	48 89 f1             	mov    %rsi,%rcx
  4019e0:	48 d3 ea             	shr    %cl,%rdx
  4019e3:	31 c9                	xor    %ecx,%ecx
  4019e5:	48 83 fe 40          	cmp    $0x40,%rsi
  4019e9:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4019ed:	48 09 c8             	or     %rcx,%rax
  4019f0:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4019f5:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4019fc:	00 00 
  4019fe:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401a03:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401a07:	83 e9 40             	sub    $0x40,%ecx
  401a0a:	89 c9                	mov    %ecx,%ecx
  401a0c:	89 ca                	mov    %ecx,%edx
  401a0e:	48 89 d1             	mov    %rdx,%rcx
  401a11:	48 d3 e8             	shr    %cl,%rax
  401a14:	48 89 c1             	mov    %rax,%rcx
  401a17:	31 c0                	xor    %eax,%eax
  401a19:	48 83 fa 40          	cmp    $0x40,%rdx
  401a1d:	48 0f 42 c1          	cmovb  %rcx,%rax
  401a21:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401a26:	e9 4a 01 00 00       	jmp    401b75 <runtime::udivmod128+0x835>
  401a2b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401a30:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401a35:	48 0f bd c1          	bsr    %rcx,%rax
  401a39:	48 83 f0 3f          	xor    $0x3f,%rax
  401a3d:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401a42:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401a47:	48 0f bd ca          	bsr    %rdx,%rcx
  401a4b:	48 83 f1 3f          	xor    $0x3f,%rcx
  401a4f:	29 c8                	sub    %ecx,%eax
  401a51:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401a55:	83 7c 24 1c 3f       	cmpl   $0x3f,0x1c(%rsp)
  401a5a:	0f 97 c0             	seta   %al
  401a5d:	24 01                	and    $0x1,%al
  401a5f:	3c 00                	cmp    $0x0,%al
  401a61:	74 37                	je     401a9a <runtime::udivmod128+0x75a>
  401a63:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a68:	48 83 f8 00          	cmp    $0x0,%rax
  401a6c:	0f 95 c0             	setne  %al
  401a6f:	24 01                	and    $0x1,%al
  401a71:	3c 00                	cmp    $0x0,%al
  401a73:	74 16                	je     401a8b <runtime::udivmod128+0x74b>
  401a75:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a7a:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  401a7f:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401a84:	48 89 10             	mov    %rdx,(%rax)
  401a87:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401a8b:	31 c0                	xor    %eax,%eax
  401a8d:	89 c2                	mov    %eax,%edx
  401a8f:	48 89 d0             	mov    %rdx,%rax
  401a92:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401a99:	c3                   	ret
  401a9a:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401a9e:	83 c0 01             	add    $0x1,%eax
  401aa1:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401aa5:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401aac:	00 00 
  401aae:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401ab3:	0f 94 c0             	sete   %al
  401ab6:	24 01                	and    $0x1,%al
  401ab8:	3c 00                	cmp    $0x0,%al
  401aba:	74 22                	je     401ade <runtime::udivmod128+0x79e>
  401abc:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401ac1:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401ac6:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401acd:	00 00 
  401acf:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401ad4:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401ad9:	e9 95 00 00 00       	jmp    401b73 <runtime::udivmod128+0x833>
  401ade:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401ae3:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401ae7:	89 ca                	mov    %ecx,%edx
  401ae9:	48 89 d1             	mov    %rdx,%rcx
  401aec:	48 d3 e8             	shr    %cl,%rax
  401aef:	48 89 c1             	mov    %rax,%rcx
  401af2:	31 c0                	xor    %eax,%eax
  401af4:	48 83 fa 40          	cmp    $0x40,%rdx
  401af8:	48 0f 42 c1          	cmovb  %rcx,%rax
  401afc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401b01:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b06:	b9 40 00 00 00       	mov    $0x40,%ecx
  401b0b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401b0f:	89 c9                	mov    %ecx,%ecx
  401b11:	89 ca                	mov    %ecx,%edx
  401b13:	48 89 d1             	mov    %rdx,%rcx
  401b16:	48 d3 e0             	shl    %cl,%rax
  401b19:	48 89 c1             	mov    %rax,%rcx
  401b1c:	31 c0                	xor    %eax,%eax
  401b1e:	48 83 fa 40          	cmp    $0x40,%rdx
  401b22:	48 0f 42 c1          	cmovb  %rcx,%rax
  401b26:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401b2b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401b2f:	89 ce                	mov    %ecx,%esi
  401b31:	48 89 f1             	mov    %rsi,%rcx
  401b34:	48 d3 ea             	shr    %cl,%rdx
  401b37:	31 c9                	xor    %ecx,%ecx
  401b39:	48 83 fe 40          	cmp    $0x40,%rsi
  401b3d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401b41:	48 09 c8             	or     %rcx,%rax
  401b44:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401b49:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401b4e:	b9 40 00 00 00       	mov    $0x40,%ecx
  401b53:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401b57:	89 c9                	mov    %ecx,%ecx
  401b59:	89 ca                	mov    %ecx,%edx
  401b5b:	48 89 d1             	mov    %rdx,%rcx
  401b5e:	48 d3 e0             	shl    %cl,%rax
  401b61:	48 89 c1             	mov    %rax,%rcx
  401b64:	31 c0                	xor    %eax,%eax
  401b66:	48 83 fa 40          	cmp    $0x40,%rdx
  401b6a:	48 0f 42 c1          	cmovb  %rcx,%rax
  401b6e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401b73:	eb 00                	jmp    401b75 <runtime::udivmod128+0x835>
  401b75:	eb 00                	jmp    401b77 <runtime::udivmod128+0x837>
  401b77:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  401b7e:	00 
  401b7f:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401b86:	00 00 
  401b88:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  401b8f:	00 00 
  401b91:	83 7c 24 1c 00       	cmpl   $0x0,0x1c(%rsp)
  401b96:	0f 97 c0             	seta   %al
  401b99:	24 01                	and    $0x1,%al
  401b9b:	3c 00                	cmp    $0x0,%al
  401b9d:	0f 84 eb 00 00 00    	je     401c8e <runtime::udivmod128+0x94e>
  401ba3:	48 8b 74 24 b8       	mov    -0x48(%rsp),%rsi
  401ba8:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  401bad:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401bb2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401bb7:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401bbc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401bc1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401bc6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  401bcb:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401bd0:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401bd5:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401bda:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  401bdf:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401be4:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401be9:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401bee:	48 01 c0             	add    %rax,%rax
  401bf1:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  401bf5:	48 09 c8             	or     %rcx,%rax
  401bf8:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401bfd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401c02:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  401c07:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  401c0c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401c11:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401c16:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401c1b:	48 f7 d0             	not    %rax
  401c1e:	48 f7 d1             	not    %rcx
  401c21:	48 01 f1             	add    %rsi,%rcx
  401c24:	48 11 d0             	adc    %rdx,%rax
  401c27:	48 c1 f8 3f          	sar    $0x3f,%rax
  401c2b:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401c30:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401c35:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401c39:	83 e0 01             	and    $0x1,%eax
  401c3c:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  401c40:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401c45:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401c4a:	48 21 ca             	and    %rcx,%rdx
  401c4d:	48 21 c6             	and    %rax,%rsi
  401c50:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401c55:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401c5a:	48 29 f1             	sub    %rsi,%rcx
  401c5d:	48 19 d0             	sbb    %rdx,%rax
  401c60:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401c65:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401c6a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401c6f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401c74:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401c79:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401c7e:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401c82:	83 e8 01             	sub    $0x1,%eax
  401c85:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401c89:	e9 03 ff ff ff       	jmp    401b91 <runtime::udivmod128+0x851>
  401c8e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401c93:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401c98:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401c9d:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  401ca2:	48 01 c9             	add    %rcx,%rcx
  401ca5:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  401ca9:	48 09 f1             	or     %rsi,%rcx
  401cac:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  401cb1:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  401cb6:	48 83 f8 00          	cmp    $0x0,%rax
  401cba:	0f 95 c0             	setne  %al
  401cbd:	24 01                	and    $0x1,%al
  401cbf:	3c 00                	cmp    $0x0,%al
  401cc1:	74 16                	je     401cd9 <runtime::udivmod128+0x999>
  401cc3:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401cc8:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401ccd:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  401cd2:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401cd6:	48 89 08             	mov    %rcx,(%rax)
  401cd9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401cde:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  401ce3:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401cea:	c3                   	ret
  401ceb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401cf0 <runtime::stderr_write>:
  401cf0:	48 83 ec 48          	sub    $0x48,%rsp
  401cf4:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  401cf9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  401cfe:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  401d03:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401d08:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401d0d:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  401d12:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  401d17:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401d1e:	00 00 
  401d20:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  401d25:	e8 16 00 00 00       	call   401d40 <runtime::[os_specific_linux.odin]::_stderr_write>
  401d2a:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401d2f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  401d34:	48 89 11             	mov    %rdx,(%rcx)
  401d37:	48 83 c4 48          	add    $0x48,%rsp
  401d3b:	c3                   	ret
  401d3c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401d40 <runtime::[os_specific_linux.odin]::_stderr_write>:
  401d40:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  401d45:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  401d4a:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  401d4f:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  401d54:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  401d59:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  401d5e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  401d63:	b8 01 00 00 00       	mov    $0x1,%eax
  401d68:	bf 02 00 00 00       	mov    $0x2,%edi
  401d6d:	0f 05                	syscall
  401d6f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401d74:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  401d7a:	0f 9c c0             	setl   %al
  401d7d:	24 01                	and    $0x1,%al
  401d7f:	3c 00                	cmp    $0x0,%al
  401d81:	74 26                	je     401da9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  401d83:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  401d8a:	ff ff 
  401d8c:	0f 9f c0             	setg   %al
  401d8f:	24 01                	and    $0x1,%al
  401d91:	3c 00                	cmp    $0x0,%al
  401d93:	74 14                	je     401da9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  401d95:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  401d9a:	31 c0                	xor    %eax,%eax
  401d9c:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  401da1:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  401da8:	c3                   	ret
  401da9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401dae:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401db3:	48 89 08             	mov    %rcx,(%rax)
  401db6:	31 c0                	xor    %eax,%eax
  401db8:	c3                   	ret
  401db9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401dc0 <runtime::heap_allocator_proc>:
  401dc0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  401dc7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  401dcc:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  401dd1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401dd6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401ddb:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  401de0:	40 88 f0             	mov    %sil,%al
  401de3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  401de7:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401dee:	00 
  401def:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401df4:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  401dfb:	00 
  401dfc:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  401e01:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401e05:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401e0a:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401e0f:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401e14:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401e19:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  401e1e:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  401e25:	00 
  401e26:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401e2d:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  401e34:	00 
  401e35:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  401e3c:	00 
  401e3d:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  401e44:	00 
  401e45:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  401e4c:	00 
  401e4d:	0f b6 c8             	movzbl %al,%ecx
  401e50:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  401e55:	2c 07                	sub    $0x7,%al
  401e57:	0f 87 5f 01 00 00    	ja     401fbc <runtime::heap_allocator_proc+0x1fc>
  401e5d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401e62:	48 8b 04 c5 c0 70 40 	mov    0x4070c0(,%rax,8),%rax
  401e69:	00 
  401e6a:	ff e0                	jmp    *%rax
  401e6c:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401e71:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401e76:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401e7b:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401e7f:	84 c0                	test   %al,%al
  401e81:	0f 94 c0             	sete   %al
  401e84:	0f 57 c0             	xorps  %xmm0,%xmm0
  401e87:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  401e8e:	00 
  401e8f:	48 89 e1             	mov    %rsp,%rcx
  401e92:	48 89 11             	mov    %rdx,(%rcx)
  401e95:	44 0f b6 c0          	movzbl %al,%r8d
  401e99:	31 c0                	xor    %eax,%eax
  401e9b:	89 c1                	mov    %eax,%ecx
  401e9d:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  401ea4:	00 
  401ea5:	48 89 ca             	mov    %rcx,%rdx
  401ea8:	e8 43 3f 00 00       	call   405df0 <runtime::heap_allocator_proc.aligned_alloc-0>
  401ead:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401eb2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  401eb9:	00 
  401eba:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  401ec1:	00 
  401ec2:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401ec6:	48 89 11             	mov    %rdx,(%rcx)
  401ec9:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401ed0:	c3                   	ret
  401ed1:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  401ed6:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401edb:	e8 60 41 00 00       	call   406040 <runtime::heap_allocator_proc.aligned_free-1>
  401ee0:	e9 d7 00 00 00       	jmp    401fbc <runtime::heap_allocator_proc+0x1fc>
  401ee5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401eea:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401ef1:	00 
  401ef2:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401ef9:	b0 04                	mov    $0x4,%al
  401efb:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401f02:	c3                   	ret
  401f03:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401f08:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401f0d:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401f12:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401f17:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  401f1c:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401f20:	2c 03                	sub    $0x3,%al
  401f22:	0f 94 c0             	sete   %al
  401f25:	0f 57 c0             	xorps  %xmm0,%xmm0
  401f28:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  401f2d:	49 89 e0             	mov    %rsp,%r8
  401f30:	4d 89 08             	mov    %r9,(%r8)
  401f33:	44 0f b6 c0          	movzbl %al,%r8d
  401f37:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  401f3c:	e8 3f 41 00 00       	call   406080 <runtime::heap_allocator_proc.aligned_resize-2>
  401f41:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401f46:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  401f4b:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  401f50:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401f54:	48 89 11             	mov    %rdx,(%rcx)
  401f57:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401f5e:	c3                   	ret
  401f5f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401f64:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401f69:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  401f6f:	0f 95 c0             	setne  %al
  401f72:	24 01                	and    $0x1,%al
  401f74:	3c 00                	cmp    $0x0,%al
  401f76:	74 08                	je     401f80 <runtime::heap_allocator_proc+0x1c0>
  401f78:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  401f7d:	c6 00 db             	movb   $0xdb,(%rax)
  401f80:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401f85:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401f8c:	00 
  401f8d:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401f94:	31 c0                	xor    %eax,%eax
  401f96:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401f9d:	c3                   	ret
  401f9e:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401fa3:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401faa:	00 
  401fab:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401fb2:	b0 04                	mov    $0x4,%al
  401fb4:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401fbb:	c3                   	ret
  401fbc:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401fc1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401fc8:	00 
  401fc9:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401fd0:	31 c0                	xor    %eax,%eax
  401fd2:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401fd9:	c3                   	ret
  401fda:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401fe0 <runtime::[internal.odin]::byte_slice>:
  401fe0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  401fe5:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  401fea:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401fef:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401ff4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401ff9:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  401ffe:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402003:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  402008:	31 c0                	xor    %eax,%eax
  40200a:	48 85 d2             	test   %rdx,%rdx
  40200d:	48 0f 49 c2          	cmovns %rdx,%rax
  402011:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  402016:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40201b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  402020:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  402025:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40202a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40202f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  402034:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  402039:	c3                   	ret
  40203a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402040 <runtime::bounds_check_error>:
  402040:	48 83 ec 58          	sub    $0x58,%rsp
  402044:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402049:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40204e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402052:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402056:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40205b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402060:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402065:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40206a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40206e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402072:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402077:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40207c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402081:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402086:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40208a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40208e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402093:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402098:	48 39 c8             	cmp    %rcx,%rax
  40209b:	0f 92 c0             	setb   %al
  40209e:	24 01                	and    $0x1,%al
  4020a0:	3c 00                	cmp    $0x0,%al
  4020a2:	74 05                	je     4020a9 <runtime::bounds_check_error+0x69>
  4020a4:	48 83 c4 58          	add    $0x58,%rsp
  4020a8:	c3                   	ret
  4020a9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4020ae:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4020b3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4020b7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4020bb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4020c0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4020c5:	e8 76 42 00 00       	call   406340 <runtime::bounds_check_error.handle_error-0>
  4020ca:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004020d0 <runtime::is_power_of_two_int>:
  4020d0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  4020d5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4020da:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4020df:	48 83 f8 00          	cmp    $0x0,%rax
  4020e3:	0f 9e c0             	setle  %al
  4020e6:	24 01                	and    $0x1,%al
  4020e8:	3c 00                	cmp    $0x0,%al
  4020ea:	74 03                	je     4020ef <runtime::is_power_of_two_int+0x1f>
  4020ec:	31 c0                	xor    %eax,%eax
  4020ee:	c3                   	ret
  4020ef:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4020f4:	48 89 c1             	mov    %rax,%rcx
  4020f7:	48 83 e9 01          	sub    $0x1,%rcx
  4020fb:	48 21 c8             	and    %rcx,%rax
  4020fe:	48 83 f8 00          	cmp    $0x0,%rax
  402102:	0f 94 c0             	sete   %al
  402105:	24 01                	and    $0x1,%al
  402107:	c3                   	ret
  402108:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40210f:	00 

0000000000402110 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  402110:	48 83 ec 18          	sub    $0x18,%rsp
  402114:	48 89 3c 24          	mov    %rdi,(%rsp)
  402118:	40 88 f0             	mov    %sil,%al
  40211b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40211f:	48 8b 04 24          	mov    (%rsp),%rax
  402123:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  402127:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40212c:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  402130:	48 83 f8 00          	cmp    $0x0,%rax
  402134:	0f 9e c0             	setle  %al
  402137:	24 01                	and    $0x1,%al
  402139:	3c 00                	cmp    $0x0,%al
  40213b:	74 07                	je     402144 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  40213d:	31 c0                	xor    %eax,%eax
  40213f:	48 83 c4 18          	add    $0x18,%rsp
  402143:	c3                   	ret
  402144:	8a 44 24 0e          	mov    0xe(%rsp),%al
  402148:	3c 00                	cmp    $0x0,%al
  40214a:	74 13                	je     40215f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  40214c:	48 8b 34 24          	mov    (%rsp),%rsi
  402150:	bf 01 00 00 00       	mov    $0x1,%edi
  402155:	e8 f6 ee ff ff       	call   401050 <calloc@plt>
  40215a:	48 83 c4 18          	add    $0x18,%rsp
  40215e:	c3                   	ret
  40215f:	48 8b 3c 24          	mov    (%rsp),%rdi
  402163:	e8 08 ef ff ff       	call   401070 <malloc@plt>
  402168:	48 83 c4 18          	add    $0x18,%rsp
  40216c:	c3                   	ret
  40216d:	0f 1f 00             	nopl   (%rax)

0000000000402170 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  402170:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  402175:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  40217a:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  40217f:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  402184:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  402189:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40218e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  402193:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  402198:	48 01 c2             	add    %rax,%rdx
  40219b:	0f 92 c0             	setb   %al
  40219e:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  4021a3:	24 01                	and    $0x1,%al
  4021a5:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  4021a9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4021ae:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  4021b3:	0f 94 c0             	sete   %al
  4021b6:	24 01                	and    $0x1,%al
  4021b8:	48 89 11             	mov    %rdx,(%rcx)
  4021bb:	c3                   	ret
  4021bc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004021c0 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  4021c0:	48 83 ec 28          	sub    $0x28,%rsp
  4021c4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4021c9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4021ce:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4021d3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4021d8:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4021dd:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4021e2:	e8 99 ee ff ff       	call   401080 <realloc@plt>
  4021e7:	48 83 c4 28          	add    $0x28,%rsp
  4021eb:	c3                   	ret
  4021ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004021f0 <runtime::memory_block_alloc>:
  4021f0:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  4021f7:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  4021fc:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  402201:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  402206:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40220b:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  402210:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402215:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  40221c:	00 
  40221d:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402222:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  402227:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  40222c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  402231:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  402236:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40223b:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  402240:	48 89 b4 24 f0 00 00 	mov    %rsi,0xf0(%rsp)
  402247:	00 
  402248:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  40224f:	00 
  402250:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  402257:	00 
  402258:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40225d:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  402264:	00 
  402265:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40226a:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  402271:	00 
  402272:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  402279:	00 
  40227a:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  402281:	00 00 00 00 00 
  402286:	c6 84 24 d7 00 00 00 	movb   $0x0,0xd7(%rsp)
  40228d:	00 
  40228e:	48 89 c1             	mov    %rax,%rcx
  402291:	48 83 e9 31          	sub    $0x31,%rcx
  402295:	b9 30 00 00 00       	mov    $0x30,%ecx
  40229a:	48 0f 43 c8          	cmovae %rax,%rcx
  40229e:	48 01 ca             	add    %rcx,%rdx
  4022a1:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  4022a8:	00 
  4022a9:	48 89 8c 24 c0 00 00 	mov    %rcx,0xc0(%rsp)
  4022b0:	00 
  4022b1:	48 89 c1             	mov    %rax,%rcx
  4022b4:	48 83 e9 10          	sub    $0x10,%rcx
  4022b8:	b9 10 00 00 00       	mov    $0x10,%ecx
  4022bd:	48 0f 4c c1          	cmovl  %rcx,%rax
  4022c1:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4022c8:	00 
  4022c9:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  4022d0:	00 
  4022d1:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  4022d8:	00 
  4022d9:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  4022e0:	00 
  4022e1:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  4022e8:	00 
  4022e9:	0f 57 c0             	xorps  %xmm0,%xmm0
  4022ec:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4022f3:	00 
  4022f4:	48 89 e0             	mov    %rsp,%rax
  4022f7:	4c 89 08             	mov    %r9,(%rax)
  4022fa:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  402301:	00 
  402302:	e8 f9 13 00 00       	call   403700 <runtime::mem_alloc>
  402307:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  40230b:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  402312:	00 
  402313:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402318:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  40231f:	00 
  402320:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  402325:	3c 00                	cmp    $0x0,%al
  402327:	74 39                	je     402362 <runtime::memory_block_alloc+0x172>
  402329:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40232e:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  402332:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402339:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402340:	00 
  402341:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  402348:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  40234f:	00 
  402350:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402357:	48 89 11             	mov    %rdx,(%rcx)
  40235a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402361:	c3                   	ret
  402362:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  402367:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40236c:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402371:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402376:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40237b:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  402380:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402387:	00 
  402388:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40238d:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  402394:	00 
  402395:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40239a:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  4023a1:	00 
  4023a2:	48 01 f0             	add    %rsi,%rax
  4023a5:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4023aa:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4023af:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4023b4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4023bb:	00 
  4023bc:	48 89 50 10          	mov    %rdx,0x10(%rax)
  4023c0:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4023c4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4023cb:	00 
  4023cc:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  4023d3:	00 
  4023d4:	48 03 8c 24 c0 00 00 	add    0xc0(%rsp),%rcx
  4023db:	00 
  4023dc:	48 89 48 18          	mov    %rcx,0x18(%rax)
  4023e0:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4023e7:	00 
  4023e8:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4023ed:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  4023f4:	00 
  4023f5:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  4023f9:	48 29 d1             	sub    %rdx,%rcx
  4023fc:	48 89 48 28          	mov    %rcx,0x28(%rax)
  402400:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402407:	00 
  402408:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  40240d:	0f 94 c0             	sete   %al
  402410:	24 01                	and    $0x1,%al
  402412:	0f b6 f8             	movzbl %al,%edi
  402415:	be 40 71 40 00       	mov    $0x407140,%esi
  40241a:	b9 b0 71 40 00       	mov    $0x4071b0,%ecx
  40241f:	ba 0f 00 00 00       	mov    $0xf,%edx
  402424:	e8 67 39 00 00       	call   405d90 <runtime::assert>
  402429:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  40242e:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402435:	00 
  402436:	48 83 38 00          	cmpq   $0x0,(%rax)
  40243a:	0f 94 c0             	sete   %al
  40243d:	24 01                	and    $0x1,%al
  40243f:	0f b6 f8             	movzbl %al,%edi
  402442:	be d8 71 40 00       	mov    $0x4071d8,%esi
  402447:	b9 f0 71 40 00       	mov    $0x4071f0,%ecx
  40244c:	ba 11 00 00 00       	mov    $0x11,%edx
  402451:	e8 3a 39 00 00       	call   405d90 <runtime::assert>
  402456:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40245b:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402462:	00 
  402463:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  40246a:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  402471:	00 
  402472:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402479:	48 89 11             	mov    %rdx,(%rcx)
  40247c:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402483:	c3                   	ret
  402484:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40248b:	00 00 00 00 00 

0000000000402490 <runtime::default_temp_allocator_destroy>:
  402490:	48 83 ec 18          	sub    $0x18,%rsp
  402494:	48 89 3c 24          	mov    %rdi,(%rsp)
  402498:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40249d:	48 8b 04 24          	mov    (%rsp),%rax
  4024a1:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4024a6:	48 83 f8 00          	cmp    $0x0,%rax
  4024aa:	0f 95 c0             	setne  %al
  4024ad:	24 01                	and    $0x1,%al
  4024af:	3c 00                	cmp    $0x0,%al
  4024b1:	74 29                	je     4024dc <runtime::default_temp_allocator_destroy+0x4c>
  4024b3:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4024b8:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4024bd:	48 be 80 72 40 00 00 	movabs $0x407280,%rsi
  4024c4:	00 00 00 
  4024c7:	e8 14 1b 00 00       	call   403fe0 <runtime::arena_destroy>
  4024cc:	48 8b 3c 24          	mov    (%rsp),%rdi
  4024d0:	31 f6                	xor    %esi,%esi
  4024d2:	ba 38 00 00 00       	mov    $0x38,%edx
  4024d7:	e8 64 eb ff ff       	call   401040 <memset@plt>
  4024dc:	48 83 c4 18          	add    $0x18,%rsp
  4024e0:	c3                   	ret
  4024e1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4024e8:	0f 1f 84 00 00 00 00 
  4024ef:	00 

00000000004024f0 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  4024f0:	48 83 ec 18          	sub    $0x18,%rsp
  4024f4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4024f9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4024fe:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402503:	e8 28 eb ff ff       	call   401030 <free@plt>
  402508:	48 83 c4 18          	add    $0x18,%rsp
  40250c:	c3                   	ret
  40250d:	0f 1f 00             	nopl   (%rax)

0000000000402510 <runtime::default_random_generator_proc>:
  402510:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402517:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40251c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402521:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  402526:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40252b:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402530:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402535:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40253a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40253f:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402546:	00 
  402547:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  40254c:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  402551:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  402556:	48 83 f8 00          	cmp    $0x0,%rax
  40255a:	0f 94 c0             	sete   %al
  40255d:	24 01                	and    $0x1,%al
  40255f:	3c 00                	cmp    $0x0,%al
  402561:	74 1a                	je     40257d <runtime::default_random_generator_proc+0x6d>
  402563:	48 c7 c1 e8 ff ff ff 	mov    $0xffffffffffffffe8,%rcx
  40256a:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402571:	00 00 
  402573:	48 01 c8             	add    %rcx,%rax
  402576:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40257b:	eb 0a                	jmp    402587 <runtime::default_random_generator_proc+0x77>
  40257d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402582:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402587:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40258c:	48 85 c0             	test   %rax,%rax
  40258f:	74 27                	je     4025b8 <runtime::default_random_generator_proc+0xa8>
  402591:	eb 00                	jmp    402593 <runtime::default_random_generator_proc+0x83>
  402593:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402598:	48 83 e8 01          	sub    $0x1,%rax
  40259c:	0f 84 17 01 00 00    	je     4026b9 <runtime::default_random_generator_proc+0x1a9>
  4025a2:	eb 00                	jmp    4025a4 <runtime::default_random_generator_proc+0x94>
  4025a4:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4025a9:	48 83 e8 02          	sub    $0x2,%rax
  4025ad:	0f 84 40 01 00 00    	je     4026f3 <runtime::default_random_generator_proc+0x1e3>
  4025b3:	e9 6b 01 00 00       	jmp    402723 <runtime::default_random_generator_proc+0x213>
  4025b8:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4025bd:	48 83 38 00          	cmpq   $0x0,(%rax)
  4025c1:	0f 94 c0             	sete   %al
  4025c4:	24 01                	and    $0x1,%al
  4025c6:	3c 00                	cmp    $0x0,%al
  4025c8:	74 21                	je     4025eb <runtime::default_random_generator_proc+0xdb>
  4025ca:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4025cf:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  4025d4:	0f 94 c0             	sete   %al
  4025d7:	24 01                	and    $0x1,%al
  4025d9:	3c 00                	cmp    $0x0,%al
  4025db:	74 0e                	je     4025eb <runtime::default_random_generator_proc+0xdb>
  4025dd:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4025e2:	31 c0                	xor    %eax,%eax
  4025e4:	89 c6                	mov    %eax,%esi
  4025e6:	e8 15 3f 00 00       	call   406500 <runtime::default_random_generator_proc.init-1>
  4025eb:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4025f0:	48 83 e8 08          	sub    $0x8,%rax
  4025f4:	75 26                	jne    40261c <runtime::default_random_generator_proc+0x10c>
  4025f6:	eb 00                	jmp    4025f8 <runtime::default_random_generator_proc+0xe8>
  4025f8:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4025fd:	e8 2e 3e 00 00       	call   406430 <runtime::default_random_generator_proc.read_u64-0>
  402602:	48 89 c1             	mov    %rax,%rcx
  402605:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40260a:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  40260f:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402614:	48 89 08             	mov    %rcx,(%rax)
  402617:	e9 9b 00 00 00       	jmp    4026b7 <runtime::default_random_generator_proc+0x1a7>
  40261c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402621:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  402626:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  40262d:	00 00 
  40262f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402634:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  40263b:	ff ff 
  40263d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402642:	48 83 c0 01          	add    $0x1,%rax
  402646:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40264b:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  402650:	7d 63                	jge    4026b5 <runtime::default_random_generator_proc+0x1a5>
  402652:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402657:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40265c:	48 01 c8             	add    %rcx,%rax
  40265f:	48 89 04 24          	mov    %rax,(%rsp)
  402663:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  402668:	0f 94 c0             	sete   %al
  40266b:	24 01                	and    $0x1,%al
  40266d:	3c 00                	cmp    $0x0,%al
  40266f:	74 14                	je     402685 <runtime::default_random_generator_proc+0x175>
  402671:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402676:	e8 b5 3d 00 00       	call   406430 <runtime::default_random_generator_proc.read_u64-0>
  40267b:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402680:	c6 44 24 57 07       	movb   $0x7,0x57(%rsp)
  402685:	48 8b 04 24          	mov    (%rsp),%rax
  402689:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40268e:	88 08                	mov    %cl,(%rax)
  402690:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402695:	48 c1 e9 08          	shr    $0x8,%rcx
  402699:	b2 01                	mov    $0x1,%dl
  40269b:	31 c0                	xor    %eax,%eax
  40269d:	f6 c2 01             	test   $0x1,%dl
  4026a0:	48 0f 45 c1          	cmovne %rcx,%rax
  4026a4:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4026a9:	8a 44 24 57          	mov    0x57(%rsp),%al
  4026ad:	2c 01                	sub    $0x1,%al
  4026af:	88 44 24 57          	mov    %al,0x57(%rsp)
  4026b3:	eb 88                	jmp    40263d <runtime::default_random_generator_proc+0x12d>
  4026b5:	eb 00                	jmp    4026b7 <runtime::default_random_generator_proc+0x1a7>
  4026b7:	eb 6a                	jmp    402723 <runtime::default_random_generator_proc+0x213>
  4026b9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4026be:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4026c3:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4026ca:	00 00 
  4026cc:	b8 08 00 00 00       	mov    $0x8,%eax
  4026d1:	48 39 d0             	cmp    %rdx,%rax
  4026d4:	48 0f 4c d0          	cmovl  %rax,%rdx
  4026d8:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4026dd:	e8 5e 09 00 00       	call   403040 <runtime::mem_copy_non_overlapping>
  4026e2:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4026e7:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4026ec:	e8 0f 3e 00 00       	call   406500 <runtime::default_random_generator_proc.init-1>
  4026f1:	eb 30                	jmp    402723 <runtime::default_random_generator_proc+0x213>
  4026f3:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4026f8:	48 83 f8 04          	cmp    $0x4,%rax
  4026fc:	0f 95 c0             	setne  %al
  4026ff:	24 01                	and    $0x1,%al
  402701:	3c 00                	cmp    $0x0,%al
  402703:	74 08                	je     40270d <runtime::default_random_generator_proc+0x1fd>
  402705:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40270c:	c3                   	ret
  40270d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402712:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402717:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40271c:	8b 08                	mov    (%rax),%ecx
  40271e:	83 c9 0a             	or     $0xa,%ecx
  402721:	89 08                	mov    %ecx,(%rax)
  402723:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40272a:	c3                   	ret
  40272b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402730 <runtime::slice_handle_error>:
  402730:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402737:	4c 89 0c 24          	mov    %r9,(%rsp)
  40273b:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  402740:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  402744:	89 54 24 14          	mov    %edx,0x14(%rsp)
  402748:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40274d:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  402752:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  402759:	00 
  40275a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40275f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402764:	4c 8b 04 24          	mov    (%rsp),%r8
  402768:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40276d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  402771:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  402775:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40277a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40277f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402784:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40278b:	00 
  40278c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402790:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402794:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  402799:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  40279e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  4027a3:	0f 57 c0             	xorps  %xmm0,%xmm0
  4027a6:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4027ab:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4027b0:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4027b7:	00 00 
  4027b9:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4027be:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4027c3:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4027ca:	00 00 
  4027cc:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4027d1:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4027d6:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4027da:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4027de:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4027e3:	e8 c8 16 00 00       	call   403eb0 <runtime::print_caller_location>
  4027e8:	bf a9 72 40 00       	mov    $0x4072a9,%edi
  4027ed:	be 17 00 00 00       	mov    $0x17,%esi
  4027f2:	e8 99 0e 00 00       	call   403690 <runtime::print_string>
  4027f7:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4027fc:	e8 af 14 00 00       	call   403cb0 <runtime::print_i64>
  402801:	bf c1 72 40 00       	mov    $0x4072c1,%edi
  402806:	be 01 00 00 00       	mov    $0x1,%esi
  40280b:	e8 80 0e 00 00       	call   403690 <runtime::print_string>
  402810:	48 8b 3c 24          	mov    (%rsp),%rdi
  402814:	e8 97 14 00 00       	call   403cb0 <runtime::print_i64>
  402819:	bf c3 72 40 00       	mov    $0x4072c3,%edi
  40281e:	be 15 00 00 00       	mov    $0x15,%esi
  402823:	e8 68 0e 00 00       	call   403690 <runtime::print_string>
  402828:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40282d:	e8 7e 14 00 00       	call   403cb0 <runtime::print_i64>
  402832:	bf 0a 00 00 00       	mov    $0xa,%edi
  402837:	e8 84 10 00 00       	call   4038c0 <runtime::print_byte>
  40283c:	e8 cf ea ff ff       	call   401310 <runtime::bounds_trap>
  402841:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402848:	0f 1f 84 00 00 00 00 
  40284f:	00 

0000000000402850 <runtime::default_temp_allocator_proc>:
  402850:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  402857:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  40285c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  402861:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402866:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  40286b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  402870:	40 88 f0             	mov    %sil,%al
  402873:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  402877:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40287e:	00 
  40287f:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402884:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40288b:	00 
  40288c:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  402891:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  402898:	00 
  402899:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40289e:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  4028a3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4028a8:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4028ad:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4028b2:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  4028b6:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  4028bb:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  4028c0:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4028c5:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  4028cc:	00 
  4028cd:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4028d4:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  4028db:	00 
  4028dc:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  4028e3:	00 
  4028e4:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  4028eb:	00 
  4028ec:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  4028f3:	00 
  4028f4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4028f7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4028fe:	00 
  4028ff:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  402906:	00 
  402907:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  40290e:	00 
  40290f:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  402916:	00 
  402917:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40291e:	00 
  40291f:	48 89 e6             	mov    %rsp,%rsi
  402922:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  402926:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  40292d:	00 
  40292e:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  402932:	4c 89 16             	mov    %r10,(%rsi)
  402935:	0f b6 f0             	movzbl %al,%esi
  402938:	e8 43 17 00 00       	call   404080 <runtime::arena_allocator_proc>
  40293d:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402942:	40 88 c7             	mov    %al,%dil
  402945:	40 88 f8             	mov    %dil,%al
  402948:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  40294f:	00 
  402950:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  402957:	00 
  402958:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40295f:	00 
  402960:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402967:	00 
  402968:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  40296f:	00 
  402970:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402974:	48 89 11             	mov    %rdx,(%rcx)
  402977:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40297e:	c3                   	ret
  40297f:	90                   	nop

0000000000402980 <runtime::multi_pointer_slice_handle_error>:
  402980:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402987:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40298c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402991:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402995:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402999:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40299e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4029a3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4029a8:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4029ad:	8b 44 24 18          	mov    0x18(%rsp),%eax
  4029b1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4029b5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4029ba:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4029bf:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4029c4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  4029cb:	00 
  4029cc:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  4029d0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  4029d4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  4029d9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  4029de:	0f 57 c0             	xorps  %xmm0,%xmm0
  4029e1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4029e6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4029eb:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4029f2:	00 00 
  4029f4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4029f9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4029fe:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402a05:	00 00 
  402a07:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  402a0c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402a11:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  402a15:	89 44 24 44          	mov    %eax,0x44(%rsp)
  402a19:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402a1e:	e8 8d 14 00 00       	call   403eb0 <runtime::print_caller_location>
  402a23:	bf a9 72 40 00       	mov    $0x4072a9,%edi
  402a28:	be 17 00 00 00       	mov    $0x17,%esi
  402a2d:	e8 5e 0c 00 00       	call   403690 <runtime::print_string>
  402a32:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402a37:	e8 74 12 00 00       	call   403cb0 <runtime::print_i64>
  402a3c:	bf c1 72 40 00       	mov    $0x4072c1,%edi
  402a41:	be 01 00 00 00       	mov    $0x1,%esi
  402a46:	e8 45 0c 00 00       	call   403690 <runtime::print_string>
  402a4b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402a50:	e8 5b 12 00 00       	call   403cb0 <runtime::print_i64>
  402a55:	bf 0a 00 00 00       	mov    $0xa,%edi
  402a5a:	e8 61 0e 00 00       	call   4038c0 <runtime::print_byte>
  402a5f:	e8 ac e8 ff ff       	call   401310 <runtime::bounds_trap>
  402a64:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402a6b:	00 00 00 00 00 

0000000000402a70 <runtime::memory_block_dealloc>:
  402a70:	48 83 ec 38          	sub    $0x38,%rsp
  402a74:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402a79:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402a7e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402a83:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402a88:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402a8d:	48 83 f8 00          	cmp    $0x0,%rax
  402a91:	0f 95 c0             	setne  %al
  402a94:	24 01                	and    $0x1,%al
  402a96:	3c 00                	cmp    $0x0,%al
  402a98:	74 35                	je     402acf <runtime::memory_block_dealloc+0x5f>
  402a9a:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  402a9f:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402aa4:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402aa9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402aae:	48 8b 42 08          	mov    0x8(%rdx),%rax
  402ab2:	48 8b 52 10          	mov    0x10(%rdx),%rdx
  402ab6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402abb:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402ac0:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402ac5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402aca:	e8 f1 0f 00 00       	call   403ac0 <runtime::mem_free>
  402acf:	48 83 c4 38          	add    $0x38,%rsp
  402ad3:	c3                   	ret
  402ad4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402adb:	00 00 00 00 00 

0000000000402ae0 <main>:
  402ae0:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  402ae7:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  402aeb:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402af0:	8b 44 24 14          	mov    0x14(%rsp),%eax
  402af4:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402af9:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  402b00:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  402b07:	00 
  402b08:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  402b0f:	00 
  402b10:	48 89 0c 24          	mov    %rcx,(%rsp)
  402b14:	4c 63 c8             	movslq %eax,%r9
  402b17:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402b1c:	bf d9 72 40 00       	mov    $0x4072d9,%edi
  402b21:	31 c0                	xor    %eax,%eax
  402b23:	41 89 c0             	mov    %eax,%r8d
  402b26:	be 2c 00 00 00       	mov    $0x2c,%esi
  402b2b:	ba 36 00 00 00       	mov    $0x36,%edx
  402b30:	b9 11 00 00 00       	mov    $0x11,%ecx
  402b35:	e8 c6 03 00 00       	call   402f00 <runtime::multi_pointer_slice_expr_error>
  402b3a:	48 8b 0c 24          	mov    (%rsp),%rcx
  402b3e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402b43:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  402b4a:	00 
  402b4b:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  402b52:	00 
  402b53:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  402b5a:	00 
  402b5b:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  402b62:	00 
  402b63:	48 c7 c0 60 a0 40 00 	mov    $0x40a060,%rax
  402b6a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402b6e:	48 89 08             	mov    %rcx,(%rax)
  402b71:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402b78:	00 
  402b79:	31 f6                	xor    %esi,%esi
  402b7b:	ba 70 00 00 00       	mov    $0x70,%edx
  402b80:	e8 bb e4 ff ff       	call   401040 <memset@plt>
  402b85:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402b8c:	00 
  402b8d:	e8 fe 24 00 00       	call   405090 <runtime::[core.odin]::__init_context>
  402b92:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  402b97:	31 f6                	xor    %esi,%esi
  402b99:	ba 70 00 00 00       	mov    $0x70,%edx
  402b9e:	e8 9d e4 ff ff       	call   401040 <memset@plt>
  402ba3:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  402ba8:	e8 93 24 00 00       	call   405040 <runtime::default_context>
  402bad:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  402bb2:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  402bb7:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  402bbc:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  402bc1:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  402bc6:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  402bcb:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  402bd2:	00 
  402bd3:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  402bda:	00 
  402bdb:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  402be2:	00 
  402be3:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  402bea:	00 
  402beb:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  402bf2:	00 
  402bf3:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  402bfa:	00 
  402bfb:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  402c02:	00 
  402c03:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  402c0a:	00 
  402c0b:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402c12:	00 
  402c13:	e8 78 e5 ff ff       	call   401190 <__$startup_runtime>
  402c18:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402c1f:	00 
  402c20:	e8 9b e5 ff ff       	call   4011c0 <journey::main>
  402c25:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402c2c:	00 
  402c2d:	e8 6e e5 ff ff       	call   4011a0 <__$cleanup_runtime>
  402c32:	31 c0                	xor    %eax,%eax
  402c34:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  402c3b:	c3                   	ret
  402c3c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402c40 <runtime::alloc_from_memory_block>:
  402c40:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  402c47:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402c4c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402c51:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402c56:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402c5b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402c60:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402c65:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402c6a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  402c71:	00 
  402c72:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  402c79:	00 
  402c7a:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  402c81:	00 
  402c82:	0f 57 c0             	xorps  %xmm0,%xmm0
  402c85:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402c8c:	00 
  402c8d:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  402c94:	00 
  402c95:	48 83 f8 00          	cmp    $0x0,%rax
  402c99:	0f 94 c0             	sete   %al
  402c9c:	24 01                	and    $0x1,%al
  402c9e:	3c 00                	cmp    $0x0,%al
  402ca0:	74 3e                	je     402ce0 <runtime::alloc_from_memory_block+0xa0>
  402ca2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402ca7:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  402cae:	00 00 00 00 00 
  402cb3:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  402cba:	00 00 00 00 00 
  402cbf:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402cc6:	01 
  402cc7:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402cce:	00 
  402ccf:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402cd6:	b0 01                	mov    $0x1,%al
  402cd8:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402cdf:	c3                   	ret
  402ce0:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  402ce5:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402cea:	e8 b1 38 00 00       	call   4065a0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  402cef:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402cf4:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  402cfb:	00 
  402cfc:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  402d03:	00 
  402d04:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  402d0b:	00 00 00 00 00 
  402d10:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  402d17:	00 
  402d18:	e8 53 f4 ff ff       	call   402170 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  402d1d:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  402d24:	00 
  402d25:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  402d2a:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  402d2e:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  402d33:	75 4a                	jne    402d7f <runtime::alloc_from_memory_block+0x13f>
  402d35:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402d3a:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402d41:	01 
  402d42:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402d49:	00 
  402d4a:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402d51:	00 
  402d52:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402d59:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402d60:	00 
  402d61:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402d68:	00 
  402d69:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402d70:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402d74:	48 89 11             	mov    %rdx,(%rcx)
  402d77:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402d7e:	c3                   	ret
  402d7f:	eb 00                	jmp    402d81 <runtime::alloc_from_memory_block+0x141>
  402d81:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  402d88:	00 
  402d89:	48 8b 78 20          	mov    0x20(%rax),%rdi
  402d8d:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  402d92:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  402d99:	00 00 
  402d9b:	48 8d 54 24 60       	lea    0x60(%rsp),%rdx
  402da0:	e8 cb f3 ff ff       	call   402170 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  402da5:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  402daa:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  402daf:	88 44 24 47          	mov    %al,0x47(%rsp)
  402db3:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  402db8:	74 1a                	je     402dd4 <runtime::alloc_from_memory_block+0x194>
  402dba:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402dbf:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402dc6:	00 
  402dc7:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  402dcb:	0f 97 c0             	seta   %al
  402dce:	24 01                	and    $0x1,%al
  402dd0:	3c 00                	cmp    $0x0,%al
  402dd2:	74 4a                	je     402e1e <runtime::alloc_from_memory_block+0x1de>
  402dd4:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402dd9:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402de0:	01 
  402de1:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402de8:	00 
  402de9:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402df0:	00 
  402df1:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402df8:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402dff:	00 
  402e00:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402e07:	00 
  402e08:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402e0f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402e13:	48 89 11             	mov    %rdx,(%rcx)
  402e16:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402e1d:	c3                   	ret
  402e1e:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  402e23:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402e2a:	00 
  402e2b:	48 8b 41 18          	mov    0x18(%rcx),%rax
  402e2f:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  402e33:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  402e3a:	00 
  402e3b:	48 01 d1             	add    %rdx,%rcx
  402e3e:	48 01 c8             	add    %rcx,%rax
  402e41:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402e46:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402e4b:	48 89 04 24          	mov    %rax,(%rsp)
  402e4f:	bf 50 71 40 00       	mov    $0x407150,%edi
  402e54:	31 c0                	xor    %eax,%eax
  402e56:	41 89 c0             	mov    %eax,%r8d
  402e59:	be 3e 00 00 00       	mov    $0x3e,%esi
  402e5e:	ba 55 00 00 00       	mov    $0x55,%edx
  402e63:	b9 31 00 00 00       	mov    $0x31,%ecx
  402e68:	e8 93 00 00 00       	call   402f00 <runtime::multi_pointer_slice_expr_error>
  402e6d:	48 8b 14 24          	mov    (%rsp),%rdx
  402e71:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402e76:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402e7b:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402e80:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402e85:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402e8a:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402e8f:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  402e96:	00 
  402e97:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402e9e:	00 
  402e9f:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  402ea6:	00 
  402ea7:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  402eac:	48 8b 50 20          	mov    0x20(%rax),%rdx
  402eb0:	48 01 f2             	add    %rsi,%rdx
  402eb3:	48 89 50 20          	mov    %rdx,0x20(%rax)
  402eb7:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402ebe:	00 
  402ebf:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402ec6:	00 
  402ec7:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402ece:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402ed5:	00 
  402ed6:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402edd:	00 
  402ede:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402ee5:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402ee9:	48 89 11             	mov    %rdx,(%rcx)
  402eec:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402ef3:	c3                   	ret
  402ef4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402efb:	00 00 00 00 00 

0000000000402f00 <runtime::multi_pointer_slice_expr_error>:
  402f00:	48 83 ec 58          	sub    $0x58,%rsp
  402f04:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402f09:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402f0e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402f12:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402f16:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402f1b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402f20:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402f25:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  402f2a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  402f2e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402f32:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402f37:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402f3c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402f41:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402f46:	89 74 24 44          	mov    %esi,0x44(%rsp)
  402f4a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  402f4e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402f53:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402f58:	48 39 c8             	cmp    %rcx,%rax
  402f5b:	0f 9e c0             	setle  %al
  402f5e:	24 01                	and    $0x1,%al
  402f60:	3c 00                	cmp    $0x0,%al
  402f62:	74 05                	je     402f69 <runtime::multi_pointer_slice_expr_error+0x69>
  402f64:	48 83 c4 58          	add    $0x58,%rsp
  402f68:	c3                   	ret
  402f69:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  402f6e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402f73:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  402f77:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  402f7b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402f80:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402f85:	e8 f6 f9 ff ff       	call   402980 <runtime::multi_pointer_slice_handle_error>
  402f8a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402f90 <runtime::slice_expr_error_hi>:
  402f90:	48 83 ec 58          	sub    $0x58,%rsp
  402f94:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402f99:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402f9e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402fa2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402fa6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402fab:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402fb0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402fb5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402fba:	8b 54 24 18          	mov    0x18(%rsp),%edx
  402fbe:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402fc2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402fc7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402fcc:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402fd1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402fd6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  402fda:	89 54 24 40          	mov    %edx,0x40(%rsp)
  402fde:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  402fe3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402fe8:	31 c0                	xor    %eax,%eax
  402fea:	48 39 c8             	cmp    %rcx,%rax
  402fed:	0f 9e c0             	setle  %al
  402ff0:	24 01                	and    $0x1,%al
  402ff2:	3c 00                	cmp    $0x0,%al
  402ff4:	74 1b                	je     403011 <runtime::slice_expr_error_hi+0x81>
  402ff6:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402ffb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403000:	48 39 c8             	cmp    %rcx,%rax
  403003:	0f 9e c0             	setle  %al
  403006:	24 01                	and    $0x1,%al
  403008:	3c 00                	cmp    $0x0,%al
  40300a:	74 05                	je     403011 <runtime::slice_expr_error_hi+0x81>
  40300c:	48 83 c4 58          	add    $0x58,%rsp
  403010:	c3                   	ret
  403011:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403016:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40301a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40301e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403023:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403028:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40302d:	48 89 e0             	mov    %rsp,%rax
  403030:	4c 89 00             	mov    %r8,(%rax)
  403033:	31 c0                	xor    %eax,%eax
  403035:	41 89 c0             	mov    %eax,%r8d
  403038:	e8 f3 f6 ff ff       	call   402730 <runtime::slice_handle_error>
  40303d:	0f 1f 00             	nopl   (%rax)

0000000000403040 <runtime::mem_copy_non_overlapping>:
  403040:	48 83 ec 38          	sub    $0x38,%rsp
  403044:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403049:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40304e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403053:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403058:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40305d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403062:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403067:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40306c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  403071:	48 83 f8 00          	cmp    $0x0,%rax
  403075:	0f 95 c0             	setne  %al
  403078:	24 01                	and    $0x1,%al
  40307a:	3c 00                	cmp    $0x0,%al
  40307c:	74 3c                	je     4030ba <runtime::mem_copy_non_overlapping+0x7a>
  40307e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403083:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  403088:	48 39 c8             	cmp    %rcx,%rax
  40308b:	0f 95 c0             	setne  %al
  40308e:	24 01                	and    $0x1,%al
  403090:	3c 00                	cmp    $0x0,%al
  403092:	74 26                	je     4030ba <runtime::mem_copy_non_overlapping+0x7a>
  403094:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403099:	48 83 f8 00          	cmp    $0x0,%rax
  40309d:	0f 9f c0             	setg   %al
  4030a0:	24 01                	and    $0x1,%al
  4030a2:	3c 00                	cmp    $0x0,%al
  4030a4:	74 14                	je     4030ba <runtime::mem_copy_non_overlapping+0x7a>
  4030a6:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4030ab:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4030b0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4030b5:	e8 a6 df ff ff       	call   401060 <memcpy@plt>
  4030ba:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4030bf:	48 83 c4 38          	add    $0x38,%rsp
  4030c3:	c3                   	ret
  4030c4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4030cb:	00 00 00 00 00 

00000000004030d0 <runtime::slice_expr_error_lo_hi>:
  4030d0:	48 83 ec 68          	sub    $0x68,%rsp
  4030d4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4030d9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4030de:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4030e2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4030e6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4030eb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4030f0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4030f5:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4030fa:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4030ff:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403104:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403109:	8b 74 24 18          	mov    0x18(%rsp),%esi
  40310d:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  403111:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403116:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  40311b:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  403120:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  403125:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  403129:	89 74 24 50          	mov    %esi,0x50(%rsp)
  40312d:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  403132:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  403137:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40313c:	31 c0                	xor    %eax,%eax
  40313e:	48 39 c8             	cmp    %rcx,%rax
  403141:	0f 9e c0             	setle  %al
  403144:	24 01                	and    $0x1,%al
  403146:	3c 00                	cmp    $0x0,%al
  403148:	74 47                	je     403191 <runtime::slice_expr_error_lo_hi+0xc1>
  40314a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40314f:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403154:	48 39 c8             	cmp    %rcx,%rax
  403157:	0f 9e c0             	setle  %al
  40315a:	24 01                	and    $0x1,%al
  40315c:	3c 00                	cmp    $0x0,%al
  40315e:	74 31                	je     403191 <runtime::slice_expr_error_lo_hi+0xc1>
  403160:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403165:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40316a:	48 39 c8             	cmp    %rcx,%rax
  40316d:	0f 9e c0             	setle  %al
  403170:	24 01                	and    $0x1,%al
  403172:	3c 00                	cmp    $0x0,%al
  403174:	74 1b                	je     403191 <runtime::slice_expr_error_lo_hi+0xc1>
  403176:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40317b:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403180:	48 39 c8             	cmp    %rcx,%rax
  403183:	0f 9e c0             	setle  %al
  403186:	24 01                	and    $0x1,%al
  403188:	3c 00                	cmp    $0x0,%al
  40318a:	74 05                	je     403191 <runtime::slice_expr_error_lo_hi+0xc1>
  40318c:	48 83 c4 68          	add    $0x68,%rsp
  403190:	c3                   	ret
  403191:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403196:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40319b:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40319f:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4031a3:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4031a8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4031ad:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  4031b2:	48 89 e0             	mov    %rsp,%rax
  4031b5:	4c 89 10             	mov    %r10,(%rax)
  4031b8:	e8 73 f5 ff ff       	call   402730 <runtime::slice_handle_error>
  4031bd:	0f 1f 00             	nopl   (%rax)

00000000004031c0 <runtime::memset>:
  4031c0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4031c5:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  4031c9:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  4031ce:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4031d3:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4031d8:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  4031dc:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4031e1:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  4031e5:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4031ea:	48 83 f8 00          	cmp    $0x0,%rax
  4031ee:	0f 95 c0             	setne  %al
  4031f1:	24 01                	and    $0x1,%al
  4031f3:	3c 00                	cmp    $0x0,%al
  4031f5:	74 63                	je     40325a <runtime::memset+0x9a>
  4031f7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4031fc:	48 83 f8 00          	cmp    $0x0,%rax
  403200:	0f 95 c0             	setne  %al
  403203:	24 01                	and    $0x1,%al
  403205:	3c 00                	cmp    $0x0,%al
  403207:	74 51                	je     40325a <runtime::memset+0x9a>
  403209:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40320e:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  403212:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  403216:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40321b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  403222:	00 00 
  403224:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  403229:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  40322e:	0f 9c c0             	setl   %al
  403231:	24 01                	and    $0x1,%al
  403233:	3c 00                	cmp    $0x0,%al
  403235:	74 21                	je     403258 <runtime::memset+0x98>
  403237:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40323c:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  403241:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  403245:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  403248:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40324d:	48 83 c0 01          	add    $0x1,%rax
  403251:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  403256:	eb cc                	jmp    403224 <runtime::memset+0x64>
  403258:	eb 00                	jmp    40325a <runtime::memset+0x9a>
  40325a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40325f:	c3                   	ret

0000000000403260 <runtime::arena_alloc>:
  403260:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  403267:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40326c:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403271:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  403276:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40327b:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403280:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  403285:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40328a:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40328f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403294:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403299:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40329e:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  4032a5:	00 
  4032a6:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  4032ad:	00 
  4032ae:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4032b5:	00 
  4032b6:	0f 57 c0             	xorps  %xmm0,%xmm0
  4032b9:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  4032c0:	00 
  4032c1:	c6 84 24 0f 01 00 00 	movb   $0x0,0x10f(%rsp)
  4032c8:	00 
  4032c9:	48 89 c2             	mov    %rax,%rdx
  4032cc:	48 83 ea 01          	sub    $0x1,%rdx
  4032d0:	48 21 d0             	and    %rdx,%rax
  4032d3:	48 83 f8 00          	cmp    $0x0,%rax
  4032d7:	0f 94 c0             	sete   %al
  4032da:	24 01                	and    $0x1,%al
  4032dc:	0f b6 f8             	movzbl %al,%edi
  4032df:	be 06 73 40 00       	mov    $0x407306,%esi
  4032e4:	ba 1a 00 00 00       	mov    $0x1a,%edx
  4032e9:	e8 a2 2a 00 00       	call   405d90 <runtime::assert>
  4032ee:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4032f3:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  4032fa:	00 
  4032fb:	48 83 bc 24 00 01 00 	cmpq   $0x0,0x100(%rsp)
  403302:	00 00 
  403304:	0f 94 c0             	sete   %al
  403307:	24 01                	and    $0x1,%al
  403309:	3c 00                	cmp    $0x0,%al
  40330b:	74 42                	je     40334f <runtime::arena_alloc+0xef>
  40330d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403312:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403319:	00 
  40331a:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403321:	00 
  403322:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403329:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403330:	00 
  403331:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403338:	00 
  403339:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403340:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403344:	48 89 11             	mov    %rdx,(%rcx)
  403347:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40334e:	c3                   	ret
  40334f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403356:	00 
  403357:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  40335c:	0f 94 c0             	sete   %al
  40335f:	24 01                	and    $0x1,%al
  403361:	3c 00                	cmp    $0x0,%al
  403363:	74 09                	je     40336e <runtime::arena_alloc+0x10e>
  403365:	31 c0                	xor    %eax,%eax
  403367:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40336c:	eb 15                	jmp    403383 <runtime::arena_alloc+0x123>
  40336e:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403375:	00 
  403376:	48 8b 40 10          	mov    0x10(%rax),%rax
  40337a:	48 8b 40 20          	mov    0x20(%rax),%rax
  40337e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403383:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403388:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40338d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403392:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  403399:	00 
  40339a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4033a1:	00 
  4033a2:	48 8b 78 10          	mov    0x10(%rax),%rdi
  4033a6:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  4033ad:	00 
  4033ae:	0f 57 c0             	xorps  %xmm0,%xmm0
  4033b1:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4033b8:	00 
  4033b9:	48 8d 8c 24 e0 00 00 	lea    0xe0(%rsp),%rcx
  4033c0:	00 
  4033c1:	e8 7a f8 ff ff       	call   402c40 <runtime::alloc_from_memory_block>
  4033c6:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  4033cd:	00 
  4033ce:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4033d5:	00 
  4033d6:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  4033dd:	00 
  4033de:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  4033e5:	00 
  4033e6:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4033ed:	80 bc 24 0f 01 00 00 	cmpb   $0x1,0x10f(%rsp)
  4033f4:	01 
  4033f5:	0f 94 c0             	sete   %al
  4033f8:	24 01                	and    $0x1,%al
  4033fa:	3c 00                	cmp    $0x0,%al
  4033fc:	0f 84 19 02 00 00    	je     40361b <runtime::arena_alloc+0x3bb>
  403402:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403409:	00 
  40340a:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  40340f:	0f 94 c0             	sete   %al
  403412:	24 01                	and    $0x1,%al
  403414:	3c 00                	cmp    $0x0,%al
  403416:	74 10                	je     403428 <runtime::arena_alloc+0x1c8>
  403418:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40341f:	00 
  403420:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  403427:	00 
  403428:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40342d:	48 8b bc 24 00 01 00 	mov    0x100(%rsp),%rdi
  403434:	00 
  403435:	e8 f6 31 00 00       	call   406630 <runtime::arena_alloc.align_forward_uint-0>
  40343a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  403441:	00 
  403442:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403449:	00 
  40344a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403451:	00 
  403452:	48 8b 40 28          	mov    0x28(%rax),%rax
  403456:	48 39 c1             	cmp    %rax,%rcx
  403459:	48 0f 47 c1          	cmova  %rcx,%rax
  40345d:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  403464:	00 
  403465:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40346c:	00 
  40346d:	48 83 38 00          	cmpq   $0x0,(%rax)
  403471:	0f 94 c0             	sete   %al
  403474:	24 01                	and    $0x1,%al
  403476:	3c 00                	cmp    $0x0,%al
  403478:	74 46                	je     4034c0 <runtime::arena_alloc+0x260>
  40347a:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40347f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403486:	00 
  403487:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40348c:	e8 8f de ff ff       	call   401320 <runtime::heap_allocator>
  403491:	48 89 c1             	mov    %rax,%rcx
  403494:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403499:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  4034a0:	00 
  4034a1:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  4034a8:	00 
  4034a9:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4034b0:	00 
  4034b1:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  4034b8:	00 
  4034b9:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4034bd:	48 89 08             	mov    %rcx,(%rax)
  4034c0:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  4034c5:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4034ca:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  4034cf:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4034d6:	00 
  4034d7:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  4034de:	00 
  4034df:	48 8b 38             	mov    (%rax),%rdi
  4034e2:	48 8b 70 08          	mov    0x8(%rax),%rsi
  4034e6:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  4034ed:	00 00 00 00 00 
  4034f2:	48 89 e0             	mov    %rsp,%rax
  4034f5:	4c 89 08             	mov    %r9,(%rax)
  4034f8:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  4034ff:	00 
  403500:	e8 eb ec ff ff       	call   4021f0 <runtime::memory_block_alloc>
  403505:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403509:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  403510:	00 
  403511:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  403516:	3c 00                	cmp    $0x0,%al
  403518:	74 4d                	je     403567 <runtime::arena_alloc+0x307>
  40351a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40351f:	8a 44 24 0f          	mov    0xf(%rsp),%al
  403523:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  40352a:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403531:	00 
  403532:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403539:	00 
  40353a:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403541:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403548:	00 
  403549:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403550:	00 
  403551:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403558:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40355c:	48 89 11             	mov    %rdx,(%rcx)
  40355f:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403566:	c3                   	ret
  403567:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40356c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403571:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403576:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40357d:	00 
  40357e:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403585:	00 
  403586:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  40358d:	00 
  40358e:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  403592:	48 89 08             	mov    %rcx,(%rax)
  403595:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40359c:	00 
  40359d:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  4035a4:	00 
  4035a5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4035a9:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4035b0:	00 
  4035b1:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  4035b8:	00 
  4035b9:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  4035bd:	48 8b 48 20          	mov    0x20(%rax),%rcx
  4035c1:	48 01 f1             	add    %rsi,%rcx
  4035c4:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4035c8:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  4035cf:	00 00 00 00 00 
  4035d4:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4035db:	00 
  4035dc:	48 8b 78 10          	mov    0x10(%rax),%rdi
  4035e0:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  4035e7:	00 
  4035e8:	0f 57 c0             	xorps  %xmm0,%xmm0
  4035eb:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4035f0:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  4035f5:	e8 46 f6 ff ff       	call   402c40 <runtime::alloc_from_memory_block>
  4035fa:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4035ff:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  403604:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  40360b:	00 
  40360c:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403613:	00 
  403614:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  40361b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403620:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403627:	00 
  403628:	48 8b 70 10          	mov    0x10(%rax),%rsi
  40362c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  403630:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  403634:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  40363b:	00 
  40363c:	48 29 fe             	sub    %rdi,%rsi
  40363f:	48 01 f2             	add    %rsi,%rdx
  403642:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403646:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  40364d:	00 
  40364e:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403655:	00 
  403656:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  40365d:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403664:	00 
  403665:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  40366c:	00 
  40366d:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403674:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403678:	48 89 11             	mov    %rdx,(%rcx)
  40367b:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403682:	c3                   	ret
  403683:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40368a:	84 00 00 00 00 00 

0000000000403690 <runtime::print_string>:
  403690:	48 83 ec 58          	sub    $0x58,%rsp
  403694:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403699:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40369e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4036a3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4036a8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4036ad:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  4036b2:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  4036b9:	00 00 
  4036bb:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4036c0:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4036c5:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4036ca:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4036cf:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4036d6:	00 00 
  4036d8:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  4036dd:	e8 0e e6 ff ff       	call   401cf0 <runtime::stderr_write>
  4036e2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4036e7:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4036ec:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4036f1:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4036f6:	48 83 c4 58          	add    $0x58,%rsp
  4036fa:	c3                   	ret
  4036fb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403700 <runtime::mem_alloc>:
  403700:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  403707:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  40370c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  403711:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403716:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40371b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403720:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  403725:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40372c:	00 
  40372d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403732:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403737:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40373c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403741:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403746:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40374d:	00 
  40374e:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  403755:	00 
  403756:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40375d:	00 
  40375e:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403765:	00 
  403766:	e8 65 e9 ff ff       	call   4020d0 <runtime::is_power_of_two_int>
  40376b:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403770:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403775:	0f b6 f8             	movzbl %al,%edi
  403778:	be 21 73 40 00       	mov    $0x407321,%esi
  40377d:	ba 20 00 00 00       	mov    $0x20,%edx
  403782:	e8 09 26 00 00       	call   405d90 <runtime::assert>
  403787:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40378c:	48 83 f8 00          	cmp    $0x0,%rax
  403790:	0f 94 c0             	sete   %al
  403793:	24 01                	and    $0x1,%al
  403795:	3c 00                	cmp    $0x0,%al
  403797:	75 12                	jne    4037ab <runtime::mem_alloc+0xab>
  403799:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  4037a0:	00 00 
  4037a2:	0f 94 c0             	sete   %al
  4037a5:	24 01                	and    $0x1,%al
  4037a7:	3c 00                	cmp    $0x0,%al
  4037a9:	74 1e                	je     4037c9 <runtime::mem_alloc+0xc9>
  4037ab:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4037b0:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4037b7:	00 
  4037b8:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4037bf:	31 c0                	xor    %eax,%eax
  4037c1:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  4037c8:	c3                   	ret
  4037c9:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4037ce:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4037d3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4037d8:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  4037dd:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4037e4:	00 
  4037e5:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  4037ec:	00 
  4037ed:	0f 57 c0             	xorps  %xmm0,%xmm0
  4037f0:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4037f5:	48 89 e6             	mov    %rsp,%rsi
  4037f8:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  4037fc:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  403801:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  403805:	4c 89 06             	mov    %r8,(%rsi)
  403808:	31 f6                	xor    %esi,%esi
  40380a:	41 89 f1             	mov    %esi,%r9d
  40380d:	4d 89 c8             	mov    %r9,%r8
  403810:	ff d0                	call   *%rax
  403812:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403817:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40381c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  403821:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403825:	48 89 11             	mov    %rdx,(%rcx)
  403828:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  40382f:	c3                   	ret

0000000000403830 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  403830:	48 83 ec 48          	sub    $0x48,%rsp
  403834:	48 89 0c 24          	mov    %rcx,(%rsp)
  403838:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40383d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403842:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403847:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40384c:	48 8b 04 24          	mov    (%rsp),%rax
  403850:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403855:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40385a:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40385f:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403864:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403869:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40386e:	48 39 c1             	cmp    %rax,%rcx
  403871:	48 0f 4c c1          	cmovl  %rcx,%rax
  403875:	31 c9                	xor    %ecx,%ecx
  403877:	48 39 c1             	cmp    %rax,%rcx
  40387a:	48 0f 4f c1          	cmovg  %rcx,%rax
  40387e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403883:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  403889:	0f 9f c0             	setg   %al
  40388c:	24 01                	and    $0x1,%al
  40388e:	3c 00                	cmp    $0x0,%al
  403890:	74 18                	je     4038aa <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)+0x7a>
  403892:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403897:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40389c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4038a1:	48 c1 e2 00          	shl    $0x0,%rdx
  4038a5:	e8 e6 d7 ff ff       	call   401090 <memmove@plt>
  4038aa:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4038af:	48 83 c4 48          	add    $0x48,%rsp
  4038b3:	c3                   	ret
  4038b4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4038bb:	00 00 00 00 00 

00000000004038c0 <runtime::print_byte>:
  4038c0:	48 83 ec 68          	sub    $0x68,%rsp
  4038c4:	40 88 f8             	mov    %dil,%al
  4038c7:	88 44 24 07          	mov    %al,0x7(%rsp)
  4038cb:	8a 54 24 07          	mov    0x7(%rsp),%dl
  4038cf:	88 54 24 67          	mov    %dl,0x67(%rsp)
  4038d3:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  4038da:	00 00 
  4038dc:	0f 57 c0             	xorps  %xmm0,%xmm0
  4038df:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4038e4:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  4038e9:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  4038ee:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4038f3:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  4038fa:	00 00 
  4038fc:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403901:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403906:	88 11                	mov    %dl,(%rcx)
  403908:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40390d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403912:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  403917:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40391c:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  403923:	00 00 
  403925:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  40392a:	e8 c1 e3 ff ff       	call   401cf0 <runtime::stderr_write>
  40392f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403934:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403939:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40393e:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403943:	48 83 c4 68          	add    $0x68,%rsp
  403947:	c3                   	ret
  403948:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40394f:	00 

0000000000403950 <runtime::matrix_bounds_check_error>:
  403950:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403957:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40395c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403961:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  403965:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  403969:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  40396e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403973:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  40397a:	00 
  40397b:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403980:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403987:	00 
  403988:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40398d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403992:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403997:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40399c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4039a1:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  4039a5:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  4039aa:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  4039af:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  4039b4:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  4039b9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  4039c0:	00 
  4039c1:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  4039c6:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  4039ca:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4039cf:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  4039d4:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4039d9:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  4039de:	48 39 c8             	cmp    %rcx,%rax
  4039e1:	0f 92 c0             	setb   %al
  4039e4:	24 01                	and    $0x1,%al
  4039e6:	3c 00                	cmp    $0x0,%al
  4039e8:	74 1e                	je     403a08 <runtime::matrix_bounds_check_error+0xb8>
  4039ea:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4039ef:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4039f4:	48 39 c8             	cmp    %rcx,%rax
  4039f7:	0f 92 c0             	setb   %al
  4039fa:	24 01                	and    $0x1,%al
  4039fc:	3c 00                	cmp    $0x0,%al
  4039fe:	74 08                	je     403a08 <runtime::matrix_bounds_check_error+0xb8>
  403a00:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  403a07:	c3                   	ret
  403a08:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  403a0d:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403a12:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  403a16:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  403a1a:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  403a1f:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403a24:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  403a29:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  403a2e:	48 89 e0             	mov    %rsp,%rax
  403a31:	4c 89 58 08          	mov    %r11,0x8(%rax)
  403a35:	4c 89 10             	mov    %r10,(%rax)
  403a38:	e8 53 2c 00 00       	call   406690 <runtime::matrix_bounds_check_error.handle_error-0>
  403a3d:	0f 1f 00             	nopl   (%rax)

0000000000403a40 <runtime::heap_alloc>:
  403a40:	48 83 ec 18          	sub    $0x18,%rsp
  403a44:	48 89 3c 24          	mov    %rdi,(%rsp)
  403a48:	40 88 f0             	mov    %sil,%al
  403a4b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  403a4f:	8a 44 24 0e          	mov    0xe(%rsp),%al
  403a53:	48 8b 3c 24          	mov    (%rsp),%rdi
  403a57:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403a5c:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403a60:	0f b6 f0             	movzbl %al,%esi
  403a63:	e8 a8 e6 ff ff       	call   402110 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  403a68:	48 83 c4 18          	add    $0x18,%rsp
  403a6c:	c3                   	ret
  403a6d:	0f 1f 00             	nopl   (%rax)

0000000000403a70 <runtime::heap_resize>:
  403a70:	48 83 ec 28          	sub    $0x28,%rsp
  403a74:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403a79:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403a7e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403a83:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403a88:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  403a8d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403a92:	e8 29 e7 ff ff       	call   4021c0 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  403a97:	48 83 c4 28          	add    $0x28,%rsp
  403a9b:	c3                   	ret
  403a9c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403aa0 <runtime::heap_free>:
  403aa0:	48 83 ec 18          	sub    $0x18,%rsp
  403aa4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403aa9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403aae:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403ab3:	e8 38 ea ff ff       	call   4024f0 <runtime::[heap_allocator_unix.odin]::_heap_free>
  403ab8:	48 83 c4 18          	add    $0x18,%rsp
  403abc:	c3                   	ret
  403abd:	0f 1f 00             	nopl   (%rax)

0000000000403ac0 <runtime::mem_free>:
  403ac0:	53                   	push   %rbx
  403ac1:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  403ac8:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  403acd:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  403ad2:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403ad7:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403adc:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403ae1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403ae6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  403aeb:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  403af0:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  403af7:	00 
  403af8:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403afd:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  403b02:	48 83 f8 00          	cmp    $0x0,%rax
  403b06:	0f 94 c0             	sete   %al
  403b09:	24 01                	and    $0x1,%al
  403b0b:	3c 00                	cmp    $0x0,%al
  403b0d:	75 0f                	jne    403b1e <runtime::mem_free+0x5e>
  403b0f:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  403b15:	0f 94 c0             	sete   %al
  403b18:	24 01                	and    $0x1,%al
  403b1a:	3c 00                	cmp    $0x0,%al
  403b1c:	74 0b                	je     403b29 <runtime::mem_free+0x69>
  403b1e:	31 c0                	xor    %eax,%eax
  403b20:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  403b27:	5b                   	pop    %rbx
  403b28:	c3                   	ret
  403b29:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  403b2e:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  403b33:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403b38:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403b3d:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  403b42:	0f 57 c0             	xorps  %xmm0,%xmm0
  403b45:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  403b4a:	be 01 00 00 00       	mov    $0x1,%esi
  403b4f:	31 c9                	xor    %ecx,%ecx
  403b51:	41 89 c9             	mov    %ecx,%r9d
  403b54:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  403b59:	4c 89 ca             	mov    %r9,%rdx
  403b5c:	4c 89 c9             	mov    %r9,%rcx
  403b5f:	48 89 1c 24          	mov    %rbx,(%rsp)
  403b63:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  403b68:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  403b6d:	ff d0                	call   *%rax
  403b6f:	88 44 24 47          	mov    %al,0x47(%rsp)
  403b73:	8a 44 24 47          	mov    0x47(%rsp),%al
  403b77:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  403b7e:	5b                   	pop    %rbx
  403b7f:	c3                   	ret

0000000000403b80 <runtime::print_u64>:
  403b80:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403b87:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403b8c:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403b91:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403b98:	00 
  403b99:	48 8d 7c 24 5f       	lea    0x5f(%rsp),%rdi
  403b9e:	31 f6                	xor    %esi,%esi
  403ba0:	ba 81 00 00 00       	mov    $0x81,%edx
  403ba5:	e8 96 d4 ff ff       	call   401040 <memset@plt>
  403baa:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403baf:	48 c7 44 24 50 81 00 	movq   $0x81,0x50(%rsp)
  403bb6:	00 00 
  403bb8:	48 c7 44 24 48 0a 00 	movq   $0xa,0x48(%rsp)
  403bbf:	00 00 
  403bc1:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403bc6:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403bcb:	48 3b 44 24 48       	cmp    0x48(%rsp),%rax
  403bd0:	0f 93 c0             	setae  %al
  403bd3:	24 01                	and    $0x1,%al
  403bd5:	3c 00                	cmp    $0x0,%al
  403bd7:	74 50                	je     403c29 <runtime::print_u64+0xa9>
  403bd9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403bde:	48 83 e8 01          	sub    $0x1,%rax
  403be2:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403be7:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403bec:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403bf1:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403bf8:	48 8b 08             	mov    (%rax),%rcx
  403bfb:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403c00:	31 d2                	xor    %edx,%edx
  403c02:	48 f7 74 24 48       	divq   0x48(%rsp)
  403c07:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403c0c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403c0f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  403c13:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403c18:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403c1d:	31 d2                	xor    %edx,%edx
  403c1f:	48 f7 f1             	div    %rcx
  403c22:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403c27:	eb 9d                	jmp    403bc6 <runtime::print_u64+0x46>
  403c29:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403c2e:	48 ff c8             	dec    %rax
  403c31:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403c36:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403c3b:	48 89 04 24          	mov    %rax,(%rsp)
  403c3f:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403c46:	48 8b 08             	mov    (%rax),%rcx
  403c49:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403c4e:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403c53:	31 d2                	xor    %edx,%edx
  403c55:	48 f7 f6             	div    %rsi
  403c58:	48 8b 04 24          	mov    (%rsp),%rax
  403c5c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403c5f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  403c63:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  403c68:	48 8d 4c 14 5f       	lea    0x5f(%rsp,%rdx,1),%rcx
  403c6d:	b8 81 00 00 00       	mov    $0x81,%eax
  403c72:	48 29 d0             	sub    %rdx,%rax
  403c75:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403c7a:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403c7f:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403c84:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403c89:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403c90:	00 00 
  403c92:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403c97:	e8 54 e0 ff ff       	call   401cf0 <runtime::stderr_write>
  403c9c:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  403ca3:	c3                   	ret
  403ca4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403cab:	00 00 00 00 00 

0000000000403cb0 <runtime::print_i64>:
  403cb0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403cb7:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403cbc:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403cc1:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403cc8:	00 
  403cc9:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403cd0:	00 
  403cd1:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  403cd8:	00 00 
  403cda:	0f 9c c0             	setl   %al
  403cdd:	24 01                	and    $0x1,%al
  403cdf:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  403ce6:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403ced:	00 
  403cee:	31 c9                	xor    %ecx,%ecx
  403cf0:	48 29 c1             	sub    %rax,%rcx
  403cf3:	48 83 f8 00          	cmp    $0x0,%rax
  403cf7:	48 0f 4c c1          	cmovl  %rcx,%rax
  403cfb:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403d02:	00 
  403d03:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  403d08:	31 f6                	xor    %esi,%esi
  403d0a:	ba 81 00 00 00       	mov    $0x81,%edx
  403d0f:	e8 2c d3 ff ff       	call   401040 <memset@plt>
  403d14:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  403d1b:	00 00 
  403d1d:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  403d24:	00 0a 
  403d26:	0f 9d c0             	setge  %al
  403d29:	24 01                	and    $0x1,%al
  403d2b:	3c 00                	cmp    $0x0,%al
  403d2d:	74 5c                	je     403d8b <runtime::print_i64+0xdb>
  403d2f:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d34:	48 83 e8 01          	sub    $0x1,%rax
  403d38:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403d3d:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d42:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  403d47:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403d4e:	48 8b 08             	mov    (%rax),%rcx
  403d51:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403d58:	00 
  403d59:	be 0a 00 00 00       	mov    $0xa,%esi
  403d5e:	48 99                	cqto
  403d60:	48 f7 fe             	idiv   %rsi
  403d63:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403d68:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403d6b:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  403d6f:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403d76:	00 
  403d77:	b9 0a 00 00 00       	mov    $0xa,%ecx
  403d7c:	48 99                	cqto
  403d7e:	48 f7 f9             	idiv   %rcx
  403d81:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403d88:	00 
  403d89:	eb 92                	jmp    403d1d <runtime::print_i64+0x6d>
  403d8b:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d90:	48 83 e8 01          	sub    $0x1,%rax
  403d94:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403d99:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d9e:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403da3:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403daa:	48 8b 08             	mov    (%rax),%rcx
  403dad:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403db4:	00 
  403db5:	be 0a 00 00 00       	mov    $0xa,%esi
  403dba:	48 99                	cqto
  403dbc:	48 f7 fe             	idiv   %rsi
  403dbf:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403dc4:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403dc7:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  403dcb:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  403dd2:	00 
  403dd3:	74 18                	je     403ded <runtime::print_i64+0x13d>
  403dd5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403dda:	48 83 e8 01          	sub    $0x1,%rax
  403dde:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403de3:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403de8:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  403ded:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  403df2:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  403df7:	b8 81 00 00 00       	mov    $0x81,%eax
  403dfc:	48 29 d0             	sub    %rdx,%rax
  403dff:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403e04:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403e09:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403e0e:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  403e13:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  403e1a:	00 00 
  403e1c:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  403e21:	e8 ca de ff ff       	call   401cf0 <runtime::stderr_write>
  403e26:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  403e2d:	c3                   	ret
  403e2e:	66 90                	xchg   %ax,%ax

0000000000403e30 <runtime::arena_free_last_memory_block>:
  403e30:	48 83 ec 28          	sub    $0x28,%rsp
  403e34:	48 89 3c 24          	mov    %rdi,(%rsp)
  403e38:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403e3d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  403e42:	48 8b 04 24          	mov    (%rsp),%rax
  403e46:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403e4b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e50:	48 8b 40 10          	mov    0x10(%rax),%rax
  403e54:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403e59:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  403e5f:	0f 95 c0             	setne  %al
  403e62:	24 01                	and    $0x1,%al
  403e64:	3c 00                	cmp    $0x0,%al
  403e66:	74 3e                	je     403ea6 <runtime::arena_free_last_memory_block+0x76>
  403e68:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  403e6d:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403e72:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e77:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403e7c:	48 8b 09             	mov    (%rcx),%rcx
  403e7f:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403e83:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e88:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403e8d:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  403e91:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403e95:	48 29 f9             	sub    %rdi,%rcx
  403e98:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403e9c:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403ea1:	e8 ca eb ff ff       	call   402a70 <runtime::memory_block_dealloc>
  403ea6:	48 83 c4 28          	add    $0x28,%rsp
  403eaa:	c3                   	ret
  403eab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403eb0 <runtime::print_caller_location>:
  403eb0:	50                   	push   %rax
  403eb1:	48 89 3c 24          	mov    %rdi,(%rsp)
  403eb5:	eb 00                	jmp    403eb7 <runtime::print_caller_location+0x7>
  403eb7:	48 8b 04 24          	mov    (%rsp),%rax
  403ebb:	48 8b 38             	mov    (%rax),%rdi
  403ebe:	48 8b 70 08          	mov    0x8(%rax),%rsi
  403ec2:	e8 c9 f7 ff ff       	call   403690 <runtime::print_string>
  403ec7:	bf 28 00 00 00       	mov    $0x28,%edi
  403ecc:	e8 ef f9 ff ff       	call   4038c0 <runtime::print_byte>
  403ed1:	48 8b 04 24          	mov    (%rsp),%rax
  403ed5:	48 63 78 10          	movslq 0x10(%rax),%rdi
  403ed9:	e8 a2 fc ff ff       	call   403b80 <runtime::print_u64>
  403ede:	48 8b 04 24          	mov    (%rsp),%rax
  403ee2:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  403ee6:	0f 95 c0             	setne  %al
  403ee9:	24 01                	and    $0x1,%al
  403eeb:	3c 00                	cmp    $0x0,%al
  403eed:	74 17                	je     403f06 <runtime::print_caller_location+0x56>
  403eef:	bf 3a 00 00 00       	mov    $0x3a,%edi
  403ef4:	e8 c7 f9 ff ff       	call   4038c0 <runtime::print_byte>
  403ef9:	48 8b 04 24          	mov    (%rsp),%rax
  403efd:	48 63 78 14          	movslq 0x14(%rax),%rdi
  403f01:	e8 7a fc ff ff       	call   403b80 <runtime::print_u64>
  403f06:	bf 29 00 00 00       	mov    $0x29,%edi
  403f0b:	e8 b0 f9 ff ff       	call   4038c0 <runtime::print_byte>
  403f10:	58                   	pop    %rax
  403f11:	c3                   	ret
  403f12:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403f19:	1f 84 00 00 00 00 00 

0000000000403f20 <runtime::arena_free_all>:
  403f20:	48 83 ec 28          	sub    $0x28,%rsp
  403f24:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403f29:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403f2e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403f33:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403f38:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403f3d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f42:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403f47:	0f 95 c0             	setne  %al
  403f4a:	24 01                	and    $0x1,%al
  403f4c:	3c 00                	cmp    $0x0,%al
  403f4e:	74 2c                	je     403f7c <runtime::arena_free_all+0x5c>
  403f50:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f55:	48 8b 40 10          	mov    0x10(%rax),%rax
  403f59:	48 83 38 00          	cmpq   $0x0,(%rax)
  403f5d:	0f 95 c0             	setne  %al
  403f60:	24 01                	and    $0x1,%al
  403f62:	3c 00                	cmp    $0x0,%al
  403f64:	74 16                	je     403f7c <runtime::arena_free_all+0x5c>
  403f66:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  403f6b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403f70:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403f75:	e8 b6 fe ff ff       	call   403e30 <runtime::arena_free_last_memory_block>
  403f7a:	eb c1                	jmp    403f3d <runtime::arena_free_all+0x1d>
  403f7c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f81:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403f86:	0f 95 c0             	setne  %al
  403f89:	24 01                	and    $0x1,%al
  403f8b:	3c 00                	cmp    $0x0,%al
  403f8d:	74 32                	je     403fc1 <runtime::arena_free_all+0xa1>
  403f8f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f94:	48 8b 40 10          	mov    0x10(%rax),%rax
  403f98:	48 8b 78 18          	mov    0x18(%rax),%rdi
  403f9c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403fa1:	48 8b 40 10          	mov    0x10(%rax),%rax
  403fa5:	48 8b 50 20          	mov    0x20(%rax),%rdx
  403fa9:	31 f6                	xor    %esi,%esi
  403fab:	e8 90 d0 ff ff       	call   401040 <memset@plt>
  403fb0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403fb5:	48 8b 40 10          	mov    0x10(%rax),%rax
  403fb9:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  403fc0:	00 
  403fc1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403fc6:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  403fcd:	00 
  403fce:	48 83 c4 28          	add    $0x28,%rsp
  403fd2:	c3                   	ret
  403fd3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403fda:	84 00 00 00 00 00 

0000000000403fe0 <runtime::arena_destroy>:
  403fe0:	48 83 ec 28          	sub    $0x28,%rsp
  403fe4:	48 89 3c 24          	mov    %rdi,(%rsp)
  403fe8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403fed:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  403ff2:	48 8b 04 24          	mov    (%rsp),%rax
  403ff6:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403ffb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404000:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404005:	0f 95 c0             	setne  %al
  404008:	24 01                	and    $0x1,%al
  40400a:	3c 00                	cmp    $0x0,%al
  40400c:	74 4e                	je     40405c <runtime::arena_destroy+0x7c>
  40400e:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  404013:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404018:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40401d:	48 8b 40 10          	mov    0x10(%rax),%rax
  404021:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404026:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40402b:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404030:	48 8b 09             	mov    (%rcx),%rcx
  404033:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404037:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40403c:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404041:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  404045:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404049:	48 29 f9             	sub    %rdi,%rcx
  40404c:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404050:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  404055:	e8 16 ea ff ff       	call   402a70 <runtime::memory_block_dealloc>
  40405a:	eb 9f                	jmp    403ffb <runtime::arena_destroy+0x1b>
  40405c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404061:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  404068:	00 
  404069:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40406e:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  404075:	00 
  404076:	48 83 c4 28          	add    $0x28,%rsp
  40407a:	c3                   	ret
  40407b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000404080 <runtime::arena_allocator_proc>:
  404080:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  404087:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  40408c:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  404093:	00 
  404094:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40409b:	00 
  40409c:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  4040a3:	00 
  4040a4:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  4040ab:	00 
  4040ac:	40 88 f0             	mov    %sil,%al
  4040af:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  4040b6:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  4040bd:	00 
  4040be:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4040c5:	00 
  4040c6:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  4040cd:	00 
  4040ce:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  4040d5:	00 
  4040d6:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  4040dd:	00 
  4040de:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4040e5:	00 
  4040e6:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  4040ed:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  4040f2:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  4040f9:	00 
  4040fa:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404101:	00 
  404102:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  404109:	00 
  40410a:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  404111:	00 
  404112:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  404119:	00 
  40411a:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  404121:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  404128:	00 
  404129:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  404130:	00 
  404131:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  404138:	00 
  404139:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  404140:	00 
  404141:	0f 57 c0             	xorps  %xmm0,%xmm0
  404144:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  40414b:	00 
  40414c:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404153:	00 
  404154:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  40415b:	00 
  40415c:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  404163:	00 
  404164:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  40416b:	00 
  40416c:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  404173:	00 
  404174:	0f b6 c8             	movzbl %al,%ecx
  404177:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40417c:	2c 07                	sub    $0x7,%al
  40417e:	0f 87 9a 07 00 00    	ja     40491e <runtime::arena_allocator_proc+0x89e>
  404184:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  404189:	48 8b 04 c5 00 71 40 	mov    0x407100(,%rax,8),%rax
  404190:	00 
  404191:	ff e0                	jmp    *%rax
  404193:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  40419a:	00 
  40419b:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4041a2:	00 
  4041a3:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4041aa:	00 
  4041ab:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4041b2:	00 
  4041b3:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4041ba:	00 
  4041bb:	0f 57 c0             	xorps  %xmm0,%xmm0
  4041be:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  4041c5:	00 
  4041c6:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  4041cd:	00 
  4041ce:	e8 8d f0 ff ff       	call   403260 <runtime::arena_alloc>
  4041d3:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4041da:	00 
  4041db:	40 88 c7             	mov    %al,%dil
  4041de:	40 88 f8             	mov    %dil,%al
  4041e1:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4041e8:	00 
  4041e9:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4041f0:	00 
  4041f1:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4041f8:	00 
  4041f9:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404200:	00 
  404201:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404208:	00 
  404209:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40420d:	48 89 11             	mov    %rdx,(%rcx)
  404210:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404217:	c3                   	ret
  404218:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  40421f:	04 
  404220:	e9 f9 06 00 00       	jmp    40491e <runtime::arena_allocator_proc+0x89e>
  404225:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  40422c:	00 
  40422d:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  404234:	00 
  404235:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  40423c:	00 
  40423d:	e8 de fc ff ff       	call   403f20 <runtime::arena_free_all>
  404242:	e9 d7 06 00 00       	jmp    40491e <runtime::arena_allocator_proc+0x89e>
  404247:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40424e:	00 
  40424f:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  404256:	00 
  404257:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  40425e:	00 00 
  404260:	0f 94 c1             	sete   %cl
  404263:	80 e1 01             	and    $0x1,%cl
  404266:	b0 01                	mov    $0x1,%al
  404268:	38 c8                	cmp    %cl,%al
  40426a:	74 25                	je     404291 <runtime::arena_allocator_proc+0x211>
  40426c:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  404273:	00 
  404274:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  40427b:	00 
  40427c:	0f 94 c1             	sete   %cl
  40427f:	80 e1 01             	and    $0x1,%cl
  404282:	b0 01                	mov    $0x1,%al
  404284:	38 c8                	cmp    %cl,%al
  404286:	0f 84 a8 00 00 00    	je     404334 <runtime::arena_allocator_proc+0x2b4>
  40428c:	e9 85 00 00 00       	jmp    404316 <runtime::arena_allocator_proc+0x296>
  404291:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404298:	00 
  404299:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4042a0:	00 
  4042a1:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4042a8:	00 
  4042a9:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4042b0:	00 
  4042b1:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4042b8:	00 
  4042b9:	0f 57 c0             	xorps  %xmm0,%xmm0
  4042bc:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  4042c3:	00 
  4042c4:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  4042cb:	00 
  4042cc:	e8 8f ef ff ff       	call   403260 <runtime::arena_alloc>
  4042d1:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4042d8:	00 
  4042d9:	40 88 c7             	mov    %al,%dil
  4042dc:	40 88 f8             	mov    %dil,%al
  4042df:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  4042e6:	00 
  4042e7:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  4042ee:	00 
  4042ef:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4042f6:	00 
  4042f7:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4042fe:	00 
  4042ff:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404306:	00 
  404307:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40430b:	48 89 11             	mov    %rdx,(%rcx)
  40430e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404315:	c3                   	ret
  404316:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  40431d:	00 00 
  40431f:	0f 94 c1             	sete   %cl
  404322:	80 e1 01             	and    $0x1,%cl
  404325:	b0 01                	mov    $0x1,%al
  404327:	38 c8                	cmp    %cl,%al
  404329:	0f 84 e5 00 00 00    	je     404414 <runtime::arena_allocator_proc+0x394>
  40432f:	e9 b7 00 00 00       	jmp    4043eb <runtime::arena_allocator_proc+0x36b>
  404334:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40433b:	00 
  40433c:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  404341:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404348:	00 
  404349:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  40434e:	bf 50 71 40 00       	mov    $0x407150,%edi
  404353:	31 c0                	xor    %eax,%eax
  404355:	41 89 c0             	mov    %eax,%r8d
  404358:	be 3e 00 00 00       	mov    $0x3e,%esi
  40435d:	ba d1 00 00 00       	mov    $0xd1,%edx
  404362:	b9 13 00 00 00       	mov    $0x13,%ecx
  404367:	e8 94 eb ff ff       	call   402f00 <runtime::multi_pointer_slice_expr_error>
  40436c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  404371:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  404376:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40437d:	00 
  40437e:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  404385:	00 
  404386:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  40438d:	00 
  40438e:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  404395:	00 
  404396:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  40439d:	00 
  40439e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4043a5:	00 
  4043a6:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4043ad:	00 
  4043ae:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4043b5:	00 
  4043b6:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4043bd:	00 
  4043be:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4043c5:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4043cc:	00 
  4043cd:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4043d4:	00 
  4043d5:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4043dc:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4043e0:	48 89 11             	mov    %rdx,(%rcx)
  4043e3:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4043ea:	c3                   	ret
  4043eb:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4043f2:	00 
  4043f3:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  4043fa:	00 
  4043fb:	48 83 e9 01          	sub    $0x1,%rcx
  4043ff:	48 21 c8             	and    %rcx,%rax
  404402:	48 83 f8 00          	cmp    $0x0,%rax
  404406:	0f 94 c1             	sete   %cl
  404409:	80 e1 01             	and    $0x1,%cl
  40440c:	b0 01                	mov    $0x1,%al
  40440e:	38 c8                	cmp    %cl,%al
  404410:	74 54                	je     404466 <runtime::arena_allocator_proc+0x3e6>
  404412:	eb 4d                	jmp    404461 <runtime::arena_allocator_proc+0x3e1>
  404414:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40441b:	00 
  40441c:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404423:	04 
  404424:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  40442b:	00 
  40442c:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404433:	00 
  404434:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40443b:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404442:	00 
  404443:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40444a:	00 
  40444b:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404452:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404456:	48 89 11             	mov    %rdx,(%rcx)
  404459:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404460:	c3                   	ret
  404461:	e9 94 02 00 00       	jmp    4046fa <runtime::arena_allocator_proc+0x67a>
  404466:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  40446d:	00 
  40446e:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  404475:	00 
  404476:	0f 92 c0             	setb   %al
  404479:	24 01                	and    $0x1,%al
  40447b:	3c 00                	cmp    $0x0,%al
  40447d:	0f 84 b7 00 00 00    	je     40453a <runtime::arena_allocator_proc+0x4ba>
  404483:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40448a:	00 
  40448b:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404490:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404497:	00 
  404498:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  40449d:	bf 50 71 40 00       	mov    $0x407150,%edi
  4044a2:	31 c0                	xor    %eax,%eax
  4044a4:	41 89 c0             	mov    %eax,%r8d
  4044a7:	be 3e 00 00 00       	mov    $0x3e,%esi
  4044ac:	ba d9 00 00 00       	mov    $0xd9,%edx
  4044b1:	b9 14 00 00 00       	mov    $0x14,%ecx
  4044b6:	e8 45 ea ff ff       	call   402f00 <runtime::multi_pointer_slice_expr_error>
  4044bb:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4044c0:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4044c5:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4044cc:	00 
  4044cd:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  4044d4:	00 
  4044d5:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  4044dc:	00 
  4044dd:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  4044e4:	00 
  4044e5:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  4044ec:	00 
  4044ed:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4044f4:	00 
  4044f5:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4044fc:	00 
  4044fd:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404504:	00 
  404505:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40450c:	00 
  40450d:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404514:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40451b:	00 
  40451c:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404523:	00 
  404524:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40452b:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40452f:	48 89 11             	mov    %rdx,(%rcx)
  404532:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404539:	c3                   	ret
  40453a:	eb 00                	jmp    40453c <runtime::arena_allocator_proc+0x4bc>
  40453c:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  404543:	00 
  404544:	48 8b 40 10          	mov    0x10(%rax),%rax
  404548:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40454f:	00 
  404550:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  404557:	00 00 
  404559:	0f 95 c0             	setne  %al
  40455c:	24 01                	and    $0x1,%al
  40455e:	3c 00                	cmp    $0x0,%al
  404560:	0f 84 92 01 00 00    	je     4046f8 <runtime::arena_allocator_proc+0x678>
  404566:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40456d:	00 
  40456e:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404575:	00 
  404576:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  40457a:	48 29 c8             	sub    %rcx,%rax
  40457d:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  404584:	00 
  404585:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40458c:	00 
  40458d:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  404594:	00 
  404595:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  40459c:	00 
  40459d:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  4045a4:	00 
  4045a5:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  4045ac:	00 
  4045ad:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  4045b4:	00 
  4045b5:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  4045bc:	00 
  4045bd:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  4045c4:	00 
  4045c5:	0f 92 c0             	setb   %al
  4045c8:	24 01                	and    $0x1,%al
  4045ca:	3c 00                	cmp    $0x0,%al
  4045cc:	0f 84 24 01 00 00    	je     4046f6 <runtime::arena_allocator_proc+0x676>
  4045d2:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4045d9:	00 
  4045da:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  4045e1:	00 
  4045e2:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  4045e6:	0f 94 c0             	sete   %al
  4045e9:	24 01                	and    $0x1,%al
  4045eb:	3c 00                	cmp    $0x0,%al
  4045ed:	0f 84 03 01 00 00    	je     4046f6 <runtime::arena_allocator_proc+0x676>
  4045f3:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  4045fa:	00 
  4045fb:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404602:	00 
  404603:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  404607:	0f 96 c0             	setbe  %al
  40460a:	24 01                	and    $0x1,%al
  40460c:	3c 00                	cmp    $0x0,%al
  40460e:	0f 84 e2 00 00 00    	je     4046f6 <runtime::arena_allocator_proc+0x676>
  404614:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40461b:	00 
  40461c:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  404623:	00 
  404624:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404628:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40462f:	00 
  404630:	48 8b 40 18          	mov    0x18(%rax),%rax
  404634:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404639:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  404640:	00 
  404641:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  404646:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  40464d:	00 
  40464e:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  404653:	bf 50 71 40 00       	mov    $0x407150,%edi
  404658:	be 3e 00 00 00       	mov    $0x3e,%esi
  40465d:	ba e4 00 00 00       	mov    $0xe4,%edx
  404662:	b9 17 00 00 00       	mov    $0x17,%ecx
  404667:	e8 94 e8 ff ff       	call   402f00 <runtime::multi_pointer_slice_expr_error>
  40466c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404671:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404676:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40467b:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404682:	00 
  404683:	48 01 f2             	add    %rsi,%rdx
  404686:	48 29 f0             	sub    %rsi,%rax
  404689:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  404690:	00 
  404691:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  404698:	00 
  404699:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  4046a0:	00 
  4046a1:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  4046a8:	00 
  4046a9:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4046b0:	00 
  4046b1:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4046b8:	00 
  4046b9:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4046c0:	00 
  4046c1:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4046c8:	00 
  4046c9:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4046d0:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4046d7:	00 
  4046d8:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4046df:	00 
  4046e0:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4046e7:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4046eb:	48 89 11             	mov    %rdx,(%rcx)
  4046ee:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4046f5:	c3                   	ret
  4046f6:	eb 00                	jmp    4046f8 <runtime::arena_allocator_proc+0x678>
  4046f8:	eb 00                	jmp    4046fa <runtime::arena_allocator_proc+0x67a>
  4046fa:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404701:	00 
  404702:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404709:	00 
  40470a:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404711:	00 
  404712:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404719:	00 
  40471a:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404721:	00 
  404722:	0f 57 c0             	xorps  %xmm0,%xmm0
  404725:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40472c:	00 
  40472d:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  404734:	00 
  404735:	e8 26 eb ff ff       	call   403260 <runtime::arena_alloc>
  40473a:	88 44 24 27          	mov    %al,0x27(%rsp)
  40473e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  404745:	00 
  404746:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40474b:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  404752:	00 
  404753:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404758:	3c 00                	cmp    $0x0,%al
  40475a:	74 50                	je     4047ac <runtime::arena_allocator_proc+0x72c>
  40475c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404763:	00 
  404764:	8a 44 24 27          	mov    0x27(%rsp),%al
  404768:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40476f:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404776:	00 
  404777:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40477e:	00 
  40477f:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404786:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40478d:	00 
  40478e:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404795:	00 
  404796:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40479d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4047a1:	48 89 11             	mov    %rdx,(%rcx)
  4047a4:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4047ab:	c3                   	ret
  4047ac:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4047b1:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4047b6:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  4047bd:	00 
  4047be:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4047c5:	00 
  4047c6:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  4047cd:	00 00 
  4047cf:	0f 94 c0             	sete   %al
  4047d2:	24 01                	and    $0x1,%al
  4047d4:	3c 00                	cmp    $0x0,%al
  4047d6:	74 45                	je     40481d <runtime::arena_allocator_proc+0x79d>
  4047d8:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4047df:	00 
  4047e0:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4047e7:	00 
  4047e8:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4047ef:	00 
  4047f0:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4047f7:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4047fe:	00 
  4047ff:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404806:	00 
  404807:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40480e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404812:	48 89 11             	mov    %rdx,(%rcx)
  404815:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40481c:	c3                   	ret
  40481d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404824:	00 
  404825:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40482a:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  404831:	00 
  404832:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404837:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40483e:	00 
  40483f:	48 89 04 24          	mov    %rax,(%rsp)
  404843:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  40484a:	00 
  40484b:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  404850:	bf 50 71 40 00       	mov    $0x407150,%edi
  404855:	31 c0                	xor    %eax,%eax
  404857:	41 89 c0             	mov    %eax,%r8d
  40485a:	be 3e 00 00 00       	mov    $0x3e,%esi
  40485f:	ba ee 00 00 00       	mov    $0xee,%edx
  404864:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  404869:	e8 92 e6 ff ff       	call   402f00 <runtime::multi_pointer_slice_expr_error>
  40486e:	48 8b 0c 24          	mov    (%rsp),%rcx
  404872:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404877:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40487c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404881:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  404888:	00 
  404889:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404890:	00 
  404891:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  404898:	00 
  404899:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  4048a0:	00 
  4048a1:	e8 8a ef ff ff       	call   403830 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  4048a6:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4048ad:	00 
  4048ae:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  4048b5:	00 
  4048b6:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4048bd:	00 
  4048be:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4048c5:	00 
  4048c6:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  4048cd:	00 
  4048ce:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  4048d5:	00 
  4048d6:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4048da:	48 89 08             	mov    %rcx,(%rax)
  4048dd:	31 c0                	xor    %eax,%eax
  4048df:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4048e6:	c3                   	ret
  4048e7:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4048ee:	00 
  4048ef:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4048f6:	00 
  4048f7:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  4048fe:	00 00 
  404900:	0f 95 c0             	setne  %al
  404903:	24 01                	and    $0x1,%al
  404905:	3c 00                	cmp    $0x0,%al
  404907:	74 0b                	je     404914 <runtime::arena_allocator_proc+0x894>
  404909:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404910:	00 
  404911:	c6 00 5d             	movb   $0x5d,(%rax)
  404914:	eb 08                	jmp    40491e <runtime::arena_allocator_proc+0x89e>
  404916:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  40491d:	04 
  40491e:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404925:	00 
  404926:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  40492d:	00 
  40492e:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404935:	00 
  404936:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40493d:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404944:	00 
  404945:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40494c:	00 
  40494d:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404954:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404958:	48 89 11             	mov    %rdx,(%rcx)
  40495b:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404962:	c3                   	ret
  404963:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40496a:	84 00 00 00 00 00 

0000000000404970 <runtime::memory_equal>:
  404970:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  404975:	48 89 74 24 b8       	mov    %rsi,-0x48(%rsp)
  40497a:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  40497f:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404984:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404989:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  40498e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404993:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404998:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40499d:	48 83 f8 00          	cmp    $0x0,%rax
  4049a1:	0f 94 c1             	sete   %cl
  4049a4:	80 e1 01             	and    $0x1,%cl
  4049a7:	b0 01                	mov    $0x1,%al
  4049a9:	38 c8                	cmp    %cl,%al
  4049ab:	74 1b                	je     4049c8 <runtime::memory_equal+0x58>
  4049ad:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4049b2:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  4049b7:	48 39 c8             	cmp    %rcx,%rax
  4049ba:	0f 94 c1             	sete   %cl
  4049bd:	80 e1 01             	and    $0x1,%cl
  4049c0:	b0 01                	mov    $0x1,%al
  4049c2:	38 c8                	cmp    %cl,%al
  4049c4:	74 07                	je     4049cd <runtime::memory_equal+0x5d>
  4049c6:	eb 03                	jmp    4049cb <runtime::memory_equal+0x5b>
  4049c8:	b0 01                	mov    $0x1,%al
  4049ca:	c3                   	ret
  4049cb:	eb 03                	jmp    4049d0 <runtime::memory_equal+0x60>
  4049cd:	b0 01                	mov    $0x1,%al
  4049cf:	c3                   	ret
  4049d0:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4049d5:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  4049da:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  4049df:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  4049e4:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4049e9:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4049ee:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  4049f5:	00 00 
  4049f7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4049fc:	48 3b 44 24 d0       	cmp    -0x30(%rsp),%rax
  404a01:	0f 92 c0             	setb   %al
  404a04:	24 01                	and    $0x1,%al
  404a06:	3c 00                	cmp    $0x0,%al
  404a08:	74 38                	je     404a42 <runtime::memory_equal+0xd2>
  404a0a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404a0f:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404a14:	8a 04 08             	mov    (%rax,%rcx,1),%al
  404a17:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  404a1c:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  404a21:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  404a24:	0f 95 c0             	setne  %al
  404a27:	24 01                	and    $0x1,%al
  404a29:	3c 00                	cmp    $0x0,%al
  404a2b:	74 03                	je     404a30 <runtime::memory_equal+0xc0>
  404a2d:	31 c0                	xor    %eax,%eax
  404a2f:	c3                   	ret
  404a30:	eb 00                	jmp    404a32 <runtime::memory_equal+0xc2>
  404a32:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404a37:	48 83 c0 01          	add    $0x1,%rax
  404a3b:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404a40:	eb b5                	jmp    4049f7 <runtime::memory_equal+0x87>
  404a42:	b0 01                	mov    $0x1,%al
  404a44:	c3                   	ret
  404a45:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  404a4c:	00 00 00 00 

0000000000404a50 <runtime::memory_compare>:
  404a50:	48 83 ec 10          	sub    $0x10,%rsp
  404a54:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  404a59:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  404a5e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  404a63:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404a68:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  404a6d:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  404a72:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404a77:	48 89 0c 24          	mov    %rcx,(%rsp)
  404a7b:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404a80:	48 39 c8             	cmp    %rcx,%rax
  404a83:	0f 94 c1             	sete   %cl
  404a86:	80 e1 01             	and    $0x1,%cl
  404a89:	b0 01                	mov    $0x1,%al
  404a8b:	38 c8                	cmp    %cl,%al
  404a8d:	74 17                	je     404aa6 <runtime::memory_compare+0x56>
  404a8f:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404a94:	48 83 f8 00          	cmp    $0x0,%rax
  404a98:	0f 94 c1             	sete   %cl
  404a9b:	80 e1 01             	and    $0x1,%cl
  404a9e:	b0 01                	mov    $0x1,%al
  404aa0:	38 c8                	cmp    %cl,%al
  404aa2:	74 20                	je     404ac4 <runtime::memory_compare+0x74>
  404aa4:	eb 07                	jmp    404aad <runtime::memory_compare+0x5d>
  404aa6:	31 c0                	xor    %eax,%eax
  404aa8:	48 83 c4 10          	add    $0x10,%rsp
  404aac:	c3                   	ret
  404aad:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  404ab2:	48 83 f8 00          	cmp    $0x0,%rax
  404ab6:	0f 94 c1             	sete   %cl
  404ab9:	80 e1 01             	and    $0x1,%cl
  404abc:	b0 01                	mov    $0x1,%al
  404abe:	38 c8                	cmp    %cl,%al
  404ac0:	74 10                	je     404ad2 <runtime::memory_compare+0x82>
  404ac2:	eb 0c                	jmp    404ad0 <runtime::memory_compare+0x80>
  404ac4:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404acb:	48 83 c4 10          	add    $0x10,%rsp
  404acf:	c3                   	ret
  404ad0:	eb 0a                	jmp    404adc <runtime::memory_compare+0x8c>
  404ad2:	b8 01 00 00 00       	mov    $0x1,%eax
  404ad7:	48 83 c4 10          	add    $0x10,%rsp
  404adb:	c3                   	ret
  404adc:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  404ae1:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  404ae6:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  404aeb:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  404af0:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  404af5:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404afa:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404aff:	48 c1 e8 03          	shr    $0x3,%rax
  404b03:	48 83 c0 01          	add    $0x1,%rax
  404b07:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404b0c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404b11:	48 83 e8 01          	sub    $0x1,%rax
  404b15:	48 c1 e0 03          	shl    $0x3,%rax
  404b19:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404b1e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  404b25:	00 00 
  404b27:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  404b2d:	0f 92 c0             	setb   %al
  404b30:	24 01                	and    $0x1,%al
  404b32:	3c 00                	cmp    $0x0,%al
  404b34:	74 09                	je     404b3f <runtime::memory_compare+0xef>
  404b36:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404b3d:	00 00 
  404b3f:	eb 00                	jmp    404b41 <runtime::memory_compare+0xf1>
  404b41:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404b46:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  404b4b:	0f 92 c0             	setb   %al
  404b4e:	24 01                	and    $0x1,%al
  404b50:	3c 00                	cmp    $0x0,%al
  404b52:	0f 84 11 01 00 00    	je     404c69 <runtime::memory_compare+0x219>
  404b58:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404b5d:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404b62:	48 c1 e1 03          	shl    $0x3,%rcx
  404b66:	48 01 c8             	add    %rcx,%rax
  404b69:	48 8b 00             	mov    (%rax),%rax
  404b6c:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404b71:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404b76:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404b7b:	48 c1 e1 03          	shl    $0x3,%rcx
  404b7f:	48 01 c8             	add    %rcx,%rax
  404b82:	48 8b 00             	mov    (%rax),%rax
  404b85:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404b8a:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404b8f:	48 33 44 24 b8       	xor    -0x48(%rsp),%rax
  404b94:	48 83 f8 00          	cmp    $0x0,%rax
  404b98:	0f 95 c0             	setne  %al
  404b9b:	24 01                	and    $0x1,%al
  404b9d:	3c 00                	cmp    $0x0,%al
  404b9f:	0f 84 af 00 00 00    	je     404c54 <runtime::memory_compare+0x204>
  404ba5:	eb 00                	jmp    404ba7 <runtime::memory_compare+0x157>
  404ba7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404bac:	48 c1 e0 03          	shl    $0x3,%rax
  404bb0:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  404bb5:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404bba:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404bbf:	0f 92 c0             	setb   %al
  404bc2:	24 01                	and    $0x1,%al
  404bc4:	3c 00                	cmp    $0x0,%al
  404bc6:	0f 84 86 00 00 00    	je     404c52 <runtime::memory_compare+0x202>
  404bcc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404bd1:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  404bd6:	8a 00                	mov    (%rax),%al
  404bd8:	88 44 24 af          	mov    %al,-0x51(%rsp)
  404bdc:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404be1:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  404be6:	8a 00                	mov    (%rax),%al
  404be8:	88 44 24 ae          	mov    %al,-0x52(%rsp)
  404bec:	8a 44 24 af          	mov    -0x51(%rsp),%al
  404bf0:	32 44 24 ae          	xor    -0x52(%rsp),%al
  404bf4:	3c 00                	cmp    $0x0,%al
  404bf6:	0f 95 c0             	setne  %al
  404bf9:	24 01                	and    $0x1,%al
  404bfb:	3c 00                	cmp    $0x0,%al
  404bfd:	74 3e                	je     404c3d <runtime::memory_compare+0x1ed>
  404bff:	0f b6 44 24 af       	movzbl -0x51(%rsp),%eax
  404c04:	0f b6 4c 24 ae       	movzbl -0x52(%rsp),%ecx
  404c09:	48 29 c8             	sub    %rcx,%rax
  404c0c:	48 83 f8 00          	cmp    $0x0,%rax
  404c10:	0f 9c c0             	setl   %al
  404c13:	24 01                	and    $0x1,%al
  404c15:	3c 00                	cmp    $0x0,%al
  404c17:	74 0e                	je     404c27 <runtime::memory_compare+0x1d7>
  404c19:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404c20:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  404c25:	eb 0c                	jmp    404c33 <runtime::memory_compare+0x1e3>
  404c27:	b8 01 00 00 00       	mov    $0x1,%eax
  404c2c:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  404c31:	eb 00                	jmp    404c33 <runtime::memory_compare+0x1e3>
  404c33:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  404c38:	48 83 c4 10          	add    $0x10,%rsp
  404c3c:	c3                   	ret
  404c3d:	eb 00                	jmp    404c3f <runtime::memory_compare+0x1ef>
  404c3f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404c44:	48 83 c0 01          	add    $0x1,%rax
  404c48:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  404c4d:	e9 63 ff ff ff       	jmp    404bb5 <runtime::memory_compare+0x165>
  404c52:	eb 00                	jmp    404c54 <runtime::memory_compare+0x204>
  404c54:	eb 00                	jmp    404c56 <runtime::memory_compare+0x206>
  404c56:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404c5b:	48 83 c0 01          	add    $0x1,%rax
  404c5f:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404c64:	e9 d8 fe ff ff       	jmp    404b41 <runtime::memory_compare+0xf1>
  404c69:	eb 00                	jmp    404c6b <runtime::memory_compare+0x21b>
  404c6b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404c70:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404c75:	0f 92 c0             	setb   %al
  404c78:	24 01                	and    $0x1,%al
  404c7a:	3c 00                	cmp    $0x0,%al
  404c7c:	0f 84 86 00 00 00    	je     404d08 <runtime::memory_compare+0x2b8>
  404c82:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404c87:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404c8c:	8a 00                	mov    (%rax),%al
  404c8e:	88 44 24 ad          	mov    %al,-0x53(%rsp)
  404c92:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404c97:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404c9c:	8a 00                	mov    (%rax),%al
  404c9e:	88 44 24 ac          	mov    %al,-0x54(%rsp)
  404ca2:	8a 44 24 ad          	mov    -0x53(%rsp),%al
  404ca6:	32 44 24 ac          	xor    -0x54(%rsp),%al
  404caa:	3c 00                	cmp    $0x0,%al
  404cac:	0f 95 c0             	setne  %al
  404caf:	24 01                	and    $0x1,%al
  404cb1:	3c 00                	cmp    $0x0,%al
  404cb3:	74 3e                	je     404cf3 <runtime::memory_compare+0x2a3>
  404cb5:	0f b6 44 24 ad       	movzbl -0x53(%rsp),%eax
  404cba:	0f b6 4c 24 ac       	movzbl -0x54(%rsp),%ecx
  404cbf:	48 29 c8             	sub    %rcx,%rax
  404cc2:	48 83 f8 00          	cmp    $0x0,%rax
  404cc6:	0f 9c c0             	setl   %al
  404cc9:	24 01                	and    $0x1,%al
  404ccb:	3c 00                	cmp    $0x0,%al
  404ccd:	74 0e                	je     404cdd <runtime::memory_compare+0x28d>
  404ccf:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404cd6:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  404cdb:	eb 0c                	jmp    404ce9 <runtime::memory_compare+0x299>
  404cdd:	b8 01 00 00 00       	mov    $0x1,%eax
  404ce2:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  404ce7:	eb 00                	jmp    404ce9 <runtime::memory_compare+0x299>
  404ce9:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  404cee:	48 83 c4 10          	add    $0x10,%rsp
  404cf2:	c3                   	ret
  404cf3:	eb 00                	jmp    404cf5 <runtime::memory_compare+0x2a5>
  404cf5:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404cfa:	48 83 c0 01          	add    $0x1,%rax
  404cfe:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404d03:	e9 63 ff ff ff       	jmp    404c6b <runtime::memory_compare+0x21b>
  404d08:	31 c0                	xor    %eax,%eax
  404d0a:	48 83 c4 10          	add    $0x10,%rsp
  404d0e:	c3                   	ret
  404d0f:	90                   	nop

0000000000404d10 <runtime::memory_compare_zero>:
  404d10:	48 89 7c 24 a0       	mov    %rdi,-0x60(%rsp)
  404d15:	48 89 74 24 a8       	mov    %rsi,-0x58(%rsp)
  404d1a:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  404d1f:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  404d24:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404d29:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  404d2e:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  404d33:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404d38:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404d3d:	48 c1 e8 03          	shr    $0x3,%rax
  404d41:	48 83 c0 01          	add    $0x1,%rax
  404d45:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404d4a:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404d4f:	48 83 e8 01          	sub    $0x1,%rax
  404d53:	48 c1 e0 03          	shl    $0x3,%rax
  404d57:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404d5c:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  404d63:	00 00 
  404d65:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  404d6b:	0f 92 c0             	setb   %al
  404d6e:	24 01                	and    $0x1,%al
  404d70:	3c 00                	cmp    $0x0,%al
  404d72:	74 09                	je     404d7d <runtime::memory_compare_zero+0x6d>
  404d74:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404d7b:	00 00 
  404d7d:	eb 00                	jmp    404d7f <runtime::memory_compare_zero+0x6f>
  404d7f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404d84:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  404d89:	0f 92 c0             	setb   %al
  404d8c:	24 01                	and    $0x1,%al
  404d8e:	3c 00                	cmp    $0x0,%al
  404d90:	0f 84 d2 00 00 00    	je     404e68 <runtime::memory_compare_zero+0x158>
  404d96:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404d9b:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404da0:	48 c1 e1 03          	shl    $0x3,%rcx
  404da4:	48 01 c8             	add    %rcx,%rax
  404da7:	48 8b 00             	mov    (%rax),%rax
  404daa:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404daf:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404db4:	48 83 f0 00          	xor    $0x0,%rax
  404db8:	48 83 f8 00          	cmp    $0x0,%rax
  404dbc:	0f 95 c0             	setne  %al
  404dbf:	24 01                	and    $0x1,%al
  404dc1:	3c 00                	cmp    $0x0,%al
  404dc3:	0f 84 8a 00 00 00    	je     404e53 <runtime::memory_compare_zero+0x143>
  404dc9:	eb 00                	jmp    404dcb <runtime::memory_compare_zero+0xbb>
  404dcb:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404dd0:	48 c1 e0 03          	shl    $0x3,%rax
  404dd4:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404dd9:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  404dde:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404de3:	0f 92 c0             	setb   %al
  404de6:	24 01                	and    $0x1,%al
  404de8:	3c 00                	cmp    $0x0,%al
  404dea:	74 65                	je     404e51 <runtime::memory_compare_zero+0x141>
  404dec:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404df1:	48 03 44 24 b8       	add    -0x48(%rsp),%rax
  404df6:	8a 00                	mov    (%rax),%al
  404df8:	88 44 24 b7          	mov    %al,-0x49(%rsp)
  404dfc:	8a 44 24 b7          	mov    -0x49(%rsp),%al
  404e00:	34 00                	xor    $0x0,%al
  404e02:	3c 00                	cmp    $0x0,%al
  404e04:	0f 95 c0             	setne  %al
  404e07:	24 01                	and    $0x1,%al
  404e09:	3c 00                	cmp    $0x0,%al
  404e0b:	74 32                	je     404e3f <runtime::memory_compare_zero+0x12f>
  404e0d:	0f b6 44 24 b7       	movzbl -0x49(%rsp),%eax
  404e12:	48 83 f8 00          	cmp    $0x0,%rax
  404e16:	0f 9c c0             	setl   %al
  404e19:	24 01                	and    $0x1,%al
  404e1b:	3c 00                	cmp    $0x0,%al
  404e1d:	74 0e                	je     404e2d <runtime::memory_compare_zero+0x11d>
  404e1f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404e26:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  404e2b:	eb 0c                	jmp    404e39 <runtime::memory_compare_zero+0x129>
  404e2d:	b8 01 00 00 00       	mov    $0x1,%eax
  404e32:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  404e37:	eb 00                	jmp    404e39 <runtime::memory_compare_zero+0x129>
  404e39:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  404e3e:	c3                   	ret
  404e3f:	eb 00                	jmp    404e41 <runtime::memory_compare_zero+0x131>
  404e41:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  404e46:	48 83 c0 01          	add    $0x1,%rax
  404e4a:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404e4f:	eb 88                	jmp    404dd9 <runtime::memory_compare_zero+0xc9>
  404e51:	eb 00                	jmp    404e53 <runtime::memory_compare_zero+0x143>
  404e53:	eb 00                	jmp    404e55 <runtime::memory_compare_zero+0x145>
  404e55:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404e5a:	48 83 c0 01          	add    $0x1,%rax
  404e5e:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404e63:	e9 17 ff ff ff       	jmp    404d7f <runtime::memory_compare_zero+0x6f>
  404e68:	eb 00                	jmp    404e6a <runtime::memory_compare_zero+0x15a>
  404e6a:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404e6f:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404e74:	0f 92 c0             	setb   %al
  404e77:	24 01                	and    $0x1,%al
  404e79:	3c 00                	cmp    $0x0,%al
  404e7b:	74 65                	je     404ee2 <runtime::memory_compare_zero+0x1d2>
  404e7d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404e82:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404e87:	8a 00                	mov    (%rax),%al
  404e89:	88 44 24 b6          	mov    %al,-0x4a(%rsp)
  404e8d:	8a 44 24 b6          	mov    -0x4a(%rsp),%al
  404e91:	34 00                	xor    $0x0,%al
  404e93:	3c 00                	cmp    $0x0,%al
  404e95:	0f 95 c0             	setne  %al
  404e98:	24 01                	and    $0x1,%al
  404e9a:	3c 00                	cmp    $0x0,%al
  404e9c:	74 32                	je     404ed0 <runtime::memory_compare_zero+0x1c0>
  404e9e:	0f b6 44 24 b6       	movzbl -0x4a(%rsp),%eax
  404ea3:	48 83 f8 00          	cmp    $0x0,%rax
  404ea7:	0f 9c c0             	setl   %al
  404eaa:	24 01                	and    $0x1,%al
  404eac:	3c 00                	cmp    $0x0,%al
  404eae:	74 0e                	je     404ebe <runtime::memory_compare_zero+0x1ae>
  404eb0:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404eb7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  404ebc:	eb 0c                	jmp    404eca <runtime::memory_compare_zero+0x1ba>
  404ebe:	b8 01 00 00 00       	mov    $0x1,%eax
  404ec3:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  404ec8:	eb 00                	jmp    404eca <runtime::memory_compare_zero+0x1ba>
  404eca:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404ecf:	c3                   	ret
  404ed0:	eb 00                	jmp    404ed2 <runtime::memory_compare_zero+0x1c2>
  404ed2:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404ed7:	48 83 c0 01          	add    $0x1,%rax
  404edb:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404ee0:	eb 88                	jmp    404e6a <runtime::memory_compare_zero+0x15a>
  404ee2:	31 c0                	xor    %eax,%eax
  404ee4:	c3                   	ret
  404ee5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  404eec:	00 00 00 00 

0000000000404ef0 <runtime::__type_info_of>:
  404ef0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  404ef5:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404efa:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404eff:	48 c7 c1 18 70 40 00 	mov    $0x407018,%rcx
  404f06:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  404f0a:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404f0f:	31 c9                	xor    %ecx,%ecx
  404f11:	89 ca                	mov    %ecx,%edx
  404f13:	48 f7 74 24 f0       	divq   -0x10(%rsp)
  404f18:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  404f1d:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  404f24:	00 00 
  404f26:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404f2d:	00 00 
  404f2f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404f34:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  404f39:	0f 83 9f 00 00 00    	jae    404fde <runtime::__type_info_of+0xee>
  404f3f:	48 c7 c0 18 70 40 00 	mov    $0x407018,%rax
  404f46:	48 8b 00             	mov    (%rax),%rax
  404f49:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  404f4e:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  404f52:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404f57:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  404f5d:	0f 95 c0             	setne  %al
  404f60:	24 01                	and    $0x1,%al
  404f62:	3c 00                	cmp    $0x0,%al
  404f64:	74 1d                	je     404f83 <runtime::__type_info_of+0x93>
  404f66:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404f6b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404f70:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  404f74:	0f 94 c0             	sete   %al
  404f77:	24 01                	and    $0x1,%al
  404f79:	3c 00                	cmp    $0x0,%al
  404f7b:	74 06                	je     404f83 <runtime::__type_info_of+0x93>
  404f7d:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404f82:	c3                   	ret
  404f83:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404f88:	48 83 c0 01          	add    $0x1,%rax
  404f8c:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  404f91:	0f 92 c0             	setb   %al
  404f94:	24 01                	and    $0x1,%al
  404f96:	3c 00                	cmp    $0x0,%al
  404f98:	74 10                	je     404faa <runtime::__type_info_of+0xba>
  404f9a:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404f9f:	48 83 c0 01          	add    $0x1,%rax
  404fa3:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404fa8:	eb 09                	jmp    404fb3 <runtime::__type_info_of+0xc3>
  404faa:	31 c0                	xor    %eax,%eax
  404fac:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404fb1:	eb 00                	jmp    404fb3 <runtime::__type_info_of+0xc3>
  404fb3:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404fb8:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  404fbd:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404fc2:	48 83 c0 01          	add    $0x1,%rax
  404fc6:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404fcb:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404fd0:	48 83 c0 01          	add    $0x1,%rax
  404fd4:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404fd9:	e9 51 ff ff ff       	jmp    404f2f <runtime::__type_info_of+0x3f>
  404fde:	48 c7 c0 18 70 40 00 	mov    $0x407018,%rax
  404fe5:	48 8b 00             	mov    (%rax),%rax
  404fe8:	48 8b 00             	mov    (%rax),%rax
  404feb:	c3                   	ret
  404fec:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000404ff0 <runtime::default_logger_proc>:
  404ff0:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  404ff5:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  404ffa:	66 44 89 c0          	mov    %r8w,%ax
  404ffe:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  405003:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  405008:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40500d:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  405012:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  405017:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40501c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  405021:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  405026:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  40502b:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  405030:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  405035:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40503a:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  40503f:	c3                   	ret

0000000000405040 <runtime::default_context>:
  405040:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  405047:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40504c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  405051:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405056:	31 f6                	xor    %esi,%esi
  405058:	ba 70 00 00 00       	mov    $0x70,%edx
  40505d:	e8 de bf ff ff       	call   401040 <memset@plt>
  405062:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405067:	e8 24 00 00 00       	call   405090 <runtime::[core.odin]::__init_context>
  40506c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  405071:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  405076:	ba 70 00 00 00       	mov    $0x70,%edx
  40507b:	e8 e0 bf ff ff       	call   401060 <memcpy@plt>
  405080:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405085:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40508c:	c3                   	ret
  40508d:	0f 1f 00             	nopl   (%rax)

0000000000405090 <runtime::[core.odin]::__init_context>:
  405090:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  405095:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40509a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40509f:	48 83 f8 00          	cmp    $0x0,%rax
  4050a3:	0f 94 c0             	sete   %al
  4050a6:	24 01                	and    $0x1,%al
  4050a8:	3c 00                	cmp    $0x0,%al
  4050aa:	74 01                	je     4050ad <runtime::[core.odin]::__init_context+0x1d>
  4050ac:	c3                   	ret
  4050ad:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050b2:	48 c7 c1 c0 1d 40 00 	mov    $0x401dc0,%rcx
  4050b9:	48 89 08             	mov    %rcx,(%rax)
  4050bc:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050c1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4050c8:	00 
  4050c9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050ce:	48 c7 c1 50 28 40 00 	mov    $0x402850,%rcx
  4050d5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4050d9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050de:	48 c7 c2 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rdx
  4050e5:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  4050ec:	00 00 
  4050ee:	48 01 d1             	add    %rdx,%rcx
  4050f1:	48 89 48 18          	mov    %rcx,0x18(%rax)
  4050f5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050fa:	48 c7 c1 40 51 40 00 	mov    $0x405140,%rcx
  405101:	48 89 48 20          	mov    %rcx,0x20(%rax)
  405105:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40510a:	48 c7 c1 f0 4f 40 00 	mov    $0x404ff0,%rcx
  405111:	48 89 48 28          	mov    %rcx,0x28(%rax)
  405115:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40511a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  405121:	00 
  405122:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405127:	48 c7 c1 10 25 40 00 	mov    $0x402510,%rcx
  40512e:	48 89 48 48          	mov    %rcx,0x48(%rax)
  405132:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405137:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  40513e:	00 
  40513f:	c3                   	ret

0000000000405140 <runtime::default_assertion_failure_proc>:
  405140:	48 83 ec 48          	sub    $0x48,%rsp
  405144:	4c 89 04 24          	mov    %r8,(%rsp)
  405148:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40514d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405152:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405157:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40515c:	4c 8b 04 24          	mov    (%rsp),%r8
  405160:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405165:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40516a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40516f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405174:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405179:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40517e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405183:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  405188:	e8 03 00 00 00       	call   405190 <runtime::default_assertion_contextless_failure_proc>
  40518d:	0f 1f 00             	nopl   (%rax)

0000000000405190 <runtime::default_assertion_contextless_failure_proc>:
  405190:	48 83 ec 48          	sub    $0x48,%rsp
  405194:	4c 89 04 24          	mov    %r8,(%rsp)
  405198:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40519d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4051a2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4051a7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4051ac:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4051b1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4051b6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4051bb:	48 8b 3c 24          	mov    (%rsp),%rdi
  4051bf:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4051c4:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4051c9:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4051ce:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4051d3:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4051d8:	e8 d3 ec ff ff       	call   403eb0 <runtime::print_caller_location>
  4051dd:	bf 42 73 40 00       	mov    $0x407342,%edi
  4051e2:	be 01 00 00 00       	mov    $0x1,%esi
  4051e7:	e8 a4 e4 ff ff       	call   403690 <runtime::print_string>
  4051ec:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4051f1:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4051f6:	e8 95 e4 ff ff       	call   403690 <runtime::print_string>
  4051fb:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405200:	48 83 f8 00          	cmp    $0x0,%rax
  405204:	0f 9f c0             	setg   %al
  405207:	24 01                	and    $0x1,%al
  405209:	3c 00                	cmp    $0x0,%al
  40520b:	74 1e                	je     40522b <runtime::default_assertion_contextless_failure_proc+0x9b>
  40520d:	bf 44 73 40 00       	mov    $0x407344,%edi
  405212:	be 02 00 00 00       	mov    $0x2,%esi
  405217:	e8 74 e4 ff ff       	call   403690 <runtime::print_string>
  40521c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  405221:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  405226:	e8 65 e4 ff ff       	call   403690 <runtime::print_string>
  40522b:	bf 0a 00 00 00       	mov    $0xa,%edi
  405230:	e8 8b e6 ff ff       	call   4038c0 <runtime::print_byte>
  405235:	0f 0b                	ud2
  405237:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40523e:	00 00 

0000000000405240 <__truncsfhf2>:
  405240:	48 83 ec 18          	sub    $0x18,%rsp
  405244:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  40524a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  405250:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  405256:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40525d:	00 
  40525e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  405265:	00 
  405266:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40526d:	00 
  40526e:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  405275:	00 
  405276:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  40527d:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  405283:	8b 44 24 10          	mov    0x10(%rsp),%eax
  405287:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  40528b:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40528f:	c1 f9 10             	sar    $0x10,%ecx
  405292:	b2 01                	mov    $0x1,%dl
  405294:	31 c0                	xor    %eax,%eax
  405296:	f6 c2 01             	test   $0x1,%dl
  405299:	0f 45 c1             	cmovne %ecx,%eax
  40529c:	25 00 80 00 00       	and    $0x8000,%eax
  4052a1:	89 44 24 08          	mov    %eax,0x8(%rsp)
  4052a5:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4052a9:	c1 f9 17             	sar    $0x17,%ecx
  4052ac:	b2 01                	mov    $0x1,%dl
  4052ae:	31 c0                	xor    %eax,%eax
  4052b0:	f6 c2 01             	test   $0x1,%dl
  4052b3:	0f 45 c1             	cmovne %ecx,%eax
  4052b6:	25 ff 00 00 00       	and    $0xff,%eax
  4052bb:	83 e8 70             	sub    $0x70,%eax
  4052be:	89 44 24 04          	mov    %eax,0x4(%rsp)
  4052c2:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  4052c6:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  4052cb:	89 04 24             	mov    %eax,(%rsp)
  4052ce:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  4052d3:	0f 9e c0             	setle  %al
  4052d6:	24 01                	and    $0x1,%al
  4052d8:	3c 00                	cmp    $0x0,%al
  4052da:	0f 84 82 00 00 00    	je     405362 <__truncsfhf2+0x122>
  4052e0:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  4052e5:	0f 9c c0             	setl   %al
  4052e8:	24 01                	and    $0x1,%al
  4052ea:	3c 00                	cmp    $0x0,%al
  4052ec:	74 16                	je     405304 <__truncsfhf2+0xc4>
  4052ee:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  4052f3:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  4052f8:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  4052ff:	48 83 c4 18          	add    $0x18,%rsp
  405303:	c3                   	ret
  405304:	8b 04 24             	mov    (%rsp),%eax
  405307:	0d 00 00 80 00       	or     $0x800000,%eax
  40530c:	ba 01 00 00 00       	mov    $0x1,%edx
  405311:	2b 54 24 04          	sub    0x4(%rsp),%edx
  405315:	89 d1                	mov    %edx,%ecx
  405317:	d3 f8                	sar    %cl,%eax
  405319:	89 c1                	mov    %eax,%ecx
  40531b:	31 c0                	xor    %eax,%eax
  40531d:	83 fa 20             	cmp    $0x20,%edx
  405320:	0f 42 c1             	cmovb  %ecx,%eax
  405323:	89 04 24             	mov    %eax,(%rsp)
  405326:	8b 04 24             	mov    (%rsp),%eax
  405329:	25 00 10 00 00       	and    $0x1000,%eax
  40532e:	83 f8 00             	cmp    $0x0,%eax
  405331:	0f 95 c0             	setne  %al
  405334:	24 01                	and    $0x1,%al
  405336:	3c 00                	cmp    $0x0,%al
  405338:	74 0b                	je     405345 <__truncsfhf2+0x105>
  40533a:	8b 04 24             	mov    (%rsp),%eax
  40533d:	05 00 20 00 00       	add    $0x2000,%eax
  405342:	89 04 24             	mov    %eax,(%rsp)
  405345:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405349:	8b 0c 24             	mov    (%rsp),%ecx
  40534c:	c1 e9 0d             	shr    $0xd,%ecx
  40534f:	09 c8                	or     %ecx,%eax
  405351:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  405356:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40535d:	48 83 c4 18          	add    $0x18,%rsp
  405361:	c3                   	ret
  405362:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  405369:	00 
  40536a:	0f 94 c0             	sete   %al
  40536d:	24 01                	and    $0x1,%al
  40536f:	3c 00                	cmp    $0x0,%al
  405371:	74 59                	je     4053cc <__truncsfhf2+0x18c>
  405373:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  405377:	0f 94 c0             	sete   %al
  40537a:	24 01                	and    $0x1,%al
  40537c:	3c 00                	cmp    $0x0,%al
  40537e:	74 1a                	je     40539a <__truncsfhf2+0x15a>
  405380:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405384:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405389:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  40538e:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  405395:	48 83 c4 18          	add    $0x18,%rsp
  405399:	c3                   	ret
  40539a:	8b 04 24             	mov    (%rsp),%eax
  40539d:	c1 f8 0d             	sar    $0xd,%eax
  4053a0:	89 04 24             	mov    %eax,(%rsp)
  4053a3:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4053a7:	8b 0c 24             	mov    (%rsp),%ecx
  4053aa:	09 c8                	or     %ecx,%eax
  4053ac:	85 c9                	test   %ecx,%ecx
  4053ae:	0f 94 c1             	sete   %cl
  4053b1:	0f b6 c9             	movzbl %cl,%ecx
  4053b4:	09 c8                	or     %ecx,%eax
  4053b6:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4053bb:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  4053c0:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  4053c7:	48 83 c4 18          	add    $0x18,%rsp
  4053cb:	c3                   	ret
  4053cc:	8b 04 24             	mov    (%rsp),%eax
  4053cf:	25 00 10 00 00       	and    $0x1000,%eax
  4053d4:	83 f8 00             	cmp    $0x0,%eax
  4053d7:	0f 95 c0             	setne  %al
  4053da:	24 01                	and    $0x1,%al
  4053dc:	3c 00                	cmp    $0x0,%al
  4053de:	74 33                	je     405413 <__truncsfhf2+0x1d3>
  4053e0:	8b 04 24             	mov    (%rsp),%eax
  4053e3:	05 00 20 00 00       	add    $0x2000,%eax
  4053e8:	89 04 24             	mov    %eax,(%rsp)
  4053eb:	8b 04 24             	mov    (%rsp),%eax
  4053ee:	25 00 00 80 00       	and    $0x800000,%eax
  4053f3:	83 f8 00             	cmp    $0x0,%eax
  4053f6:	0f 95 c0             	setne  %al
  4053f9:	24 01                	and    $0x1,%al
  4053fb:	3c 00                	cmp    $0x0,%al
  4053fd:	74 12                	je     405411 <__truncsfhf2+0x1d1>
  4053ff:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  405406:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40540a:	83 c0 01             	add    $0x1,%eax
  40540d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405411:	eb 00                	jmp    405413 <__truncsfhf2+0x1d3>
  405413:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  405418:	0f 9f c0             	setg   %al
  40541b:	24 01                	and    $0x1,%al
  40541d:	3c 00                	cmp    $0x0,%al
  40541f:	74 75                	je     405496 <__truncsfhf2+0x256>
  405421:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  405428:	00 00 00 
  40542b:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405430:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  405437:	00 00 
  405439:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  40543f:	0f 9c c0             	setl   %al
  405442:	24 01                	and    $0x1,%al
  405444:	3c 00                	cmp    $0x0,%al
  405446:	74 34                	je     40547c <__truncsfhf2+0x23c>
  405448:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40544d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405452:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405457:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  40545d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405462:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405467:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40546c:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405471:	48 83 c0 01          	add    $0x1,%rax
  405475:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40547a:	eb bd                	jmp    405439 <__truncsfhf2+0x1f9>
  40547c:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405480:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405485:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  40548a:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  405491:	48 83 c4 18          	add    $0x18,%rsp
  405495:	c3                   	ret
  405496:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40549a:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  40549e:	c1 e1 0a             	shl    $0xa,%ecx
  4054a1:	09 c8                	or     %ecx,%eax
  4054a3:	8b 0c 24             	mov    (%rsp),%ecx
  4054a6:	c1 e9 0d             	shr    $0xd,%ecx
  4054a9:	09 c8                	or     %ecx,%eax
  4054ab:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  4054b0:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  4054b7:	48 83 c4 18          	add    $0x18,%rsp
  4054bb:	c3                   	ret
  4054bc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004054c0 <__truncdfhf2>:
  4054c0:	48 83 ec 18          	sub    $0x18,%rsp
  4054c4:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  4054ca:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  4054d0:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  4054d6:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  4054da:	e8 61 fd ff ff       	call   405240 <__truncsfhf2>
  4054df:	48 83 c4 18          	add    $0x18,%rsp
  4054e3:	c3                   	ret
  4054e4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4054eb:	00 00 00 00 00 

00000000004054f0 <__gnu_h2f_ieee>:
  4054f0:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  4054f6:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  4054fc:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  405503:	00 
  405504:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  40550b:	00 
  40550c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  405511:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  405516:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  40551d:	00 
  40551e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  405525:	00 
  405526:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  40552d:	00 
  40552e:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  405535:	77 
  405536:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  40553d:	47 
  40553e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405543:	66 25 ff 7f          	and    $0x7fff,%ax
  405547:	0f b7 c8             	movzwl %ax,%ecx
  40554a:	c1 e1 0d             	shl    $0xd,%ecx
  40554d:	b2 01                	mov    $0x1,%dl
  40554f:	31 c0                	xor    %eax,%eax
  405551:	f6 c2 01             	test   $0x1,%dl
  405554:	0f 45 c1             	cmovne %ecx,%eax
  405557:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40555b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  405561:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  405567:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  40556d:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405573:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  405578:	0f 93 c0             	setae  %al
  40557b:	24 01                	and    $0x1,%al
  40557d:	3c 00                	cmp    $0x0,%al
  40557f:	74 0d                	je     40558e <__gnu_h2f_ieee+0x9e>
  405581:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405585:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  40558a:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40558e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405593:	66 25 00 80          	and    $0x8000,%ax
  405597:	0f b7 c8             	movzwl %ax,%ecx
  40559a:	c1 e1 10             	shl    $0x10,%ecx
  40559d:	b2 01                	mov    $0x1,%dl
  40559f:	31 c0                	xor    %eax,%eax
  4055a1:	f6 c2 01             	test   $0x1,%dl
  4055a4:	0f 45 c1             	cmovne %ecx,%eax
  4055a7:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  4055ab:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4055af:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  4055b5:	c3                   	ret
  4055b6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4055bd:	00 00 00 

00000000004055c0 <__gnu_f2h_ieee>:
  4055c0:	50                   	push   %rax
  4055c1:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  4055c6:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  4055cb:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  4055d1:	e8 6a fc ff ff       	call   405240 <__truncsfhf2>
  4055d6:	58                   	pop    %rax
  4055d7:	c3                   	ret
  4055d8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4055df:	00 

00000000004055e0 <__extendhfsf2>:
  4055e0:	50                   	push   %rax
  4055e1:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  4055e7:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  4055ed:	0f 28 c8             	movaps %xmm0,%xmm1
  4055f0:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  4055f7:	00 
  4055f8:	e8 f3 fe ff ff       	call   4054f0 <__gnu_h2f_ieee>
  4055fd:	58                   	pop    %rax
  4055fe:	c3                   	ret
  4055ff:	90                   	nop

0000000000405600 <__floattidf>:
  405600:	53                   	push   %rbx
  405601:	48 83 ec 10          	sub    $0x10,%rsp
  405605:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40560a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40560f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405614:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405619:	48 89 04 24          	mov    %rax,(%rsp)
  40561d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405622:	48 09 c8             	or     %rcx,%rax
  405625:	0f 94 c0             	sete   %al
  405628:	24 01                	and    $0x1,%al
  40562a:	3c 00                	cmp    $0x0,%al
  40562c:	74 09                	je     405637 <__floattidf+0x37>
  40562e:	0f 57 c0             	xorps  %xmm0,%xmm0
  405631:	48 83 c4 10          	add    $0x10,%rsp
  405635:	5b                   	pop    %rbx
  405636:	c3                   	ret
  405637:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40563c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405641:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405646:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40564b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405650:	48 c1 f8 3f          	sar    $0x3f,%rax
  405654:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405659:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40565e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405663:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405668:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40566d:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405672:	48 31 d0             	xor    %rdx,%rax
  405675:	48 31 f1             	xor    %rsi,%rcx
  405678:	48 29 f1             	sub    %rsi,%rcx
  40567b:	48 19 d0             	sbb    %rdx,%rax
  40567e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405683:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405688:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  40568d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  405692:	48 0f bd c2          	bsr    %rdx,%rax
  405696:	48 83 f0 3f          	xor    $0x3f,%rax
  40569a:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40569f:	48 0f bd ce          	bsr    %rsi,%rcx
  4056a3:	48 83 f1 3f          	xor    $0x3f,%rcx
  4056a7:	48 83 c1 40          	add    $0x40,%rcx
  4056ab:	48 85 d2             	test   %rdx,%rdx
  4056ae:	48 0f 45 c8          	cmovne %rax,%rcx
  4056b2:	31 c0                	xor    %eax,%eax
  4056b4:	ba 80 00 00 00       	mov    $0x80,%edx
  4056b9:	48 29 ca             	sub    %rcx,%rdx
  4056bc:	48 89 c1             	mov    %rax,%rcx
  4056bf:	48 19 c9             	sbb    %rcx,%rcx
  4056c2:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4056c7:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4056cc:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  4056d0:	ff c9                	dec    %ecx
  4056d2:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  4056d6:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4056db:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4056e0:	ba 35 00 00 00       	mov    $0x35,%edx
  4056e5:	48 29 f2             	sub    %rsi,%rdx
  4056e8:	48 19 c8             	sbb    %rcx,%rax
  4056eb:	0f 9c c0             	setl   %al
  4056ee:	24 01                	and    $0x1,%al
  4056f0:	3c 00                	cmp    $0x0,%al
  4056f2:	0f 84 c0 01 00 00    	je     4058b8 <__floattidf+0x2b8>
  4056f8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4056fd:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  405702:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405707:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40570c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  405711:	0f 28 0d b8 1c 00 00 	movaps 0x1cb8(%rip),%xmm1        # 4073d0 <runtime::type_table+0x3b8>
  405718:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40571c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  405721:	74 17                	je     40573a <__floattidf+0x13a>
  405723:	eb 00                	jmp    405725 <__floattidf+0x125>
  405725:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  40572a:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40572f:	48 83 f0 37          	xor    $0x37,%rax
  405733:	48 09 c8             	or     %rcx,%rax
  405736:	74 26                	je     40575e <__floattidf+0x15e>
  405738:	eb 29                	jmp    405763 <__floattidf+0x163>
  40573a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40573f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405744:	48 89 d0             	mov    %rdx,%rax
  405747:	48 01 c0             	add    %rax,%rax
  40574a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40574f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405754:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405759:	e9 d6 00 00 00       	jmp    405834 <__floattidf+0x234>
  40575e:	e9 d1 00 00 00       	jmp    405834 <__floattidf+0x234>
  405763:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405768:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  40576d:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405772:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405777:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40577c:	49 89 fb             	mov    %rdi,%r11
  40577f:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405783:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405787:	44 88 db             	mov    %r11b,%bl
  40578a:	88 d9                	mov    %bl,%cl
  40578c:	49 89 f2             	mov    %rsi,%r10
  40578f:	49 d3 ea             	shr    %cl,%r10
  405792:	88 d9                	mov    %bl,%cl
  405794:	49 89 d1             	mov    %rdx,%r9
  405797:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40579b:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  4057a0:	45 31 c0             	xor    %r8d,%r8d
  4057a3:	f6 c3 40             	test   $0x40,%bl
  4057a6:	4d 0f 45 ca          	cmovne %r10,%r9
  4057aa:	4d 0f 45 d0          	cmovne %r8,%r10
  4057ae:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  4057b5:	48 83 d8 00          	sbb    $0x0,%rax
  4057b9:	4c 89 c0             	mov    %r8,%rax
  4057bc:	49 0f 42 c2          	cmovb  %r10,%rax
  4057c0:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  4057c5:	4c 89 c0             	mov    %r8,%rax
  4057c8:	49 0f 42 c1          	cmovb  %r9,%rax
  4057cc:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  4057d2:	49 29 fb             	sub    %rdi,%r11
  4057d5:	4c 89 c7             	mov    %r8,%rdi
  4057d8:	48 19 cf             	sbb    %rcx,%rdi
  4057db:	45 88 d9             	mov    %r11b,%r9b
  4057de:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  4057e5:	44 88 c9             	mov    %r9b,%cl
  4057e8:	4c 89 d3             	mov    %r10,%rbx
  4057eb:	48 d3 eb             	shr    %cl,%rbx
  4057ee:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  4057f3:	41 f6 c1 40          	test   $0x40,%r9b
  4057f7:	49 89 d9             	mov    %rbx,%r9
  4057fa:	4d 0f 45 c8          	cmovne %r8,%r9
  4057fe:	4c 0f 45 d3          	cmovne %rbx,%r10
  405802:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405809:	48 83 df 00          	sbb    $0x0,%rdi
  40580d:	4c 89 c7             	mov    %r8,%rdi
  405810:	49 0f 42 fa          	cmovb  %r10,%rdi
  405814:	4d 0f 42 c1          	cmovb  %r9,%r8
  405818:	4c 21 c6             	and    %r8,%rsi
  40581b:	48 21 fa             	and    %rdi,%rdx
  40581e:	48 09 f2             	or     %rsi,%rdx
  405821:	0f 95 c2             	setne  %dl
  405824:	0f b6 d2             	movzbl %dl,%edx
  405827:	48 09 d0             	or     %rdx,%rax
  40582a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40582f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405834:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405839:	89 c1                	mov    %eax,%ecx
  40583b:	83 e1 04             	and    $0x4,%ecx
  40583e:	c1 e9 02             	shr    $0x2,%ecx
  405841:	48 09 c8             	or     %rcx,%rax
  405844:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405849:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40584e:	48 83 c0 01          	add    $0x1,%rax
  405852:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  405858:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40585d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405862:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405867:	48 89 c8             	mov    %rcx,%rax
  40586a:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40586f:	48 c1 f9 02          	sar    $0x2,%rcx
  405873:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405878:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40587d:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  405881:	24 20                	and    $0x20,%al
  405883:	c0 e8 05             	shr    $0x5,%al
  405886:	24 01                	and    $0x1,%al
  405888:	3c 00                	cmp    $0x0,%al
  40588a:	74 2a                	je     4058b6 <__floattidf+0x2b6>
  40588c:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405891:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405896:	48 89 c8             	mov    %rcx,%rax
  405899:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40589e:	48 d1 f9             	sar    $1,%rcx
  4058a1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4058a6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4058ab:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  4058af:	83 c0 01             	add    $0x1,%eax
  4058b2:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  4058b6:	eb 5c                	jmp    405914 <__floattidf+0x314>
  4058b8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  4058bc:	b9 35 00 00 00       	mov    $0x35,%ecx
  4058c1:	29 c1                	sub    %eax,%ecx
  4058c3:	83 e1 7f             	and    $0x7f,%ecx
  4058c6:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  4058ca:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4058cf:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  4058d4:	40 88 cf             	mov    %cl,%dil
  4058d7:	40 88 f9             	mov    %dil,%cl
  4058da:	48 89 c2             	mov    %rax,%rdx
  4058dd:	48 d3 e2             	shl    %cl,%rdx
  4058e0:	40 88 f9             	mov    %dil,%cl
  4058e3:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  4058e7:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  4058eb:	31 c0                	xor    %eax,%eax
  4058ed:	40 f6 c7 40          	test   $0x40,%dil
  4058f1:	48 0f 45 f2          	cmovne %rdx,%rsi
  4058f5:	48 0f 45 d0          	cmovne %rax,%rdx
  4058f9:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  4058ff:	48 89 c1             	mov    %rax,%rcx
  405902:	48 0f 42 ce          	cmovb  %rsi,%rcx
  405906:	48 0f 42 c2          	cmovb  %rdx,%rax
  40590a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40590f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405914:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  40591b:	00 00 
  40591d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  405921:	25 00 00 00 80       	and    $0x80000000,%eax
  405926:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  40592a:	c1 e1 14             	shl    $0x14,%ecx
  40592d:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  405933:	09 c8                	or     %ecx,%eax
  405935:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  405939:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  40593f:	09 c8                	or     %ecx,%eax
  405941:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405945:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405949:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40594d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405953:	48 83 c4 10          	add    $0x10,%rsp
  405957:	5b                   	pop    %rbx
  405958:	c3                   	ret
  405959:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000405960 <__floattidf_unsigned>:
  405960:	53                   	push   %rbx
  405961:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405966:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40596b:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405970:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405975:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40597a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40597f:	48 09 c8             	or     %rcx,%rax
  405982:	0f 94 c0             	sete   %al
  405985:	24 01                	and    $0x1,%al
  405987:	3c 00                	cmp    $0x0,%al
  405989:	74 05                	je     405990 <__floattidf_unsigned+0x30>
  40598b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40598e:	5b                   	pop    %rbx
  40598f:	c3                   	ret
  405990:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405995:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40599a:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40599f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4059a4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  4059a9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4059ae:	48 0f bd c2          	bsr    %rdx,%rax
  4059b2:	48 83 f0 3f          	xor    $0x3f,%rax
  4059b6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  4059bb:	48 0f bd ce          	bsr    %rsi,%rcx
  4059bf:	48 83 f1 3f          	xor    $0x3f,%rcx
  4059c3:	48 83 c1 40          	add    $0x40,%rcx
  4059c7:	48 85 d2             	test   %rdx,%rdx
  4059ca:	48 0f 45 c8          	cmovne %rax,%rcx
  4059ce:	31 c0                	xor    %eax,%eax
  4059d0:	ba 80 00 00 00       	mov    $0x80,%edx
  4059d5:	48 29 ca             	sub    %rcx,%rdx
  4059d8:	48 89 c1             	mov    %rax,%rcx
  4059db:	48 19 c9             	sbb    %rcx,%rcx
  4059de:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4059e3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4059e8:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  4059ec:	ff c9                	dec    %ecx
  4059ee:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  4059f2:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4059f7:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4059fc:	ba 35 00 00 00       	mov    $0x35,%edx
  405a01:	48 29 f2             	sub    %rsi,%rdx
  405a04:	48 19 c8             	sbb    %rcx,%rax
  405a07:	0f 92 c0             	setb   %al
  405a0a:	24 01                	and    $0x1,%al
  405a0c:	3c 00                	cmp    $0x0,%al
  405a0e:	0f 84 c0 01 00 00    	je     405bd4 <__floattidf_unsigned+0x274>
  405a14:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405a19:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  405a1e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405a23:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405a28:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  405a2d:	0f 28 0d 9c 19 00 00 	movaps 0x199c(%rip),%xmm1        # 4073d0 <runtime::type_table+0x3b8>
  405a34:	66 0f ef c1          	pxor   %xmm1,%xmm0
  405a38:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  405a3d:	74 17                	je     405a56 <__floattidf_unsigned+0xf6>
  405a3f:	eb 00                	jmp    405a41 <__floattidf_unsigned+0xe1>
  405a41:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  405a46:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405a4b:	48 83 f0 37          	xor    $0x37,%rax
  405a4f:	48 09 c8             	or     %rcx,%rax
  405a52:	74 26                	je     405a7a <__floattidf_unsigned+0x11a>
  405a54:	eb 29                	jmp    405a7f <__floattidf_unsigned+0x11f>
  405a56:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405a5b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405a60:	48 89 d0             	mov    %rdx,%rax
  405a63:	48 01 c0             	add    %rax,%rax
  405a66:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  405a6b:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405a70:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405a75:	e9 d6 00 00 00       	jmp    405b50 <__floattidf_unsigned+0x1f0>
  405a7a:	e9 d1 00 00 00       	jmp    405b50 <__floattidf_unsigned+0x1f0>
  405a7f:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405a84:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  405a89:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405a8e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405a93:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405a98:	49 89 fb             	mov    %rdi,%r11
  405a9b:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405a9f:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405aa3:	44 88 db             	mov    %r11b,%bl
  405aa6:	88 d9                	mov    %bl,%cl
  405aa8:	49 89 f2             	mov    %rsi,%r10
  405aab:	49 d3 ea             	shr    %cl,%r10
  405aae:	88 d9                	mov    %bl,%cl
  405ab0:	49 89 d1             	mov    %rdx,%r9
  405ab3:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  405ab7:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  405abc:	45 31 c0             	xor    %r8d,%r8d
  405abf:	f6 c3 40             	test   $0x40,%bl
  405ac2:	4d 0f 45 ca          	cmovne %r10,%r9
  405ac6:	4d 0f 45 d0          	cmovne %r8,%r10
  405aca:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405ad1:	48 83 d8 00          	sbb    $0x0,%rax
  405ad5:	4c 89 c0             	mov    %r8,%rax
  405ad8:	49 0f 42 c2          	cmovb  %r10,%rax
  405adc:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405ae1:	4c 89 c0             	mov    %r8,%rax
  405ae4:	49 0f 42 c1          	cmovb  %r9,%rax
  405ae8:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  405aee:	49 29 fb             	sub    %rdi,%r11
  405af1:	4c 89 c7             	mov    %r8,%rdi
  405af4:	48 19 cf             	sbb    %rcx,%rdi
  405af7:	45 88 d9             	mov    %r11b,%r9b
  405afa:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  405b01:	44 88 c9             	mov    %r9b,%cl
  405b04:	4c 89 d3             	mov    %r10,%rbx
  405b07:	48 d3 eb             	shr    %cl,%rbx
  405b0a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  405b0f:	41 f6 c1 40          	test   $0x40,%r9b
  405b13:	49 89 d9             	mov    %rbx,%r9
  405b16:	4d 0f 45 c8          	cmovne %r8,%r9
  405b1a:	4c 0f 45 d3          	cmovne %rbx,%r10
  405b1e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405b25:	48 83 df 00          	sbb    $0x0,%rdi
  405b29:	4c 89 c7             	mov    %r8,%rdi
  405b2c:	49 0f 42 fa          	cmovb  %r10,%rdi
  405b30:	4d 0f 42 c1          	cmovb  %r9,%r8
  405b34:	4c 21 c6             	and    %r8,%rsi
  405b37:	48 21 fa             	and    %rdi,%rdx
  405b3a:	48 09 f2             	or     %rsi,%rdx
  405b3d:	0f 95 c2             	setne  %dl
  405b40:	0f b6 d2             	movzbl %dl,%edx
  405b43:	48 09 d0             	or     %rdx,%rax
  405b46:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405b4b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b50:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405b55:	89 c1                	mov    %eax,%ecx
  405b57:	83 e1 04             	and    $0x4,%ecx
  405b5a:	c1 e9 02             	shr    $0x2,%ecx
  405b5d:	48 09 c8             	or     %rcx,%rax
  405b60:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b65:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405b6a:	48 83 c0 01          	add    $0x1,%rax
  405b6e:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  405b74:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b79:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405b7e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405b83:	48 89 c8             	mov    %rcx,%rax
  405b86:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  405b8b:	48 c1 e9 02          	shr    $0x2,%rcx
  405b8f:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405b94:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b99:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  405b9d:	24 20                	and    $0x20,%al
  405b9f:	c0 e8 05             	shr    $0x5,%al
  405ba2:	24 01                	and    $0x1,%al
  405ba4:	3c 00                	cmp    $0x0,%al
  405ba6:	74 2a                	je     405bd2 <__floattidf_unsigned+0x272>
  405ba8:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405bad:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405bb2:	48 89 c8             	mov    %rcx,%rax
  405bb5:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  405bba:	48 d1 e9             	shr    $1,%rcx
  405bbd:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405bc2:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405bc7:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  405bcb:	83 c0 01             	add    $0x1,%eax
  405bce:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405bd2:	eb 6a                	jmp    405c3e <__floattidf_unsigned+0x2de>
  405bd4:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  405bd9:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405bde:	31 c0                	xor    %eax,%eax
  405be0:	bf 35 00 00 00       	mov    $0x35,%edi
  405be5:	48 29 d7             	sub    %rdx,%rdi
  405be8:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  405bed:	48 19 c8             	sbb    %rcx,%rax
  405bf0:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  405bf5:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405bfa:	41 88 f8             	mov    %dil,%r8b
  405bfd:	44 88 c1             	mov    %r8b,%cl
  405c00:	4c 89 ce             	mov    %r9,%rsi
  405c03:	48 d3 e6             	shl    %cl,%rsi
  405c06:	44 88 c1             	mov    %r8b,%cl
  405c09:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  405c0d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  405c12:	41 f6 c0 40          	test   $0x40,%r8b
  405c16:	48 0f 45 d6          	cmovne %rsi,%rdx
  405c1a:	48 0f 45 f1          	cmovne %rcx,%rsi
  405c1e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  405c25:	48 83 d8 00          	sbb    $0x0,%rax
  405c29:	48 89 c8             	mov    %rcx,%rax
  405c2c:	48 0f 42 c6          	cmovb  %rsi,%rax
  405c30:	48 0f 42 ca          	cmovb  %rdx,%rcx
  405c34:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405c39:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405c3e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  405c45:	00 00 
  405c47:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  405c4b:	c1 e2 14             	shl    $0x14,%edx
  405c4e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  405c52:	25 ff ff 0f 00       	and    $0xfffff,%eax
  405c57:	89 c1                	mov    %eax,%ecx
  405c59:	89 d0                	mov    %edx,%eax
  405c5b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  405c62:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405c66:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  405c6a:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  405c6e:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405c74:	5b                   	pop    %rbx
  405c75:	c3                   	ret
  405c76:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  405c7d:	00 00 00 

0000000000405c80 <__umodti3>:
  405c80:	48 83 ec 58          	sub    $0x58,%rsp
  405c84:	48 89 0c 24          	mov    %rcx,(%rsp)
  405c88:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405c8d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405c92:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405c97:	48 8b 0c 24          	mov    (%rsp),%rcx
  405c9b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  405ca0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405ca5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405caa:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  405caf:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  405cb4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  405cb9:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405cbe:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  405cc3:	e8 78 b6 ff ff       	call   401340 <runtime::udivmod128>
  405cc8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405ccd:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  405cd2:	48 83 c4 58          	add    $0x58,%rsp
  405cd6:	c3                   	ret
  405cd7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405cde:	00 00 

0000000000405ce0 <__udivmodti4>:
  405ce0:	48 83 ec 58          	sub    $0x58,%rsp
  405ce4:	4c 89 04 24          	mov    %r8,(%rsp)
  405ce8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405ced:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405cf2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405cf7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  405cfc:	4c 8b 04 24          	mov    (%rsp),%r8
  405d00:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405d05:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405d0a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405d0f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405d14:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  405d19:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  405d1e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  405d23:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405d28:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  405d2d:	e8 0e b6 ff ff       	call   401340 <runtime::udivmod128>
  405d32:	48 83 c4 58          	add    $0x58,%rsp
  405d36:	c3                   	ret
  405d37:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405d3e:	00 00 

0000000000405d40 <__udivti3>:
  405d40:	48 83 ec 48          	sub    $0x48,%rsp
  405d44:	48 89 0c 24          	mov    %rcx,(%rsp)
  405d48:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405d4d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405d52:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405d57:	48 8b 0c 24          	mov    (%rsp),%rcx
  405d5b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  405d60:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405d65:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405d6a:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  405d6f:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405d74:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  405d79:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405d7e:	31 c0                	xor    %eax,%eax
  405d80:	41 89 c0             	mov    %eax,%r8d
  405d83:	e8 58 ff ff ff       	call   405ce0 <__udivmodti4>
  405d88:	48 83 c4 48          	add    $0x48,%rsp
  405d8c:	c3                   	ret
  405d8d:	0f 1f 00             	nopl   (%rax)

0000000000405d90 <runtime::assert>:
  405d90:	48 83 ec 48          	sub    $0x48,%rsp
  405d94:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  405d99:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  405d9e:	40 88 f8             	mov    %dil,%al
  405da1:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  405da5:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405daa:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  405daf:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  405db3:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  405db8:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  405dbd:	88 44 24 47          	mov    %al,0x47(%rsp)
  405dc1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  405dc6:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405dcb:	3c 00                	cmp    $0x0,%al
  405dcd:	75 19                	jne    405de8 <runtime::assert+0x58>
  405dcf:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405dd4:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405dd9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405dde:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  405de3:	e8 18 0a 00 00       	call   406800 <runtime::assert.internal-0>
  405de8:	48 83 c4 48          	add    $0x48,%rsp
  405dec:	c3                   	ret
  405ded:	0f 1f 00             	nopl   (%rax)

0000000000405df0 <runtime::heap_allocator_proc.aligned_alloc-0>:
  405df0:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  405df7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  405dfc:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  405e01:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405e06:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  405e0b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  405e10:	44 88 c0             	mov    %r8b,%al
  405e13:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  405e17:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  405e1e:	00 
  405e1f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  405e24:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405e29:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405e2e:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  405e33:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  405e37:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405e3c:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  405e43:	00 
  405e44:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  405e4b:	00 
  405e4c:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  405e53:	00 
  405e54:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  405e5b:	00 
  405e5c:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  405e63:	b9 08 00 00 00       	mov    $0x8,%ecx
  405e68:	48 83 fe 08          	cmp    $0x8,%rsi
  405e6c:	48 0f 4f ce          	cmovg  %rsi,%rcx
  405e70:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  405e77:	00 
  405e78:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405e7f:	00 
  405e80:	48 83 e9 01          	sub    $0x1,%rcx
  405e84:	48 83 c1 08          	add    $0x8,%rcx
  405e88:	48 01 d1             	add    %rdx,%rcx
  405e8b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  405e92:	00 
  405e93:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  405e9a:	00 00 
  405e9c:	48 83 f8 00          	cmp    $0x0,%rax
  405ea0:	0f 95 c1             	setne  %cl
  405ea3:	80 e1 01             	and    $0x1,%cl
  405ea6:	31 c0                	xor    %eax,%eax
  405ea8:	80 f9 00             	cmp    $0x0,%cl
  405eab:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405eaf:	74 17                	je     405ec8 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  405eb1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  405eb6:	48 83 f8 08          	cmp    $0x8,%rax
  405eba:	0f 9f c0             	setg   %al
  405ebd:	24 01                	and    $0x1,%al
  405ebf:	3c 00                	cmp    $0x0,%al
  405ec1:	0f 95 c0             	setne  %al
  405ec4:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405ec8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405ecd:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  405ed1:	80 e1 01             	and    $0x1,%cl
  405ed4:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  405ed8:	48 83 f8 00          	cmp    $0x0,%rax
  405edc:	0f 95 c0             	setne  %al
  405edf:	24 01                	and    $0x1,%al
  405ee1:	3c 00                	cmp    $0x0,%al
  405ee3:	74 2e                	je     405f13 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  405ee5:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  405eea:	75 27                	jne    405f13 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  405eec:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405ef1:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  405ef5:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  405efa:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  405eff:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  405f06:	00 
  405f07:	e8 64 db ff ff       	call   403a70 <runtime::heap_resize>
  405f0c:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  405f11:	eb 19                	jmp    405f2c <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  405f13:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  405f17:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  405f1e:	00 
  405f1f:	0f b6 f0             	movzbl %al,%esi
  405f22:	e8 19 db ff ff       	call   403a40 <runtime::heap_alloc>
  405f27:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  405f2c:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  405f31:	48 83 c0 08          	add    $0x8,%rax
  405f35:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  405f3a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  405f3f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405f44:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  405f49:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  405f4e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  405f53:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  405f5a:	00 
  405f5b:	48 83 e8 01          	sub    $0x1,%rax
  405f5f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405f66:	00 
  405f67:	48 83 e9 01          	sub    $0x1,%rcx
  405f6b:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  405f6f:	48 21 c8             	and    %rcx,%rax
  405f72:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  405f77:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  405f7d:	0f 94 c0             	sete   %al
  405f80:	24 01                	and    $0x1,%al
  405f82:	3c 00                	cmp    $0x0,%al
  405f84:	74 3c                	je     405fc2 <runtime::heap_allocator_proc.aligned_alloc-0+0x1d2>
  405f86:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405f8b:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405f90:	e8 ab 00 00 00       	call   406040 <runtime::heap_allocator_proc.aligned_free-1>
  405f95:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405f9a:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  405f9f:	e8 9c 00 00 00       	call   406040 <runtime::heap_allocator_proc.aligned_free-1>
  405fa4:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405fa9:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405fb0:	00 
  405fb1:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  405fb8:	b0 01                	mov    $0x1,%al
  405fba:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  405fc1:	c3                   	ret
  405fc2:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405fc7:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405fcc:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  405fd1:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  405fd6:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  405fda:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  405fdf:	74 2f                	je     406010 <runtime::heap_allocator_proc.aligned_alloc-0+0x220>
  405fe1:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405fe6:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  405feb:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405ff0:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  405ff5:	48 39 d0             	cmp    %rdx,%rax
  405ff8:	48 0f 4c d0          	cmovl  %rax,%rdx
  405ffc:	e8 3f d0 ff ff       	call   403040 <runtime::mem_copy_non_overlapping>
  406001:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406006:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40600b:	e8 30 00 00 00       	call   406040 <runtime::heap_allocator_proc.aligned_free-1>
  406010:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406015:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  40601a:	e8 c1 bf ff ff       	call   401fe0 <runtime::[internal.odin]::byte_slice>
  40601f:	48 89 c1             	mov    %rax,%rcx
  406022:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406027:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40602b:	48 89 08             	mov    %rcx,(%rax)
  40602e:	31 c0                	xor    %eax,%eax
  406030:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  406037:	c3                   	ret
  406038:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40603f:	00 

0000000000406040 <runtime::heap_allocator_proc.aligned_free-1>:
  406040:	48 83 ec 18          	sub    $0x18,%rsp
  406044:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406049:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40604e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406053:	48 83 f8 00          	cmp    $0x0,%rax
  406057:	0f 95 c0             	setne  %al
  40605a:	24 01                	and    $0x1,%al
  40605c:	3c 00                	cmp    $0x0,%al
  40605e:	74 0e                	je     40606e <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  406060:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406065:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  406069:	e8 32 da ff ff       	call   403aa0 <runtime::heap_free>
  40606e:	48 83 c4 18          	add    $0x18,%rsp
  406072:	c3                   	ret
  406073:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40607a:	84 00 00 00 00 00 

0000000000406080 <runtime::heap_allocator_proc.aligned_resize-2>:
  406080:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  406087:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40608c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406091:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  406096:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40609b:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  4060a0:	44 88 c0             	mov    %r8b,%al
  4060a3:	88 44 24 57          	mov    %al,0x57(%rsp)
  4060a7:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4060ae:	00 
  4060af:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4060b4:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4060b9:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  4060bd:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4060c2:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4060c7:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4060cc:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4060d3:	00 
  4060d4:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  4060db:	00 
  4060dc:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  4060e3:	00 
  4060e4:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  4060eb:	00 
  4060ec:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  4060f3:	0f 57 c0             	xorps  %xmm0,%xmm0
  4060f6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4060fd:	00 
  4060fe:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  406105:	00 
  406106:	48 83 f8 00          	cmp    $0x0,%rax
  40610a:	0f 94 c0             	sete   %al
  40610d:	24 01                	and    $0x1,%al
  40610f:	3c 00                	cmp    $0x0,%al
  406111:	0f 84 80 00 00 00    	je     406197 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  406117:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40611c:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406121:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406126:	8a 44 24 57          	mov    0x57(%rsp),%al
  40612a:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  40612f:	0f 57 c0             	xorps  %xmm0,%xmm0
  406132:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  406139:	00 
  40613a:	48 89 e2             	mov    %rsp,%rdx
  40613d:	4c 89 02             	mov    %r8,(%rdx)
  406140:	44 0f b6 c0          	movzbl %al,%r8d
  406144:	31 c0                	xor    %eax,%eax
  406146:	89 c2                	mov    %eax,%edx
  406148:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  40614f:	00 
  406150:	e8 9b fc ff ff       	call   405df0 <runtime::heap_allocator_proc.aligned_alloc-0>
  406155:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40615a:	40 88 c7             	mov    %al,%dil
  40615d:	40 88 f8             	mov    %dil,%al
  406160:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  406167:	00 
  406168:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  40616f:	00 
  406170:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406177:	00 
  406178:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40617f:	00 
  406180:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  406187:	00 
  406188:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40618c:	48 89 11             	mov    %rdx,(%rcx)
  40618f:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406196:	c3                   	ret
  406197:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40619c:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4061a1:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4061a6:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4061ab:	8a 44 24 57          	mov    0x57(%rsp),%al
  4061af:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  4061b4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4061b7:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4061be:	00 
  4061bf:	49 89 e0             	mov    %rsp,%r8
  4061c2:	4d 89 08             	mov    %r9,(%r8)
  4061c5:	44 0f b6 c0          	movzbl %al,%r8d
  4061c9:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  4061d0:	00 
  4061d1:	e8 1a fc ff ff       	call   405df0 <runtime::heap_allocator_proc.aligned_alloc-0>
  4061d6:	88 44 24 17          	mov    %al,0x17(%rsp)
  4061da:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  4061e1:	00 
  4061e2:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4061e7:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4061ee:	00 
  4061ef:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4061f4:	3c 00                	cmp    $0x0,%al
  4061f6:	74 4d                	je     406245 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  4061f8:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4061fd:	8a 44 24 17          	mov    0x17(%rsp),%al
  406201:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406208:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40620f:	00 
  406210:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406217:	00 
  406218:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40621f:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406226:	00 
  406227:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40622e:	00 
  40622f:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406236:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40623a:	48 89 11             	mov    %rdx,(%rcx)
  40623d:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406244:	c3                   	ret
  406245:	8a 44 24 57          	mov    0x57(%rsp),%al
  406249:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40624e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406253:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40625a:	00 
  40625b:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  406262:	00 
  406263:	3c 00                	cmp    $0x0,%al
  406265:	0f 84 87 00 00 00    	je     4062f2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  40626b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406270:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  406275:	48 39 c8             	cmp    %rcx,%rax
  406278:	0f 9f c0             	setg   %al
  40627b:	24 01                	and    $0x1,%al
  40627d:	3c 00                	cmp    $0x0,%al
  40627f:	74 71                	je     4062f2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  406281:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  406286:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  40628d:	00 
  40628e:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406293:	48 89 e0             	mov    %rsp,%rax
  406296:	4c 89 08             	mov    %r9,(%rax)
  406299:	bf 47 73 40 00       	mov    $0x407347,%edi
  40629e:	be 30 00 00 00       	mov    $0x30,%esi
  4062a3:	ba 4b 00 00 00       	mov    $0x4b,%edx
  4062a8:	b9 25 00 00 00       	mov    $0x25,%ecx
  4062ad:	e8 1e ce ff ff       	call   4030d0 <runtime::slice_expr_error_lo_hi>
  4062b2:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4062b7:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4062bc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4062c1:	48 89 c6             	mov    %rax,%rsi
  4062c4:	48 03 b4 24 e0 00 00 	add    0xe0(%rsp),%rsi
  4062cb:	00 
  4062cc:	48 29 c1             	sub    %rax,%rcx
  4062cf:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4062d4:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4062d9:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4062de:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  4062e3:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4062e8:	48 29 c2             	sub    %rax,%rdx
  4062eb:	31 f6                	xor    %esi,%esi
  4062ed:	e8 4e ad ff ff       	call   401040 <memset@plt>
  4062f2:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4062f7:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4062fe:	00 
  4062ff:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406306:	00 
  406307:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40630e:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406315:	00 
  406316:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40631d:	00 
  40631e:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406325:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  406329:	48 89 11             	mov    %rdx,(%rcx)
  40632c:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406333:	c3                   	ret
  406334:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40633b:	00 00 00 00 00 

0000000000406340 <runtime::bounds_check_error.handle_error-0>:
  406340:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  406347:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40634c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406351:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406355:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406359:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40635e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406363:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406368:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40636d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406371:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406375:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40637a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40637f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  406384:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40638b:	00 
  40638c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  406390:	89 44 24 70          	mov    %eax,0x70(%rsp)
  406394:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  406399:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40639e:	0f 57 c0             	xorps  %xmm0,%xmm0
  4063a1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4063a6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4063ab:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4063b2:	00 00 
  4063b4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4063b9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4063be:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4063c5:	00 00 
  4063c7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4063cc:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4063d1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4063d5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4063d9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4063de:	e8 cd da ff ff       	call   403eb0 <runtime::print_caller_location>
  4063e3:	bf 78 73 40 00       	mov    $0x407378,%edi
  4063e8:	be 07 00 00 00       	mov    $0x7,%esi
  4063ed:	e8 9e d2 ff ff       	call   403690 <runtime::print_string>
  4063f2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4063f7:	e8 b4 d8 ff ff       	call   403cb0 <runtime::print_i64>
  4063fc:	bf c3 72 40 00       	mov    $0x4072c3,%edi
  406401:	be 15 00 00 00       	mov    $0x15,%esi
  406406:	e8 85 d2 ff ff       	call   403690 <runtime::print_string>
  40640b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406410:	e8 9b d8 ff ff       	call   403cb0 <runtime::print_i64>
  406415:	bf 0a 00 00 00       	mov    $0xa,%edi
  40641a:	e8 a1 d4 ff ff       	call   4038c0 <runtime::print_byte>
  40641f:	e8 ec ae ff ff       	call   401310 <runtime::bounds_trap>
  406424:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40642b:	00 00 00 00 00 

0000000000406430 <runtime::default_random_generator_proc.read_u64-0>:
  406430:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  406435:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40643a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40643f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406444:	48 8b 00             	mov    (%rax),%rax
  406447:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40644c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406451:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  406458:	f4 51 58 
  40645b:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  406461:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406466:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  40646a:	48 83 ca 01          	or     $0x1,%rdx
  40646e:	48 01 d1             	add    %rdx,%rcx
  406471:	48 89 08             	mov    %rcx,(%rax)
  406474:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406479:	48 c1 e9 3b          	shr    $0x3b,%rcx
  40647d:	b2 01                	mov    $0x1,%dl
  40647f:	31 c0                	xor    %eax,%eax
  406481:	f6 c2 01             	test   $0x1,%dl
  406484:	48 0f 45 c1          	cmovne %rcx,%rax
  406488:	48 83 c0 05          	add    $0x5,%rax
  40648c:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  406491:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  406498:	75 f1 ae 
  40649b:	48 0f af c1          	imul   %rcx,%rax
  40649f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4064a4:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4064a9:	48 c1 e9 3b          	shr    $0x3b,%rcx
  4064ad:	b2 01                	mov    $0x1,%dl
  4064af:	31 c0                	xor    %eax,%eax
  4064b1:	f6 c2 01             	test   $0x1,%dl
  4064b4:	48 0f 45 c1          	cmovne %rcx,%rax
  4064b8:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4064bd:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4064c2:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4064c7:	48 89 d1             	mov    %rdx,%rcx
  4064ca:	48 d3 e8             	shr    %cl,%rax
  4064cd:	48 89 c1             	mov    %rax,%rcx
  4064d0:	31 c0                	xor    %eax,%eax
  4064d2:	48 83 fa 40          	cmp    $0x40,%rdx
  4064d6:	48 0f 42 c1          	cmovb  %rcx,%rax
  4064da:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4064df:	31 c9                	xor    %ecx,%ecx
  4064e1:	89 ce                	mov    %ecx,%esi
  4064e3:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  4064e8:	48 83 e6 3f          	and    $0x3f,%rsi
  4064ec:	48 89 f1             	mov    %rsi,%rcx
  4064ef:	48 d3 e2             	shl    %cl,%rdx
  4064f2:	31 c9                	xor    %ecx,%ecx
  4064f4:	48 83 fe 40          	cmp    $0x40,%rsi
  4064f8:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4064fc:	48 09 c8             	or     %rcx,%rax
  4064ff:	c3                   	ret

0000000000406500 <runtime::default_random_generator_proc.init-1>:
  406500:	48 83 ec 28          	sub    $0x28,%rsp
  406504:	48 89 3c 24          	mov    %rdi,(%rsp)
  406508:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40650d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406512:	48 8b 0c 24          	mov    (%rsp),%rcx
  406516:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40651b:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  406520:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406525:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  40652b:	0f 94 c0             	sete   %al
  40652e:	24 01                	and    $0x1,%al
  406530:	3c 00                	cmp    $0x0,%al
  406532:	74 0e                	je     406542 <runtime::default_random_generator_proc.init-1+0x42>
  406534:	0f 31                	rdtsc
  406536:	48 c1 e2 20          	shl    $0x20,%rdx
  40653a:	48 09 d0             	or     %rdx,%rax
  40653d:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406542:	48 8b 3c 24          	mov    (%rsp),%rdi
  406546:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40654b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  406552:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406557:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40655c:	48 d1 e2             	shl    $1,%rdx
  40655f:	40 b6 01             	mov    $0x1,%sil
  406562:	31 c9                	xor    %ecx,%ecx
  406564:	40 f6 c6 01          	test   $0x1,%sil
  406568:	48 0f 45 ca          	cmovne %rdx,%rcx
  40656c:	48 83 c9 01          	or     $0x1,%rcx
  406570:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406574:	e8 b7 fe ff ff       	call   406430 <runtime::default_random_generator_proc.read_u64-0>
  406579:	48 8b 3c 24          	mov    (%rsp),%rdi
  40657d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406582:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406587:	48 03 08             	add    (%rax),%rcx
  40658a:	48 89 08             	mov    %rcx,(%rax)
  40658d:	e8 9e fe ff ff       	call   406430 <runtime::default_random_generator_proc.read_u64-0>
  406592:	48 83 c4 28          	add    $0x28,%rsp
  406596:	c3                   	ret
  406597:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40659e:	00 00 

00000000004065a0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  4065a0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  4065a5:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  4065aa:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4065af:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4065b4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4065b9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4065be:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4065c5:	00 00 
  4065c7:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4065cc:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  4065d0:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4065d5:	48 03 4a 20          	add    0x20(%rdx),%rcx
  4065d9:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  4065de:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4065e3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4065e8:	48 83 e8 01          	sub    $0x1,%rax
  4065ec:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4065f1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4065f6:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  4065fb:	48 83 f8 00          	cmp    $0x0,%rax
  4065ff:	0f 95 c0             	setne  %al
  406602:	24 01                	and    $0x1,%al
  406604:	3c 00                	cmp    $0x0,%al
  406606:	74 17                	je     40661f <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  406608:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40660d:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406612:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  406617:	48 29 c8             	sub    %rcx,%rax
  40661a:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40661f:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406624:	c3                   	ret
  406625:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40662c:	00 00 00 00 

0000000000406630 <runtime::arena_alloc.align_forward_uint-0>:
  406630:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  406635:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40663a:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40663f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406644:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406649:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40664e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406653:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406658:	48 83 e9 01          	sub    $0x1,%rcx
  40665c:	48 21 c8             	and    %rcx,%rax
  40665f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406664:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  40666a:	0f 95 c0             	setne  %al
  40666d:	24 01                	and    $0x1,%al
  40666f:	3c 00                	cmp    $0x0,%al
  406671:	74 14                	je     406687 <runtime::arena_alloc.align_forward_uint-0+0x57>
  406673:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406678:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  40667d:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  406682:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406687:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40668c:	c3                   	ret
  40668d:	0f 1f 00             	nopl   (%rax)

0000000000406690 <runtime::matrix_bounds_check_error.handle_error-0>:
  406690:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  406697:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40669c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4066a1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4066a5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4066a9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4066ae:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4066b3:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  4066ba:	00 
  4066bb:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4066c0:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4066c7:	00 
  4066c8:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4066cd:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4066d2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  4066d7:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4066dc:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  4066e1:	8b 44 24 18          	mov    0x18(%rsp),%eax
  4066e5:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4066e9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4066ee:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4066f3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  4066fa:	00 
  4066fb:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  406702:	00 
  406703:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  40670a:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  406711:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  406718:	00 
  406719:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  406720:	00 
  406721:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  406726:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  40672b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40672e:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406733:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406738:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40673f:	00 00 
  406741:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406746:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40674b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406752:	00 00 
  406754:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  406759:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40675e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  406762:	89 44 24 54          	mov    %eax,0x54(%rsp)
  406766:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  40676b:	e8 40 d7 ff ff       	call   403eb0 <runtime::print_caller_location>
  406770:	bf 80 73 40 00       	mov    $0x407380,%edi
  406775:	be 11 00 00 00       	mov    $0x11,%esi
  40677a:	e8 11 cf ff ff       	call   403690 <runtime::print_string>
  40677f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406784:	e8 27 d5 ff ff       	call   403cb0 <runtime::print_i64>
  406789:	bf 92 73 40 00       	mov    $0x407392,%edi
  40678e:	be 02 00 00 00       	mov    $0x2,%esi
  406793:	e8 f8 ce ff ff       	call   403690 <runtime::print_string>
  406798:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40679d:	e8 0e d5 ff ff       	call   403cb0 <runtime::print_i64>
  4067a2:	bf 95 73 40 00       	mov    $0x407395,%edi
  4067a7:	be 16 00 00 00       	mov    $0x16,%esi
  4067ac:	e8 df ce ff ff       	call   403690 <runtime::print_string>
  4067b1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4067b6:	e8 f5 d4 ff ff       	call   403cb0 <runtime::print_i64>
  4067bb:	bf ac 73 40 00       	mov    $0x4073ac,%edi
  4067c0:	be 06 00 00 00       	mov    $0x6,%esi
  4067c5:	e8 c6 ce ff ff       	call   403690 <runtime::print_string>
  4067ca:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4067cf:	e8 dc d4 ff ff       	call   403cb0 <runtime::print_i64>
  4067d4:	bf b3 73 40 00       	mov    $0x4073b3,%edi
  4067d9:	be 01 00 00 00       	mov    $0x1,%esi
  4067de:	e8 ad ce ff ff       	call   403690 <runtime::print_string>
  4067e3:	bf 0a 00 00 00       	mov    $0xa,%edi
  4067e8:	e8 d3 d0 ff ff       	call   4038c0 <runtime::print_byte>
  4067ed:	e8 1e ab ff ff       	call   401310 <runtime::bounds_trap>
  4067f2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4067f9:	1f 84 00 00 00 00 00 

0000000000406800 <runtime::assert.internal-0>:
  406800:	48 83 ec 38          	sub    $0x38,%rsp
  406804:	48 89 0c 24          	mov    %rcx,(%rsp)
  406808:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40680d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406812:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406817:	48 8b 04 24          	mov    (%rsp),%rax
  40681b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406820:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406825:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40682a:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40682f:	48 8b 40 20          	mov    0x20(%rax),%rax
  406833:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406838:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  40683e:	0f 94 c0             	sete   %al
  406841:	24 01                	and    $0x1,%al
  406843:	3c 00                	cmp    $0x0,%al
  406845:	74 0c                	je     406853 <runtime::assert.internal-0+0x53>
  406847:	48 c7 c0 40 51 40 00 	mov    $0x405140,%rax
  40684e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406853:	4c 8b 0c 24          	mov    (%rsp),%r9
  406857:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40685c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406861:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406866:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40686b:	bf b5 73 40 00       	mov    $0x4073b5,%edi
  406870:	be 11 00 00 00       	mov    $0x11,%esi
  406875:	ff d0                	call   *%rax

Disassembly of section .fini:

0000000000406878 <_fini>:
  406878:	f3 0f 1e fa          	endbr64
  40687c:	48 83 ec 08          	sub    $0x8,%rsp
  406880:	48 83 c4 08          	add    $0x8,%rsp
  406884:	c3                   	ret
