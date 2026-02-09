
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
  4010b8:	48 c7 c7 b0 2a 40 00 	mov    $0x402ab0,%rdi
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

0000000000401190 <journey::main>:
  401190:	55                   	push   %rbp
  401191:	48 89 e5             	mov    %rsp,%rbp
  401194:	48 83 e4 c0          	and    $0xffffffffffffffc0,%rsp
  401198:	48 81 ec c0 01 00 00 	sub    $0x1c0,%rsp
  40119f:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4011a4:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4011a9:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  4011ae:	e8 05 00 00 00       	call   4011b8 <journey::init_world:proc(world:^journey::World($PAGE_COUNT=1, $THREAD_COUNT=4),unique_data_capacity:$$24)>
  4011b3:	48 89 ec             	mov    %rbp,%rsp
  4011b6:	5d                   	pop    %rbp
  4011b7:	c3                   	ret

00000000004011b8 <journey::init_world:proc(world:^journey::World($PAGE_COUNT=1, $THREAD_COUNT=4),unique_data_capacity:$$24)>:
  4011b8:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  4011bd:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4011c2:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4011c7:	31 c0                	xor    %eax,%eax
  4011c9:	41 89 c1             	mov    %eax,%r9d
  4011cc:	b8 09 00 00 00       	mov    $0x9,%eax
  4011d1:	be 00 90 01 00       	mov    $0x19000,%esi
  4011d6:	ba 03 00 00 00       	mov    $0x3,%edx
  4011db:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4011e1:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4011e8:	4c 89 cf             	mov    %r9,%rdi
  4011eb:	0f 05                	syscall
  4011ed:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4011f2:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4011f7:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4011fc:	48 89 08             	mov    %rcx,(%rax)
  4011ff:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401204:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401209:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  401210:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401214:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401219:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40121e:	b8 ba 00 00 00       	mov    $0xba,%eax
  401223:	0f 05                	syscall
  401225:	48 89 c1             	mov    %rax,%rcx
  401228:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40122d:	48 89 48 10          	mov    %rcx,0x10(%rax)
  401231:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401236:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40123b:	31 c0                	xor    %eax,%eax
  40123d:	41 89 c1             	mov    %eax,%r9d
  401240:	b8 09 00 00 00       	mov    $0x9,%eax
  401245:	be 00 00 02 00       	mov    $0x20000,%esi
  40124a:	ba 03 00 00 00       	mov    $0x3,%edx
  40124f:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401255:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  40125c:	4c 89 cf             	mov    %r9,%rdi
  40125f:	0f 05                	syscall
  401261:	48 89 c1             	mov    %rax,%rcx
  401264:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401269:	48 89 48 18          	mov    %rcx,0x18(%rax)
  40126d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401272:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401277:	31 c0                	xor    %eax,%eax
  401279:	41 89 c1             	mov    %eax,%r9d
  40127c:	b8 09 00 00 00       	mov    $0x9,%eax
  401281:	be 00 00 04 00       	mov    $0x40000,%esi
  401286:	ba 03 00 00 00       	mov    $0x3,%edx
  40128b:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401291:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401298:	4c 89 cf             	mov    %r9,%rdi
  40129b:	0f 05                	syscall
  40129d:	48 89 c1             	mov    %rax,%rcx
  4012a0:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4012a5:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4012a9:	c3                   	ret
  4012aa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004012b0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  4012b0:	50                   	push   %rax
  4012b1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4012b5:	eb 00                	jmp    4012b7 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x7>
  4012b7:	48 8b 34 24          	mov    (%rsp),%rsi
  4012bb:	48 c7 c0 c0 ff ff ff 	mov    $0xffffffffffffffc0,%rax
  4012c2:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  4012c9:	00 00 
  4012cb:	48 01 c7             	add    %rax,%rdi
  4012ce:	e8 8d 11 00 00       	call   402460 <runtime::default_temp_allocator_destroy>
  4012d3:	58                   	pop    %rax
  4012d4:	c3                   	ret
  4012d5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4012dc:	00 00 00 00 

00000000004012e0 <runtime::bounds_trap>:
  4012e0:	eb 00                	jmp    4012e2 <runtime::bounds_trap+0x2>
  4012e2:	0f 0b                	ud2
  4012e4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4012eb:	00 00 00 00 00 

00000000004012f0 <runtime::heap_allocator>:
  4012f0:	48 c7 c0 90 1d 40 00 	mov    $0x401d90,%rax
  4012f7:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4012fc:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401303:	00 00 
  401305:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40130a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40130f:	c3                   	ret

0000000000401310 <runtime::udivmod128>:
  401310:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  401317:	4c 89 44 24 a8       	mov    %r8,-0x58(%rsp)
  40131c:	48 89 4c 24 b0       	mov    %rcx,-0x50(%rsp)
  401321:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401326:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40132b:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  401330:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401335:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40133a:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40133f:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  401344:	48 8b 7c 24 a8       	mov    -0x58(%rsp),%rdi
  401349:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  401350:	00 
  401351:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  401358:	00 
  401359:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  401360:	00 
  401361:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  401368:	00 
  401369:	48 89 bc 24 88 00 00 	mov    %rdi,0x88(%rsp)
  401370:	00 
  401371:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  401376:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  40137b:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  401380:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  401385:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  40138a:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  40138f:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  401394:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401399:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40139e:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4013a3:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4013a8:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4013ad:	0f 57 c0             	xorps  %xmm0,%xmm0
  4013b0:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4013b5:	0f 57 c0             	xorps  %xmm0,%xmm0
  4013b8:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  4013bd:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  4013c4:	00 
  4013c5:	48 83 7c 24 68 00    	cmpq   $0x0,0x68(%rsp)
  4013cb:	0f 94 c0             	sete   %al
  4013ce:	24 01                	and    $0x1,%al
  4013d0:	3c 00                	cmp    $0x0,%al
  4013d2:	0f 84 a1 00 00 00    	je     401479 <runtime::udivmod128+0x169>
  4013d8:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  4013de:	0f 94 c0             	sete   %al
  4013e1:	24 01                	and    $0x1,%al
  4013e3:	3c 00                	cmp    $0x0,%al
  4013e5:	74 5c                	je     401443 <runtime::udivmod128+0x133>
  4013e7:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4013ec:	48 83 f8 00          	cmp    $0x0,%rax
  4013f0:	0f 95 c0             	setne  %al
  4013f3:	24 01                	and    $0x1,%al
  4013f5:	3c 00                	cmp    $0x0,%al
  4013f7:	74 29                	je     401422 <runtime::udivmod128+0x112>
  4013f9:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4013fe:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401403:	31 d2                	xor    %edx,%edx
  401405:	48 f7 f1             	div    %rcx
  401408:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40140d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  401412:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401417:	48 89 08             	mov    %rcx,(%rax)
  40141a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401421:	00 
  401422:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401427:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40142c:	31 d2                	xor    %edx,%edx
  40142e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  401433:	48 f7 f1             	div    %rcx
  401436:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  40143b:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401442:	c3                   	ret
  401443:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401448:	48 83 f8 00          	cmp    $0x0,%rax
  40144c:	0f 95 c0             	setne  %al
  40144f:	24 01                	and    $0x1,%al
  401451:	3c 00                	cmp    $0x0,%al
  401453:	74 15                	je     40146a <runtime::udivmod128+0x15a>
  401455:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40145a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40145f:	48 89 08             	mov    %rcx,(%rax)
  401462:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401469:	00 
  40146a:	31 c0                	xor    %eax,%eax
  40146c:	89 c2                	mov    %eax,%edx
  40146e:	48 89 d0             	mov    %rdx,%rax
  401471:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401478:	c3                   	ret
  401479:	48 83 7c 24 40 00    	cmpq   $0x0,0x40(%rsp)
  40147f:	0f 94 c0             	sete   %al
  401482:	24 01                	and    $0x1,%al
  401484:	3c 00                	cmp    $0x0,%al
  401486:	0f 84 8b 02 00 00    	je     401717 <runtime::udivmod128+0x407>
  40148c:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401492:	0f 94 c0             	sete   %al
  401495:	24 01                	and    $0x1,%al
  401497:	3c 00                	cmp    $0x0,%al
  401499:	74 52                	je     4014ed <runtime::udivmod128+0x1dd>
  40149b:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4014a0:	48 83 f8 00          	cmp    $0x0,%rax
  4014a4:	0f 95 c0             	setne  %al
  4014a7:	24 01                	and    $0x1,%al
  4014a9:	3c 00                	cmp    $0x0,%al
  4014ab:	74 1f                	je     4014cc <runtime::udivmod128+0x1bc>
  4014ad:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4014b2:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4014b7:	31 d2                	xor    %edx,%edx
  4014b9:	48 f7 f1             	div    %rcx
  4014bc:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4014c1:	48 89 10             	mov    %rdx,(%rax)
  4014c4:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4014cb:	00 
  4014cc:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4014d1:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4014d6:	31 d2                	xor    %edx,%edx
  4014d8:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  4014dd:	48 f7 f1             	div    %rcx
  4014e0:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  4014e5:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4014ec:	c3                   	ret
  4014ed:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  4014f3:	0f 94 c0             	sete   %al
  4014f6:	24 01                	and    $0x1,%al
  4014f8:	3c 00                	cmp    $0x0,%al
  4014fa:	74 66                	je     401562 <runtime::udivmod128+0x252>
  4014fc:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401501:	48 83 f8 00          	cmp    $0x0,%rax
  401505:	0f 95 c0             	setne  %al
  401508:	24 01                	and    $0x1,%al
  40150a:	3c 00                	cmp    $0x0,%al
  40150c:	74 33                	je     401541 <runtime::udivmod128+0x231>
  40150e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401513:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401518:	31 d2                	xor    %edx,%edx
  40151a:	48 f7 f1             	div    %rcx
  40151d:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401522:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401527:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  40152e:	00 00 
  401530:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401535:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40153a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40153e:	48 89 08             	mov    %rcx,(%rax)
  401541:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401546:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40154b:	31 d2                	xor    %edx,%edx
  40154d:	48 89 54 24 90       	mov    %rdx,-0x70(%rsp)
  401552:	48 f7 f1             	div    %rcx
  401555:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  40155a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401561:	c3                   	ret
  401562:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401567:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40156c:	48 83 e9 01          	sub    $0x1,%rcx
  401570:	48 21 c8             	and    %rcx,%rax
  401573:	48 83 f8 00          	cmp    $0x0,%rax
  401577:	0f 94 c0             	sete   %al
  40157a:	24 01                	and    $0x1,%al
  40157c:	3c 00                	cmp    $0x0,%al
  40157e:	74 7a                	je     4015fa <runtime::udivmod128+0x2ea>
  401580:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401585:	48 83 f8 00          	cmp    $0x0,%rax
  401589:	0f 95 c0             	setne  %al
  40158c:	24 01                	and    $0x1,%al
  40158e:	3c 00                	cmp    $0x0,%al
  401590:	74 35                	je     4015c7 <runtime::udivmod128+0x2b7>
  401592:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401597:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40159c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4015a1:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4015a6:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4015ab:	48 ff ca             	dec    %rdx
  4015ae:	48 21 d1             	and    %rdx,%rcx
  4015b1:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4015b6:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4015bb:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4015c0:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4015c4:	48 89 08             	mov    %rcx,(%rax)
  4015c7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4015cc:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4015d1:	ba 40 00 00 00       	mov    $0x40,%edx
  4015d6:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  4015db:	88 d1                	mov    %dl,%cl
  4015dd:	48 d3 e8             	shr    %cl,%rax
  4015e0:	48 89 c1             	mov    %rax,%rcx
  4015e3:	31 c0                	xor    %eax,%eax
  4015e5:	48 83 ea 40          	sub    $0x40,%rdx
  4015e9:	89 c2                	mov    %eax,%edx
  4015eb:	48 89 d0             	mov    %rdx,%rax
  4015ee:	48 0f 42 c1          	cmovb  %rcx,%rax
  4015f2:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4015f9:	c3                   	ret
  4015fa:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4015ff:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401604:	48 0f bd c1          	bsr    %rcx,%rax
  401608:	48 83 f0 3f          	xor    $0x3f,%rax
  40160c:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401611:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401616:	48 0f bd ca          	bsr    %rdx,%rcx
  40161a:	48 83 f1 3f          	xor    $0x3f,%rcx
  40161e:	29 c8                	sub    %ecx,%eax
  401620:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401624:	83 7c 24 1c 3e       	cmpl   $0x3e,0x1c(%rsp)
  401629:	0f 97 c0             	seta   %al
  40162c:	24 01                	and    $0x1,%al
  40162e:	3c 00                	cmp    $0x0,%al
  401630:	74 37                	je     401669 <runtime::udivmod128+0x359>
  401632:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401637:	48 83 f8 00          	cmp    $0x0,%rax
  40163b:	0f 95 c0             	setne  %al
  40163e:	24 01                	and    $0x1,%al
  401640:	3c 00                	cmp    $0x0,%al
  401642:	74 16                	je     40165a <runtime::udivmod128+0x34a>
  401644:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401649:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  40164e:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401653:	48 89 10             	mov    %rdx,(%rax)
  401656:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40165a:	31 c0                	xor    %eax,%eax
  40165c:	89 c2                	mov    %eax,%edx
  40165e:	48 89 d0             	mov    %rdx,%rax
  401661:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401668:	c3                   	ret
  401669:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  40166d:	83 c0 01             	add    $0x1,%eax
  401670:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401674:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40167b:	00 00 
  40167d:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401682:	b9 40 00 00 00       	mov    $0x40,%ecx
  401687:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40168b:	89 c9                	mov    %ecx,%ecx
  40168d:	89 ca                	mov    %ecx,%edx
  40168f:	48 89 d1             	mov    %rdx,%rcx
  401692:	48 d3 e0             	shl    %cl,%rax
  401695:	48 89 c1             	mov    %rax,%rcx
  401698:	31 c0                	xor    %eax,%eax
  40169a:	48 83 fa 40          	cmp    $0x40,%rdx
  40169e:	48 0f 42 c1          	cmovb  %rcx,%rax
  4016a2:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4016a7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4016ac:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4016b0:	89 ca                	mov    %ecx,%edx
  4016b2:	48 89 d1             	mov    %rdx,%rcx
  4016b5:	48 d3 e8             	shr    %cl,%rax
  4016b8:	48 89 c1             	mov    %rax,%rcx
  4016bb:	31 c0                	xor    %eax,%eax
  4016bd:	48 83 fa 40          	cmp    $0x40,%rdx
  4016c1:	48 0f 42 c1          	cmovb  %rcx,%rax
  4016c5:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4016ca:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4016cf:	b9 40 00 00 00       	mov    $0x40,%ecx
  4016d4:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4016d8:	89 c9                	mov    %ecx,%ecx
  4016da:	89 ca                	mov    %ecx,%edx
  4016dc:	48 89 d1             	mov    %rdx,%rcx
  4016df:	48 d3 e0             	shl    %cl,%rax
  4016e2:	48 89 c1             	mov    %rax,%rcx
  4016e5:	31 c0                	xor    %eax,%eax
  4016e7:	48 83 fa 40          	cmp    $0x40,%rdx
  4016eb:	48 0f 42 c1          	cmovb  %rcx,%rax
  4016ef:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4016f4:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4016f8:	89 ce                	mov    %ecx,%esi
  4016fa:	48 89 f1             	mov    %rsi,%rcx
  4016fd:	48 d3 ea             	shr    %cl,%rdx
  401700:	31 c9                	xor    %ecx,%ecx
  401702:	48 83 fe 40          	cmp    $0x40,%rsi
  401706:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40170a:	48 09 c8             	or     %rcx,%rax
  40170d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401712:	e9 30 04 00 00       	jmp    401b47 <runtime::udivmod128+0x837>
  401717:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  40171d:	0f 94 c0             	sete   %al
  401720:	24 01                	and    $0x1,%al
  401722:	3c 00                	cmp    $0x0,%al
  401724:	0f 84 d1 02 00 00    	je     4019fb <runtime::udivmod128+0x6eb>
  40172a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40172f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401734:	48 83 e9 01          	sub    $0x1,%rcx
  401738:	48 21 c8             	and    %rcx,%rax
  40173b:	48 83 f8 00          	cmp    $0x0,%rax
  40173f:	0f 94 c0             	sete   %al
  401742:	24 01                	and    $0x1,%al
  401744:	3c 00                	cmp    $0x0,%al
  401746:	0f 84 de 00 00 00    	je     40182a <runtime::udivmod128+0x51a>
  40174c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401751:	48 83 f8 00          	cmp    $0x0,%rax
  401755:	0f 95 c0             	setne  %al
  401758:	24 01                	and    $0x1,%al
  40175a:	3c 00                	cmp    $0x0,%al
  40175c:	74 20                	je     40177e <runtime::udivmod128+0x46e>
  40175e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401763:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401768:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40176d:	48 ff ca             	dec    %rdx
  401770:	48 21 d1             	and    %rdx,%rcx
  401773:	48 89 08             	mov    %rcx,(%rax)
  401776:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40177d:	00 
  40177e:	48 83 7c 24 40 01    	cmpq   $0x1,0x40(%rsp)
  401784:	0f 94 c0             	sete   %al
  401787:	24 01                	and    $0x1,%al
  401789:	3c 00                	cmp    $0x0,%al
  40178b:	74 12                	je     40179f <runtime::udivmod128+0x48f>
  40178d:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401792:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401797:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40179e:	c3                   	ret
  40179f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4017a4:	b8 40 00 00 00       	mov    $0x40,%eax
  4017a9:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  4017ae:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4017b2:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  4017b7:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  4017bb:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  4017bf:	88 c1                	mov    %al,%cl
  4017c1:	48 d3 ea             	shr    %cl,%rdx
  4017c4:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  4017c8:	31 c0                	xor    %eax,%eax
  4017ca:	83 e9 40             	sub    $0x40,%ecx
  4017cd:	48 89 c1             	mov    %rax,%rcx
  4017d0:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4017d4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4017d9:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4017de:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  4017e3:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  4017e7:	40 88 f1             	mov    %sil,%cl
  4017ea:	48 d3 ef             	shr    %cl,%rdi
  4017ed:	83 ee 40             	sub    $0x40,%esi
  4017f0:	48 89 c1             	mov    %rax,%rcx
  4017f3:	48 0f 42 cf          	cmovb  %rdi,%rcx
  4017f7:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  4017fc:	f7 de                	neg    %esi
  4017fe:	40 88 f1             	mov    %sil,%cl
  401801:	48 d3 e2             	shl    %cl,%rdx
  401804:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  401809:	83 ee 40             	sub    $0x40,%esi
  40180c:	48 0f 42 c2          	cmovb  %rdx,%rax
  401810:	48 09 c8             	or     %rcx,%rax
  401813:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401818:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40181d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401822:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401829:	c3                   	ret
  40182a:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40182f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401834:	48 0f bd c1          	bsr    %rcx,%rax
  401838:	48 83 f0 3f          	xor    $0x3f,%rax
  40183c:	83 c0 41             	add    $0x41,%eax
  40183f:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401844:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401849:	48 0f bd ca          	bsr    %rdx,%rcx
  40184d:	48 83 f1 3f          	xor    $0x3f,%rcx
  401851:	29 c8                	sub    %ecx,%eax
  401853:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401857:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  40185c:	0f 94 c1             	sete   %cl
  40185f:	80 e1 01             	and    $0x1,%cl
  401862:	b0 01                	mov    $0x1,%al
  401864:	38 c8                	cmp    %cl,%al
  401866:	74 13                	je     40187b <runtime::udivmod128+0x56b>
  401868:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  40186d:	0f 92 c1             	setb   %cl
  401870:	80 e1 01             	and    $0x1,%cl
  401873:	b0 01                	mov    $0x1,%al
  401875:	38 c8                	cmp    %cl,%al
  401877:	74 32                	je     4018ab <runtime::udivmod128+0x59b>
  401879:	eb 2b                	jmp    4018a6 <runtime::udivmod128+0x596>
  40187b:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401882:	00 00 
  401884:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401889:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40188e:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401895:	00 00 
  401897:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40189c:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4018a1:	e9 50 01 00 00       	jmp    4019f6 <runtime::udivmod128+0x6e6>
  4018a6:	e9 a3 00 00 00       	jmp    40194e <runtime::udivmod128+0x63e>
  4018ab:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4018b2:	00 00 
  4018b4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4018b9:	b9 40 00 00 00       	mov    $0x40,%ecx
  4018be:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4018c2:	89 c9                	mov    %ecx,%ecx
  4018c4:	89 ca                	mov    %ecx,%edx
  4018c6:	48 89 d1             	mov    %rdx,%rcx
  4018c9:	48 d3 e0             	shl    %cl,%rax
  4018cc:	48 89 c1             	mov    %rax,%rcx
  4018cf:	31 c0                	xor    %eax,%eax
  4018d1:	48 83 fa 40          	cmp    $0x40,%rdx
  4018d5:	48 0f 42 c1          	cmovb  %rcx,%rax
  4018d9:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4018de:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4018e3:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4018e7:	89 ca                	mov    %ecx,%edx
  4018e9:	48 89 d1             	mov    %rdx,%rcx
  4018ec:	48 d3 e8             	shr    %cl,%rax
  4018ef:	48 89 c1             	mov    %rax,%rcx
  4018f2:	31 c0                	xor    %eax,%eax
  4018f4:	48 83 fa 40          	cmp    $0x40,%rdx
  4018f8:	48 0f 42 c1          	cmovb  %rcx,%rax
  4018fc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401901:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401906:	b9 40 00 00 00       	mov    $0x40,%ecx
  40190b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40190f:	89 c9                	mov    %ecx,%ecx
  401911:	89 ca                	mov    %ecx,%edx
  401913:	48 89 d1             	mov    %rdx,%rcx
  401916:	48 d3 e0             	shl    %cl,%rax
  401919:	48 89 c1             	mov    %rax,%rcx
  40191c:	31 c0                	xor    %eax,%eax
  40191e:	48 83 fa 40          	cmp    $0x40,%rdx
  401922:	48 0f 42 c1          	cmovb  %rcx,%rax
  401926:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40192b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40192f:	89 ce                	mov    %ecx,%esi
  401931:	48 89 f1             	mov    %rsi,%rcx
  401934:	48 d3 ea             	shr    %cl,%rdx
  401937:	31 c9                	xor    %ecx,%ecx
  401939:	48 83 fe 40          	cmp    $0x40,%rsi
  40193d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401941:	48 09 c8             	or     %rcx,%rax
  401944:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401949:	e9 a8 00 00 00       	jmp    4019f6 <runtime::udivmod128+0x6e6>
  40194e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401953:	b9 80 00 00 00       	mov    $0x80,%ecx
  401958:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40195c:	89 c9                	mov    %ecx,%ecx
  40195e:	89 ca                	mov    %ecx,%edx
  401960:	48 89 d1             	mov    %rdx,%rcx
  401963:	48 d3 e0             	shl    %cl,%rax
  401966:	48 89 c1             	mov    %rax,%rcx
  401969:	31 c0                	xor    %eax,%eax
  40196b:	48 83 fa 40          	cmp    $0x40,%rdx
  40196f:	48 0f 42 c1          	cmovb  %rcx,%rax
  401973:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401978:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40197d:	b9 80 00 00 00       	mov    $0x80,%ecx
  401982:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401986:	89 c9                	mov    %ecx,%ecx
  401988:	89 ca                	mov    %ecx,%edx
  40198a:	48 89 d1             	mov    %rdx,%rcx
  40198d:	48 d3 e0             	shl    %cl,%rax
  401990:	48 89 c1             	mov    %rax,%rcx
  401993:	31 c0                	xor    %eax,%eax
  401995:	48 83 fa 40          	cmp    $0x40,%rdx
  401999:	48 0f 42 c1          	cmovb  %rcx,%rax
  40199d:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4019a2:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4019a6:	83 e9 40             	sub    $0x40,%ecx
  4019a9:	89 c9                	mov    %ecx,%ecx
  4019ab:	89 ce                	mov    %ecx,%esi
  4019ad:	48 89 f1             	mov    %rsi,%rcx
  4019b0:	48 d3 ea             	shr    %cl,%rdx
  4019b3:	31 c9                	xor    %ecx,%ecx
  4019b5:	48 83 fe 40          	cmp    $0x40,%rsi
  4019b9:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4019bd:	48 09 c8             	or     %rcx,%rax
  4019c0:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4019c5:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4019cc:	00 00 
  4019ce:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4019d3:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4019d7:	83 e9 40             	sub    $0x40,%ecx
  4019da:	89 c9                	mov    %ecx,%ecx
  4019dc:	89 ca                	mov    %ecx,%edx
  4019de:	48 89 d1             	mov    %rdx,%rcx
  4019e1:	48 d3 e8             	shr    %cl,%rax
  4019e4:	48 89 c1             	mov    %rax,%rcx
  4019e7:	31 c0                	xor    %eax,%eax
  4019e9:	48 83 fa 40          	cmp    $0x40,%rdx
  4019ed:	48 0f 42 c1          	cmovb  %rcx,%rax
  4019f1:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4019f6:	e9 4a 01 00 00       	jmp    401b45 <runtime::udivmod128+0x835>
  4019fb:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401a00:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401a05:	48 0f bd c1          	bsr    %rcx,%rax
  401a09:	48 83 f0 3f          	xor    $0x3f,%rax
  401a0d:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401a12:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401a17:	48 0f bd ca          	bsr    %rdx,%rcx
  401a1b:	48 83 f1 3f          	xor    $0x3f,%rcx
  401a1f:	29 c8                	sub    %ecx,%eax
  401a21:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401a25:	83 7c 24 1c 3f       	cmpl   $0x3f,0x1c(%rsp)
  401a2a:	0f 97 c0             	seta   %al
  401a2d:	24 01                	and    $0x1,%al
  401a2f:	3c 00                	cmp    $0x0,%al
  401a31:	74 37                	je     401a6a <runtime::udivmod128+0x75a>
  401a33:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a38:	48 83 f8 00          	cmp    $0x0,%rax
  401a3c:	0f 95 c0             	setne  %al
  401a3f:	24 01                	and    $0x1,%al
  401a41:	3c 00                	cmp    $0x0,%al
  401a43:	74 16                	je     401a5b <runtime::udivmod128+0x74b>
  401a45:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a4a:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  401a4f:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401a54:	48 89 10             	mov    %rdx,(%rax)
  401a57:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401a5b:	31 c0                	xor    %eax,%eax
  401a5d:	89 c2                	mov    %eax,%edx
  401a5f:	48 89 d0             	mov    %rdx,%rax
  401a62:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401a69:	c3                   	ret
  401a6a:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401a6e:	83 c0 01             	add    $0x1,%eax
  401a71:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401a75:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401a7c:	00 00 
  401a7e:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401a83:	0f 94 c0             	sete   %al
  401a86:	24 01                	and    $0x1,%al
  401a88:	3c 00                	cmp    $0x0,%al
  401a8a:	74 22                	je     401aae <runtime::udivmod128+0x79e>
  401a8c:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401a91:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401a96:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401a9d:	00 00 
  401a9f:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401aa4:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401aa9:	e9 95 00 00 00       	jmp    401b43 <runtime::udivmod128+0x833>
  401aae:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401ab3:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401ab7:	89 ca                	mov    %ecx,%edx
  401ab9:	48 89 d1             	mov    %rdx,%rcx
  401abc:	48 d3 e8             	shr    %cl,%rax
  401abf:	48 89 c1             	mov    %rax,%rcx
  401ac2:	31 c0                	xor    %eax,%eax
  401ac4:	48 83 fa 40          	cmp    $0x40,%rdx
  401ac8:	48 0f 42 c1          	cmovb  %rcx,%rax
  401acc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401ad1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401ad6:	b9 40 00 00 00       	mov    $0x40,%ecx
  401adb:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401adf:	89 c9                	mov    %ecx,%ecx
  401ae1:	89 ca                	mov    %ecx,%edx
  401ae3:	48 89 d1             	mov    %rdx,%rcx
  401ae6:	48 d3 e0             	shl    %cl,%rax
  401ae9:	48 89 c1             	mov    %rax,%rcx
  401aec:	31 c0                	xor    %eax,%eax
  401aee:	48 83 fa 40          	cmp    $0x40,%rdx
  401af2:	48 0f 42 c1          	cmovb  %rcx,%rax
  401af6:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401afb:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401aff:	89 ce                	mov    %ecx,%esi
  401b01:	48 89 f1             	mov    %rsi,%rcx
  401b04:	48 d3 ea             	shr    %cl,%rdx
  401b07:	31 c9                	xor    %ecx,%ecx
  401b09:	48 83 fe 40          	cmp    $0x40,%rsi
  401b0d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401b11:	48 09 c8             	or     %rcx,%rax
  401b14:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401b19:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401b1e:	b9 40 00 00 00       	mov    $0x40,%ecx
  401b23:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401b27:	89 c9                	mov    %ecx,%ecx
  401b29:	89 ca                	mov    %ecx,%edx
  401b2b:	48 89 d1             	mov    %rdx,%rcx
  401b2e:	48 d3 e0             	shl    %cl,%rax
  401b31:	48 89 c1             	mov    %rax,%rcx
  401b34:	31 c0                	xor    %eax,%eax
  401b36:	48 83 fa 40          	cmp    $0x40,%rdx
  401b3a:	48 0f 42 c1          	cmovb  %rcx,%rax
  401b3e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401b43:	eb 00                	jmp    401b45 <runtime::udivmod128+0x835>
  401b45:	eb 00                	jmp    401b47 <runtime::udivmod128+0x837>
  401b47:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  401b4e:	00 
  401b4f:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401b56:	00 00 
  401b58:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  401b5f:	00 00 
  401b61:	83 7c 24 1c 00       	cmpl   $0x0,0x1c(%rsp)
  401b66:	0f 97 c0             	seta   %al
  401b69:	24 01                	and    $0x1,%al
  401b6b:	3c 00                	cmp    $0x0,%al
  401b6d:	0f 84 eb 00 00 00    	je     401c5e <runtime::udivmod128+0x94e>
  401b73:	48 8b 74 24 b8       	mov    -0x48(%rsp),%rsi
  401b78:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  401b7d:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401b82:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401b87:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401b8c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401b91:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401b96:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  401b9b:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401ba0:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401ba5:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401baa:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  401baf:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401bb4:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401bb9:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401bbe:	48 01 c0             	add    %rax,%rax
  401bc1:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  401bc5:	48 09 c8             	or     %rcx,%rax
  401bc8:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401bcd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401bd2:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  401bd7:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  401bdc:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401be1:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401be6:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401beb:	48 f7 d0             	not    %rax
  401bee:	48 f7 d1             	not    %rcx
  401bf1:	48 01 f1             	add    %rsi,%rcx
  401bf4:	48 11 d0             	adc    %rdx,%rax
  401bf7:	48 c1 f8 3f          	sar    $0x3f,%rax
  401bfb:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401c00:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401c05:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401c09:	83 e0 01             	and    $0x1,%eax
  401c0c:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  401c10:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401c15:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401c1a:	48 21 ca             	and    %rcx,%rdx
  401c1d:	48 21 c6             	and    %rax,%rsi
  401c20:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401c25:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401c2a:	48 29 f1             	sub    %rsi,%rcx
  401c2d:	48 19 d0             	sbb    %rdx,%rax
  401c30:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401c35:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401c3a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401c3f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401c44:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401c49:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401c4e:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401c52:	83 e8 01             	sub    $0x1,%eax
  401c55:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401c59:	e9 03 ff ff ff       	jmp    401b61 <runtime::udivmod128+0x851>
  401c5e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401c63:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401c68:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401c6d:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  401c72:	48 01 c9             	add    %rcx,%rcx
  401c75:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  401c79:	48 09 f1             	or     %rsi,%rcx
  401c7c:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  401c81:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  401c86:	48 83 f8 00          	cmp    $0x0,%rax
  401c8a:	0f 95 c0             	setne  %al
  401c8d:	24 01                	and    $0x1,%al
  401c8f:	3c 00                	cmp    $0x0,%al
  401c91:	74 16                	je     401ca9 <runtime::udivmod128+0x999>
  401c93:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401c98:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401c9d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  401ca2:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401ca6:	48 89 08             	mov    %rcx,(%rax)
  401ca9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401cae:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  401cb3:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401cba:	c3                   	ret
  401cbb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401cc0 <runtime::stderr_write>:
  401cc0:	48 83 ec 48          	sub    $0x48,%rsp
  401cc4:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  401cc9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  401cce:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  401cd3:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401cd8:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401cdd:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  401ce2:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  401ce7:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401cee:	00 00 
  401cf0:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  401cf5:	e8 16 00 00 00       	call   401d10 <runtime::[os_specific_linux.odin]::_stderr_write>
  401cfa:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401cff:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  401d04:	48 89 11             	mov    %rdx,(%rcx)
  401d07:	48 83 c4 48          	add    $0x48,%rsp
  401d0b:	c3                   	ret
  401d0c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401d10 <runtime::[os_specific_linux.odin]::_stderr_write>:
  401d10:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  401d15:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  401d1a:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  401d1f:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  401d24:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  401d29:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  401d2e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  401d33:	b8 01 00 00 00       	mov    $0x1,%eax
  401d38:	bf 02 00 00 00       	mov    $0x2,%edi
  401d3d:	0f 05                	syscall
  401d3f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401d44:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  401d4a:	0f 9c c0             	setl   %al
  401d4d:	24 01                	and    $0x1,%al
  401d4f:	3c 00                	cmp    $0x0,%al
  401d51:	74 26                	je     401d79 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  401d53:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  401d5a:	ff ff 
  401d5c:	0f 9f c0             	setg   %al
  401d5f:	24 01                	and    $0x1,%al
  401d61:	3c 00                	cmp    $0x0,%al
  401d63:	74 14                	je     401d79 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  401d65:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  401d6a:	31 c0                	xor    %eax,%eax
  401d6c:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  401d71:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  401d78:	c3                   	ret
  401d79:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401d7e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401d83:	48 89 08             	mov    %rcx,(%rax)
  401d86:	31 c0                	xor    %eax,%eax
  401d88:	c3                   	ret
  401d89:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401d90 <runtime::heap_allocator_proc>:
  401d90:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  401d97:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  401d9c:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  401da1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401da6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401dab:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  401db0:	40 88 f0             	mov    %sil,%al
  401db3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  401db7:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  401dbe:	00 
  401dbf:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401dc4:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  401dcb:	00 
  401dcc:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  401dd1:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401dd5:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401dda:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401ddf:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401de4:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401de9:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  401dee:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  401df5:	00 
  401df6:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  401dfd:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  401e04:	00 
  401e05:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  401e0c:	00 
  401e0d:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  401e14:	00 
  401e15:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  401e1c:	00 
  401e1d:	0f b6 c8             	movzbl %al,%ecx
  401e20:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  401e25:	2c 07                	sub    $0x7,%al
  401e27:	0f 87 5f 01 00 00    	ja     401f8c <runtime::heap_allocator_proc+0x1fc>
  401e2d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401e32:	48 8b 04 c5 10 70 40 	mov    0x407010(,%rax,8),%rax
  401e39:	00 
  401e3a:	ff e0                	jmp    *%rax
  401e3c:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401e41:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401e46:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401e4b:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401e4f:	84 c0                	test   %al,%al
  401e51:	0f 94 c0             	sete   %al
  401e54:	0f 57 c0             	xorps  %xmm0,%xmm0
  401e57:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  401e5e:	00 
  401e5f:	48 89 e1             	mov    %rsp,%rcx
  401e62:	48 89 11             	mov    %rdx,(%rcx)
  401e65:	44 0f b6 c0          	movzbl %al,%r8d
  401e69:	31 c0                	xor    %eax,%eax
  401e6b:	89 c1                	mov    %eax,%ecx
  401e6d:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  401e74:	00 
  401e75:	48 89 ca             	mov    %rcx,%rdx
  401e78:	e8 43 3f 00 00       	call   405dc0 <runtime::heap_allocator_proc.aligned_alloc-0>
  401e7d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401e82:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  401e89:	00 
  401e8a:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  401e91:	00 
  401e92:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401e96:	48 89 11             	mov    %rdx,(%rcx)
  401e99:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401ea0:	c3                   	ret
  401ea1:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  401ea6:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401eab:	e8 60 41 00 00       	call   406010 <runtime::heap_allocator_proc.aligned_free-1>
  401eb0:	e9 d7 00 00 00       	jmp    401f8c <runtime::heap_allocator_proc+0x1fc>
  401eb5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401eba:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401ec1:	00 
  401ec2:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401ec9:	b0 04                	mov    $0x4,%al
  401ecb:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401ed2:	c3                   	ret
  401ed3:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401ed8:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401edd:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401ee2:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  401ee7:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  401eec:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  401ef0:	2c 03                	sub    $0x3,%al
  401ef2:	0f 94 c0             	sete   %al
  401ef5:	0f 57 c0             	xorps  %xmm0,%xmm0
  401ef8:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  401efd:	49 89 e0             	mov    %rsp,%r8
  401f00:	4d 89 08             	mov    %r9,(%r8)
  401f03:	44 0f b6 c0          	movzbl %al,%r8d
  401f07:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  401f0c:	e8 3f 41 00 00       	call   406050 <runtime::heap_allocator_proc.aligned_resize-2>
  401f11:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401f16:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  401f1b:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  401f20:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  401f24:	48 89 11             	mov    %rdx,(%rcx)
  401f27:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401f2e:	c3                   	ret
  401f2f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401f34:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401f39:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  401f3f:	0f 95 c0             	setne  %al
  401f42:	24 01                	and    $0x1,%al
  401f44:	3c 00                	cmp    $0x0,%al
  401f46:	74 08                	je     401f50 <runtime::heap_allocator_proc+0x1c0>
  401f48:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  401f4d:	c6 00 db             	movb   $0xdb,(%rax)
  401f50:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401f55:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401f5c:	00 
  401f5d:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401f64:	31 c0                	xor    %eax,%eax
  401f66:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401f6d:	c3                   	ret
  401f6e:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401f73:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401f7a:	00 
  401f7b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401f82:	b0 04                	mov    $0x4,%al
  401f84:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401f8b:	c3                   	ret
  401f8c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401f91:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401f98:	00 
  401f99:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  401fa0:	31 c0                	xor    %eax,%eax
  401fa2:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  401fa9:	c3                   	ret
  401faa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401fb0 <runtime::[internal.odin]::byte_slice>:
  401fb0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  401fb5:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  401fba:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401fbf:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401fc4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401fc9:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  401fce:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401fd3:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401fd8:	31 c0                	xor    %eax,%eax
  401fda:	48 85 d2             	test   %rdx,%rdx
  401fdd:	48 0f 49 c2          	cmovns %rdx,%rax
  401fe1:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  401fe6:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401feb:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401ff0:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401ff5:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  401ffa:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  401fff:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  402004:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  402009:	c3                   	ret
  40200a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402010 <runtime::bounds_check_error>:
  402010:	48 83 ec 58          	sub    $0x58,%rsp
  402014:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402019:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40201e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402022:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402026:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40202b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402030:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402035:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40203a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40203e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402042:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402047:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40204c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402051:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402056:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40205a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40205e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402063:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402068:	48 39 c8             	cmp    %rcx,%rax
  40206b:	0f 92 c0             	setb   %al
  40206e:	24 01                	and    $0x1,%al
  402070:	3c 00                	cmp    $0x0,%al
  402072:	74 05                	je     402079 <runtime::bounds_check_error+0x69>
  402074:	48 83 c4 58          	add    $0x58,%rsp
  402078:	c3                   	ret
  402079:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40207e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402083:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  402087:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40208b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402090:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402095:	e8 76 42 00 00       	call   406310 <runtime::bounds_check_error.handle_error-0>
  40209a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004020a0 <runtime::is_power_of_two_int>:
  4020a0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  4020a5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4020aa:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4020af:	48 83 f8 00          	cmp    $0x0,%rax
  4020b3:	0f 9e c0             	setle  %al
  4020b6:	24 01                	and    $0x1,%al
  4020b8:	3c 00                	cmp    $0x0,%al
  4020ba:	74 03                	je     4020bf <runtime::is_power_of_two_int+0x1f>
  4020bc:	31 c0                	xor    %eax,%eax
  4020be:	c3                   	ret
  4020bf:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4020c4:	48 89 c1             	mov    %rax,%rcx
  4020c7:	48 83 e9 01          	sub    $0x1,%rcx
  4020cb:	48 21 c8             	and    %rcx,%rax
  4020ce:	48 83 f8 00          	cmp    $0x0,%rax
  4020d2:	0f 94 c0             	sete   %al
  4020d5:	24 01                	and    $0x1,%al
  4020d7:	c3                   	ret
  4020d8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4020df:	00 

00000000004020e0 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  4020e0:	48 83 ec 18          	sub    $0x18,%rsp
  4020e4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4020e8:	40 88 f0             	mov    %sil,%al
  4020eb:	88 44 24 0e          	mov    %al,0xe(%rsp)
  4020ef:	48 8b 04 24          	mov    (%rsp),%rax
  4020f3:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  4020f7:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4020fc:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  402100:	48 83 f8 00          	cmp    $0x0,%rax
  402104:	0f 9e c0             	setle  %al
  402107:	24 01                	and    $0x1,%al
  402109:	3c 00                	cmp    $0x0,%al
  40210b:	74 07                	je     402114 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  40210d:	31 c0                	xor    %eax,%eax
  40210f:	48 83 c4 18          	add    $0x18,%rsp
  402113:	c3                   	ret
  402114:	8a 44 24 0e          	mov    0xe(%rsp),%al
  402118:	3c 00                	cmp    $0x0,%al
  40211a:	74 13                	je     40212f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  40211c:	48 8b 34 24          	mov    (%rsp),%rsi
  402120:	bf 01 00 00 00       	mov    $0x1,%edi
  402125:	e8 26 ef ff ff       	call   401050 <calloc@plt>
  40212a:	48 83 c4 18          	add    $0x18,%rsp
  40212e:	c3                   	ret
  40212f:	48 8b 3c 24          	mov    (%rsp),%rdi
  402133:	e8 38 ef ff ff       	call   401070 <malloc@plt>
  402138:	48 83 c4 18          	add    $0x18,%rsp
  40213c:	c3                   	ret
  40213d:	0f 1f 00             	nopl   (%rax)

0000000000402140 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  402140:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  402145:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  40214a:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  40214f:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  402154:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  402159:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40215e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  402163:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  402168:	48 01 c2             	add    %rax,%rdx
  40216b:	0f 92 c0             	setb   %al
  40216e:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  402173:	24 01                	and    $0x1,%al
  402175:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  402179:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40217e:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  402183:	0f 94 c0             	sete   %al
  402186:	24 01                	and    $0x1,%al
  402188:	48 89 11             	mov    %rdx,(%rcx)
  40218b:	c3                   	ret
  40218c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402190 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  402190:	48 83 ec 28          	sub    $0x28,%rsp
  402194:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402199:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40219e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4021a3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4021a8:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4021ad:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4021b2:	e8 c9 ee ff ff       	call   401080 <realloc@plt>
  4021b7:	48 83 c4 28          	add    $0x28,%rsp
  4021bb:	c3                   	ret
  4021bc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004021c0 <runtime::memory_block_alloc>:
  4021c0:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  4021c7:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  4021cc:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  4021d1:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4021d6:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4021db:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  4021e0:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  4021e5:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  4021ec:	00 
  4021ed:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4021f2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  4021f7:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  4021fc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  402201:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  402206:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40220b:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  402210:	48 89 b4 24 f0 00 00 	mov    %rsi,0xf0(%rsp)
  402217:	00 
  402218:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  40221f:	00 
  402220:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  402227:	00 
  402228:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40222d:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  402234:	00 
  402235:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40223a:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  402241:	00 
  402242:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  402249:	00 
  40224a:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  402251:	00 00 00 00 00 
  402256:	c6 84 24 d7 00 00 00 	movb   $0x0,0xd7(%rsp)
  40225d:	00 
  40225e:	48 89 c1             	mov    %rax,%rcx
  402261:	48 83 e9 31          	sub    $0x31,%rcx
  402265:	b9 30 00 00 00       	mov    $0x30,%ecx
  40226a:	48 0f 43 c8          	cmovae %rax,%rcx
  40226e:	48 01 ca             	add    %rcx,%rdx
  402271:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  402278:	00 
  402279:	48 89 8c 24 c0 00 00 	mov    %rcx,0xc0(%rsp)
  402280:	00 
  402281:	48 89 c1             	mov    %rax,%rcx
  402284:	48 83 e9 10          	sub    $0x10,%rcx
  402288:	b9 10 00 00 00       	mov    $0x10,%ecx
  40228d:	48 0f 4c c1          	cmovl  %rcx,%rax
  402291:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  402298:	00 
  402299:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  4022a0:	00 
  4022a1:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  4022a8:	00 
  4022a9:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  4022b0:	00 
  4022b1:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  4022b8:	00 
  4022b9:	0f 57 c0             	xorps  %xmm0,%xmm0
  4022bc:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4022c3:	00 
  4022c4:	48 89 e0             	mov    %rsp,%rax
  4022c7:	4c 89 08             	mov    %r9,(%rax)
  4022ca:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  4022d1:	00 
  4022d2:	e8 f9 13 00 00       	call   4036d0 <runtime::mem_alloc>
  4022d7:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  4022db:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4022e2:	00 
  4022e3:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4022e8:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  4022ef:	00 
  4022f0:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4022f5:	3c 00                	cmp    $0x0,%al
  4022f7:	74 39                	je     402332 <runtime::memory_block_alloc+0x172>
  4022f9:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4022fe:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  402302:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402309:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402310:	00 
  402311:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  402318:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  40231f:	00 
  402320:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402327:	48 89 11             	mov    %rdx,(%rcx)
  40232a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402331:	c3                   	ret
  402332:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  402337:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40233c:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402341:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402346:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40234b:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  402350:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402357:	00 
  402358:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40235d:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  402364:	00 
  402365:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40236a:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  402371:	00 
  402372:	48 01 f0             	add    %rsi,%rax
  402375:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40237a:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40237f:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  402384:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40238b:	00 
  40238c:	48 89 50 10          	mov    %rdx,0x10(%rax)
  402390:	48 89 48 08          	mov    %rcx,0x8(%rax)
  402394:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40239b:	00 
  40239c:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  4023a3:	00 
  4023a4:	48 03 8c 24 c0 00 00 	add    0xc0(%rsp),%rcx
  4023ab:	00 
  4023ac:	48 89 48 18          	mov    %rcx,0x18(%rax)
  4023b0:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4023b7:	00 
  4023b8:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4023bd:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  4023c4:	00 
  4023c5:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  4023c9:	48 29 d1             	sub    %rdx,%rcx
  4023cc:	48 89 48 28          	mov    %rcx,0x28(%rax)
  4023d0:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4023d7:	00 
  4023d8:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  4023dd:	0f 94 c0             	sete   %al
  4023e0:	24 01                	and    $0x1,%al
  4023e2:	0f b6 f8             	movzbl %al,%edi
  4023e5:	be 90 70 40 00       	mov    $0x407090,%esi
  4023ea:	b9 00 71 40 00       	mov    $0x407100,%ecx
  4023ef:	ba 0f 00 00 00       	mov    $0xf,%edx
  4023f4:	e8 67 39 00 00       	call   405d60 <runtime::assert>
  4023f9:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  4023fe:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402405:	00 
  402406:	48 83 38 00          	cmpq   $0x0,(%rax)
  40240a:	0f 94 c0             	sete   %al
  40240d:	24 01                	and    $0x1,%al
  40240f:	0f b6 f8             	movzbl %al,%edi
  402412:	be 28 71 40 00       	mov    $0x407128,%esi
  402417:	b9 40 71 40 00       	mov    $0x407140,%ecx
  40241c:	ba 11 00 00 00       	mov    $0x11,%edx
  402421:	e8 3a 39 00 00       	call   405d60 <runtime::assert>
  402426:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40242b:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402432:	00 
  402433:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  40243a:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  402441:	00 
  402442:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402449:	48 89 11             	mov    %rdx,(%rcx)
  40244c:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402453:	c3                   	ret
  402454:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40245b:	00 00 00 00 00 

0000000000402460 <runtime::default_temp_allocator_destroy>:
  402460:	48 83 ec 18          	sub    $0x18,%rsp
  402464:	48 89 3c 24          	mov    %rdi,(%rsp)
  402468:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40246d:	48 8b 04 24          	mov    (%rsp),%rax
  402471:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402476:	48 83 f8 00          	cmp    $0x0,%rax
  40247a:	0f 95 c0             	setne  %al
  40247d:	24 01                	and    $0x1,%al
  40247f:	3c 00                	cmp    $0x0,%al
  402481:	74 29                	je     4024ac <runtime::default_temp_allocator_destroy+0x4c>
  402483:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402488:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40248d:	48 be d0 71 40 00 00 	movabs $0x4071d0,%rsi
  402494:	00 00 00 
  402497:	e8 14 1b 00 00       	call   403fb0 <runtime::arena_destroy>
  40249c:	48 8b 3c 24          	mov    (%rsp),%rdi
  4024a0:	31 f6                	xor    %esi,%esi
  4024a2:	ba 38 00 00 00       	mov    $0x38,%edx
  4024a7:	e8 94 eb ff ff       	call   401040 <memset@plt>
  4024ac:	48 83 c4 18          	add    $0x18,%rsp
  4024b0:	c3                   	ret
  4024b1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4024b8:	0f 1f 84 00 00 00 00 
  4024bf:	00 

00000000004024c0 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  4024c0:	48 83 ec 18          	sub    $0x18,%rsp
  4024c4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4024c9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4024ce:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4024d3:	e8 58 eb ff ff       	call   401030 <free@plt>
  4024d8:	48 83 c4 18          	add    $0x18,%rsp
  4024dc:	c3                   	ret
  4024dd:	0f 1f 00             	nopl   (%rax)

00000000004024e0 <runtime::default_random_generator_proc>:
  4024e0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4024e7:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4024ec:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4024f1:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4024f6:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4024fb:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402500:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402505:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40250a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40250f:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402516:	00 
  402517:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  40251c:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  402521:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  402526:	48 83 f8 00          	cmp    $0x0,%rax
  40252a:	0f 94 c0             	sete   %al
  40252d:	24 01                	and    $0x1,%al
  40252f:	3c 00                	cmp    $0x0,%al
  402531:	74 1a                	je     40254d <runtime::default_random_generator_proc+0x6d>
  402533:	48 c7 c1 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rcx
  40253a:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402541:	00 00 
  402543:	48 01 c8             	add    %rcx,%rax
  402546:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40254b:	eb 0a                	jmp    402557 <runtime::default_random_generator_proc+0x77>
  40254d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402552:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402557:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40255c:	48 85 c0             	test   %rax,%rax
  40255f:	74 27                	je     402588 <runtime::default_random_generator_proc+0xa8>
  402561:	eb 00                	jmp    402563 <runtime::default_random_generator_proc+0x83>
  402563:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402568:	48 83 e8 01          	sub    $0x1,%rax
  40256c:	0f 84 17 01 00 00    	je     402689 <runtime::default_random_generator_proc+0x1a9>
  402572:	eb 00                	jmp    402574 <runtime::default_random_generator_proc+0x94>
  402574:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402579:	48 83 e8 02          	sub    $0x2,%rax
  40257d:	0f 84 40 01 00 00    	je     4026c3 <runtime::default_random_generator_proc+0x1e3>
  402583:	e9 6b 01 00 00       	jmp    4026f3 <runtime::default_random_generator_proc+0x213>
  402588:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40258d:	48 83 38 00          	cmpq   $0x0,(%rax)
  402591:	0f 94 c0             	sete   %al
  402594:	24 01                	and    $0x1,%al
  402596:	3c 00                	cmp    $0x0,%al
  402598:	74 21                	je     4025bb <runtime::default_random_generator_proc+0xdb>
  40259a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40259f:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  4025a4:	0f 94 c0             	sete   %al
  4025a7:	24 01                	and    $0x1,%al
  4025a9:	3c 00                	cmp    $0x0,%al
  4025ab:	74 0e                	je     4025bb <runtime::default_random_generator_proc+0xdb>
  4025ad:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4025b2:	31 c0                	xor    %eax,%eax
  4025b4:	89 c6                	mov    %eax,%esi
  4025b6:	e8 15 3f 00 00       	call   4064d0 <runtime::default_random_generator_proc.init-1>
  4025bb:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4025c0:	48 83 e8 08          	sub    $0x8,%rax
  4025c4:	75 26                	jne    4025ec <runtime::default_random_generator_proc+0x10c>
  4025c6:	eb 00                	jmp    4025c8 <runtime::default_random_generator_proc+0xe8>
  4025c8:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4025cd:	e8 2e 3e 00 00       	call   406400 <runtime::default_random_generator_proc.read_u64-0>
  4025d2:	48 89 c1             	mov    %rax,%rcx
  4025d5:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4025da:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4025df:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4025e4:	48 89 08             	mov    %rcx,(%rax)
  4025e7:	e9 9b 00 00 00       	jmp    402687 <runtime::default_random_generator_proc+0x1a7>
  4025ec:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4025f1:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  4025f6:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  4025fd:	00 00 
  4025ff:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402604:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  40260b:	ff ff 
  40260d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402612:	48 83 c0 01          	add    $0x1,%rax
  402616:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40261b:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  402620:	7d 63                	jge    402685 <runtime::default_random_generator_proc+0x1a5>
  402622:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402627:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40262c:	48 01 c8             	add    %rcx,%rax
  40262f:	48 89 04 24          	mov    %rax,(%rsp)
  402633:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  402638:	0f 94 c0             	sete   %al
  40263b:	24 01                	and    $0x1,%al
  40263d:	3c 00                	cmp    $0x0,%al
  40263f:	74 14                	je     402655 <runtime::default_random_generator_proc+0x175>
  402641:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402646:	e8 b5 3d 00 00       	call   406400 <runtime::default_random_generator_proc.read_u64-0>
  40264b:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402650:	c6 44 24 57 07       	movb   $0x7,0x57(%rsp)
  402655:	48 8b 04 24          	mov    (%rsp),%rax
  402659:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40265e:	88 08                	mov    %cl,(%rax)
  402660:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402665:	48 c1 e9 08          	shr    $0x8,%rcx
  402669:	b2 01                	mov    $0x1,%dl
  40266b:	31 c0                	xor    %eax,%eax
  40266d:	f6 c2 01             	test   $0x1,%dl
  402670:	48 0f 45 c1          	cmovne %rcx,%rax
  402674:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402679:	8a 44 24 57          	mov    0x57(%rsp),%al
  40267d:	2c 01                	sub    $0x1,%al
  40267f:	88 44 24 57          	mov    %al,0x57(%rsp)
  402683:	eb 88                	jmp    40260d <runtime::default_random_generator_proc+0x12d>
  402685:	eb 00                	jmp    402687 <runtime::default_random_generator_proc+0x1a7>
  402687:	eb 6a                	jmp    4026f3 <runtime::default_random_generator_proc+0x213>
  402689:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40268e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402693:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40269a:	00 00 
  40269c:	b8 08 00 00 00       	mov    $0x8,%eax
  4026a1:	48 39 d0             	cmp    %rdx,%rax
  4026a4:	48 0f 4c d0          	cmovl  %rax,%rdx
  4026a8:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4026ad:	e8 5e 09 00 00       	call   403010 <runtime::mem_copy_non_overlapping>
  4026b2:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4026b7:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4026bc:	e8 0f 3e 00 00       	call   4064d0 <runtime::default_random_generator_proc.init-1>
  4026c1:	eb 30                	jmp    4026f3 <runtime::default_random_generator_proc+0x213>
  4026c3:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4026c8:	48 83 f8 04          	cmp    $0x4,%rax
  4026cc:	0f 95 c0             	setne  %al
  4026cf:	24 01                	and    $0x1,%al
  4026d1:	3c 00                	cmp    $0x0,%al
  4026d3:	74 08                	je     4026dd <runtime::default_random_generator_proc+0x1fd>
  4026d5:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4026dc:	c3                   	ret
  4026dd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4026e2:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4026e7:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4026ec:	8b 08                	mov    (%rax),%ecx
  4026ee:	83 c9 0a             	or     $0xa,%ecx
  4026f1:	89 08                	mov    %ecx,(%rax)
  4026f3:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4026fa:	c3                   	ret
  4026fb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402700 <runtime::slice_handle_error>:
  402700:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402707:	4c 89 0c 24          	mov    %r9,(%rsp)
  40270b:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  402710:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  402714:	89 54 24 14          	mov    %edx,0x14(%rsp)
  402718:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40271d:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  402722:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  402729:	00 
  40272a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40272f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402734:	4c 8b 04 24          	mov    (%rsp),%r8
  402738:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40273d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  402741:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  402745:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40274a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40274f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402754:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40275b:	00 
  40275c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402760:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402764:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  402769:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  40276e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402773:	0f 57 c0             	xorps  %xmm0,%xmm0
  402776:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40277b:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402780:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402787:	00 00 
  402789:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40278e:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402793:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  40279a:	00 00 
  40279c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4027a1:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4027a6:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4027aa:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4027ae:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4027b3:	e8 c8 16 00 00       	call   403e80 <runtime::print_caller_location>
  4027b8:	bf f9 71 40 00       	mov    $0x4071f9,%edi
  4027bd:	be 17 00 00 00       	mov    $0x17,%esi
  4027c2:	e8 99 0e 00 00       	call   403660 <runtime::print_string>
  4027c7:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4027cc:	e8 af 14 00 00       	call   403c80 <runtime::print_i64>
  4027d1:	bf 11 72 40 00       	mov    $0x407211,%edi
  4027d6:	be 01 00 00 00       	mov    $0x1,%esi
  4027db:	e8 80 0e 00 00       	call   403660 <runtime::print_string>
  4027e0:	48 8b 3c 24          	mov    (%rsp),%rdi
  4027e4:	e8 97 14 00 00       	call   403c80 <runtime::print_i64>
  4027e9:	bf 13 72 40 00       	mov    $0x407213,%edi
  4027ee:	be 15 00 00 00       	mov    $0x15,%esi
  4027f3:	e8 68 0e 00 00       	call   403660 <runtime::print_string>
  4027f8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4027fd:	e8 7e 14 00 00       	call   403c80 <runtime::print_i64>
  402802:	bf 0a 00 00 00       	mov    $0xa,%edi
  402807:	e8 84 10 00 00       	call   403890 <runtime::print_byte>
  40280c:	e8 cf ea ff ff       	call   4012e0 <runtime::bounds_trap>
  402811:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402818:	0f 1f 84 00 00 00 00 
  40281f:	00 

0000000000402820 <runtime::default_temp_allocator_proc>:
  402820:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  402827:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  40282c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  402831:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402836:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  40283b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  402840:	40 88 f0             	mov    %sil,%al
  402843:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  402847:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40284e:	00 
  40284f:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402854:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40285b:	00 
  40285c:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  402861:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  402868:	00 
  402869:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40286e:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  402873:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402878:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40287d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  402882:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  402886:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  40288b:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  402890:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  402895:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  40289c:	00 
  40289d:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4028a4:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  4028ab:	00 
  4028ac:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  4028b3:	00 
  4028b4:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  4028bb:	00 
  4028bc:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  4028c3:	00 
  4028c4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4028c7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4028ce:	00 
  4028cf:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  4028d6:	00 
  4028d7:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  4028de:	00 
  4028df:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  4028e6:	00 
  4028e7:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  4028ee:	00 
  4028ef:	48 89 e6             	mov    %rsp,%rsi
  4028f2:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  4028f6:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  4028fd:	00 
  4028fe:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  402902:	4c 89 16             	mov    %r10,(%rsi)
  402905:	0f b6 f0             	movzbl %al,%esi
  402908:	e8 43 17 00 00       	call   404050 <runtime::arena_allocator_proc>
  40290d:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402912:	40 88 c7             	mov    %al,%dil
  402915:	40 88 f8             	mov    %dil,%al
  402918:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  40291f:	00 
  402920:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  402927:	00 
  402928:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40292f:	00 
  402930:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402937:	00 
  402938:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  40293f:	00 
  402940:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402944:	48 89 11             	mov    %rdx,(%rcx)
  402947:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40294e:	c3                   	ret
  40294f:	90                   	nop

0000000000402950 <runtime::multi_pointer_slice_handle_error>:
  402950:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402957:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40295c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402961:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402965:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402969:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40296e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402973:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402978:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40297d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  402981:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402985:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40298a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40298f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402994:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40299b:	00 
  40299c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  4029a0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  4029a4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  4029a9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  4029ae:	0f 57 c0             	xorps  %xmm0,%xmm0
  4029b1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4029b6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4029bb:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4029c2:	00 00 
  4029c4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4029c9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4029ce:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4029d5:	00 00 
  4029d7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4029dc:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4029e1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4029e5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4029e9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4029ee:	e8 8d 14 00 00       	call   403e80 <runtime::print_caller_location>
  4029f3:	bf f9 71 40 00       	mov    $0x4071f9,%edi
  4029f8:	be 17 00 00 00       	mov    $0x17,%esi
  4029fd:	e8 5e 0c 00 00       	call   403660 <runtime::print_string>
  402a02:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402a07:	e8 74 12 00 00       	call   403c80 <runtime::print_i64>
  402a0c:	bf 11 72 40 00       	mov    $0x407211,%edi
  402a11:	be 01 00 00 00       	mov    $0x1,%esi
  402a16:	e8 45 0c 00 00       	call   403660 <runtime::print_string>
  402a1b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402a20:	e8 5b 12 00 00       	call   403c80 <runtime::print_i64>
  402a25:	bf 0a 00 00 00       	mov    $0xa,%edi
  402a2a:	e8 61 0e 00 00       	call   403890 <runtime::print_byte>
  402a2f:	e8 ac e8 ff ff       	call   4012e0 <runtime::bounds_trap>
  402a34:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402a3b:	00 00 00 00 00 

0000000000402a40 <runtime::memory_block_dealloc>:
  402a40:	48 83 ec 38          	sub    $0x38,%rsp
  402a44:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402a49:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402a4e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402a53:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402a58:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402a5d:	48 83 f8 00          	cmp    $0x0,%rax
  402a61:	0f 95 c0             	setne  %al
  402a64:	24 01                	and    $0x1,%al
  402a66:	3c 00                	cmp    $0x0,%al
  402a68:	74 35                	je     402a9f <runtime::memory_block_dealloc+0x5f>
  402a6a:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  402a6f:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402a74:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402a79:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402a7e:	48 8b 42 08          	mov    0x8(%rdx),%rax
  402a82:	48 8b 52 10          	mov    0x10(%rdx),%rdx
  402a86:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402a8b:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402a90:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402a95:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402a9a:	e8 f1 0f 00 00       	call   403a90 <runtime::mem_free>
  402a9f:	48 83 c4 38          	add    $0x38,%rsp
  402aa3:	c3                   	ret
  402aa4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402aab:	00 00 00 00 00 

0000000000402ab0 <main>:
  402ab0:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  402ab7:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  402abb:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402ac0:	8b 44 24 14          	mov    0x14(%rsp),%eax
  402ac4:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402ac9:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  402ad0:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  402ad7:	00 
  402ad8:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  402adf:	00 
  402ae0:	48 89 0c 24          	mov    %rcx,(%rsp)
  402ae4:	4c 63 c8             	movslq %eax,%r9
  402ae7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402aec:	bf 29 72 40 00       	mov    $0x407229,%edi
  402af1:	31 c0                	xor    %eax,%eax
  402af3:	41 89 c0             	mov    %eax,%r8d
  402af6:	be 2c 00 00 00       	mov    $0x2c,%esi
  402afb:	ba 36 00 00 00       	mov    $0x36,%edx
  402b00:	b9 11 00 00 00       	mov    $0x11,%ecx
  402b05:	e8 c6 03 00 00       	call   402ed0 <runtime::multi_pointer_slice_expr_error>
  402b0a:	48 8b 0c 24          	mov    (%rsp),%rcx
  402b0e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402b13:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  402b1a:	00 
  402b1b:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  402b22:	00 
  402b23:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  402b2a:	00 
  402b2b:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  402b32:	00 
  402b33:	48 c7 c0 60 a0 40 00 	mov    $0x40a060,%rax
  402b3a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402b3e:	48 89 08             	mov    %rcx,(%rax)
  402b41:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402b48:	00 
  402b49:	31 f6                	xor    %esi,%esi
  402b4b:	ba 70 00 00 00       	mov    $0x70,%edx
  402b50:	e8 eb e4 ff ff       	call   401040 <memset@plt>
  402b55:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402b5c:	00 
  402b5d:	e8 fe 24 00 00       	call   405060 <runtime::[core.odin]::__init_context>
  402b62:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  402b67:	31 f6                	xor    %esi,%esi
  402b69:	ba 70 00 00 00       	mov    $0x70,%edx
  402b6e:	e8 cd e4 ff ff       	call   401040 <memset@plt>
  402b73:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  402b78:	e8 93 24 00 00       	call   405010 <runtime::default_context>
  402b7d:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  402b82:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  402b87:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  402b8c:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  402b91:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  402b96:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  402b9b:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  402ba2:	00 
  402ba3:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  402baa:	00 
  402bab:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  402bb2:	00 
  402bb3:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  402bba:	00 
  402bbb:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  402bc2:	00 
  402bc3:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  402bca:	00 
  402bcb:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  402bd2:	00 
  402bd3:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  402bda:	00 
  402bdb:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402be2:	00 
  402be3:	e8 68 3c 00 00       	call   406850 <__$startup_runtime>
  402be8:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402bef:	00 
  402bf0:	e8 9b e5 ff ff       	call   401190 <journey::main>
  402bf5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402bfc:	00 
  402bfd:	e8 5e 3c 00 00       	call   406860 <__$cleanup_runtime>
  402c02:	31 c0                	xor    %eax,%eax
  402c04:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  402c0b:	c3                   	ret
  402c0c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402c10 <runtime::alloc_from_memory_block>:
  402c10:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  402c17:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402c1c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402c21:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402c26:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402c2b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402c30:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402c35:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402c3a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  402c41:	00 
  402c42:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  402c49:	00 
  402c4a:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  402c51:	00 
  402c52:	0f 57 c0             	xorps  %xmm0,%xmm0
  402c55:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402c5c:	00 
  402c5d:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  402c64:	00 
  402c65:	48 83 f8 00          	cmp    $0x0,%rax
  402c69:	0f 94 c0             	sete   %al
  402c6c:	24 01                	and    $0x1,%al
  402c6e:	3c 00                	cmp    $0x0,%al
  402c70:	74 3e                	je     402cb0 <runtime::alloc_from_memory_block+0xa0>
  402c72:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c77:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  402c7e:	00 00 00 00 00 
  402c83:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  402c8a:	00 00 00 00 00 
  402c8f:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402c96:	01 
  402c97:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402c9e:	00 
  402c9f:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402ca6:	b0 01                	mov    $0x1,%al
  402ca8:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402caf:	c3                   	ret
  402cb0:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  402cb5:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402cba:	e8 b1 38 00 00       	call   406570 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  402cbf:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402cc4:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  402ccb:	00 
  402ccc:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  402cd3:	00 
  402cd4:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  402cdb:	00 00 00 00 00 
  402ce0:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  402ce7:	00 
  402ce8:	e8 53 f4 ff ff       	call   402140 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  402ced:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  402cf4:	00 
  402cf5:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  402cfa:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  402cfe:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  402d03:	75 4a                	jne    402d4f <runtime::alloc_from_memory_block+0x13f>
  402d05:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402d0a:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402d11:	01 
  402d12:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402d19:	00 
  402d1a:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402d21:	00 
  402d22:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402d29:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402d30:	00 
  402d31:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402d38:	00 
  402d39:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402d40:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402d44:	48 89 11             	mov    %rdx,(%rcx)
  402d47:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402d4e:	c3                   	ret
  402d4f:	eb 00                	jmp    402d51 <runtime::alloc_from_memory_block+0x141>
  402d51:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  402d58:	00 
  402d59:	48 8b 78 20          	mov    0x20(%rax),%rdi
  402d5d:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  402d62:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  402d69:	00 00 
  402d6b:	48 8d 54 24 60       	lea    0x60(%rsp),%rdx
  402d70:	e8 cb f3 ff ff       	call   402140 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  402d75:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  402d7a:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  402d7f:	88 44 24 47          	mov    %al,0x47(%rsp)
  402d83:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  402d88:	74 1a                	je     402da4 <runtime::alloc_from_memory_block+0x194>
  402d8a:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402d8f:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402d96:	00 
  402d97:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  402d9b:	0f 97 c0             	seta   %al
  402d9e:	24 01                	and    $0x1,%al
  402da0:	3c 00                	cmp    $0x0,%al
  402da2:	74 4a                	je     402dee <runtime::alloc_from_memory_block+0x1de>
  402da4:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402da9:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402db0:	01 
  402db1:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402db8:	00 
  402db9:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402dc0:	00 
  402dc1:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402dc8:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402dcf:	00 
  402dd0:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402dd7:	00 
  402dd8:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402ddf:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402de3:	48 89 11             	mov    %rdx,(%rcx)
  402de6:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402ded:	c3                   	ret
  402dee:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  402df3:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  402dfa:	00 
  402dfb:	48 8b 41 18          	mov    0x18(%rcx),%rax
  402dff:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  402e03:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  402e0a:	00 
  402e0b:	48 01 d1             	add    %rdx,%rcx
  402e0e:	48 01 c8             	add    %rcx,%rax
  402e11:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402e16:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402e1b:	48 89 04 24          	mov    %rax,(%rsp)
  402e1f:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  402e24:	31 c0                	xor    %eax,%eax
  402e26:	41 89 c0             	mov    %eax,%r8d
  402e29:	be 3e 00 00 00       	mov    $0x3e,%esi
  402e2e:	ba 55 00 00 00       	mov    $0x55,%edx
  402e33:	b9 31 00 00 00       	mov    $0x31,%ecx
  402e38:	e8 93 00 00 00       	call   402ed0 <runtime::multi_pointer_slice_expr_error>
  402e3d:	48 8b 14 24          	mov    (%rsp),%rdx
  402e41:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402e46:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402e4b:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402e50:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402e55:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402e5a:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402e5f:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  402e66:	00 
  402e67:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402e6e:	00 
  402e6f:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  402e76:	00 
  402e77:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  402e7c:	48 8b 50 20          	mov    0x20(%rax),%rdx
  402e80:	48 01 f2             	add    %rsi,%rdx
  402e83:	48 89 50 20          	mov    %rdx,0x20(%rax)
  402e87:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  402e8e:	00 
  402e8f:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402e96:	00 
  402e97:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  402e9e:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402ea5:	00 
  402ea6:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402ead:	00 
  402eae:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  402eb5:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402eb9:	48 89 11             	mov    %rdx,(%rcx)
  402ebc:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  402ec3:	c3                   	ret
  402ec4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402ecb:	00 00 00 00 00 

0000000000402ed0 <runtime::multi_pointer_slice_expr_error>:
  402ed0:	48 83 ec 58          	sub    $0x58,%rsp
  402ed4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402ed9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402ede:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402ee2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402ee6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402eeb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402ef0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402ef5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  402efa:	8b 54 24 18          	mov    0x18(%rsp),%edx
  402efe:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402f02:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402f07:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402f0c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402f11:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402f16:	89 74 24 44          	mov    %esi,0x44(%rsp)
  402f1a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  402f1e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402f23:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402f28:	48 39 c8             	cmp    %rcx,%rax
  402f2b:	0f 9e c0             	setle  %al
  402f2e:	24 01                	and    $0x1,%al
  402f30:	3c 00                	cmp    $0x0,%al
  402f32:	74 05                	je     402f39 <runtime::multi_pointer_slice_expr_error+0x69>
  402f34:	48 83 c4 58          	add    $0x58,%rsp
  402f38:	c3                   	ret
  402f39:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  402f3e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402f43:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  402f47:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  402f4b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402f50:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402f55:	e8 f6 f9 ff ff       	call   402950 <runtime::multi_pointer_slice_handle_error>
  402f5a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402f60 <runtime::slice_expr_error_hi>:
  402f60:	48 83 ec 58          	sub    $0x58,%rsp
  402f64:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402f69:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402f6e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402f72:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402f76:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402f7b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402f80:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402f85:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402f8a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  402f8e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402f92:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402f97:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402f9c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402fa1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402fa6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  402faa:	89 54 24 40          	mov    %edx,0x40(%rsp)
  402fae:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  402fb3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402fb8:	31 c0                	xor    %eax,%eax
  402fba:	48 39 c8             	cmp    %rcx,%rax
  402fbd:	0f 9e c0             	setle  %al
  402fc0:	24 01                	and    $0x1,%al
  402fc2:	3c 00                	cmp    $0x0,%al
  402fc4:	74 1b                	je     402fe1 <runtime::slice_expr_error_hi+0x81>
  402fc6:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402fcb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  402fd0:	48 39 c8             	cmp    %rcx,%rax
  402fd3:	0f 9e c0             	setle  %al
  402fd6:	24 01                	and    $0x1,%al
  402fd8:	3c 00                	cmp    $0x0,%al
  402fda:	74 05                	je     402fe1 <runtime::slice_expr_error_hi+0x81>
  402fdc:	48 83 c4 58          	add    $0x58,%rsp
  402fe0:	c3                   	ret
  402fe1:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  402fe6:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  402fea:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  402fee:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402ff3:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402ff8:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  402ffd:	48 89 e0             	mov    %rsp,%rax
  403000:	4c 89 00             	mov    %r8,(%rax)
  403003:	31 c0                	xor    %eax,%eax
  403005:	41 89 c0             	mov    %eax,%r8d
  403008:	e8 f3 f6 ff ff       	call   402700 <runtime::slice_handle_error>
  40300d:	0f 1f 00             	nopl   (%rax)

0000000000403010 <runtime::mem_copy_non_overlapping>:
  403010:	48 83 ec 38          	sub    $0x38,%rsp
  403014:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403019:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40301e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403023:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403028:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40302d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403032:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403037:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40303c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  403041:	48 83 f8 00          	cmp    $0x0,%rax
  403045:	0f 95 c0             	setne  %al
  403048:	24 01                	and    $0x1,%al
  40304a:	3c 00                	cmp    $0x0,%al
  40304c:	74 3c                	je     40308a <runtime::mem_copy_non_overlapping+0x7a>
  40304e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403053:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  403058:	48 39 c8             	cmp    %rcx,%rax
  40305b:	0f 95 c0             	setne  %al
  40305e:	24 01                	and    $0x1,%al
  403060:	3c 00                	cmp    $0x0,%al
  403062:	74 26                	je     40308a <runtime::mem_copy_non_overlapping+0x7a>
  403064:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403069:	48 83 f8 00          	cmp    $0x0,%rax
  40306d:	0f 9f c0             	setg   %al
  403070:	24 01                	and    $0x1,%al
  403072:	3c 00                	cmp    $0x0,%al
  403074:	74 14                	je     40308a <runtime::mem_copy_non_overlapping+0x7a>
  403076:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40307b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403080:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403085:	e8 d6 df ff ff       	call   401060 <memcpy@plt>
  40308a:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40308f:	48 83 c4 38          	add    $0x38,%rsp
  403093:	c3                   	ret
  403094:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40309b:	00 00 00 00 00 

00000000004030a0 <runtime::slice_expr_error_lo_hi>:
  4030a0:	48 83 ec 68          	sub    $0x68,%rsp
  4030a4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4030a9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4030ae:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4030b2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4030b6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4030bb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4030c0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4030c5:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4030ca:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4030cf:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4030d4:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4030d9:	8b 74 24 18          	mov    0x18(%rsp),%esi
  4030dd:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  4030e1:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  4030e6:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  4030eb:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  4030f0:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  4030f5:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  4030f9:	89 74 24 50          	mov    %esi,0x50(%rsp)
  4030fd:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  403102:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  403107:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40310c:	31 c0                	xor    %eax,%eax
  40310e:	48 39 c8             	cmp    %rcx,%rax
  403111:	0f 9e c0             	setle  %al
  403114:	24 01                	and    $0x1,%al
  403116:	3c 00                	cmp    $0x0,%al
  403118:	74 47                	je     403161 <runtime::slice_expr_error_lo_hi+0xc1>
  40311a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40311f:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403124:	48 39 c8             	cmp    %rcx,%rax
  403127:	0f 9e c0             	setle  %al
  40312a:	24 01                	and    $0x1,%al
  40312c:	3c 00                	cmp    $0x0,%al
  40312e:	74 31                	je     403161 <runtime::slice_expr_error_lo_hi+0xc1>
  403130:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403135:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40313a:	48 39 c8             	cmp    %rcx,%rax
  40313d:	0f 9e c0             	setle  %al
  403140:	24 01                	and    $0x1,%al
  403142:	3c 00                	cmp    $0x0,%al
  403144:	74 1b                	je     403161 <runtime::slice_expr_error_lo_hi+0xc1>
  403146:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40314b:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403150:	48 39 c8             	cmp    %rcx,%rax
  403153:	0f 9e c0             	setle  %al
  403156:	24 01                	and    $0x1,%al
  403158:	3c 00                	cmp    $0x0,%al
  40315a:	74 05                	je     403161 <runtime::slice_expr_error_lo_hi+0xc1>
  40315c:	48 83 c4 68          	add    $0x68,%rsp
  403160:	c3                   	ret
  403161:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403166:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40316b:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40316f:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  403173:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403178:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40317d:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  403182:	48 89 e0             	mov    %rsp,%rax
  403185:	4c 89 10             	mov    %r10,(%rax)
  403188:	e8 73 f5 ff ff       	call   402700 <runtime::slice_handle_error>
  40318d:	0f 1f 00             	nopl   (%rax)

0000000000403190 <runtime::memset>:
  403190:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  403195:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  403199:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  40319e:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4031a3:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4031a8:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  4031ac:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4031b1:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  4031b5:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4031ba:	48 83 f8 00          	cmp    $0x0,%rax
  4031be:	0f 95 c0             	setne  %al
  4031c1:	24 01                	and    $0x1,%al
  4031c3:	3c 00                	cmp    $0x0,%al
  4031c5:	74 63                	je     40322a <runtime::memset+0x9a>
  4031c7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4031cc:	48 83 f8 00          	cmp    $0x0,%rax
  4031d0:	0f 95 c0             	setne  %al
  4031d3:	24 01                	and    $0x1,%al
  4031d5:	3c 00                	cmp    $0x0,%al
  4031d7:	74 51                	je     40322a <runtime::memset+0x9a>
  4031d9:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4031de:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  4031e2:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  4031e6:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4031eb:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  4031f2:	00 00 
  4031f4:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4031f9:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  4031fe:	0f 9c c0             	setl   %al
  403201:	24 01                	and    $0x1,%al
  403203:	3c 00                	cmp    $0x0,%al
  403205:	74 21                	je     403228 <runtime::memset+0x98>
  403207:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40320c:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  403211:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  403215:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  403218:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40321d:	48 83 c0 01          	add    $0x1,%rax
  403221:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  403226:	eb cc                	jmp    4031f4 <runtime::memset+0x64>
  403228:	eb 00                	jmp    40322a <runtime::memset+0x9a>
  40322a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40322f:	c3                   	ret

0000000000403230 <runtime::arena_alloc>:
  403230:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  403237:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40323c:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403241:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  403246:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40324b:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403250:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  403255:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40325a:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40325f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403264:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403269:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40326e:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  403275:	00 
  403276:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  40327d:	00 
  40327e:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  403285:	00 
  403286:	0f 57 c0             	xorps  %xmm0,%xmm0
  403289:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  403290:	00 
  403291:	c6 84 24 0f 01 00 00 	movb   $0x0,0x10f(%rsp)
  403298:	00 
  403299:	48 89 c2             	mov    %rax,%rdx
  40329c:	48 83 ea 01          	sub    $0x1,%rdx
  4032a0:	48 21 d0             	and    %rdx,%rax
  4032a3:	48 83 f8 00          	cmp    $0x0,%rax
  4032a7:	0f 94 c0             	sete   %al
  4032aa:	24 01                	and    $0x1,%al
  4032ac:	0f b6 f8             	movzbl %al,%edi
  4032af:	be 56 72 40 00       	mov    $0x407256,%esi
  4032b4:	ba 1a 00 00 00       	mov    $0x1a,%edx
  4032b9:	e8 a2 2a 00 00       	call   405d60 <runtime::assert>
  4032be:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4032c3:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  4032ca:	00 
  4032cb:	48 83 bc 24 00 01 00 	cmpq   $0x0,0x100(%rsp)
  4032d2:	00 00 
  4032d4:	0f 94 c0             	sete   %al
  4032d7:	24 01                	and    $0x1,%al
  4032d9:	3c 00                	cmp    $0x0,%al
  4032db:	74 42                	je     40331f <runtime::arena_alloc+0xef>
  4032dd:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4032e2:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  4032e9:	00 
  4032ea:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  4032f1:	00 
  4032f2:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  4032f9:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403300:	00 
  403301:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403308:	00 
  403309:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403310:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403314:	48 89 11             	mov    %rdx,(%rcx)
  403317:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40331e:	c3                   	ret
  40331f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403326:	00 
  403327:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  40332c:	0f 94 c0             	sete   %al
  40332f:	24 01                	and    $0x1,%al
  403331:	3c 00                	cmp    $0x0,%al
  403333:	74 09                	je     40333e <runtime::arena_alloc+0x10e>
  403335:	31 c0                	xor    %eax,%eax
  403337:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40333c:	eb 15                	jmp    403353 <runtime::arena_alloc+0x123>
  40333e:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403345:	00 
  403346:	48 8b 40 10          	mov    0x10(%rax),%rax
  40334a:	48 8b 40 20          	mov    0x20(%rax),%rax
  40334e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403353:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403358:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40335d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403362:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  403369:	00 
  40336a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403371:	00 
  403372:	48 8b 78 10          	mov    0x10(%rax),%rdi
  403376:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  40337d:	00 
  40337e:	0f 57 c0             	xorps  %xmm0,%xmm0
  403381:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  403388:	00 
  403389:	48 8d 8c 24 e0 00 00 	lea    0xe0(%rsp),%rcx
  403390:	00 
  403391:	e8 7a f8 ff ff       	call   402c10 <runtime::alloc_from_memory_block>
  403396:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  40339d:	00 
  40339e:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4033a5:	00 
  4033a6:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  4033ad:	00 
  4033ae:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  4033b5:	00 
  4033b6:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4033bd:	80 bc 24 0f 01 00 00 	cmpb   $0x1,0x10f(%rsp)
  4033c4:	01 
  4033c5:	0f 94 c0             	sete   %al
  4033c8:	24 01                	and    $0x1,%al
  4033ca:	3c 00                	cmp    $0x0,%al
  4033cc:	0f 84 19 02 00 00    	je     4035eb <runtime::arena_alloc+0x3bb>
  4033d2:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4033d9:	00 
  4033da:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  4033df:	0f 94 c0             	sete   %al
  4033e2:	24 01                	and    $0x1,%al
  4033e4:	3c 00                	cmp    $0x0,%al
  4033e6:	74 10                	je     4033f8 <runtime::arena_alloc+0x1c8>
  4033e8:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4033ef:	00 
  4033f0:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  4033f7:	00 
  4033f8:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4033fd:	48 8b bc 24 00 01 00 	mov    0x100(%rsp),%rdi
  403404:	00 
  403405:	e8 f6 31 00 00       	call   406600 <runtime::arena_alloc.align_forward_uint-0>
  40340a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  403411:	00 
  403412:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403419:	00 
  40341a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403421:	00 
  403422:	48 8b 40 28          	mov    0x28(%rax),%rax
  403426:	48 39 c1             	cmp    %rax,%rcx
  403429:	48 0f 47 c1          	cmova  %rcx,%rax
  40342d:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  403434:	00 
  403435:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40343c:	00 
  40343d:	48 83 38 00          	cmpq   $0x0,(%rax)
  403441:	0f 94 c0             	sete   %al
  403444:	24 01                	and    $0x1,%al
  403446:	3c 00                	cmp    $0x0,%al
  403448:	74 46                	je     403490 <runtime::arena_alloc+0x260>
  40344a:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40344f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403456:	00 
  403457:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40345c:	e8 8f de ff ff       	call   4012f0 <runtime::heap_allocator>
  403461:	48 89 c1             	mov    %rax,%rcx
  403464:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403469:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  403470:	00 
  403471:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  403478:	00 
  403479:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403480:	00 
  403481:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  403488:	00 
  403489:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40348d:	48 89 08             	mov    %rcx,(%rax)
  403490:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  403495:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40349a:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  40349f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4034a6:	00 
  4034a7:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  4034ae:	00 
  4034af:	48 8b 38             	mov    (%rax),%rdi
  4034b2:	48 8b 70 08          	mov    0x8(%rax),%rsi
  4034b6:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  4034bd:	00 00 00 00 00 
  4034c2:	48 89 e0             	mov    %rsp,%rax
  4034c5:	4c 89 08             	mov    %r9,(%rax)
  4034c8:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  4034cf:	00 
  4034d0:	e8 eb ec ff ff       	call   4021c0 <runtime::memory_block_alloc>
  4034d5:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4034d9:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4034e0:	00 
  4034e1:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4034e6:	3c 00                	cmp    $0x0,%al
  4034e8:	74 4d                	je     403537 <runtime::arena_alloc+0x307>
  4034ea:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4034ef:	8a 44 24 0f          	mov    0xf(%rsp),%al
  4034f3:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4034fa:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403501:	00 
  403502:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403509:	00 
  40350a:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403511:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403518:	00 
  403519:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403520:	00 
  403521:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403528:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40352c:	48 89 11             	mov    %rdx,(%rcx)
  40352f:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403536:	c3                   	ret
  403537:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40353c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403541:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403546:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40354d:	00 
  40354e:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403555:	00 
  403556:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  40355d:	00 
  40355e:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  403562:	48 89 08             	mov    %rcx,(%rax)
  403565:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40356c:	00 
  40356d:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403574:	00 
  403575:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403579:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403580:	00 
  403581:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403588:	00 
  403589:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  40358d:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403591:	48 01 f1             	add    %rsi,%rcx
  403594:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403598:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  40359f:	00 00 00 00 00 
  4035a4:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4035ab:	00 
  4035ac:	48 8b 78 10          	mov    0x10(%rax),%rdi
  4035b0:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  4035b7:	00 
  4035b8:	0f 57 c0             	xorps  %xmm0,%xmm0
  4035bb:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4035c0:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  4035c5:	e8 46 f6 ff ff       	call   402c10 <runtime::alloc_from_memory_block>
  4035ca:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  4035cf:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  4035d4:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  4035db:	00 
  4035dc:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  4035e3:	00 
  4035e4:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4035eb:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4035f0:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4035f7:	00 
  4035f8:	48 8b 70 10          	mov    0x10(%rax),%rsi
  4035fc:	48 8b 50 18          	mov    0x18(%rax),%rdx
  403600:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  403604:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  40360b:	00 
  40360c:	48 29 fe             	sub    %rdi,%rsi
  40360f:	48 01 f2             	add    %rsi,%rdx
  403612:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403616:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  40361d:	00 
  40361e:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403625:	00 
  403626:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  40362d:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403634:	00 
  403635:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  40363c:	00 
  40363d:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403644:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403648:	48 89 11             	mov    %rdx,(%rcx)
  40364b:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403652:	c3                   	ret
  403653:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40365a:	84 00 00 00 00 00 

0000000000403660 <runtime::print_string>:
  403660:	48 83 ec 58          	sub    $0x58,%rsp
  403664:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403669:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40366e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403673:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403678:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40367d:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  403682:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  403689:	00 00 
  40368b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403690:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403695:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40369a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40369f:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4036a6:	00 00 
  4036a8:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  4036ad:	e8 0e e6 ff ff       	call   401cc0 <runtime::stderr_write>
  4036b2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4036b7:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4036bc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4036c1:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4036c6:	48 83 c4 58          	add    $0x58,%rsp
  4036ca:	c3                   	ret
  4036cb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004036d0 <runtime::mem_alloc>:
  4036d0:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  4036d7:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  4036dc:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  4036e1:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  4036e6:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4036eb:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4036f0:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4036f5:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4036fc:	00 
  4036fd:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403702:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403707:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40370c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403711:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403716:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40371d:	00 
  40371e:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  403725:	00 
  403726:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40372d:	00 
  40372e:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403735:	00 
  403736:	e8 65 e9 ff ff       	call   4020a0 <runtime::is_power_of_two_int>
  40373b:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403740:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403745:	0f b6 f8             	movzbl %al,%edi
  403748:	be 71 72 40 00       	mov    $0x407271,%esi
  40374d:	ba 20 00 00 00       	mov    $0x20,%edx
  403752:	e8 09 26 00 00       	call   405d60 <runtime::assert>
  403757:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40375c:	48 83 f8 00          	cmp    $0x0,%rax
  403760:	0f 94 c0             	sete   %al
  403763:	24 01                	and    $0x1,%al
  403765:	3c 00                	cmp    $0x0,%al
  403767:	75 12                	jne    40377b <runtime::mem_alloc+0xab>
  403769:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  403770:	00 00 
  403772:	0f 94 c0             	sete   %al
  403775:	24 01                	and    $0x1,%al
  403777:	3c 00                	cmp    $0x0,%al
  403779:	74 1e                	je     403799 <runtime::mem_alloc+0xc9>
  40377b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403780:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  403787:	00 
  403788:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40378f:	31 c0                	xor    %eax,%eax
  403791:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403798:	c3                   	ret
  403799:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40379e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4037a3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4037a8:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  4037ad:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4037b4:	00 
  4037b5:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  4037bc:	00 
  4037bd:	0f 57 c0             	xorps  %xmm0,%xmm0
  4037c0:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4037c5:	48 89 e6             	mov    %rsp,%rsi
  4037c8:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  4037cc:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  4037d1:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  4037d5:	4c 89 06             	mov    %r8,(%rsi)
  4037d8:	31 f6                	xor    %esi,%esi
  4037da:	41 89 f1             	mov    %esi,%r9d
  4037dd:	4d 89 c8             	mov    %r9,%r8
  4037e0:	ff d0                	call   *%rax
  4037e2:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4037e7:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4037ec:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4037f1:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4037f5:	48 89 11             	mov    %rdx,(%rcx)
  4037f8:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  4037ff:	c3                   	ret

0000000000403800 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  403800:	48 83 ec 48          	sub    $0x48,%rsp
  403804:	48 89 0c 24          	mov    %rcx,(%rsp)
  403808:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40380d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403812:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403817:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40381c:	48 8b 04 24          	mov    (%rsp),%rax
  403820:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403825:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40382a:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40382f:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403834:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403839:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40383e:	48 39 c1             	cmp    %rax,%rcx
  403841:	48 0f 4c c1          	cmovl  %rcx,%rax
  403845:	31 c9                	xor    %ecx,%ecx
  403847:	48 39 c1             	cmp    %rax,%rcx
  40384a:	48 0f 4f c1          	cmovg  %rcx,%rax
  40384e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403853:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  403859:	0f 9f c0             	setg   %al
  40385c:	24 01                	and    $0x1,%al
  40385e:	3c 00                	cmp    $0x0,%al
  403860:	74 18                	je     40387a <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)+0x7a>
  403862:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403867:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40386c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  403871:	48 c1 e2 00          	shl    $0x0,%rdx
  403875:	e8 16 d8 ff ff       	call   401090 <memmove@plt>
  40387a:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40387f:	48 83 c4 48          	add    $0x48,%rsp
  403883:	c3                   	ret
  403884:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40388b:	00 00 00 00 00 

0000000000403890 <runtime::print_byte>:
  403890:	48 83 ec 68          	sub    $0x68,%rsp
  403894:	40 88 f8             	mov    %dil,%al
  403897:	88 44 24 07          	mov    %al,0x7(%rsp)
  40389b:	8a 54 24 07          	mov    0x7(%rsp),%dl
  40389f:	88 54 24 67          	mov    %dl,0x67(%rsp)
  4038a3:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  4038aa:	00 00 
  4038ac:	0f 57 c0             	xorps  %xmm0,%xmm0
  4038af:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4038b4:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  4038b9:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  4038be:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4038c3:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  4038ca:	00 00 
  4038cc:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4038d1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4038d6:	88 11                	mov    %dl,(%rcx)
  4038d8:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4038dd:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4038e2:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4038e7:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4038ec:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  4038f3:	00 00 
  4038f5:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  4038fa:	e8 c1 e3 ff ff       	call   401cc0 <runtime::stderr_write>
  4038ff:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403904:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403909:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40390e:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403913:	48 83 c4 68          	add    $0x68,%rsp
  403917:	c3                   	ret
  403918:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40391f:	00 

0000000000403920 <runtime::matrix_bounds_check_error>:
  403920:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403927:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40392c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403931:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  403935:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  403939:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  40393e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403943:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  40394a:	00 
  40394b:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403950:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403957:	00 
  403958:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40395d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403962:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403967:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40396c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403971:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  403975:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  40397a:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  40397f:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  403984:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  403989:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  403990:	00 
  403991:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  403996:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  40399a:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40399f:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  4039a4:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4039a9:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  4039ae:	48 39 c8             	cmp    %rcx,%rax
  4039b1:	0f 92 c0             	setb   %al
  4039b4:	24 01                	and    $0x1,%al
  4039b6:	3c 00                	cmp    $0x0,%al
  4039b8:	74 1e                	je     4039d8 <runtime::matrix_bounds_check_error+0xb8>
  4039ba:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4039bf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4039c4:	48 39 c8             	cmp    %rcx,%rax
  4039c7:	0f 92 c0             	setb   %al
  4039ca:	24 01                	and    $0x1,%al
  4039cc:	3c 00                	cmp    $0x0,%al
  4039ce:	74 08                	je     4039d8 <runtime::matrix_bounds_check_error+0xb8>
  4039d0:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4039d7:	c3                   	ret
  4039d8:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  4039dd:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  4039e2:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  4039e6:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  4039ea:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4039ef:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4039f4:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  4039f9:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  4039fe:	48 89 e0             	mov    %rsp,%rax
  403a01:	4c 89 58 08          	mov    %r11,0x8(%rax)
  403a05:	4c 89 10             	mov    %r10,(%rax)
  403a08:	e8 53 2c 00 00       	call   406660 <runtime::matrix_bounds_check_error.handle_error-0>
  403a0d:	0f 1f 00             	nopl   (%rax)

0000000000403a10 <runtime::heap_alloc>:
  403a10:	48 83 ec 18          	sub    $0x18,%rsp
  403a14:	48 89 3c 24          	mov    %rdi,(%rsp)
  403a18:	40 88 f0             	mov    %sil,%al
  403a1b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  403a1f:	8a 44 24 0e          	mov    0xe(%rsp),%al
  403a23:	48 8b 3c 24          	mov    (%rsp),%rdi
  403a27:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403a2c:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403a30:	0f b6 f0             	movzbl %al,%esi
  403a33:	e8 a8 e6 ff ff       	call   4020e0 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  403a38:	48 83 c4 18          	add    $0x18,%rsp
  403a3c:	c3                   	ret
  403a3d:	0f 1f 00             	nopl   (%rax)

0000000000403a40 <runtime::heap_resize>:
  403a40:	48 83 ec 28          	sub    $0x28,%rsp
  403a44:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403a49:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403a4e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403a53:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403a58:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  403a5d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403a62:	e8 29 e7 ff ff       	call   402190 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  403a67:	48 83 c4 28          	add    $0x28,%rsp
  403a6b:	c3                   	ret
  403a6c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403a70 <runtime::heap_free>:
  403a70:	48 83 ec 18          	sub    $0x18,%rsp
  403a74:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403a79:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403a7e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403a83:	e8 38 ea ff ff       	call   4024c0 <runtime::[heap_allocator_unix.odin]::_heap_free>
  403a88:	48 83 c4 18          	add    $0x18,%rsp
  403a8c:	c3                   	ret
  403a8d:	0f 1f 00             	nopl   (%rax)

0000000000403a90 <runtime::mem_free>:
  403a90:	53                   	push   %rbx
  403a91:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  403a98:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  403a9d:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  403aa2:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403aa7:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403aac:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403ab1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403ab6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  403abb:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  403ac0:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  403ac7:	00 
  403ac8:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403acd:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  403ad2:	48 83 f8 00          	cmp    $0x0,%rax
  403ad6:	0f 94 c0             	sete   %al
  403ad9:	24 01                	and    $0x1,%al
  403adb:	3c 00                	cmp    $0x0,%al
  403add:	75 0f                	jne    403aee <runtime::mem_free+0x5e>
  403adf:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  403ae5:	0f 94 c0             	sete   %al
  403ae8:	24 01                	and    $0x1,%al
  403aea:	3c 00                	cmp    $0x0,%al
  403aec:	74 0b                	je     403af9 <runtime::mem_free+0x69>
  403aee:	31 c0                	xor    %eax,%eax
  403af0:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  403af7:	5b                   	pop    %rbx
  403af8:	c3                   	ret
  403af9:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  403afe:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  403b03:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403b08:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403b0d:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  403b12:	0f 57 c0             	xorps  %xmm0,%xmm0
  403b15:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  403b1a:	be 01 00 00 00       	mov    $0x1,%esi
  403b1f:	31 c9                	xor    %ecx,%ecx
  403b21:	41 89 c9             	mov    %ecx,%r9d
  403b24:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  403b29:	4c 89 ca             	mov    %r9,%rdx
  403b2c:	4c 89 c9             	mov    %r9,%rcx
  403b2f:	48 89 1c 24          	mov    %rbx,(%rsp)
  403b33:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  403b38:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  403b3d:	ff d0                	call   *%rax
  403b3f:	88 44 24 47          	mov    %al,0x47(%rsp)
  403b43:	8a 44 24 47          	mov    0x47(%rsp),%al
  403b47:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  403b4e:	5b                   	pop    %rbx
  403b4f:	c3                   	ret

0000000000403b50 <runtime::print_u64>:
  403b50:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403b57:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403b5c:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403b61:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403b68:	00 
  403b69:	48 8d 7c 24 5f       	lea    0x5f(%rsp),%rdi
  403b6e:	31 f6                	xor    %esi,%esi
  403b70:	ba 81 00 00 00       	mov    $0x81,%edx
  403b75:	e8 c6 d4 ff ff       	call   401040 <memset@plt>
  403b7a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403b7f:	48 c7 44 24 50 81 00 	movq   $0x81,0x50(%rsp)
  403b86:	00 00 
  403b88:	48 c7 44 24 48 0a 00 	movq   $0xa,0x48(%rsp)
  403b8f:	00 00 
  403b91:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403b96:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403b9b:	48 3b 44 24 48       	cmp    0x48(%rsp),%rax
  403ba0:	0f 93 c0             	setae  %al
  403ba3:	24 01                	and    $0x1,%al
  403ba5:	3c 00                	cmp    $0x0,%al
  403ba7:	74 50                	je     403bf9 <runtime::print_u64+0xa9>
  403ba9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403bae:	48 83 e8 01          	sub    $0x1,%rax
  403bb2:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403bb7:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403bbc:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403bc1:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403bc8:	48 8b 08             	mov    (%rax),%rcx
  403bcb:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403bd0:	31 d2                	xor    %edx,%edx
  403bd2:	48 f7 74 24 48       	divq   0x48(%rsp)
  403bd7:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403bdc:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403bdf:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  403be3:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403be8:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403bed:	31 d2                	xor    %edx,%edx
  403bef:	48 f7 f1             	div    %rcx
  403bf2:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403bf7:	eb 9d                	jmp    403b96 <runtime::print_u64+0x46>
  403bf9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403bfe:	48 ff c8             	dec    %rax
  403c01:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403c06:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403c0b:	48 89 04 24          	mov    %rax,(%rsp)
  403c0f:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403c16:	48 8b 08             	mov    (%rax),%rcx
  403c19:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403c1e:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403c23:	31 d2                	xor    %edx,%edx
  403c25:	48 f7 f6             	div    %rsi
  403c28:	48 8b 04 24          	mov    (%rsp),%rax
  403c2c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403c2f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  403c33:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  403c38:	48 8d 4c 14 5f       	lea    0x5f(%rsp,%rdx,1),%rcx
  403c3d:	b8 81 00 00 00       	mov    $0x81,%eax
  403c42:	48 29 d0             	sub    %rdx,%rax
  403c45:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403c4a:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403c4f:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403c54:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403c59:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403c60:	00 00 
  403c62:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403c67:	e8 54 e0 ff ff       	call   401cc0 <runtime::stderr_write>
  403c6c:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  403c73:	c3                   	ret
  403c74:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403c7b:	00 00 00 00 00 

0000000000403c80 <runtime::print_i64>:
  403c80:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403c87:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403c8c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403c91:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403c98:	00 
  403c99:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403ca0:	00 
  403ca1:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  403ca8:	00 00 
  403caa:	0f 9c c0             	setl   %al
  403cad:	24 01                	and    $0x1,%al
  403caf:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  403cb6:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403cbd:	00 
  403cbe:	31 c9                	xor    %ecx,%ecx
  403cc0:	48 29 c1             	sub    %rax,%rcx
  403cc3:	48 83 f8 00          	cmp    $0x0,%rax
  403cc7:	48 0f 4c c1          	cmovl  %rcx,%rax
  403ccb:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403cd2:	00 
  403cd3:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  403cd8:	31 f6                	xor    %esi,%esi
  403cda:	ba 81 00 00 00       	mov    $0x81,%edx
  403cdf:	e8 5c d3 ff ff       	call   401040 <memset@plt>
  403ce4:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  403ceb:	00 00 
  403ced:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  403cf4:	00 0a 
  403cf6:	0f 9d c0             	setge  %al
  403cf9:	24 01                	and    $0x1,%al
  403cfb:	3c 00                	cmp    $0x0,%al
  403cfd:	74 5c                	je     403d5b <runtime::print_i64+0xdb>
  403cff:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d04:	48 83 e8 01          	sub    $0x1,%rax
  403d08:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403d0d:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d12:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  403d17:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403d1e:	48 8b 08             	mov    (%rax),%rcx
  403d21:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403d28:	00 
  403d29:	be 0a 00 00 00       	mov    $0xa,%esi
  403d2e:	48 99                	cqto
  403d30:	48 f7 fe             	idiv   %rsi
  403d33:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403d38:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403d3b:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  403d3f:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403d46:	00 
  403d47:	b9 0a 00 00 00       	mov    $0xa,%ecx
  403d4c:	48 99                	cqto
  403d4e:	48 f7 f9             	idiv   %rcx
  403d51:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  403d58:	00 
  403d59:	eb 92                	jmp    403ced <runtime::print_i64+0x6d>
  403d5b:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d60:	48 83 e8 01          	sub    $0x1,%rax
  403d64:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403d69:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d6e:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403d73:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403d7a:	48 8b 08             	mov    (%rax),%rcx
  403d7d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  403d84:	00 
  403d85:	be 0a 00 00 00       	mov    $0xa,%esi
  403d8a:	48 99                	cqto
  403d8c:	48 f7 fe             	idiv   %rsi
  403d8f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403d94:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403d97:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  403d9b:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  403da2:	00 
  403da3:	74 18                	je     403dbd <runtime::print_i64+0x13d>
  403da5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403daa:	48 83 e8 01          	sub    $0x1,%rax
  403dae:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403db3:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403db8:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  403dbd:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  403dc2:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  403dc7:	b8 81 00 00 00       	mov    $0x81,%eax
  403dcc:	48 29 d0             	sub    %rdx,%rax
  403dcf:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403dd4:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403dd9:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403dde:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  403de3:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  403dea:	00 00 
  403dec:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  403df1:	e8 ca de ff ff       	call   401cc0 <runtime::stderr_write>
  403df6:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  403dfd:	c3                   	ret
  403dfe:	66 90                	xchg   %ax,%ax

0000000000403e00 <runtime::arena_free_last_memory_block>:
  403e00:	48 83 ec 28          	sub    $0x28,%rsp
  403e04:	48 89 3c 24          	mov    %rdi,(%rsp)
  403e08:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403e0d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  403e12:	48 8b 04 24          	mov    (%rsp),%rax
  403e16:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403e1b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e20:	48 8b 40 10          	mov    0x10(%rax),%rax
  403e24:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403e29:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  403e2f:	0f 95 c0             	setne  %al
  403e32:	24 01                	and    $0x1,%al
  403e34:	3c 00                	cmp    $0x0,%al
  403e36:	74 3e                	je     403e76 <runtime::arena_free_last_memory_block+0x76>
  403e38:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  403e3d:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403e42:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e47:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403e4c:	48 8b 09             	mov    (%rcx),%rcx
  403e4f:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403e53:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403e58:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403e5d:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  403e61:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403e65:	48 29 f9             	sub    %rdi,%rcx
  403e68:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403e6c:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403e71:	e8 ca eb ff ff       	call   402a40 <runtime::memory_block_dealloc>
  403e76:	48 83 c4 28          	add    $0x28,%rsp
  403e7a:	c3                   	ret
  403e7b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403e80 <runtime::print_caller_location>:
  403e80:	50                   	push   %rax
  403e81:	48 89 3c 24          	mov    %rdi,(%rsp)
  403e85:	eb 00                	jmp    403e87 <runtime::print_caller_location+0x7>
  403e87:	48 8b 04 24          	mov    (%rsp),%rax
  403e8b:	48 8b 38             	mov    (%rax),%rdi
  403e8e:	48 8b 70 08          	mov    0x8(%rax),%rsi
  403e92:	e8 c9 f7 ff ff       	call   403660 <runtime::print_string>
  403e97:	bf 28 00 00 00       	mov    $0x28,%edi
  403e9c:	e8 ef f9 ff ff       	call   403890 <runtime::print_byte>
  403ea1:	48 8b 04 24          	mov    (%rsp),%rax
  403ea5:	48 63 78 10          	movslq 0x10(%rax),%rdi
  403ea9:	e8 a2 fc ff ff       	call   403b50 <runtime::print_u64>
  403eae:	48 8b 04 24          	mov    (%rsp),%rax
  403eb2:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  403eb6:	0f 95 c0             	setne  %al
  403eb9:	24 01                	and    $0x1,%al
  403ebb:	3c 00                	cmp    $0x0,%al
  403ebd:	74 17                	je     403ed6 <runtime::print_caller_location+0x56>
  403ebf:	bf 3a 00 00 00       	mov    $0x3a,%edi
  403ec4:	e8 c7 f9 ff ff       	call   403890 <runtime::print_byte>
  403ec9:	48 8b 04 24          	mov    (%rsp),%rax
  403ecd:	48 63 78 14          	movslq 0x14(%rax),%rdi
  403ed1:	e8 7a fc ff ff       	call   403b50 <runtime::print_u64>
  403ed6:	bf 29 00 00 00       	mov    $0x29,%edi
  403edb:	e8 b0 f9 ff ff       	call   403890 <runtime::print_byte>
  403ee0:	58                   	pop    %rax
  403ee1:	c3                   	ret
  403ee2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403ee9:	1f 84 00 00 00 00 00 

0000000000403ef0 <runtime::arena_free_all>:
  403ef0:	48 83 ec 28          	sub    $0x28,%rsp
  403ef4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403ef9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403efe:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403f03:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403f08:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403f0d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f12:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403f17:	0f 95 c0             	setne  %al
  403f1a:	24 01                	and    $0x1,%al
  403f1c:	3c 00                	cmp    $0x0,%al
  403f1e:	74 2c                	je     403f4c <runtime::arena_free_all+0x5c>
  403f20:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f25:	48 8b 40 10          	mov    0x10(%rax),%rax
  403f29:	48 83 38 00          	cmpq   $0x0,(%rax)
  403f2d:	0f 95 c0             	setne  %al
  403f30:	24 01                	and    $0x1,%al
  403f32:	3c 00                	cmp    $0x0,%al
  403f34:	74 16                	je     403f4c <runtime::arena_free_all+0x5c>
  403f36:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  403f3b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403f40:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403f45:	e8 b6 fe ff ff       	call   403e00 <runtime::arena_free_last_memory_block>
  403f4a:	eb c1                	jmp    403f0d <runtime::arena_free_all+0x1d>
  403f4c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f51:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403f56:	0f 95 c0             	setne  %al
  403f59:	24 01                	and    $0x1,%al
  403f5b:	3c 00                	cmp    $0x0,%al
  403f5d:	74 32                	je     403f91 <runtime::arena_free_all+0xa1>
  403f5f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f64:	48 8b 40 10          	mov    0x10(%rax),%rax
  403f68:	48 8b 78 18          	mov    0x18(%rax),%rdi
  403f6c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f71:	48 8b 40 10          	mov    0x10(%rax),%rax
  403f75:	48 8b 50 20          	mov    0x20(%rax),%rdx
  403f79:	31 f6                	xor    %esi,%esi
  403f7b:	e8 c0 d0 ff ff       	call   401040 <memset@plt>
  403f80:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f85:	48 8b 40 10          	mov    0x10(%rax),%rax
  403f89:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  403f90:	00 
  403f91:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f96:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  403f9d:	00 
  403f9e:	48 83 c4 28          	add    $0x28,%rsp
  403fa2:	c3                   	ret
  403fa3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403faa:	84 00 00 00 00 00 

0000000000403fb0 <runtime::arena_destroy>:
  403fb0:	48 83 ec 28          	sub    $0x28,%rsp
  403fb4:	48 89 3c 24          	mov    %rdi,(%rsp)
  403fb8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403fbd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  403fc2:	48 8b 04 24          	mov    (%rsp),%rax
  403fc6:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403fcb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403fd0:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403fd5:	0f 95 c0             	setne  %al
  403fd8:	24 01                	and    $0x1,%al
  403fda:	3c 00                	cmp    $0x0,%al
  403fdc:	74 4e                	je     40402c <runtime::arena_destroy+0x7c>
  403fde:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  403fe3:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403fe8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403fed:	48 8b 40 10          	mov    0x10(%rax),%rax
  403ff1:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403ff6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403ffb:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404000:	48 8b 09             	mov    (%rcx),%rcx
  404003:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404007:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40400c:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404011:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  404015:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404019:	48 29 f9             	sub    %rdi,%rcx
  40401c:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404020:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  404025:	e8 16 ea ff ff       	call   402a40 <runtime::memory_block_dealloc>
  40402a:	eb 9f                	jmp    403fcb <runtime::arena_destroy+0x1b>
  40402c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404031:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  404038:	00 
  404039:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40403e:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  404045:	00 
  404046:	48 83 c4 28          	add    $0x28,%rsp
  40404a:	c3                   	ret
  40404b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000404050 <runtime::arena_allocator_proc>:
  404050:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  404057:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  40405c:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  404063:	00 
  404064:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40406b:	00 
  40406c:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  404073:	00 
  404074:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  40407b:	00 
  40407c:	40 88 f0             	mov    %sil,%al
  40407f:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  404086:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  40408d:	00 
  40408e:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  404095:	00 
  404096:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  40409d:	00 
  40409e:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  4040a5:	00 
  4040a6:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  4040ad:	00 
  4040ae:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4040b5:	00 
  4040b6:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  4040bd:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  4040c2:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  4040c9:	00 
  4040ca:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  4040d1:	00 
  4040d2:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  4040d9:	00 
  4040da:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  4040e1:	00 
  4040e2:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  4040e9:	00 
  4040ea:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  4040f1:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  4040f8:	00 
  4040f9:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  404100:	00 
  404101:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  404108:	00 
  404109:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  404110:	00 
  404111:	0f 57 c0             	xorps  %xmm0,%xmm0
  404114:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  40411b:	00 
  40411c:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404123:	00 
  404124:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  40412b:	00 
  40412c:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  404133:	00 
  404134:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  40413b:	00 
  40413c:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  404143:	00 
  404144:	0f b6 c8             	movzbl %al,%ecx
  404147:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40414c:	2c 07                	sub    $0x7,%al
  40414e:	0f 87 9a 07 00 00    	ja     4048ee <runtime::arena_allocator_proc+0x89e>
  404154:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  404159:	48 8b 04 c5 50 70 40 	mov    0x407050(,%rax,8),%rax
  404160:	00 
  404161:	ff e0                	jmp    *%rax
  404163:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  40416a:	00 
  40416b:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404172:	00 
  404173:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  40417a:	00 
  40417b:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404182:	00 
  404183:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  40418a:	00 
  40418b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40418e:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  404195:	00 
  404196:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  40419d:	00 
  40419e:	e8 8d f0 ff ff       	call   403230 <runtime::arena_alloc>
  4041a3:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4041aa:	00 
  4041ab:	40 88 c7             	mov    %al,%dil
  4041ae:	40 88 f8             	mov    %dil,%al
  4041b1:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4041b8:	00 
  4041b9:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4041c0:	00 
  4041c1:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4041c8:	00 
  4041c9:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4041d0:	00 
  4041d1:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  4041d8:	00 
  4041d9:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4041dd:	48 89 11             	mov    %rdx,(%rcx)
  4041e0:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4041e7:	c3                   	ret
  4041e8:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  4041ef:	04 
  4041f0:	e9 f9 06 00 00       	jmp    4048ee <runtime::arena_allocator_proc+0x89e>
  4041f5:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  4041fc:	00 
  4041fd:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  404204:	00 
  404205:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  40420c:	00 
  40420d:	e8 de fc ff ff       	call   403ef0 <runtime::arena_free_all>
  404212:	e9 d7 06 00 00       	jmp    4048ee <runtime::arena_allocator_proc+0x89e>
  404217:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40421e:	00 
  40421f:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  404226:	00 
  404227:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  40422e:	00 00 
  404230:	0f 94 c1             	sete   %cl
  404233:	80 e1 01             	and    $0x1,%cl
  404236:	b0 01                	mov    $0x1,%al
  404238:	38 c8                	cmp    %cl,%al
  40423a:	74 25                	je     404261 <runtime::arena_allocator_proc+0x211>
  40423c:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  404243:	00 
  404244:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  40424b:	00 
  40424c:	0f 94 c1             	sete   %cl
  40424f:	80 e1 01             	and    $0x1,%cl
  404252:	b0 01                	mov    $0x1,%al
  404254:	38 c8                	cmp    %cl,%al
  404256:	0f 84 a8 00 00 00    	je     404304 <runtime::arena_allocator_proc+0x2b4>
  40425c:	e9 85 00 00 00       	jmp    4042e6 <runtime::arena_allocator_proc+0x296>
  404261:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404268:	00 
  404269:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404270:	00 
  404271:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404278:	00 
  404279:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404280:	00 
  404281:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404288:	00 
  404289:	0f 57 c0             	xorps  %xmm0,%xmm0
  40428c:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  404293:	00 
  404294:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  40429b:	00 
  40429c:	e8 8f ef ff ff       	call   403230 <runtime::arena_alloc>
  4042a1:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4042a8:	00 
  4042a9:	40 88 c7             	mov    %al,%dil
  4042ac:	40 88 f8             	mov    %dil,%al
  4042af:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  4042b6:	00 
  4042b7:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  4042be:	00 
  4042bf:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4042c6:	00 
  4042c7:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4042ce:	00 
  4042cf:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  4042d6:	00 
  4042d7:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4042db:	48 89 11             	mov    %rdx,(%rcx)
  4042de:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4042e5:	c3                   	ret
  4042e6:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  4042ed:	00 00 
  4042ef:	0f 94 c1             	sete   %cl
  4042f2:	80 e1 01             	and    $0x1,%cl
  4042f5:	b0 01                	mov    $0x1,%al
  4042f7:	38 c8                	cmp    %cl,%al
  4042f9:	0f 84 e5 00 00 00    	je     4043e4 <runtime::arena_allocator_proc+0x394>
  4042ff:	e9 b7 00 00 00       	jmp    4043bb <runtime::arena_allocator_proc+0x36b>
  404304:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40430b:	00 
  40430c:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  404311:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404318:	00 
  404319:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  40431e:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404323:	31 c0                	xor    %eax,%eax
  404325:	41 89 c0             	mov    %eax,%r8d
  404328:	be 3e 00 00 00       	mov    $0x3e,%esi
  40432d:	ba d1 00 00 00       	mov    $0xd1,%edx
  404332:	b9 13 00 00 00       	mov    $0x13,%ecx
  404337:	e8 94 eb ff ff       	call   402ed0 <runtime::multi_pointer_slice_expr_error>
  40433c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  404341:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  404346:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40434d:	00 
  40434e:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  404355:	00 
  404356:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  40435d:	00 
  40435e:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  404365:	00 
  404366:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  40436d:	00 
  40436e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404375:	00 
  404376:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  40437d:	00 
  40437e:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404385:	00 
  404386:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40438d:	00 
  40438e:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404395:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40439c:	00 
  40439d:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4043a4:	00 
  4043a5:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4043ac:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4043b0:	48 89 11             	mov    %rdx,(%rcx)
  4043b3:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4043ba:	c3                   	ret
  4043bb:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4043c2:	00 
  4043c3:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  4043ca:	00 
  4043cb:	48 83 e9 01          	sub    $0x1,%rcx
  4043cf:	48 21 c8             	and    %rcx,%rax
  4043d2:	48 83 f8 00          	cmp    $0x0,%rax
  4043d6:	0f 94 c1             	sete   %cl
  4043d9:	80 e1 01             	and    $0x1,%cl
  4043dc:	b0 01                	mov    $0x1,%al
  4043de:	38 c8                	cmp    %cl,%al
  4043e0:	74 54                	je     404436 <runtime::arena_allocator_proc+0x3e6>
  4043e2:	eb 4d                	jmp    404431 <runtime::arena_allocator_proc+0x3e1>
  4043e4:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4043eb:	00 
  4043ec:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  4043f3:	04 
  4043f4:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4043fb:	00 
  4043fc:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404403:	00 
  404404:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40440b:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404412:	00 
  404413:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40441a:	00 
  40441b:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404422:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404426:	48 89 11             	mov    %rdx,(%rcx)
  404429:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404430:	c3                   	ret
  404431:	e9 94 02 00 00       	jmp    4046ca <runtime::arena_allocator_proc+0x67a>
  404436:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  40443d:	00 
  40443e:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  404445:	00 
  404446:	0f 92 c0             	setb   %al
  404449:	24 01                	and    $0x1,%al
  40444b:	3c 00                	cmp    $0x0,%al
  40444d:	0f 84 b7 00 00 00    	je     40450a <runtime::arena_allocator_proc+0x4ba>
  404453:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40445a:	00 
  40445b:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404460:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404467:	00 
  404468:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  40446d:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404472:	31 c0                	xor    %eax,%eax
  404474:	41 89 c0             	mov    %eax,%r8d
  404477:	be 3e 00 00 00       	mov    $0x3e,%esi
  40447c:	ba d9 00 00 00       	mov    $0xd9,%edx
  404481:	b9 14 00 00 00       	mov    $0x14,%ecx
  404486:	e8 45 ea ff ff       	call   402ed0 <runtime::multi_pointer_slice_expr_error>
  40448b:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  404490:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  404495:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40449c:	00 
  40449d:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  4044a4:	00 
  4044a5:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  4044ac:	00 
  4044ad:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  4044b4:	00 
  4044b5:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  4044bc:	00 
  4044bd:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4044c4:	00 
  4044c5:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4044cc:	00 
  4044cd:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4044d4:	00 
  4044d5:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4044dc:	00 
  4044dd:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4044e4:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4044eb:	00 
  4044ec:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4044f3:	00 
  4044f4:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4044fb:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4044ff:	48 89 11             	mov    %rdx,(%rcx)
  404502:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404509:	c3                   	ret
  40450a:	eb 00                	jmp    40450c <runtime::arena_allocator_proc+0x4bc>
  40450c:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  404513:	00 
  404514:	48 8b 40 10          	mov    0x10(%rax),%rax
  404518:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40451f:	00 
  404520:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  404527:	00 00 
  404529:	0f 95 c0             	setne  %al
  40452c:	24 01                	and    $0x1,%al
  40452e:	3c 00                	cmp    $0x0,%al
  404530:	0f 84 92 01 00 00    	je     4046c8 <runtime::arena_allocator_proc+0x678>
  404536:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40453d:	00 
  40453e:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404545:	00 
  404546:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  40454a:	48 29 c8             	sub    %rcx,%rax
  40454d:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  404554:	00 
  404555:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40455c:	00 
  40455d:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  404564:	00 
  404565:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  40456c:	00 
  40456d:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404574:	00 
  404575:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  40457c:	00 
  40457d:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  404584:	00 
  404585:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40458c:	00 
  40458d:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  404594:	00 
  404595:	0f 92 c0             	setb   %al
  404598:	24 01                	and    $0x1,%al
  40459a:	3c 00                	cmp    $0x0,%al
  40459c:	0f 84 24 01 00 00    	je     4046c6 <runtime::arena_allocator_proc+0x676>
  4045a2:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4045a9:	00 
  4045aa:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  4045b1:	00 
  4045b2:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  4045b6:	0f 94 c0             	sete   %al
  4045b9:	24 01                	and    $0x1,%al
  4045bb:	3c 00                	cmp    $0x0,%al
  4045bd:	0f 84 03 01 00 00    	je     4046c6 <runtime::arena_allocator_proc+0x676>
  4045c3:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  4045ca:	00 
  4045cb:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  4045d2:	00 
  4045d3:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  4045d7:	0f 96 c0             	setbe  %al
  4045da:	24 01                	and    $0x1,%al
  4045dc:	3c 00                	cmp    $0x0,%al
  4045de:	0f 84 e2 00 00 00    	je     4046c6 <runtime::arena_allocator_proc+0x676>
  4045e4:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4045eb:	00 
  4045ec:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  4045f3:	00 
  4045f4:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4045f8:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4045ff:	00 
  404600:	48 8b 40 18          	mov    0x18(%rax),%rax
  404604:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404609:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  404610:	00 
  404611:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  404616:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  40461d:	00 
  40461e:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  404623:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404628:	be 3e 00 00 00       	mov    $0x3e,%esi
  40462d:	ba e4 00 00 00       	mov    $0xe4,%edx
  404632:	b9 17 00 00 00       	mov    $0x17,%ecx
  404637:	e8 94 e8 ff ff       	call   402ed0 <runtime::multi_pointer_slice_expr_error>
  40463c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404641:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404646:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40464b:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404652:	00 
  404653:	48 01 f2             	add    %rsi,%rdx
  404656:	48 29 f0             	sub    %rsi,%rax
  404659:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  404660:	00 
  404661:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  404668:	00 
  404669:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  404670:	00 
  404671:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  404678:	00 
  404679:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404680:	00 
  404681:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404688:	00 
  404689:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404690:	00 
  404691:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404698:	00 
  404699:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4046a0:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4046a7:	00 
  4046a8:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4046af:	00 
  4046b0:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4046b7:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4046bb:	48 89 11             	mov    %rdx,(%rcx)
  4046be:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4046c5:	c3                   	ret
  4046c6:	eb 00                	jmp    4046c8 <runtime::arena_allocator_proc+0x678>
  4046c8:	eb 00                	jmp    4046ca <runtime::arena_allocator_proc+0x67a>
  4046ca:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  4046d1:	00 
  4046d2:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4046d9:	00 
  4046da:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4046e1:	00 
  4046e2:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4046e9:	00 
  4046ea:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4046f1:	00 
  4046f2:	0f 57 c0             	xorps  %xmm0,%xmm0
  4046f5:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  4046fc:	00 
  4046fd:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  404704:	00 
  404705:	e8 26 eb ff ff       	call   403230 <runtime::arena_alloc>
  40470a:	88 44 24 27          	mov    %al,0x27(%rsp)
  40470e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  404715:	00 
  404716:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40471b:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  404722:	00 
  404723:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404728:	3c 00                	cmp    $0x0,%al
  40472a:	74 50                	je     40477c <runtime::arena_allocator_proc+0x72c>
  40472c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404733:	00 
  404734:	8a 44 24 27          	mov    0x27(%rsp),%al
  404738:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40473f:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404746:	00 
  404747:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40474e:	00 
  40474f:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404756:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40475d:	00 
  40475e:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404765:	00 
  404766:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40476d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404771:	48 89 11             	mov    %rdx,(%rcx)
  404774:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40477b:	c3                   	ret
  40477c:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404781:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  404786:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  40478d:	00 
  40478e:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404795:	00 
  404796:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  40479d:	00 00 
  40479f:	0f 94 c0             	sete   %al
  4047a2:	24 01                	and    $0x1,%al
  4047a4:	3c 00                	cmp    $0x0,%al
  4047a6:	74 45                	je     4047ed <runtime::arena_allocator_proc+0x79d>
  4047a8:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4047af:	00 
  4047b0:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4047b7:	00 
  4047b8:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4047bf:	00 
  4047c0:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4047c7:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4047ce:	00 
  4047cf:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4047d6:	00 
  4047d7:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4047de:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4047e2:	48 89 11             	mov    %rdx,(%rcx)
  4047e5:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4047ec:	c3                   	ret
  4047ed:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4047f4:	00 
  4047f5:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4047fa:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  404801:	00 
  404802:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404807:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40480e:	00 
  40480f:	48 89 04 24          	mov    %rax,(%rsp)
  404813:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  40481a:	00 
  40481b:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  404820:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404825:	31 c0                	xor    %eax,%eax
  404827:	41 89 c0             	mov    %eax,%r8d
  40482a:	be 3e 00 00 00       	mov    $0x3e,%esi
  40482f:	ba ee 00 00 00       	mov    $0xee,%edx
  404834:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  404839:	e8 92 e6 ff ff       	call   402ed0 <runtime::multi_pointer_slice_expr_error>
  40483e:	48 8b 0c 24          	mov    (%rsp),%rcx
  404842:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404847:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  40484c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404851:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  404858:	00 
  404859:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404860:	00 
  404861:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  404868:	00 
  404869:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  404870:	00 
  404871:	e8 8a ef ff ff       	call   403800 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  404876:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40487d:	00 
  40487e:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  404885:	00 
  404886:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40488d:	00 
  40488e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404895:	00 
  404896:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  40489d:	00 
  40489e:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  4048a5:	00 
  4048a6:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4048aa:	48 89 08             	mov    %rcx,(%rax)
  4048ad:	31 c0                	xor    %eax,%eax
  4048af:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4048b6:	c3                   	ret
  4048b7:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4048be:	00 
  4048bf:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4048c6:	00 
  4048c7:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  4048ce:	00 00 
  4048d0:	0f 95 c0             	setne  %al
  4048d3:	24 01                	and    $0x1,%al
  4048d5:	3c 00                	cmp    $0x0,%al
  4048d7:	74 0b                	je     4048e4 <runtime::arena_allocator_proc+0x894>
  4048d9:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4048e0:	00 
  4048e1:	c6 00 5d             	movb   $0x5d,(%rax)
  4048e4:	eb 08                	jmp    4048ee <runtime::arena_allocator_proc+0x89e>
  4048e6:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  4048ed:	04 
  4048ee:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4048f5:	00 
  4048f6:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4048fd:	00 
  4048fe:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404905:	00 
  404906:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40490d:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404914:	00 
  404915:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40491c:	00 
  40491d:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404924:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404928:	48 89 11             	mov    %rdx,(%rcx)
  40492b:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404932:	c3                   	ret
  404933:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40493a:	84 00 00 00 00 00 

0000000000404940 <runtime::memory_equal>:
  404940:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  404945:	48 89 74 24 b8       	mov    %rsi,-0x48(%rsp)
  40494a:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  40494f:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404954:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404959:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  40495e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404963:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404968:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40496d:	48 83 f8 00          	cmp    $0x0,%rax
  404971:	0f 94 c1             	sete   %cl
  404974:	80 e1 01             	and    $0x1,%cl
  404977:	b0 01                	mov    $0x1,%al
  404979:	38 c8                	cmp    %cl,%al
  40497b:	74 1b                	je     404998 <runtime::memory_equal+0x58>
  40497d:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404982:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404987:	48 39 c8             	cmp    %rcx,%rax
  40498a:	0f 94 c1             	sete   %cl
  40498d:	80 e1 01             	and    $0x1,%cl
  404990:	b0 01                	mov    $0x1,%al
  404992:	38 c8                	cmp    %cl,%al
  404994:	74 07                	je     40499d <runtime::memory_equal+0x5d>
  404996:	eb 03                	jmp    40499b <runtime::memory_equal+0x5b>
  404998:	b0 01                	mov    $0x1,%al
  40499a:	c3                   	ret
  40499b:	eb 03                	jmp    4049a0 <runtime::memory_equal+0x60>
  40499d:	b0 01                	mov    $0x1,%al
  40499f:	c3                   	ret
  4049a0:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4049a5:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  4049aa:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  4049af:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  4049b4:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4049b9:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4049be:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  4049c5:	00 00 
  4049c7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4049cc:	48 3b 44 24 d0       	cmp    -0x30(%rsp),%rax
  4049d1:	0f 92 c0             	setb   %al
  4049d4:	24 01                	and    $0x1,%al
  4049d6:	3c 00                	cmp    $0x0,%al
  4049d8:	74 38                	je     404a12 <runtime::memory_equal+0xd2>
  4049da:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4049df:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4049e4:	8a 04 08             	mov    (%rax,%rcx,1),%al
  4049e7:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4049ec:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4049f1:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  4049f4:	0f 95 c0             	setne  %al
  4049f7:	24 01                	and    $0x1,%al
  4049f9:	3c 00                	cmp    $0x0,%al
  4049fb:	74 03                	je     404a00 <runtime::memory_equal+0xc0>
  4049fd:	31 c0                	xor    %eax,%eax
  4049ff:	c3                   	ret
  404a00:	eb 00                	jmp    404a02 <runtime::memory_equal+0xc2>
  404a02:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404a07:	48 83 c0 01          	add    $0x1,%rax
  404a0b:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404a10:	eb b5                	jmp    4049c7 <runtime::memory_equal+0x87>
  404a12:	b0 01                	mov    $0x1,%al
  404a14:	c3                   	ret
  404a15:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  404a1c:	00 00 00 00 

0000000000404a20 <runtime::memory_compare>:
  404a20:	48 83 ec 10          	sub    $0x10,%rsp
  404a24:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  404a29:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  404a2e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  404a33:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404a38:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  404a3d:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  404a42:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404a47:	48 89 0c 24          	mov    %rcx,(%rsp)
  404a4b:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404a50:	48 39 c8             	cmp    %rcx,%rax
  404a53:	0f 94 c1             	sete   %cl
  404a56:	80 e1 01             	and    $0x1,%cl
  404a59:	b0 01                	mov    $0x1,%al
  404a5b:	38 c8                	cmp    %cl,%al
  404a5d:	74 17                	je     404a76 <runtime::memory_compare+0x56>
  404a5f:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404a64:	48 83 f8 00          	cmp    $0x0,%rax
  404a68:	0f 94 c1             	sete   %cl
  404a6b:	80 e1 01             	and    $0x1,%cl
  404a6e:	b0 01                	mov    $0x1,%al
  404a70:	38 c8                	cmp    %cl,%al
  404a72:	74 20                	je     404a94 <runtime::memory_compare+0x74>
  404a74:	eb 07                	jmp    404a7d <runtime::memory_compare+0x5d>
  404a76:	31 c0                	xor    %eax,%eax
  404a78:	48 83 c4 10          	add    $0x10,%rsp
  404a7c:	c3                   	ret
  404a7d:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  404a82:	48 83 f8 00          	cmp    $0x0,%rax
  404a86:	0f 94 c1             	sete   %cl
  404a89:	80 e1 01             	and    $0x1,%cl
  404a8c:	b0 01                	mov    $0x1,%al
  404a8e:	38 c8                	cmp    %cl,%al
  404a90:	74 10                	je     404aa2 <runtime::memory_compare+0x82>
  404a92:	eb 0c                	jmp    404aa0 <runtime::memory_compare+0x80>
  404a94:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404a9b:	48 83 c4 10          	add    $0x10,%rsp
  404a9f:	c3                   	ret
  404aa0:	eb 0a                	jmp    404aac <runtime::memory_compare+0x8c>
  404aa2:	b8 01 00 00 00       	mov    $0x1,%eax
  404aa7:	48 83 c4 10          	add    $0x10,%rsp
  404aab:	c3                   	ret
  404aac:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  404ab1:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  404ab6:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  404abb:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  404ac0:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  404ac5:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404aca:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404acf:	48 c1 e8 03          	shr    $0x3,%rax
  404ad3:	48 83 c0 01          	add    $0x1,%rax
  404ad7:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404adc:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404ae1:	48 83 e8 01          	sub    $0x1,%rax
  404ae5:	48 c1 e0 03          	shl    $0x3,%rax
  404ae9:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404aee:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  404af5:	00 00 
  404af7:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  404afd:	0f 92 c0             	setb   %al
  404b00:	24 01                	and    $0x1,%al
  404b02:	3c 00                	cmp    $0x0,%al
  404b04:	74 09                	je     404b0f <runtime::memory_compare+0xef>
  404b06:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404b0d:	00 00 
  404b0f:	eb 00                	jmp    404b11 <runtime::memory_compare+0xf1>
  404b11:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404b16:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  404b1b:	0f 92 c0             	setb   %al
  404b1e:	24 01                	and    $0x1,%al
  404b20:	3c 00                	cmp    $0x0,%al
  404b22:	0f 84 11 01 00 00    	je     404c39 <runtime::memory_compare+0x219>
  404b28:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404b2d:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404b32:	48 c1 e1 03          	shl    $0x3,%rcx
  404b36:	48 01 c8             	add    %rcx,%rax
  404b39:	48 8b 00             	mov    (%rax),%rax
  404b3c:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404b41:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404b46:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404b4b:	48 c1 e1 03          	shl    $0x3,%rcx
  404b4f:	48 01 c8             	add    %rcx,%rax
  404b52:	48 8b 00             	mov    (%rax),%rax
  404b55:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404b5a:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404b5f:	48 33 44 24 b8       	xor    -0x48(%rsp),%rax
  404b64:	48 83 f8 00          	cmp    $0x0,%rax
  404b68:	0f 95 c0             	setne  %al
  404b6b:	24 01                	and    $0x1,%al
  404b6d:	3c 00                	cmp    $0x0,%al
  404b6f:	0f 84 af 00 00 00    	je     404c24 <runtime::memory_compare+0x204>
  404b75:	eb 00                	jmp    404b77 <runtime::memory_compare+0x157>
  404b77:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404b7c:	48 c1 e0 03          	shl    $0x3,%rax
  404b80:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  404b85:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404b8a:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404b8f:	0f 92 c0             	setb   %al
  404b92:	24 01                	and    $0x1,%al
  404b94:	3c 00                	cmp    $0x0,%al
  404b96:	0f 84 86 00 00 00    	je     404c22 <runtime::memory_compare+0x202>
  404b9c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404ba1:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  404ba6:	8a 00                	mov    (%rax),%al
  404ba8:	88 44 24 af          	mov    %al,-0x51(%rsp)
  404bac:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404bb1:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  404bb6:	8a 00                	mov    (%rax),%al
  404bb8:	88 44 24 ae          	mov    %al,-0x52(%rsp)
  404bbc:	8a 44 24 af          	mov    -0x51(%rsp),%al
  404bc0:	32 44 24 ae          	xor    -0x52(%rsp),%al
  404bc4:	3c 00                	cmp    $0x0,%al
  404bc6:	0f 95 c0             	setne  %al
  404bc9:	24 01                	and    $0x1,%al
  404bcb:	3c 00                	cmp    $0x0,%al
  404bcd:	74 3e                	je     404c0d <runtime::memory_compare+0x1ed>
  404bcf:	0f b6 44 24 af       	movzbl -0x51(%rsp),%eax
  404bd4:	0f b6 4c 24 ae       	movzbl -0x52(%rsp),%ecx
  404bd9:	48 29 c8             	sub    %rcx,%rax
  404bdc:	48 83 f8 00          	cmp    $0x0,%rax
  404be0:	0f 9c c0             	setl   %al
  404be3:	24 01                	and    $0x1,%al
  404be5:	3c 00                	cmp    $0x0,%al
  404be7:	74 0e                	je     404bf7 <runtime::memory_compare+0x1d7>
  404be9:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404bf0:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  404bf5:	eb 0c                	jmp    404c03 <runtime::memory_compare+0x1e3>
  404bf7:	b8 01 00 00 00       	mov    $0x1,%eax
  404bfc:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  404c01:	eb 00                	jmp    404c03 <runtime::memory_compare+0x1e3>
  404c03:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  404c08:	48 83 c4 10          	add    $0x10,%rsp
  404c0c:	c3                   	ret
  404c0d:	eb 00                	jmp    404c0f <runtime::memory_compare+0x1ef>
  404c0f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404c14:	48 83 c0 01          	add    $0x1,%rax
  404c18:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  404c1d:	e9 63 ff ff ff       	jmp    404b85 <runtime::memory_compare+0x165>
  404c22:	eb 00                	jmp    404c24 <runtime::memory_compare+0x204>
  404c24:	eb 00                	jmp    404c26 <runtime::memory_compare+0x206>
  404c26:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404c2b:	48 83 c0 01          	add    $0x1,%rax
  404c2f:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404c34:	e9 d8 fe ff ff       	jmp    404b11 <runtime::memory_compare+0xf1>
  404c39:	eb 00                	jmp    404c3b <runtime::memory_compare+0x21b>
  404c3b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404c40:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404c45:	0f 92 c0             	setb   %al
  404c48:	24 01                	and    $0x1,%al
  404c4a:	3c 00                	cmp    $0x0,%al
  404c4c:	0f 84 86 00 00 00    	je     404cd8 <runtime::memory_compare+0x2b8>
  404c52:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404c57:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404c5c:	8a 00                	mov    (%rax),%al
  404c5e:	88 44 24 ad          	mov    %al,-0x53(%rsp)
  404c62:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404c67:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404c6c:	8a 00                	mov    (%rax),%al
  404c6e:	88 44 24 ac          	mov    %al,-0x54(%rsp)
  404c72:	8a 44 24 ad          	mov    -0x53(%rsp),%al
  404c76:	32 44 24 ac          	xor    -0x54(%rsp),%al
  404c7a:	3c 00                	cmp    $0x0,%al
  404c7c:	0f 95 c0             	setne  %al
  404c7f:	24 01                	and    $0x1,%al
  404c81:	3c 00                	cmp    $0x0,%al
  404c83:	74 3e                	je     404cc3 <runtime::memory_compare+0x2a3>
  404c85:	0f b6 44 24 ad       	movzbl -0x53(%rsp),%eax
  404c8a:	0f b6 4c 24 ac       	movzbl -0x54(%rsp),%ecx
  404c8f:	48 29 c8             	sub    %rcx,%rax
  404c92:	48 83 f8 00          	cmp    $0x0,%rax
  404c96:	0f 9c c0             	setl   %al
  404c99:	24 01                	and    $0x1,%al
  404c9b:	3c 00                	cmp    $0x0,%al
  404c9d:	74 0e                	je     404cad <runtime::memory_compare+0x28d>
  404c9f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404ca6:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  404cab:	eb 0c                	jmp    404cb9 <runtime::memory_compare+0x299>
  404cad:	b8 01 00 00 00       	mov    $0x1,%eax
  404cb2:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  404cb7:	eb 00                	jmp    404cb9 <runtime::memory_compare+0x299>
  404cb9:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  404cbe:	48 83 c4 10          	add    $0x10,%rsp
  404cc2:	c3                   	ret
  404cc3:	eb 00                	jmp    404cc5 <runtime::memory_compare+0x2a5>
  404cc5:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404cca:	48 83 c0 01          	add    $0x1,%rax
  404cce:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404cd3:	e9 63 ff ff ff       	jmp    404c3b <runtime::memory_compare+0x21b>
  404cd8:	31 c0                	xor    %eax,%eax
  404cda:	48 83 c4 10          	add    $0x10,%rsp
  404cde:	c3                   	ret
  404cdf:	90                   	nop

0000000000404ce0 <runtime::memory_compare_zero>:
  404ce0:	48 89 7c 24 a0       	mov    %rdi,-0x60(%rsp)
  404ce5:	48 89 74 24 a8       	mov    %rsi,-0x58(%rsp)
  404cea:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  404cef:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  404cf4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  404cf9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  404cfe:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  404d03:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404d08:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404d0d:	48 c1 e8 03          	shr    $0x3,%rax
  404d11:	48 83 c0 01          	add    $0x1,%rax
  404d15:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404d1a:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404d1f:	48 83 e8 01          	sub    $0x1,%rax
  404d23:	48 c1 e0 03          	shl    $0x3,%rax
  404d27:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404d2c:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  404d33:	00 00 
  404d35:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  404d3b:	0f 92 c0             	setb   %al
  404d3e:	24 01                	and    $0x1,%al
  404d40:	3c 00                	cmp    $0x0,%al
  404d42:	74 09                	je     404d4d <runtime::memory_compare_zero+0x6d>
  404d44:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404d4b:	00 00 
  404d4d:	eb 00                	jmp    404d4f <runtime::memory_compare_zero+0x6f>
  404d4f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404d54:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  404d59:	0f 92 c0             	setb   %al
  404d5c:	24 01                	and    $0x1,%al
  404d5e:	3c 00                	cmp    $0x0,%al
  404d60:	0f 84 d2 00 00 00    	je     404e38 <runtime::memory_compare_zero+0x158>
  404d66:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404d6b:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404d70:	48 c1 e1 03          	shl    $0x3,%rcx
  404d74:	48 01 c8             	add    %rcx,%rax
  404d77:	48 8b 00             	mov    (%rax),%rax
  404d7a:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404d7f:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404d84:	48 83 f0 00          	xor    $0x0,%rax
  404d88:	48 83 f8 00          	cmp    $0x0,%rax
  404d8c:	0f 95 c0             	setne  %al
  404d8f:	24 01                	and    $0x1,%al
  404d91:	3c 00                	cmp    $0x0,%al
  404d93:	0f 84 8a 00 00 00    	je     404e23 <runtime::memory_compare_zero+0x143>
  404d99:	eb 00                	jmp    404d9b <runtime::memory_compare_zero+0xbb>
  404d9b:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404da0:	48 c1 e0 03          	shl    $0x3,%rax
  404da4:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404da9:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  404dae:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404db3:	0f 92 c0             	setb   %al
  404db6:	24 01                	and    $0x1,%al
  404db8:	3c 00                	cmp    $0x0,%al
  404dba:	74 65                	je     404e21 <runtime::memory_compare_zero+0x141>
  404dbc:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404dc1:	48 03 44 24 b8       	add    -0x48(%rsp),%rax
  404dc6:	8a 00                	mov    (%rax),%al
  404dc8:	88 44 24 b7          	mov    %al,-0x49(%rsp)
  404dcc:	8a 44 24 b7          	mov    -0x49(%rsp),%al
  404dd0:	34 00                	xor    $0x0,%al
  404dd2:	3c 00                	cmp    $0x0,%al
  404dd4:	0f 95 c0             	setne  %al
  404dd7:	24 01                	and    $0x1,%al
  404dd9:	3c 00                	cmp    $0x0,%al
  404ddb:	74 32                	je     404e0f <runtime::memory_compare_zero+0x12f>
  404ddd:	0f b6 44 24 b7       	movzbl -0x49(%rsp),%eax
  404de2:	48 83 f8 00          	cmp    $0x0,%rax
  404de6:	0f 9c c0             	setl   %al
  404de9:	24 01                	and    $0x1,%al
  404deb:	3c 00                	cmp    $0x0,%al
  404ded:	74 0e                	je     404dfd <runtime::memory_compare_zero+0x11d>
  404def:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404df6:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  404dfb:	eb 0c                	jmp    404e09 <runtime::memory_compare_zero+0x129>
  404dfd:	b8 01 00 00 00       	mov    $0x1,%eax
  404e02:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  404e07:	eb 00                	jmp    404e09 <runtime::memory_compare_zero+0x129>
  404e09:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  404e0e:	c3                   	ret
  404e0f:	eb 00                	jmp    404e11 <runtime::memory_compare_zero+0x131>
  404e11:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  404e16:	48 83 c0 01          	add    $0x1,%rax
  404e1a:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404e1f:	eb 88                	jmp    404da9 <runtime::memory_compare_zero+0xc9>
  404e21:	eb 00                	jmp    404e23 <runtime::memory_compare_zero+0x143>
  404e23:	eb 00                	jmp    404e25 <runtime::memory_compare_zero+0x145>
  404e25:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404e2a:	48 83 c0 01          	add    $0x1,%rax
  404e2e:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404e33:	e9 17 ff ff ff       	jmp    404d4f <runtime::memory_compare_zero+0x6f>
  404e38:	eb 00                	jmp    404e3a <runtime::memory_compare_zero+0x15a>
  404e3a:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404e3f:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404e44:	0f 92 c0             	setb   %al
  404e47:	24 01                	and    $0x1,%al
  404e49:	3c 00                	cmp    $0x0,%al
  404e4b:	74 65                	je     404eb2 <runtime::memory_compare_zero+0x1d2>
  404e4d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404e52:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404e57:	8a 00                	mov    (%rax),%al
  404e59:	88 44 24 b6          	mov    %al,-0x4a(%rsp)
  404e5d:	8a 44 24 b6          	mov    -0x4a(%rsp),%al
  404e61:	34 00                	xor    $0x0,%al
  404e63:	3c 00                	cmp    $0x0,%al
  404e65:	0f 95 c0             	setne  %al
  404e68:	24 01                	and    $0x1,%al
  404e6a:	3c 00                	cmp    $0x0,%al
  404e6c:	74 32                	je     404ea0 <runtime::memory_compare_zero+0x1c0>
  404e6e:	0f b6 44 24 b6       	movzbl -0x4a(%rsp),%eax
  404e73:	48 83 f8 00          	cmp    $0x0,%rax
  404e77:	0f 9c c0             	setl   %al
  404e7a:	24 01                	and    $0x1,%al
  404e7c:	3c 00                	cmp    $0x0,%al
  404e7e:	74 0e                	je     404e8e <runtime::memory_compare_zero+0x1ae>
  404e80:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404e87:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  404e8c:	eb 0c                	jmp    404e9a <runtime::memory_compare_zero+0x1ba>
  404e8e:	b8 01 00 00 00       	mov    $0x1,%eax
  404e93:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  404e98:	eb 00                	jmp    404e9a <runtime::memory_compare_zero+0x1ba>
  404e9a:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404e9f:	c3                   	ret
  404ea0:	eb 00                	jmp    404ea2 <runtime::memory_compare_zero+0x1c2>
  404ea2:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404ea7:	48 83 c0 01          	add    $0x1,%rax
  404eab:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404eb0:	eb 88                	jmp    404e3a <runtime::memory_compare_zero+0x15a>
  404eb2:	31 c0                	xor    %eax,%eax
  404eb4:	c3                   	ret
  404eb5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  404ebc:	00 00 00 00 

0000000000404ec0 <runtime::__type_info_of>:
  404ec0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  404ec5:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404eca:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  404ecf:	48 c7 c1 38 73 40 00 	mov    $0x407338,%rcx
  404ed6:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  404eda:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404edf:	31 c9                	xor    %ecx,%ecx
  404ee1:	89 ca                	mov    %ecx,%edx
  404ee3:	48 f7 74 24 f0       	divq   -0x10(%rsp)
  404ee8:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  404eed:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  404ef4:	00 00 
  404ef6:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404efd:	00 00 
  404eff:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404f04:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  404f09:	0f 83 9f 00 00 00    	jae    404fae <runtime::__type_info_of+0xee>
  404f0f:	48 c7 c0 38 73 40 00 	mov    $0x407338,%rax
  404f16:	48 8b 00             	mov    (%rax),%rax
  404f19:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  404f1e:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  404f22:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404f27:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  404f2d:	0f 95 c0             	setne  %al
  404f30:	24 01                	and    $0x1,%al
  404f32:	3c 00                	cmp    $0x0,%al
  404f34:	74 1d                	je     404f53 <runtime::__type_info_of+0x93>
  404f36:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404f3b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404f40:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  404f44:	0f 94 c0             	sete   %al
  404f47:	24 01                	and    $0x1,%al
  404f49:	3c 00                	cmp    $0x0,%al
  404f4b:	74 06                	je     404f53 <runtime::__type_info_of+0x93>
  404f4d:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404f52:	c3                   	ret
  404f53:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404f58:	48 83 c0 01          	add    $0x1,%rax
  404f5c:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  404f61:	0f 92 c0             	setb   %al
  404f64:	24 01                	and    $0x1,%al
  404f66:	3c 00                	cmp    $0x0,%al
  404f68:	74 10                	je     404f7a <runtime::__type_info_of+0xba>
  404f6a:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404f6f:	48 83 c0 01          	add    $0x1,%rax
  404f73:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404f78:	eb 09                	jmp    404f83 <runtime::__type_info_of+0xc3>
  404f7a:	31 c0                	xor    %eax,%eax
  404f7c:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404f81:	eb 00                	jmp    404f83 <runtime::__type_info_of+0xc3>
  404f83:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404f88:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  404f8d:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404f92:	48 83 c0 01          	add    $0x1,%rax
  404f96:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404f9b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404fa0:	48 83 c0 01          	add    $0x1,%rax
  404fa4:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404fa9:	e9 51 ff ff ff       	jmp    404eff <runtime::__type_info_of+0x3f>
  404fae:	48 c7 c0 38 73 40 00 	mov    $0x407338,%rax
  404fb5:	48 8b 00             	mov    (%rax),%rax
  404fb8:	48 8b 00             	mov    (%rax),%rax
  404fbb:	c3                   	ret
  404fbc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000404fc0 <runtime::default_logger_proc>:
  404fc0:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  404fc5:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  404fca:	66 44 89 c0          	mov    %r8w,%ax
  404fce:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  404fd3:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  404fd8:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  404fdd:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  404fe2:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  404fe7:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  404fec:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  404ff1:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  404ff6:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  404ffb:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  405000:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  405005:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40500a:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  40500f:	c3                   	ret

0000000000405010 <runtime::default_context>:
  405010:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  405017:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40501c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  405021:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405026:	31 f6                	xor    %esi,%esi
  405028:	ba 70 00 00 00       	mov    $0x70,%edx
  40502d:	e8 0e c0 ff ff       	call   401040 <memset@plt>
  405032:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405037:	e8 24 00 00 00       	call   405060 <runtime::[core.odin]::__init_context>
  40503c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  405041:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  405046:	ba 70 00 00 00       	mov    $0x70,%edx
  40504b:	e8 10 c0 ff ff       	call   401060 <memcpy@plt>
  405050:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405055:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40505c:	c3                   	ret
  40505d:	0f 1f 00             	nopl   (%rax)

0000000000405060 <runtime::[core.odin]::__init_context>:
  405060:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  405065:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40506a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40506f:	48 83 f8 00          	cmp    $0x0,%rax
  405073:	0f 94 c0             	sete   %al
  405076:	24 01                	and    $0x1,%al
  405078:	3c 00                	cmp    $0x0,%al
  40507a:	74 01                	je     40507d <runtime::[core.odin]::__init_context+0x1d>
  40507c:	c3                   	ret
  40507d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405082:	48 c7 c1 90 1d 40 00 	mov    $0x401d90,%rcx
  405089:	48 89 08             	mov    %rcx,(%rax)
  40508c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405091:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405098:	00 
  405099:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40509e:	48 c7 c1 20 28 40 00 	mov    $0x402820,%rcx
  4050a5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4050a9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050ae:	48 c7 c2 c0 ff ff ff 	mov    $0xffffffffffffffc0,%rdx
  4050b5:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  4050bc:	00 00 
  4050be:	48 01 d1             	add    %rdx,%rcx
  4050c1:	48 89 48 18          	mov    %rcx,0x18(%rax)
  4050c5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050ca:	48 c7 c1 10 51 40 00 	mov    $0x405110,%rcx
  4050d1:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4050d5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050da:	48 c7 c1 c0 4f 40 00 	mov    $0x404fc0,%rcx
  4050e1:	48 89 48 28          	mov    %rcx,0x28(%rax)
  4050e5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050ea:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  4050f1:	00 
  4050f2:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4050f7:	48 c7 c1 e0 24 40 00 	mov    $0x4024e0,%rcx
  4050fe:	48 89 48 48          	mov    %rcx,0x48(%rax)
  405102:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405107:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  40510e:	00 
  40510f:	c3                   	ret

0000000000405110 <runtime::default_assertion_failure_proc>:
  405110:	48 83 ec 48          	sub    $0x48,%rsp
  405114:	4c 89 04 24          	mov    %r8,(%rsp)
  405118:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40511d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405122:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405127:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40512c:	4c 8b 04 24          	mov    (%rsp),%r8
  405130:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405135:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40513a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40513f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405144:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405149:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40514e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405153:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  405158:	e8 03 00 00 00       	call   405160 <runtime::default_assertion_contextless_failure_proc>
  40515d:	0f 1f 00             	nopl   (%rax)

0000000000405160 <runtime::default_assertion_contextless_failure_proc>:
  405160:	48 83 ec 48          	sub    $0x48,%rsp
  405164:	4c 89 04 24          	mov    %r8,(%rsp)
  405168:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40516d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405172:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405177:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40517c:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405181:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  405186:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40518b:	48 8b 3c 24          	mov    (%rsp),%rdi
  40518f:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405194:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405199:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40519e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4051a3:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4051a8:	e8 d3 ec ff ff       	call   403e80 <runtime::print_caller_location>
  4051ad:	bf 92 72 40 00       	mov    $0x407292,%edi
  4051b2:	be 01 00 00 00       	mov    $0x1,%esi
  4051b7:	e8 a4 e4 ff ff       	call   403660 <runtime::print_string>
  4051bc:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4051c1:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4051c6:	e8 95 e4 ff ff       	call   403660 <runtime::print_string>
  4051cb:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4051d0:	48 83 f8 00          	cmp    $0x0,%rax
  4051d4:	0f 9f c0             	setg   %al
  4051d7:	24 01                	and    $0x1,%al
  4051d9:	3c 00                	cmp    $0x0,%al
  4051db:	74 1e                	je     4051fb <runtime::default_assertion_contextless_failure_proc+0x9b>
  4051dd:	bf 94 72 40 00       	mov    $0x407294,%edi
  4051e2:	be 02 00 00 00       	mov    $0x2,%esi
  4051e7:	e8 74 e4 ff ff       	call   403660 <runtime::print_string>
  4051ec:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4051f1:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4051f6:	e8 65 e4 ff ff       	call   403660 <runtime::print_string>
  4051fb:	bf 0a 00 00 00       	mov    $0xa,%edi
  405200:	e8 8b e6 ff ff       	call   403890 <runtime::print_byte>
  405205:	0f 0b                	ud2
  405207:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40520e:	00 00 

0000000000405210 <__truncsfhf2>:
  405210:	48 83 ec 18          	sub    $0x18,%rsp
  405214:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  40521a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  405220:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  405226:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40522d:	00 
  40522e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  405235:	00 
  405236:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40523d:	00 
  40523e:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  405245:	00 
  405246:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  40524d:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  405253:	8b 44 24 10          	mov    0x10(%rsp),%eax
  405257:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  40525b:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40525f:	c1 f9 10             	sar    $0x10,%ecx
  405262:	b2 01                	mov    $0x1,%dl
  405264:	31 c0                	xor    %eax,%eax
  405266:	f6 c2 01             	test   $0x1,%dl
  405269:	0f 45 c1             	cmovne %ecx,%eax
  40526c:	25 00 80 00 00       	and    $0x8000,%eax
  405271:	89 44 24 08          	mov    %eax,0x8(%rsp)
  405275:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  405279:	c1 f9 17             	sar    $0x17,%ecx
  40527c:	b2 01                	mov    $0x1,%dl
  40527e:	31 c0                	xor    %eax,%eax
  405280:	f6 c2 01             	test   $0x1,%dl
  405283:	0f 45 c1             	cmovne %ecx,%eax
  405286:	25 ff 00 00 00       	and    $0xff,%eax
  40528b:	83 e8 70             	sub    $0x70,%eax
  40528e:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405292:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  405296:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  40529b:	89 04 24             	mov    %eax,(%rsp)
  40529e:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  4052a3:	0f 9e c0             	setle  %al
  4052a6:	24 01                	and    $0x1,%al
  4052a8:	3c 00                	cmp    $0x0,%al
  4052aa:	0f 84 82 00 00 00    	je     405332 <__truncsfhf2+0x122>
  4052b0:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  4052b5:	0f 9c c0             	setl   %al
  4052b8:	24 01                	and    $0x1,%al
  4052ba:	3c 00                	cmp    $0x0,%al
  4052bc:	74 16                	je     4052d4 <__truncsfhf2+0xc4>
  4052be:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  4052c3:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  4052c8:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  4052cf:	48 83 c4 18          	add    $0x18,%rsp
  4052d3:	c3                   	ret
  4052d4:	8b 04 24             	mov    (%rsp),%eax
  4052d7:	0d 00 00 80 00       	or     $0x800000,%eax
  4052dc:	ba 01 00 00 00       	mov    $0x1,%edx
  4052e1:	2b 54 24 04          	sub    0x4(%rsp),%edx
  4052e5:	89 d1                	mov    %edx,%ecx
  4052e7:	d3 f8                	sar    %cl,%eax
  4052e9:	89 c1                	mov    %eax,%ecx
  4052eb:	31 c0                	xor    %eax,%eax
  4052ed:	83 fa 20             	cmp    $0x20,%edx
  4052f0:	0f 42 c1             	cmovb  %ecx,%eax
  4052f3:	89 04 24             	mov    %eax,(%rsp)
  4052f6:	8b 04 24             	mov    (%rsp),%eax
  4052f9:	25 00 10 00 00       	and    $0x1000,%eax
  4052fe:	83 f8 00             	cmp    $0x0,%eax
  405301:	0f 95 c0             	setne  %al
  405304:	24 01                	and    $0x1,%al
  405306:	3c 00                	cmp    $0x0,%al
  405308:	74 0b                	je     405315 <__truncsfhf2+0x105>
  40530a:	8b 04 24             	mov    (%rsp),%eax
  40530d:	05 00 20 00 00       	add    $0x2000,%eax
  405312:	89 04 24             	mov    %eax,(%rsp)
  405315:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405319:	8b 0c 24             	mov    (%rsp),%ecx
  40531c:	c1 e9 0d             	shr    $0xd,%ecx
  40531f:	09 c8                	or     %ecx,%eax
  405321:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  405326:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40532d:	48 83 c4 18          	add    $0x18,%rsp
  405331:	c3                   	ret
  405332:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  405339:	00 
  40533a:	0f 94 c0             	sete   %al
  40533d:	24 01                	and    $0x1,%al
  40533f:	3c 00                	cmp    $0x0,%al
  405341:	74 59                	je     40539c <__truncsfhf2+0x18c>
  405343:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  405347:	0f 94 c0             	sete   %al
  40534a:	24 01                	and    $0x1,%al
  40534c:	3c 00                	cmp    $0x0,%al
  40534e:	74 1a                	je     40536a <__truncsfhf2+0x15a>
  405350:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405354:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405359:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  40535e:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  405365:	48 83 c4 18          	add    $0x18,%rsp
  405369:	c3                   	ret
  40536a:	8b 04 24             	mov    (%rsp),%eax
  40536d:	c1 f8 0d             	sar    $0xd,%eax
  405370:	89 04 24             	mov    %eax,(%rsp)
  405373:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405377:	8b 0c 24             	mov    (%rsp),%ecx
  40537a:	09 c8                	or     %ecx,%eax
  40537c:	85 c9                	test   %ecx,%ecx
  40537e:	0f 94 c1             	sete   %cl
  405381:	0f b6 c9             	movzbl %cl,%ecx
  405384:	09 c8                	or     %ecx,%eax
  405386:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40538b:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  405390:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  405397:	48 83 c4 18          	add    $0x18,%rsp
  40539b:	c3                   	ret
  40539c:	8b 04 24             	mov    (%rsp),%eax
  40539f:	25 00 10 00 00       	and    $0x1000,%eax
  4053a4:	83 f8 00             	cmp    $0x0,%eax
  4053a7:	0f 95 c0             	setne  %al
  4053aa:	24 01                	and    $0x1,%al
  4053ac:	3c 00                	cmp    $0x0,%al
  4053ae:	74 33                	je     4053e3 <__truncsfhf2+0x1d3>
  4053b0:	8b 04 24             	mov    (%rsp),%eax
  4053b3:	05 00 20 00 00       	add    $0x2000,%eax
  4053b8:	89 04 24             	mov    %eax,(%rsp)
  4053bb:	8b 04 24             	mov    (%rsp),%eax
  4053be:	25 00 00 80 00       	and    $0x800000,%eax
  4053c3:	83 f8 00             	cmp    $0x0,%eax
  4053c6:	0f 95 c0             	setne  %al
  4053c9:	24 01                	and    $0x1,%al
  4053cb:	3c 00                	cmp    $0x0,%al
  4053cd:	74 12                	je     4053e1 <__truncsfhf2+0x1d1>
  4053cf:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  4053d6:	8b 44 24 04          	mov    0x4(%rsp),%eax
  4053da:	83 c0 01             	add    $0x1,%eax
  4053dd:	89 44 24 04          	mov    %eax,0x4(%rsp)
  4053e1:	eb 00                	jmp    4053e3 <__truncsfhf2+0x1d3>
  4053e3:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  4053e8:	0f 9f c0             	setg   %al
  4053eb:	24 01                	and    $0x1,%al
  4053ed:	3c 00                	cmp    $0x0,%al
  4053ef:	74 75                	je     405466 <__truncsfhf2+0x256>
  4053f1:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  4053f8:	00 00 00 
  4053fb:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405400:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  405407:	00 00 
  405409:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  40540f:	0f 9c c0             	setl   %al
  405412:	24 01                	and    $0x1,%al
  405414:	3c 00                	cmp    $0x0,%al
  405416:	74 34                	je     40544c <__truncsfhf2+0x23c>
  405418:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40541d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405422:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405427:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  40542d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405432:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405437:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40543c:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405441:	48 83 c0 01          	add    $0x1,%rax
  405445:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40544a:	eb bd                	jmp    405409 <__truncsfhf2+0x1f9>
  40544c:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405450:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405455:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  40545a:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  405461:	48 83 c4 18          	add    $0x18,%rsp
  405465:	c3                   	ret
  405466:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40546a:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  40546e:	c1 e1 0a             	shl    $0xa,%ecx
  405471:	09 c8                	or     %ecx,%eax
  405473:	8b 0c 24             	mov    (%rsp),%ecx
  405476:	c1 e9 0d             	shr    $0xd,%ecx
  405479:	09 c8                	or     %ecx,%eax
  40547b:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  405480:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  405487:	48 83 c4 18          	add    $0x18,%rsp
  40548b:	c3                   	ret
  40548c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405490 <__truncdfhf2>:
  405490:	48 83 ec 18          	sub    $0x18,%rsp
  405494:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  40549a:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  4054a0:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  4054a6:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  4054aa:	e8 61 fd ff ff       	call   405210 <__truncsfhf2>
  4054af:	48 83 c4 18          	add    $0x18,%rsp
  4054b3:	c3                   	ret
  4054b4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4054bb:	00 00 00 00 00 

00000000004054c0 <__gnu_h2f_ieee>:
  4054c0:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  4054c6:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  4054cc:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  4054d3:	00 
  4054d4:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  4054db:	00 
  4054dc:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  4054e1:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  4054e6:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  4054ed:	00 
  4054ee:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  4054f5:	00 
  4054f6:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  4054fd:	00 
  4054fe:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  405505:	77 
  405506:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  40550d:	47 
  40550e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405513:	66 25 ff 7f          	and    $0x7fff,%ax
  405517:	0f b7 c8             	movzwl %ax,%ecx
  40551a:	c1 e1 0d             	shl    $0xd,%ecx
  40551d:	b2 01                	mov    $0x1,%dl
  40551f:	31 c0                	xor    %eax,%eax
  405521:	f6 c2 01             	test   $0x1,%dl
  405524:	0f 45 c1             	cmovne %ecx,%eax
  405527:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40552b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  405531:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  405537:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  40553d:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405543:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  405548:	0f 93 c0             	setae  %al
  40554b:	24 01                	and    $0x1,%al
  40554d:	3c 00                	cmp    $0x0,%al
  40554f:	74 0d                	je     40555e <__gnu_h2f_ieee+0x9e>
  405551:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405555:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  40555a:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40555e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405563:	66 25 00 80          	and    $0x8000,%ax
  405567:	0f b7 c8             	movzwl %ax,%ecx
  40556a:	c1 e1 10             	shl    $0x10,%ecx
  40556d:	b2 01                	mov    $0x1,%dl
  40556f:	31 c0                	xor    %eax,%eax
  405571:	f6 c2 01             	test   $0x1,%dl
  405574:	0f 45 c1             	cmovne %ecx,%eax
  405577:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  40557b:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40557f:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405585:	c3                   	ret
  405586:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40558d:	00 00 00 

0000000000405590 <__gnu_f2h_ieee>:
  405590:	50                   	push   %rax
  405591:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  405596:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  40559b:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  4055a1:	e8 6a fc ff ff       	call   405210 <__truncsfhf2>
  4055a6:	58                   	pop    %rax
  4055a7:	c3                   	ret
  4055a8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4055af:	00 

00000000004055b0 <__extendhfsf2>:
  4055b0:	50                   	push   %rax
  4055b1:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  4055b7:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  4055bd:	0f 28 c8             	movaps %xmm0,%xmm1
  4055c0:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  4055c7:	00 
  4055c8:	e8 f3 fe ff ff       	call   4054c0 <__gnu_h2f_ieee>
  4055cd:	58                   	pop    %rax
  4055ce:	c3                   	ret
  4055cf:	90                   	nop

00000000004055d0 <__floattidf>:
  4055d0:	53                   	push   %rbx
  4055d1:	48 83 ec 10          	sub    $0x10,%rsp
  4055d5:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  4055da:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4055df:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  4055e4:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4055e9:	48 89 04 24          	mov    %rax,(%rsp)
  4055ed:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4055f2:	48 09 c8             	or     %rcx,%rax
  4055f5:	0f 94 c0             	sete   %al
  4055f8:	24 01                	and    $0x1,%al
  4055fa:	3c 00                	cmp    $0x0,%al
  4055fc:	74 09                	je     405607 <__floattidf+0x37>
  4055fe:	0f 57 c0             	xorps  %xmm0,%xmm0
  405601:	48 83 c4 10          	add    $0x10,%rsp
  405605:	5b                   	pop    %rbx
  405606:	c3                   	ret
  405607:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40560c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405611:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405616:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40561b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405620:	48 c1 f8 3f          	sar    $0x3f,%rax
  405624:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405629:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40562e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405633:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405638:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40563d:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405642:	48 31 d0             	xor    %rdx,%rax
  405645:	48 31 f1             	xor    %rsi,%rcx
  405648:	48 29 f1             	sub    %rsi,%rcx
  40564b:	48 19 d0             	sbb    %rdx,%rax
  40564e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405653:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405658:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  40565d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  405662:	48 0f bd c2          	bsr    %rdx,%rax
  405666:	48 83 f0 3f          	xor    $0x3f,%rax
  40566a:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40566f:	48 0f bd ce          	bsr    %rsi,%rcx
  405673:	48 83 f1 3f          	xor    $0x3f,%rcx
  405677:	48 83 c1 40          	add    $0x40,%rcx
  40567b:	48 85 d2             	test   %rdx,%rdx
  40567e:	48 0f 45 c8          	cmovne %rax,%rcx
  405682:	31 c0                	xor    %eax,%eax
  405684:	ba 80 00 00 00       	mov    $0x80,%edx
  405689:	48 29 ca             	sub    %rcx,%rdx
  40568c:	48 89 c1             	mov    %rax,%rcx
  40568f:	48 19 c9             	sbb    %rcx,%rcx
  405692:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405697:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  40569c:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  4056a0:	ff c9                	dec    %ecx
  4056a2:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  4056a6:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4056ab:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4056b0:	ba 35 00 00 00       	mov    $0x35,%edx
  4056b5:	48 29 f2             	sub    %rsi,%rdx
  4056b8:	48 19 c8             	sbb    %rcx,%rax
  4056bb:	0f 9c c0             	setl   %al
  4056be:	24 01                	and    $0x1,%al
  4056c0:	3c 00                	cmp    $0x0,%al
  4056c2:	0f 84 c0 01 00 00    	je     405888 <__floattidf+0x2b8>
  4056c8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4056cd:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  4056d2:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4056d7:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  4056dc:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  4056e1:	0f 28 0d 38 1c 00 00 	movaps 0x1c38(%rip),%xmm1        # 407320 <_IO_stdin_used+0x320>
  4056e8:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4056ec:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  4056f1:	74 17                	je     40570a <__floattidf+0x13a>
  4056f3:	eb 00                	jmp    4056f5 <__floattidf+0x125>
  4056f5:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  4056fa:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  4056ff:	48 83 f0 37          	xor    $0x37,%rax
  405703:	48 09 c8             	or     %rcx,%rax
  405706:	74 26                	je     40572e <__floattidf+0x15e>
  405708:	eb 29                	jmp    405733 <__floattidf+0x163>
  40570a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40570f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405714:	48 89 d0             	mov    %rdx,%rax
  405717:	48 01 c0             	add    %rax,%rax
  40571a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40571f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405724:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405729:	e9 d6 00 00 00       	jmp    405804 <__floattidf+0x234>
  40572e:	e9 d1 00 00 00       	jmp    405804 <__floattidf+0x234>
  405733:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405738:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  40573d:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405742:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405747:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40574c:	49 89 fb             	mov    %rdi,%r11
  40574f:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405753:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405757:	44 88 db             	mov    %r11b,%bl
  40575a:	88 d9                	mov    %bl,%cl
  40575c:	49 89 f2             	mov    %rsi,%r10
  40575f:	49 d3 ea             	shr    %cl,%r10
  405762:	88 d9                	mov    %bl,%cl
  405764:	49 89 d1             	mov    %rdx,%r9
  405767:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40576b:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  405770:	45 31 c0             	xor    %r8d,%r8d
  405773:	f6 c3 40             	test   $0x40,%bl
  405776:	4d 0f 45 ca          	cmovne %r10,%r9
  40577a:	4d 0f 45 d0          	cmovne %r8,%r10
  40577e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405785:	48 83 d8 00          	sbb    $0x0,%rax
  405789:	4c 89 c0             	mov    %r8,%rax
  40578c:	49 0f 42 c2          	cmovb  %r10,%rax
  405790:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405795:	4c 89 c0             	mov    %r8,%rax
  405798:	49 0f 42 c1          	cmovb  %r9,%rax
  40579c:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  4057a2:	49 29 fb             	sub    %rdi,%r11
  4057a5:	4c 89 c7             	mov    %r8,%rdi
  4057a8:	48 19 cf             	sbb    %rcx,%rdi
  4057ab:	45 88 d9             	mov    %r11b,%r9b
  4057ae:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  4057b5:	44 88 c9             	mov    %r9b,%cl
  4057b8:	4c 89 d3             	mov    %r10,%rbx
  4057bb:	48 d3 eb             	shr    %cl,%rbx
  4057be:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  4057c3:	41 f6 c1 40          	test   $0x40,%r9b
  4057c7:	49 89 d9             	mov    %rbx,%r9
  4057ca:	4d 0f 45 c8          	cmovne %r8,%r9
  4057ce:	4c 0f 45 d3          	cmovne %rbx,%r10
  4057d2:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  4057d9:	48 83 df 00          	sbb    $0x0,%rdi
  4057dd:	4c 89 c7             	mov    %r8,%rdi
  4057e0:	49 0f 42 fa          	cmovb  %r10,%rdi
  4057e4:	4d 0f 42 c1          	cmovb  %r9,%r8
  4057e8:	4c 21 c6             	and    %r8,%rsi
  4057eb:	48 21 fa             	and    %rdi,%rdx
  4057ee:	48 09 f2             	or     %rsi,%rdx
  4057f1:	0f 95 c2             	setne  %dl
  4057f4:	0f b6 d2             	movzbl %dl,%edx
  4057f7:	48 09 d0             	or     %rdx,%rax
  4057fa:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4057ff:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405804:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405809:	89 c1                	mov    %eax,%ecx
  40580b:	83 e1 04             	and    $0x4,%ecx
  40580e:	c1 e9 02             	shr    $0x2,%ecx
  405811:	48 09 c8             	or     %rcx,%rax
  405814:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405819:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40581e:	48 83 c0 01          	add    $0x1,%rax
  405822:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  405828:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40582d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405832:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405837:	48 89 c8             	mov    %rcx,%rax
  40583a:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40583f:	48 c1 f9 02          	sar    $0x2,%rcx
  405843:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405848:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40584d:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  405851:	24 20                	and    $0x20,%al
  405853:	c0 e8 05             	shr    $0x5,%al
  405856:	24 01                	and    $0x1,%al
  405858:	3c 00                	cmp    $0x0,%al
  40585a:	74 2a                	je     405886 <__floattidf+0x2b6>
  40585c:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405861:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405866:	48 89 c8             	mov    %rcx,%rax
  405869:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40586e:	48 d1 f9             	sar    $1,%rcx
  405871:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405876:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40587b:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40587f:	83 c0 01             	add    $0x1,%eax
  405882:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405886:	eb 5c                	jmp    4058e4 <__floattidf+0x314>
  405888:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  40588c:	b9 35 00 00 00       	mov    $0x35,%ecx
  405891:	29 c1                	sub    %eax,%ecx
  405893:	83 e1 7f             	and    $0x7f,%ecx
  405896:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  40589a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40589f:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  4058a4:	40 88 cf             	mov    %cl,%dil
  4058a7:	40 88 f9             	mov    %dil,%cl
  4058aa:	48 89 c2             	mov    %rax,%rdx
  4058ad:	48 d3 e2             	shl    %cl,%rdx
  4058b0:	40 88 f9             	mov    %dil,%cl
  4058b3:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  4058b7:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  4058bb:	31 c0                	xor    %eax,%eax
  4058bd:	40 f6 c7 40          	test   $0x40,%dil
  4058c1:	48 0f 45 f2          	cmovne %rdx,%rsi
  4058c5:	48 0f 45 d0          	cmovne %rax,%rdx
  4058c9:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  4058cf:	48 89 c1             	mov    %rax,%rcx
  4058d2:	48 0f 42 ce          	cmovb  %rsi,%rcx
  4058d6:	48 0f 42 c2          	cmovb  %rdx,%rax
  4058da:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4058df:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4058e4:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  4058eb:	00 00 
  4058ed:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4058f1:	25 00 00 00 80       	and    $0x80000000,%eax
  4058f6:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  4058fa:	c1 e1 14             	shl    $0x14,%ecx
  4058fd:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  405903:	09 c8                	or     %ecx,%eax
  405905:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  405909:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  40590f:	09 c8                	or     %ecx,%eax
  405911:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405915:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405919:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40591d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405923:	48 83 c4 10          	add    $0x10,%rsp
  405927:	5b                   	pop    %rbx
  405928:	c3                   	ret
  405929:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000405930 <__floattidf_unsigned>:
  405930:	53                   	push   %rbx
  405931:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405936:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40593b:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405940:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405945:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40594a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40594f:	48 09 c8             	or     %rcx,%rax
  405952:	0f 94 c0             	sete   %al
  405955:	24 01                	and    $0x1,%al
  405957:	3c 00                	cmp    $0x0,%al
  405959:	74 05                	je     405960 <__floattidf_unsigned+0x30>
  40595b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40595e:	5b                   	pop    %rbx
  40595f:	c3                   	ret
  405960:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405965:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40596a:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40596f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405974:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  405979:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40597e:	48 0f bd c2          	bsr    %rdx,%rax
  405982:	48 83 f0 3f          	xor    $0x3f,%rax
  405986:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40598b:	48 0f bd ce          	bsr    %rsi,%rcx
  40598f:	48 83 f1 3f          	xor    $0x3f,%rcx
  405993:	48 83 c1 40          	add    $0x40,%rcx
  405997:	48 85 d2             	test   %rdx,%rdx
  40599a:	48 0f 45 c8          	cmovne %rax,%rcx
  40599e:	31 c0                	xor    %eax,%eax
  4059a0:	ba 80 00 00 00       	mov    $0x80,%edx
  4059a5:	48 29 ca             	sub    %rcx,%rdx
  4059a8:	48 89 c1             	mov    %rax,%rcx
  4059ab:	48 19 c9             	sbb    %rcx,%rcx
  4059ae:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4059b3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4059b8:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  4059bc:	ff c9                	dec    %ecx
  4059be:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  4059c2:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4059c7:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4059cc:	ba 35 00 00 00       	mov    $0x35,%edx
  4059d1:	48 29 f2             	sub    %rsi,%rdx
  4059d4:	48 19 c8             	sbb    %rcx,%rax
  4059d7:	0f 92 c0             	setb   %al
  4059da:	24 01                	and    $0x1,%al
  4059dc:	3c 00                	cmp    $0x0,%al
  4059de:	0f 84 c0 01 00 00    	je     405ba4 <__floattidf_unsigned+0x274>
  4059e4:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4059e9:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  4059ee:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4059f3:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  4059f8:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  4059fd:	0f 28 0d 1c 19 00 00 	movaps 0x191c(%rip),%xmm1        # 407320 <_IO_stdin_used+0x320>
  405a04:	66 0f ef c1          	pxor   %xmm1,%xmm0
  405a08:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  405a0d:	74 17                	je     405a26 <__floattidf_unsigned+0xf6>
  405a0f:	eb 00                	jmp    405a11 <__floattidf_unsigned+0xe1>
  405a11:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  405a16:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405a1b:	48 83 f0 37          	xor    $0x37,%rax
  405a1f:	48 09 c8             	or     %rcx,%rax
  405a22:	74 26                	je     405a4a <__floattidf_unsigned+0x11a>
  405a24:	eb 29                	jmp    405a4f <__floattidf_unsigned+0x11f>
  405a26:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405a2b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405a30:	48 89 d0             	mov    %rdx,%rax
  405a33:	48 01 c0             	add    %rax,%rax
  405a36:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  405a3b:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405a40:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405a45:	e9 d6 00 00 00       	jmp    405b20 <__floattidf_unsigned+0x1f0>
  405a4a:	e9 d1 00 00 00       	jmp    405b20 <__floattidf_unsigned+0x1f0>
  405a4f:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405a54:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  405a59:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405a5e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405a63:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405a68:	49 89 fb             	mov    %rdi,%r11
  405a6b:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405a6f:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405a73:	44 88 db             	mov    %r11b,%bl
  405a76:	88 d9                	mov    %bl,%cl
  405a78:	49 89 f2             	mov    %rsi,%r10
  405a7b:	49 d3 ea             	shr    %cl,%r10
  405a7e:	88 d9                	mov    %bl,%cl
  405a80:	49 89 d1             	mov    %rdx,%r9
  405a83:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  405a87:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  405a8c:	45 31 c0             	xor    %r8d,%r8d
  405a8f:	f6 c3 40             	test   $0x40,%bl
  405a92:	4d 0f 45 ca          	cmovne %r10,%r9
  405a96:	4d 0f 45 d0          	cmovne %r8,%r10
  405a9a:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405aa1:	48 83 d8 00          	sbb    $0x0,%rax
  405aa5:	4c 89 c0             	mov    %r8,%rax
  405aa8:	49 0f 42 c2          	cmovb  %r10,%rax
  405aac:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405ab1:	4c 89 c0             	mov    %r8,%rax
  405ab4:	49 0f 42 c1          	cmovb  %r9,%rax
  405ab8:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  405abe:	49 29 fb             	sub    %rdi,%r11
  405ac1:	4c 89 c7             	mov    %r8,%rdi
  405ac4:	48 19 cf             	sbb    %rcx,%rdi
  405ac7:	45 88 d9             	mov    %r11b,%r9b
  405aca:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  405ad1:	44 88 c9             	mov    %r9b,%cl
  405ad4:	4c 89 d3             	mov    %r10,%rbx
  405ad7:	48 d3 eb             	shr    %cl,%rbx
  405ada:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  405adf:	41 f6 c1 40          	test   $0x40,%r9b
  405ae3:	49 89 d9             	mov    %rbx,%r9
  405ae6:	4d 0f 45 c8          	cmovne %r8,%r9
  405aea:	4c 0f 45 d3          	cmovne %rbx,%r10
  405aee:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405af5:	48 83 df 00          	sbb    $0x0,%rdi
  405af9:	4c 89 c7             	mov    %r8,%rdi
  405afc:	49 0f 42 fa          	cmovb  %r10,%rdi
  405b00:	4d 0f 42 c1          	cmovb  %r9,%r8
  405b04:	4c 21 c6             	and    %r8,%rsi
  405b07:	48 21 fa             	and    %rdi,%rdx
  405b0a:	48 09 f2             	or     %rsi,%rdx
  405b0d:	0f 95 c2             	setne  %dl
  405b10:	0f b6 d2             	movzbl %dl,%edx
  405b13:	48 09 d0             	or     %rdx,%rax
  405b16:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405b1b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b20:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405b25:	89 c1                	mov    %eax,%ecx
  405b27:	83 e1 04             	and    $0x4,%ecx
  405b2a:	c1 e9 02             	shr    $0x2,%ecx
  405b2d:	48 09 c8             	or     %rcx,%rax
  405b30:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b35:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405b3a:	48 83 c0 01          	add    $0x1,%rax
  405b3e:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  405b44:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b49:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405b4e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405b53:	48 89 c8             	mov    %rcx,%rax
  405b56:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  405b5b:	48 c1 e9 02          	shr    $0x2,%rcx
  405b5f:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405b64:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b69:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  405b6d:	24 20                	and    $0x20,%al
  405b6f:	c0 e8 05             	shr    $0x5,%al
  405b72:	24 01                	and    $0x1,%al
  405b74:	3c 00                	cmp    $0x0,%al
  405b76:	74 2a                	je     405ba2 <__floattidf_unsigned+0x272>
  405b78:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405b7d:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405b82:	48 89 c8             	mov    %rcx,%rax
  405b85:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  405b8a:	48 d1 e9             	shr    $1,%rcx
  405b8d:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405b92:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405b97:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  405b9b:	83 c0 01             	add    $0x1,%eax
  405b9e:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405ba2:	eb 6a                	jmp    405c0e <__floattidf_unsigned+0x2de>
  405ba4:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  405ba9:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405bae:	31 c0                	xor    %eax,%eax
  405bb0:	bf 35 00 00 00       	mov    $0x35,%edi
  405bb5:	48 29 d7             	sub    %rdx,%rdi
  405bb8:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  405bbd:	48 19 c8             	sbb    %rcx,%rax
  405bc0:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  405bc5:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405bca:	41 88 f8             	mov    %dil,%r8b
  405bcd:	44 88 c1             	mov    %r8b,%cl
  405bd0:	4c 89 ce             	mov    %r9,%rsi
  405bd3:	48 d3 e6             	shl    %cl,%rsi
  405bd6:	44 88 c1             	mov    %r8b,%cl
  405bd9:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  405bdd:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  405be2:	41 f6 c0 40          	test   $0x40,%r8b
  405be6:	48 0f 45 d6          	cmovne %rsi,%rdx
  405bea:	48 0f 45 f1          	cmovne %rcx,%rsi
  405bee:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  405bf5:	48 83 d8 00          	sbb    $0x0,%rax
  405bf9:	48 89 c8             	mov    %rcx,%rax
  405bfc:	48 0f 42 c6          	cmovb  %rsi,%rax
  405c00:	48 0f 42 ca          	cmovb  %rdx,%rcx
  405c04:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405c09:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405c0e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  405c15:	00 00 
  405c17:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  405c1b:	c1 e2 14             	shl    $0x14,%edx
  405c1e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  405c22:	25 ff ff 0f 00       	and    $0xfffff,%eax
  405c27:	89 c1                	mov    %eax,%ecx
  405c29:	89 d0                	mov    %edx,%eax
  405c2b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  405c32:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405c36:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  405c3a:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  405c3e:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405c44:	5b                   	pop    %rbx
  405c45:	c3                   	ret
  405c46:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  405c4d:	00 00 00 

0000000000405c50 <__umodti3>:
  405c50:	48 83 ec 58          	sub    $0x58,%rsp
  405c54:	48 89 0c 24          	mov    %rcx,(%rsp)
  405c58:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405c5d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405c62:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405c67:	48 8b 0c 24          	mov    (%rsp),%rcx
  405c6b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  405c70:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405c75:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405c7a:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  405c7f:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  405c84:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  405c89:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405c8e:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  405c93:	e8 78 b6 ff ff       	call   401310 <runtime::udivmod128>
  405c98:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405c9d:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  405ca2:	48 83 c4 58          	add    $0x58,%rsp
  405ca6:	c3                   	ret
  405ca7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405cae:	00 00 

0000000000405cb0 <__udivmodti4>:
  405cb0:	48 83 ec 58          	sub    $0x58,%rsp
  405cb4:	4c 89 04 24          	mov    %r8,(%rsp)
  405cb8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405cbd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405cc2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405cc7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  405ccc:	4c 8b 04 24          	mov    (%rsp),%r8
  405cd0:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405cd5:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405cda:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405cdf:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405ce4:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  405ce9:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  405cee:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  405cf3:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405cf8:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  405cfd:	e8 0e b6 ff ff       	call   401310 <runtime::udivmod128>
  405d02:	48 83 c4 58          	add    $0x58,%rsp
  405d06:	c3                   	ret
  405d07:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405d0e:	00 00 

0000000000405d10 <__udivti3>:
  405d10:	48 83 ec 48          	sub    $0x48,%rsp
  405d14:	48 89 0c 24          	mov    %rcx,(%rsp)
  405d18:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405d1d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405d22:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405d27:	48 8b 0c 24          	mov    (%rsp),%rcx
  405d2b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  405d30:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405d35:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405d3a:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  405d3f:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405d44:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  405d49:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405d4e:	31 c0                	xor    %eax,%eax
  405d50:	41 89 c0             	mov    %eax,%r8d
  405d53:	e8 58 ff ff ff       	call   405cb0 <__udivmodti4>
  405d58:	48 83 c4 48          	add    $0x48,%rsp
  405d5c:	c3                   	ret
  405d5d:	0f 1f 00             	nopl   (%rax)

0000000000405d60 <runtime::assert>:
  405d60:	48 83 ec 48          	sub    $0x48,%rsp
  405d64:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  405d69:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  405d6e:	40 88 f8             	mov    %dil,%al
  405d71:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  405d75:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405d7a:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  405d7f:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  405d83:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  405d88:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  405d8d:	88 44 24 47          	mov    %al,0x47(%rsp)
  405d91:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  405d96:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405d9b:	3c 00                	cmp    $0x0,%al
  405d9d:	75 19                	jne    405db8 <runtime::assert+0x58>
  405d9f:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405da4:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  405da9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405dae:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  405db3:	e8 18 0a 00 00       	call   4067d0 <runtime::assert.internal-0>
  405db8:	48 83 c4 48          	add    $0x48,%rsp
  405dbc:	c3                   	ret
  405dbd:	0f 1f 00             	nopl   (%rax)

0000000000405dc0 <runtime::heap_allocator_proc.aligned_alloc-0>:
  405dc0:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  405dc7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  405dcc:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  405dd1:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405dd6:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  405ddb:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  405de0:	44 88 c0             	mov    %r8b,%al
  405de3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  405de7:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  405dee:	00 
  405def:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  405df4:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405df9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405dfe:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  405e03:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  405e07:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405e0c:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  405e13:	00 
  405e14:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  405e1b:	00 
  405e1c:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  405e23:	00 
  405e24:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  405e2b:	00 
  405e2c:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  405e33:	b9 08 00 00 00       	mov    $0x8,%ecx
  405e38:	48 83 fe 08          	cmp    $0x8,%rsi
  405e3c:	48 0f 4f ce          	cmovg  %rsi,%rcx
  405e40:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  405e47:	00 
  405e48:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405e4f:	00 
  405e50:	48 83 e9 01          	sub    $0x1,%rcx
  405e54:	48 83 c1 08          	add    $0x8,%rcx
  405e58:	48 01 d1             	add    %rdx,%rcx
  405e5b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  405e62:	00 
  405e63:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  405e6a:	00 00 
  405e6c:	48 83 f8 00          	cmp    $0x0,%rax
  405e70:	0f 95 c1             	setne  %cl
  405e73:	80 e1 01             	and    $0x1,%cl
  405e76:	31 c0                	xor    %eax,%eax
  405e78:	80 f9 00             	cmp    $0x0,%cl
  405e7b:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405e7f:	74 17                	je     405e98 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  405e81:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  405e86:	48 83 f8 08          	cmp    $0x8,%rax
  405e8a:	0f 9f c0             	setg   %al
  405e8d:	24 01                	and    $0x1,%al
  405e8f:	3c 00                	cmp    $0x0,%al
  405e91:	0f 95 c0             	setne  %al
  405e94:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405e98:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405e9d:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  405ea1:	80 e1 01             	and    $0x1,%cl
  405ea4:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  405ea8:	48 83 f8 00          	cmp    $0x0,%rax
  405eac:	0f 95 c0             	setne  %al
  405eaf:	24 01                	and    $0x1,%al
  405eb1:	3c 00                	cmp    $0x0,%al
  405eb3:	74 2e                	je     405ee3 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  405eb5:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  405eba:	75 27                	jne    405ee3 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  405ebc:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405ec1:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  405ec5:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  405eca:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  405ecf:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  405ed6:	00 
  405ed7:	e8 64 db ff ff       	call   403a40 <runtime::heap_resize>
  405edc:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  405ee1:	eb 19                	jmp    405efc <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  405ee3:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  405ee7:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  405eee:	00 
  405eef:	0f b6 f0             	movzbl %al,%esi
  405ef2:	e8 19 db ff ff       	call   403a10 <runtime::heap_alloc>
  405ef7:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  405efc:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  405f01:	48 83 c0 08          	add    $0x8,%rax
  405f05:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  405f0a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  405f0f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405f14:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  405f19:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  405f1e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  405f23:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  405f2a:	00 
  405f2b:	48 83 e8 01          	sub    $0x1,%rax
  405f2f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405f36:	00 
  405f37:	48 83 e9 01          	sub    $0x1,%rcx
  405f3b:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  405f3f:	48 21 c8             	and    %rcx,%rax
  405f42:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  405f47:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  405f4d:	0f 94 c0             	sete   %al
  405f50:	24 01                	and    $0x1,%al
  405f52:	3c 00                	cmp    $0x0,%al
  405f54:	74 3c                	je     405f92 <runtime::heap_allocator_proc.aligned_alloc-0+0x1d2>
  405f56:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405f5b:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405f60:	e8 ab 00 00 00       	call   406010 <runtime::heap_allocator_proc.aligned_free-1>
  405f65:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405f6a:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  405f6f:	e8 9c 00 00 00       	call   406010 <runtime::heap_allocator_proc.aligned_free-1>
  405f74:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405f79:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405f80:	00 
  405f81:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  405f88:	b0 01                	mov    $0x1,%al
  405f8a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  405f91:	c3                   	ret
  405f92:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405f97:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405f9c:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  405fa1:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  405fa6:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  405faa:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  405faf:	74 2f                	je     405fe0 <runtime::heap_allocator_proc.aligned_alloc-0+0x220>
  405fb1:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405fb6:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  405fbb:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  405fc0:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  405fc5:	48 39 d0             	cmp    %rdx,%rax
  405fc8:	48 0f 4c d0          	cmovl  %rax,%rdx
  405fcc:	e8 3f d0 ff ff       	call   403010 <runtime::mem_copy_non_overlapping>
  405fd1:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405fd6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405fdb:	e8 30 00 00 00       	call   406010 <runtime::heap_allocator_proc.aligned_free-1>
  405fe0:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  405fe5:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  405fea:	e8 c1 bf ff ff       	call   401fb0 <runtime::[internal.odin]::byte_slice>
  405fef:	48 89 c1             	mov    %rax,%rcx
  405ff2:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405ff7:	48 89 50 08          	mov    %rdx,0x8(%rax)
  405ffb:	48 89 08             	mov    %rcx,(%rax)
  405ffe:	31 c0                	xor    %eax,%eax
  406000:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  406007:	c3                   	ret
  406008:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40600f:	00 

0000000000406010 <runtime::heap_allocator_proc.aligned_free-1>:
  406010:	48 83 ec 18          	sub    $0x18,%rsp
  406014:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406019:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40601e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406023:	48 83 f8 00          	cmp    $0x0,%rax
  406027:	0f 95 c0             	setne  %al
  40602a:	24 01                	and    $0x1,%al
  40602c:	3c 00                	cmp    $0x0,%al
  40602e:	74 0e                	je     40603e <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  406030:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406035:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  406039:	e8 32 da ff ff       	call   403a70 <runtime::heap_free>
  40603e:	48 83 c4 18          	add    $0x18,%rsp
  406042:	c3                   	ret
  406043:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40604a:	84 00 00 00 00 00 

0000000000406050 <runtime::heap_allocator_proc.aligned_resize-2>:
  406050:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  406057:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40605c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406061:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  406066:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40606b:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  406070:	44 88 c0             	mov    %r8b,%al
  406073:	88 44 24 57          	mov    %al,0x57(%rsp)
  406077:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40607e:	00 
  40607f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  406084:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  406089:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  40608d:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  406092:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  406097:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40609c:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4060a3:	00 
  4060a4:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  4060ab:	00 
  4060ac:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  4060b3:	00 
  4060b4:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  4060bb:	00 
  4060bc:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  4060c3:	0f 57 c0             	xorps  %xmm0,%xmm0
  4060c6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4060cd:	00 
  4060ce:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  4060d5:	00 
  4060d6:	48 83 f8 00          	cmp    $0x0,%rax
  4060da:	0f 94 c0             	sete   %al
  4060dd:	24 01                	and    $0x1,%al
  4060df:	3c 00                	cmp    $0x0,%al
  4060e1:	0f 84 80 00 00 00    	je     406167 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  4060e7:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4060ec:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4060f1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4060f6:	8a 44 24 57          	mov    0x57(%rsp),%al
  4060fa:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  4060ff:	0f 57 c0             	xorps  %xmm0,%xmm0
  406102:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  406109:	00 
  40610a:	48 89 e2             	mov    %rsp,%rdx
  40610d:	4c 89 02             	mov    %r8,(%rdx)
  406110:	44 0f b6 c0          	movzbl %al,%r8d
  406114:	31 c0                	xor    %eax,%eax
  406116:	89 c2                	mov    %eax,%edx
  406118:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  40611f:	00 
  406120:	e8 9b fc ff ff       	call   405dc0 <runtime::heap_allocator_proc.aligned_alloc-0>
  406125:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40612a:	40 88 c7             	mov    %al,%dil
  40612d:	40 88 f8             	mov    %dil,%al
  406130:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  406137:	00 
  406138:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  40613f:	00 
  406140:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406147:	00 
  406148:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40614f:	00 
  406150:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  406157:	00 
  406158:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40615c:	48 89 11             	mov    %rdx,(%rcx)
  40615f:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406166:	c3                   	ret
  406167:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40616c:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  406171:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406176:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40617b:	8a 44 24 57          	mov    0x57(%rsp),%al
  40617f:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  406184:	0f 57 c0             	xorps  %xmm0,%xmm0
  406187:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40618e:	00 
  40618f:	49 89 e0             	mov    %rsp,%r8
  406192:	4d 89 08             	mov    %r9,(%r8)
  406195:	44 0f b6 c0          	movzbl %al,%r8d
  406199:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  4061a0:	00 
  4061a1:	e8 1a fc ff ff       	call   405dc0 <runtime::heap_allocator_proc.aligned_alloc-0>
  4061a6:	88 44 24 17          	mov    %al,0x17(%rsp)
  4061aa:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  4061b1:	00 
  4061b2:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4061b7:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4061be:	00 
  4061bf:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4061c4:	3c 00                	cmp    $0x0,%al
  4061c6:	74 4d                	je     406215 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  4061c8:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4061cd:	8a 44 24 17          	mov    0x17(%rsp),%al
  4061d1:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4061d8:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4061df:	00 
  4061e0:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  4061e7:	00 
  4061e8:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  4061ef:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  4061f6:	00 
  4061f7:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4061fe:	00 
  4061ff:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406206:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40620a:	48 89 11             	mov    %rdx,(%rcx)
  40620d:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406214:	c3                   	ret
  406215:	8a 44 24 57          	mov    0x57(%rsp),%al
  406219:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40621e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406223:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40622a:	00 
  40622b:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  406232:	00 
  406233:	3c 00                	cmp    $0x0,%al
  406235:	0f 84 87 00 00 00    	je     4062c2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  40623b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406240:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  406245:	48 39 c8             	cmp    %rcx,%rax
  406248:	0f 9f c0             	setg   %al
  40624b:	24 01                	and    $0x1,%al
  40624d:	3c 00                	cmp    $0x0,%al
  40624f:	74 71                	je     4062c2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  406251:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  406256:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  40625d:	00 
  40625e:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406263:	48 89 e0             	mov    %rsp,%rax
  406266:	4c 89 08             	mov    %r9,(%rax)
  406269:	bf 97 72 40 00       	mov    $0x407297,%edi
  40626e:	be 30 00 00 00       	mov    $0x30,%esi
  406273:	ba 4b 00 00 00       	mov    $0x4b,%edx
  406278:	b9 25 00 00 00       	mov    $0x25,%ecx
  40627d:	e8 1e ce ff ff       	call   4030a0 <runtime::slice_expr_error_lo_hi>
  406282:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406287:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40628c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  406291:	48 89 c6             	mov    %rax,%rsi
  406294:	48 03 b4 24 e0 00 00 	add    0xe0(%rsp),%rsi
  40629b:	00 
  40629c:	48 29 c1             	sub    %rax,%rcx
  40629f:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4062a4:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4062a9:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4062ae:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  4062b3:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4062b8:	48 29 c2             	sub    %rax,%rdx
  4062bb:	31 f6                	xor    %esi,%esi
  4062bd:	e8 7e ad ff ff       	call   401040 <memset@plt>
  4062c2:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4062c7:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4062ce:	00 
  4062cf:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  4062d6:	00 
  4062d7:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  4062de:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  4062e5:	00 
  4062e6:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4062ed:	00 
  4062ee:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  4062f5:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4062f9:	48 89 11             	mov    %rdx,(%rcx)
  4062fc:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406303:	c3                   	ret
  406304:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40630b:	00 00 00 00 00 

0000000000406310 <runtime::bounds_check_error.handle_error-0>:
  406310:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  406317:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40631c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406321:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406325:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406329:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40632e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406333:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406338:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40633d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406341:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406345:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40634a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40634f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  406354:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40635b:	00 
  40635c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  406360:	89 44 24 70          	mov    %eax,0x70(%rsp)
  406364:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  406369:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40636e:	0f 57 c0             	xorps  %xmm0,%xmm0
  406371:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406376:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40637b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  406382:	00 00 
  406384:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406389:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40638e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  406395:	00 00 
  406397:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40639c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4063a1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4063a5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4063a9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4063ae:	e8 cd da ff ff       	call   403e80 <runtime::print_caller_location>
  4063b3:	bf c8 72 40 00       	mov    $0x4072c8,%edi
  4063b8:	be 07 00 00 00       	mov    $0x7,%esi
  4063bd:	e8 9e d2 ff ff       	call   403660 <runtime::print_string>
  4063c2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4063c7:	e8 b4 d8 ff ff       	call   403c80 <runtime::print_i64>
  4063cc:	bf 13 72 40 00       	mov    $0x407213,%edi
  4063d1:	be 15 00 00 00       	mov    $0x15,%esi
  4063d6:	e8 85 d2 ff ff       	call   403660 <runtime::print_string>
  4063db:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4063e0:	e8 9b d8 ff ff       	call   403c80 <runtime::print_i64>
  4063e5:	bf 0a 00 00 00       	mov    $0xa,%edi
  4063ea:	e8 a1 d4 ff ff       	call   403890 <runtime::print_byte>
  4063ef:	e8 ec ae ff ff       	call   4012e0 <runtime::bounds_trap>
  4063f4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4063fb:	00 00 00 00 00 

0000000000406400 <runtime::default_random_generator_proc.read_u64-0>:
  406400:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  406405:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40640a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40640f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406414:	48 8b 00             	mov    (%rax),%rax
  406417:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40641c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406421:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  406428:	f4 51 58 
  40642b:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  406431:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406436:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  40643a:	48 83 ca 01          	or     $0x1,%rdx
  40643e:	48 01 d1             	add    %rdx,%rcx
  406441:	48 89 08             	mov    %rcx,(%rax)
  406444:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406449:	48 c1 e9 3b          	shr    $0x3b,%rcx
  40644d:	b2 01                	mov    $0x1,%dl
  40644f:	31 c0                	xor    %eax,%eax
  406451:	f6 c2 01             	test   $0x1,%dl
  406454:	48 0f 45 c1          	cmovne %rcx,%rax
  406458:	48 83 c0 05          	add    $0x5,%rax
  40645c:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  406461:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  406468:	75 f1 ae 
  40646b:	48 0f af c1          	imul   %rcx,%rax
  40646f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406474:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406479:	48 c1 e9 3b          	shr    $0x3b,%rcx
  40647d:	b2 01                	mov    $0x1,%dl
  40647f:	31 c0                	xor    %eax,%eax
  406481:	f6 c2 01             	test   $0x1,%dl
  406484:	48 0f 45 c1          	cmovne %rcx,%rax
  406488:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40648d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406492:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  406497:	48 89 d1             	mov    %rdx,%rcx
  40649a:	48 d3 e8             	shr    %cl,%rax
  40649d:	48 89 c1             	mov    %rax,%rcx
  4064a0:	31 c0                	xor    %eax,%eax
  4064a2:	48 83 fa 40          	cmp    $0x40,%rdx
  4064a6:	48 0f 42 c1          	cmovb  %rcx,%rax
  4064aa:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4064af:	31 c9                	xor    %ecx,%ecx
  4064b1:	89 ce                	mov    %ecx,%esi
  4064b3:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  4064b8:	48 83 e6 3f          	and    $0x3f,%rsi
  4064bc:	48 89 f1             	mov    %rsi,%rcx
  4064bf:	48 d3 e2             	shl    %cl,%rdx
  4064c2:	31 c9                	xor    %ecx,%ecx
  4064c4:	48 83 fe 40          	cmp    $0x40,%rsi
  4064c8:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4064cc:	48 09 c8             	or     %rcx,%rax
  4064cf:	c3                   	ret

00000000004064d0 <runtime::default_random_generator_proc.init-1>:
  4064d0:	48 83 ec 28          	sub    $0x28,%rsp
  4064d4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4064d8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4064dd:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4064e2:	48 8b 0c 24          	mov    (%rsp),%rcx
  4064e6:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4064eb:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4064f0:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4064f5:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  4064fb:	0f 94 c0             	sete   %al
  4064fe:	24 01                	and    $0x1,%al
  406500:	3c 00                	cmp    $0x0,%al
  406502:	74 0e                	je     406512 <runtime::default_random_generator_proc.init-1+0x42>
  406504:	0f 31                	rdtsc
  406506:	48 c1 e2 20          	shl    $0x20,%rdx
  40650a:	48 09 d0             	or     %rdx,%rax
  40650d:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406512:	48 8b 3c 24          	mov    (%rsp),%rdi
  406516:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40651b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  406522:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406527:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40652c:	48 d1 e2             	shl    $1,%rdx
  40652f:	40 b6 01             	mov    $0x1,%sil
  406532:	31 c9                	xor    %ecx,%ecx
  406534:	40 f6 c6 01          	test   $0x1,%sil
  406538:	48 0f 45 ca          	cmovne %rdx,%rcx
  40653c:	48 83 c9 01          	or     $0x1,%rcx
  406540:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406544:	e8 b7 fe ff ff       	call   406400 <runtime::default_random_generator_proc.read_u64-0>
  406549:	48 8b 3c 24          	mov    (%rsp),%rdi
  40654d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406552:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406557:	48 03 08             	add    (%rax),%rcx
  40655a:	48 89 08             	mov    %rcx,(%rax)
  40655d:	e8 9e fe ff ff       	call   406400 <runtime::default_random_generator_proc.read_u64-0>
  406562:	48 83 c4 28          	add    $0x28,%rsp
  406566:	c3                   	ret
  406567:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40656e:	00 00 

0000000000406570 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  406570:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  406575:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  40657a:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40657f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  406584:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  406589:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40658e:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  406595:	00 00 
  406597:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40659c:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  4065a0:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4065a5:	48 03 4a 20          	add    0x20(%rdx),%rcx
  4065a9:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  4065ae:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4065b3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4065b8:	48 83 e8 01          	sub    $0x1,%rax
  4065bc:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4065c1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4065c6:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  4065cb:	48 83 f8 00          	cmp    $0x0,%rax
  4065cf:	0f 95 c0             	setne  %al
  4065d2:	24 01                	and    $0x1,%al
  4065d4:	3c 00                	cmp    $0x0,%al
  4065d6:	74 17                	je     4065ef <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  4065d8:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4065dd:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4065e2:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  4065e7:	48 29 c8             	sub    %rcx,%rax
  4065ea:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4065ef:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4065f4:	c3                   	ret
  4065f5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4065fc:	00 00 00 00 

0000000000406600 <runtime::arena_alloc.align_forward_uint-0>:
  406600:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  406605:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40660a:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40660f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406614:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406619:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40661e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406623:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406628:	48 83 e9 01          	sub    $0x1,%rcx
  40662c:	48 21 c8             	and    %rcx,%rax
  40662f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406634:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  40663a:	0f 95 c0             	setne  %al
  40663d:	24 01                	and    $0x1,%al
  40663f:	3c 00                	cmp    $0x0,%al
  406641:	74 14                	je     406657 <runtime::arena_alloc.align_forward_uint-0+0x57>
  406643:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406648:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  40664d:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  406652:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406657:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40665c:	c3                   	ret
  40665d:	0f 1f 00             	nopl   (%rax)

0000000000406660 <runtime::matrix_bounds_check_error.handle_error-0>:
  406660:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  406667:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40666c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406671:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406675:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406679:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40667e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406683:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  40668a:	00 
  40668b:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406690:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  406697:	00 
  406698:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40669d:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4066a2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  4066a7:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4066ac:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  4066b1:	8b 44 24 18          	mov    0x18(%rsp),%eax
  4066b5:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4066b9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4066be:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4066c3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  4066ca:	00 
  4066cb:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  4066d2:	00 
  4066d3:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  4066da:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  4066e1:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  4066e8:	00 
  4066e9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  4066f0:	00 
  4066f1:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  4066f6:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  4066fb:	0f 57 c0             	xorps  %xmm0,%xmm0
  4066fe:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406703:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406708:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40670f:	00 00 
  406711:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406716:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40671b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406722:	00 00 
  406724:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  406729:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40672e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  406732:	89 44 24 54          	mov    %eax,0x54(%rsp)
  406736:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  40673b:	e8 40 d7 ff ff       	call   403e80 <runtime::print_caller_location>
  406740:	bf d0 72 40 00       	mov    $0x4072d0,%edi
  406745:	be 11 00 00 00       	mov    $0x11,%esi
  40674a:	e8 11 cf ff ff       	call   403660 <runtime::print_string>
  40674f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406754:	e8 27 d5 ff ff       	call   403c80 <runtime::print_i64>
  406759:	bf e2 72 40 00       	mov    $0x4072e2,%edi
  40675e:	be 02 00 00 00       	mov    $0x2,%esi
  406763:	e8 f8 ce ff ff       	call   403660 <runtime::print_string>
  406768:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40676d:	e8 0e d5 ff ff       	call   403c80 <runtime::print_i64>
  406772:	bf e5 72 40 00       	mov    $0x4072e5,%edi
  406777:	be 16 00 00 00       	mov    $0x16,%esi
  40677c:	e8 df ce ff ff       	call   403660 <runtime::print_string>
  406781:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406786:	e8 f5 d4 ff ff       	call   403c80 <runtime::print_i64>
  40678b:	bf fc 72 40 00       	mov    $0x4072fc,%edi
  406790:	be 06 00 00 00       	mov    $0x6,%esi
  406795:	e8 c6 ce ff ff       	call   403660 <runtime::print_string>
  40679a:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40679f:	e8 dc d4 ff ff       	call   403c80 <runtime::print_i64>
  4067a4:	bf 03 73 40 00       	mov    $0x407303,%edi
  4067a9:	be 01 00 00 00       	mov    $0x1,%esi
  4067ae:	e8 ad ce ff ff       	call   403660 <runtime::print_string>
  4067b3:	bf 0a 00 00 00       	mov    $0xa,%edi
  4067b8:	e8 d3 d0 ff ff       	call   403890 <runtime::print_byte>
  4067bd:	e8 1e ab ff ff       	call   4012e0 <runtime::bounds_trap>
  4067c2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4067c9:	1f 84 00 00 00 00 00 

00000000004067d0 <runtime::assert.internal-0>:
  4067d0:	48 83 ec 38          	sub    $0x38,%rsp
  4067d4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4067d8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4067dd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4067e2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4067e7:	48 8b 04 24          	mov    (%rsp),%rax
  4067eb:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4067f0:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4067f5:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4067fa:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4067ff:	48 8b 40 20          	mov    0x20(%rax),%rax
  406803:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406808:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  40680e:	0f 94 c0             	sete   %al
  406811:	24 01                	and    $0x1,%al
  406813:	3c 00                	cmp    $0x0,%al
  406815:	74 0c                	je     406823 <runtime::assert.internal-0+0x53>
  406817:	48 c7 c0 10 51 40 00 	mov    $0x405110,%rax
  40681e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406823:	4c 8b 0c 24          	mov    (%rsp),%r9
  406827:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40682c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406831:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406836:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40683b:	bf 05 73 40 00       	mov    $0x407305,%edi
  406840:	be 11 00 00 00       	mov    $0x11,%esi
  406845:	ff d0                	call   *%rax
  406847:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40684e:	00 00 

0000000000406850 <__$startup_runtime>:
  406850:	eb 00                	jmp    406852 <__$startup_runtime+0x2>
  406852:	c3                   	ret
  406853:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40685a:	84 00 00 00 00 00 

0000000000406860 <__$cleanup_runtime>:
  406860:	50                   	push   %rax
  406861:	48 89 3c 24          	mov    %rdi,(%rsp)
  406865:	eb 00                	jmp    406867 <__$cleanup_runtime+0x7>
  406867:	48 8b 3c 24          	mov    (%rsp),%rdi
  40686b:	e8 40 aa ff ff       	call   4012b0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  406870:	58                   	pop    %rax
  406871:	c3                   	ret

Disassembly of section .fini:

0000000000406874 <_fini>:
  406874:	f3 0f 1e fa          	endbr64
  406878:	48 83 ec 08          	sub    $0x8,%rsp
  40687c:	48 83 c4 08          	add    $0x8,%rsp
  406880:	c3                   	ret
