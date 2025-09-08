
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
  4010b8:	48 c7 c7 20 31 40 00 	mov    $0x403120,%rdi
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

0000000000401190 <journey::main>:
  401190:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  401197:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40119c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4011a1:	e8 b4 01 00 00       	call   40135a <journey::create_world:proc(indice_bit_capacity:$$900,unique_data_capacity:$$30)->(:journey::World)>
  4011a6:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4011ab:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  4011b2:	00 
  4011b3:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4011ba:	00 
  4011bb:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  4011c2:	00 
  4011c3:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4011ca:	00 
  4011cb:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  4011d2:	00 
  4011d3:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4011da:	00 
  4011db:	e8 19 02 00 00       	call   4013f9 <journey::create_world:proc(indice_bit_capacity:$$100,unique_data_capacity:$$57)->(:journey::World)>
  4011e0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4011e5:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  4011ec:	00 
  4011ed:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4011f4:	00 
  4011f5:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4011fc:	00 
  4011fd:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  401204:	00 
  401205:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  40120c:	00 
  40120d:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401214:	00 
  401215:	e8 7e 02 00 00       	call   401498 <journey::create_world:proc(indice_bit_capacity:$$575,unique_data_capacity:$$123)->(:journey::World)>
  40121a:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40121f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  401224:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  401229:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40122e:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  401233:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  40123a:	00 
  40123b:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401242:	00 
  401243:	e8 ef 02 00 00       	call   401537 <journey::create_world:proc(indice_bit_capacity:$$236,unique_data_capacity:$$353)->(:journey::World)>
  401248:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40124d:	48 89 54 24 68       	mov    %rdx,0x68(%rsp)
  401252:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  401257:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40125c:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  401261:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  401268:	00 
  401269:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401270:	00 
  401271:	e8 5b 03 00 00       	call   4015d1 <journey::create_world:proc(indice_bit_capacity:$$345,unique_data_capacity:$$512)->(:journey::World)>
  401276:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40127b:	48 89 54 24 58       	mov    %rdx,0x58(%rsp)
  401280:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401285:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40128a:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  40128f:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  401296:	00 
  401297:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  40129e:	00 
  40129f:	e8 c7 03 00 00       	call   40166b <journey::create_world:proc(indice_bit_capacity:$$1,unique_data_capacity:$$2)->(:journey::World)>
  4012a4:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4012a9:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4012ae:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4012b3:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4012b8:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4012bd:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  4012c4:	00 
  4012c5:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4012cc:	00 
  4012cd:	e8 38 04 00 00       	call   40170a <journey::create_world:proc(indice_bit_capacity:$$53,unique_data_capacity:$$24)->(:journey::World)>
  4012d2:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4012d7:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4012dc:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4012e1:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4012e6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4012eb:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  4012f2:	00 
  4012f3:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4012fa:	00 
  4012fb:	e8 a9 04 00 00       	call   4017a9 <journey::create_world:proc(indice_bit_capacity:$$999,unique_data_capacity:$$1)->(:journey::World)>
  401300:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401305:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40130a:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40130f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401314:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  401319:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  401320:	00 
  401321:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401328:	00 
  401329:	e8 1a 05 00 00       	call   401848 <journey::create_world:proc(indice_bit_capacity:$$1,unique_data_capacity:$$999)->(:journey::World)>
  40132e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  401333:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  401338:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40133d:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  401342:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  401349:	00 
  40134a:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  401351:	00 
  401352:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401359:	c3                   	ret

000000000040135a <journey::create_world:proc(indice_bit_capacity:$$900,unique_data_capacity:$$30)->(:journey::World)>:
  40135a:	31 c0                	xor    %eax,%eax
  40135c:	41 89 c1             	mov    %eax,%r9d
  40135f:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  401364:	b8 09 00 00 00       	mov    $0x9,%eax
  401369:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40136e:	be 00 10 00 00       	mov    $0x1000,%esi
  401373:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  401378:	ba 03 00 00 00       	mov    $0x3,%edx
  40137d:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  401382:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401388:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  40138d:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401394:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  401399:	4c 89 cf             	mov    %r9,%rdi
  40139c:	0f 05                	syscall
  40139e:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  4013a3:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  4013a8:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  4013ad:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4013b2:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4013b7:	48 89 c1             	mov    %rax,%rcx
  4013ba:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4013bf:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4013c4:	4c 89 cf             	mov    %r9,%rdi
  4013c7:	0f 05                	syscall
  4013c9:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4013ce:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4013d3:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  4013da:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4013df:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4013e4:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4013e9:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4013ee:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4013f3:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4013f8:	c3                   	ret

00000000004013f9 <journey::create_world:proc(indice_bit_capacity:$$100,unique_data_capacity:$$57)->(:journey::World)>:
  4013f9:	31 c0                	xor    %eax,%eax
  4013fb:	41 89 c1             	mov    %eax,%r9d
  4013fe:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  401403:	b8 09 00 00 00       	mov    $0x9,%eax
  401408:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40140d:	be 00 10 00 00       	mov    $0x1000,%esi
  401412:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  401417:	ba 03 00 00 00       	mov    $0x3,%edx
  40141c:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  401421:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401427:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  40142c:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401433:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  401438:	4c 89 cf             	mov    %r9,%rdi
  40143b:	0f 05                	syscall
  40143d:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  401442:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  401447:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  40144c:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401451:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  401456:	48 89 c1             	mov    %rax,%rcx
  401459:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40145e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401463:	4c 89 cf             	mov    %r9,%rdi
  401466:	0f 05                	syscall
  401468:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40146d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401472:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  401479:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40147e:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401483:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  401488:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40148d:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401492:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  401497:	c3                   	ret

0000000000401498 <journey::create_world:proc(indice_bit_capacity:$$575,unique_data_capacity:$$123)->(:journey::World)>:
  401498:	31 c0                	xor    %eax,%eax
  40149a:	41 89 c1             	mov    %eax,%r9d
  40149d:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  4014a2:	b8 09 00 00 00       	mov    $0x9,%eax
  4014a7:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4014ac:	be 00 10 00 00       	mov    $0x1000,%esi
  4014b1:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  4014b6:	ba 03 00 00 00       	mov    $0x3,%edx
  4014bb:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  4014c0:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4014c6:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  4014cb:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4014d2:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  4014d7:	4c 89 cf             	mov    %r9,%rdi
  4014da:	0f 05                	syscall
  4014dc:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  4014e1:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  4014e6:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  4014eb:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4014f0:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4014f5:	48 89 c1             	mov    %rax,%rcx
  4014f8:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4014fd:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401502:	4c 89 cf             	mov    %r9,%rdi
  401505:	0f 05                	syscall
  401507:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40150c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401511:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  401518:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40151d:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401522:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  401527:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40152c:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401531:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  401536:	c3                   	ret

0000000000401537 <journey::create_world:proc(indice_bit_capacity:$$236,unique_data_capacity:$$353)->(:journey::World)>:
  401537:	31 c0                	xor    %eax,%eax
  401539:	41 89 c1             	mov    %eax,%r9d
  40153c:	4c 89 4c 24 b8       	mov    %r9,-0x48(%rsp)
  401541:	b8 09 00 00 00       	mov    $0x9,%eax
  401546:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40154b:	be 00 30 00 00       	mov    $0x3000,%esi
  401550:	ba 03 00 00 00       	mov    $0x3,%edx
  401555:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40155a:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401560:	4c 89 54 24 c8       	mov    %r10,-0x38(%rsp)
  401565:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  40156c:	4c 89 44 24 c0       	mov    %r8,-0x40(%rsp)
  401571:	4c 89 cf             	mov    %r9,%rdi
  401574:	0f 05                	syscall
  401576:	4c 8b 4c 24 b8       	mov    -0x48(%rsp),%r9
  40157b:	4c 8b 44 24 c0       	mov    -0x40(%rsp),%r8
  401580:	4c 8b 54 24 c8       	mov    -0x38(%rsp),%r10
  401585:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  40158a:	48 89 c1             	mov    %rax,%rcx
  40158d:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401592:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401597:	be 00 10 00 00       	mov    $0x1000,%esi
  40159c:	4c 89 cf             	mov    %r9,%rdi
  40159f:	0f 05                	syscall
  4015a1:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4015a6:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4015ab:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  4015b2:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4015b7:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4015bc:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4015c1:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4015c6:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4015cb:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4015d0:	c3                   	ret

00000000004015d1 <journey::create_world:proc(indice_bit_capacity:$$345,unique_data_capacity:$$512)->(:journey::World)>:
  4015d1:	31 c0                	xor    %eax,%eax
  4015d3:	41 89 c1             	mov    %eax,%r9d
  4015d6:	4c 89 4c 24 b8       	mov    %r9,-0x48(%rsp)
  4015db:	b8 09 00 00 00       	mov    $0x9,%eax
  4015e0:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4015e5:	be 00 40 00 00       	mov    $0x4000,%esi
  4015ea:	ba 03 00 00 00       	mov    $0x3,%edx
  4015ef:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4015f4:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4015fa:	4c 89 54 24 c8       	mov    %r10,-0x38(%rsp)
  4015ff:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401606:	4c 89 44 24 c0       	mov    %r8,-0x40(%rsp)
  40160b:	4c 89 cf             	mov    %r9,%rdi
  40160e:	0f 05                	syscall
  401610:	4c 8b 4c 24 b8       	mov    -0x48(%rsp),%r9
  401615:	4c 8b 44 24 c0       	mov    -0x40(%rsp),%r8
  40161a:	4c 8b 54 24 c8       	mov    -0x38(%rsp),%r10
  40161f:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  401624:	48 89 c1             	mov    %rax,%rcx
  401627:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40162c:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401631:	be 00 10 00 00       	mov    $0x1000,%esi
  401636:	4c 89 cf             	mov    %r9,%rdi
  401639:	0f 05                	syscall
  40163b:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401640:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401645:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  40164c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401651:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401656:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40165b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401660:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401665:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40166a:	c3                   	ret

000000000040166b <journey::create_world:proc(indice_bit_capacity:$$1,unique_data_capacity:$$2)->(:journey::World)>:
  40166b:	31 c0                	xor    %eax,%eax
  40166d:	41 89 c1             	mov    %eax,%r9d
  401670:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  401675:	b8 09 00 00 00       	mov    $0x9,%eax
  40167a:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40167f:	be 00 10 00 00       	mov    $0x1000,%esi
  401684:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  401689:	ba 03 00 00 00       	mov    $0x3,%edx
  40168e:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  401693:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401699:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  40169e:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4016a5:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  4016aa:	4c 89 cf             	mov    %r9,%rdi
  4016ad:	0f 05                	syscall
  4016af:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  4016b4:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  4016b9:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  4016be:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4016c3:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4016c8:	48 89 c1             	mov    %rax,%rcx
  4016cb:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4016d0:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4016d5:	4c 89 cf             	mov    %r9,%rdi
  4016d8:	0f 05                	syscall
  4016da:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4016df:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4016e4:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  4016eb:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4016f0:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4016f5:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4016fa:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4016ff:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401704:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  401709:	c3                   	ret

000000000040170a <journey::create_world:proc(indice_bit_capacity:$$53,unique_data_capacity:$$24)->(:journey::World)>:
  40170a:	31 c0                	xor    %eax,%eax
  40170c:	41 89 c1             	mov    %eax,%r9d
  40170f:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  401714:	b8 09 00 00 00       	mov    $0x9,%eax
  401719:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40171e:	be 00 10 00 00       	mov    $0x1000,%esi
  401723:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  401728:	ba 03 00 00 00       	mov    $0x3,%edx
  40172d:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  401732:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401738:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  40173d:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401744:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  401749:	4c 89 cf             	mov    %r9,%rdi
  40174c:	0f 05                	syscall
  40174e:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  401753:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  401758:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  40175d:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401762:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  401767:	48 89 c1             	mov    %rax,%rcx
  40176a:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40176f:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401774:	4c 89 cf             	mov    %r9,%rdi
  401777:	0f 05                	syscall
  401779:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40177e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401783:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  40178a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40178f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401794:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  401799:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40179e:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4017a3:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4017a8:	c3                   	ret

00000000004017a9 <journey::create_world:proc(indice_bit_capacity:$$999,unique_data_capacity:$$1)->(:journey::World)>:
  4017a9:	31 c0                	xor    %eax,%eax
  4017ab:	41 89 c1             	mov    %eax,%r9d
  4017ae:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  4017b3:	b8 09 00 00 00       	mov    $0x9,%eax
  4017b8:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4017bd:	be 00 10 00 00       	mov    $0x1000,%esi
  4017c2:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  4017c7:	ba 03 00 00 00       	mov    $0x3,%edx
  4017cc:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  4017d1:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4017d7:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  4017dc:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4017e3:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  4017e8:	4c 89 cf             	mov    %r9,%rdi
  4017eb:	0f 05                	syscall
  4017ed:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  4017f2:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  4017f7:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  4017fc:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401801:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  401806:	48 89 c1             	mov    %rax,%rcx
  401809:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40180e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401813:	4c 89 cf             	mov    %r9,%rdi
  401816:	0f 05                	syscall
  401818:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40181d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401822:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  401829:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40182e:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401833:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  401838:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40183d:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401842:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  401847:	c3                   	ret

0000000000401848 <journey::create_world:proc(indice_bit_capacity:$$1,unique_data_capacity:$$999)->(:journey::World)>:
  401848:	31 c0                	xor    %eax,%eax
  40184a:	41 89 c1             	mov    %eax,%r9d
  40184d:	4c 89 4c 24 b8       	mov    %r9,-0x48(%rsp)
  401852:	b8 09 00 00 00       	mov    $0x9,%eax
  401857:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40185c:	be 00 80 00 00       	mov    $0x8000,%esi
  401861:	ba 03 00 00 00       	mov    $0x3,%edx
  401866:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40186b:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401871:	4c 89 54 24 c8       	mov    %r10,-0x38(%rsp)
  401876:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  40187d:	4c 89 44 24 c0       	mov    %r8,-0x40(%rsp)
  401882:	4c 89 cf             	mov    %r9,%rdi
  401885:	0f 05                	syscall
  401887:	4c 8b 4c 24 b8       	mov    -0x48(%rsp),%r9
  40188c:	4c 8b 44 24 c0       	mov    -0x40(%rsp),%r8
  401891:	4c 8b 54 24 c8       	mov    -0x38(%rsp),%r10
  401896:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  40189b:	48 89 c1             	mov    %rax,%rcx
  40189e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4018a3:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4018a8:	be 00 10 00 00       	mov    $0x1000,%esi
  4018ad:	4c 89 cf             	mov    %r9,%rdi
  4018b0:	0f 05                	syscall
  4018b2:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4018b7:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4018bc:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  4018c3:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4018c8:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4018cd:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4018d2:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4018d7:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4018dc:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4018e1:	c3                   	ret
  4018e2:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4018e9:	00 00 00 
  4018ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004018f0 <__$startup_runtime>:
  4018f0:	eb 00                	jmp    4018f2 <__$startup_runtime+0x2>
  4018f2:	c3                   	ret
  4018f3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4018fa:	84 00 00 00 00 00 

0000000000401900 <__$cleanup_runtime>:
  401900:	50                   	push   %rax
  401901:	48 89 3c 24          	mov    %rdi,(%rsp)
  401905:	eb 00                	jmp    401907 <__$cleanup_runtime+0x7>
  401907:	48 8b 3c 24          	mov    (%rsp),%rdi
  40190b:	e8 10 00 00 00       	call   401920 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  401910:	58                   	pop    %rax
  401911:	c3                   	ret
  401912:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401919:	00 00 00 
  40191c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401920 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  401920:	50                   	push   %rax
  401921:	48 89 3c 24          	mov    %rdi,(%rsp)
  401925:	eb 00                	jmp    401927 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x7>
  401927:	48 8b 34 24          	mov    (%rsp),%rsi
  40192b:	48 c7 c0 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rax
  401932:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  401939:	00 00 
  40193b:	48 01 c7             	add    %rax,%rdi
  40193e:	e8 8d 11 00 00       	call   402ad0 <runtime::default_temp_allocator_destroy>
  401943:	58                   	pop    %rax
  401944:	c3                   	ret
  401945:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40194c:	00 00 00 00 

0000000000401950 <runtime::bounds_trap>:
  401950:	eb 00                	jmp    401952 <runtime::bounds_trap+0x2>
  401952:	0f 0b                	ud2
  401954:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40195b:	00 00 00 00 00 

0000000000401960 <runtime::heap_allocator>:
  401960:	48 c7 c0 00 24 40 00 	mov    $0x402400,%rax
  401967:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40196c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401973:	00 00 
  401975:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40197a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40197f:	c3                   	ret

0000000000401980 <runtime::udivmod128>:
  401980:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  401987:	4c 89 44 24 a8       	mov    %r8,-0x58(%rsp)
  40198c:	48 89 4c 24 b0       	mov    %rcx,-0x50(%rsp)
  401991:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401996:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40199b:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  4019a0:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4019a5:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  4019aa:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4019af:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  4019b4:	48 8b 7c 24 a8       	mov    -0x58(%rsp),%rdi
  4019b9:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4019c0:	00 
  4019c1:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4019c8:	00 
  4019c9:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  4019d0:	00 
  4019d1:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4019d8:	00 
  4019d9:	48 89 bc 24 88 00 00 	mov    %rdi,0x88(%rsp)
  4019e0:	00 
  4019e1:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4019e6:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  4019eb:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4019f0:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4019f5:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4019fa:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  4019ff:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  401a04:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  401a09:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  401a0e:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  401a13:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  401a18:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401a1d:	0f 57 c0             	xorps  %xmm0,%xmm0
  401a20:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401a25:	0f 57 c0             	xorps  %xmm0,%xmm0
  401a28:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  401a2d:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  401a34:	00 
  401a35:	48 83 7c 24 68 00    	cmpq   $0x0,0x68(%rsp)
  401a3b:	0f 94 c0             	sete   %al
  401a3e:	24 01                	and    $0x1,%al
  401a40:	3c 00                	cmp    $0x0,%al
  401a42:	0f 84 a1 00 00 00    	je     401ae9 <runtime::udivmod128+0x169>
  401a48:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401a4e:	0f 94 c0             	sete   %al
  401a51:	24 01                	and    $0x1,%al
  401a53:	3c 00                	cmp    $0x0,%al
  401a55:	74 5c                	je     401ab3 <runtime::udivmod128+0x133>
  401a57:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a5c:	48 83 f8 00          	cmp    $0x0,%rax
  401a60:	0f 95 c0             	setne  %al
  401a63:	24 01                	and    $0x1,%al
  401a65:	3c 00                	cmp    $0x0,%al
  401a67:	74 29                	je     401a92 <runtime::udivmod128+0x112>
  401a69:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401a6e:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401a73:	31 d2                	xor    %edx,%edx
  401a75:	48 f7 f1             	div    %rcx
  401a78:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a7d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  401a82:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401a87:	48 89 08             	mov    %rcx,(%rax)
  401a8a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401a91:	00 
  401a92:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401a97:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401a9c:	31 d2                	xor    %edx,%edx
  401a9e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  401aa3:	48 f7 f1             	div    %rcx
  401aa6:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  401aab:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401ab2:	c3                   	ret
  401ab3:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401ab8:	48 83 f8 00          	cmp    $0x0,%rax
  401abc:	0f 95 c0             	setne  %al
  401abf:	24 01                	and    $0x1,%al
  401ac1:	3c 00                	cmp    $0x0,%al
  401ac3:	74 15                	je     401ada <runtime::udivmod128+0x15a>
  401ac5:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401aca:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401acf:	48 89 08             	mov    %rcx,(%rax)
  401ad2:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401ad9:	00 
  401ada:	31 c0                	xor    %eax,%eax
  401adc:	89 c2                	mov    %eax,%edx
  401ade:	48 89 d0             	mov    %rdx,%rax
  401ae1:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401ae8:	c3                   	ret
  401ae9:	48 83 7c 24 40 00    	cmpq   $0x0,0x40(%rsp)
  401aef:	0f 94 c0             	sete   %al
  401af2:	24 01                	and    $0x1,%al
  401af4:	3c 00                	cmp    $0x0,%al
  401af6:	0f 84 8b 02 00 00    	je     401d87 <runtime::udivmod128+0x407>
  401afc:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401b02:	0f 94 c0             	sete   %al
  401b05:	24 01                	and    $0x1,%al
  401b07:	3c 00                	cmp    $0x0,%al
  401b09:	74 52                	je     401b5d <runtime::udivmod128+0x1dd>
  401b0b:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b10:	48 83 f8 00          	cmp    $0x0,%rax
  401b14:	0f 95 c0             	setne  %al
  401b17:	24 01                	and    $0x1,%al
  401b19:	3c 00                	cmp    $0x0,%al
  401b1b:	74 1f                	je     401b3c <runtime::udivmod128+0x1bc>
  401b1d:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b22:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401b27:	31 d2                	xor    %edx,%edx
  401b29:	48 f7 f1             	div    %rcx
  401b2c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b31:	48 89 10             	mov    %rdx,(%rax)
  401b34:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401b3b:	00 
  401b3c:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b41:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401b46:	31 d2                	xor    %edx,%edx
  401b48:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  401b4d:	48 f7 f1             	div    %rcx
  401b50:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  401b55:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401b5c:	c3                   	ret
  401b5d:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  401b63:	0f 94 c0             	sete   %al
  401b66:	24 01                	and    $0x1,%al
  401b68:	3c 00                	cmp    $0x0,%al
  401b6a:	74 66                	je     401bd2 <runtime::udivmod128+0x252>
  401b6c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b71:	48 83 f8 00          	cmp    $0x0,%rax
  401b75:	0f 95 c0             	setne  %al
  401b78:	24 01                	and    $0x1,%al
  401b7a:	3c 00                	cmp    $0x0,%al
  401b7c:	74 33                	je     401bb1 <runtime::udivmod128+0x231>
  401b7e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b83:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401b88:	31 d2                	xor    %edx,%edx
  401b8a:	48 f7 f1             	div    %rcx
  401b8d:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b92:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401b97:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  401b9e:	00 00 
  401ba0:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401ba5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401baa:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401bae:	48 89 08             	mov    %rcx,(%rax)
  401bb1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401bb6:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401bbb:	31 d2                	xor    %edx,%edx
  401bbd:	48 89 54 24 90       	mov    %rdx,-0x70(%rsp)
  401bc2:	48 f7 f1             	div    %rcx
  401bc5:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  401bca:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401bd1:	c3                   	ret
  401bd2:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401bd7:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401bdc:	48 83 e9 01          	sub    $0x1,%rcx
  401be0:	48 21 c8             	and    %rcx,%rax
  401be3:	48 83 f8 00          	cmp    $0x0,%rax
  401be7:	0f 94 c0             	sete   %al
  401bea:	24 01                	and    $0x1,%al
  401bec:	3c 00                	cmp    $0x0,%al
  401bee:	74 7a                	je     401c6a <runtime::udivmod128+0x2ea>
  401bf0:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401bf5:	48 83 f8 00          	cmp    $0x0,%rax
  401bf9:	0f 95 c0             	setne  %al
  401bfc:	24 01                	and    $0x1,%al
  401bfe:	3c 00                	cmp    $0x0,%al
  401c00:	74 35                	je     401c37 <runtime::udivmod128+0x2b7>
  401c02:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401c07:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401c0c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401c11:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  401c16:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  401c1b:	48 ff ca             	dec    %rdx
  401c1e:	48 21 d1             	and    %rdx,%rcx
  401c21:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401c26:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401c2b:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401c30:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401c34:	48 89 08             	mov    %rcx,(%rax)
  401c37:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401c3c:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401c41:	ba 40 00 00 00       	mov    $0x40,%edx
  401c46:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  401c4b:	88 d1                	mov    %dl,%cl
  401c4d:	48 d3 e8             	shr    %cl,%rax
  401c50:	48 89 c1             	mov    %rax,%rcx
  401c53:	31 c0                	xor    %eax,%eax
  401c55:	48 83 ea 40          	sub    $0x40,%rdx
  401c59:	89 c2                	mov    %eax,%edx
  401c5b:	48 89 d0             	mov    %rdx,%rax
  401c5e:	48 0f 42 c1          	cmovb  %rcx,%rax
  401c62:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401c69:	c3                   	ret
  401c6a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401c6f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401c74:	48 0f bd c1          	bsr    %rcx,%rax
  401c78:	48 83 f0 3f          	xor    $0x3f,%rax
  401c7c:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401c81:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401c86:	48 0f bd ca          	bsr    %rdx,%rcx
  401c8a:	48 83 f1 3f          	xor    $0x3f,%rcx
  401c8e:	29 c8                	sub    %ecx,%eax
  401c90:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401c94:	83 7c 24 1c 3e       	cmpl   $0x3e,0x1c(%rsp)
  401c99:	0f 97 c0             	seta   %al
  401c9c:	24 01                	and    $0x1,%al
  401c9e:	3c 00                	cmp    $0x0,%al
  401ca0:	74 37                	je     401cd9 <runtime::udivmod128+0x359>
  401ca2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401ca7:	48 83 f8 00          	cmp    $0x0,%rax
  401cab:	0f 95 c0             	setne  %al
  401cae:	24 01                	and    $0x1,%al
  401cb0:	3c 00                	cmp    $0x0,%al
  401cb2:	74 16                	je     401cca <runtime::udivmod128+0x34a>
  401cb4:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401cb9:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  401cbe:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401cc3:	48 89 10             	mov    %rdx,(%rax)
  401cc6:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401cca:	31 c0                	xor    %eax,%eax
  401ccc:	89 c2                	mov    %eax,%edx
  401cce:	48 89 d0             	mov    %rdx,%rax
  401cd1:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401cd8:	c3                   	ret
  401cd9:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401cdd:	83 c0 01             	add    $0x1,%eax
  401ce0:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401ce4:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401ceb:	00 00 
  401ced:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401cf2:	b9 40 00 00 00       	mov    $0x40,%ecx
  401cf7:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401cfb:	89 c9                	mov    %ecx,%ecx
  401cfd:	89 ca                	mov    %ecx,%edx
  401cff:	48 89 d1             	mov    %rdx,%rcx
  401d02:	48 d3 e0             	shl    %cl,%rax
  401d05:	48 89 c1             	mov    %rax,%rcx
  401d08:	31 c0                	xor    %eax,%eax
  401d0a:	48 83 fa 40          	cmp    $0x40,%rdx
  401d0e:	48 0f 42 c1          	cmovb  %rcx,%rax
  401d12:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401d17:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401d1c:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401d20:	89 ca                	mov    %ecx,%edx
  401d22:	48 89 d1             	mov    %rdx,%rcx
  401d25:	48 d3 e8             	shr    %cl,%rax
  401d28:	48 89 c1             	mov    %rax,%rcx
  401d2b:	31 c0                	xor    %eax,%eax
  401d2d:	48 83 fa 40          	cmp    $0x40,%rdx
  401d31:	48 0f 42 c1          	cmovb  %rcx,%rax
  401d35:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401d3a:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401d3f:	b9 40 00 00 00       	mov    $0x40,%ecx
  401d44:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401d48:	89 c9                	mov    %ecx,%ecx
  401d4a:	89 ca                	mov    %ecx,%edx
  401d4c:	48 89 d1             	mov    %rdx,%rcx
  401d4f:	48 d3 e0             	shl    %cl,%rax
  401d52:	48 89 c1             	mov    %rax,%rcx
  401d55:	31 c0                	xor    %eax,%eax
  401d57:	48 83 fa 40          	cmp    $0x40,%rdx
  401d5b:	48 0f 42 c1          	cmovb  %rcx,%rax
  401d5f:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401d64:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401d68:	89 ce                	mov    %ecx,%esi
  401d6a:	48 89 f1             	mov    %rsi,%rcx
  401d6d:	48 d3 ea             	shr    %cl,%rdx
  401d70:	31 c9                	xor    %ecx,%ecx
  401d72:	48 83 fe 40          	cmp    $0x40,%rsi
  401d76:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401d7a:	48 09 c8             	or     %rcx,%rax
  401d7d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401d82:	e9 30 04 00 00       	jmp    4021b7 <runtime::udivmod128+0x837>
  401d87:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401d8d:	0f 94 c0             	sete   %al
  401d90:	24 01                	and    $0x1,%al
  401d92:	3c 00                	cmp    $0x0,%al
  401d94:	0f 84 d1 02 00 00    	je     40206b <runtime::udivmod128+0x6eb>
  401d9a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401d9f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401da4:	48 83 e9 01          	sub    $0x1,%rcx
  401da8:	48 21 c8             	and    %rcx,%rax
  401dab:	48 83 f8 00          	cmp    $0x0,%rax
  401daf:	0f 94 c0             	sete   %al
  401db2:	24 01                	and    $0x1,%al
  401db4:	3c 00                	cmp    $0x0,%al
  401db6:	0f 84 de 00 00 00    	je     401e9a <runtime::udivmod128+0x51a>
  401dbc:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401dc1:	48 83 f8 00          	cmp    $0x0,%rax
  401dc5:	0f 95 c0             	setne  %al
  401dc8:	24 01                	and    $0x1,%al
  401dca:	3c 00                	cmp    $0x0,%al
  401dcc:	74 20                	je     401dee <runtime::udivmod128+0x46e>
  401dce:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401dd3:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401dd8:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401ddd:	48 ff ca             	dec    %rdx
  401de0:	48 21 d1             	and    %rdx,%rcx
  401de3:	48 89 08             	mov    %rcx,(%rax)
  401de6:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401ded:	00 
  401dee:	48 83 7c 24 40 01    	cmpq   $0x1,0x40(%rsp)
  401df4:	0f 94 c0             	sete   %al
  401df7:	24 01                	and    $0x1,%al
  401df9:	3c 00                	cmp    $0x0,%al
  401dfb:	74 12                	je     401e0f <runtime::udivmod128+0x48f>
  401dfd:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401e02:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401e07:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401e0e:	c3                   	ret
  401e0f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401e14:	b8 40 00 00 00       	mov    $0x40,%eax
  401e19:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  401e1e:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401e22:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401e27:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401e2b:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  401e2f:	88 c1                	mov    %al,%cl
  401e31:	48 d3 ea             	shr    %cl,%rdx
  401e34:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  401e38:	31 c0                	xor    %eax,%eax
  401e3a:	83 e9 40             	sub    $0x40,%ecx
  401e3d:	48 89 c1             	mov    %rax,%rcx
  401e40:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401e44:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  401e49:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  401e4e:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401e53:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  401e57:	40 88 f1             	mov    %sil,%cl
  401e5a:	48 d3 ef             	shr    %cl,%rdi
  401e5d:	83 ee 40             	sub    $0x40,%esi
  401e60:	48 89 c1             	mov    %rax,%rcx
  401e63:	48 0f 42 cf          	cmovb  %rdi,%rcx
  401e67:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  401e6c:	f7 de                	neg    %esi
  401e6e:	40 88 f1             	mov    %sil,%cl
  401e71:	48 d3 e2             	shl    %cl,%rdx
  401e74:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  401e79:	83 ee 40             	sub    $0x40,%esi
  401e7c:	48 0f 42 c2          	cmovb  %rdx,%rax
  401e80:	48 09 c8             	or     %rcx,%rax
  401e83:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401e88:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401e8d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401e92:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401e99:	c3                   	ret
  401e9a:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401e9f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401ea4:	48 0f bd c1          	bsr    %rcx,%rax
  401ea8:	48 83 f0 3f          	xor    $0x3f,%rax
  401eac:	83 c0 41             	add    $0x41,%eax
  401eaf:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401eb4:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401eb9:	48 0f bd ca          	bsr    %rdx,%rcx
  401ebd:	48 83 f1 3f          	xor    $0x3f,%rcx
  401ec1:	29 c8                	sub    %ecx,%eax
  401ec3:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401ec7:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401ecc:	0f 94 c1             	sete   %cl
  401ecf:	80 e1 01             	and    $0x1,%cl
  401ed2:	b0 01                	mov    $0x1,%al
  401ed4:	38 c8                	cmp    %cl,%al
  401ed6:	74 13                	je     401eeb <runtime::udivmod128+0x56b>
  401ed8:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401edd:	0f 92 c1             	setb   %cl
  401ee0:	80 e1 01             	and    $0x1,%cl
  401ee3:	b0 01                	mov    $0x1,%al
  401ee5:	38 c8                	cmp    %cl,%al
  401ee7:	74 32                	je     401f1b <runtime::udivmod128+0x59b>
  401ee9:	eb 2b                	jmp    401f16 <runtime::udivmod128+0x596>
  401eeb:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401ef2:	00 00 
  401ef4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401ef9:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401efe:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401f05:	00 00 
  401f07:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401f0c:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401f11:	e9 50 01 00 00       	jmp    402066 <runtime::udivmod128+0x6e6>
  401f16:	e9 a3 00 00 00       	jmp    401fbe <runtime::udivmod128+0x63e>
  401f1b:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401f22:	00 00 
  401f24:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401f29:	b9 40 00 00 00       	mov    $0x40,%ecx
  401f2e:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401f32:	89 c9                	mov    %ecx,%ecx
  401f34:	89 ca                	mov    %ecx,%edx
  401f36:	48 89 d1             	mov    %rdx,%rcx
  401f39:	48 d3 e0             	shl    %cl,%rax
  401f3c:	48 89 c1             	mov    %rax,%rcx
  401f3f:	31 c0                	xor    %eax,%eax
  401f41:	48 83 fa 40          	cmp    $0x40,%rdx
  401f45:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f49:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401f4e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401f53:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401f57:	89 ca                	mov    %ecx,%edx
  401f59:	48 89 d1             	mov    %rdx,%rcx
  401f5c:	48 d3 e8             	shr    %cl,%rax
  401f5f:	48 89 c1             	mov    %rax,%rcx
  401f62:	31 c0                	xor    %eax,%eax
  401f64:	48 83 fa 40          	cmp    $0x40,%rdx
  401f68:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f6c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401f71:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401f76:	b9 40 00 00 00       	mov    $0x40,%ecx
  401f7b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401f7f:	89 c9                	mov    %ecx,%ecx
  401f81:	89 ca                	mov    %ecx,%edx
  401f83:	48 89 d1             	mov    %rdx,%rcx
  401f86:	48 d3 e0             	shl    %cl,%rax
  401f89:	48 89 c1             	mov    %rax,%rcx
  401f8c:	31 c0                	xor    %eax,%eax
  401f8e:	48 83 fa 40          	cmp    $0x40,%rdx
  401f92:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f96:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401f9b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401f9f:	89 ce                	mov    %ecx,%esi
  401fa1:	48 89 f1             	mov    %rsi,%rcx
  401fa4:	48 d3 ea             	shr    %cl,%rdx
  401fa7:	31 c9                	xor    %ecx,%ecx
  401fa9:	48 83 fe 40          	cmp    $0x40,%rsi
  401fad:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401fb1:	48 09 c8             	or     %rcx,%rax
  401fb4:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401fb9:	e9 a8 00 00 00       	jmp    402066 <runtime::udivmod128+0x6e6>
  401fbe:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401fc3:	b9 80 00 00 00       	mov    $0x80,%ecx
  401fc8:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401fcc:	89 c9                	mov    %ecx,%ecx
  401fce:	89 ca                	mov    %ecx,%edx
  401fd0:	48 89 d1             	mov    %rdx,%rcx
  401fd3:	48 d3 e0             	shl    %cl,%rax
  401fd6:	48 89 c1             	mov    %rax,%rcx
  401fd9:	31 c0                	xor    %eax,%eax
  401fdb:	48 83 fa 40          	cmp    $0x40,%rdx
  401fdf:	48 0f 42 c1          	cmovb  %rcx,%rax
  401fe3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401fe8:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401fed:	b9 80 00 00 00       	mov    $0x80,%ecx
  401ff2:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401ff6:	89 c9                	mov    %ecx,%ecx
  401ff8:	89 ca                	mov    %ecx,%edx
  401ffa:	48 89 d1             	mov    %rdx,%rcx
  401ffd:	48 d3 e0             	shl    %cl,%rax
  402000:	48 89 c1             	mov    %rax,%rcx
  402003:	31 c0                	xor    %eax,%eax
  402005:	48 83 fa 40          	cmp    $0x40,%rdx
  402009:	48 0f 42 c1          	cmovb  %rcx,%rax
  40200d:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  402012:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402016:	83 e9 40             	sub    $0x40,%ecx
  402019:	89 c9                	mov    %ecx,%ecx
  40201b:	89 ce                	mov    %ecx,%esi
  40201d:	48 89 f1             	mov    %rsi,%rcx
  402020:	48 d3 ea             	shr    %cl,%rdx
  402023:	31 c9                	xor    %ecx,%ecx
  402025:	48 83 fe 40          	cmp    $0x40,%rsi
  402029:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40202d:	48 09 c8             	or     %rcx,%rax
  402030:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402035:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  40203c:	00 00 
  40203e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402043:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402047:	83 e9 40             	sub    $0x40,%ecx
  40204a:	89 c9                	mov    %ecx,%ecx
  40204c:	89 ca                	mov    %ecx,%edx
  40204e:	48 89 d1             	mov    %rdx,%rcx
  402051:	48 d3 e8             	shr    %cl,%rax
  402054:	48 89 c1             	mov    %rax,%rcx
  402057:	31 c0                	xor    %eax,%eax
  402059:	48 83 fa 40          	cmp    $0x40,%rdx
  40205d:	48 0f 42 c1          	cmovb  %rcx,%rax
  402061:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402066:	e9 4a 01 00 00       	jmp    4021b5 <runtime::udivmod128+0x835>
  40206b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402070:	b8 7f 00 00 00       	mov    $0x7f,%eax
  402075:	48 0f bd c1          	bsr    %rcx,%rax
  402079:	48 83 f0 3f          	xor    $0x3f,%rax
  40207d:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  402082:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  402087:	48 0f bd ca          	bsr    %rdx,%rcx
  40208b:	48 83 f1 3f          	xor    $0x3f,%rcx
  40208f:	29 c8                	sub    %ecx,%eax
  402091:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  402095:	83 7c 24 1c 3f       	cmpl   $0x3f,0x1c(%rsp)
  40209a:	0f 97 c0             	seta   %al
  40209d:	24 01                	and    $0x1,%al
  40209f:	3c 00                	cmp    $0x0,%al
  4020a1:	74 37                	je     4020da <runtime::udivmod128+0x75a>
  4020a3:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4020a8:	48 83 f8 00          	cmp    $0x0,%rax
  4020ac:	0f 95 c0             	setne  %al
  4020af:	24 01                	and    $0x1,%al
  4020b1:	3c 00                	cmp    $0x0,%al
  4020b3:	74 16                	je     4020cb <runtime::udivmod128+0x74b>
  4020b5:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4020ba:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4020bf:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4020c4:	48 89 10             	mov    %rdx,(%rax)
  4020c7:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4020cb:	31 c0                	xor    %eax,%eax
  4020cd:	89 c2                	mov    %eax,%edx
  4020cf:	48 89 d0             	mov    %rdx,%rax
  4020d2:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4020d9:	c3                   	ret
  4020da:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  4020de:	83 c0 01             	add    $0x1,%eax
  4020e1:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4020e5:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4020ec:	00 00 
  4020ee:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  4020f3:	0f 94 c0             	sete   %al
  4020f6:	24 01                	and    $0x1,%al
  4020f8:	3c 00                	cmp    $0x0,%al
  4020fa:	74 22                	je     40211e <runtime::udivmod128+0x79e>
  4020fc:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402101:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402106:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  40210d:	00 00 
  40210f:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402114:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402119:	e9 95 00 00 00       	jmp    4021b3 <runtime::udivmod128+0x833>
  40211e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402123:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402127:	89 ca                	mov    %ecx,%edx
  402129:	48 89 d1             	mov    %rdx,%rcx
  40212c:	48 d3 e8             	shr    %cl,%rax
  40212f:	48 89 c1             	mov    %rax,%rcx
  402132:	31 c0                	xor    %eax,%eax
  402134:	48 83 fa 40          	cmp    $0x40,%rdx
  402138:	48 0f 42 c1          	cmovb  %rcx,%rax
  40213c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402141:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402146:	b9 40 00 00 00       	mov    $0x40,%ecx
  40214b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40214f:	89 c9                	mov    %ecx,%ecx
  402151:	89 ca                	mov    %ecx,%edx
  402153:	48 89 d1             	mov    %rdx,%rcx
  402156:	48 d3 e0             	shl    %cl,%rax
  402159:	48 89 c1             	mov    %rax,%rcx
  40215c:	31 c0                	xor    %eax,%eax
  40215e:	48 83 fa 40          	cmp    $0x40,%rdx
  402162:	48 0f 42 c1          	cmovb  %rcx,%rax
  402166:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40216b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40216f:	89 ce                	mov    %ecx,%esi
  402171:	48 89 f1             	mov    %rsi,%rcx
  402174:	48 d3 ea             	shr    %cl,%rdx
  402177:	31 c9                	xor    %ecx,%ecx
  402179:	48 83 fe 40          	cmp    $0x40,%rsi
  40217d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  402181:	48 09 c8             	or     %rcx,%rax
  402184:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402189:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40218e:	b9 40 00 00 00       	mov    $0x40,%ecx
  402193:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  402197:	89 c9                	mov    %ecx,%ecx
  402199:	89 ca                	mov    %ecx,%edx
  40219b:	48 89 d1             	mov    %rdx,%rcx
  40219e:	48 d3 e0             	shl    %cl,%rax
  4021a1:	48 89 c1             	mov    %rax,%rcx
  4021a4:	31 c0                	xor    %eax,%eax
  4021a6:	48 83 fa 40          	cmp    $0x40,%rdx
  4021aa:	48 0f 42 c1          	cmovb  %rcx,%rax
  4021ae:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4021b3:	eb 00                	jmp    4021b5 <runtime::udivmod128+0x835>
  4021b5:	eb 00                	jmp    4021b7 <runtime::udivmod128+0x837>
  4021b7:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  4021be:	00 
  4021bf:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  4021c6:	00 00 
  4021c8:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  4021cf:	00 00 
  4021d1:	83 7c 24 1c 00       	cmpl   $0x0,0x1c(%rsp)
  4021d6:	0f 97 c0             	seta   %al
  4021d9:	24 01                	and    $0x1,%al
  4021db:	3c 00                	cmp    $0x0,%al
  4021dd:	0f 84 eb 00 00 00    	je     4022ce <runtime::udivmod128+0x94e>
  4021e3:	48 8b 74 24 b8       	mov    -0x48(%rsp),%rsi
  4021e8:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  4021ed:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4021f2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4021f7:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  4021fc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402201:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402206:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40220b:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  402210:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402215:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40221a:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40221f:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  402224:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402229:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40222e:	48 01 c0             	add    %rax,%rax
  402231:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  402235:	48 09 c8             	or     %rcx,%rax
  402238:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40223d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402242:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  402247:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40224c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  402251:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  402256:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40225b:	48 f7 d0             	not    %rax
  40225e:	48 f7 d1             	not    %rcx
  402261:	48 01 f1             	add    %rsi,%rcx
  402264:	48 11 d0             	adc    %rdx,%rax
  402267:	48 c1 f8 3f          	sar    $0x3f,%rax
  40226b:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402270:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  402275:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  402279:	83 e0 01             	and    $0x1,%eax
  40227c:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  402280:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  402285:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40228a:	48 21 ca             	and    %rcx,%rdx
  40228d:	48 21 c6             	and    %rax,%rsi
  402290:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  402295:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40229a:	48 29 f1             	sub    %rsi,%rcx
  40229d:	48 19 d0             	sbb    %rdx,%rax
  4022a0:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4022a5:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4022aa:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4022af:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4022b4:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4022b9:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4022be:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  4022c2:	83 e8 01             	sub    $0x1,%eax
  4022c5:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4022c9:	e9 03 ff ff ff       	jmp    4021d1 <runtime::udivmod128+0x851>
  4022ce:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4022d3:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4022d8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4022dd:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  4022e2:	48 01 c9             	add    %rcx,%rcx
  4022e5:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  4022e9:	48 09 f1             	or     %rsi,%rcx
  4022ec:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  4022f1:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  4022f6:	48 83 f8 00          	cmp    $0x0,%rax
  4022fa:	0f 95 c0             	setne  %al
  4022fd:	24 01                	and    $0x1,%al
  4022ff:	3c 00                	cmp    $0x0,%al
  402301:	74 16                	je     402319 <runtime::udivmod128+0x999>
  402303:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  402308:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40230d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  402312:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402316:	48 89 08             	mov    %rcx,(%rax)
  402319:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40231e:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  402323:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40232a:	c3                   	ret
  40232b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402330 <runtime::stderr_write>:
  402330:	48 83 ec 48          	sub    $0x48,%rsp
  402334:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  402339:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40233e:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  402343:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402348:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40234d:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  402352:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  402357:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40235e:	00 00 
  402360:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  402365:	e8 16 00 00 00       	call   402380 <runtime::[os_specific_linux.odin]::_stderr_write>
  40236a:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40236f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402374:	48 89 11             	mov    %rdx,(%rcx)
  402377:	48 83 c4 48          	add    $0x48,%rsp
  40237b:	c3                   	ret
  40237c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402380 <runtime::[os_specific_linux.odin]::_stderr_write>:
  402380:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  402385:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40238a:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  40238f:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  402394:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  402399:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  40239e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4023a3:	b8 01 00 00 00       	mov    $0x1,%eax
  4023a8:	bf 02 00 00 00       	mov    $0x2,%edi
  4023ad:	0f 05                	syscall
  4023af:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4023b4:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  4023ba:	0f 9c c0             	setl   %al
  4023bd:	24 01                	and    $0x1,%al
  4023bf:	3c 00                	cmp    $0x0,%al
  4023c1:	74 26                	je     4023e9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4023c3:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  4023ca:	ff ff 
  4023cc:	0f 9f c0             	setg   %al
  4023cf:	24 01                	and    $0x1,%al
  4023d1:	3c 00                	cmp    $0x0,%al
  4023d3:	74 14                	je     4023e9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4023d5:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  4023da:	31 c0                	xor    %eax,%eax
  4023dc:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  4023e1:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  4023e8:	c3                   	ret
  4023e9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4023ee:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4023f3:	48 89 08             	mov    %rcx,(%rax)
  4023f6:	31 c0                	xor    %eax,%eax
  4023f8:	c3                   	ret
  4023f9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000402400 <runtime::heap_allocator_proc>:
  402400:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  402407:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40240c:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  402411:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402416:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40241b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  402420:	40 88 f0             	mov    %sil,%al
  402423:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  402427:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40242e:	00 
  40242f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402434:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40243b:	00 
  40243c:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402441:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  402445:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40244a:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40244f:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402454:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402459:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  40245e:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  402465:	00 
  402466:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  40246d:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  402474:	00 
  402475:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  40247c:	00 
  40247d:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  402484:	00 
  402485:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40248c:	00 
  40248d:	0f b6 c8             	movzbl %al,%ecx
  402490:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  402495:	2c 07                	sub    $0x7,%al
  402497:	0f 87 5f 01 00 00    	ja     4025fc <runtime::heap_allocator_proc+0x1fc>
  40249d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4024a2:	48 8b 04 c5 c0 70 40 	mov    0x4070c0(,%rax,8),%rax
  4024a9:	00 
  4024aa:	ff e0                	jmp    *%rax
  4024ac:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4024b1:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4024b6:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  4024bb:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  4024bf:	84 c0                	test   %al,%al
  4024c1:	0f 94 c0             	sete   %al
  4024c4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4024c7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4024ce:	00 
  4024cf:	48 89 e1             	mov    %rsp,%rcx
  4024d2:	48 89 11             	mov    %rdx,(%rcx)
  4024d5:	44 0f b6 c0          	movzbl %al,%r8d
  4024d9:	31 c0                	xor    %eax,%eax
  4024db:	89 c1                	mov    %eax,%ecx
  4024dd:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  4024e4:	00 
  4024e5:	48 89 ca             	mov    %rcx,%rdx
  4024e8:	e8 43 3f 00 00       	call   406430 <runtime::heap_allocator_proc.aligned_alloc-0>
  4024ed:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4024f2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4024f9:	00 
  4024fa:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  402501:	00 
  402502:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402506:	48 89 11             	mov    %rdx,(%rcx)
  402509:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402510:	c3                   	ret
  402511:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  402516:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40251b:	e8 60 41 00 00       	call   406680 <runtime::heap_allocator_proc.aligned_free-1>
  402520:	e9 d7 00 00 00       	jmp    4025fc <runtime::heap_allocator_proc+0x1fc>
  402525:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40252a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402531:	00 
  402532:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402539:	b0 04                	mov    $0x4,%al
  40253b:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402542:	c3                   	ret
  402543:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402548:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40254d:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402552:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  402557:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  40255c:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  402560:	2c 03                	sub    $0x3,%al
  402562:	0f 94 c0             	sete   %al
  402565:	0f 57 c0             	xorps  %xmm0,%xmm0
  402568:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40256d:	49 89 e0             	mov    %rsp,%r8
  402570:	4d 89 08             	mov    %r9,(%r8)
  402573:	44 0f b6 c0          	movzbl %al,%r8d
  402577:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  40257c:	e8 3f 41 00 00       	call   4066c0 <runtime::heap_allocator_proc.aligned_resize-2>
  402581:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402586:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40258b:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  402590:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402594:	48 89 11             	mov    %rdx,(%rcx)
  402597:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40259e:	c3                   	ret
  40259f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4025a4:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4025a9:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  4025af:	0f 95 c0             	setne  %al
  4025b2:	24 01                	and    $0x1,%al
  4025b4:	3c 00                	cmp    $0x0,%al
  4025b6:	74 08                	je     4025c0 <runtime::heap_allocator_proc+0x1c0>
  4025b8:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4025bd:	c6 00 db             	movb   $0xdb,(%rax)
  4025c0:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4025c5:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4025cc:	00 
  4025cd:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4025d4:	31 c0                	xor    %eax,%eax
  4025d6:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4025dd:	c3                   	ret
  4025de:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4025e3:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4025ea:	00 
  4025eb:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4025f2:	b0 04                	mov    $0x4,%al
  4025f4:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4025fb:	c3                   	ret
  4025fc:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402601:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402608:	00 
  402609:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402610:	31 c0                	xor    %eax,%eax
  402612:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402619:	c3                   	ret
  40261a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402620 <runtime::[internal.odin]::byte_slice>:
  402620:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  402625:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40262a:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  40262f:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  402634:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402639:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  40263e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402643:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  402648:	31 c0                	xor    %eax,%eax
  40264a:	48 85 d2             	test   %rdx,%rdx
  40264d:	48 0f 49 c2          	cmovns %rdx,%rax
  402651:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  402656:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40265b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  402660:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  402665:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40266a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40266f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  402674:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  402679:	c3                   	ret
  40267a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402680 <runtime::bounds_check_error>:
  402680:	48 83 ec 58          	sub    $0x58,%rsp
  402684:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402689:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40268e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402692:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402696:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40269b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4026a0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4026a5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4026aa:	8b 54 24 18          	mov    0x18(%rsp),%edx
  4026ae:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  4026b2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4026b7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4026bc:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4026c1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  4026c6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  4026ca:	89 54 24 40          	mov    %edx,0x40(%rsp)
  4026ce:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4026d3:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4026d8:	48 39 c8             	cmp    %rcx,%rax
  4026db:	0f 92 c0             	setb   %al
  4026de:	24 01                	and    $0x1,%al
  4026e0:	3c 00                	cmp    $0x0,%al
  4026e2:	74 05                	je     4026e9 <runtime::bounds_check_error+0x69>
  4026e4:	48 83 c4 58          	add    $0x58,%rsp
  4026e8:	c3                   	ret
  4026e9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4026ee:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4026f3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4026f7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4026fb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402700:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402705:	e8 76 42 00 00       	call   406980 <runtime::bounds_check_error.handle_error-0>
  40270a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402710 <runtime::is_power_of_two_int>:
  402710:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  402715:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40271a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40271f:	48 83 f8 00          	cmp    $0x0,%rax
  402723:	0f 9e c0             	setle  %al
  402726:	24 01                	and    $0x1,%al
  402728:	3c 00                	cmp    $0x0,%al
  40272a:	74 03                	je     40272f <runtime::is_power_of_two_int+0x1f>
  40272c:	31 c0                	xor    %eax,%eax
  40272e:	c3                   	ret
  40272f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  402734:	48 89 c1             	mov    %rax,%rcx
  402737:	48 83 e9 01          	sub    $0x1,%rcx
  40273b:	48 21 c8             	and    %rcx,%rax
  40273e:	48 83 f8 00          	cmp    $0x0,%rax
  402742:	0f 94 c0             	sete   %al
  402745:	24 01                	and    $0x1,%al
  402747:	c3                   	ret
  402748:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40274f:	00 

0000000000402750 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  402750:	48 83 ec 18          	sub    $0x18,%rsp
  402754:	48 89 3c 24          	mov    %rdi,(%rsp)
  402758:	40 88 f0             	mov    %sil,%al
  40275b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40275f:	48 8b 04 24          	mov    (%rsp),%rax
  402763:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  402767:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40276c:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  402770:	48 83 f8 00          	cmp    $0x0,%rax
  402774:	0f 9e c0             	setle  %al
  402777:	24 01                	and    $0x1,%al
  402779:	3c 00                	cmp    $0x0,%al
  40277b:	74 07                	je     402784 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  40277d:	31 c0                	xor    %eax,%eax
  40277f:	48 83 c4 18          	add    $0x18,%rsp
  402783:	c3                   	ret
  402784:	8a 44 24 0e          	mov    0xe(%rsp),%al
  402788:	3c 00                	cmp    $0x0,%al
  40278a:	74 13                	je     40279f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  40278c:	48 8b 34 24          	mov    (%rsp),%rsi
  402790:	bf 01 00 00 00       	mov    $0x1,%edi
  402795:	e8 b6 e8 ff ff       	call   401050 <calloc@plt>
  40279a:	48 83 c4 18          	add    $0x18,%rsp
  40279e:	c3                   	ret
  40279f:	48 8b 3c 24          	mov    (%rsp),%rdi
  4027a3:	e8 c8 e8 ff ff       	call   401070 <malloc@plt>
  4027a8:	48 83 c4 18          	add    $0x18,%rsp
  4027ac:	c3                   	ret
  4027ad:	0f 1f 00             	nopl   (%rax)

00000000004027b0 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  4027b0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  4027b5:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  4027ba:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  4027bf:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4027c4:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4027c9:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4027ce:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4027d3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4027d8:	48 01 c2             	add    %rax,%rdx
  4027db:	0f 92 c0             	setb   %al
  4027de:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  4027e3:	24 01                	and    $0x1,%al
  4027e5:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  4027e9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4027ee:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  4027f3:	0f 94 c0             	sete   %al
  4027f6:	24 01                	and    $0x1,%al
  4027f8:	48 89 11             	mov    %rdx,(%rcx)
  4027fb:	c3                   	ret
  4027fc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402800 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  402800:	48 83 ec 28          	sub    $0x28,%rsp
  402804:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402809:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40280e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402813:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402818:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40281d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402822:	e8 59 e8 ff ff       	call   401080 <realloc@plt>
  402827:	48 83 c4 28          	add    $0x28,%rsp
  40282b:	c3                   	ret
  40282c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402830 <runtime::memory_block_alloc>:
  402830:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  402837:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  40283c:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  402841:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  402846:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40284b:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  402850:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402855:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  40285c:	00 
  40285d:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402862:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  402867:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  40286c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  402871:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  402876:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40287b:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  402880:	48 89 b4 24 f0 00 00 	mov    %rsi,0xf0(%rsp)
  402887:	00 
  402888:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  40288f:	00 
  402890:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  402897:	00 
  402898:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40289d:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  4028a4:	00 
  4028a5:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4028aa:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  4028b1:	00 
  4028b2:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4028b9:	00 
  4028ba:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  4028c1:	00 00 00 00 00 
  4028c6:	c6 84 24 d7 00 00 00 	movb   $0x0,0xd7(%rsp)
  4028cd:	00 
  4028ce:	48 89 c1             	mov    %rax,%rcx
  4028d1:	48 83 e9 31          	sub    $0x31,%rcx
  4028d5:	b9 30 00 00 00       	mov    $0x30,%ecx
  4028da:	48 0f 43 c8          	cmovae %rax,%rcx
  4028de:	48 01 ca             	add    %rcx,%rdx
  4028e1:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  4028e8:	00 
  4028e9:	48 89 8c 24 c0 00 00 	mov    %rcx,0xc0(%rsp)
  4028f0:	00 
  4028f1:	48 89 c1             	mov    %rax,%rcx
  4028f4:	48 83 e9 10          	sub    $0x10,%rcx
  4028f8:	b9 10 00 00 00       	mov    $0x10,%ecx
  4028fd:	48 0f 4c c1          	cmovl  %rcx,%rax
  402901:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  402908:	00 
  402909:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  402910:	00 
  402911:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  402918:	00 
  402919:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  402920:	00 
  402921:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  402928:	00 
  402929:	0f 57 c0             	xorps  %xmm0,%xmm0
  40292c:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402933:	00 
  402934:	48 89 e0             	mov    %rsp,%rax
  402937:	4c 89 08             	mov    %r9,(%rax)
  40293a:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  402941:	00 
  402942:	e8 f9 13 00 00       	call   403d40 <runtime::mem_alloc>
  402947:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  40294b:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  402952:	00 
  402953:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402958:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  40295f:	00 
  402960:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  402965:	3c 00                	cmp    $0x0,%al
  402967:	74 39                	je     4029a2 <runtime::memory_block_alloc+0x172>
  402969:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40296e:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  402972:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402979:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402980:	00 
  402981:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  402988:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  40298f:	00 
  402990:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402997:	48 89 11             	mov    %rdx,(%rcx)
  40299a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  4029a1:	c3                   	ret
  4029a2:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  4029a7:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4029ac:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4029b1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4029b6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4029bb:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4029c0:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4029c7:	00 
  4029c8:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4029cd:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4029d4:	00 
  4029d5:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4029da:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  4029e1:	00 
  4029e2:	48 01 f0             	add    %rsi,%rax
  4029e5:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4029ea:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4029ef:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4029f4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4029fb:	00 
  4029fc:	48 89 50 10          	mov    %rdx,0x10(%rax)
  402a00:	48 89 48 08          	mov    %rcx,0x8(%rax)
  402a04:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402a0b:	00 
  402a0c:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  402a13:	00 
  402a14:	48 03 8c 24 c0 00 00 	add    0xc0(%rsp),%rcx
  402a1b:	00 
  402a1c:	48 89 48 18          	mov    %rcx,0x18(%rax)
  402a20:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402a27:	00 
  402a28:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  402a2d:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402a34:	00 
  402a35:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  402a39:	48 29 d1             	sub    %rdx,%rcx
  402a3c:	48 89 48 28          	mov    %rcx,0x28(%rax)
  402a40:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402a47:	00 
  402a48:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  402a4d:	0f 94 c0             	sete   %al
  402a50:	24 01                	and    $0x1,%al
  402a52:	0f b6 f8             	movzbl %al,%edi
  402a55:	be 40 71 40 00       	mov    $0x407140,%esi
  402a5a:	b9 b0 71 40 00       	mov    $0x4071b0,%ecx
  402a5f:	ba 0f 00 00 00       	mov    $0xf,%edx
  402a64:	e8 67 39 00 00       	call   4063d0 <runtime::assert>
  402a69:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  402a6e:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402a75:	00 
  402a76:	48 83 38 00          	cmpq   $0x0,(%rax)
  402a7a:	0f 94 c0             	sete   %al
  402a7d:	24 01                	and    $0x1,%al
  402a7f:	0f b6 f8             	movzbl %al,%edi
  402a82:	be d8 71 40 00       	mov    $0x4071d8,%esi
  402a87:	b9 f0 71 40 00       	mov    $0x4071f0,%ecx
  402a8c:	ba 11 00 00 00       	mov    $0x11,%edx
  402a91:	e8 3a 39 00 00       	call   4063d0 <runtime::assert>
  402a96:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  402a9b:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402aa2:	00 
  402aa3:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  402aaa:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  402ab1:	00 
  402ab2:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402ab9:	48 89 11             	mov    %rdx,(%rcx)
  402abc:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402ac3:	c3                   	ret
  402ac4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402acb:	00 00 00 00 00 

0000000000402ad0 <runtime::default_temp_allocator_destroy>:
  402ad0:	48 83 ec 18          	sub    $0x18,%rsp
  402ad4:	48 89 3c 24          	mov    %rdi,(%rsp)
  402ad8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402add:	48 8b 04 24          	mov    (%rsp),%rax
  402ae1:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402ae6:	48 83 f8 00          	cmp    $0x0,%rax
  402aea:	0f 95 c0             	setne  %al
  402aed:	24 01                	and    $0x1,%al
  402aef:	3c 00                	cmp    $0x0,%al
  402af1:	74 29                	je     402b1c <runtime::default_temp_allocator_destroy+0x4c>
  402af3:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402af8:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402afd:	48 be 80 72 40 00 00 	movabs $0x407280,%rsi
  402b04:	00 00 00 
  402b07:	e8 14 1b 00 00       	call   404620 <runtime::arena_destroy>
  402b0c:	48 8b 3c 24          	mov    (%rsp),%rdi
  402b10:	31 f6                	xor    %esi,%esi
  402b12:	ba 38 00 00 00       	mov    $0x38,%edx
  402b17:	e8 24 e5 ff ff       	call   401040 <memset@plt>
  402b1c:	48 83 c4 18          	add    $0x18,%rsp
  402b20:	c3                   	ret
  402b21:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402b28:	0f 1f 84 00 00 00 00 
  402b2f:	00 

0000000000402b30 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  402b30:	48 83 ec 18          	sub    $0x18,%rsp
  402b34:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402b39:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402b3e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402b43:	e8 e8 e4 ff ff       	call   401030 <free@plt>
  402b48:	48 83 c4 18          	add    $0x18,%rsp
  402b4c:	c3                   	ret
  402b4d:	0f 1f 00             	nopl   (%rax)

0000000000402b50 <runtime::default_random_generator_proc>:
  402b50:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402b57:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402b5c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402b61:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  402b66:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  402b6b:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402b70:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402b75:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402b7a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  402b7f:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402b86:	00 
  402b87:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  402b8c:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  402b91:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  402b96:	48 83 f8 00          	cmp    $0x0,%rax
  402b9a:	0f 94 c0             	sete   %al
  402b9d:	24 01                	and    $0x1,%al
  402b9f:	3c 00                	cmp    $0x0,%al
  402ba1:	74 1a                	je     402bbd <runtime::default_random_generator_proc+0x6d>
  402ba3:	48 c7 c1 e8 ff ff ff 	mov    $0xffffffffffffffe8,%rcx
  402baa:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402bb1:	00 00 
  402bb3:	48 01 c8             	add    %rcx,%rax
  402bb6:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402bbb:	eb 0a                	jmp    402bc7 <runtime::default_random_generator_proc+0x77>
  402bbd:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402bc2:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402bc7:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402bcc:	48 85 c0             	test   %rax,%rax
  402bcf:	74 27                	je     402bf8 <runtime::default_random_generator_proc+0xa8>
  402bd1:	eb 00                	jmp    402bd3 <runtime::default_random_generator_proc+0x83>
  402bd3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402bd8:	48 83 e8 01          	sub    $0x1,%rax
  402bdc:	0f 84 17 01 00 00    	je     402cf9 <runtime::default_random_generator_proc+0x1a9>
  402be2:	eb 00                	jmp    402be4 <runtime::default_random_generator_proc+0x94>
  402be4:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402be9:	48 83 e8 02          	sub    $0x2,%rax
  402bed:	0f 84 40 01 00 00    	je     402d33 <runtime::default_random_generator_proc+0x1e3>
  402bf3:	e9 6b 01 00 00       	jmp    402d63 <runtime::default_random_generator_proc+0x213>
  402bf8:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402bfd:	48 83 38 00          	cmpq   $0x0,(%rax)
  402c01:	0f 94 c0             	sete   %al
  402c04:	24 01                	and    $0x1,%al
  402c06:	3c 00                	cmp    $0x0,%al
  402c08:	74 21                	je     402c2b <runtime::default_random_generator_proc+0xdb>
  402c0a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402c0f:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  402c14:	0f 94 c0             	sete   %al
  402c17:	24 01                	and    $0x1,%al
  402c19:	3c 00                	cmp    $0x0,%al
  402c1b:	74 0e                	je     402c2b <runtime::default_random_generator_proc+0xdb>
  402c1d:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402c22:	31 c0                	xor    %eax,%eax
  402c24:	89 c6                	mov    %eax,%esi
  402c26:	e8 15 3f 00 00       	call   406b40 <runtime::default_random_generator_proc.init-1>
  402c2b:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402c30:	48 83 e8 08          	sub    $0x8,%rax
  402c34:	75 26                	jne    402c5c <runtime::default_random_generator_proc+0x10c>
  402c36:	eb 00                	jmp    402c38 <runtime::default_random_generator_proc+0xe8>
  402c38:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402c3d:	e8 2e 3e 00 00       	call   406a70 <runtime::default_random_generator_proc.read_u64-0>
  402c42:	48 89 c1             	mov    %rax,%rcx
  402c45:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c4a:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  402c4f:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402c54:	48 89 08             	mov    %rcx,(%rax)
  402c57:	e9 9b 00 00 00       	jmp    402cf7 <runtime::default_random_generator_proc+0x1a7>
  402c5c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402c61:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  402c66:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  402c6d:	00 00 
  402c6f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402c74:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  402c7b:	ff ff 
  402c7d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402c82:	48 83 c0 01          	add    $0x1,%rax
  402c86:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402c8b:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  402c90:	7d 63                	jge    402cf5 <runtime::default_random_generator_proc+0x1a5>
  402c92:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c97:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  402c9c:	48 01 c8             	add    %rcx,%rax
  402c9f:	48 89 04 24          	mov    %rax,(%rsp)
  402ca3:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  402ca8:	0f 94 c0             	sete   %al
  402cab:	24 01                	and    $0x1,%al
  402cad:	3c 00                	cmp    $0x0,%al
  402caf:	74 14                	je     402cc5 <runtime::default_random_generator_proc+0x175>
  402cb1:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402cb6:	e8 b5 3d 00 00       	call   406a70 <runtime::default_random_generator_proc.read_u64-0>
  402cbb:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402cc0:	c6 44 24 57 07       	movb   $0x7,0x57(%rsp)
  402cc5:	48 8b 04 24          	mov    (%rsp),%rax
  402cc9:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402cce:	88 08                	mov    %cl,(%rax)
  402cd0:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402cd5:	48 c1 e9 08          	shr    $0x8,%rcx
  402cd9:	b2 01                	mov    $0x1,%dl
  402cdb:	31 c0                	xor    %eax,%eax
  402cdd:	f6 c2 01             	test   $0x1,%dl
  402ce0:	48 0f 45 c1          	cmovne %rcx,%rax
  402ce4:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402ce9:	8a 44 24 57          	mov    0x57(%rsp),%al
  402ced:	2c 01                	sub    $0x1,%al
  402cef:	88 44 24 57          	mov    %al,0x57(%rsp)
  402cf3:	eb 88                	jmp    402c7d <runtime::default_random_generator_proc+0x12d>
  402cf5:	eb 00                	jmp    402cf7 <runtime::default_random_generator_proc+0x1a7>
  402cf7:	eb 6a                	jmp    402d63 <runtime::default_random_generator_proc+0x213>
  402cf9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402cfe:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402d03:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  402d0a:	00 00 
  402d0c:	b8 08 00 00 00       	mov    $0x8,%eax
  402d11:	48 39 d0             	cmp    %rdx,%rax
  402d14:	48 0f 4c d0          	cmovl  %rax,%rdx
  402d18:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402d1d:	e8 5e 09 00 00       	call   403680 <runtime::mem_copy_non_overlapping>
  402d22:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402d27:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  402d2c:	e8 0f 3e 00 00       	call   406b40 <runtime::default_random_generator_proc.init-1>
  402d31:	eb 30                	jmp    402d63 <runtime::default_random_generator_proc+0x213>
  402d33:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402d38:	48 83 f8 04          	cmp    $0x4,%rax
  402d3c:	0f 95 c0             	setne  %al
  402d3f:	24 01                	and    $0x1,%al
  402d41:	3c 00                	cmp    $0x0,%al
  402d43:	74 08                	je     402d4d <runtime::default_random_generator_proc+0x1fd>
  402d45:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402d4c:	c3                   	ret
  402d4d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402d52:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402d57:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402d5c:	8b 08                	mov    (%rax),%ecx
  402d5e:	83 c9 0a             	or     $0xa,%ecx
  402d61:	89 08                	mov    %ecx,(%rax)
  402d63:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402d6a:	c3                   	ret
  402d6b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402d70 <runtime::slice_handle_error>:
  402d70:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402d77:	4c 89 0c 24          	mov    %r9,(%rsp)
  402d7b:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  402d80:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  402d84:	89 54 24 14          	mov    %edx,0x14(%rsp)
  402d88:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402d8d:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  402d92:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  402d99:	00 
  402d9a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402d9f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402da4:	4c 8b 04 24          	mov    (%rsp),%r8
  402da8:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  402dad:	8b 44 24 10          	mov    0x10(%rsp),%eax
  402db1:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  402db5:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402dba:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  402dbf:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402dc4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  402dcb:	00 
  402dcc:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402dd0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402dd4:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  402dd9:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  402dde:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402de3:	0f 57 c0             	xorps  %xmm0,%xmm0
  402de6:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402deb:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402df0:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402df7:	00 00 
  402df9:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402dfe:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402e03:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402e0a:	00 00 
  402e0c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  402e11:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402e16:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  402e1a:	89 44 24 44          	mov    %eax,0x44(%rsp)
  402e1e:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402e23:	e8 c8 16 00 00       	call   4044f0 <runtime::print_caller_location>
  402e28:	bf a9 72 40 00       	mov    $0x4072a9,%edi
  402e2d:	be 17 00 00 00       	mov    $0x17,%esi
  402e32:	e8 99 0e 00 00       	call   403cd0 <runtime::print_string>
  402e37:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402e3c:	e8 af 14 00 00       	call   4042f0 <runtime::print_i64>
  402e41:	bf c1 72 40 00       	mov    $0x4072c1,%edi
  402e46:	be 01 00 00 00       	mov    $0x1,%esi
  402e4b:	e8 80 0e 00 00       	call   403cd0 <runtime::print_string>
  402e50:	48 8b 3c 24          	mov    (%rsp),%rdi
  402e54:	e8 97 14 00 00       	call   4042f0 <runtime::print_i64>
  402e59:	bf c3 72 40 00       	mov    $0x4072c3,%edi
  402e5e:	be 15 00 00 00       	mov    $0x15,%esi
  402e63:	e8 68 0e 00 00       	call   403cd0 <runtime::print_string>
  402e68:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402e6d:	e8 7e 14 00 00       	call   4042f0 <runtime::print_i64>
  402e72:	bf 0a 00 00 00       	mov    $0xa,%edi
  402e77:	e8 84 10 00 00       	call   403f00 <runtime::print_byte>
  402e7c:	e8 cf ea ff ff       	call   401950 <runtime::bounds_trap>
  402e81:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402e88:	0f 1f 84 00 00 00 00 
  402e8f:	00 

0000000000402e90 <runtime::default_temp_allocator_proc>:
  402e90:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  402e97:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  402e9c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  402ea1:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402ea6:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  402eab:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  402eb0:	40 88 f0             	mov    %sil,%al
  402eb3:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  402eb7:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  402ebe:	00 
  402ebf:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402ec4:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  402ecb:	00 
  402ecc:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  402ed1:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  402ed8:	00 
  402ed9:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402ede:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  402ee3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402ee8:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  402eed:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  402ef2:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  402ef6:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  402efb:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  402f00:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  402f05:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  402f0c:	00 
  402f0d:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  402f14:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  402f1b:	00 
  402f1c:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  402f23:	00 
  402f24:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  402f2b:	00 
  402f2c:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  402f33:	00 
  402f34:	0f 57 c0             	xorps  %xmm0,%xmm0
  402f37:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402f3e:	00 
  402f3f:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  402f46:	00 
  402f47:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  402f4e:	00 
  402f4f:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  402f56:	00 
  402f57:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  402f5e:	00 
  402f5f:	48 89 e6             	mov    %rsp,%rsi
  402f62:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  402f66:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  402f6d:	00 
  402f6e:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  402f72:	4c 89 16             	mov    %r10,(%rsi)
  402f75:	0f b6 f0             	movzbl %al,%esi
  402f78:	e8 43 17 00 00       	call   4046c0 <runtime::arena_allocator_proc>
  402f7d:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402f82:	40 88 c7             	mov    %al,%dil
  402f85:	40 88 f8             	mov    %dil,%al
  402f88:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  402f8f:	00 
  402f90:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  402f97:	00 
  402f98:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402f9f:	00 
  402fa0:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402fa7:	00 
  402fa8:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  402faf:	00 
  402fb0:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402fb4:	48 89 11             	mov    %rdx,(%rcx)
  402fb7:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402fbe:	c3                   	ret
  402fbf:	90                   	nop

0000000000402fc0 <runtime::multi_pointer_slice_handle_error>:
  402fc0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402fc7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402fcc:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402fd1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402fd5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402fd9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402fde:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402fe3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402fe8:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402fed:	8b 44 24 18          	mov    0x18(%rsp),%eax
  402ff1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402ff5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402ffa:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402fff:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403004:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40300b:	00 
  40300c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  403010:	89 44 24 70          	mov    %eax,0x70(%rsp)
  403014:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  403019:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40301e:	0f 57 c0             	xorps  %xmm0,%xmm0
  403021:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403026:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40302b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403032:	00 00 
  403034:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403039:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40303e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403045:	00 00 
  403047:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40304c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403051:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  403055:	89 44 24 44          	mov    %eax,0x44(%rsp)
  403059:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40305e:	e8 8d 14 00 00       	call   4044f0 <runtime::print_caller_location>
  403063:	bf a9 72 40 00       	mov    $0x4072a9,%edi
  403068:	be 17 00 00 00       	mov    $0x17,%esi
  40306d:	e8 5e 0c 00 00       	call   403cd0 <runtime::print_string>
  403072:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403077:	e8 74 12 00 00       	call   4042f0 <runtime::print_i64>
  40307c:	bf c1 72 40 00       	mov    $0x4072c1,%edi
  403081:	be 01 00 00 00       	mov    $0x1,%esi
  403086:	e8 45 0c 00 00       	call   403cd0 <runtime::print_string>
  40308b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403090:	e8 5b 12 00 00       	call   4042f0 <runtime::print_i64>
  403095:	bf 0a 00 00 00       	mov    $0xa,%edi
  40309a:	e8 61 0e 00 00       	call   403f00 <runtime::print_byte>
  40309f:	e8 ac e8 ff ff       	call   401950 <runtime::bounds_trap>
  4030a4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4030ab:	00 00 00 00 00 

00000000004030b0 <runtime::memory_block_dealloc>:
  4030b0:	48 83 ec 38          	sub    $0x38,%rsp
  4030b4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4030b9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4030be:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4030c3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4030c8:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4030cd:	48 83 f8 00          	cmp    $0x0,%rax
  4030d1:	0f 95 c0             	setne  %al
  4030d4:	24 01                	and    $0x1,%al
  4030d6:	3c 00                	cmp    $0x0,%al
  4030d8:	74 35                	je     40310f <runtime::memory_block_dealloc+0x5f>
  4030da:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  4030df:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4030e4:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4030e9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4030ee:	48 8b 42 08          	mov    0x8(%rdx),%rax
  4030f2:	48 8b 52 10          	mov    0x10(%rdx),%rdx
  4030f6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4030fb:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403100:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403105:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40310a:	e8 f1 0f 00 00       	call   404100 <runtime::mem_free>
  40310f:	48 83 c4 38          	add    $0x38,%rsp
  403113:	c3                   	ret
  403114:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40311b:	00 00 00 00 00 

0000000000403120 <main>:
  403120:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  403127:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  40312b:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403130:	8b 44 24 14          	mov    0x14(%rsp),%eax
  403134:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403139:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  403140:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  403147:	00 
  403148:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  40314f:	00 
  403150:	48 89 0c 24          	mov    %rcx,(%rsp)
  403154:	4c 63 c8             	movslq %eax,%r9
  403157:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40315c:	bf d9 72 40 00       	mov    $0x4072d9,%edi
  403161:	31 c0                	xor    %eax,%eax
  403163:	41 89 c0             	mov    %eax,%r8d
  403166:	be 2c 00 00 00       	mov    $0x2c,%esi
  40316b:	ba 36 00 00 00       	mov    $0x36,%edx
  403170:	b9 11 00 00 00       	mov    $0x11,%ecx
  403175:	e8 c6 03 00 00       	call   403540 <runtime::multi_pointer_slice_expr_error>
  40317a:	48 8b 0c 24          	mov    (%rsp),%rcx
  40317e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403183:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  40318a:	00 
  40318b:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  403192:	00 
  403193:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  40319a:	00 
  40319b:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  4031a2:	00 
  4031a3:	48 c7 c0 60 a0 40 00 	mov    $0x40a060,%rax
  4031aa:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4031ae:	48 89 08             	mov    %rcx,(%rax)
  4031b1:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4031b8:	00 
  4031b9:	31 f6                	xor    %esi,%esi
  4031bb:	ba 70 00 00 00       	mov    $0x70,%edx
  4031c0:	e8 7b de ff ff       	call   401040 <memset@plt>
  4031c5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4031cc:	00 
  4031cd:	e8 fe 24 00 00       	call   4056d0 <runtime::[core.odin]::__init_context>
  4031d2:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  4031d7:	31 f6                	xor    %esi,%esi
  4031d9:	ba 70 00 00 00       	mov    $0x70,%edx
  4031de:	e8 5d de ff ff       	call   401040 <memset@plt>
  4031e3:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  4031e8:	e8 93 24 00 00       	call   405680 <runtime::default_context>
  4031ed:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  4031f2:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  4031f7:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  4031fc:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  403201:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  403206:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  40320b:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  403212:	00 
  403213:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  40321a:	00 
  40321b:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  403222:	00 
  403223:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  40322a:	00 
  40322b:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  403232:	00 
  403233:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  40323a:	00 
  40323b:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  403242:	00 
  403243:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40324a:	00 
  40324b:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  403252:	00 
  403253:	e8 98 e6 ff ff       	call   4018f0 <__$startup_runtime>
  403258:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40325f:	00 
  403260:	e8 2b df ff ff       	call   401190 <journey::main>
  403265:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40326c:	00 
  40326d:	e8 8e e6 ff ff       	call   401900 <__$cleanup_runtime>
  403272:	31 c0                	xor    %eax,%eax
  403274:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  40327b:	c3                   	ret
  40327c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403280 <runtime::alloc_from_memory_block>:
  403280:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  403287:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40328c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403291:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403296:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40329b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4032a0:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4032a5:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4032aa:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  4032b1:	00 
  4032b2:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  4032b9:	00 
  4032ba:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  4032c1:	00 
  4032c2:	0f 57 c0             	xorps  %xmm0,%xmm0
  4032c5:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4032cc:	00 
  4032cd:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  4032d4:	00 
  4032d5:	48 83 f8 00          	cmp    $0x0,%rax
  4032d9:	0f 94 c0             	sete   %al
  4032dc:	24 01                	and    $0x1,%al
  4032de:	3c 00                	cmp    $0x0,%al
  4032e0:	74 3e                	je     403320 <runtime::alloc_from_memory_block+0xa0>
  4032e2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4032e7:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  4032ee:	00 00 00 00 00 
  4032f3:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  4032fa:	00 00 00 00 00 
  4032ff:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  403306:	01 
  403307:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40330e:	00 
  40330f:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  403316:	b0 01                	mov    $0x1,%al
  403318:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40331f:	c3                   	ret
  403320:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403325:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40332a:	e8 b1 38 00 00       	call   406be0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  40332f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403334:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40333b:	00 
  40333c:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  403343:	00 
  403344:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40334b:	00 00 00 00 00 
  403350:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  403357:	00 
  403358:	e8 53 f4 ff ff       	call   4027b0 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  40335d:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  403364:	00 
  403365:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40336a:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  40336e:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  403373:	75 4a                	jne    4033bf <runtime::alloc_from_memory_block+0x13f>
  403375:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40337a:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  403381:	01 
  403382:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  403389:	00 
  40338a:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403391:	00 
  403392:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  403399:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4033a0:	00 
  4033a1:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4033a8:	00 
  4033a9:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  4033b0:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4033b4:	48 89 11             	mov    %rdx,(%rcx)
  4033b7:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  4033be:	c3                   	ret
  4033bf:	eb 00                	jmp    4033c1 <runtime::alloc_from_memory_block+0x141>
  4033c1:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4033c8:	00 
  4033c9:	48 8b 78 20          	mov    0x20(%rax),%rdi
  4033cd:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4033d2:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  4033d9:	00 00 
  4033db:	48 8d 54 24 60       	lea    0x60(%rsp),%rdx
  4033e0:	e8 cb f3 ff ff       	call   4027b0 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  4033e5:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4033ea:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4033ef:	88 44 24 47          	mov    %al,0x47(%rsp)
  4033f3:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  4033f8:	74 1a                	je     403414 <runtime::alloc_from_memory_block+0x194>
  4033fa:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4033ff:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403406:	00 
  403407:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  40340b:	0f 97 c0             	seta   %al
  40340e:	24 01                	and    $0x1,%al
  403410:	3c 00                	cmp    $0x0,%al
  403412:	74 4a                	je     40345e <runtime::alloc_from_memory_block+0x1de>
  403414:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403419:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  403420:	01 
  403421:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  403428:	00 
  403429:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403430:	00 
  403431:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  403438:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40343f:	00 
  403440:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403447:	00 
  403448:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  40344f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403453:	48 89 11             	mov    %rdx,(%rcx)
  403456:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40345d:	c3                   	ret
  40345e:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403463:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  40346a:	00 
  40346b:	48 8b 41 18          	mov    0x18(%rcx),%rax
  40346f:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  403473:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40347a:	00 
  40347b:	48 01 d1             	add    %rdx,%rcx
  40347e:	48 01 c8             	add    %rcx,%rax
  403481:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403486:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40348b:	48 89 04 24          	mov    %rax,(%rsp)
  40348f:	bf 50 71 40 00       	mov    $0x407150,%edi
  403494:	31 c0                	xor    %eax,%eax
  403496:	41 89 c0             	mov    %eax,%r8d
  403499:	be 3e 00 00 00       	mov    $0x3e,%esi
  40349e:	ba 55 00 00 00       	mov    $0x55,%edx
  4034a3:	b9 31 00 00 00       	mov    $0x31,%ecx
  4034a8:	e8 93 00 00 00       	call   403540 <runtime::multi_pointer_slice_expr_error>
  4034ad:	48 8b 14 24          	mov    (%rsp),%rdx
  4034b1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4034b6:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4034bb:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4034c0:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4034c5:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4034ca:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4034cf:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  4034d6:	00 
  4034d7:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4034de:	00 
  4034df:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4034e6:	00 
  4034e7:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4034ec:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4034f0:	48 01 f2             	add    %rsi,%rdx
  4034f3:	48 89 50 20          	mov    %rdx,0x20(%rax)
  4034f7:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4034fe:	00 
  4034ff:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403506:	00 
  403507:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  40350e:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  403515:	00 
  403516:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40351d:	00 
  40351e:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  403525:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403529:	48 89 11             	mov    %rdx,(%rcx)
  40352c:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  403533:	c3                   	ret
  403534:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40353b:	00 00 00 00 00 

0000000000403540 <runtime::multi_pointer_slice_expr_error>:
  403540:	48 83 ec 58          	sub    $0x58,%rsp
  403544:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403549:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40354e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403552:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403556:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40355b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403560:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403565:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40356a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40356e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403572:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403577:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40357c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403581:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403586:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40358a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40358e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403593:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403598:	48 39 c8             	cmp    %rcx,%rax
  40359b:	0f 9e c0             	setle  %al
  40359e:	24 01                	and    $0x1,%al
  4035a0:	3c 00                	cmp    $0x0,%al
  4035a2:	74 05                	je     4035a9 <runtime::multi_pointer_slice_expr_error+0x69>
  4035a4:	48 83 c4 58          	add    $0x58,%rsp
  4035a8:	c3                   	ret
  4035a9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4035ae:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4035b3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4035b7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4035bb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4035c0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4035c5:	e8 f6 f9 ff ff       	call   402fc0 <runtime::multi_pointer_slice_handle_error>
  4035ca:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004035d0 <runtime::slice_expr_error_hi>:
  4035d0:	48 83 ec 58          	sub    $0x58,%rsp
  4035d4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4035d9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4035de:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4035e2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4035e6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4035eb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4035f0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4035f5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4035fa:	8b 54 24 18          	mov    0x18(%rsp),%edx
  4035fe:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403602:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403607:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40360c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403611:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403616:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40361a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40361e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403623:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403628:	31 c0                	xor    %eax,%eax
  40362a:	48 39 c8             	cmp    %rcx,%rax
  40362d:	0f 9e c0             	setle  %al
  403630:	24 01                	and    $0x1,%al
  403632:	3c 00                	cmp    $0x0,%al
  403634:	74 1b                	je     403651 <runtime::slice_expr_error_hi+0x81>
  403636:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40363b:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403640:	48 39 c8             	cmp    %rcx,%rax
  403643:	0f 9e c0             	setle  %al
  403646:	24 01                	and    $0x1,%al
  403648:	3c 00                	cmp    $0x0,%al
  40364a:	74 05                	je     403651 <runtime::slice_expr_error_hi+0x81>
  40364c:	48 83 c4 58          	add    $0x58,%rsp
  403650:	c3                   	ret
  403651:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403656:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40365a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40365e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403663:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403668:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40366d:	48 89 e0             	mov    %rsp,%rax
  403670:	4c 89 00             	mov    %r8,(%rax)
  403673:	31 c0                	xor    %eax,%eax
  403675:	41 89 c0             	mov    %eax,%r8d
  403678:	e8 f3 f6 ff ff       	call   402d70 <runtime::slice_handle_error>
  40367d:	0f 1f 00             	nopl   (%rax)

0000000000403680 <runtime::mem_copy_non_overlapping>:
  403680:	48 83 ec 38          	sub    $0x38,%rsp
  403684:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403689:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40368e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403693:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403698:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40369d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4036a2:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4036a7:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4036ac:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4036b1:	48 83 f8 00          	cmp    $0x0,%rax
  4036b5:	0f 95 c0             	setne  %al
  4036b8:	24 01                	and    $0x1,%al
  4036ba:	3c 00                	cmp    $0x0,%al
  4036bc:	74 3c                	je     4036fa <runtime::mem_copy_non_overlapping+0x7a>
  4036be:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4036c3:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4036c8:	48 39 c8             	cmp    %rcx,%rax
  4036cb:	0f 95 c0             	setne  %al
  4036ce:	24 01                	and    $0x1,%al
  4036d0:	3c 00                	cmp    $0x0,%al
  4036d2:	74 26                	je     4036fa <runtime::mem_copy_non_overlapping+0x7a>
  4036d4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4036d9:	48 83 f8 00          	cmp    $0x0,%rax
  4036dd:	0f 9f c0             	setg   %al
  4036e0:	24 01                	and    $0x1,%al
  4036e2:	3c 00                	cmp    $0x0,%al
  4036e4:	74 14                	je     4036fa <runtime::mem_copy_non_overlapping+0x7a>
  4036e6:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4036eb:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4036f0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4036f5:	e8 66 d9 ff ff       	call   401060 <memcpy@plt>
  4036fa:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4036ff:	48 83 c4 38          	add    $0x38,%rsp
  403703:	c3                   	ret
  403704:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40370b:	00 00 00 00 00 

0000000000403710 <runtime::slice_expr_error_lo_hi>:
  403710:	48 83 ec 68          	sub    $0x68,%rsp
  403714:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403719:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40371e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403722:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403726:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40372b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403730:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403735:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40373a:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40373f:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403744:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403749:	8b 74 24 18          	mov    0x18(%rsp),%esi
  40374d:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  403751:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403756:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  40375b:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  403760:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  403765:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  403769:	89 74 24 50          	mov    %esi,0x50(%rsp)
  40376d:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  403772:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  403777:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40377c:	31 c0                	xor    %eax,%eax
  40377e:	48 39 c8             	cmp    %rcx,%rax
  403781:	0f 9e c0             	setle  %al
  403784:	24 01                	and    $0x1,%al
  403786:	3c 00                	cmp    $0x0,%al
  403788:	74 47                	je     4037d1 <runtime::slice_expr_error_lo_hi+0xc1>
  40378a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40378f:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403794:	48 39 c8             	cmp    %rcx,%rax
  403797:	0f 9e c0             	setle  %al
  40379a:	24 01                	and    $0x1,%al
  40379c:	3c 00                	cmp    $0x0,%al
  40379e:	74 31                	je     4037d1 <runtime::slice_expr_error_lo_hi+0xc1>
  4037a0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4037a5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4037aa:	48 39 c8             	cmp    %rcx,%rax
  4037ad:	0f 9e c0             	setle  %al
  4037b0:	24 01                	and    $0x1,%al
  4037b2:	3c 00                	cmp    $0x0,%al
  4037b4:	74 1b                	je     4037d1 <runtime::slice_expr_error_lo_hi+0xc1>
  4037b6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4037bb:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4037c0:	48 39 c8             	cmp    %rcx,%rax
  4037c3:	0f 9e c0             	setle  %al
  4037c6:	24 01                	and    $0x1,%al
  4037c8:	3c 00                	cmp    $0x0,%al
  4037ca:	74 05                	je     4037d1 <runtime::slice_expr_error_lo_hi+0xc1>
  4037cc:	48 83 c4 68          	add    $0x68,%rsp
  4037d0:	c3                   	ret
  4037d1:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4037d6:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4037db:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4037df:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4037e3:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4037e8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4037ed:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  4037f2:	48 89 e0             	mov    %rsp,%rax
  4037f5:	4c 89 10             	mov    %r10,(%rax)
  4037f8:	e8 73 f5 ff ff       	call   402d70 <runtime::slice_handle_error>
  4037fd:	0f 1f 00             	nopl   (%rax)

0000000000403800 <runtime::memset>:
  403800:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  403805:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  403809:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  40380e:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  403813:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  403818:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  40381c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  403821:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  403825:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40382a:	48 83 f8 00          	cmp    $0x0,%rax
  40382e:	0f 95 c0             	setne  %al
  403831:	24 01                	and    $0x1,%al
  403833:	3c 00                	cmp    $0x0,%al
  403835:	74 63                	je     40389a <runtime::memset+0x9a>
  403837:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40383c:	48 83 f8 00          	cmp    $0x0,%rax
  403840:	0f 95 c0             	setne  %al
  403843:	24 01                	and    $0x1,%al
  403845:	3c 00                	cmp    $0x0,%al
  403847:	74 51                	je     40389a <runtime::memset+0x9a>
  403849:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40384e:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  403852:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  403856:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40385b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  403862:	00 00 
  403864:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  403869:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  40386e:	0f 9c c0             	setl   %al
  403871:	24 01                	and    $0x1,%al
  403873:	3c 00                	cmp    $0x0,%al
  403875:	74 21                	je     403898 <runtime::memset+0x98>
  403877:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40387c:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  403881:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  403885:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  403888:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40388d:	48 83 c0 01          	add    $0x1,%rax
  403891:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  403896:	eb cc                	jmp    403864 <runtime::memset+0x64>
  403898:	eb 00                	jmp    40389a <runtime::memset+0x9a>
  40389a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40389f:	c3                   	ret

00000000004038a0 <runtime::arena_alloc>:
  4038a0:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  4038a7:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4038ac:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  4038b1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4038b6:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4038bb:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4038c0:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  4038c5:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4038ca:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  4038cf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4038d4:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4038d9:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  4038de:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  4038e5:	00 
  4038e6:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  4038ed:	00 
  4038ee:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4038f5:	00 
  4038f6:	0f 57 c0             	xorps  %xmm0,%xmm0
  4038f9:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  403900:	00 
  403901:	c6 84 24 0f 01 00 00 	movb   $0x0,0x10f(%rsp)
  403908:	00 
  403909:	48 89 c2             	mov    %rax,%rdx
  40390c:	48 83 ea 01          	sub    $0x1,%rdx
  403910:	48 21 d0             	and    %rdx,%rax
  403913:	48 83 f8 00          	cmp    $0x0,%rax
  403917:	0f 94 c0             	sete   %al
  40391a:	24 01                	and    $0x1,%al
  40391c:	0f b6 f8             	movzbl %al,%edi
  40391f:	be 06 73 40 00       	mov    $0x407306,%esi
  403924:	ba 1a 00 00 00       	mov    $0x1a,%edx
  403929:	e8 a2 2a 00 00       	call   4063d0 <runtime::assert>
  40392e:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403933:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  40393a:	00 
  40393b:	48 83 bc 24 00 01 00 	cmpq   $0x0,0x100(%rsp)
  403942:	00 00 
  403944:	0f 94 c0             	sete   %al
  403947:	24 01                	and    $0x1,%al
  403949:	3c 00                	cmp    $0x0,%al
  40394b:	74 42                	je     40398f <runtime::arena_alloc+0xef>
  40394d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403952:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403959:	00 
  40395a:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403961:	00 
  403962:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403969:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403970:	00 
  403971:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403978:	00 
  403979:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403980:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403984:	48 89 11             	mov    %rdx,(%rcx)
  403987:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40398e:	c3                   	ret
  40398f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403996:	00 
  403997:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  40399c:	0f 94 c0             	sete   %al
  40399f:	24 01                	and    $0x1,%al
  4039a1:	3c 00                	cmp    $0x0,%al
  4039a3:	74 09                	je     4039ae <runtime::arena_alloc+0x10e>
  4039a5:	31 c0                	xor    %eax,%eax
  4039a7:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4039ac:	eb 15                	jmp    4039c3 <runtime::arena_alloc+0x123>
  4039ae:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4039b5:	00 
  4039b6:	48 8b 40 10          	mov    0x10(%rax),%rax
  4039ba:	48 8b 40 20          	mov    0x20(%rax),%rax
  4039be:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4039c3:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  4039c8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4039cd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4039d2:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  4039d9:	00 
  4039da:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4039e1:	00 
  4039e2:	48 8b 78 10          	mov    0x10(%rax),%rdi
  4039e6:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  4039ed:	00 
  4039ee:	0f 57 c0             	xorps  %xmm0,%xmm0
  4039f1:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4039f8:	00 
  4039f9:	48 8d 8c 24 e0 00 00 	lea    0xe0(%rsp),%rcx
  403a00:	00 
  403a01:	e8 7a f8 ff ff       	call   403280 <runtime::alloc_from_memory_block>
  403a06:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  403a0d:	00 
  403a0e:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  403a15:	00 
  403a16:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  403a1d:	00 
  403a1e:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403a25:	00 
  403a26:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403a2d:	80 bc 24 0f 01 00 00 	cmpb   $0x1,0x10f(%rsp)
  403a34:	01 
  403a35:	0f 94 c0             	sete   %al
  403a38:	24 01                	and    $0x1,%al
  403a3a:	3c 00                	cmp    $0x0,%al
  403a3c:	0f 84 19 02 00 00    	je     403c5b <runtime::arena_alloc+0x3bb>
  403a42:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403a49:	00 
  403a4a:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  403a4f:	0f 94 c0             	sete   %al
  403a52:	24 01                	and    $0x1,%al
  403a54:	3c 00                	cmp    $0x0,%al
  403a56:	74 10                	je     403a68 <runtime::arena_alloc+0x1c8>
  403a58:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403a5f:	00 
  403a60:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  403a67:	00 
  403a68:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403a6d:	48 8b bc 24 00 01 00 	mov    0x100(%rsp),%rdi
  403a74:	00 
  403a75:	e8 f6 31 00 00       	call   406c70 <runtime::arena_alloc.align_forward_uint-0>
  403a7a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  403a81:	00 
  403a82:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403a89:	00 
  403a8a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403a91:	00 
  403a92:	48 8b 40 28          	mov    0x28(%rax),%rax
  403a96:	48 39 c1             	cmp    %rax,%rcx
  403a99:	48 0f 47 c1          	cmova  %rcx,%rax
  403a9d:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  403aa4:	00 
  403aa5:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403aac:	00 
  403aad:	48 83 38 00          	cmpq   $0x0,(%rax)
  403ab1:	0f 94 c0             	sete   %al
  403ab4:	24 01                	and    $0x1,%al
  403ab6:	3c 00                	cmp    $0x0,%al
  403ab8:	74 46                	je     403b00 <runtime::arena_alloc+0x260>
  403aba:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  403abf:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403ac6:	00 
  403ac7:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403acc:	e8 8f de ff ff       	call   401960 <runtime::heap_allocator>
  403ad1:	48 89 c1             	mov    %rax,%rcx
  403ad4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403ad9:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  403ae0:	00 
  403ae1:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  403ae8:	00 
  403ae9:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403af0:	00 
  403af1:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  403af8:	00 
  403af9:	48 89 50 08          	mov    %rdx,0x8(%rax)
  403afd:	48 89 08             	mov    %rcx,(%rax)
  403b00:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  403b05:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  403b0a:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  403b0f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403b16:	00 
  403b17:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  403b1e:	00 
  403b1f:	48 8b 38             	mov    (%rax),%rdi
  403b22:	48 8b 70 08          	mov    0x8(%rax),%rsi
  403b26:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  403b2d:	00 00 00 00 00 
  403b32:	48 89 e0             	mov    %rsp,%rax
  403b35:	4c 89 08             	mov    %r9,(%rax)
  403b38:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  403b3f:	00 
  403b40:	e8 eb ec ff ff       	call   402830 <runtime::memory_block_alloc>
  403b45:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403b49:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  403b50:	00 
  403b51:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  403b56:	3c 00                	cmp    $0x0,%al
  403b58:	74 4d                	je     403ba7 <runtime::arena_alloc+0x307>
  403b5a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403b5f:	8a 44 24 0f          	mov    0xf(%rsp),%al
  403b63:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403b6a:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403b71:	00 
  403b72:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403b79:	00 
  403b7a:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403b81:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403b88:	00 
  403b89:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403b90:	00 
  403b91:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403b98:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403b9c:	48 89 11             	mov    %rdx,(%rcx)
  403b9f:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403ba6:	c3                   	ret
  403ba7:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403bac:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403bb1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403bb6:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403bbd:	00 
  403bbe:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403bc5:	00 
  403bc6:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  403bcd:	00 
  403bce:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  403bd2:	48 89 08             	mov    %rcx,(%rax)
  403bd5:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403bdc:	00 
  403bdd:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403be4:	00 
  403be5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403be9:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403bf0:	00 
  403bf1:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403bf8:	00 
  403bf9:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  403bfd:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403c01:	48 01 f1             	add    %rsi,%rcx
  403c04:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403c08:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  403c0f:	00 00 00 00 00 
  403c14:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403c1b:	00 
  403c1c:	48 8b 78 10          	mov    0x10(%rax),%rdi
  403c20:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  403c27:	00 
  403c28:	0f 57 c0             	xorps  %xmm0,%xmm0
  403c2b:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  403c30:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  403c35:	e8 46 f6 ff ff       	call   403280 <runtime::alloc_from_memory_block>
  403c3a:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  403c3f:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  403c44:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  403c4b:	00 
  403c4c:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403c53:	00 
  403c54:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403c5b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403c60:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403c67:	00 
  403c68:	48 8b 70 10          	mov    0x10(%rax),%rsi
  403c6c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  403c70:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  403c74:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  403c7b:	00 
  403c7c:	48 29 fe             	sub    %rdi,%rsi
  403c7f:	48 01 f2             	add    %rsi,%rdx
  403c82:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403c86:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403c8d:	00 
  403c8e:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403c95:	00 
  403c96:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403c9d:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403ca4:	00 
  403ca5:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403cac:	00 
  403cad:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403cb4:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403cb8:	48 89 11             	mov    %rdx,(%rcx)
  403cbb:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403cc2:	c3                   	ret
  403cc3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403cca:	84 00 00 00 00 00 

0000000000403cd0 <runtime::print_string>:
  403cd0:	48 83 ec 58          	sub    $0x58,%rsp
  403cd4:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403cd9:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403cde:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403ce3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403ce8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403ced:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  403cf2:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  403cf9:	00 00 
  403cfb:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403d00:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403d05:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403d0a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403d0f:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403d16:	00 00 
  403d18:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403d1d:	e8 0e e6 ff ff       	call   402330 <runtime::stderr_write>
  403d22:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403d27:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403d2c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403d31:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403d36:	48 83 c4 58          	add    $0x58,%rsp
  403d3a:	c3                   	ret
  403d3b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403d40 <runtime::mem_alloc>:
  403d40:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  403d47:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  403d4c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  403d51:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403d56:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403d5b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403d60:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  403d65:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403d6c:	00 
  403d6d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403d72:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403d77:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403d7c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d81:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403d86:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403d8d:	00 
  403d8e:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  403d95:	00 
  403d96:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  403d9d:	00 
  403d9e:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403da5:	00 
  403da6:	e8 65 e9 ff ff       	call   402710 <runtime::is_power_of_two_int>
  403dab:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403db0:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403db5:	0f b6 f8             	movzbl %al,%edi
  403db8:	be 21 73 40 00       	mov    $0x407321,%esi
  403dbd:	ba 20 00 00 00       	mov    $0x20,%edx
  403dc2:	e8 09 26 00 00       	call   4063d0 <runtime::assert>
  403dc7:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403dcc:	48 83 f8 00          	cmp    $0x0,%rax
  403dd0:	0f 94 c0             	sete   %al
  403dd3:	24 01                	and    $0x1,%al
  403dd5:	3c 00                	cmp    $0x0,%al
  403dd7:	75 12                	jne    403deb <runtime::mem_alloc+0xab>
  403dd9:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  403de0:	00 00 
  403de2:	0f 94 c0             	sete   %al
  403de5:	24 01                	and    $0x1,%al
  403de7:	3c 00                	cmp    $0x0,%al
  403de9:	74 1e                	je     403e09 <runtime::mem_alloc+0xc9>
  403deb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403df0:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  403df7:	00 
  403df8:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  403dff:	31 c0                	xor    %eax,%eax
  403e01:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403e08:	c3                   	ret
  403e09:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403e0e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403e13:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403e18:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  403e1d:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403e24:	00 
  403e25:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  403e2c:	00 
  403e2d:	0f 57 c0             	xorps  %xmm0,%xmm0
  403e30:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  403e35:	48 89 e6             	mov    %rsp,%rsi
  403e38:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  403e3c:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  403e41:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  403e45:	4c 89 06             	mov    %r8,(%rsi)
  403e48:	31 f6                	xor    %esi,%esi
  403e4a:	41 89 f1             	mov    %esi,%r9d
  403e4d:	4d 89 c8             	mov    %r9,%r8
  403e50:	ff d0                	call   *%rax
  403e52:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403e57:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  403e5c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  403e61:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403e65:	48 89 11             	mov    %rdx,(%rcx)
  403e68:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403e6f:	c3                   	ret

0000000000403e70 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  403e70:	48 83 ec 48          	sub    $0x48,%rsp
  403e74:	48 89 0c 24          	mov    %rcx,(%rsp)
  403e78:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  403e7d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403e82:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403e87:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  403e8c:	48 8b 04 24          	mov    (%rsp),%rax
  403e90:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403e95:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403e9a:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403e9f:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403ea4:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403ea9:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  403eae:	48 39 c1             	cmp    %rax,%rcx
  403eb1:	48 0f 4c c1          	cmovl  %rcx,%rax
  403eb5:	31 c9                	xor    %ecx,%ecx
  403eb7:	48 39 c1             	cmp    %rax,%rcx
  403eba:	48 0f 4f c1          	cmovg  %rcx,%rax
  403ebe:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403ec3:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  403ec9:	0f 9f c0             	setg   %al
  403ecc:	24 01                	and    $0x1,%al
  403ece:	3c 00                	cmp    $0x0,%al
  403ed0:	74 18                	je     403eea <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)+0x7a>
  403ed2:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403ed7:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403edc:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  403ee1:	48 c1 e2 00          	shl    $0x0,%rdx
  403ee5:	e8 a6 d1 ff ff       	call   401090 <memmove@plt>
  403eea:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403eef:	48 83 c4 48          	add    $0x48,%rsp
  403ef3:	c3                   	ret
  403ef4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403efb:	00 00 00 00 00 

0000000000403f00 <runtime::print_byte>:
  403f00:	48 83 ec 68          	sub    $0x68,%rsp
  403f04:	40 88 f8             	mov    %dil,%al
  403f07:	88 44 24 07          	mov    %al,0x7(%rsp)
  403f0b:	8a 54 24 07          	mov    0x7(%rsp),%dl
  403f0f:	88 54 24 67          	mov    %dl,0x67(%rsp)
  403f13:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  403f1a:	00 00 
  403f1c:	0f 57 c0             	xorps  %xmm0,%xmm0
  403f1f:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403f24:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  403f29:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  403f2e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403f33:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  403f3a:	00 00 
  403f3c:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403f41:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403f46:	88 11                	mov    %dl,(%rcx)
  403f48:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403f4d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403f52:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  403f57:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403f5c:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  403f63:	00 00 
  403f65:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  403f6a:	e8 c1 e3 ff ff       	call   402330 <runtime::stderr_write>
  403f6f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403f74:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403f79:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  403f7e:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403f83:	48 83 c4 68          	add    $0x68,%rsp
  403f87:	c3                   	ret
  403f88:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  403f8f:	00 

0000000000403f90 <runtime::matrix_bounds_check_error>:
  403f90:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403f97:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  403f9c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403fa1:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  403fa5:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  403fa9:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403fae:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403fb3:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  403fba:	00 
  403fbb:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403fc0:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403fc7:	00 
  403fc8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403fcd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403fd2:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403fd7:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  403fdc:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403fe1:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  403fe5:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  403fea:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  403fef:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  403ff4:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  403ff9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  404000:	00 
  404001:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  404006:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  40400a:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40400f:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  404014:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  404019:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40401e:	48 39 c8             	cmp    %rcx,%rax
  404021:	0f 92 c0             	setb   %al
  404024:	24 01                	and    $0x1,%al
  404026:	3c 00                	cmp    $0x0,%al
  404028:	74 1e                	je     404048 <runtime::matrix_bounds_check_error+0xb8>
  40402a:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40402f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  404034:	48 39 c8             	cmp    %rcx,%rax
  404037:	0f 92 c0             	setb   %al
  40403a:	24 01                	and    $0x1,%al
  40403c:	3c 00                	cmp    $0x0,%al
  40403e:	74 08                	je     404048 <runtime::matrix_bounds_check_error+0xb8>
  404040:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  404047:	c3                   	ret
  404048:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40404d:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  404052:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  404056:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  40405a:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40405f:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  404064:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  404069:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  40406e:	48 89 e0             	mov    %rsp,%rax
  404071:	4c 89 58 08          	mov    %r11,0x8(%rax)
  404075:	4c 89 10             	mov    %r10,(%rax)
  404078:	e8 53 2c 00 00       	call   406cd0 <runtime::matrix_bounds_check_error.handle_error-0>
  40407d:	0f 1f 00             	nopl   (%rax)

0000000000404080 <runtime::heap_alloc>:
  404080:	48 83 ec 18          	sub    $0x18,%rsp
  404084:	48 89 3c 24          	mov    %rdi,(%rsp)
  404088:	40 88 f0             	mov    %sil,%al
  40408b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40408f:	8a 44 24 0e          	mov    0xe(%rsp),%al
  404093:	48 8b 3c 24          	mov    (%rsp),%rdi
  404097:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40409c:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4040a0:	0f b6 f0             	movzbl %al,%esi
  4040a3:	e8 a8 e6 ff ff       	call   402750 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  4040a8:	48 83 c4 18          	add    $0x18,%rsp
  4040ac:	c3                   	ret
  4040ad:	0f 1f 00             	nopl   (%rax)

00000000004040b0 <runtime::heap_resize>:
  4040b0:	48 83 ec 28          	sub    $0x28,%rsp
  4040b4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4040b9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4040be:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4040c3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4040c8:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4040cd:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4040d2:	e8 29 e7 ff ff       	call   402800 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  4040d7:	48 83 c4 28          	add    $0x28,%rsp
  4040db:	c3                   	ret
  4040dc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004040e0 <runtime::heap_free>:
  4040e0:	48 83 ec 18          	sub    $0x18,%rsp
  4040e4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4040e9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4040ee:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4040f3:	e8 38 ea ff ff       	call   402b30 <runtime::[heap_allocator_unix.odin]::_heap_free>
  4040f8:	48 83 c4 18          	add    $0x18,%rsp
  4040fc:	c3                   	ret
  4040fd:	0f 1f 00             	nopl   (%rax)

0000000000404100 <runtime::mem_free>:
  404100:	53                   	push   %rbx
  404101:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  404108:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40410d:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  404112:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  404117:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40411c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  404121:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  404126:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40412b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  404130:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  404137:	00 
  404138:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40413d:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  404142:	48 83 f8 00          	cmp    $0x0,%rax
  404146:	0f 94 c0             	sete   %al
  404149:	24 01                	and    $0x1,%al
  40414b:	3c 00                	cmp    $0x0,%al
  40414d:	75 0f                	jne    40415e <runtime::mem_free+0x5e>
  40414f:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  404155:	0f 94 c0             	sete   %al
  404158:	24 01                	and    $0x1,%al
  40415a:	3c 00                	cmp    $0x0,%al
  40415c:	74 0b                	je     404169 <runtime::mem_free+0x69>
  40415e:	31 c0                	xor    %eax,%eax
  404160:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  404167:	5b                   	pop    %rbx
  404168:	c3                   	ret
  404169:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  40416e:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  404173:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  404178:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40417d:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  404182:	0f 57 c0             	xorps  %xmm0,%xmm0
  404185:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  40418a:	be 01 00 00 00       	mov    $0x1,%esi
  40418f:	31 c9                	xor    %ecx,%ecx
  404191:	41 89 c9             	mov    %ecx,%r9d
  404194:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  404199:	4c 89 ca             	mov    %r9,%rdx
  40419c:	4c 89 c9             	mov    %r9,%rcx
  40419f:	48 89 1c 24          	mov    %rbx,(%rsp)
  4041a3:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  4041a8:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  4041ad:	ff d0                	call   *%rax
  4041af:	88 44 24 47          	mov    %al,0x47(%rsp)
  4041b3:	8a 44 24 47          	mov    0x47(%rsp),%al
  4041b7:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  4041be:	5b                   	pop    %rbx
  4041bf:	c3                   	ret

00000000004041c0 <runtime::print_u64>:
  4041c0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4041c7:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4041cc:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4041d1:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4041d8:	00 
  4041d9:	48 8d 7c 24 5f       	lea    0x5f(%rsp),%rdi
  4041de:	31 f6                	xor    %esi,%esi
  4041e0:	ba 81 00 00 00       	mov    $0x81,%edx
  4041e5:	e8 56 ce ff ff       	call   401040 <memset@plt>
  4041ea:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4041ef:	48 c7 44 24 50 81 00 	movq   $0x81,0x50(%rsp)
  4041f6:	00 00 
  4041f8:	48 c7 44 24 48 0a 00 	movq   $0xa,0x48(%rsp)
  4041ff:	00 00 
  404201:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404206:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40420b:	48 3b 44 24 48       	cmp    0x48(%rsp),%rax
  404210:	0f 93 c0             	setae  %al
  404213:	24 01                	and    $0x1,%al
  404215:	3c 00                	cmp    $0x0,%al
  404217:	74 50                	je     404269 <runtime::print_u64+0xa9>
  404219:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40421e:	48 83 e8 01          	sub    $0x1,%rax
  404222:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404227:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40422c:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404231:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  404238:	48 8b 08             	mov    (%rax),%rcx
  40423b:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404240:	31 d2                	xor    %edx,%edx
  404242:	48 f7 74 24 48       	divq   0x48(%rsp)
  404247:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40424c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  40424f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  404253:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  404258:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40425d:	31 d2                	xor    %edx,%edx
  40425f:	48 f7 f1             	div    %rcx
  404262:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404267:	eb 9d                	jmp    404206 <runtime::print_u64+0x46>
  404269:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40426e:	48 ff c8             	dec    %rax
  404271:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404276:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40427b:	48 89 04 24          	mov    %rax,(%rsp)
  40427f:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  404286:	48 8b 08             	mov    (%rax),%rcx
  404289:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40428e:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404293:	31 d2                	xor    %edx,%edx
  404295:	48 f7 f6             	div    %rsi
  404298:	48 8b 04 24          	mov    (%rsp),%rax
  40429c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  40429f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  4042a3:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4042a8:	48 8d 4c 14 5f       	lea    0x5f(%rsp,%rdx,1),%rcx
  4042ad:	b8 81 00 00 00       	mov    $0x81,%eax
  4042b2:	48 29 d0             	sub    %rdx,%rax
  4042b5:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4042ba:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4042bf:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4042c4:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4042c9:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4042d0:	00 00 
  4042d2:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  4042d7:	e8 54 e0 ff ff       	call   402330 <runtime::stderr_write>
  4042dc:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4042e3:	c3                   	ret
  4042e4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4042eb:	00 00 00 00 00 

00000000004042f0 <runtime::print_i64>:
  4042f0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4042f7:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4042fc:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  404301:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404308:	00 
  404309:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404310:	00 
  404311:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  404318:	00 00 
  40431a:	0f 9c c0             	setl   %al
  40431d:	24 01                	and    $0x1,%al
  40431f:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  404326:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40432d:	00 
  40432e:	31 c9                	xor    %ecx,%ecx
  404330:	48 29 c1             	sub    %rax,%rcx
  404333:	48 83 f8 00          	cmp    $0x0,%rax
  404337:	48 0f 4c c1          	cmovl  %rcx,%rax
  40433b:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404342:	00 
  404343:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  404348:	31 f6                	xor    %esi,%esi
  40434a:	ba 81 00 00 00       	mov    $0x81,%edx
  40434f:	e8 ec cc ff ff       	call   401040 <memset@plt>
  404354:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  40435b:	00 00 
  40435d:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  404364:	00 0a 
  404366:	0f 9d c0             	setge  %al
  404369:	24 01                	and    $0x1,%al
  40436b:	3c 00                	cmp    $0x0,%al
  40436d:	74 5c                	je     4043cb <runtime::print_i64+0xdb>
  40436f:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404374:	48 83 e8 01          	sub    $0x1,%rax
  404378:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40437d:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404382:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  404387:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  40438e:	48 8b 08             	mov    (%rax),%rcx
  404391:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404398:	00 
  404399:	be 0a 00 00 00       	mov    $0xa,%esi
  40439e:	48 99                	cqto
  4043a0:	48 f7 fe             	idiv   %rsi
  4043a3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4043a8:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  4043ab:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  4043af:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4043b6:	00 
  4043b7:	b9 0a 00 00 00       	mov    $0xa,%ecx
  4043bc:	48 99                	cqto
  4043be:	48 f7 f9             	idiv   %rcx
  4043c1:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4043c8:	00 
  4043c9:	eb 92                	jmp    40435d <runtime::print_i64+0x6d>
  4043cb:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4043d0:	48 83 e8 01          	sub    $0x1,%rax
  4043d4:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4043d9:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4043de:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4043e3:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  4043ea:	48 8b 08             	mov    (%rax),%rcx
  4043ed:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4043f4:	00 
  4043f5:	be 0a 00 00 00       	mov    $0xa,%esi
  4043fa:	48 99                	cqto
  4043fc:	48 f7 fe             	idiv   %rsi
  4043ff:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404404:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  404407:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  40440b:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  404412:	00 
  404413:	74 18                	je     40442d <runtime::print_i64+0x13d>
  404415:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40441a:	48 83 e8 01          	sub    $0x1,%rax
  40441e:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404423:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404428:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  40442d:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  404432:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  404437:	b8 81 00 00 00       	mov    $0x81,%eax
  40443c:	48 29 d0             	sub    %rdx,%rax
  40443f:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  404444:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404449:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40444e:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  404453:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40445a:	00 00 
  40445c:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  404461:	e8 ca de ff ff       	call   402330 <runtime::stderr_write>
  404466:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40446d:	c3                   	ret
  40446e:	66 90                	xchg   %ax,%ax

0000000000404470 <runtime::arena_free_last_memory_block>:
  404470:	48 83 ec 28          	sub    $0x28,%rsp
  404474:	48 89 3c 24          	mov    %rdi,(%rsp)
  404478:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40447d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404482:	48 8b 04 24          	mov    (%rsp),%rax
  404486:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40448b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404490:	48 8b 40 10          	mov    0x10(%rax),%rax
  404494:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404499:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  40449f:	0f 95 c0             	setne  %al
  4044a2:	24 01                	and    $0x1,%al
  4044a4:	3c 00                	cmp    $0x0,%al
  4044a6:	74 3e                	je     4044e6 <runtime::arena_free_last_memory_block+0x76>
  4044a8:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4044ad:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4044b2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4044b7:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4044bc:	48 8b 09             	mov    (%rcx),%rcx
  4044bf:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4044c3:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4044c8:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4044cd:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  4044d1:	48 8b 48 20          	mov    0x20(%rax),%rcx
  4044d5:	48 29 f9             	sub    %rdi,%rcx
  4044d8:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4044dc:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4044e1:	e8 ca eb ff ff       	call   4030b0 <runtime::memory_block_dealloc>
  4044e6:	48 83 c4 28          	add    $0x28,%rsp
  4044ea:	c3                   	ret
  4044eb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004044f0 <runtime::print_caller_location>:
  4044f0:	50                   	push   %rax
  4044f1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4044f5:	eb 00                	jmp    4044f7 <runtime::print_caller_location+0x7>
  4044f7:	48 8b 04 24          	mov    (%rsp),%rax
  4044fb:	48 8b 38             	mov    (%rax),%rdi
  4044fe:	48 8b 70 08          	mov    0x8(%rax),%rsi
  404502:	e8 c9 f7 ff ff       	call   403cd0 <runtime::print_string>
  404507:	bf 28 00 00 00       	mov    $0x28,%edi
  40450c:	e8 ef f9 ff ff       	call   403f00 <runtime::print_byte>
  404511:	48 8b 04 24          	mov    (%rsp),%rax
  404515:	48 63 78 10          	movslq 0x10(%rax),%rdi
  404519:	e8 a2 fc ff ff       	call   4041c0 <runtime::print_u64>
  40451e:	48 8b 04 24          	mov    (%rsp),%rax
  404522:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  404526:	0f 95 c0             	setne  %al
  404529:	24 01                	and    $0x1,%al
  40452b:	3c 00                	cmp    $0x0,%al
  40452d:	74 17                	je     404546 <runtime::print_caller_location+0x56>
  40452f:	bf 3a 00 00 00       	mov    $0x3a,%edi
  404534:	e8 c7 f9 ff ff       	call   403f00 <runtime::print_byte>
  404539:	48 8b 04 24          	mov    (%rsp),%rax
  40453d:	48 63 78 14          	movslq 0x14(%rax),%rdi
  404541:	e8 7a fc ff ff       	call   4041c0 <runtime::print_u64>
  404546:	bf 29 00 00 00       	mov    $0x29,%edi
  40454b:	e8 b0 f9 ff ff       	call   403f00 <runtime::print_byte>
  404550:	58                   	pop    %rax
  404551:	c3                   	ret
  404552:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404559:	1f 84 00 00 00 00 00 

0000000000404560 <runtime::arena_free_all>:
  404560:	48 83 ec 28          	sub    $0x28,%rsp
  404564:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404569:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40456e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  404573:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404578:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40457d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404582:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404587:	0f 95 c0             	setne  %al
  40458a:	24 01                	and    $0x1,%al
  40458c:	3c 00                	cmp    $0x0,%al
  40458e:	74 2c                	je     4045bc <runtime::arena_free_all+0x5c>
  404590:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404595:	48 8b 40 10          	mov    0x10(%rax),%rax
  404599:	48 83 38 00          	cmpq   $0x0,(%rax)
  40459d:	0f 95 c0             	setne  %al
  4045a0:	24 01                	and    $0x1,%al
  4045a2:	3c 00                	cmp    $0x0,%al
  4045a4:	74 16                	je     4045bc <runtime::arena_free_all+0x5c>
  4045a6:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4045ab:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4045b0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4045b5:	e8 b6 fe ff ff       	call   404470 <runtime::arena_free_last_memory_block>
  4045ba:	eb c1                	jmp    40457d <runtime::arena_free_all+0x1d>
  4045bc:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4045c1:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  4045c6:	0f 95 c0             	setne  %al
  4045c9:	24 01                	and    $0x1,%al
  4045cb:	3c 00                	cmp    $0x0,%al
  4045cd:	74 32                	je     404601 <runtime::arena_free_all+0xa1>
  4045cf:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4045d4:	48 8b 40 10          	mov    0x10(%rax),%rax
  4045d8:	48 8b 78 18          	mov    0x18(%rax),%rdi
  4045dc:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4045e1:	48 8b 40 10          	mov    0x10(%rax),%rax
  4045e5:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4045e9:	31 f6                	xor    %esi,%esi
  4045eb:	e8 50 ca ff ff       	call   401040 <memset@plt>
  4045f0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4045f5:	48 8b 40 10          	mov    0x10(%rax),%rax
  4045f9:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  404600:	00 
  404601:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404606:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  40460d:	00 
  40460e:	48 83 c4 28          	add    $0x28,%rsp
  404612:	c3                   	ret
  404613:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40461a:	84 00 00 00 00 00 

0000000000404620 <runtime::arena_destroy>:
  404620:	48 83 ec 28          	sub    $0x28,%rsp
  404624:	48 89 3c 24          	mov    %rdi,(%rsp)
  404628:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40462d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404632:	48 8b 04 24          	mov    (%rsp),%rax
  404636:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40463b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404640:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404645:	0f 95 c0             	setne  %al
  404648:	24 01                	and    $0x1,%al
  40464a:	3c 00                	cmp    $0x0,%al
  40464c:	74 4e                	je     40469c <runtime::arena_destroy+0x7c>
  40464e:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  404653:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404658:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40465d:	48 8b 40 10          	mov    0x10(%rax),%rax
  404661:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404666:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40466b:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404670:	48 8b 09             	mov    (%rcx),%rcx
  404673:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404677:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40467c:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404681:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  404685:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404689:	48 29 f9             	sub    %rdi,%rcx
  40468c:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404690:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  404695:	e8 16 ea ff ff       	call   4030b0 <runtime::memory_block_dealloc>
  40469a:	eb 9f                	jmp    40463b <runtime::arena_destroy+0x1b>
  40469c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4046a1:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  4046a8:	00 
  4046a9:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4046ae:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  4046b5:	00 
  4046b6:	48 83 c4 28          	add    $0x28,%rsp
  4046ba:	c3                   	ret
  4046bb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004046c0 <runtime::arena_allocator_proc>:
  4046c0:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  4046c7:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  4046cc:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  4046d3:	00 
  4046d4:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  4046db:	00 
  4046dc:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  4046e3:	00 
  4046e4:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  4046eb:	00 
  4046ec:	40 88 f0             	mov    %sil,%al
  4046ef:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  4046f6:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  4046fd:	00 
  4046fe:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  404705:	00 
  404706:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  40470d:	00 
  40470e:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  404715:	00 
  404716:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  40471d:	00 
  40471e:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  404725:	00 
  404726:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  40472d:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  404732:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  404739:	00 
  40473a:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404741:	00 
  404742:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  404749:	00 
  40474a:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  404751:	00 
  404752:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  404759:	00 
  40475a:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  404761:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  404768:	00 
  404769:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  404770:	00 
  404771:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  404778:	00 
  404779:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  404780:	00 
  404781:	0f 57 c0             	xorps  %xmm0,%xmm0
  404784:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  40478b:	00 
  40478c:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404793:	00 
  404794:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  40479b:	00 
  40479c:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  4047a3:	00 
  4047a4:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  4047ab:	00 
  4047ac:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  4047b3:	00 
  4047b4:	0f b6 c8             	movzbl %al,%ecx
  4047b7:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4047bc:	2c 07                	sub    $0x7,%al
  4047be:	0f 87 9a 07 00 00    	ja     404f5e <runtime::arena_allocator_proc+0x89e>
  4047c4:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4047c9:	48 8b 04 c5 00 71 40 	mov    0x407100(,%rax,8),%rax
  4047d0:	00 
  4047d1:	ff e0                	jmp    *%rax
  4047d3:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  4047da:	00 
  4047db:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4047e2:	00 
  4047e3:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4047ea:	00 
  4047eb:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4047f2:	00 
  4047f3:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4047fa:	00 
  4047fb:	0f 57 c0             	xorps  %xmm0,%xmm0
  4047fe:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  404805:	00 
  404806:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  40480d:	00 
  40480e:	e8 8d f0 ff ff       	call   4038a0 <runtime::arena_alloc>
  404813:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40481a:	00 
  40481b:	40 88 c7             	mov    %al,%dil
  40481e:	40 88 f8             	mov    %dil,%al
  404821:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  404828:	00 
  404829:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  404830:	00 
  404831:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404838:	00 
  404839:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404840:	00 
  404841:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404848:	00 
  404849:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40484d:	48 89 11             	mov    %rdx,(%rcx)
  404850:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404857:	c3                   	ret
  404858:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  40485f:	04 
  404860:	e9 f9 06 00 00       	jmp    404f5e <runtime::arena_allocator_proc+0x89e>
  404865:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  40486c:	00 
  40486d:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  404874:	00 
  404875:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  40487c:	00 
  40487d:	e8 de fc ff ff       	call   404560 <runtime::arena_free_all>
  404882:	e9 d7 06 00 00       	jmp    404f5e <runtime::arena_allocator_proc+0x89e>
  404887:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40488e:	00 
  40488f:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  404896:	00 
  404897:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  40489e:	00 00 
  4048a0:	0f 94 c1             	sete   %cl
  4048a3:	80 e1 01             	and    $0x1,%cl
  4048a6:	b0 01                	mov    $0x1,%al
  4048a8:	38 c8                	cmp    %cl,%al
  4048aa:	74 25                	je     4048d1 <runtime::arena_allocator_proc+0x211>
  4048ac:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  4048b3:	00 
  4048b4:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  4048bb:	00 
  4048bc:	0f 94 c1             	sete   %cl
  4048bf:	80 e1 01             	and    $0x1,%cl
  4048c2:	b0 01                	mov    $0x1,%al
  4048c4:	38 c8                	cmp    %cl,%al
  4048c6:	0f 84 a8 00 00 00    	je     404974 <runtime::arena_allocator_proc+0x2b4>
  4048cc:	e9 85 00 00 00       	jmp    404956 <runtime::arena_allocator_proc+0x296>
  4048d1:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  4048d8:	00 
  4048d9:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4048e0:	00 
  4048e1:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4048e8:	00 
  4048e9:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4048f0:	00 
  4048f1:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4048f8:	00 
  4048f9:	0f 57 c0             	xorps  %xmm0,%xmm0
  4048fc:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  404903:	00 
  404904:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  40490b:	00 
  40490c:	e8 8f ef ff ff       	call   4038a0 <runtime::arena_alloc>
  404911:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404918:	00 
  404919:	40 88 c7             	mov    %al,%dil
  40491c:	40 88 f8             	mov    %dil,%al
  40491f:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  404926:	00 
  404927:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  40492e:	00 
  40492f:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404936:	00 
  404937:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40493e:	00 
  40493f:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404946:	00 
  404947:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40494b:	48 89 11             	mov    %rdx,(%rcx)
  40494e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404955:	c3                   	ret
  404956:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  40495d:	00 00 
  40495f:	0f 94 c1             	sete   %cl
  404962:	80 e1 01             	and    $0x1,%cl
  404965:	b0 01                	mov    $0x1,%al
  404967:	38 c8                	cmp    %cl,%al
  404969:	0f 84 e5 00 00 00    	je     404a54 <runtime::arena_allocator_proc+0x394>
  40496f:	e9 b7 00 00 00       	jmp    404a2b <runtime::arena_allocator_proc+0x36b>
  404974:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40497b:	00 
  40497c:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  404981:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404988:	00 
  404989:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  40498e:	bf 50 71 40 00       	mov    $0x407150,%edi
  404993:	31 c0                	xor    %eax,%eax
  404995:	41 89 c0             	mov    %eax,%r8d
  404998:	be 3e 00 00 00       	mov    $0x3e,%esi
  40499d:	ba d1 00 00 00       	mov    $0xd1,%edx
  4049a2:	b9 13 00 00 00       	mov    $0x13,%ecx
  4049a7:	e8 94 eb ff ff       	call   403540 <runtime::multi_pointer_slice_expr_error>
  4049ac:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4049b1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4049b6:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4049bd:	00 
  4049be:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  4049c5:	00 
  4049c6:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  4049cd:	00 
  4049ce:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  4049d5:	00 
  4049d6:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  4049dd:	00 
  4049de:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4049e5:	00 
  4049e6:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4049ed:	00 
  4049ee:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4049f5:	00 
  4049f6:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4049fd:	00 
  4049fe:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404a05:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404a0c:	00 
  404a0d:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404a14:	00 
  404a15:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404a1c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404a20:	48 89 11             	mov    %rdx,(%rcx)
  404a23:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404a2a:	c3                   	ret
  404a2b:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404a32:	00 
  404a33:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  404a3a:	00 
  404a3b:	48 83 e9 01          	sub    $0x1,%rcx
  404a3f:	48 21 c8             	and    %rcx,%rax
  404a42:	48 83 f8 00          	cmp    $0x0,%rax
  404a46:	0f 94 c1             	sete   %cl
  404a49:	80 e1 01             	and    $0x1,%cl
  404a4c:	b0 01                	mov    $0x1,%al
  404a4e:	38 c8                	cmp    %cl,%al
  404a50:	74 54                	je     404aa6 <runtime::arena_allocator_proc+0x3e6>
  404a52:	eb 4d                	jmp    404aa1 <runtime::arena_allocator_proc+0x3e1>
  404a54:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404a5b:	00 
  404a5c:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404a63:	04 
  404a64:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404a6b:	00 
  404a6c:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404a73:	00 
  404a74:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404a7b:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404a82:	00 
  404a83:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404a8a:	00 
  404a8b:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404a92:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404a96:	48 89 11             	mov    %rdx,(%rcx)
  404a99:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404aa0:	c3                   	ret
  404aa1:	e9 94 02 00 00       	jmp    404d3a <runtime::arena_allocator_proc+0x67a>
  404aa6:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  404aad:	00 
  404aae:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  404ab5:	00 
  404ab6:	0f 92 c0             	setb   %al
  404ab9:	24 01                	and    $0x1,%al
  404abb:	3c 00                	cmp    $0x0,%al
  404abd:	0f 84 b7 00 00 00    	je     404b7a <runtime::arena_allocator_proc+0x4ba>
  404ac3:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404aca:	00 
  404acb:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404ad0:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404ad7:	00 
  404ad8:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  404add:	bf 50 71 40 00       	mov    $0x407150,%edi
  404ae2:	31 c0                	xor    %eax,%eax
  404ae4:	41 89 c0             	mov    %eax,%r8d
  404ae7:	be 3e 00 00 00       	mov    $0x3e,%esi
  404aec:	ba d9 00 00 00       	mov    $0xd9,%edx
  404af1:	b9 14 00 00 00       	mov    $0x14,%ecx
  404af6:	e8 45 ea ff ff       	call   403540 <runtime::multi_pointer_slice_expr_error>
  404afb:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  404b00:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  404b05:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404b0c:	00 
  404b0d:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  404b14:	00 
  404b15:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  404b1c:	00 
  404b1d:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  404b24:	00 
  404b25:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  404b2c:	00 
  404b2d:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404b34:	00 
  404b35:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404b3c:	00 
  404b3d:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404b44:	00 
  404b45:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404b4c:	00 
  404b4d:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404b54:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404b5b:	00 
  404b5c:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404b63:	00 
  404b64:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404b6b:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404b6f:	48 89 11             	mov    %rdx,(%rcx)
  404b72:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404b79:	c3                   	ret
  404b7a:	eb 00                	jmp    404b7c <runtime::arena_allocator_proc+0x4bc>
  404b7c:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  404b83:	00 
  404b84:	48 8b 40 10          	mov    0x10(%rax),%rax
  404b88:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  404b8f:	00 
  404b90:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  404b97:	00 00 
  404b99:	0f 95 c0             	setne  %al
  404b9c:	24 01                	and    $0x1,%al
  404b9e:	3c 00                	cmp    $0x0,%al
  404ba0:	0f 84 92 01 00 00    	je     404d38 <runtime::arena_allocator_proc+0x678>
  404ba6:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404bad:	00 
  404bae:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404bb5:	00 
  404bb6:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  404bba:	48 29 c8             	sub    %rcx,%rax
  404bbd:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  404bc4:	00 
  404bc5:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404bcc:	00 
  404bcd:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  404bd4:	00 
  404bd5:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  404bdc:	00 
  404bdd:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404be4:	00 
  404be5:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  404bec:	00 
  404bed:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  404bf4:	00 
  404bf5:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404bfc:	00 
  404bfd:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  404c04:	00 
  404c05:	0f 92 c0             	setb   %al
  404c08:	24 01                	and    $0x1,%al
  404c0a:	3c 00                	cmp    $0x0,%al
  404c0c:	0f 84 24 01 00 00    	je     404d36 <runtime::arena_allocator_proc+0x676>
  404c12:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  404c19:	00 
  404c1a:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404c21:	00 
  404c22:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  404c26:	0f 94 c0             	sete   %al
  404c29:	24 01                	and    $0x1,%al
  404c2b:	3c 00                	cmp    $0x0,%al
  404c2d:	0f 84 03 01 00 00    	je     404d36 <runtime::arena_allocator_proc+0x676>
  404c33:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  404c3a:	00 
  404c3b:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404c42:	00 
  404c43:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  404c47:	0f 96 c0             	setbe  %al
  404c4a:	24 01                	and    $0x1,%al
  404c4c:	3c 00                	cmp    $0x0,%al
  404c4e:	0f 84 e2 00 00 00    	je     404d36 <runtime::arena_allocator_proc+0x676>
  404c54:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404c5b:	00 
  404c5c:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  404c63:	00 
  404c64:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404c68:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404c6f:	00 
  404c70:	48 8b 40 18          	mov    0x18(%rax),%rax
  404c74:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404c79:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  404c80:	00 
  404c81:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  404c86:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  404c8d:	00 
  404c8e:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  404c93:	bf 50 71 40 00       	mov    $0x407150,%edi
  404c98:	be 3e 00 00 00       	mov    $0x3e,%esi
  404c9d:	ba e4 00 00 00       	mov    $0xe4,%edx
  404ca2:	b9 17 00 00 00       	mov    $0x17,%ecx
  404ca7:	e8 94 e8 ff ff       	call   403540 <runtime::multi_pointer_slice_expr_error>
  404cac:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404cb1:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404cb6:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404cbb:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404cc2:	00 
  404cc3:	48 01 f2             	add    %rsi,%rdx
  404cc6:	48 29 f0             	sub    %rsi,%rax
  404cc9:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  404cd0:	00 
  404cd1:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  404cd8:	00 
  404cd9:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  404ce0:	00 
  404ce1:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  404ce8:	00 
  404ce9:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404cf0:	00 
  404cf1:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404cf8:	00 
  404cf9:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404d00:	00 
  404d01:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404d08:	00 
  404d09:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404d10:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404d17:	00 
  404d18:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404d1f:	00 
  404d20:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404d27:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404d2b:	48 89 11             	mov    %rdx,(%rcx)
  404d2e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404d35:	c3                   	ret
  404d36:	eb 00                	jmp    404d38 <runtime::arena_allocator_proc+0x678>
  404d38:	eb 00                	jmp    404d3a <runtime::arena_allocator_proc+0x67a>
  404d3a:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404d41:	00 
  404d42:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404d49:	00 
  404d4a:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404d51:	00 
  404d52:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404d59:	00 
  404d5a:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404d61:	00 
  404d62:	0f 57 c0             	xorps  %xmm0,%xmm0
  404d65:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  404d6c:	00 
  404d6d:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  404d74:	00 
  404d75:	e8 26 eb ff ff       	call   4038a0 <runtime::arena_alloc>
  404d7a:	88 44 24 27          	mov    %al,0x27(%rsp)
  404d7e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  404d85:	00 
  404d86:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  404d8b:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  404d92:	00 
  404d93:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404d98:	3c 00                	cmp    $0x0,%al
  404d9a:	74 50                	je     404dec <runtime::arena_allocator_proc+0x72c>
  404d9c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404da3:	00 
  404da4:	8a 44 24 27          	mov    0x27(%rsp),%al
  404da8:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404daf:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404db6:	00 
  404db7:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404dbe:	00 
  404dbf:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404dc6:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404dcd:	00 
  404dce:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404dd5:	00 
  404dd6:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404ddd:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404de1:	48 89 11             	mov    %rdx,(%rcx)
  404de4:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404deb:	c3                   	ret
  404dec:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404df1:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  404df6:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  404dfd:	00 
  404dfe:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404e05:	00 
  404e06:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  404e0d:	00 00 
  404e0f:	0f 94 c0             	sete   %al
  404e12:	24 01                	and    $0x1,%al
  404e14:	3c 00                	cmp    $0x0,%al
  404e16:	74 45                	je     404e5d <runtime::arena_allocator_proc+0x79d>
  404e18:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404e1f:	00 
  404e20:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404e27:	00 
  404e28:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404e2f:	00 
  404e30:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404e37:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404e3e:	00 
  404e3f:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404e46:	00 
  404e47:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404e4e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404e52:	48 89 11             	mov    %rdx,(%rcx)
  404e55:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404e5c:	c3                   	ret
  404e5d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404e64:	00 
  404e65:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  404e6a:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  404e71:	00 
  404e72:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404e77:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404e7e:	00 
  404e7f:	48 89 04 24          	mov    %rax,(%rsp)
  404e83:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  404e8a:	00 
  404e8b:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  404e90:	bf 50 71 40 00       	mov    $0x407150,%edi
  404e95:	31 c0                	xor    %eax,%eax
  404e97:	41 89 c0             	mov    %eax,%r8d
  404e9a:	be 3e 00 00 00       	mov    $0x3e,%esi
  404e9f:	ba ee 00 00 00       	mov    $0xee,%edx
  404ea4:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  404ea9:	e8 92 e6 ff ff       	call   403540 <runtime::multi_pointer_slice_expr_error>
  404eae:	48 8b 0c 24          	mov    (%rsp),%rcx
  404eb2:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404eb7:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  404ebc:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404ec1:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  404ec8:	00 
  404ec9:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404ed0:	00 
  404ed1:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  404ed8:	00 
  404ed9:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  404ee0:	00 
  404ee1:	e8 8a ef ff ff       	call   403e70 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  404ee6:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  404eed:	00 
  404eee:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  404ef5:	00 
  404ef6:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  404efd:	00 
  404efe:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404f05:	00 
  404f06:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  404f0d:	00 
  404f0e:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404f15:	00 
  404f16:	48 89 50 08          	mov    %rdx,0x8(%rax)
  404f1a:	48 89 08             	mov    %rcx,(%rax)
  404f1d:	31 c0                	xor    %eax,%eax
  404f1f:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404f26:	c3                   	ret
  404f27:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404f2e:	00 
  404f2f:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  404f36:	00 
  404f37:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  404f3e:	00 00 
  404f40:	0f 95 c0             	setne  %al
  404f43:	24 01                	and    $0x1,%al
  404f45:	3c 00                	cmp    $0x0,%al
  404f47:	74 0b                	je     404f54 <runtime::arena_allocator_proc+0x894>
  404f49:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404f50:	00 
  404f51:	c6 00 5d             	movb   $0x5d,(%rax)
  404f54:	eb 08                	jmp    404f5e <runtime::arena_allocator_proc+0x89e>
  404f56:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404f5d:	04 
  404f5e:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404f65:	00 
  404f66:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404f6d:	00 
  404f6e:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404f75:	00 
  404f76:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404f7d:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404f84:	00 
  404f85:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404f8c:	00 
  404f8d:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404f94:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404f98:	48 89 11             	mov    %rdx,(%rcx)
  404f9b:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404fa2:	c3                   	ret
  404fa3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404faa:	84 00 00 00 00 00 

0000000000404fb0 <runtime::memory_equal>:
  404fb0:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  404fb5:	48 89 74 24 b8       	mov    %rsi,-0x48(%rsp)
  404fba:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  404fbf:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404fc4:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404fc9:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  404fce:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404fd3:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404fd8:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  404fdd:	48 83 f8 00          	cmp    $0x0,%rax
  404fe1:	0f 94 c1             	sete   %cl
  404fe4:	80 e1 01             	and    $0x1,%cl
  404fe7:	b0 01                	mov    $0x1,%al
  404fe9:	38 c8                	cmp    %cl,%al
  404feb:	74 1b                	je     405008 <runtime::memory_equal+0x58>
  404fed:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404ff2:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404ff7:	48 39 c8             	cmp    %rcx,%rax
  404ffa:	0f 94 c1             	sete   %cl
  404ffd:	80 e1 01             	and    $0x1,%cl
  405000:	b0 01                	mov    $0x1,%al
  405002:	38 c8                	cmp    %cl,%al
  405004:	74 07                	je     40500d <runtime::memory_equal+0x5d>
  405006:	eb 03                	jmp    40500b <runtime::memory_equal+0x5b>
  405008:	b0 01                	mov    $0x1,%al
  40500a:	c3                   	ret
  40500b:	eb 03                	jmp    405010 <runtime::memory_equal+0x60>
  40500d:	b0 01                	mov    $0x1,%al
  40500f:	c3                   	ret
  405010:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  405015:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40501a:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  40501f:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  405024:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405029:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40502e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  405035:	00 00 
  405037:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40503c:	48 3b 44 24 d0       	cmp    -0x30(%rsp),%rax
  405041:	0f 92 c0             	setb   %al
  405044:	24 01                	and    $0x1,%al
  405046:	3c 00                	cmp    $0x0,%al
  405048:	74 38                	je     405082 <runtime::memory_equal+0xd2>
  40504a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40504f:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  405054:	8a 04 08             	mov    (%rax,%rcx,1),%al
  405057:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40505c:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  405061:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  405064:	0f 95 c0             	setne  %al
  405067:	24 01                	and    $0x1,%al
  405069:	3c 00                	cmp    $0x0,%al
  40506b:	74 03                	je     405070 <runtime::memory_equal+0xc0>
  40506d:	31 c0                	xor    %eax,%eax
  40506f:	c3                   	ret
  405070:	eb 00                	jmp    405072 <runtime::memory_equal+0xc2>
  405072:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405077:	48 83 c0 01          	add    $0x1,%rax
  40507b:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  405080:	eb b5                	jmp    405037 <runtime::memory_equal+0x87>
  405082:	b0 01                	mov    $0x1,%al
  405084:	c3                   	ret
  405085:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40508c:	00 00 00 00 

0000000000405090 <runtime::memory_compare>:
  405090:	48 83 ec 10          	sub    $0x10,%rsp
  405094:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  405099:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  40509e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  4050a3:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  4050a8:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  4050ad:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  4050b2:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4050b7:	48 89 0c 24          	mov    %rcx,(%rsp)
  4050bb:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4050c0:	48 39 c8             	cmp    %rcx,%rax
  4050c3:	0f 94 c1             	sete   %cl
  4050c6:	80 e1 01             	and    $0x1,%cl
  4050c9:	b0 01                	mov    $0x1,%al
  4050cb:	38 c8                	cmp    %cl,%al
  4050cd:	74 17                	je     4050e6 <runtime::memory_compare+0x56>
  4050cf:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  4050d4:	48 83 f8 00          	cmp    $0x0,%rax
  4050d8:	0f 94 c1             	sete   %cl
  4050db:	80 e1 01             	and    $0x1,%cl
  4050de:	b0 01                	mov    $0x1,%al
  4050e0:	38 c8                	cmp    %cl,%al
  4050e2:	74 20                	je     405104 <runtime::memory_compare+0x74>
  4050e4:	eb 07                	jmp    4050ed <runtime::memory_compare+0x5d>
  4050e6:	31 c0                	xor    %eax,%eax
  4050e8:	48 83 c4 10          	add    $0x10,%rsp
  4050ec:	c3                   	ret
  4050ed:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  4050f2:	48 83 f8 00          	cmp    $0x0,%rax
  4050f6:	0f 94 c1             	sete   %cl
  4050f9:	80 e1 01             	and    $0x1,%cl
  4050fc:	b0 01                	mov    $0x1,%al
  4050fe:	38 c8                	cmp    %cl,%al
  405100:	74 10                	je     405112 <runtime::memory_compare+0x82>
  405102:	eb 0c                	jmp    405110 <runtime::memory_compare+0x80>
  405104:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40510b:	48 83 c4 10          	add    $0x10,%rsp
  40510f:	c3                   	ret
  405110:	eb 0a                	jmp    40511c <runtime::memory_compare+0x8c>
  405112:	b8 01 00 00 00       	mov    $0x1,%eax
  405117:	48 83 c4 10          	add    $0x10,%rsp
  40511b:	c3                   	ret
  40511c:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405121:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  405126:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  40512b:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  405130:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405135:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40513a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40513f:	48 c1 e8 03          	shr    $0x3,%rax
  405143:	48 83 c0 01          	add    $0x1,%rax
  405147:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40514c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405151:	48 83 e8 01          	sub    $0x1,%rax
  405155:	48 c1 e0 03          	shl    $0x3,%rax
  405159:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40515e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  405165:	00 00 
  405167:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  40516d:	0f 92 c0             	setb   %al
  405170:	24 01                	and    $0x1,%al
  405172:	3c 00                	cmp    $0x0,%al
  405174:	74 09                	je     40517f <runtime::memory_compare+0xef>
  405176:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  40517d:	00 00 
  40517f:	eb 00                	jmp    405181 <runtime::memory_compare+0xf1>
  405181:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405186:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  40518b:	0f 92 c0             	setb   %al
  40518e:	24 01                	and    $0x1,%al
  405190:	3c 00                	cmp    $0x0,%al
  405192:	0f 84 11 01 00 00    	je     4052a9 <runtime::memory_compare+0x219>
  405198:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40519d:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4051a2:	48 c1 e1 03          	shl    $0x3,%rcx
  4051a6:	48 01 c8             	add    %rcx,%rax
  4051a9:	48 8b 00             	mov    (%rax),%rax
  4051ac:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4051b1:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4051b6:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4051bb:	48 c1 e1 03          	shl    $0x3,%rcx
  4051bf:	48 01 c8             	add    %rcx,%rax
  4051c2:	48 8b 00             	mov    (%rax),%rax
  4051c5:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  4051ca:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4051cf:	48 33 44 24 b8       	xor    -0x48(%rsp),%rax
  4051d4:	48 83 f8 00          	cmp    $0x0,%rax
  4051d8:	0f 95 c0             	setne  %al
  4051db:	24 01                	and    $0x1,%al
  4051dd:	3c 00                	cmp    $0x0,%al
  4051df:	0f 84 af 00 00 00    	je     405294 <runtime::memory_compare+0x204>
  4051e5:	eb 00                	jmp    4051e7 <runtime::memory_compare+0x157>
  4051e7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4051ec:	48 c1 e0 03          	shl    $0x3,%rax
  4051f0:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4051f5:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4051fa:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4051ff:	0f 92 c0             	setb   %al
  405202:	24 01                	and    $0x1,%al
  405204:	3c 00                	cmp    $0x0,%al
  405206:	0f 84 86 00 00 00    	je     405292 <runtime::memory_compare+0x202>
  40520c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405211:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  405216:	8a 00                	mov    (%rax),%al
  405218:	88 44 24 af          	mov    %al,-0x51(%rsp)
  40521c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405221:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  405226:	8a 00                	mov    (%rax),%al
  405228:	88 44 24 ae          	mov    %al,-0x52(%rsp)
  40522c:	8a 44 24 af          	mov    -0x51(%rsp),%al
  405230:	32 44 24 ae          	xor    -0x52(%rsp),%al
  405234:	3c 00                	cmp    $0x0,%al
  405236:	0f 95 c0             	setne  %al
  405239:	24 01                	and    $0x1,%al
  40523b:	3c 00                	cmp    $0x0,%al
  40523d:	74 3e                	je     40527d <runtime::memory_compare+0x1ed>
  40523f:	0f b6 44 24 af       	movzbl -0x51(%rsp),%eax
  405244:	0f b6 4c 24 ae       	movzbl -0x52(%rsp),%ecx
  405249:	48 29 c8             	sub    %rcx,%rax
  40524c:	48 83 f8 00          	cmp    $0x0,%rax
  405250:	0f 9c c0             	setl   %al
  405253:	24 01                	and    $0x1,%al
  405255:	3c 00                	cmp    $0x0,%al
  405257:	74 0e                	je     405267 <runtime::memory_compare+0x1d7>
  405259:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405260:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  405265:	eb 0c                	jmp    405273 <runtime::memory_compare+0x1e3>
  405267:	b8 01 00 00 00       	mov    $0x1,%eax
  40526c:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  405271:	eb 00                	jmp    405273 <runtime::memory_compare+0x1e3>
  405273:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  405278:	48 83 c4 10          	add    $0x10,%rsp
  40527c:	c3                   	ret
  40527d:	eb 00                	jmp    40527f <runtime::memory_compare+0x1ef>
  40527f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405284:	48 83 c0 01          	add    $0x1,%rax
  405288:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40528d:	e9 63 ff ff ff       	jmp    4051f5 <runtime::memory_compare+0x165>
  405292:	eb 00                	jmp    405294 <runtime::memory_compare+0x204>
  405294:	eb 00                	jmp    405296 <runtime::memory_compare+0x206>
  405296:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40529b:	48 83 c0 01          	add    $0x1,%rax
  40529f:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4052a4:	e9 d8 fe ff ff       	jmp    405181 <runtime::memory_compare+0xf1>
  4052a9:	eb 00                	jmp    4052ab <runtime::memory_compare+0x21b>
  4052ab:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4052b0:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4052b5:	0f 92 c0             	setb   %al
  4052b8:	24 01                	and    $0x1,%al
  4052ba:	3c 00                	cmp    $0x0,%al
  4052bc:	0f 84 86 00 00 00    	je     405348 <runtime::memory_compare+0x2b8>
  4052c2:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4052c7:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  4052cc:	8a 00                	mov    (%rax),%al
  4052ce:	88 44 24 ad          	mov    %al,-0x53(%rsp)
  4052d2:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4052d7:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  4052dc:	8a 00                	mov    (%rax),%al
  4052de:	88 44 24 ac          	mov    %al,-0x54(%rsp)
  4052e2:	8a 44 24 ad          	mov    -0x53(%rsp),%al
  4052e6:	32 44 24 ac          	xor    -0x54(%rsp),%al
  4052ea:	3c 00                	cmp    $0x0,%al
  4052ec:	0f 95 c0             	setne  %al
  4052ef:	24 01                	and    $0x1,%al
  4052f1:	3c 00                	cmp    $0x0,%al
  4052f3:	74 3e                	je     405333 <runtime::memory_compare+0x2a3>
  4052f5:	0f b6 44 24 ad       	movzbl -0x53(%rsp),%eax
  4052fa:	0f b6 4c 24 ac       	movzbl -0x54(%rsp),%ecx
  4052ff:	48 29 c8             	sub    %rcx,%rax
  405302:	48 83 f8 00          	cmp    $0x0,%rax
  405306:	0f 9c c0             	setl   %al
  405309:	24 01                	and    $0x1,%al
  40530b:	3c 00                	cmp    $0x0,%al
  40530d:	74 0e                	je     40531d <runtime::memory_compare+0x28d>
  40530f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405316:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40531b:	eb 0c                	jmp    405329 <runtime::memory_compare+0x299>
  40531d:	b8 01 00 00 00       	mov    $0x1,%eax
  405322:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  405327:	eb 00                	jmp    405329 <runtime::memory_compare+0x299>
  405329:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  40532e:	48 83 c4 10          	add    $0x10,%rsp
  405332:	c3                   	ret
  405333:	eb 00                	jmp    405335 <runtime::memory_compare+0x2a5>
  405335:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40533a:	48 83 c0 01          	add    $0x1,%rax
  40533e:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405343:	e9 63 ff ff ff       	jmp    4052ab <runtime::memory_compare+0x21b>
  405348:	31 c0                	xor    %eax,%eax
  40534a:	48 83 c4 10          	add    $0x10,%rsp
  40534e:	c3                   	ret
  40534f:	90                   	nop

0000000000405350 <runtime::memory_compare_zero>:
  405350:	48 89 7c 24 a0       	mov    %rdi,-0x60(%rsp)
  405355:	48 89 74 24 a8       	mov    %rsi,-0x58(%rsp)
  40535a:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40535f:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  405364:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405369:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40536e:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405373:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405378:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40537d:	48 c1 e8 03          	shr    $0x3,%rax
  405381:	48 83 c0 01          	add    $0x1,%rax
  405385:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40538a:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40538f:	48 83 e8 01          	sub    $0x1,%rax
  405393:	48 c1 e0 03          	shl    $0x3,%rax
  405397:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40539c:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  4053a3:	00 00 
  4053a5:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  4053ab:	0f 92 c0             	setb   %al
  4053ae:	24 01                	and    $0x1,%al
  4053b0:	3c 00                	cmp    $0x0,%al
  4053b2:	74 09                	je     4053bd <runtime::memory_compare_zero+0x6d>
  4053b4:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4053bb:	00 00 
  4053bd:	eb 00                	jmp    4053bf <runtime::memory_compare_zero+0x6f>
  4053bf:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4053c4:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  4053c9:	0f 92 c0             	setb   %al
  4053cc:	24 01                	and    $0x1,%al
  4053ce:	3c 00                	cmp    $0x0,%al
  4053d0:	0f 84 d2 00 00 00    	je     4054a8 <runtime::memory_compare_zero+0x158>
  4053d6:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4053db:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4053e0:	48 c1 e1 03          	shl    $0x3,%rcx
  4053e4:	48 01 c8             	add    %rcx,%rax
  4053e7:	48 8b 00             	mov    (%rax),%rax
  4053ea:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4053ef:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4053f4:	48 83 f0 00          	xor    $0x0,%rax
  4053f8:	48 83 f8 00          	cmp    $0x0,%rax
  4053fc:	0f 95 c0             	setne  %al
  4053ff:	24 01                	and    $0x1,%al
  405401:	3c 00                	cmp    $0x0,%al
  405403:	0f 84 8a 00 00 00    	je     405493 <runtime::memory_compare_zero+0x143>
  405409:	eb 00                	jmp    40540b <runtime::memory_compare_zero+0xbb>
  40540b:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405410:	48 c1 e0 03          	shl    $0x3,%rax
  405414:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405419:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40541e:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  405423:	0f 92 c0             	setb   %al
  405426:	24 01                	and    $0x1,%al
  405428:	3c 00                	cmp    $0x0,%al
  40542a:	74 65                	je     405491 <runtime::memory_compare_zero+0x141>
  40542c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405431:	48 03 44 24 b8       	add    -0x48(%rsp),%rax
  405436:	8a 00                	mov    (%rax),%al
  405438:	88 44 24 b7          	mov    %al,-0x49(%rsp)
  40543c:	8a 44 24 b7          	mov    -0x49(%rsp),%al
  405440:	34 00                	xor    $0x0,%al
  405442:	3c 00                	cmp    $0x0,%al
  405444:	0f 95 c0             	setne  %al
  405447:	24 01                	and    $0x1,%al
  405449:	3c 00                	cmp    $0x0,%al
  40544b:	74 32                	je     40547f <runtime::memory_compare_zero+0x12f>
  40544d:	0f b6 44 24 b7       	movzbl -0x49(%rsp),%eax
  405452:	48 83 f8 00          	cmp    $0x0,%rax
  405456:	0f 9c c0             	setl   %al
  405459:	24 01                	and    $0x1,%al
  40545b:	3c 00                	cmp    $0x0,%al
  40545d:	74 0e                	je     40546d <runtime::memory_compare_zero+0x11d>
  40545f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405466:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40546b:	eb 0c                	jmp    405479 <runtime::memory_compare_zero+0x129>
  40546d:	b8 01 00 00 00       	mov    $0x1,%eax
  405472:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405477:	eb 00                	jmp    405479 <runtime::memory_compare_zero+0x129>
  405479:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40547e:	c3                   	ret
  40547f:	eb 00                	jmp    405481 <runtime::memory_compare_zero+0x131>
  405481:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405486:	48 83 c0 01          	add    $0x1,%rax
  40548a:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40548f:	eb 88                	jmp    405419 <runtime::memory_compare_zero+0xc9>
  405491:	eb 00                	jmp    405493 <runtime::memory_compare_zero+0x143>
  405493:	eb 00                	jmp    405495 <runtime::memory_compare_zero+0x145>
  405495:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40549a:	48 83 c0 01          	add    $0x1,%rax
  40549e:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4054a3:	e9 17 ff ff ff       	jmp    4053bf <runtime::memory_compare_zero+0x6f>
  4054a8:	eb 00                	jmp    4054aa <runtime::memory_compare_zero+0x15a>
  4054aa:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4054af:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4054b4:	0f 92 c0             	setb   %al
  4054b7:	24 01                	and    $0x1,%al
  4054b9:	3c 00                	cmp    $0x0,%al
  4054bb:	74 65                	je     405522 <runtime::memory_compare_zero+0x1d2>
  4054bd:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4054c2:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  4054c7:	8a 00                	mov    (%rax),%al
  4054c9:	88 44 24 b6          	mov    %al,-0x4a(%rsp)
  4054cd:	8a 44 24 b6          	mov    -0x4a(%rsp),%al
  4054d1:	34 00                	xor    $0x0,%al
  4054d3:	3c 00                	cmp    $0x0,%al
  4054d5:	0f 95 c0             	setne  %al
  4054d8:	24 01                	and    $0x1,%al
  4054da:	3c 00                	cmp    $0x0,%al
  4054dc:	74 32                	je     405510 <runtime::memory_compare_zero+0x1c0>
  4054de:	0f b6 44 24 b6       	movzbl -0x4a(%rsp),%eax
  4054e3:	48 83 f8 00          	cmp    $0x0,%rax
  4054e7:	0f 9c c0             	setl   %al
  4054ea:	24 01                	and    $0x1,%al
  4054ec:	3c 00                	cmp    $0x0,%al
  4054ee:	74 0e                	je     4054fe <runtime::memory_compare_zero+0x1ae>
  4054f0:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4054f7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  4054fc:	eb 0c                	jmp    40550a <runtime::memory_compare_zero+0x1ba>
  4054fe:	b8 01 00 00 00       	mov    $0x1,%eax
  405503:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405508:	eb 00                	jmp    40550a <runtime::memory_compare_zero+0x1ba>
  40550a:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  40550f:	c3                   	ret
  405510:	eb 00                	jmp    405512 <runtime::memory_compare_zero+0x1c2>
  405512:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405517:	48 83 c0 01          	add    $0x1,%rax
  40551b:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405520:	eb 88                	jmp    4054aa <runtime::memory_compare_zero+0x15a>
  405522:	31 c0                	xor    %eax,%eax
  405524:	c3                   	ret
  405525:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40552c:	00 00 00 00 

0000000000405530 <runtime::__type_info_of>:
  405530:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  405535:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40553a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40553f:	48 c7 c1 18 70 40 00 	mov    $0x407018,%rcx
  405546:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  40554a:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40554f:	31 c9                	xor    %ecx,%ecx
  405551:	89 ca                	mov    %ecx,%edx
  405553:	48 f7 74 24 f0       	divq   -0x10(%rsp)
  405558:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  40555d:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  405564:	00 00 
  405566:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  40556d:	00 00 
  40556f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405574:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  405579:	0f 83 9f 00 00 00    	jae    40561e <runtime::__type_info_of+0xee>
  40557f:	48 c7 c0 18 70 40 00 	mov    $0x407018,%rax
  405586:	48 8b 00             	mov    (%rax),%rax
  405589:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40558e:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  405592:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405597:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  40559d:	0f 95 c0             	setne  %al
  4055a0:	24 01                	and    $0x1,%al
  4055a2:	3c 00                	cmp    $0x0,%al
  4055a4:	74 1d                	je     4055c3 <runtime::__type_info_of+0x93>
  4055a6:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4055ab:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4055b0:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  4055b4:	0f 94 c0             	sete   %al
  4055b7:	24 01                	and    $0x1,%al
  4055b9:	3c 00                	cmp    $0x0,%al
  4055bb:	74 06                	je     4055c3 <runtime::__type_info_of+0x93>
  4055bd:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4055c2:	c3                   	ret
  4055c3:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4055c8:	48 83 c0 01          	add    $0x1,%rax
  4055cc:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  4055d1:	0f 92 c0             	setb   %al
  4055d4:	24 01                	and    $0x1,%al
  4055d6:	3c 00                	cmp    $0x0,%al
  4055d8:	74 10                	je     4055ea <runtime::__type_info_of+0xba>
  4055da:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4055df:	48 83 c0 01          	add    $0x1,%rax
  4055e3:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4055e8:	eb 09                	jmp    4055f3 <runtime::__type_info_of+0xc3>
  4055ea:	31 c0                	xor    %eax,%eax
  4055ec:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4055f1:	eb 00                	jmp    4055f3 <runtime::__type_info_of+0xc3>
  4055f3:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4055f8:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4055fd:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405602:	48 83 c0 01          	add    $0x1,%rax
  405606:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40560b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405610:	48 83 c0 01          	add    $0x1,%rax
  405614:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  405619:	e9 51 ff ff ff       	jmp    40556f <runtime::__type_info_of+0x3f>
  40561e:	48 c7 c0 18 70 40 00 	mov    $0x407018,%rax
  405625:	48 8b 00             	mov    (%rax),%rax
  405628:	48 8b 00             	mov    (%rax),%rax
  40562b:	c3                   	ret
  40562c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405630 <runtime::default_logger_proc>:
  405630:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405635:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40563a:	66 44 89 c0          	mov    %r8w,%ax
  40563e:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  405643:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  405648:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40564d:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  405652:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  405657:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40565c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  405661:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  405666:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  40566b:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  405670:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  405675:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40567a:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  40567f:	c3                   	ret

0000000000405680 <runtime::default_context>:
  405680:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  405687:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40568c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  405691:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405696:	31 f6                	xor    %esi,%esi
  405698:	ba 70 00 00 00       	mov    $0x70,%edx
  40569d:	e8 9e b9 ff ff       	call   401040 <memset@plt>
  4056a2:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  4056a7:	e8 24 00 00 00       	call   4056d0 <runtime::[core.odin]::__init_context>
  4056ac:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4056b1:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  4056b6:	ba 70 00 00 00       	mov    $0x70,%edx
  4056bb:	e8 a0 b9 ff ff       	call   401060 <memcpy@plt>
  4056c0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4056c5:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4056cc:	c3                   	ret
  4056cd:	0f 1f 00             	nopl   (%rax)

00000000004056d0 <runtime::[core.odin]::__init_context>:
  4056d0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  4056d5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4056da:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4056df:	48 83 f8 00          	cmp    $0x0,%rax
  4056e3:	0f 94 c0             	sete   %al
  4056e6:	24 01                	and    $0x1,%al
  4056e8:	3c 00                	cmp    $0x0,%al
  4056ea:	74 01                	je     4056ed <runtime::[core.odin]::__init_context+0x1d>
  4056ec:	c3                   	ret
  4056ed:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4056f2:	48 c7 c1 00 24 40 00 	mov    $0x402400,%rcx
  4056f9:	48 89 08             	mov    %rcx,(%rax)
  4056fc:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405701:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405708:	00 
  405709:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40570e:	48 c7 c1 90 2e 40 00 	mov    $0x402e90,%rcx
  405715:	48 89 48 10          	mov    %rcx,0x10(%rax)
  405719:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40571e:	48 c7 c2 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rdx
  405725:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  40572c:	00 00 
  40572e:	48 01 d1             	add    %rdx,%rcx
  405731:	48 89 48 18          	mov    %rcx,0x18(%rax)
  405735:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40573a:	48 c7 c1 80 57 40 00 	mov    $0x405780,%rcx
  405741:	48 89 48 20          	mov    %rcx,0x20(%rax)
  405745:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40574a:	48 c7 c1 30 56 40 00 	mov    $0x405630,%rcx
  405751:	48 89 48 28          	mov    %rcx,0x28(%rax)
  405755:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40575a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  405761:	00 
  405762:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405767:	48 c7 c1 50 2b 40 00 	mov    $0x402b50,%rcx
  40576e:	48 89 48 48          	mov    %rcx,0x48(%rax)
  405772:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405777:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  40577e:	00 
  40577f:	c3                   	ret

0000000000405780 <runtime::default_assertion_failure_proc>:
  405780:	48 83 ec 48          	sub    $0x48,%rsp
  405784:	4c 89 04 24          	mov    %r8,(%rsp)
  405788:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40578d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405792:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405797:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40579c:	4c 8b 04 24          	mov    (%rsp),%r8
  4057a0:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4057a5:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4057aa:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4057af:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4057b4:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4057b9:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4057be:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4057c3:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4057c8:	e8 03 00 00 00       	call   4057d0 <runtime::default_assertion_contextless_failure_proc>
  4057cd:	0f 1f 00             	nopl   (%rax)

00000000004057d0 <runtime::default_assertion_contextless_failure_proc>:
  4057d0:	48 83 ec 48          	sub    $0x48,%rsp
  4057d4:	4c 89 04 24          	mov    %r8,(%rsp)
  4057d8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4057dd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4057e2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4057e7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4057ec:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4057f1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4057f6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4057fb:	48 8b 3c 24          	mov    (%rsp),%rdi
  4057ff:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405804:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405809:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40580e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405813:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  405818:	e8 d3 ec ff ff       	call   4044f0 <runtime::print_caller_location>
  40581d:	bf 42 73 40 00       	mov    $0x407342,%edi
  405822:	be 01 00 00 00       	mov    $0x1,%esi
  405827:	e8 a4 e4 ff ff       	call   403cd0 <runtime::print_string>
  40582c:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405831:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405836:	e8 95 e4 ff ff       	call   403cd0 <runtime::print_string>
  40583b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405840:	48 83 f8 00          	cmp    $0x0,%rax
  405844:	0f 9f c0             	setg   %al
  405847:	24 01                	and    $0x1,%al
  405849:	3c 00                	cmp    $0x0,%al
  40584b:	74 1e                	je     40586b <runtime::default_assertion_contextless_failure_proc+0x9b>
  40584d:	bf 44 73 40 00       	mov    $0x407344,%edi
  405852:	be 02 00 00 00       	mov    $0x2,%esi
  405857:	e8 74 e4 ff ff       	call   403cd0 <runtime::print_string>
  40585c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  405861:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  405866:	e8 65 e4 ff ff       	call   403cd0 <runtime::print_string>
  40586b:	bf 0a 00 00 00       	mov    $0xa,%edi
  405870:	e8 8b e6 ff ff       	call   403f00 <runtime::print_byte>
  405875:	0f 0b                	ud2
  405877:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40587e:	00 00 

0000000000405880 <__truncsfhf2>:
  405880:	48 83 ec 18          	sub    $0x18,%rsp
  405884:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  40588a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  405890:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  405896:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40589d:	00 
  40589e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  4058a5:	00 
  4058a6:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  4058ad:	00 
  4058ae:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  4058b5:	00 
  4058b6:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  4058bd:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  4058c3:	8b 44 24 10          	mov    0x10(%rsp),%eax
  4058c7:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  4058cb:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4058cf:	c1 f9 10             	sar    $0x10,%ecx
  4058d2:	b2 01                	mov    $0x1,%dl
  4058d4:	31 c0                	xor    %eax,%eax
  4058d6:	f6 c2 01             	test   $0x1,%dl
  4058d9:	0f 45 c1             	cmovne %ecx,%eax
  4058dc:	25 00 80 00 00       	and    $0x8000,%eax
  4058e1:	89 44 24 08          	mov    %eax,0x8(%rsp)
  4058e5:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4058e9:	c1 f9 17             	sar    $0x17,%ecx
  4058ec:	b2 01                	mov    $0x1,%dl
  4058ee:	31 c0                	xor    %eax,%eax
  4058f0:	f6 c2 01             	test   $0x1,%dl
  4058f3:	0f 45 c1             	cmovne %ecx,%eax
  4058f6:	25 ff 00 00 00       	and    $0xff,%eax
  4058fb:	83 e8 70             	sub    $0x70,%eax
  4058fe:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405902:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  405906:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  40590b:	89 04 24             	mov    %eax,(%rsp)
  40590e:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  405913:	0f 9e c0             	setle  %al
  405916:	24 01                	and    $0x1,%al
  405918:	3c 00                	cmp    $0x0,%al
  40591a:	0f 84 82 00 00 00    	je     4059a2 <__truncsfhf2+0x122>
  405920:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  405925:	0f 9c c0             	setl   %al
  405928:	24 01                	and    $0x1,%al
  40592a:	3c 00                	cmp    $0x0,%al
  40592c:	74 16                	je     405944 <__truncsfhf2+0xc4>
  40592e:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  405933:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  405938:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  40593f:	48 83 c4 18          	add    $0x18,%rsp
  405943:	c3                   	ret
  405944:	8b 04 24             	mov    (%rsp),%eax
  405947:	0d 00 00 80 00       	or     $0x800000,%eax
  40594c:	ba 01 00 00 00       	mov    $0x1,%edx
  405951:	2b 54 24 04          	sub    0x4(%rsp),%edx
  405955:	89 d1                	mov    %edx,%ecx
  405957:	d3 f8                	sar    %cl,%eax
  405959:	89 c1                	mov    %eax,%ecx
  40595b:	31 c0                	xor    %eax,%eax
  40595d:	83 fa 20             	cmp    $0x20,%edx
  405960:	0f 42 c1             	cmovb  %ecx,%eax
  405963:	89 04 24             	mov    %eax,(%rsp)
  405966:	8b 04 24             	mov    (%rsp),%eax
  405969:	25 00 10 00 00       	and    $0x1000,%eax
  40596e:	83 f8 00             	cmp    $0x0,%eax
  405971:	0f 95 c0             	setne  %al
  405974:	24 01                	and    $0x1,%al
  405976:	3c 00                	cmp    $0x0,%al
  405978:	74 0b                	je     405985 <__truncsfhf2+0x105>
  40597a:	8b 04 24             	mov    (%rsp),%eax
  40597d:	05 00 20 00 00       	add    $0x2000,%eax
  405982:	89 04 24             	mov    %eax,(%rsp)
  405985:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405989:	8b 0c 24             	mov    (%rsp),%ecx
  40598c:	c1 e9 0d             	shr    $0xd,%ecx
  40598f:	09 c8                	or     %ecx,%eax
  405991:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  405996:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40599d:	48 83 c4 18          	add    $0x18,%rsp
  4059a1:	c3                   	ret
  4059a2:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  4059a9:	00 
  4059aa:	0f 94 c0             	sete   %al
  4059ad:	24 01                	and    $0x1,%al
  4059af:	3c 00                	cmp    $0x0,%al
  4059b1:	74 59                	je     405a0c <__truncsfhf2+0x18c>
  4059b3:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  4059b7:	0f 94 c0             	sete   %al
  4059ba:	24 01                	and    $0x1,%al
  4059bc:	3c 00                	cmp    $0x0,%al
  4059be:	74 1a                	je     4059da <__truncsfhf2+0x15a>
  4059c0:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4059c4:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4059c9:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  4059ce:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  4059d5:	48 83 c4 18          	add    $0x18,%rsp
  4059d9:	c3                   	ret
  4059da:	8b 04 24             	mov    (%rsp),%eax
  4059dd:	c1 f8 0d             	sar    $0xd,%eax
  4059e0:	89 04 24             	mov    %eax,(%rsp)
  4059e3:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4059e7:	8b 0c 24             	mov    (%rsp),%ecx
  4059ea:	09 c8                	or     %ecx,%eax
  4059ec:	85 c9                	test   %ecx,%ecx
  4059ee:	0f 94 c1             	sete   %cl
  4059f1:	0f b6 c9             	movzbl %cl,%ecx
  4059f4:	09 c8                	or     %ecx,%eax
  4059f6:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4059fb:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  405a00:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  405a07:	48 83 c4 18          	add    $0x18,%rsp
  405a0b:	c3                   	ret
  405a0c:	8b 04 24             	mov    (%rsp),%eax
  405a0f:	25 00 10 00 00       	and    $0x1000,%eax
  405a14:	83 f8 00             	cmp    $0x0,%eax
  405a17:	0f 95 c0             	setne  %al
  405a1a:	24 01                	and    $0x1,%al
  405a1c:	3c 00                	cmp    $0x0,%al
  405a1e:	74 33                	je     405a53 <__truncsfhf2+0x1d3>
  405a20:	8b 04 24             	mov    (%rsp),%eax
  405a23:	05 00 20 00 00       	add    $0x2000,%eax
  405a28:	89 04 24             	mov    %eax,(%rsp)
  405a2b:	8b 04 24             	mov    (%rsp),%eax
  405a2e:	25 00 00 80 00       	and    $0x800000,%eax
  405a33:	83 f8 00             	cmp    $0x0,%eax
  405a36:	0f 95 c0             	setne  %al
  405a39:	24 01                	and    $0x1,%al
  405a3b:	3c 00                	cmp    $0x0,%al
  405a3d:	74 12                	je     405a51 <__truncsfhf2+0x1d1>
  405a3f:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  405a46:	8b 44 24 04          	mov    0x4(%rsp),%eax
  405a4a:	83 c0 01             	add    $0x1,%eax
  405a4d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405a51:	eb 00                	jmp    405a53 <__truncsfhf2+0x1d3>
  405a53:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  405a58:	0f 9f c0             	setg   %al
  405a5b:	24 01                	and    $0x1,%al
  405a5d:	3c 00                	cmp    $0x0,%al
  405a5f:	74 75                	je     405ad6 <__truncsfhf2+0x256>
  405a61:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  405a68:	00 00 00 
  405a6b:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405a70:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  405a77:	00 00 
  405a79:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  405a7f:	0f 9c c0             	setl   %al
  405a82:	24 01                	and    $0x1,%al
  405a84:	3c 00                	cmp    $0x0,%al
  405a86:	74 34                	je     405abc <__truncsfhf2+0x23c>
  405a88:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405a8d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405a92:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405a97:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  405a9d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405aa2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405aa7:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405aac:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405ab1:	48 83 c0 01          	add    $0x1,%rax
  405ab5:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  405aba:	eb bd                	jmp    405a79 <__truncsfhf2+0x1f9>
  405abc:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405ac0:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405ac5:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  405aca:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  405ad1:	48 83 c4 18          	add    $0x18,%rsp
  405ad5:	c3                   	ret
  405ad6:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405ada:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  405ade:	c1 e1 0a             	shl    $0xa,%ecx
  405ae1:	09 c8                	or     %ecx,%eax
  405ae3:	8b 0c 24             	mov    (%rsp),%ecx
  405ae6:	c1 e9 0d             	shr    $0xd,%ecx
  405ae9:	09 c8                	or     %ecx,%eax
  405aeb:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  405af0:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  405af7:	48 83 c4 18          	add    $0x18,%rsp
  405afb:	c3                   	ret
  405afc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405b00 <__truncdfhf2>:
  405b00:	48 83 ec 18          	sub    $0x18,%rsp
  405b04:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  405b0a:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  405b10:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  405b16:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  405b1a:	e8 61 fd ff ff       	call   405880 <__truncsfhf2>
  405b1f:	48 83 c4 18          	add    $0x18,%rsp
  405b23:	c3                   	ret
  405b24:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  405b2b:	00 00 00 00 00 

0000000000405b30 <__gnu_h2f_ieee>:
  405b30:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  405b36:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  405b3c:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  405b43:	00 
  405b44:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  405b4b:	00 
  405b4c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  405b51:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  405b56:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  405b5d:	00 
  405b5e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  405b65:	00 
  405b66:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  405b6d:	00 
  405b6e:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  405b75:	77 
  405b76:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  405b7d:	47 
  405b7e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405b83:	66 25 ff 7f          	and    $0x7fff,%ax
  405b87:	0f b7 c8             	movzwl %ax,%ecx
  405b8a:	c1 e1 0d             	shl    $0xd,%ecx
  405b8d:	b2 01                	mov    $0x1,%dl
  405b8f:	31 c0                	xor    %eax,%eax
  405b91:	f6 c2 01             	test   $0x1,%dl
  405b94:	0f 45 c1             	cmovne %ecx,%eax
  405b97:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405b9b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  405ba1:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  405ba7:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  405bad:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405bb3:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  405bb8:	0f 93 c0             	setae  %al
  405bbb:	24 01                	and    $0x1,%al
  405bbd:	3c 00                	cmp    $0x0,%al
  405bbf:	74 0d                	je     405bce <__gnu_h2f_ieee+0x9e>
  405bc1:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405bc5:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  405bca:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405bce:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405bd3:	66 25 00 80          	and    $0x8000,%ax
  405bd7:	0f b7 c8             	movzwl %ax,%ecx
  405bda:	c1 e1 10             	shl    $0x10,%ecx
  405bdd:	b2 01                	mov    $0x1,%dl
  405bdf:	31 c0                	xor    %eax,%eax
  405be1:	f6 c2 01             	test   $0x1,%dl
  405be4:	0f 45 c1             	cmovne %ecx,%eax
  405be7:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  405beb:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405bef:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405bf5:	c3                   	ret
  405bf6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  405bfd:	00 00 00 

0000000000405c00 <__gnu_f2h_ieee>:
  405c00:	50                   	push   %rax
  405c01:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  405c06:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  405c0b:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  405c11:	e8 6a fc ff ff       	call   405880 <__truncsfhf2>
  405c16:	58                   	pop    %rax
  405c17:	c3                   	ret
  405c18:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  405c1f:	00 

0000000000405c20 <__extendhfsf2>:
  405c20:	50                   	push   %rax
  405c21:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  405c27:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  405c2d:	0f 28 c8             	movaps %xmm0,%xmm1
  405c30:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  405c37:	00 
  405c38:	e8 f3 fe ff ff       	call   405b30 <__gnu_h2f_ieee>
  405c3d:	58                   	pop    %rax
  405c3e:	c3                   	ret
  405c3f:	90                   	nop

0000000000405c40 <__floattidf>:
  405c40:	53                   	push   %rbx
  405c41:	48 83 ec 10          	sub    $0x10,%rsp
  405c45:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405c4a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  405c4f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405c54:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405c59:	48 89 04 24          	mov    %rax,(%rsp)
  405c5d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405c62:	48 09 c8             	or     %rcx,%rax
  405c65:	0f 94 c0             	sete   %al
  405c68:	24 01                	and    $0x1,%al
  405c6a:	3c 00                	cmp    $0x0,%al
  405c6c:	74 09                	je     405c77 <__floattidf+0x37>
  405c6e:	0f 57 c0             	xorps  %xmm0,%xmm0
  405c71:	48 83 c4 10          	add    $0x10,%rsp
  405c75:	5b                   	pop    %rbx
  405c76:	c3                   	ret
  405c77:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405c7c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405c81:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405c86:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405c8b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405c90:	48 c1 f8 3f          	sar    $0x3f,%rax
  405c94:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405c99:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405c9e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405ca3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405ca8:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  405cad:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405cb2:	48 31 d0             	xor    %rdx,%rax
  405cb5:	48 31 f1             	xor    %rsi,%rcx
  405cb8:	48 29 f1             	sub    %rsi,%rcx
  405cbb:	48 19 d0             	sbb    %rdx,%rax
  405cbe:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405cc3:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405cc8:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  405ccd:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  405cd2:	48 0f bd c2          	bsr    %rdx,%rax
  405cd6:	48 83 f0 3f          	xor    $0x3f,%rax
  405cda:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  405cdf:	48 0f bd ce          	bsr    %rsi,%rcx
  405ce3:	48 83 f1 3f          	xor    $0x3f,%rcx
  405ce7:	48 83 c1 40          	add    $0x40,%rcx
  405ceb:	48 85 d2             	test   %rdx,%rdx
  405cee:	48 0f 45 c8          	cmovne %rax,%rcx
  405cf2:	31 c0                	xor    %eax,%eax
  405cf4:	ba 80 00 00 00       	mov    $0x80,%edx
  405cf9:	48 29 ca             	sub    %rcx,%rdx
  405cfc:	48 89 c1             	mov    %rax,%rcx
  405cff:	48 19 c9             	sbb    %rcx,%rcx
  405d02:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405d07:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405d0c:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  405d10:	ff c9                	dec    %ecx
  405d12:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  405d16:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  405d1b:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405d20:	ba 35 00 00 00       	mov    $0x35,%edx
  405d25:	48 29 f2             	sub    %rsi,%rdx
  405d28:	48 19 c8             	sbb    %rcx,%rax
  405d2b:	0f 9c c0             	setl   %al
  405d2e:	24 01                	and    $0x1,%al
  405d30:	3c 00                	cmp    $0x0,%al
  405d32:	0f 84 c0 01 00 00    	je     405ef8 <__floattidf+0x2b8>
  405d38:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405d3d:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  405d42:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405d47:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405d4c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  405d51:	0f 28 0d 78 16 00 00 	movaps 0x1678(%rip),%xmm1        # 4073d0 <runtime::type_table+0x3b8>
  405d58:	66 0f ef c1          	pxor   %xmm1,%xmm0
  405d5c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  405d61:	74 17                	je     405d7a <__floattidf+0x13a>
  405d63:	eb 00                	jmp    405d65 <__floattidf+0x125>
  405d65:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  405d6a:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405d6f:	48 83 f0 37          	xor    $0x37,%rax
  405d73:	48 09 c8             	or     %rcx,%rax
  405d76:	74 26                	je     405d9e <__floattidf+0x15e>
  405d78:	eb 29                	jmp    405da3 <__floattidf+0x163>
  405d7a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405d7f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405d84:	48 89 d0             	mov    %rdx,%rax
  405d87:	48 01 c0             	add    %rax,%rax
  405d8a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  405d8f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405d94:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405d99:	e9 d6 00 00 00       	jmp    405e74 <__floattidf+0x234>
  405d9e:	e9 d1 00 00 00       	jmp    405e74 <__floattidf+0x234>
  405da3:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405da8:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  405dad:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405db2:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405db7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405dbc:	49 89 fb             	mov    %rdi,%r11
  405dbf:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405dc3:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405dc7:	44 88 db             	mov    %r11b,%bl
  405dca:	88 d9                	mov    %bl,%cl
  405dcc:	49 89 f2             	mov    %rsi,%r10
  405dcf:	49 d3 ea             	shr    %cl,%r10
  405dd2:	88 d9                	mov    %bl,%cl
  405dd4:	49 89 d1             	mov    %rdx,%r9
  405dd7:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  405ddb:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  405de0:	45 31 c0             	xor    %r8d,%r8d
  405de3:	f6 c3 40             	test   $0x40,%bl
  405de6:	4d 0f 45 ca          	cmovne %r10,%r9
  405dea:	4d 0f 45 d0          	cmovne %r8,%r10
  405dee:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405df5:	48 83 d8 00          	sbb    $0x0,%rax
  405df9:	4c 89 c0             	mov    %r8,%rax
  405dfc:	49 0f 42 c2          	cmovb  %r10,%rax
  405e00:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405e05:	4c 89 c0             	mov    %r8,%rax
  405e08:	49 0f 42 c1          	cmovb  %r9,%rax
  405e0c:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  405e12:	49 29 fb             	sub    %rdi,%r11
  405e15:	4c 89 c7             	mov    %r8,%rdi
  405e18:	48 19 cf             	sbb    %rcx,%rdi
  405e1b:	45 88 d9             	mov    %r11b,%r9b
  405e1e:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  405e25:	44 88 c9             	mov    %r9b,%cl
  405e28:	4c 89 d3             	mov    %r10,%rbx
  405e2b:	48 d3 eb             	shr    %cl,%rbx
  405e2e:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  405e33:	41 f6 c1 40          	test   $0x40,%r9b
  405e37:	49 89 d9             	mov    %rbx,%r9
  405e3a:	4d 0f 45 c8          	cmovne %r8,%r9
  405e3e:	4c 0f 45 d3          	cmovne %rbx,%r10
  405e42:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405e49:	48 83 df 00          	sbb    $0x0,%rdi
  405e4d:	4c 89 c7             	mov    %r8,%rdi
  405e50:	49 0f 42 fa          	cmovb  %r10,%rdi
  405e54:	4d 0f 42 c1          	cmovb  %r9,%r8
  405e58:	4c 21 c6             	and    %r8,%rsi
  405e5b:	48 21 fa             	and    %rdi,%rdx
  405e5e:	48 09 f2             	or     %rsi,%rdx
  405e61:	0f 95 c2             	setne  %dl
  405e64:	0f b6 d2             	movzbl %dl,%edx
  405e67:	48 09 d0             	or     %rdx,%rax
  405e6a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405e6f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405e74:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405e79:	89 c1                	mov    %eax,%ecx
  405e7b:	83 e1 04             	and    $0x4,%ecx
  405e7e:	c1 e9 02             	shr    $0x2,%ecx
  405e81:	48 09 c8             	or     %rcx,%rax
  405e84:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405e89:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405e8e:	48 83 c0 01          	add    $0x1,%rax
  405e92:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  405e98:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405e9d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405ea2:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405ea7:	48 89 c8             	mov    %rcx,%rax
  405eaa:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  405eaf:	48 c1 f9 02          	sar    $0x2,%rcx
  405eb3:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405eb8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405ebd:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  405ec1:	24 20                	and    $0x20,%al
  405ec3:	c0 e8 05             	shr    $0x5,%al
  405ec6:	24 01                	and    $0x1,%al
  405ec8:	3c 00                	cmp    $0x0,%al
  405eca:	74 2a                	je     405ef6 <__floattidf+0x2b6>
  405ecc:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405ed1:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405ed6:	48 89 c8             	mov    %rcx,%rax
  405ed9:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  405ede:	48 d1 f9             	sar    $1,%rcx
  405ee1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405ee6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405eeb:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  405eef:	83 c0 01             	add    $0x1,%eax
  405ef2:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405ef6:	eb 5c                	jmp    405f54 <__floattidf+0x314>
  405ef8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  405efc:	b9 35 00 00 00       	mov    $0x35,%ecx
  405f01:	29 c1                	sub    %eax,%ecx
  405f03:	83 e1 7f             	and    $0x7f,%ecx
  405f06:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  405f0a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405f0f:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  405f14:	40 88 cf             	mov    %cl,%dil
  405f17:	40 88 f9             	mov    %dil,%cl
  405f1a:	48 89 c2             	mov    %rax,%rdx
  405f1d:	48 d3 e2             	shl    %cl,%rdx
  405f20:	40 88 f9             	mov    %dil,%cl
  405f23:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  405f27:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  405f2b:	31 c0                	xor    %eax,%eax
  405f2d:	40 f6 c7 40          	test   $0x40,%dil
  405f31:	48 0f 45 f2          	cmovne %rdx,%rsi
  405f35:	48 0f 45 d0          	cmovne %rax,%rdx
  405f39:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  405f3f:	48 89 c1             	mov    %rax,%rcx
  405f42:	48 0f 42 ce          	cmovb  %rsi,%rcx
  405f46:	48 0f 42 c2          	cmovb  %rdx,%rax
  405f4a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405f4f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405f54:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  405f5b:	00 00 
  405f5d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  405f61:	25 00 00 00 80       	and    $0x80000000,%eax
  405f66:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  405f6a:	c1 e1 14             	shl    $0x14,%ecx
  405f6d:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  405f73:	09 c8                	or     %ecx,%eax
  405f75:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  405f79:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  405f7f:	09 c8                	or     %ecx,%eax
  405f81:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405f85:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405f89:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  405f8d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405f93:	48 83 c4 10          	add    $0x10,%rsp
  405f97:	5b                   	pop    %rbx
  405f98:	c3                   	ret
  405f99:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000405fa0 <__floattidf_unsigned>:
  405fa0:	53                   	push   %rbx
  405fa1:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405fa6:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  405fab:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405fb0:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405fb5:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405fba:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405fbf:	48 09 c8             	or     %rcx,%rax
  405fc2:	0f 94 c0             	sete   %al
  405fc5:	24 01                	and    $0x1,%al
  405fc7:	3c 00                	cmp    $0x0,%al
  405fc9:	74 05                	je     405fd0 <__floattidf_unsigned+0x30>
  405fcb:	0f 57 c0             	xorps  %xmm0,%xmm0
  405fce:	5b                   	pop    %rbx
  405fcf:	c3                   	ret
  405fd0:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405fd5:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405fda:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  405fdf:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405fe4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  405fe9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405fee:	48 0f bd c2          	bsr    %rdx,%rax
  405ff2:	48 83 f0 3f          	xor    $0x3f,%rax
  405ff6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  405ffb:	48 0f bd ce          	bsr    %rsi,%rcx
  405fff:	48 83 f1 3f          	xor    $0x3f,%rcx
  406003:	48 83 c1 40          	add    $0x40,%rcx
  406007:	48 85 d2             	test   %rdx,%rdx
  40600a:	48 0f 45 c8          	cmovne %rax,%rcx
  40600e:	31 c0                	xor    %eax,%eax
  406010:	ba 80 00 00 00       	mov    $0x80,%edx
  406015:	48 29 ca             	sub    %rcx,%rdx
  406018:	48 89 c1             	mov    %rax,%rcx
  40601b:	48 19 c9             	sbb    %rcx,%rcx
  40601e:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  406023:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406028:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  40602c:	ff c9                	dec    %ecx
  40602e:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  406032:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  406037:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40603c:	ba 35 00 00 00       	mov    $0x35,%edx
  406041:	48 29 f2             	sub    %rsi,%rdx
  406044:	48 19 c8             	sbb    %rcx,%rax
  406047:	0f 92 c0             	setb   %al
  40604a:	24 01                	and    $0x1,%al
  40604c:	3c 00                	cmp    $0x0,%al
  40604e:	0f 84 c0 01 00 00    	je     406214 <__floattidf_unsigned+0x274>
  406054:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406059:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40605e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406063:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  406068:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40606d:	0f 28 0d 5c 13 00 00 	movaps 0x135c(%rip),%xmm1        # 4073d0 <runtime::type_table+0x3b8>
  406074:	66 0f ef c1          	pxor   %xmm1,%xmm0
  406078:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  40607d:	74 17                	je     406096 <__floattidf_unsigned+0xf6>
  40607f:	eb 00                	jmp    406081 <__floattidf_unsigned+0xe1>
  406081:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  406086:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40608b:	48 83 f0 37          	xor    $0x37,%rax
  40608f:	48 09 c8             	or     %rcx,%rax
  406092:	74 26                	je     4060ba <__floattidf_unsigned+0x11a>
  406094:	eb 29                	jmp    4060bf <__floattidf_unsigned+0x11f>
  406096:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40609b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4060a0:	48 89 d0             	mov    %rdx,%rax
  4060a3:	48 01 c0             	add    %rax,%rax
  4060a6:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  4060ab:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4060b0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4060b5:	e9 d6 00 00 00       	jmp    406190 <__floattidf_unsigned+0x1f0>
  4060ba:	e9 d1 00 00 00       	jmp    406190 <__floattidf_unsigned+0x1f0>
  4060bf:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4060c4:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  4060c9:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  4060ce:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4060d3:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  4060d8:	49 89 fb             	mov    %rdi,%r11
  4060db:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  4060df:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  4060e3:	44 88 db             	mov    %r11b,%bl
  4060e6:	88 d9                	mov    %bl,%cl
  4060e8:	49 89 f2             	mov    %rsi,%r10
  4060eb:	49 d3 ea             	shr    %cl,%r10
  4060ee:	88 d9                	mov    %bl,%cl
  4060f0:	49 89 d1             	mov    %rdx,%r9
  4060f3:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  4060f7:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  4060fc:	45 31 c0             	xor    %r8d,%r8d
  4060ff:	f6 c3 40             	test   $0x40,%bl
  406102:	4d 0f 45 ca          	cmovne %r10,%r9
  406106:	4d 0f 45 d0          	cmovne %r8,%r10
  40610a:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  406111:	48 83 d8 00          	sbb    $0x0,%rax
  406115:	4c 89 c0             	mov    %r8,%rax
  406118:	49 0f 42 c2          	cmovb  %r10,%rax
  40611c:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  406121:	4c 89 c0             	mov    %r8,%rax
  406124:	49 0f 42 c1          	cmovb  %r9,%rax
  406128:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  40612e:	49 29 fb             	sub    %rdi,%r11
  406131:	4c 89 c7             	mov    %r8,%rdi
  406134:	48 19 cf             	sbb    %rcx,%rdi
  406137:	45 88 d9             	mov    %r11b,%r9b
  40613a:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  406141:	44 88 c9             	mov    %r9b,%cl
  406144:	4c 89 d3             	mov    %r10,%rbx
  406147:	48 d3 eb             	shr    %cl,%rbx
  40614a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40614f:	41 f6 c1 40          	test   $0x40,%r9b
  406153:	49 89 d9             	mov    %rbx,%r9
  406156:	4d 0f 45 c8          	cmovne %r8,%r9
  40615a:	4c 0f 45 d3          	cmovne %rbx,%r10
  40615e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  406165:	48 83 df 00          	sbb    $0x0,%rdi
  406169:	4c 89 c7             	mov    %r8,%rdi
  40616c:	49 0f 42 fa          	cmovb  %r10,%rdi
  406170:	4d 0f 42 c1          	cmovb  %r9,%r8
  406174:	4c 21 c6             	and    %r8,%rsi
  406177:	48 21 fa             	and    %rdi,%rdx
  40617a:	48 09 f2             	or     %rsi,%rdx
  40617d:	0f 95 c2             	setne  %dl
  406180:	0f b6 d2             	movzbl %dl,%edx
  406183:	48 09 d0             	or     %rdx,%rax
  406186:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40618b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406190:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  406195:	89 c1                	mov    %eax,%ecx
  406197:	83 e1 04             	and    $0x4,%ecx
  40619a:	c1 e9 02             	shr    $0x2,%ecx
  40619d:	48 09 c8             	or     %rcx,%rax
  4061a0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4061a5:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4061aa:	48 83 c0 01          	add    $0x1,%rax
  4061ae:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  4061b4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4061b9:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4061be:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4061c3:	48 89 c8             	mov    %rcx,%rax
  4061c6:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  4061cb:	48 c1 e9 02          	shr    $0x2,%rcx
  4061cf:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4061d4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4061d9:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  4061dd:	24 20                	and    $0x20,%al
  4061df:	c0 e8 05             	shr    $0x5,%al
  4061e2:	24 01                	and    $0x1,%al
  4061e4:	3c 00                	cmp    $0x0,%al
  4061e6:	74 2a                	je     406212 <__floattidf_unsigned+0x272>
  4061e8:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4061ed:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4061f2:	48 89 c8             	mov    %rcx,%rax
  4061f5:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  4061fa:	48 d1 e9             	shr    $1,%rcx
  4061fd:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  406202:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406207:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40620b:	83 c0 01             	add    $0x1,%eax
  40620e:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  406212:	eb 6a                	jmp    40627e <__floattidf_unsigned+0x2de>
  406214:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  406219:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40621e:	31 c0                	xor    %eax,%eax
  406220:	bf 35 00 00 00       	mov    $0x35,%edi
  406225:	48 29 d7             	sub    %rdx,%rdi
  406228:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40622d:	48 19 c8             	sbb    %rcx,%rax
  406230:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  406235:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40623a:	41 88 f8             	mov    %dil,%r8b
  40623d:	44 88 c1             	mov    %r8b,%cl
  406240:	4c 89 ce             	mov    %r9,%rsi
  406243:	48 d3 e6             	shl    %cl,%rsi
  406246:	44 88 c1             	mov    %r8b,%cl
  406249:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  40624d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  406252:	41 f6 c0 40          	test   $0x40,%r8b
  406256:	48 0f 45 d6          	cmovne %rsi,%rdx
  40625a:	48 0f 45 f1          	cmovne %rcx,%rsi
  40625e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  406265:	48 83 d8 00          	sbb    $0x0,%rax
  406269:	48 89 c8             	mov    %rcx,%rax
  40626c:	48 0f 42 c6          	cmovb  %rsi,%rax
  406270:	48 0f 42 ca          	cmovb  %rdx,%rcx
  406274:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  406279:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40627e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  406285:	00 00 
  406287:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  40628b:	c1 e2 14             	shl    $0x14,%edx
  40628e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  406292:	25 ff ff 0f 00       	and    $0xfffff,%eax
  406297:	89 c1                	mov    %eax,%ecx
  406299:	89 d0                	mov    %edx,%eax
  40629b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  4062a2:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  4062a6:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4062aa:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  4062ae:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  4062b4:	5b                   	pop    %rbx
  4062b5:	c3                   	ret
  4062b6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4062bd:	00 00 00 

00000000004062c0 <__umodti3>:
  4062c0:	48 83 ec 58          	sub    $0x58,%rsp
  4062c4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4062c8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4062cd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4062d2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4062d7:	48 8b 0c 24          	mov    (%rsp),%rcx
  4062db:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4062e0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4062e5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4062ea:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  4062ef:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  4062f4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4062f9:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4062fe:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  406303:	e8 78 b6 ff ff       	call   401980 <runtime::udivmod128>
  406308:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40630d:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406312:	48 83 c4 58          	add    $0x58,%rsp
  406316:	c3                   	ret
  406317:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40631e:	00 00 

0000000000406320 <__udivmodti4>:
  406320:	48 83 ec 58          	sub    $0x58,%rsp
  406324:	4c 89 04 24          	mov    %r8,(%rsp)
  406328:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40632d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  406332:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  406337:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40633c:	4c 8b 04 24          	mov    (%rsp),%r8
  406340:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406345:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40634a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40634f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406354:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  406359:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40635e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  406363:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406368:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40636d:	e8 0e b6 ff ff       	call   401980 <runtime::udivmod128>
  406372:	48 83 c4 58          	add    $0x58,%rsp
  406376:	c3                   	ret
  406377:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40637e:	00 00 

0000000000406380 <__udivti3>:
  406380:	48 83 ec 48          	sub    $0x48,%rsp
  406384:	48 89 0c 24          	mov    %rcx,(%rsp)
  406388:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40638d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406392:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406397:	48 8b 0c 24          	mov    (%rsp),%rcx
  40639b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4063a0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4063a5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4063aa:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  4063af:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4063b4:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4063b9:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4063be:	31 c0                	xor    %eax,%eax
  4063c0:	41 89 c0             	mov    %eax,%r8d
  4063c3:	e8 58 ff ff ff       	call   406320 <__udivmodti4>
  4063c8:	48 83 c4 48          	add    $0x48,%rsp
  4063cc:	c3                   	ret
  4063cd:	0f 1f 00             	nopl   (%rax)

00000000004063d0 <runtime::assert>:
  4063d0:	48 83 ec 48          	sub    $0x48,%rsp
  4063d4:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  4063d9:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4063de:	40 88 f8             	mov    %dil,%al
  4063e1:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  4063e5:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4063ea:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  4063ef:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  4063f3:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4063f8:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4063fd:	88 44 24 47          	mov    %al,0x47(%rsp)
  406401:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  406406:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40640b:	3c 00                	cmp    $0x0,%al
  40640d:	75 19                	jne    406428 <runtime::assert+0x58>
  40640f:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406414:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  406419:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40641e:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406423:	e8 18 0a 00 00       	call   406e40 <runtime::assert.internal-0>
  406428:	48 83 c4 48          	add    $0x48,%rsp
  40642c:	c3                   	ret
  40642d:	0f 1f 00             	nopl   (%rax)

0000000000406430 <runtime::heap_allocator_proc.aligned_alloc-0>:
  406430:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  406437:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40643c:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406441:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  406446:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40644b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  406450:	44 88 c0             	mov    %r8b,%al
  406453:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  406457:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40645e:	00 
  40645f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  406464:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406469:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40646e:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  406473:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  406477:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40647c:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  406483:	00 
  406484:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40648b:	00 
  40648c:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  406493:	00 
  406494:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  40649b:	00 
  40649c:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  4064a3:	b9 08 00 00 00       	mov    $0x8,%ecx
  4064a8:	48 83 fe 08          	cmp    $0x8,%rsi
  4064ac:	48 0f 4f ce          	cmovg  %rsi,%rcx
  4064b0:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  4064b7:	00 
  4064b8:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  4064bf:	00 
  4064c0:	48 83 e9 01          	sub    $0x1,%rcx
  4064c4:	48 83 c1 08          	add    $0x8,%rcx
  4064c8:	48 01 d1             	add    %rdx,%rcx
  4064cb:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  4064d2:	00 
  4064d3:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  4064da:	00 00 
  4064dc:	48 83 f8 00          	cmp    $0x0,%rax
  4064e0:	0f 95 c1             	setne  %cl
  4064e3:	80 e1 01             	and    $0x1,%cl
  4064e6:	31 c0                	xor    %eax,%eax
  4064e8:	80 f9 00             	cmp    $0x0,%cl
  4064eb:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4064ef:	74 17                	je     406508 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  4064f1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4064f6:	48 83 f8 08          	cmp    $0x8,%rax
  4064fa:	0f 9f c0             	setg   %al
  4064fd:	24 01                	and    $0x1,%al
  4064ff:	3c 00                	cmp    $0x0,%al
  406501:	0f 95 c0             	setne  %al
  406504:	88 44 24 0f          	mov    %al,0xf(%rsp)
  406508:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40650d:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  406511:	80 e1 01             	and    $0x1,%cl
  406514:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  406518:	48 83 f8 00          	cmp    $0x0,%rax
  40651c:	0f 95 c0             	setne  %al
  40651f:	24 01                	and    $0x1,%al
  406521:	3c 00                	cmp    $0x0,%al
  406523:	74 2e                	je     406553 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  406525:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40652a:	75 27                	jne    406553 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  40652c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406531:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  406535:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40653a:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40653f:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  406546:	00 
  406547:	e8 64 db ff ff       	call   4040b0 <runtime::heap_resize>
  40654c:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  406551:	eb 19                	jmp    40656c <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  406553:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  406557:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  40655e:	00 
  40655f:	0f b6 f0             	movzbl %al,%esi
  406562:	e8 19 db ff ff       	call   404080 <runtime::heap_alloc>
  406567:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40656c:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  406571:	48 83 c0 08          	add    $0x8,%rax
  406575:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40657a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40657f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  406584:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  406589:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40658e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  406593:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  40659a:	00 
  40659b:	48 83 e8 01          	sub    $0x1,%rax
  40659f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  4065a6:	00 
  4065a7:	48 83 e9 01          	sub    $0x1,%rcx
  4065ab:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  4065af:	48 21 c8             	and    %rcx,%rax
  4065b2:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4065b7:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  4065bd:	0f 94 c0             	sete   %al
  4065c0:	24 01                	and    $0x1,%al
  4065c2:	3c 00                	cmp    $0x0,%al
  4065c4:	74 3c                	je     406602 <runtime::heap_allocator_proc.aligned_alloc-0+0x1d2>
  4065c6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4065cb:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4065d0:	e8 ab 00 00 00       	call   406680 <runtime::heap_allocator_proc.aligned_free-1>
  4065d5:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4065da:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  4065df:	e8 9c 00 00 00       	call   406680 <runtime::heap_allocator_proc.aligned_free-1>
  4065e4:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4065e9:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4065f0:	00 
  4065f1:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4065f8:	b0 01                	mov    $0x1,%al
  4065fa:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  406601:	c3                   	ret
  406602:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  406607:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40660c:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  406611:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  406616:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  40661a:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40661f:	74 2f                	je     406650 <runtime::heap_allocator_proc.aligned_alloc-0+0x220>
  406621:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406626:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40662b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  406630:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  406635:	48 39 d0             	cmp    %rdx,%rax
  406638:	48 0f 4c d0          	cmovl  %rax,%rdx
  40663c:	e8 3f d0 ff ff       	call   403680 <runtime::mem_copy_non_overlapping>
  406641:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406646:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40664b:	e8 30 00 00 00       	call   406680 <runtime::heap_allocator_proc.aligned_free-1>
  406650:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406655:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  40665a:	e8 c1 bf ff ff       	call   402620 <runtime::[internal.odin]::byte_slice>
  40665f:	48 89 c1             	mov    %rax,%rcx
  406662:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406667:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40666b:	48 89 08             	mov    %rcx,(%rax)
  40666e:	31 c0                	xor    %eax,%eax
  406670:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  406677:	c3                   	ret
  406678:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40667f:	00 

0000000000406680 <runtime::heap_allocator_proc.aligned_free-1>:
  406680:	48 83 ec 18          	sub    $0x18,%rsp
  406684:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406689:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40668e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406693:	48 83 f8 00          	cmp    $0x0,%rax
  406697:	0f 95 c0             	setne  %al
  40669a:	24 01                	and    $0x1,%al
  40669c:	3c 00                	cmp    $0x0,%al
  40669e:	74 0e                	je     4066ae <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  4066a0:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4066a5:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  4066a9:	e8 32 da ff ff       	call   4040e0 <runtime::heap_free>
  4066ae:	48 83 c4 18          	add    $0x18,%rsp
  4066b2:	c3                   	ret
  4066b3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4066ba:	84 00 00 00 00 00 

00000000004066c0 <runtime::heap_allocator_proc.aligned_resize-2>:
  4066c0:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  4066c7:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  4066cc:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4066d1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4066d6:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4066db:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  4066e0:	44 88 c0             	mov    %r8b,%al
  4066e3:	88 44 24 57          	mov    %al,0x57(%rsp)
  4066e7:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4066ee:	00 
  4066ef:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4066f4:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4066f9:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  4066fd:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  406702:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  406707:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40670c:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  406713:	00 
  406714:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  40671b:	00 
  40671c:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  406723:	00 
  406724:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  40672b:	00 
  40672c:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  406733:	0f 57 c0             	xorps  %xmm0,%xmm0
  406736:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  40673d:	00 
  40673e:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  406745:	00 
  406746:	48 83 f8 00          	cmp    $0x0,%rax
  40674a:	0f 94 c0             	sete   %al
  40674d:	24 01                	and    $0x1,%al
  40674f:	3c 00                	cmp    $0x0,%al
  406751:	0f 84 80 00 00 00    	je     4067d7 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  406757:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40675c:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406761:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406766:	8a 44 24 57          	mov    0x57(%rsp),%al
  40676a:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  40676f:	0f 57 c0             	xorps  %xmm0,%xmm0
  406772:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  406779:	00 
  40677a:	48 89 e2             	mov    %rsp,%rdx
  40677d:	4c 89 02             	mov    %r8,(%rdx)
  406780:	44 0f b6 c0          	movzbl %al,%r8d
  406784:	31 c0                	xor    %eax,%eax
  406786:	89 c2                	mov    %eax,%edx
  406788:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  40678f:	00 
  406790:	e8 9b fc ff ff       	call   406430 <runtime::heap_allocator_proc.aligned_alloc-0>
  406795:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40679a:	40 88 c7             	mov    %al,%dil
  40679d:	40 88 f8             	mov    %dil,%al
  4067a0:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  4067a7:	00 
  4067a8:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  4067af:	00 
  4067b0:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  4067b7:	00 
  4067b8:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4067bf:	00 
  4067c0:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  4067c7:	00 
  4067c8:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4067cc:	48 89 11             	mov    %rdx,(%rcx)
  4067cf:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4067d6:	c3                   	ret
  4067d7:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4067dc:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4067e1:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4067e6:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4067eb:	8a 44 24 57          	mov    0x57(%rsp),%al
  4067ef:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  4067f4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4067f7:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4067fe:	00 
  4067ff:	49 89 e0             	mov    %rsp,%r8
  406802:	4d 89 08             	mov    %r9,(%r8)
  406805:	44 0f b6 c0          	movzbl %al,%r8d
  406809:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  406810:	00 
  406811:	e8 1a fc ff ff       	call   406430 <runtime::heap_allocator_proc.aligned_alloc-0>
  406816:	88 44 24 17          	mov    %al,0x17(%rsp)
  40681a:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  406821:	00 
  406822:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406827:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40682e:	00 
  40682f:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  406834:	3c 00                	cmp    $0x0,%al
  406836:	74 4d                	je     406885 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  406838:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40683d:	8a 44 24 17          	mov    0x17(%rsp),%al
  406841:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406848:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40684f:	00 
  406850:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406857:	00 
  406858:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40685f:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406866:	00 
  406867:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40686e:	00 
  40686f:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406876:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40687a:	48 89 11             	mov    %rdx,(%rcx)
  40687d:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406884:	c3                   	ret
  406885:	8a 44 24 57          	mov    0x57(%rsp),%al
  406889:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40688e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406893:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40689a:	00 
  40689b:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  4068a2:	00 
  4068a3:	3c 00                	cmp    $0x0,%al
  4068a5:	0f 84 87 00 00 00    	je     406932 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  4068ab:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4068b0:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4068b5:	48 39 c8             	cmp    %rcx,%rax
  4068b8:	0f 9f c0             	setg   %al
  4068bb:	24 01                	and    $0x1,%al
  4068bd:	3c 00                	cmp    $0x0,%al
  4068bf:	74 71                	je     406932 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  4068c1:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  4068c6:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  4068cd:	00 
  4068ce:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4068d3:	48 89 e0             	mov    %rsp,%rax
  4068d6:	4c 89 08             	mov    %r9,(%rax)
  4068d9:	bf 47 73 40 00       	mov    $0x407347,%edi
  4068de:	be 30 00 00 00       	mov    $0x30,%esi
  4068e3:	ba 4b 00 00 00       	mov    $0x4b,%edx
  4068e8:	b9 25 00 00 00       	mov    $0x25,%ecx
  4068ed:	e8 1e ce ff ff       	call   403710 <runtime::slice_expr_error_lo_hi>
  4068f2:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4068f7:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4068fc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  406901:	48 89 c6             	mov    %rax,%rsi
  406904:	48 03 b4 24 e0 00 00 	add    0xe0(%rsp),%rsi
  40690b:	00 
  40690c:	48 29 c1             	sub    %rax,%rcx
  40690f:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  406914:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  406919:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40691e:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  406923:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  406928:	48 29 c2             	sub    %rax,%rdx
  40692b:	31 f6                	xor    %esi,%esi
  40692d:	e8 0e a7 ff ff       	call   401040 <memset@plt>
  406932:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  406937:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40693e:	00 
  40693f:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406946:	00 
  406947:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40694e:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406955:	00 
  406956:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40695d:	00 
  40695e:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406965:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  406969:	48 89 11             	mov    %rdx,(%rcx)
  40696c:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406973:	c3                   	ret
  406974:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40697b:	00 00 00 00 00 

0000000000406980 <runtime::bounds_check_error.handle_error-0>:
  406980:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  406987:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40698c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406991:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406995:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406999:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40699e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4069a3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4069a8:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4069ad:	8b 44 24 18          	mov    0x18(%rsp),%eax
  4069b1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4069b5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4069ba:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4069bf:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4069c4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  4069cb:	00 
  4069cc:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  4069d0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  4069d4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  4069d9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  4069de:	0f 57 c0             	xorps  %xmm0,%xmm0
  4069e1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4069e6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4069eb:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4069f2:	00 00 
  4069f4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4069f9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4069fe:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  406a05:	00 00 
  406a07:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  406a0c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406a11:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  406a15:	89 44 24 44          	mov    %eax,0x44(%rsp)
  406a19:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  406a1e:	e8 cd da ff ff       	call   4044f0 <runtime::print_caller_location>
  406a23:	bf 78 73 40 00       	mov    $0x407378,%edi
  406a28:	be 07 00 00 00       	mov    $0x7,%esi
  406a2d:	e8 9e d2 ff ff       	call   403cd0 <runtime::print_string>
  406a32:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406a37:	e8 b4 d8 ff ff       	call   4042f0 <runtime::print_i64>
  406a3c:	bf c3 72 40 00       	mov    $0x4072c3,%edi
  406a41:	be 15 00 00 00       	mov    $0x15,%esi
  406a46:	e8 85 d2 ff ff       	call   403cd0 <runtime::print_string>
  406a4b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406a50:	e8 9b d8 ff ff       	call   4042f0 <runtime::print_i64>
  406a55:	bf 0a 00 00 00       	mov    $0xa,%edi
  406a5a:	e8 a1 d4 ff ff       	call   403f00 <runtime::print_byte>
  406a5f:	e8 ec ae ff ff       	call   401950 <runtime::bounds_trap>
  406a64:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  406a6b:	00 00 00 00 00 

0000000000406a70 <runtime::default_random_generator_proc.read_u64-0>:
  406a70:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  406a75:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406a7a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406a7f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406a84:	48 8b 00             	mov    (%rax),%rax
  406a87:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406a8c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406a91:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  406a98:	f4 51 58 
  406a9b:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  406aa1:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406aa6:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  406aaa:	48 83 ca 01          	or     $0x1,%rdx
  406aae:	48 01 d1             	add    %rdx,%rcx
  406ab1:	48 89 08             	mov    %rcx,(%rax)
  406ab4:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406ab9:	48 c1 e9 3b          	shr    $0x3b,%rcx
  406abd:	b2 01                	mov    $0x1,%dl
  406abf:	31 c0                	xor    %eax,%eax
  406ac1:	f6 c2 01             	test   $0x1,%dl
  406ac4:	48 0f 45 c1          	cmovne %rcx,%rax
  406ac8:	48 83 c0 05          	add    $0x5,%rax
  406acc:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  406ad1:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  406ad8:	75 f1 ae 
  406adb:	48 0f af c1          	imul   %rcx,%rax
  406adf:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406ae4:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406ae9:	48 c1 e9 3b          	shr    $0x3b,%rcx
  406aed:	b2 01                	mov    $0x1,%dl
  406aef:	31 c0                	xor    %eax,%eax
  406af1:	f6 c2 01             	test   $0x1,%dl
  406af4:	48 0f 45 c1          	cmovne %rcx,%rax
  406af8:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406afd:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406b02:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  406b07:	48 89 d1             	mov    %rdx,%rcx
  406b0a:	48 d3 e8             	shr    %cl,%rax
  406b0d:	48 89 c1             	mov    %rax,%rcx
  406b10:	31 c0                	xor    %eax,%eax
  406b12:	48 83 fa 40          	cmp    $0x40,%rdx
  406b16:	48 0f 42 c1          	cmovb  %rcx,%rax
  406b1a:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  406b1f:	31 c9                	xor    %ecx,%ecx
  406b21:	89 ce                	mov    %ecx,%esi
  406b23:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  406b28:	48 83 e6 3f          	and    $0x3f,%rsi
  406b2c:	48 89 f1             	mov    %rsi,%rcx
  406b2f:	48 d3 e2             	shl    %cl,%rdx
  406b32:	31 c9                	xor    %ecx,%ecx
  406b34:	48 83 fe 40          	cmp    $0x40,%rsi
  406b38:	48 0f 42 ca          	cmovb  %rdx,%rcx
  406b3c:	48 09 c8             	or     %rcx,%rax
  406b3f:	c3                   	ret

0000000000406b40 <runtime::default_random_generator_proc.init-1>:
  406b40:	48 83 ec 28          	sub    $0x28,%rsp
  406b44:	48 89 3c 24          	mov    %rdi,(%rsp)
  406b48:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  406b4d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406b52:	48 8b 0c 24          	mov    (%rsp),%rcx
  406b56:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  406b5b:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  406b60:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406b65:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  406b6b:	0f 94 c0             	sete   %al
  406b6e:	24 01                	and    $0x1,%al
  406b70:	3c 00                	cmp    $0x0,%al
  406b72:	74 0e                	je     406b82 <runtime::default_random_generator_proc.init-1+0x42>
  406b74:	0f 31                	rdtsc
  406b76:	48 c1 e2 20          	shl    $0x20,%rdx
  406b7a:	48 09 d0             	or     %rdx,%rax
  406b7d:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406b82:	48 8b 3c 24          	mov    (%rsp),%rdi
  406b86:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406b8b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  406b92:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406b97:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  406b9c:	48 d1 e2             	shl    $1,%rdx
  406b9f:	40 b6 01             	mov    $0x1,%sil
  406ba2:	31 c9                	xor    %ecx,%ecx
  406ba4:	40 f6 c6 01          	test   $0x1,%sil
  406ba8:	48 0f 45 ca          	cmovne %rdx,%rcx
  406bac:	48 83 c9 01          	or     $0x1,%rcx
  406bb0:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406bb4:	e8 b7 fe ff ff       	call   406a70 <runtime::default_random_generator_proc.read_u64-0>
  406bb9:	48 8b 3c 24          	mov    (%rsp),%rdi
  406bbd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406bc2:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406bc7:	48 03 08             	add    (%rax),%rcx
  406bca:	48 89 08             	mov    %rcx,(%rax)
  406bcd:	e8 9e fe ff ff       	call   406a70 <runtime::default_random_generator_proc.read_u64-0>
  406bd2:	48 83 c4 28          	add    $0x28,%rsp
  406bd6:	c3                   	ret
  406bd7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  406bde:	00 00 

0000000000406be0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  406be0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  406be5:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  406bea:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406bef:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  406bf4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  406bf9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406bfe:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  406c05:	00 00 
  406c07:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  406c0c:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  406c10:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406c15:	48 03 4a 20          	add    0x20(%rdx),%rcx
  406c19:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  406c1e:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406c23:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406c28:	48 83 e8 01          	sub    $0x1,%rax
  406c2c:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  406c31:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406c36:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  406c3b:	48 83 f8 00          	cmp    $0x0,%rax
  406c3f:	0f 95 c0             	setne  %al
  406c42:	24 01                	and    $0x1,%al
  406c44:	3c 00                	cmp    $0x0,%al
  406c46:	74 17                	je     406c5f <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  406c48:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406c4d:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406c52:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  406c57:	48 29 c8             	sub    %rcx,%rax
  406c5a:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406c5f:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406c64:	c3                   	ret
  406c65:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  406c6c:	00 00 00 00 

0000000000406c70 <runtime::arena_alloc.align_forward_uint-0>:
  406c70:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  406c75:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  406c7a:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406c7f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406c84:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406c89:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  406c8e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406c93:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406c98:	48 83 e9 01          	sub    $0x1,%rcx
  406c9c:	48 21 c8             	and    %rcx,%rax
  406c9f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406ca4:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  406caa:	0f 95 c0             	setne  %al
  406cad:	24 01                	and    $0x1,%al
  406caf:	3c 00                	cmp    $0x0,%al
  406cb1:	74 14                	je     406cc7 <runtime::arena_alloc.align_forward_uint-0+0x57>
  406cb3:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406cb8:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  406cbd:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  406cc2:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406cc7:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406ccc:	c3                   	ret
  406ccd:	0f 1f 00             	nopl   (%rax)

0000000000406cd0 <runtime::matrix_bounds_check_error.handle_error-0>:
  406cd0:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  406cd7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406cdc:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406ce1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406ce5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406ce9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  406cee:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406cf3:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  406cfa:	00 
  406cfb:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406d00:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  406d07:	00 
  406d08:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406d0d:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406d12:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  406d17:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  406d1c:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  406d21:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406d25:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406d29:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406d2e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406d33:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  406d3a:	00 
  406d3b:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  406d42:	00 
  406d43:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  406d4a:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  406d51:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  406d58:	00 
  406d59:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  406d60:	00 
  406d61:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  406d66:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  406d6b:	0f 57 c0             	xorps  %xmm0,%xmm0
  406d6e:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406d73:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406d78:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406d7f:	00 00 
  406d81:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406d86:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406d8b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406d92:	00 00 
  406d94:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  406d99:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  406d9e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  406da2:	89 44 24 54          	mov    %eax,0x54(%rsp)
  406da6:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  406dab:	e8 40 d7 ff ff       	call   4044f0 <runtime::print_caller_location>
  406db0:	bf 80 73 40 00       	mov    $0x407380,%edi
  406db5:	be 11 00 00 00       	mov    $0x11,%esi
  406dba:	e8 11 cf ff ff       	call   403cd0 <runtime::print_string>
  406dbf:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406dc4:	e8 27 d5 ff ff       	call   4042f0 <runtime::print_i64>
  406dc9:	bf 92 73 40 00       	mov    $0x407392,%edi
  406dce:	be 02 00 00 00       	mov    $0x2,%esi
  406dd3:	e8 f8 ce ff ff       	call   403cd0 <runtime::print_string>
  406dd8:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406ddd:	e8 0e d5 ff ff       	call   4042f0 <runtime::print_i64>
  406de2:	bf 95 73 40 00       	mov    $0x407395,%edi
  406de7:	be 16 00 00 00       	mov    $0x16,%esi
  406dec:	e8 df ce ff ff       	call   403cd0 <runtime::print_string>
  406df1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406df6:	e8 f5 d4 ff ff       	call   4042f0 <runtime::print_i64>
  406dfb:	bf ac 73 40 00       	mov    $0x4073ac,%edi
  406e00:	be 06 00 00 00       	mov    $0x6,%esi
  406e05:	e8 c6 ce ff ff       	call   403cd0 <runtime::print_string>
  406e0a:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406e0f:	e8 dc d4 ff ff       	call   4042f0 <runtime::print_i64>
  406e14:	bf b3 73 40 00       	mov    $0x4073b3,%edi
  406e19:	be 01 00 00 00       	mov    $0x1,%esi
  406e1e:	e8 ad ce ff ff       	call   403cd0 <runtime::print_string>
  406e23:	bf 0a 00 00 00       	mov    $0xa,%edi
  406e28:	e8 d3 d0 ff ff       	call   403f00 <runtime::print_byte>
  406e2d:	e8 1e ab ff ff       	call   401950 <runtime::bounds_trap>
  406e32:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  406e39:	1f 84 00 00 00 00 00 

0000000000406e40 <runtime::assert.internal-0>:
  406e40:	48 83 ec 38          	sub    $0x38,%rsp
  406e44:	48 89 0c 24          	mov    %rcx,(%rsp)
  406e48:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  406e4d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406e52:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406e57:	48 8b 04 24          	mov    (%rsp),%rax
  406e5b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406e60:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406e65:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  406e6a:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406e6f:	48 8b 40 20          	mov    0x20(%rax),%rax
  406e73:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406e78:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  406e7e:	0f 94 c0             	sete   %al
  406e81:	24 01                	and    $0x1,%al
  406e83:	3c 00                	cmp    $0x0,%al
  406e85:	74 0c                	je     406e93 <runtime::assert.internal-0+0x53>
  406e87:	48 c7 c0 80 57 40 00 	mov    $0x405780,%rax
  406e8e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406e93:	4c 8b 0c 24          	mov    (%rsp),%r9
  406e97:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  406e9c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406ea1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406ea6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406eab:	bf b5 73 40 00       	mov    $0x4073b5,%edi
  406eb0:	be 11 00 00 00       	mov    $0x11,%esi
  406eb5:	ff d0                	call   *%rax

Disassembly of section .fini:

0000000000406eb8 <_fini>:
  406eb8:	f3 0f 1e fa          	endbr64
  406ebc:	48 83 ec 08          	sub    $0x8,%rsp
  406ec0:	48 83 c4 08          	add    $0x8,%rsp
  406ec4:	c3                   	ret
