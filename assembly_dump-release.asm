
/home/khalid/Documents/GitHub/Journey_ECS/main-release.bin:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 c1 9f 00 00 	mov    0x9fc1(%rip),%rax        # 40afd0 <__gmon_start__@Base>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 9f 00 00    	push   0x9fca(%rip)        # 40aff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 9f 00 00    	jmp    *0x9fcc(%rip)        # 40aff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 9f 00 00    	jmp    *0x9fca(%rip)        # 40b000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <memset@plt>:
  401040:	ff 25 c2 9f 00 00    	jmp    *0x9fc2(%rip)        # 40b008 <memset@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <calloc@plt>:
  401050:	ff 25 ba 9f 00 00    	jmp    *0x9fba(%rip)        # 40b010 <calloc@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <memcpy@plt>:
  401060:	ff 25 b2 9f 00 00    	jmp    *0x9fb2(%rip)        # 40b018 <memcpy@GLIBC_2.14>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <malloc@plt>:
  401070:	ff 25 aa 9f 00 00    	jmp    *0x9faa(%rip)        # 40b020 <malloc@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <realloc@plt>:
  401080:	ff 25 a2 9f 00 00    	jmp    *0x9fa2(%rip)        # 40b028 <realloc@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <memmove@plt>:
  401090:	ff 25 9a 9f 00 00    	jmp    *0x9f9a(%rip)        # 40b030 <memmove@GLIBC_2.2.5>
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
  4010b8:	48 c7 c7 b0 32 40 00 	mov    $0x4032b0,%rdi
  4010bf:	ff 15 fb 9e 00 00    	call   *0x9efb(%rip)        # 40afc0 <__libc_start_main@GLIBC_2.34>
  4010c5:	f4                   	hlt
  4010c6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4010cd:	00 00 00 

00000000004010d0 <_dl_relocate_static_pie>:
  4010d0:	f3 0f 1e fa          	endbr64
  4010d4:	c3                   	ret
  4010d5:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4010dc:	00 00 00 
  4010df:	90                   	nop
  4010e0:	b8 58 b0 40 00       	mov    $0x40b058,%eax
  4010e5:	48 3d 58 b0 40 00    	cmp    $0x40b058,%rax
  4010eb:	74 13                	je     401100 <_dl_relocate_static_pie+0x30>
  4010ed:	48 8b 05 d4 9e 00 00 	mov    0x9ed4(%rip),%rax        # 40afc8 <_ITM_deregisterTMCloneTable@Base>
  4010f4:	48 85 c0             	test   %rax,%rax
  4010f7:	74 07                	je     401100 <_dl_relocate_static_pie+0x30>
  4010f9:	bf 58 b0 40 00       	mov    $0x40b058,%edi
  4010fe:	ff e0                	jmp    *%rax
  401100:	c3                   	ret
  401101:	0f 1f 40 00          	nopl   0x0(%rax)
  401105:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40110c:	00 00 00 00 
  401110:	be 58 b0 40 00       	mov    $0x40b058,%esi
  401115:	48 81 ee 58 b0 40 00 	sub    $0x40b058,%rsi
  40111c:	48 89 f0             	mov    %rsi,%rax
  40111f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401123:	48 c1 f8 03          	sar    $0x3,%rax
  401127:	48 01 c6             	add    %rax,%rsi
  40112a:	48 d1 fe             	sar    $1,%rsi
  40112d:	74 19                	je     401148 <_dl_relocate_static_pie+0x78>
  40112f:	48 8b 05 a2 9e 00 00 	mov    0x9ea2(%rip),%rax        # 40afd8 <_ITM_registerTMCloneTable@Base>
  401136:	48 85 c0             	test   %rax,%rax
  401139:	74 0d                	je     401148 <_dl_relocate_static_pie+0x78>
  40113b:	bf 58 b0 40 00       	mov    $0x40b058,%edi
  401140:	ff e0                	jmp    *%rax
  401142:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401148:	c3                   	ret
  401149:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
  401150:	f3 0f 1e fa          	endbr64
  401154:	80 3d fd 9e 00 00 00 	cmpb   $0x0,0x9efd(%rip)        # 40b058 <__TMC_END__>
  40115b:	75 13                	jne    401170 <_dl_relocate_static_pie+0xa0>
  40115d:	55                   	push   %rbp
  40115e:	48 89 e5             	mov    %rsp,%rbp
  401161:	e8 7a ff ff ff       	call   4010e0 <_dl_relocate_static_pie+0x10>
  401166:	c6 05 eb 9e 00 00 01 	movb   $0x1,0x9eeb(%rip)        # 40b058 <__TMC_END__>
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

00000000004018f0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  4018f0:	50                   	push   %rax
  4018f1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4018f5:	eb 00                	jmp    4018f7 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x7>
  4018f7:	48 8b 34 24          	mov    (%rsp),%rsi
  4018fb:	48 c7 c0 c0 ff ff ff 	mov    $0xffffffffffffffc0,%rax
  401902:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  401909:	00 00 
  40190b:	48 01 c7             	add    %rax,%rdi
  40190e:	e8 dd 12 00 00       	call   402bf0 <runtime::default_temp_allocator_destroy>
  401913:	58                   	pop    %rax
  401914:	c3                   	ret
  401915:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40191c:	00 00 00 00 

0000000000401920 <runtime::bounds_trap>:
  401920:	eb 00                	jmp    401922 <runtime::bounds_trap+0x2>
  401922:	0f 0b                	ud2
  401924:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40192b:	00 00 00 00 00 

0000000000401930 <runtime::heap_allocator>:
  401930:	48 c7 c0 d0 23 40 00 	mov    $0x4023d0,%rax
  401937:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40193c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401943:	00 00 
  401945:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40194a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40194f:	c3                   	ret

0000000000401950 <runtime::udivmod128>:
  401950:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  401957:	4c 89 44 24 a8       	mov    %r8,-0x58(%rsp)
  40195c:	48 89 4c 24 b0       	mov    %rcx,-0x50(%rsp)
  401961:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401966:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40196b:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  401970:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401975:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40197a:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40197f:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  401984:	48 8b 7c 24 a8       	mov    -0x58(%rsp),%rdi
  401989:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  401990:	00 
  401991:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  401998:	00 
  401999:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  4019a0:	00 
  4019a1:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4019a8:	00 
  4019a9:	48 89 bc 24 88 00 00 	mov    %rdi,0x88(%rsp)
  4019b0:	00 
  4019b1:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4019b6:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  4019bb:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4019c0:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4019c5:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4019ca:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  4019cf:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4019d4:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4019d9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4019de:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4019e3:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4019e8:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4019ed:	0f 57 c0             	xorps  %xmm0,%xmm0
  4019f0:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4019f5:	0f 57 c0             	xorps  %xmm0,%xmm0
  4019f8:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  4019fd:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  401a04:	00 
  401a05:	48 83 7c 24 68 00    	cmpq   $0x0,0x68(%rsp)
  401a0b:	0f 94 c0             	sete   %al
  401a0e:	24 01                	and    $0x1,%al
  401a10:	3c 00                	cmp    $0x0,%al
  401a12:	0f 84 a1 00 00 00    	je     401ab9 <runtime::udivmod128+0x169>
  401a18:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401a1e:	0f 94 c0             	sete   %al
  401a21:	24 01                	and    $0x1,%al
  401a23:	3c 00                	cmp    $0x0,%al
  401a25:	74 5c                	je     401a83 <runtime::udivmod128+0x133>
  401a27:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a2c:	48 83 f8 00          	cmp    $0x0,%rax
  401a30:	0f 95 c0             	setne  %al
  401a33:	24 01                	and    $0x1,%al
  401a35:	3c 00                	cmp    $0x0,%al
  401a37:	74 29                	je     401a62 <runtime::udivmod128+0x112>
  401a39:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401a3e:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401a43:	31 d2                	xor    %edx,%edx
  401a45:	48 f7 f1             	div    %rcx
  401a48:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a4d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  401a52:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401a57:	48 89 08             	mov    %rcx,(%rax)
  401a5a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401a61:	00 
  401a62:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401a67:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401a6c:	31 d2                	xor    %edx,%edx
  401a6e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  401a73:	48 f7 f1             	div    %rcx
  401a76:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  401a7b:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401a82:	c3                   	ret
  401a83:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a88:	48 83 f8 00          	cmp    $0x0,%rax
  401a8c:	0f 95 c0             	setne  %al
  401a8f:	24 01                	and    $0x1,%al
  401a91:	3c 00                	cmp    $0x0,%al
  401a93:	74 15                	je     401aaa <runtime::udivmod128+0x15a>
  401a95:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a9a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401a9f:	48 89 08             	mov    %rcx,(%rax)
  401aa2:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401aa9:	00 
  401aaa:	31 c0                	xor    %eax,%eax
  401aac:	89 c2                	mov    %eax,%edx
  401aae:	48 89 d0             	mov    %rdx,%rax
  401ab1:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401ab8:	c3                   	ret
  401ab9:	48 83 7c 24 40 00    	cmpq   $0x0,0x40(%rsp)
  401abf:	0f 94 c0             	sete   %al
  401ac2:	24 01                	and    $0x1,%al
  401ac4:	3c 00                	cmp    $0x0,%al
  401ac6:	0f 84 8b 02 00 00    	je     401d57 <runtime::udivmod128+0x407>
  401acc:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401ad2:	0f 94 c0             	sete   %al
  401ad5:	24 01                	and    $0x1,%al
  401ad7:	3c 00                	cmp    $0x0,%al
  401ad9:	74 52                	je     401b2d <runtime::udivmod128+0x1dd>
  401adb:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401ae0:	48 83 f8 00          	cmp    $0x0,%rax
  401ae4:	0f 95 c0             	setne  %al
  401ae7:	24 01                	and    $0x1,%al
  401ae9:	3c 00                	cmp    $0x0,%al
  401aeb:	74 1f                	je     401b0c <runtime::udivmod128+0x1bc>
  401aed:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401af2:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401af7:	31 d2                	xor    %edx,%edx
  401af9:	48 f7 f1             	div    %rcx
  401afc:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b01:	48 89 10             	mov    %rdx,(%rax)
  401b04:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401b0b:	00 
  401b0c:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b11:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401b16:	31 d2                	xor    %edx,%edx
  401b18:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  401b1d:	48 f7 f1             	div    %rcx
  401b20:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  401b25:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401b2c:	c3                   	ret
  401b2d:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  401b33:	0f 94 c0             	sete   %al
  401b36:	24 01                	and    $0x1,%al
  401b38:	3c 00                	cmp    $0x0,%al
  401b3a:	74 66                	je     401ba2 <runtime::udivmod128+0x252>
  401b3c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b41:	48 83 f8 00          	cmp    $0x0,%rax
  401b45:	0f 95 c0             	setne  %al
  401b48:	24 01                	and    $0x1,%al
  401b4a:	3c 00                	cmp    $0x0,%al
  401b4c:	74 33                	je     401b81 <runtime::udivmod128+0x231>
  401b4e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b53:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401b58:	31 d2                	xor    %edx,%edx
  401b5a:	48 f7 f1             	div    %rcx
  401b5d:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b62:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401b67:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  401b6e:	00 00 
  401b70:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401b75:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401b7a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401b7e:	48 89 08             	mov    %rcx,(%rax)
  401b81:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b86:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401b8b:	31 d2                	xor    %edx,%edx
  401b8d:	48 89 54 24 90       	mov    %rdx,-0x70(%rsp)
  401b92:	48 f7 f1             	div    %rcx
  401b95:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  401b9a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401ba1:	c3                   	ret
  401ba2:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401ba7:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401bac:	48 83 e9 01          	sub    $0x1,%rcx
  401bb0:	48 21 c8             	and    %rcx,%rax
  401bb3:	48 83 f8 00          	cmp    $0x0,%rax
  401bb7:	0f 94 c0             	sete   %al
  401bba:	24 01                	and    $0x1,%al
  401bbc:	3c 00                	cmp    $0x0,%al
  401bbe:	74 7a                	je     401c3a <runtime::udivmod128+0x2ea>
  401bc0:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401bc5:	48 83 f8 00          	cmp    $0x0,%rax
  401bc9:	0f 95 c0             	setne  %al
  401bcc:	24 01                	and    $0x1,%al
  401bce:	3c 00                	cmp    $0x0,%al
  401bd0:	74 35                	je     401c07 <runtime::udivmod128+0x2b7>
  401bd2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401bd7:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401bdc:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401be1:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  401be6:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  401beb:	48 ff ca             	dec    %rdx
  401bee:	48 21 d1             	and    %rdx,%rcx
  401bf1:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401bf6:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401bfb:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401c00:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401c04:	48 89 08             	mov    %rcx,(%rax)
  401c07:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401c0c:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401c11:	ba 40 00 00 00       	mov    $0x40,%edx
  401c16:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  401c1b:	88 d1                	mov    %dl,%cl
  401c1d:	48 d3 e8             	shr    %cl,%rax
  401c20:	48 89 c1             	mov    %rax,%rcx
  401c23:	31 c0                	xor    %eax,%eax
  401c25:	48 83 ea 40          	sub    $0x40,%rdx
  401c29:	89 c2                	mov    %eax,%edx
  401c2b:	48 89 d0             	mov    %rdx,%rax
  401c2e:	48 0f 42 c1          	cmovb  %rcx,%rax
  401c32:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401c39:	c3                   	ret
  401c3a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401c3f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401c44:	48 0f bd c1          	bsr    %rcx,%rax
  401c48:	48 83 f0 3f          	xor    $0x3f,%rax
  401c4c:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401c51:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401c56:	48 0f bd ca          	bsr    %rdx,%rcx
  401c5a:	48 83 f1 3f          	xor    $0x3f,%rcx
  401c5e:	29 c8                	sub    %ecx,%eax
  401c60:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401c64:	83 7c 24 1c 3e       	cmpl   $0x3e,0x1c(%rsp)
  401c69:	0f 97 c0             	seta   %al
  401c6c:	24 01                	and    $0x1,%al
  401c6e:	3c 00                	cmp    $0x0,%al
  401c70:	74 37                	je     401ca9 <runtime::udivmod128+0x359>
  401c72:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401c77:	48 83 f8 00          	cmp    $0x0,%rax
  401c7b:	0f 95 c0             	setne  %al
  401c7e:	24 01                	and    $0x1,%al
  401c80:	3c 00                	cmp    $0x0,%al
  401c82:	74 16                	je     401c9a <runtime::udivmod128+0x34a>
  401c84:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401c89:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  401c8e:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401c93:	48 89 10             	mov    %rdx,(%rax)
  401c96:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401c9a:	31 c0                	xor    %eax,%eax
  401c9c:	89 c2                	mov    %eax,%edx
  401c9e:	48 89 d0             	mov    %rdx,%rax
  401ca1:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401ca8:	c3                   	ret
  401ca9:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401cad:	83 c0 01             	add    $0x1,%eax
  401cb0:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401cb4:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401cbb:	00 00 
  401cbd:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401cc2:	b9 40 00 00 00       	mov    $0x40,%ecx
  401cc7:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401ccb:	89 c9                	mov    %ecx,%ecx
  401ccd:	89 ca                	mov    %ecx,%edx
  401ccf:	48 89 d1             	mov    %rdx,%rcx
  401cd2:	48 d3 e0             	shl    %cl,%rax
  401cd5:	48 89 c1             	mov    %rax,%rcx
  401cd8:	31 c0                	xor    %eax,%eax
  401cda:	48 83 fa 40          	cmp    $0x40,%rdx
  401cde:	48 0f 42 c1          	cmovb  %rcx,%rax
  401ce2:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401ce7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401cec:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401cf0:	89 ca                	mov    %ecx,%edx
  401cf2:	48 89 d1             	mov    %rdx,%rcx
  401cf5:	48 d3 e8             	shr    %cl,%rax
  401cf8:	48 89 c1             	mov    %rax,%rcx
  401cfb:	31 c0                	xor    %eax,%eax
  401cfd:	48 83 fa 40          	cmp    $0x40,%rdx
  401d01:	48 0f 42 c1          	cmovb  %rcx,%rax
  401d05:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401d0a:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401d0f:	b9 40 00 00 00       	mov    $0x40,%ecx
  401d14:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401d18:	89 c9                	mov    %ecx,%ecx
  401d1a:	89 ca                	mov    %ecx,%edx
  401d1c:	48 89 d1             	mov    %rdx,%rcx
  401d1f:	48 d3 e0             	shl    %cl,%rax
  401d22:	48 89 c1             	mov    %rax,%rcx
  401d25:	31 c0                	xor    %eax,%eax
  401d27:	48 83 fa 40          	cmp    $0x40,%rdx
  401d2b:	48 0f 42 c1          	cmovb  %rcx,%rax
  401d2f:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401d34:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401d38:	89 ce                	mov    %ecx,%esi
  401d3a:	48 89 f1             	mov    %rsi,%rcx
  401d3d:	48 d3 ea             	shr    %cl,%rdx
  401d40:	31 c9                	xor    %ecx,%ecx
  401d42:	48 83 fe 40          	cmp    $0x40,%rsi
  401d46:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401d4a:	48 09 c8             	or     %rcx,%rax
  401d4d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401d52:	e9 30 04 00 00       	jmp    402187 <runtime::udivmod128+0x837>
  401d57:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401d5d:	0f 94 c0             	sete   %al
  401d60:	24 01                	and    $0x1,%al
  401d62:	3c 00                	cmp    $0x0,%al
  401d64:	0f 84 d1 02 00 00    	je     40203b <runtime::udivmod128+0x6eb>
  401d6a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401d6f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401d74:	48 83 e9 01          	sub    $0x1,%rcx
  401d78:	48 21 c8             	and    %rcx,%rax
  401d7b:	48 83 f8 00          	cmp    $0x0,%rax
  401d7f:	0f 94 c0             	sete   %al
  401d82:	24 01                	and    $0x1,%al
  401d84:	3c 00                	cmp    $0x0,%al
  401d86:	0f 84 de 00 00 00    	je     401e6a <runtime::udivmod128+0x51a>
  401d8c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401d91:	48 83 f8 00          	cmp    $0x0,%rax
  401d95:	0f 95 c0             	setne  %al
  401d98:	24 01                	and    $0x1,%al
  401d9a:	3c 00                	cmp    $0x0,%al
  401d9c:	74 20                	je     401dbe <runtime::udivmod128+0x46e>
  401d9e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401da3:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401da8:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401dad:	48 ff ca             	dec    %rdx
  401db0:	48 21 d1             	and    %rdx,%rcx
  401db3:	48 89 08             	mov    %rcx,(%rax)
  401db6:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401dbd:	00 
  401dbe:	48 83 7c 24 40 01    	cmpq   $0x1,0x40(%rsp)
  401dc4:	0f 94 c0             	sete   %al
  401dc7:	24 01                	and    $0x1,%al
  401dc9:	3c 00                	cmp    $0x0,%al
  401dcb:	74 12                	je     401ddf <runtime::udivmod128+0x48f>
  401dcd:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401dd2:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401dd7:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401dde:	c3                   	ret
  401ddf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401de4:	b8 40 00 00 00       	mov    $0x40,%eax
  401de9:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  401dee:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401df2:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401df7:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401dfb:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  401dff:	88 c1                	mov    %al,%cl
  401e01:	48 d3 ea             	shr    %cl,%rdx
  401e04:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  401e08:	31 c0                	xor    %eax,%eax
  401e0a:	83 e9 40             	sub    $0x40,%ecx
  401e0d:	48 89 c1             	mov    %rax,%rcx
  401e10:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401e14:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  401e19:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  401e1e:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401e23:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  401e27:	40 88 f1             	mov    %sil,%cl
  401e2a:	48 d3 ef             	shr    %cl,%rdi
  401e2d:	83 ee 40             	sub    $0x40,%esi
  401e30:	48 89 c1             	mov    %rax,%rcx
  401e33:	48 0f 42 cf          	cmovb  %rdi,%rcx
  401e37:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  401e3c:	f7 de                	neg    %esi
  401e3e:	40 88 f1             	mov    %sil,%cl
  401e41:	48 d3 e2             	shl    %cl,%rdx
  401e44:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  401e49:	83 ee 40             	sub    $0x40,%esi
  401e4c:	48 0f 42 c2          	cmovb  %rdx,%rax
  401e50:	48 09 c8             	or     %rcx,%rax
  401e53:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401e58:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401e5d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401e62:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401e69:	c3                   	ret
  401e6a:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401e6f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401e74:	48 0f bd c1          	bsr    %rcx,%rax
  401e78:	48 83 f0 3f          	xor    $0x3f,%rax
  401e7c:	83 c0 41             	add    $0x41,%eax
  401e7f:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401e84:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401e89:	48 0f bd ca          	bsr    %rdx,%rcx
  401e8d:	48 83 f1 3f          	xor    $0x3f,%rcx
  401e91:	29 c8                	sub    %ecx,%eax
  401e93:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401e97:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401e9c:	0f 94 c1             	sete   %cl
  401e9f:	80 e1 01             	and    $0x1,%cl
  401ea2:	b0 01                	mov    $0x1,%al
  401ea4:	38 c8                	cmp    %cl,%al
  401ea6:	74 13                	je     401ebb <runtime::udivmod128+0x56b>
  401ea8:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401ead:	0f 92 c1             	setb   %cl
  401eb0:	80 e1 01             	and    $0x1,%cl
  401eb3:	b0 01                	mov    $0x1,%al
  401eb5:	38 c8                	cmp    %cl,%al
  401eb7:	74 32                	je     401eeb <runtime::udivmod128+0x59b>
  401eb9:	eb 2b                	jmp    401ee6 <runtime::udivmod128+0x596>
  401ebb:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401ec2:	00 00 
  401ec4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401ec9:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401ece:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401ed5:	00 00 
  401ed7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401edc:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401ee1:	e9 50 01 00 00       	jmp    402036 <runtime::udivmod128+0x6e6>
  401ee6:	e9 a3 00 00 00       	jmp    401f8e <runtime::udivmod128+0x63e>
  401eeb:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401ef2:	00 00 
  401ef4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401ef9:	b9 40 00 00 00       	mov    $0x40,%ecx
  401efe:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401f02:	89 c9                	mov    %ecx,%ecx
  401f04:	89 ca                	mov    %ecx,%edx
  401f06:	48 89 d1             	mov    %rdx,%rcx
  401f09:	48 d3 e0             	shl    %cl,%rax
  401f0c:	48 89 c1             	mov    %rax,%rcx
  401f0f:	31 c0                	xor    %eax,%eax
  401f11:	48 83 fa 40          	cmp    $0x40,%rdx
  401f15:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f19:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401f1e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401f23:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401f27:	89 ca                	mov    %ecx,%edx
  401f29:	48 89 d1             	mov    %rdx,%rcx
  401f2c:	48 d3 e8             	shr    %cl,%rax
  401f2f:	48 89 c1             	mov    %rax,%rcx
  401f32:	31 c0                	xor    %eax,%eax
  401f34:	48 83 fa 40          	cmp    $0x40,%rdx
  401f38:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f3c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401f41:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401f46:	b9 40 00 00 00       	mov    $0x40,%ecx
  401f4b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401f4f:	89 c9                	mov    %ecx,%ecx
  401f51:	89 ca                	mov    %ecx,%edx
  401f53:	48 89 d1             	mov    %rdx,%rcx
  401f56:	48 d3 e0             	shl    %cl,%rax
  401f59:	48 89 c1             	mov    %rax,%rcx
  401f5c:	31 c0                	xor    %eax,%eax
  401f5e:	48 83 fa 40          	cmp    $0x40,%rdx
  401f62:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f66:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401f6b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401f6f:	89 ce                	mov    %ecx,%esi
  401f71:	48 89 f1             	mov    %rsi,%rcx
  401f74:	48 d3 ea             	shr    %cl,%rdx
  401f77:	31 c9                	xor    %ecx,%ecx
  401f79:	48 83 fe 40          	cmp    $0x40,%rsi
  401f7d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401f81:	48 09 c8             	or     %rcx,%rax
  401f84:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401f89:	e9 a8 00 00 00       	jmp    402036 <runtime::udivmod128+0x6e6>
  401f8e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401f93:	b9 80 00 00 00       	mov    $0x80,%ecx
  401f98:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401f9c:	89 c9                	mov    %ecx,%ecx
  401f9e:	89 ca                	mov    %ecx,%edx
  401fa0:	48 89 d1             	mov    %rdx,%rcx
  401fa3:	48 d3 e0             	shl    %cl,%rax
  401fa6:	48 89 c1             	mov    %rax,%rcx
  401fa9:	31 c0                	xor    %eax,%eax
  401fab:	48 83 fa 40          	cmp    $0x40,%rdx
  401faf:	48 0f 42 c1          	cmovb  %rcx,%rax
  401fb3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401fb8:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401fbd:	b9 80 00 00 00       	mov    $0x80,%ecx
  401fc2:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401fc6:	89 c9                	mov    %ecx,%ecx
  401fc8:	89 ca                	mov    %ecx,%edx
  401fca:	48 89 d1             	mov    %rdx,%rcx
  401fcd:	48 d3 e0             	shl    %cl,%rax
  401fd0:	48 89 c1             	mov    %rax,%rcx
  401fd3:	31 c0                	xor    %eax,%eax
  401fd5:	48 83 fa 40          	cmp    $0x40,%rdx
  401fd9:	48 0f 42 c1          	cmovb  %rcx,%rax
  401fdd:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401fe2:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401fe6:	83 e9 40             	sub    $0x40,%ecx
  401fe9:	89 c9                	mov    %ecx,%ecx
  401feb:	89 ce                	mov    %ecx,%esi
  401fed:	48 89 f1             	mov    %rsi,%rcx
  401ff0:	48 d3 ea             	shr    %cl,%rdx
  401ff3:	31 c9                	xor    %ecx,%ecx
  401ff5:	48 83 fe 40          	cmp    $0x40,%rsi
  401ff9:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401ffd:	48 09 c8             	or     %rcx,%rax
  402000:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402005:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  40200c:	00 00 
  40200e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402013:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402017:	83 e9 40             	sub    $0x40,%ecx
  40201a:	89 c9                	mov    %ecx,%ecx
  40201c:	89 ca                	mov    %ecx,%edx
  40201e:	48 89 d1             	mov    %rdx,%rcx
  402021:	48 d3 e8             	shr    %cl,%rax
  402024:	48 89 c1             	mov    %rax,%rcx
  402027:	31 c0                	xor    %eax,%eax
  402029:	48 83 fa 40          	cmp    $0x40,%rdx
  40202d:	48 0f 42 c1          	cmovb  %rcx,%rax
  402031:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402036:	e9 4a 01 00 00       	jmp    402185 <runtime::udivmod128+0x835>
  40203b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402040:	b8 7f 00 00 00       	mov    $0x7f,%eax
  402045:	48 0f bd c1          	bsr    %rcx,%rax
  402049:	48 83 f0 3f          	xor    $0x3f,%rax
  40204d:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  402052:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  402057:	48 0f bd ca          	bsr    %rdx,%rcx
  40205b:	48 83 f1 3f          	xor    $0x3f,%rcx
  40205f:	29 c8                	sub    %ecx,%eax
  402061:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  402065:	83 7c 24 1c 3f       	cmpl   $0x3f,0x1c(%rsp)
  40206a:	0f 97 c0             	seta   %al
  40206d:	24 01                	and    $0x1,%al
  40206f:	3c 00                	cmp    $0x0,%al
  402071:	74 37                	je     4020aa <runtime::udivmod128+0x75a>
  402073:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  402078:	48 83 f8 00          	cmp    $0x0,%rax
  40207c:	0f 95 c0             	setne  %al
  40207f:	24 01                	and    $0x1,%al
  402081:	3c 00                	cmp    $0x0,%al
  402083:	74 16                	je     40209b <runtime::udivmod128+0x74b>
  402085:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40208a:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  40208f:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  402094:	48 89 10             	mov    %rdx,(%rax)
  402097:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40209b:	31 c0                	xor    %eax,%eax
  40209d:	89 c2                	mov    %eax,%edx
  40209f:	48 89 d0             	mov    %rdx,%rax
  4020a2:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4020a9:	c3                   	ret
  4020aa:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  4020ae:	83 c0 01             	add    $0x1,%eax
  4020b1:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4020b5:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4020bc:	00 00 
  4020be:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  4020c3:	0f 94 c0             	sete   %al
  4020c6:	24 01                	and    $0x1,%al
  4020c8:	3c 00                	cmp    $0x0,%al
  4020ca:	74 22                	je     4020ee <runtime::udivmod128+0x79e>
  4020cc:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4020d1:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4020d6:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4020dd:	00 00 
  4020df:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4020e4:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4020e9:	e9 95 00 00 00       	jmp    402183 <runtime::udivmod128+0x833>
  4020ee:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4020f3:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4020f7:	89 ca                	mov    %ecx,%edx
  4020f9:	48 89 d1             	mov    %rdx,%rcx
  4020fc:	48 d3 e8             	shr    %cl,%rax
  4020ff:	48 89 c1             	mov    %rax,%rcx
  402102:	31 c0                	xor    %eax,%eax
  402104:	48 83 fa 40          	cmp    $0x40,%rdx
  402108:	48 0f 42 c1          	cmovb  %rcx,%rax
  40210c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402111:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402116:	b9 40 00 00 00       	mov    $0x40,%ecx
  40211b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40211f:	89 c9                	mov    %ecx,%ecx
  402121:	89 ca                	mov    %ecx,%edx
  402123:	48 89 d1             	mov    %rdx,%rcx
  402126:	48 d3 e0             	shl    %cl,%rax
  402129:	48 89 c1             	mov    %rax,%rcx
  40212c:	31 c0                	xor    %eax,%eax
  40212e:	48 83 fa 40          	cmp    $0x40,%rdx
  402132:	48 0f 42 c1          	cmovb  %rcx,%rax
  402136:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40213b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40213f:	89 ce                	mov    %ecx,%esi
  402141:	48 89 f1             	mov    %rsi,%rcx
  402144:	48 d3 ea             	shr    %cl,%rdx
  402147:	31 c9                	xor    %ecx,%ecx
  402149:	48 83 fe 40          	cmp    $0x40,%rsi
  40214d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  402151:	48 09 c8             	or     %rcx,%rax
  402154:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402159:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40215e:	b9 40 00 00 00       	mov    $0x40,%ecx
  402163:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  402167:	89 c9                	mov    %ecx,%ecx
  402169:	89 ca                	mov    %ecx,%edx
  40216b:	48 89 d1             	mov    %rdx,%rcx
  40216e:	48 d3 e0             	shl    %cl,%rax
  402171:	48 89 c1             	mov    %rax,%rcx
  402174:	31 c0                	xor    %eax,%eax
  402176:	48 83 fa 40          	cmp    $0x40,%rdx
  40217a:	48 0f 42 c1          	cmovb  %rcx,%rax
  40217e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402183:	eb 00                	jmp    402185 <runtime::udivmod128+0x835>
  402185:	eb 00                	jmp    402187 <runtime::udivmod128+0x837>
  402187:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  40218e:	00 
  40218f:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  402196:	00 00 
  402198:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  40219f:	00 00 
  4021a1:	83 7c 24 1c 00       	cmpl   $0x0,0x1c(%rsp)
  4021a6:	0f 97 c0             	seta   %al
  4021a9:	24 01                	and    $0x1,%al
  4021ab:	3c 00                	cmp    $0x0,%al
  4021ad:	0f 84 eb 00 00 00    	je     40229e <runtime::udivmod128+0x94e>
  4021b3:	48 8b 74 24 b8       	mov    -0x48(%rsp),%rsi
  4021b8:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  4021bd:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4021c2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4021c7:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  4021cc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4021d1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4021d6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4021db:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  4021e0:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4021e5:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4021ea:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4021ef:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  4021f4:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4021f9:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4021fe:	48 01 c0             	add    %rax,%rax
  402201:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  402205:	48 09 c8             	or     %rcx,%rax
  402208:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40220d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402212:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  402217:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40221c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  402221:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  402226:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40222b:	48 f7 d0             	not    %rax
  40222e:	48 f7 d1             	not    %rcx
  402231:	48 01 f1             	add    %rsi,%rcx
  402234:	48 11 d0             	adc    %rdx,%rax
  402237:	48 c1 f8 3f          	sar    $0x3f,%rax
  40223b:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402240:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  402245:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  402249:	83 e0 01             	and    $0x1,%eax
  40224c:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  402250:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  402255:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40225a:	48 21 ca             	and    %rcx,%rdx
  40225d:	48 21 c6             	and    %rax,%rsi
  402260:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  402265:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40226a:	48 29 f1             	sub    %rsi,%rcx
  40226d:	48 19 d0             	sbb    %rdx,%rax
  402270:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  402275:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40227a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40227f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402284:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  402289:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40228e:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  402292:	83 e8 01             	sub    $0x1,%eax
  402295:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  402299:	e9 03 ff ff ff       	jmp    4021a1 <runtime::udivmod128+0x851>
  40229e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4022a3:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4022a8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4022ad:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  4022b2:	48 01 c9             	add    %rcx,%rcx
  4022b5:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  4022b9:	48 09 f1             	or     %rsi,%rcx
  4022bc:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  4022c1:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  4022c6:	48 83 f8 00          	cmp    $0x0,%rax
  4022ca:	0f 95 c0             	setne  %al
  4022cd:	24 01                	and    $0x1,%al
  4022cf:	3c 00                	cmp    $0x0,%al
  4022d1:	74 16                	je     4022e9 <runtime::udivmod128+0x999>
  4022d3:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4022d8:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4022dd:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4022e2:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4022e6:	48 89 08             	mov    %rcx,(%rax)
  4022e9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4022ee:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  4022f3:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4022fa:	c3                   	ret
  4022fb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402300 <runtime::stderr_write>:
  402300:	48 83 ec 48          	sub    $0x48,%rsp
  402304:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  402309:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40230e:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  402313:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402318:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40231d:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  402322:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  402327:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40232e:	00 00 
  402330:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  402335:	e8 16 00 00 00       	call   402350 <runtime::[os_specific_linux.odin]::_stderr_write>
  40233a:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40233f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402344:	48 89 11             	mov    %rdx,(%rcx)
  402347:	48 83 c4 48          	add    $0x48,%rsp
  40234b:	c3                   	ret
  40234c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402350 <runtime::[os_specific_linux.odin]::_stderr_write>:
  402350:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  402355:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40235a:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  40235f:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  402364:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  402369:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  40236e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  402373:	b8 01 00 00 00       	mov    $0x1,%eax
  402378:	bf 02 00 00 00       	mov    $0x2,%edi
  40237d:	0f 05                	syscall
  40237f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402384:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  40238a:	0f 9c c0             	setl   %al
  40238d:	24 01                	and    $0x1,%al
  40238f:	3c 00                	cmp    $0x0,%al
  402391:	74 26                	je     4023b9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  402393:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  40239a:	ff ff 
  40239c:	0f 9f c0             	setg   %al
  40239f:	24 01                	and    $0x1,%al
  4023a1:	3c 00                	cmp    $0x0,%al
  4023a3:	74 14                	je     4023b9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4023a5:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  4023aa:	31 c0                	xor    %eax,%eax
  4023ac:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  4023b1:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  4023b8:	c3                   	ret
  4023b9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4023be:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4023c3:	48 89 08             	mov    %rcx,(%rax)
  4023c6:	31 c0                	xor    %eax,%eax
  4023c8:	c3                   	ret
  4023c9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004023d0 <runtime::heap_allocator_proc>:
  4023d0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4023d7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  4023dc:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  4023e1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4023e6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4023eb:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  4023f0:	40 88 f0             	mov    %sil,%al
  4023f3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  4023f7:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  4023fe:	00 
  4023ff:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402404:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40240b:	00 
  40240c:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402411:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  402415:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40241a:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40241f:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402424:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402429:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  40242e:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  402435:	00 
  402436:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  40243d:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  402444:	00 
  402445:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  40244c:	00 
  40244d:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  402454:	00 
  402455:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40245c:	00 
  40245d:	0f b6 c8             	movzbl %al,%ecx
  402460:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  402465:	2c 07                	sub    $0x7,%al
  402467:	0f 87 5f 01 00 00    	ja     4025cc <runtime::heap_allocator_proc+0x1fc>
  40246d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402472:	48 8b 04 c5 10 80 40 	mov    0x408010(,%rax,8),%rax
  402479:	00 
  40247a:	ff e0                	jmp    *%rax
  40247c:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402481:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402486:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40248b:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  40248f:	84 c0                	test   %al,%al
  402491:	0f 94 c0             	sete   %al
  402494:	0f 57 c0             	xorps  %xmm0,%xmm0
  402497:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40249e:	00 
  40249f:	48 89 e1             	mov    %rsp,%rcx
  4024a2:	48 89 11             	mov    %rdx,(%rcx)
  4024a5:	44 0f b6 c0          	movzbl %al,%r8d
  4024a9:	31 c0                	xor    %eax,%eax
  4024ab:	89 c1                	mov    %eax,%ecx
  4024ad:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  4024b4:	00 
  4024b5:	48 89 ca             	mov    %rcx,%rdx
  4024b8:	e8 b3 41 00 00       	call   406670 <runtime::heap_allocator_proc.aligned_alloc-0>
  4024bd:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4024c2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4024c9:	00 
  4024ca:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4024d1:	00 
  4024d2:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4024d6:	48 89 11             	mov    %rdx,(%rcx)
  4024d9:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4024e0:	c3                   	ret
  4024e1:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4024e6:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4024eb:	e8 20 44 00 00       	call   406910 <runtime::heap_allocator_proc.aligned_free-1>
  4024f0:	e9 d7 00 00 00       	jmp    4025cc <runtime::heap_allocator_proc+0x1fc>
  4024f5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4024fa:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402501:	00 
  402502:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402509:	b0 04                	mov    $0x4,%al
  40250b:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402512:	c3                   	ret
  402513:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402518:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40251d:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402522:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  402527:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  40252c:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  402530:	2c 03                	sub    $0x3,%al
  402532:	0f 94 c0             	sete   %al
  402535:	0f 57 c0             	xorps  %xmm0,%xmm0
  402538:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40253d:	49 89 e0             	mov    %rsp,%r8
  402540:	4d 89 08             	mov    %r9,(%r8)
  402543:	44 0f b6 c0          	movzbl %al,%r8d
  402547:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  40254c:	e8 ff 43 00 00       	call   406950 <runtime::heap_allocator_proc.aligned_resize-2>
  402551:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402556:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40255b:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  402560:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402564:	48 89 11             	mov    %rdx,(%rcx)
  402567:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40256e:	c3                   	ret
  40256f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402574:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402579:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  40257f:	0f 95 c0             	setne  %al
  402582:	24 01                	and    $0x1,%al
  402584:	3c 00                	cmp    $0x0,%al
  402586:	74 08                	je     402590 <runtime::heap_allocator_proc+0x1c0>
  402588:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40258d:	c6 00 db             	movb   $0xdb,(%rax)
  402590:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402595:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40259c:	00 
  40259d:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4025a4:	31 c0                	xor    %eax,%eax
  4025a6:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4025ad:	c3                   	ret
  4025ae:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4025b3:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4025ba:	00 
  4025bb:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4025c2:	b0 04                	mov    $0x4,%al
  4025c4:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4025cb:	c3                   	ret
  4025cc:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4025d1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4025d8:	00 
  4025d9:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4025e0:	31 c0                	xor    %eax,%eax
  4025e2:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4025e9:	c3                   	ret
  4025ea:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004025f0 <runtime::[internal.odin]::byte_slice>:
  4025f0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4025f5:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  4025fa:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  4025ff:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  402604:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402609:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  40260e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402613:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  402618:	31 c0                	xor    %eax,%eax
  40261a:	48 85 d2             	test   %rdx,%rdx
  40261d:	48 0f 49 c2          	cmovns %rdx,%rax
  402621:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  402626:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40262b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  402630:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  402635:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40263a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40263f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  402644:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  402649:	c3                   	ret
  40264a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402650 <runtime::bounds_check_error>:
  402650:	48 83 ec 58          	sub    $0x58,%rsp
  402654:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402659:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40265e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402662:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402666:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40266b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402670:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402675:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40267a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40267e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402682:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402687:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40268c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402691:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402696:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40269a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40269e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4026a3:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4026a8:	48 39 c8             	cmp    %rcx,%rax
  4026ab:	0f 92 c0             	setb   %al
  4026ae:	24 01                	and    $0x1,%al
  4026b0:	3c 00                	cmp    $0x0,%al
  4026b2:	74 05                	je     4026b9 <runtime::bounds_check_error+0x69>
  4026b4:	48 83 c4 58          	add    $0x58,%rsp
  4026b8:	c3                   	ret
  4026b9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4026be:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4026c3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4026c7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4026cb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4026d0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4026d5:	e8 36 45 00 00       	call   406c10 <runtime::bounds_check_error.handle_error-0>
  4026da:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004026e0 <runtime::is_power_of_two_int>:
  4026e0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  4026e5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4026ea:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4026ef:	48 83 f8 00          	cmp    $0x0,%rax
  4026f3:	0f 9e c0             	setle  %al
  4026f6:	24 01                	and    $0x1,%al
  4026f8:	3c 00                	cmp    $0x0,%al
  4026fa:	74 03                	je     4026ff <runtime::is_power_of_two_int+0x1f>
  4026fc:	31 c0                	xor    %eax,%eax
  4026fe:	c3                   	ret
  4026ff:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  402704:	48 89 c1             	mov    %rax,%rcx
  402707:	48 83 e9 01          	sub    $0x1,%rcx
  40270b:	48 21 c8             	and    %rcx,%rax
  40270e:	48 83 f8 00          	cmp    $0x0,%rax
  402712:	0f 94 c0             	sete   %al
  402715:	24 01                	and    $0x1,%al
  402717:	c3                   	ret
  402718:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40271f:	00 

0000000000402720 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  402720:	48 83 ec 18          	sub    $0x18,%rsp
  402724:	48 89 3c 24          	mov    %rdi,(%rsp)
  402728:	40 88 f0             	mov    %sil,%al
  40272b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40272f:	48 8b 04 24          	mov    (%rsp),%rax
  402733:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  402737:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40273c:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  402740:	48 83 f8 00          	cmp    $0x0,%rax
  402744:	0f 9e c0             	setle  %al
  402747:	24 01                	and    $0x1,%al
  402749:	3c 00                	cmp    $0x0,%al
  40274b:	74 07                	je     402754 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  40274d:	31 c0                	xor    %eax,%eax
  40274f:	48 83 c4 18          	add    $0x18,%rsp
  402753:	c3                   	ret
  402754:	8a 44 24 0e          	mov    0xe(%rsp),%al
  402758:	3c 00                	cmp    $0x0,%al
  40275a:	74 13                	je     40276f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  40275c:	48 8b 34 24          	mov    (%rsp),%rsi
  402760:	bf 01 00 00 00       	mov    $0x1,%edi
  402765:	e8 e6 e8 ff ff       	call   401050 <calloc@plt>
  40276a:	48 83 c4 18          	add    $0x18,%rsp
  40276e:	c3                   	ret
  40276f:	48 8b 3c 24          	mov    (%rsp),%rdi
  402773:	e8 f8 e8 ff ff       	call   401070 <malloc@plt>
  402778:	48 83 c4 18          	add    $0x18,%rsp
  40277c:	c3                   	ret
  40277d:	0f 1f 00             	nopl   (%rax)

0000000000402780 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  402780:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  402785:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  40278a:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  40278f:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  402794:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  402799:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40279e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4027a3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4027a8:	48 01 c2             	add    %rax,%rdx
  4027ab:	0f 92 c0             	setb   %al
  4027ae:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  4027b3:	24 01                	and    $0x1,%al
  4027b5:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  4027b9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4027be:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  4027c3:	0f 94 c0             	sete   %al
  4027c6:	24 01                	and    $0x1,%al
  4027c8:	48 89 11             	mov    %rdx,(%rcx)
  4027cb:	c3                   	ret
  4027cc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004027d0 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  4027d0:	48 83 ec 28          	sub    $0x28,%rsp
  4027d4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4027d9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4027de:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4027e3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4027e8:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4027ed:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4027f2:	e8 89 e8 ff ff       	call   401080 <realloc@plt>
  4027f7:	48 83 c4 28          	add    $0x28,%rsp
  4027fb:	c3                   	ret
  4027fc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402800 <runtime::memory_block_alloc>:
  402800:	48 81 ec 68 01 00 00 	sub    $0x168,%rsp
  402807:	4c 89 4c 24 60       	mov    %r9,0x60(%rsp)
  40280c:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  402811:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  402816:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40281b:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  402822:	00 
  402823:	48 89 bc 24 88 00 00 	mov    %rdi,0x88(%rsp)
  40282a:	00 
  40282b:	48 8b 84 24 70 01 00 	mov    0x170(%rsp),%rax
  402832:	00 
  402833:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40283a:	00 
  40283b:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  402840:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  402845:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  40284c:	00 
  40284d:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  402854:	00 
  402855:	48 89 b4 24 20 01 00 	mov    %rsi,0x120(%rsp)
  40285c:	00 
  40285d:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  402864:	00 
  402865:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  40286c:	00 
  40286d:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  402872:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  402879:	00 
  40287a:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40287f:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  402886:	00 
  402887:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40288e:	00 
  40288f:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  402896:	00 00 00 00 00 
  40289b:	c6 84 24 07 01 00 00 	movb   $0x0,0x107(%rsp)
  4028a2:	00 
  4028a3:	48 89 c1             	mov    %rax,%rcx
  4028a6:	48 83 e9 31          	sub    $0x31,%rcx
  4028aa:	b9 30 00 00 00       	mov    $0x30,%ecx
  4028af:	48 0f 43 c8          	cmovae %rax,%rcx
  4028b3:	48 01 ca             	add    %rcx,%rdx
  4028b6:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  4028bd:	00 
  4028be:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  4028c5:	00 
  4028c6:	48 89 c1             	mov    %rax,%rcx
  4028c9:	48 83 e9 10          	sub    $0x10,%rcx
  4028cd:	b9 10 00 00 00       	mov    $0x10,%ecx
  4028d2:	48 0f 4c c1          	cmovl  %rcx,%rax
  4028d6:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  4028dd:	00 
  4028de:	48 8b b4 24 f8 00 00 	mov    0xf8(%rsp),%rsi
  4028e5:	00 
  4028e6:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  4028eb:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4028f2:	00 
  4028f3:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4028f8:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  4028ff:	00 
  402900:	48 8b 94 24 28 01 00 	mov    0x128(%rsp),%rdx
  402907:	00 
  402908:	0f 57 c0             	xorps  %xmm0,%xmm0
  40290b:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  402912:	00 
  402913:	48 89 b4 24 58 01 00 	mov    %rsi,0x158(%rsp)
  40291a:	00 
  40291b:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  402922:	00 
  402923:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  40292a:	00 
  40292b:	48 89 8c 24 40 01 00 	mov    %rcx,0x140(%rsp)
  402932:	00 
  402933:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  40293a:	00 
  40293b:	48 83 f8 00          	cmp    $0x0,%rax
  40293f:	0f 9e c0             	setle  %al
  402942:	a8 01                	test   $0x1,%al
  402944:	75 02                	jne    402948 <runtime::memory_block_alloc+0x148>
  402946:	eb 08                	jmp    402950 <runtime::memory_block_alloc+0x150>
  402948:	31 c0                	xor    %eax,%eax
  40294a:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  40294e:	eb 1c                	jmp    40296c <runtime::memory_block_alloc+0x16c>
  402950:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  402955:	48 89 c1             	mov    %rax,%rcx
  402958:	48 83 e9 01          	sub    $0x1,%rcx
  40295c:	48 21 c8             	and    %rcx,%rax
  40295f:	48 83 f8 00          	cmp    $0x0,%rax
  402963:	0f 94 c0             	sete   %al
  402966:	24 01                	and    $0x1,%al
  402968:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  40296c:	4c 8b 84 24 90 00 00 	mov    0x90(%rsp),%r8
  402973:	00 
  402974:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  402979:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  40297d:	0f b6 f8             	movzbl %al,%edi
  402980:	be 71 82 40 00       	mov    $0x408271,%esi
  402985:	ba 20 00 00 00       	mov    $0x20,%edx
  40298a:	e8 81 3c 00 00       	call   406610 <runtime::assert>
  40298f:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  402994:	48 83 f8 00          	cmp    $0x0,%rax
  402998:	0f 94 c0             	sete   %al
  40299b:	a8 01                	test   $0x1,%al
  40299d:	75 12                	jne    4029b1 <runtime::memory_block_alloc+0x1b1>
  40299f:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  4029a6:	00 00 
  4029a8:	0f 94 c0             	sete   %al
  4029ab:	a8 01                	test   $0x1,%al
  4029ad:	75 02                	jne    4029b1 <runtime::memory_block_alloc+0x1b1>
  4029af:	eb 20                	jmp    4029d1 <runtime::memory_block_alloc+0x1d1>
  4029b1:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  4029b8:	00 00 00 00 00 
  4029bd:	48 c7 84 24 d0 00 00 	movq   $0x0,0xd0(%rsp)
  4029c4:	00 00 00 00 00 
  4029c9:	31 c0                	xor    %eax,%eax
  4029cb:	88 44 24 3e          	mov    %al,0x3e(%rsp)
  4029cf:	eb 76                	jmp    402a47 <runtime::memory_block_alloc+0x247>
  4029d1:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4029d6:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4029db:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  4029e0:	4c 8b 8c 24 90 00 00 	mov    0x90(%rsp),%r9
  4029e7:	00 
  4029e8:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4029ef:	00 
  4029f0:	48 8b bc 24 48 01 00 	mov    0x148(%rsp),%rdi
  4029f7:	00 
  4029f8:	0f 57 c0             	xorps  %xmm0,%xmm0
  4029fb:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  402a02:	00 
  402a03:	48 89 e6             	mov    %rsp,%rsi
  402a06:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  402a0a:	4c 8d 8c 24 30 01 00 	lea    0x130(%rsp),%r9
  402a11:	00 
  402a12:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  402a16:	4c 89 06             	mov    %r8,(%rsi)
  402a19:	31 f6                	xor    %esi,%esi
  402a1b:	41 89 f1             	mov    %esi,%r9d
  402a1e:	4d 89 c8             	mov    %r9,%r8
  402a21:	ff d0                	call   *%rax
  402a23:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  402a2a:	00 
  402a2b:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  402a32:	00 
  402a33:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  402a3a:	00 
  402a3b:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  402a42:	00 
  402a43:	88 44 24 3e          	mov    %al,0x3e(%rsp)
  402a47:	8a 44 24 3e          	mov    0x3e(%rsp),%al
  402a4b:	88 44 24 27          	mov    %al,0x27(%rsp)
  402a4f:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  402a56:	00 
  402a57:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  402a5c:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  402a63:	00 
  402a64:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402a69:	3c 00                	cmp    $0x0,%al
  402a6b:	74 39                	je     402aa6 <runtime::memory_block_alloc+0x2a6>
  402a6d:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  402a72:	8a 44 24 27          	mov    0x27(%rsp),%al
  402a76:	88 84 24 07 01 00 00 	mov    %al,0x107(%rsp)
  402a7d:	48 8b 94 24 08 01 00 	mov    0x108(%rsp),%rdx
  402a84:	00 
  402a85:	8a 84 24 07 01 00 00 	mov    0x107(%rsp),%al
  402a8c:	48 89 94 24 08 01 00 	mov    %rdx,0x108(%rsp)
  402a93:	00 
  402a94:	88 84 24 07 01 00 00 	mov    %al,0x107(%rsp)
  402a9b:	48 89 11             	mov    %rdx,(%rcx)
  402a9e:	48 81 c4 68 01 00 00 	add    $0x168,%rsp
  402aa5:	c3                   	ret
  402aa6:	4c 8b 84 24 90 00 00 	mov    0x90(%rsp),%r8
  402aad:	00 
  402aae:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  402ab3:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  402ab8:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  402abd:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  402ac2:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402ac9:	00 
  402aca:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  402ad1:	00 
  402ad2:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  402ad9:	00 
  402ada:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  402ae1:	00 
  402ae2:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  402ae9:	00 
  402aea:	48 8b b4 24 b0 00 00 	mov    0xb0(%rsp),%rsi
  402af1:	00 
  402af2:	48 01 f0             	add    %rsi,%rax
  402af5:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402afc:	00 
  402afd:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  402b04:	00 
  402b05:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  402b0c:	00 
  402b0d:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  402b14:	00 
  402b15:	48 89 50 10          	mov    %rdx,0x10(%rax)
  402b19:	48 89 48 08          	mov    %rcx,0x8(%rax)
  402b1d:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  402b24:	00 
  402b25:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  402b2c:	00 
  402b2d:	48 03 8c 24 f0 00 00 	add    0xf0(%rsp),%rcx
  402b34:	00 
  402b35:	48 89 48 18          	mov    %rcx,0x18(%rax)
  402b39:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  402b40:	00 
  402b41:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  402b48:	00 
  402b49:	48 8b 94 24 08 01 00 	mov    0x108(%rsp),%rdx
  402b50:	00 
  402b51:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  402b55:	48 29 d1             	sub    %rdx,%rcx
  402b58:	48 89 48 28          	mov    %rcx,0x28(%rax)
  402b5c:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  402b63:	00 
  402b64:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  402b69:	0f 94 c0             	sete   %al
  402b6c:	24 01                	and    $0x1,%al
  402b6e:	0f b6 f8             	movzbl %al,%edi
  402b71:	be 90 80 40 00       	mov    $0x408090,%esi
  402b76:	b9 00 81 40 00       	mov    $0x408100,%ecx
  402b7b:	ba 0f 00 00 00       	mov    $0xf,%edx
  402b80:	e8 8b 3a 00 00       	call   406610 <runtime::assert>
  402b85:	4c 8b 84 24 90 00 00 	mov    0x90(%rsp),%r8
  402b8c:	00 
  402b8d:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  402b94:	00 
  402b95:	48 83 38 00          	cmpq   $0x0,(%rax)
  402b99:	0f 94 c0             	sete   %al
  402b9c:	24 01                	and    $0x1,%al
  402b9e:	0f b6 f8             	movzbl %al,%edi
  402ba1:	be 28 81 40 00       	mov    $0x408128,%esi
  402ba6:	b9 40 81 40 00       	mov    $0x408140,%ecx
  402bab:	ba 11 00 00 00       	mov    $0x11,%edx
  402bb0:	e8 5b 3a 00 00       	call   406610 <runtime::assert>
  402bb5:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  402bba:	48 8b 94 24 08 01 00 	mov    0x108(%rsp),%rdx
  402bc1:	00 
  402bc2:	8a 84 24 07 01 00 00 	mov    0x107(%rsp),%al
  402bc9:	48 89 94 24 08 01 00 	mov    %rdx,0x108(%rsp)
  402bd0:	00 
  402bd1:	88 84 24 07 01 00 00 	mov    %al,0x107(%rsp)
  402bd8:	48 89 11             	mov    %rdx,(%rcx)
  402bdb:	48 81 c4 68 01 00 00 	add    $0x168,%rsp
  402be2:	c3                   	ret
  402be3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402bea:	84 00 00 00 00 00 

0000000000402bf0 <runtime::default_temp_allocator_destroy>:
  402bf0:	48 83 ec 18          	sub    $0x18,%rsp
  402bf4:	48 89 3c 24          	mov    %rdi,(%rsp)
  402bf8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402bfd:	48 8b 04 24          	mov    (%rsp),%rax
  402c01:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402c06:	48 83 f8 00          	cmp    $0x0,%rax
  402c0a:	0f 95 c0             	setne  %al
  402c0d:	24 01                	and    $0x1,%al
  402c0f:	3c 00                	cmp    $0x0,%al
  402c11:	74 29                	je     402c3c <runtime::default_temp_allocator_destroy+0x4c>
  402c13:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402c18:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402c1d:	48 be d0 81 40 00 00 	movabs $0x4081d0,%rsi
  402c24:	00 00 00 
  402c27:	e8 34 1c 00 00       	call   404860 <runtime::arena_destroy>
  402c2c:	48 8b 3c 24          	mov    (%rsp),%rdi
  402c30:	31 f6                	xor    %esi,%esi
  402c32:	ba 38 00 00 00       	mov    $0x38,%edx
  402c37:	e8 04 e4 ff ff       	call   401040 <memset@plt>
  402c3c:	48 83 c4 18          	add    $0x18,%rsp
  402c40:	c3                   	ret
  402c41:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402c48:	0f 1f 84 00 00 00 00 
  402c4f:	00 

0000000000402c50 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  402c50:	48 83 ec 18          	sub    $0x18,%rsp
  402c54:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402c59:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402c5e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402c63:	e8 c8 e3 ff ff       	call   401030 <free@plt>
  402c68:	48 83 c4 18          	add    $0x18,%rsp
  402c6c:	c3                   	ret
  402c6d:	0f 1f 00             	nopl   (%rax)

0000000000402c70 <runtime::default_random_generator_proc>:
  402c70:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402c77:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402c7c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402c81:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  402c86:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  402c8b:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402c90:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402c95:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402c9a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  402c9f:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402ca6:	00 
  402ca7:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  402cac:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  402cb1:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  402cb6:	48 83 f8 00          	cmp    $0x0,%rax
  402cba:	0f 94 c0             	sete   %al
  402cbd:	24 01                	and    $0x1,%al
  402cbf:	3c 00                	cmp    $0x0,%al
  402cc1:	74 1a                	je     402cdd <runtime::default_random_generator_proc+0x6d>
  402cc3:	48 c7 c1 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rcx
  402cca:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402cd1:	00 00 
  402cd3:	48 01 c8             	add    %rcx,%rax
  402cd6:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402cdb:	eb 0a                	jmp    402ce7 <runtime::default_random_generator_proc+0x77>
  402cdd:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402ce2:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402ce7:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402cec:	48 85 c0             	test   %rax,%rax
  402cef:	74 27                	je     402d18 <runtime::default_random_generator_proc+0xa8>
  402cf1:	eb 00                	jmp    402cf3 <runtime::default_random_generator_proc+0x83>
  402cf3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402cf8:	48 83 e8 01          	sub    $0x1,%rax
  402cfc:	0f 84 17 01 00 00    	je     402e19 <runtime::default_random_generator_proc+0x1a9>
  402d02:	eb 00                	jmp    402d04 <runtime::default_random_generator_proc+0x94>
  402d04:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402d09:	48 83 e8 02          	sub    $0x2,%rax
  402d0d:	0f 84 40 01 00 00    	je     402e53 <runtime::default_random_generator_proc+0x1e3>
  402d13:	e9 6b 01 00 00       	jmp    402e83 <runtime::default_random_generator_proc+0x213>
  402d18:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402d1d:	48 83 38 00          	cmpq   $0x0,(%rax)
  402d21:	0f 94 c0             	sete   %al
  402d24:	24 01                	and    $0x1,%al
  402d26:	3c 00                	cmp    $0x0,%al
  402d28:	74 21                	je     402d4b <runtime::default_random_generator_proc+0xdb>
  402d2a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402d2f:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  402d34:	0f 94 c0             	sete   %al
  402d37:	24 01                	and    $0x1,%al
  402d39:	3c 00                	cmp    $0x0,%al
  402d3b:	74 0e                	je     402d4b <runtime::default_random_generator_proc+0xdb>
  402d3d:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402d42:	31 c0                	xor    %eax,%eax
  402d44:	89 c6                	mov    %eax,%esi
  402d46:	e8 85 40 00 00       	call   406dd0 <runtime::default_random_generator_proc.init-1>
  402d4b:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402d50:	48 83 e8 08          	sub    $0x8,%rax
  402d54:	75 26                	jne    402d7c <runtime::default_random_generator_proc+0x10c>
  402d56:	eb 00                	jmp    402d58 <runtime::default_random_generator_proc+0xe8>
  402d58:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402d5d:	e8 9e 3f 00 00       	call   406d00 <runtime::default_random_generator_proc.read_u64-0>
  402d62:	48 89 c1             	mov    %rax,%rcx
  402d65:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402d6a:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  402d6f:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402d74:	48 89 08             	mov    %rcx,(%rax)
  402d77:	e9 9b 00 00 00       	jmp    402e17 <runtime::default_random_generator_proc+0x1a7>
  402d7c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402d81:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  402d86:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  402d8d:	00 00 
  402d8f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402d94:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  402d9b:	ff ff 
  402d9d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402da2:	48 83 c0 01          	add    $0x1,%rax
  402da6:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402dab:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  402db0:	7d 63                	jge    402e15 <runtime::default_random_generator_proc+0x1a5>
  402db2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402db7:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  402dbc:	48 01 c8             	add    %rcx,%rax
  402dbf:	48 89 04 24          	mov    %rax,(%rsp)
  402dc3:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  402dc8:	0f 94 c0             	sete   %al
  402dcb:	24 01                	and    $0x1,%al
  402dcd:	3c 00                	cmp    $0x0,%al
  402dcf:	74 14                	je     402de5 <runtime::default_random_generator_proc+0x175>
  402dd1:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402dd6:	e8 25 3f 00 00       	call   406d00 <runtime::default_random_generator_proc.read_u64-0>
  402ddb:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402de0:	c6 44 24 57 07       	movb   $0x7,0x57(%rsp)
  402de5:	48 8b 04 24          	mov    (%rsp),%rax
  402de9:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402dee:	88 08                	mov    %cl,(%rax)
  402df0:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402df5:	48 c1 e9 08          	shr    $0x8,%rcx
  402df9:	b2 01                	mov    $0x1,%dl
  402dfb:	31 c0                	xor    %eax,%eax
  402dfd:	f6 c2 01             	test   $0x1,%dl
  402e00:	48 0f 45 c1          	cmovne %rcx,%rax
  402e04:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402e09:	8a 44 24 57          	mov    0x57(%rsp),%al
  402e0d:	2c 01                	sub    $0x1,%al
  402e0f:	88 44 24 57          	mov    %al,0x57(%rsp)
  402e13:	eb 88                	jmp    402d9d <runtime::default_random_generator_proc+0x12d>
  402e15:	eb 00                	jmp    402e17 <runtime::default_random_generator_proc+0x1a7>
  402e17:	eb 6a                	jmp    402e83 <runtime::default_random_generator_proc+0x213>
  402e19:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402e1e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402e23:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  402e2a:	00 00 
  402e2c:	b8 08 00 00 00       	mov    $0x8,%eax
  402e31:	48 39 d0             	cmp    %rdx,%rax
  402e34:	48 0f 4c d0          	cmovl  %rax,%rdx
  402e38:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402e3d:	e8 3e 0a 00 00       	call   403880 <runtime::mem_copy_non_overlapping>
  402e42:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402e47:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  402e4c:	e8 7f 3f 00 00       	call   406dd0 <runtime::default_random_generator_proc.init-1>
  402e51:	eb 30                	jmp    402e83 <runtime::default_random_generator_proc+0x213>
  402e53:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402e58:	48 83 f8 04          	cmp    $0x4,%rax
  402e5c:	0f 95 c0             	setne  %al
  402e5f:	24 01                	and    $0x1,%al
  402e61:	3c 00                	cmp    $0x0,%al
  402e63:	74 08                	je     402e6d <runtime::default_random_generator_proc+0x1fd>
  402e65:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402e6c:	c3                   	ret
  402e6d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402e72:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402e77:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402e7c:	8b 08                	mov    (%rax),%ecx
  402e7e:	83 c9 0a             	or     $0xa,%ecx
  402e81:	89 08                	mov    %ecx,(%rax)
  402e83:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402e8a:	c3                   	ret
  402e8b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402e90 <runtime::slice_handle_error>:
  402e90:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402e97:	4c 89 0c 24          	mov    %r9,(%rsp)
  402e9b:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  402ea0:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  402ea4:	89 54 24 14          	mov    %edx,0x14(%rsp)
  402ea8:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402ead:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  402eb2:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  402eb9:	00 
  402eba:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402ebf:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402ec4:	4c 8b 04 24          	mov    (%rsp),%r8
  402ec8:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  402ecd:	8b 44 24 10          	mov    0x10(%rsp),%eax
  402ed1:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  402ed5:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402eda:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  402edf:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402ee4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  402eeb:	00 
  402eec:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402ef0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402ef4:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  402ef9:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  402efe:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402f03:	0f 57 c0             	xorps  %xmm0,%xmm0
  402f06:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402f0b:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402f10:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402f17:	00 00 
  402f19:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402f1e:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402f23:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402f2a:	00 00 
  402f2c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  402f31:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402f36:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  402f3a:	89 44 24 44          	mov    %eax,0x44(%rsp)
  402f3e:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402f43:	e8 e8 17 00 00       	call   404730 <runtime::print_caller_location>
  402f48:	bf f9 81 40 00       	mov    $0x4081f9,%edi
  402f4d:	be 17 00 00 00       	mov    $0x17,%esi
  402f52:	e8 79 0f 00 00       	call   403ed0 <runtime::print_string>
  402f57:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402f5c:	e8 cf 15 00 00       	call   404530 <runtime::print_i64>
  402f61:	bf 11 82 40 00       	mov    $0x408211,%edi
  402f66:	be 01 00 00 00       	mov    $0x1,%esi
  402f6b:	e8 60 0f 00 00       	call   403ed0 <runtime::print_string>
  402f70:	48 8b 3c 24          	mov    (%rsp),%rdi
  402f74:	e8 b7 15 00 00       	call   404530 <runtime::print_i64>
  402f79:	bf 13 82 40 00       	mov    $0x408213,%edi
  402f7e:	be 15 00 00 00       	mov    $0x15,%esi
  402f83:	e8 48 0f 00 00       	call   403ed0 <runtime::print_string>
  402f88:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402f8d:	e8 9e 15 00 00       	call   404530 <runtime::print_i64>
  402f92:	bf 0a 00 00 00       	mov    $0xa,%edi
  402f97:	e8 a4 11 00 00       	call   404140 <runtime::print_byte>
  402f9c:	e8 7f e9 ff ff       	call   401920 <runtime::bounds_trap>
  402fa1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402fa8:	0f 1f 84 00 00 00 00 
  402faf:	00 

0000000000402fb0 <runtime::default_temp_allocator_proc>:
  402fb0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  402fb7:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  402fbc:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  402fc1:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402fc6:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  402fcb:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  402fd0:	40 88 f0             	mov    %sil,%al
  402fd3:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  402fd7:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  402fde:	00 
  402fdf:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402fe4:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  402feb:	00 
  402fec:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  402ff1:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  402ff8:	00 
  402ff9:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402ffe:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  403003:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403008:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40300d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403012:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  403016:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  40301b:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  403020:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  403025:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  40302c:	00 
  40302d:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  403034:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  40303b:	00 
  40303c:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  403043:	00 
  403044:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  40304b:	00 
  40304c:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  403053:	00 
  403054:	0f 57 c0             	xorps  %xmm0,%xmm0
  403057:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40305e:	00 
  40305f:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  403066:	00 
  403067:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  40306e:	00 
  40306f:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  403076:	00 
  403077:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40307e:	00 
  40307f:	48 89 e6             	mov    %rsp,%rsi
  403082:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  403086:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  40308d:	00 
  40308e:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  403092:	4c 89 16             	mov    %r10,(%rsi)
  403095:	0f b6 f0             	movzbl %al,%esi
  403098:	e8 63 18 00 00       	call   404900 <runtime::arena_allocator_proc>
  40309d:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4030a2:	40 88 c7             	mov    %al,%dil
  4030a5:	40 88 f8             	mov    %dil,%al
  4030a8:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  4030af:	00 
  4030b0:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  4030b7:	00 
  4030b8:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4030bf:	00 
  4030c0:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4030c7:	00 
  4030c8:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  4030cf:	00 
  4030d0:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4030d4:	48 89 11             	mov    %rdx,(%rcx)
  4030d7:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4030de:	c3                   	ret
  4030df:	90                   	nop

00000000004030e0 <runtime::multi_pointer_slice_handle_error>:
  4030e0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4030e7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4030ec:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4030f1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4030f5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4030f9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4030fe:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403103:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403108:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40310d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  403111:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  403115:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40311a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40311f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403124:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40312b:	00 
  40312c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  403130:	89 44 24 70          	mov    %eax,0x70(%rsp)
  403134:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  403139:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40313e:	0f 57 c0             	xorps  %xmm0,%xmm0
  403141:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403146:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40314b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403152:	00 00 
  403154:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403159:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40315e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403165:	00 00 
  403167:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40316c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403171:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  403175:	89 44 24 44          	mov    %eax,0x44(%rsp)
  403179:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40317e:	e8 ad 15 00 00       	call   404730 <runtime::print_caller_location>
  403183:	bf f9 81 40 00       	mov    $0x4081f9,%edi
  403188:	be 17 00 00 00       	mov    $0x17,%esi
  40318d:	e8 3e 0d 00 00       	call   403ed0 <runtime::print_string>
  403192:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403197:	e8 94 13 00 00       	call   404530 <runtime::print_i64>
  40319c:	bf 11 82 40 00       	mov    $0x408211,%edi
  4031a1:	be 01 00 00 00       	mov    $0x1,%esi
  4031a6:	e8 25 0d 00 00       	call   403ed0 <runtime::print_string>
  4031ab:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4031b0:	e8 7b 13 00 00       	call   404530 <runtime::print_i64>
  4031b5:	bf 0a 00 00 00       	mov    $0xa,%edi
  4031ba:	e8 81 0f 00 00       	call   404140 <runtime::print_byte>
  4031bf:	e8 5c e7 ff ff       	call   401920 <runtime::bounds_trap>
  4031c4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4031cb:	00 00 00 00 00 

00000000004031d0 <runtime::memory_block_dealloc>:
  4031d0:	53                   	push   %rbx
  4031d1:	48 81 ec 80 00 00 00 	sub    $0x80,%rsp
  4031d8:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4031dd:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4031e2:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4031e7:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4031ec:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4031f1:	48 83 f8 00          	cmp    $0x0,%rax
  4031f5:	0f 95 c0             	setne  %al
  4031f8:	24 01                	and    $0x1,%al
  4031fa:	3c 00                	cmp    $0x0,%al
  4031fc:	0f 84 9d 00 00 00    	je     40329f <runtime::memory_block_dealloc+0xcf>
  403202:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403207:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40320c:	48 8b 4a 08          	mov    0x8(%rdx),%rcx
  403210:	48 8b 52 10          	mov    0x10(%rdx),%rdx
  403214:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  403219:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40321e:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403223:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403228:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40322d:	48 89 54 24 68       	mov    %rdx,0x68(%rsp)
  403232:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  403237:	48 83 f8 00          	cmp    $0x0,%rax
  40323b:	0f 94 c0             	sete   %al
  40323e:	a8 01                	test   $0x1,%al
  403240:	75 0f                	jne    403251 <runtime::memory_block_dealloc+0x81>
  403242:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  403248:	0f 94 c0             	sete   %al
  40324b:	a8 01                	test   $0x1,%al
  40324d:	75 02                	jne    403251 <runtime::memory_block_dealloc+0x81>
  40324f:	eb 02                	jmp    403253 <runtime::memory_block_dealloc+0x83>
  403251:	eb 4a                	jmp    40329d <runtime::memory_block_dealloc+0xcd>
  403253:	4c 8b 54 24 28       	mov    0x28(%rsp),%r10
  403258:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  40325d:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  403262:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  403267:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40326c:	0f 57 c0             	xorps  %xmm0,%xmm0
  40326f:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  403274:	be 01 00 00 00       	mov    $0x1,%esi
  403279:	31 c9                	xor    %ecx,%ecx
  40327b:	41 89 c9             	mov    %ecx,%r9d
  40327e:	4c 8d 5c 24 50       	lea    0x50(%rsp),%r11
  403283:	4c 89 ca             	mov    %r9,%rdx
  403286:	4c 89 c9             	mov    %r9,%rcx
  403289:	48 89 1c 24          	mov    %rbx,(%rsp)
  40328d:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  403292:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  403297:	ff d0                	call   *%rax
  403299:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  40329d:	eb 00                	jmp    40329f <runtime::memory_block_dealloc+0xcf>
  40329f:	48 81 c4 80 00 00 00 	add    $0x80,%rsp
  4032a6:	5b                   	pop    %rbx
  4032a7:	c3                   	ret
  4032a8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4032af:	00 

00000000004032b0 <main>:
  4032b0:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  4032b7:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  4032bb:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4032c0:	8b 44 24 14          	mov    0x14(%rsp),%eax
  4032c4:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4032c9:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  4032d0:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  4032d7:	00 
  4032d8:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  4032df:	00 
  4032e0:	48 89 0c 24          	mov    %rcx,(%rsp)
  4032e4:	4c 63 c8             	movslq %eax,%r9
  4032e7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4032ec:	bf 29 82 40 00       	mov    $0x408229,%edi
  4032f1:	31 c0                	xor    %eax,%eax
  4032f3:	41 89 c0             	mov    %eax,%r8d
  4032f6:	be 2c 00 00 00       	mov    $0x2c,%esi
  4032fb:	ba 36 00 00 00       	mov    $0x36,%edx
  403300:	b9 11 00 00 00       	mov    $0x11,%ecx
  403305:	e8 36 04 00 00       	call   403740 <runtime::multi_pointer_slice_expr_error>
  40330a:	48 8b 0c 24          	mov    (%rsp),%rcx
  40330e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403313:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  40331a:	00 
  40331b:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  403322:	00 
  403323:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  40332a:	00 
  40332b:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403332:	00 
  403333:	48 c7 c0 60 b0 40 00 	mov    $0x40b060,%rax
  40333a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40333e:	48 89 08             	mov    %rcx,(%rax)
  403341:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  403348:	00 
  403349:	31 f6                	xor    %esi,%esi
  40334b:	ba 70 00 00 00       	mov    $0x70,%edx
  403350:	e8 eb dc ff ff       	call   401040 <memset@plt>
  403355:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40335c:	00 
  40335d:	e8 ae 25 00 00       	call   405910 <runtime::[core.odin]::__init_context>
  403362:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  403367:	31 f6                	xor    %esi,%esi
  403369:	ba 70 00 00 00       	mov    $0x70,%edx
  40336e:	e8 cd dc ff ff       	call   401040 <memset@plt>
  403373:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  403378:	e8 43 25 00 00       	call   4058c0 <runtime::default_context>
  40337d:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  403382:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  403387:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  40338c:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  403391:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  403396:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  40339b:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  4033a2:	00 
  4033a3:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  4033aa:	00 
  4033ab:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  4033b2:	00 
  4033b3:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  4033ba:	00 
  4033bb:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  4033c2:	00 
  4033c3:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  4033ca:	00 
  4033cb:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  4033d2:	00 
  4033d3:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4033da:	00 
  4033db:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4033e2:	00 
  4033e3:	e8 68 3d 00 00       	call   407150 <__$startup_runtime>
  4033e8:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4033ef:	00 
  4033f0:	e8 9b dd ff ff       	call   401190 <journey::main>
  4033f5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4033fc:	00 
  4033fd:	e8 5e 3d 00 00       	call   407160 <__$cleanup_runtime>
  403402:	31 c0                	xor    %eax,%eax
  403404:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  40340b:	c3                   	ret
  40340c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403410 <runtime::alloc_from_memory_block>:
  403410:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  403417:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40341c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403421:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403426:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40342b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403430:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403435:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40343a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  403441:	00 
  403442:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  403449:	00 
  40344a:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  403451:	00 
  403452:	0f 57 c0             	xorps  %xmm0,%xmm0
  403455:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40345c:	00 
  40345d:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  403464:	00 
  403465:	48 83 f8 00          	cmp    $0x0,%rax
  403469:	0f 94 c0             	sete   %al
  40346c:	24 01                	and    $0x1,%al
  40346e:	3c 00                	cmp    $0x0,%al
  403470:	74 3e                	je     4034b0 <runtime::alloc_from_memory_block+0xa0>
  403472:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403477:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  40347e:	00 00 00 00 00 
  403483:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  40348a:	00 00 00 00 00 
  40348f:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  403496:	01 
  403497:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40349e:	00 
  40349f:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4034a6:	b0 01                	mov    $0x1,%al
  4034a8:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  4034af:	c3                   	ret
  4034b0:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4034b5:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4034ba:	e8 b1 39 00 00       	call   406e70 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  4034bf:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4034c4:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4034cb:	00 
  4034cc:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  4034d3:	00 
  4034d4:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  4034db:	00 00 00 00 00 
  4034e0:	48 89 8c 24 00 01 00 	mov    %rcx,0x100(%rsp)
  4034e7:	00 
  4034e8:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  4034ef:	00 
  4034f0:	48 01 c1             	add    %rax,%rcx
  4034f3:	0f 92 c0             	setb   %al
  4034f6:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  4034fd:	00 
  4034fe:	24 01                	and    $0x1,%al
  403500:	88 84 24 ef 00 00 00 	mov    %al,0xef(%rsp)
  403507:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  40350e:	00 
  40350f:	80 bc 24 ef 00 00 00 	cmpb   $0x0,0xef(%rsp)
  403516:	00 
  403517:	0f 94 c0             	sete   %al
  40351a:	24 01                	and    $0x1,%al
  40351c:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  403523:	00 
  403524:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40352b:	00 
  40352c:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  403531:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  403535:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  40353a:	75 4a                	jne    403586 <runtime::alloc_from_memory_block+0x176>
  40353c:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403541:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  403548:	01 
  403549:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  403550:	00 
  403551:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403558:	00 
  403559:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  403560:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  403567:	00 
  403568:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40356f:	00 
  403570:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  403577:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40357b:	48 89 11             	mov    %rdx,(%rcx)
  40357e:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  403585:	c3                   	ret
  403586:	eb 00                	jmp    403588 <runtime::alloc_from_memory_block+0x178>
  403588:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40358f:	00 
  403590:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403594:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403599:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  4035a0:	00 00 
  4035a2:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  4035a9:	00 
  4035aa:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4035b1:	00 
  4035b2:	48 01 c1             	add    %rax,%rcx
  4035b5:	0f 92 c0             	setb   %al
  4035b8:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  4035bf:	00 
  4035c0:	24 01                	and    $0x1,%al
  4035c2:	88 84 24 cf 00 00 00 	mov    %al,0xcf(%rsp)
  4035c9:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  4035d0:	00 
  4035d1:	80 bc 24 cf 00 00 00 	cmpb   $0x0,0xcf(%rsp)
  4035d8:	00 
  4035d9:	0f 94 c0             	sete   %al
  4035dc:	24 01                	and    $0x1,%al
  4035de:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  4035e3:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4035e8:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4035ed:	88 44 24 47          	mov    %al,0x47(%rsp)
  4035f1:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  4035f6:	74 1a                	je     403612 <runtime::alloc_from_memory_block+0x202>
  4035f8:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4035fd:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403604:	00 
  403605:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  403609:	0f 97 c0             	seta   %al
  40360c:	24 01                	and    $0x1,%al
  40360e:	3c 00                	cmp    $0x0,%al
  403610:	74 4a                	je     40365c <runtime::alloc_from_memory_block+0x24c>
  403612:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403617:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  40361e:	01 
  40361f:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  403626:	00 
  403627:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  40362e:	00 
  40362f:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  403636:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40363d:	00 
  40363e:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403645:	00 
  403646:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  40364d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403651:	48 89 11             	mov    %rdx,(%rcx)
  403654:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  40365b:	c3                   	ret
  40365c:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403661:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403668:	00 
  403669:	48 8b 41 18          	mov    0x18(%rcx),%rax
  40366d:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  403671:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  403678:	00 
  403679:	48 01 d1             	add    %rdx,%rcx
  40367c:	48 01 c8             	add    %rcx,%rax
  40367f:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403684:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403689:	48 89 04 24          	mov    %rax,(%rsp)
  40368d:	bf a0 80 40 00       	mov    $0x4080a0,%edi
  403692:	31 c0                	xor    %eax,%eax
  403694:	41 89 c0             	mov    %eax,%r8d
  403697:	be 3e 00 00 00       	mov    $0x3e,%esi
  40369c:	ba 55 00 00 00       	mov    $0x55,%edx
  4036a1:	b9 31 00 00 00       	mov    $0x31,%ecx
  4036a6:	e8 95 00 00 00       	call   403740 <runtime::multi_pointer_slice_expr_error>
  4036ab:	48 8b 14 24          	mov    (%rsp),%rdx
  4036af:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4036b4:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4036b9:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4036be:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4036c3:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4036c8:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4036cd:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  4036d4:	00 
  4036d5:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4036dc:	00 
  4036dd:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4036e4:	00 
  4036e5:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4036ea:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4036ee:	48 01 f2             	add    %rsi,%rdx
  4036f1:	48 89 50 20          	mov    %rdx,0x20(%rax)
  4036f5:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4036fc:	00 
  4036fd:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403704:	00 
  403705:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  40370c:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  403713:	00 
  403714:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40371b:	00 
  40371c:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  403723:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403727:	48 89 11             	mov    %rdx,(%rcx)
  40372a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  403731:	c3                   	ret
  403732:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403739:	1f 84 00 00 00 00 00 

0000000000403740 <runtime::multi_pointer_slice_expr_error>:
  403740:	48 83 ec 58          	sub    $0x58,%rsp
  403744:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403749:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40374e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403752:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403756:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40375b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403760:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403765:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40376a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40376e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403772:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403777:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40377c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403781:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403786:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40378a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40378e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403793:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403798:	48 39 c8             	cmp    %rcx,%rax
  40379b:	0f 9e c0             	setle  %al
  40379e:	24 01                	and    $0x1,%al
  4037a0:	3c 00                	cmp    $0x0,%al
  4037a2:	74 05                	je     4037a9 <runtime::multi_pointer_slice_expr_error+0x69>
  4037a4:	48 83 c4 58          	add    $0x58,%rsp
  4037a8:	c3                   	ret
  4037a9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4037ae:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4037b3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4037b7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4037bb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4037c0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4037c5:	e8 16 f9 ff ff       	call   4030e0 <runtime::multi_pointer_slice_handle_error>
  4037ca:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004037d0 <runtime::slice_expr_error_hi>:
  4037d0:	48 83 ec 58          	sub    $0x58,%rsp
  4037d4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4037d9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4037de:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4037e2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4037e6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4037eb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4037f0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4037f5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4037fa:	8b 54 24 18          	mov    0x18(%rsp),%edx
  4037fe:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403802:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403807:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40380c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403811:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403816:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40381a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40381e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403823:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403828:	31 c0                	xor    %eax,%eax
  40382a:	48 39 c8             	cmp    %rcx,%rax
  40382d:	0f 9e c0             	setle  %al
  403830:	24 01                	and    $0x1,%al
  403832:	3c 00                	cmp    $0x0,%al
  403834:	74 1b                	je     403851 <runtime::slice_expr_error_hi+0x81>
  403836:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40383b:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403840:	48 39 c8             	cmp    %rcx,%rax
  403843:	0f 9e c0             	setle  %al
  403846:	24 01                	and    $0x1,%al
  403848:	3c 00                	cmp    $0x0,%al
  40384a:	74 05                	je     403851 <runtime::slice_expr_error_hi+0x81>
  40384c:	48 83 c4 58          	add    $0x58,%rsp
  403850:	c3                   	ret
  403851:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403856:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40385a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40385e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403863:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403868:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40386d:	48 89 e0             	mov    %rsp,%rax
  403870:	4c 89 00             	mov    %r8,(%rax)
  403873:	31 c0                	xor    %eax,%eax
  403875:	41 89 c0             	mov    %eax,%r8d
  403878:	e8 13 f6 ff ff       	call   402e90 <runtime::slice_handle_error>
  40387d:	0f 1f 00             	nopl   (%rax)

0000000000403880 <runtime::mem_copy_non_overlapping>:
  403880:	48 83 ec 38          	sub    $0x38,%rsp
  403884:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403889:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40388e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403893:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403898:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40389d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4038a2:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4038a7:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4038ac:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4038b1:	48 83 f8 00          	cmp    $0x0,%rax
  4038b5:	0f 95 c0             	setne  %al
  4038b8:	24 01                	and    $0x1,%al
  4038ba:	3c 00                	cmp    $0x0,%al
  4038bc:	74 3c                	je     4038fa <runtime::mem_copy_non_overlapping+0x7a>
  4038be:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4038c3:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4038c8:	48 39 c8             	cmp    %rcx,%rax
  4038cb:	0f 95 c0             	setne  %al
  4038ce:	24 01                	and    $0x1,%al
  4038d0:	3c 00                	cmp    $0x0,%al
  4038d2:	74 26                	je     4038fa <runtime::mem_copy_non_overlapping+0x7a>
  4038d4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4038d9:	48 83 f8 00          	cmp    $0x0,%rax
  4038dd:	0f 9f c0             	setg   %al
  4038e0:	24 01                	and    $0x1,%al
  4038e2:	3c 00                	cmp    $0x0,%al
  4038e4:	74 14                	je     4038fa <runtime::mem_copy_non_overlapping+0x7a>
  4038e6:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4038eb:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4038f0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4038f5:	e8 66 d7 ff ff       	call   401060 <memcpy@plt>
  4038fa:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4038ff:	48 83 c4 38          	add    $0x38,%rsp
  403903:	c3                   	ret
  403904:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40390b:	00 00 00 00 00 

0000000000403910 <runtime::slice_expr_error_lo_hi>:
  403910:	48 83 ec 68          	sub    $0x68,%rsp
  403914:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403919:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40391e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403922:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403926:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40392b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403930:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403935:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40393a:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40393f:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403944:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403949:	8b 74 24 18          	mov    0x18(%rsp),%esi
  40394d:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  403951:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403956:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  40395b:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  403960:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  403965:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  403969:	89 74 24 50          	mov    %esi,0x50(%rsp)
  40396d:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  403972:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  403977:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40397c:	31 c0                	xor    %eax,%eax
  40397e:	48 39 c8             	cmp    %rcx,%rax
  403981:	0f 9e c0             	setle  %al
  403984:	24 01                	and    $0x1,%al
  403986:	3c 00                	cmp    $0x0,%al
  403988:	74 47                	je     4039d1 <runtime::slice_expr_error_lo_hi+0xc1>
  40398a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40398f:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403994:	48 39 c8             	cmp    %rcx,%rax
  403997:	0f 9e c0             	setle  %al
  40399a:	24 01                	and    $0x1,%al
  40399c:	3c 00                	cmp    $0x0,%al
  40399e:	74 31                	je     4039d1 <runtime::slice_expr_error_lo_hi+0xc1>
  4039a0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4039a5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4039aa:	48 39 c8             	cmp    %rcx,%rax
  4039ad:	0f 9e c0             	setle  %al
  4039b0:	24 01                	and    $0x1,%al
  4039b2:	3c 00                	cmp    $0x0,%al
  4039b4:	74 1b                	je     4039d1 <runtime::slice_expr_error_lo_hi+0xc1>
  4039b6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4039bb:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4039c0:	48 39 c8             	cmp    %rcx,%rax
  4039c3:	0f 9e c0             	setle  %al
  4039c6:	24 01                	and    $0x1,%al
  4039c8:	3c 00                	cmp    $0x0,%al
  4039ca:	74 05                	je     4039d1 <runtime::slice_expr_error_lo_hi+0xc1>
  4039cc:	48 83 c4 68          	add    $0x68,%rsp
  4039d0:	c3                   	ret
  4039d1:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4039d6:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4039db:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4039df:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4039e3:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4039e8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4039ed:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  4039f2:	48 89 e0             	mov    %rsp,%rax
  4039f5:	4c 89 10             	mov    %r10,(%rax)
  4039f8:	e8 93 f4 ff ff       	call   402e90 <runtime::slice_handle_error>
  4039fd:	0f 1f 00             	nopl   (%rax)

0000000000403a00 <runtime::memset>:
  403a00:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  403a05:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  403a09:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  403a0e:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  403a13:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  403a18:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  403a1c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  403a21:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  403a25:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  403a2a:	48 83 f8 00          	cmp    $0x0,%rax
  403a2e:	0f 95 c0             	setne  %al
  403a31:	24 01                	and    $0x1,%al
  403a33:	3c 00                	cmp    $0x0,%al
  403a35:	74 63                	je     403a9a <runtime::memset+0x9a>
  403a37:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  403a3c:	48 83 f8 00          	cmp    $0x0,%rax
  403a40:	0f 95 c0             	setne  %al
  403a43:	24 01                	and    $0x1,%al
  403a45:	3c 00                	cmp    $0x0,%al
  403a47:	74 51                	je     403a9a <runtime::memset+0x9a>
  403a49:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  403a4e:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  403a52:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  403a56:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  403a5b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  403a62:	00 00 
  403a64:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  403a69:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  403a6e:	0f 9c c0             	setl   %al
  403a71:	24 01                	and    $0x1,%al
  403a73:	3c 00                	cmp    $0x0,%al
  403a75:	74 21                	je     403a98 <runtime::memset+0x98>
  403a77:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  403a7c:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  403a81:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  403a85:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  403a88:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  403a8d:	48 83 c0 01          	add    $0x1,%rax
  403a91:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  403a96:	eb cc                	jmp    403a64 <runtime::memset+0x64>
  403a98:	eb 00                	jmp    403a9a <runtime::memset+0x9a>
  403a9a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  403a9f:	c3                   	ret

0000000000403aa0 <runtime::arena_alloc>:
  403aa0:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  403aa7:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403aac:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403ab1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  403ab6:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403abb:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403ac0:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  403ac5:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  403aca:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403acf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403ad4:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403ad9:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  403ade:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  403ae5:	00 
  403ae6:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  403aed:	00 
  403aee:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  403af5:	00 
  403af6:	0f 57 c0             	xorps  %xmm0,%xmm0
  403af9:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  403b00:	00 
  403b01:	c6 84 24 0f 01 00 00 	movb   $0x0,0x10f(%rsp)
  403b08:	00 
  403b09:	48 89 c2             	mov    %rax,%rdx
  403b0c:	48 83 ea 01          	sub    $0x1,%rdx
  403b10:	48 21 d0             	and    %rdx,%rax
  403b13:	48 83 f8 00          	cmp    $0x0,%rax
  403b17:	0f 94 c0             	sete   %al
  403b1a:	24 01                	and    $0x1,%al
  403b1c:	0f b6 f8             	movzbl %al,%edi
  403b1f:	be 56 82 40 00       	mov    $0x408256,%esi
  403b24:	ba 1a 00 00 00       	mov    $0x1a,%edx
  403b29:	e8 e2 2a 00 00       	call   406610 <runtime::assert>
  403b2e:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403b33:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  403b3a:	00 
  403b3b:	48 83 bc 24 00 01 00 	cmpq   $0x0,0x100(%rsp)
  403b42:	00 00 
  403b44:	0f 94 c0             	sete   %al
  403b47:	24 01                	and    $0x1,%al
  403b49:	3c 00                	cmp    $0x0,%al
  403b4b:	74 42                	je     403b8f <runtime::arena_alloc+0xef>
  403b4d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403b52:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403b59:	00 
  403b5a:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403b61:	00 
  403b62:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403b69:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403b70:	00 
  403b71:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403b78:	00 
  403b79:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403b80:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403b84:	48 89 11             	mov    %rdx,(%rcx)
  403b87:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403b8e:	c3                   	ret
  403b8f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403b96:	00 
  403b97:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  403b9c:	0f 94 c0             	sete   %al
  403b9f:	24 01                	and    $0x1,%al
  403ba1:	3c 00                	cmp    $0x0,%al
  403ba3:	74 09                	je     403bae <runtime::arena_alloc+0x10e>
  403ba5:	31 c0                	xor    %eax,%eax
  403ba7:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403bac:	eb 15                	jmp    403bc3 <runtime::arena_alloc+0x123>
  403bae:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403bb5:	00 
  403bb6:	48 8b 40 10          	mov    0x10(%rax),%rax
  403bba:	48 8b 40 20          	mov    0x20(%rax),%rax
  403bbe:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403bc3:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403bc8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403bcd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403bd2:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  403bd9:	00 
  403bda:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403be1:	00 
  403be2:	48 8b 78 10          	mov    0x10(%rax),%rdi
  403be6:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  403bed:	00 
  403bee:	0f 57 c0             	xorps  %xmm0,%xmm0
  403bf1:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  403bf8:	00 
  403bf9:	48 8d 8c 24 e0 00 00 	lea    0xe0(%rsp),%rcx
  403c00:	00 
  403c01:	e8 0a f8 ff ff       	call   403410 <runtime::alloc_from_memory_block>
  403c06:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  403c0d:	00 
  403c0e:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  403c15:	00 
  403c16:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  403c1d:	00 
  403c1e:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403c25:	00 
  403c26:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403c2d:	80 bc 24 0f 01 00 00 	cmpb   $0x1,0x10f(%rsp)
  403c34:	01 
  403c35:	0f 94 c0             	sete   %al
  403c38:	24 01                	and    $0x1,%al
  403c3a:	3c 00                	cmp    $0x0,%al
  403c3c:	0f 84 19 02 00 00    	je     403e5b <runtime::arena_alloc+0x3bb>
  403c42:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403c49:	00 
  403c4a:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  403c4f:	0f 94 c0             	sete   %al
  403c52:	24 01                	and    $0x1,%al
  403c54:	3c 00                	cmp    $0x0,%al
  403c56:	74 10                	je     403c68 <runtime::arena_alloc+0x1c8>
  403c58:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403c5f:	00 
  403c60:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  403c67:	00 
  403c68:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403c6d:	48 8b bc 24 00 01 00 	mov    0x100(%rsp),%rdi
  403c74:	00 
  403c75:	e8 86 32 00 00       	call   406f00 <runtime::arena_alloc.align_forward_uint-0>
  403c7a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  403c81:	00 
  403c82:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403c89:	00 
  403c8a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403c91:	00 
  403c92:	48 8b 40 28          	mov    0x28(%rax),%rax
  403c96:	48 39 c1             	cmp    %rax,%rcx
  403c99:	48 0f 47 c1          	cmova  %rcx,%rax
  403c9d:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  403ca4:	00 
  403ca5:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403cac:	00 
  403cad:	48 83 38 00          	cmpq   $0x0,(%rax)
  403cb1:	0f 94 c0             	sete   %al
  403cb4:	24 01                	and    $0x1,%al
  403cb6:	3c 00                	cmp    $0x0,%al
  403cb8:	74 46                	je     403d00 <runtime::arena_alloc+0x260>
  403cba:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  403cbf:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403cc6:	00 
  403cc7:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403ccc:	e8 5f dc ff ff       	call   401930 <runtime::heap_allocator>
  403cd1:	48 89 c1             	mov    %rax,%rcx
  403cd4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403cd9:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  403ce0:	00 
  403ce1:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  403ce8:	00 
  403ce9:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403cf0:	00 
  403cf1:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  403cf8:	00 
  403cf9:	48 89 50 08          	mov    %rdx,0x8(%rax)
  403cfd:	48 89 08             	mov    %rcx,(%rax)
  403d00:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  403d05:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  403d0a:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  403d0f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403d16:	00 
  403d17:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  403d1e:	00 
  403d1f:	48 8b 38             	mov    (%rax),%rdi
  403d22:	48 8b 70 08          	mov    0x8(%rax),%rsi
  403d26:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  403d2d:	00 00 00 00 00 
  403d32:	48 89 e0             	mov    %rsp,%rax
  403d35:	4c 89 08             	mov    %r9,(%rax)
  403d38:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  403d3f:	00 
  403d40:	e8 bb ea ff ff       	call   402800 <runtime::memory_block_alloc>
  403d45:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403d49:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  403d50:	00 
  403d51:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  403d56:	3c 00                	cmp    $0x0,%al
  403d58:	74 4d                	je     403da7 <runtime::arena_alloc+0x307>
  403d5a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403d5f:	8a 44 24 0f          	mov    0xf(%rsp),%al
  403d63:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403d6a:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403d71:	00 
  403d72:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403d79:	00 
  403d7a:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403d81:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403d88:	00 
  403d89:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403d90:	00 
  403d91:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403d98:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403d9c:	48 89 11             	mov    %rdx,(%rcx)
  403d9f:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403da6:	c3                   	ret
  403da7:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403dac:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403db1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403db6:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403dbd:	00 
  403dbe:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403dc5:	00 
  403dc6:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  403dcd:	00 
  403dce:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  403dd2:	48 89 08             	mov    %rcx,(%rax)
  403dd5:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403ddc:	00 
  403ddd:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403de4:	00 
  403de5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403de9:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403df0:	00 
  403df1:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403df8:	00 
  403df9:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  403dfd:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403e01:	48 01 f1             	add    %rsi,%rcx
  403e04:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403e08:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  403e0f:	00 00 00 00 00 
  403e14:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403e1b:	00 
  403e1c:	48 8b 78 10          	mov    0x10(%rax),%rdi
  403e20:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  403e27:	00 
  403e28:	0f 57 c0             	xorps  %xmm0,%xmm0
  403e2b:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  403e30:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  403e35:	e8 d6 f5 ff ff       	call   403410 <runtime::alloc_from_memory_block>
  403e3a:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  403e3f:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  403e44:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  403e4b:	00 
  403e4c:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403e53:	00 
  403e54:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403e5b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403e60:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403e67:	00 
  403e68:	48 8b 70 10          	mov    0x10(%rax),%rsi
  403e6c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  403e70:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  403e74:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  403e7b:	00 
  403e7c:	48 29 fe             	sub    %rdi,%rsi
  403e7f:	48 01 f2             	add    %rsi,%rdx
  403e82:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403e86:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403e8d:	00 
  403e8e:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403e95:	00 
  403e96:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403e9d:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403ea4:	00 
  403ea5:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403eac:	00 
  403ead:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403eb4:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403eb8:	48 89 11             	mov    %rdx,(%rcx)
  403ebb:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403ec2:	c3                   	ret
  403ec3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403eca:	84 00 00 00 00 00 

0000000000403ed0 <runtime::print_string>:
  403ed0:	48 83 ec 58          	sub    $0x58,%rsp
  403ed4:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403ed9:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403ede:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403ee3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403ee8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403eed:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  403ef2:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  403ef9:	00 00 
  403efb:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403f00:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403f05:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403f0a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403f0f:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403f16:	00 00 
  403f18:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403f1d:	e8 de e3 ff ff       	call   402300 <runtime::stderr_write>
  403f22:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403f27:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403f2c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403f31:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403f36:	48 83 c4 58          	add    $0x58,%rsp
  403f3a:	c3                   	ret
  403f3b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403f40 <runtime::mem_alloc>:
  403f40:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  403f47:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  403f4c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  403f51:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403f56:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403f5b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403f60:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  403f65:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403f6c:	00 
  403f6d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403f72:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403f77:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403f7c:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  403f81:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403f86:	48 89 b4 24 98 00 00 	mov    %rsi,0x98(%rsp)
  403f8d:	00 
  403f8e:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  403f95:	00 
  403f96:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  403f9d:	00 
  403f9e:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  403fa5:	00 
  403fa6:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  403fad:	00 
  403fae:	48 83 f8 00          	cmp    $0x0,%rax
  403fb2:	0f 9e c0             	setle  %al
  403fb5:	a8 01                	test   $0x1,%al
  403fb7:	75 02                	jne    403fbb <runtime::mem_alloc+0x7b>
  403fb9:	eb 08                	jmp    403fc3 <runtime::mem_alloc+0x83>
  403fbb:	31 c0                	xor    %eax,%eax
  403fbd:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  403fc1:	eb 1c                	jmp    403fdf <runtime::mem_alloc+0x9f>
  403fc3:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403fc8:	48 89 c1             	mov    %rax,%rcx
  403fcb:	48 83 e9 01          	sub    $0x1,%rcx
  403fcf:	48 21 c8             	and    %rcx,%rax
  403fd2:	48 83 f8 00          	cmp    $0x0,%rax
  403fd6:	0f 94 c0             	sete   %al
  403fd9:	24 01                	and    $0x1,%al
  403fdb:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  403fdf:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403fe4:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403fe9:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  403fed:	0f b6 f8             	movzbl %al,%edi
  403ff0:	be 71 82 40 00       	mov    $0x408271,%esi
  403ff5:	ba 20 00 00 00       	mov    $0x20,%edx
  403ffa:	e8 11 26 00 00       	call   406610 <runtime::assert>
  403fff:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  404004:	48 83 f8 00          	cmp    $0x0,%rax
  404008:	0f 94 c0             	sete   %al
  40400b:	24 01                	and    $0x1,%al
  40400d:	3c 00                	cmp    $0x0,%al
  40400f:	75 12                	jne    404023 <runtime::mem_alloc+0xe3>
  404011:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  404018:	00 00 
  40401a:	0f 94 c0             	sete   %al
  40401d:	24 01                	and    $0x1,%al
  40401f:	3c 00                	cmp    $0x0,%al
  404021:	74 1e                	je     404041 <runtime::mem_alloc+0x101>
  404023:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404028:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40402f:	00 
  404030:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  404037:	31 c0                	xor    %eax,%eax
  404039:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  404040:	c3                   	ret
  404041:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  404046:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40404b:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  404050:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  404055:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40405c:	00 
  40405d:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  404064:	00 
  404065:	0f 57 c0             	xorps  %xmm0,%xmm0
  404068:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40406d:	48 89 e6             	mov    %rsp,%rsi
  404070:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  404074:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  404079:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  40407d:	4c 89 06             	mov    %r8,(%rsi)
  404080:	31 f6                	xor    %esi,%esi
  404082:	41 89 f1             	mov    %esi,%r9d
  404085:	4d 89 c8             	mov    %r9,%r8
  404088:	ff d0                	call   *%rax
  40408a:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40408f:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  404094:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  404099:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40409d:	48 89 11             	mov    %rdx,(%rcx)
  4040a0:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  4040a7:	c3                   	ret
  4040a8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4040af:	00 

00000000004040b0 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  4040b0:	48 83 ec 48          	sub    $0x48,%rsp
  4040b4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4040b8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4040bd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4040c2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4040c7:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4040cc:	48 8b 04 24          	mov    (%rsp),%rax
  4040d0:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4040d5:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4040da:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4040df:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4040e4:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4040e9:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4040ee:	48 39 c1             	cmp    %rax,%rcx
  4040f1:	48 0f 4c c1          	cmovl  %rcx,%rax
  4040f5:	31 c9                	xor    %ecx,%ecx
  4040f7:	48 39 c1             	cmp    %rax,%rcx
  4040fa:	48 0f 4f c1          	cmovg  %rcx,%rax
  4040fe:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  404103:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  404109:	0f 9f c0             	setg   %al
  40410c:	24 01                	and    $0x1,%al
  40410e:	3c 00                	cmp    $0x0,%al
  404110:	74 18                	je     40412a <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)+0x7a>
  404112:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404117:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40411c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  404121:	48 c1 e2 00          	shl    $0x0,%rdx
  404125:	e8 66 cf ff ff       	call   401090 <memmove@plt>
  40412a:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40412f:	48 83 c4 48          	add    $0x48,%rsp
  404133:	c3                   	ret
  404134:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40413b:	00 00 00 00 00 

0000000000404140 <runtime::print_byte>:
  404140:	48 83 ec 68          	sub    $0x68,%rsp
  404144:	40 88 f8             	mov    %dil,%al
  404147:	88 44 24 07          	mov    %al,0x7(%rsp)
  40414b:	8a 54 24 07          	mov    0x7(%rsp),%dl
  40414f:	88 54 24 67          	mov    %dl,0x67(%rsp)
  404153:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  40415a:	00 00 
  40415c:	0f 57 c0             	xorps  %xmm0,%xmm0
  40415f:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  404164:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  404169:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  40416e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  404173:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  40417a:	00 00 
  40417c:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  404181:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  404186:	88 11                	mov    %dl,(%rcx)
  404188:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40418d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404192:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  404197:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40419c:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  4041a3:	00 00 
  4041a5:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  4041aa:	e8 51 e1 ff ff       	call   402300 <runtime::stderr_write>
  4041af:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4041b4:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4041b9:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4041be:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4041c3:	48 83 c4 68          	add    $0x68,%rsp
  4041c7:	c3                   	ret
  4041c8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4041cf:	00 

00000000004041d0 <runtime::matrix_bounds_check_error>:
  4041d0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4041d7:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  4041dc:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  4041e1:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  4041e5:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  4041e9:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  4041ee:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4041f3:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  4041fa:	00 
  4041fb:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404200:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  404207:	00 
  404208:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40420d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404212:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  404217:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40421c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404221:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  404225:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  40422a:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  40422f:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  404234:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  404239:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  404240:	00 
  404241:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  404246:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  40424a:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40424f:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  404254:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  404259:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40425e:	48 39 c8             	cmp    %rcx,%rax
  404261:	0f 92 c0             	setb   %al
  404264:	24 01                	and    $0x1,%al
  404266:	3c 00                	cmp    $0x0,%al
  404268:	74 1e                	je     404288 <runtime::matrix_bounds_check_error+0xb8>
  40426a:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40426f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  404274:	48 39 c8             	cmp    %rcx,%rax
  404277:	0f 92 c0             	setb   %al
  40427a:	24 01                	and    $0x1,%al
  40427c:	3c 00                	cmp    $0x0,%al
  40427e:	74 08                	je     404288 <runtime::matrix_bounds_check_error+0xb8>
  404280:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  404287:	c3                   	ret
  404288:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40428d:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  404292:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  404296:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  40429a:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40429f:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4042a4:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  4042a9:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  4042ae:	48 89 e0             	mov    %rsp,%rax
  4042b1:	4c 89 58 08          	mov    %r11,0x8(%rax)
  4042b5:	4c 89 10             	mov    %r10,(%rax)
  4042b8:	e8 a3 2c 00 00       	call   406f60 <runtime::matrix_bounds_check_error.handle_error-0>
  4042bd:	0f 1f 00             	nopl   (%rax)

00000000004042c0 <runtime::heap_alloc>:
  4042c0:	48 83 ec 18          	sub    $0x18,%rsp
  4042c4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4042c8:	40 88 f0             	mov    %sil,%al
  4042cb:	88 44 24 0e          	mov    %al,0xe(%rsp)
  4042cf:	8a 44 24 0e          	mov    0xe(%rsp),%al
  4042d3:	48 8b 3c 24          	mov    (%rsp),%rdi
  4042d7:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4042dc:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4042e0:	0f b6 f0             	movzbl %al,%esi
  4042e3:	e8 38 e4 ff ff       	call   402720 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  4042e8:	48 83 c4 18          	add    $0x18,%rsp
  4042ec:	c3                   	ret
  4042ed:	0f 1f 00             	nopl   (%rax)

00000000004042f0 <runtime::heap_resize>:
  4042f0:	48 83 ec 28          	sub    $0x28,%rsp
  4042f4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4042f9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4042fe:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404303:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  404308:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40430d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  404312:	e8 b9 e4 ff ff       	call   4027d0 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  404317:	48 83 c4 28          	add    $0x28,%rsp
  40431b:	c3                   	ret
  40431c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000404320 <runtime::heap_free>:
  404320:	48 83 ec 18          	sub    $0x18,%rsp
  404324:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404329:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40432e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  404333:	e8 18 e9 ff ff       	call   402c50 <runtime::[heap_allocator_unix.odin]::_heap_free>
  404338:	48 83 c4 18          	add    $0x18,%rsp
  40433c:	c3                   	ret
  40433d:	0f 1f 00             	nopl   (%rax)

0000000000404340 <runtime::mem_free>:
  404340:	53                   	push   %rbx
  404341:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  404348:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40434d:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  404352:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  404357:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40435c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  404361:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  404366:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40436b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  404370:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  404377:	00 
  404378:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40437d:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  404382:	48 83 f8 00          	cmp    $0x0,%rax
  404386:	0f 94 c0             	sete   %al
  404389:	24 01                	and    $0x1,%al
  40438b:	3c 00                	cmp    $0x0,%al
  40438d:	75 0f                	jne    40439e <runtime::mem_free+0x5e>
  40438f:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  404395:	0f 94 c0             	sete   %al
  404398:	24 01                	and    $0x1,%al
  40439a:	3c 00                	cmp    $0x0,%al
  40439c:	74 0b                	je     4043a9 <runtime::mem_free+0x69>
  40439e:	31 c0                	xor    %eax,%eax
  4043a0:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  4043a7:	5b                   	pop    %rbx
  4043a8:	c3                   	ret
  4043a9:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  4043ae:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  4043b3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4043b8:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4043bd:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  4043c2:	0f 57 c0             	xorps  %xmm0,%xmm0
  4043c5:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  4043ca:	be 01 00 00 00       	mov    $0x1,%esi
  4043cf:	31 c9                	xor    %ecx,%ecx
  4043d1:	41 89 c9             	mov    %ecx,%r9d
  4043d4:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  4043d9:	4c 89 ca             	mov    %r9,%rdx
  4043dc:	4c 89 c9             	mov    %r9,%rcx
  4043df:	48 89 1c 24          	mov    %rbx,(%rsp)
  4043e3:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  4043e8:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  4043ed:	ff d0                	call   *%rax
  4043ef:	88 44 24 47          	mov    %al,0x47(%rsp)
  4043f3:	8a 44 24 47          	mov    0x47(%rsp),%al
  4043f7:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  4043fe:	5b                   	pop    %rbx
  4043ff:	c3                   	ret

0000000000404400 <runtime::print_u64>:
  404400:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  404407:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40440c:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404411:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404418:	00 
  404419:	48 8d 7c 24 5f       	lea    0x5f(%rsp),%rdi
  40441e:	31 f6                	xor    %esi,%esi
  404420:	ba 81 00 00 00       	mov    $0x81,%edx
  404425:	e8 16 cc ff ff       	call   401040 <memset@plt>
  40442a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40442f:	48 c7 44 24 50 81 00 	movq   $0x81,0x50(%rsp)
  404436:	00 00 
  404438:	48 c7 44 24 48 0a 00 	movq   $0xa,0x48(%rsp)
  40443f:	00 00 
  404441:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404446:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40444b:	48 3b 44 24 48       	cmp    0x48(%rsp),%rax
  404450:	0f 93 c0             	setae  %al
  404453:	24 01                	and    $0x1,%al
  404455:	3c 00                	cmp    $0x0,%al
  404457:	74 50                	je     4044a9 <runtime::print_u64+0xa9>
  404459:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40445e:	48 83 e8 01          	sub    $0x1,%rax
  404462:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404467:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40446c:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404471:	48 c7 c0 48 b0 40 00 	mov    $0x40b048,%rax
  404478:	48 8b 08             	mov    (%rax),%rcx
  40447b:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404480:	31 d2                	xor    %edx,%edx
  404482:	48 f7 74 24 48       	divq   0x48(%rsp)
  404487:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40448c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  40448f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  404493:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  404498:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40449d:	31 d2                	xor    %edx,%edx
  40449f:	48 f7 f1             	div    %rcx
  4044a2:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4044a7:	eb 9d                	jmp    404446 <runtime::print_u64+0x46>
  4044a9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4044ae:	48 ff c8             	dec    %rax
  4044b1:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4044b6:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4044bb:	48 89 04 24          	mov    %rax,(%rsp)
  4044bf:	48 c7 c0 48 b0 40 00 	mov    $0x40b048,%rax
  4044c6:	48 8b 08             	mov    (%rax),%rcx
  4044c9:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4044ce:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4044d3:	31 d2                	xor    %edx,%edx
  4044d5:	48 f7 f6             	div    %rsi
  4044d8:	48 8b 04 24          	mov    (%rsp),%rax
  4044dc:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  4044df:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  4044e3:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4044e8:	48 8d 4c 14 5f       	lea    0x5f(%rsp,%rdx,1),%rcx
  4044ed:	b8 81 00 00 00       	mov    $0x81,%eax
  4044f2:	48 29 d0             	sub    %rdx,%rax
  4044f5:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4044fa:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4044ff:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  404504:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  404509:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  404510:	00 00 
  404512:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  404517:	e8 e4 dd ff ff       	call   402300 <runtime::stderr_write>
  40451c:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  404523:	c3                   	ret
  404524:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40452b:	00 00 00 00 00 

0000000000404530 <runtime::print_i64>:
  404530:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  404537:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40453c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  404541:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404548:	00 
  404549:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404550:	00 
  404551:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  404558:	00 00 
  40455a:	0f 9c c0             	setl   %al
  40455d:	24 01                	and    $0x1,%al
  40455f:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  404566:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40456d:	00 
  40456e:	31 c9                	xor    %ecx,%ecx
  404570:	48 29 c1             	sub    %rax,%rcx
  404573:	48 83 f8 00          	cmp    $0x0,%rax
  404577:	48 0f 4c c1          	cmovl  %rcx,%rax
  40457b:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404582:	00 
  404583:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  404588:	31 f6                	xor    %esi,%esi
  40458a:	ba 81 00 00 00       	mov    $0x81,%edx
  40458f:	e8 ac ca ff ff       	call   401040 <memset@plt>
  404594:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  40459b:	00 00 
  40459d:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  4045a4:	00 0a 
  4045a6:	0f 9d c0             	setge  %al
  4045a9:	24 01                	and    $0x1,%al
  4045ab:	3c 00                	cmp    $0x0,%al
  4045ad:	74 5c                	je     40460b <runtime::print_i64+0xdb>
  4045af:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4045b4:	48 83 e8 01          	sub    $0x1,%rax
  4045b8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4045bd:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4045c2:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4045c7:	48 c7 c0 48 b0 40 00 	mov    $0x40b048,%rax
  4045ce:	48 8b 08             	mov    (%rax),%rcx
  4045d1:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4045d8:	00 
  4045d9:	be 0a 00 00 00       	mov    $0xa,%esi
  4045de:	48 99                	cqto
  4045e0:	48 f7 fe             	idiv   %rsi
  4045e3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4045e8:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  4045eb:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  4045ef:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4045f6:	00 
  4045f7:	b9 0a 00 00 00       	mov    $0xa,%ecx
  4045fc:	48 99                	cqto
  4045fe:	48 f7 f9             	idiv   %rcx
  404601:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404608:	00 
  404609:	eb 92                	jmp    40459d <runtime::print_i64+0x6d>
  40460b:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404610:	48 83 e8 01          	sub    $0x1,%rax
  404614:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404619:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40461e:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404623:	48 c7 c0 48 b0 40 00 	mov    $0x40b048,%rax
  40462a:	48 8b 08             	mov    (%rax),%rcx
  40462d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404634:	00 
  404635:	be 0a 00 00 00       	mov    $0xa,%esi
  40463a:	48 99                	cqto
  40463c:	48 f7 fe             	idiv   %rsi
  40463f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404644:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  404647:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  40464b:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  404652:	00 
  404653:	74 18                	je     40466d <runtime::print_i64+0x13d>
  404655:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40465a:	48 83 e8 01          	sub    $0x1,%rax
  40465e:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404663:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404668:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  40466d:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  404672:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  404677:	b8 81 00 00 00       	mov    $0x81,%eax
  40467c:	48 29 d0             	sub    %rdx,%rax
  40467f:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  404684:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404689:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40468e:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  404693:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40469a:	00 00 
  40469c:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  4046a1:	e8 5a dc ff ff       	call   402300 <runtime::stderr_write>
  4046a6:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4046ad:	c3                   	ret
  4046ae:	66 90                	xchg   %ax,%ax

00000000004046b0 <runtime::arena_free_last_memory_block>:
  4046b0:	48 83 ec 28          	sub    $0x28,%rsp
  4046b4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4046b8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4046bd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4046c2:	48 8b 04 24          	mov    (%rsp),%rax
  4046c6:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4046cb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4046d0:	48 8b 40 10          	mov    0x10(%rax),%rax
  4046d4:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4046d9:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  4046df:	0f 95 c0             	setne  %al
  4046e2:	24 01                	and    $0x1,%al
  4046e4:	3c 00                	cmp    $0x0,%al
  4046e6:	74 3e                	je     404726 <runtime::arena_free_last_memory_block+0x76>
  4046e8:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4046ed:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4046f2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4046f7:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4046fc:	48 8b 09             	mov    (%rcx),%rcx
  4046ff:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404703:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404708:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40470d:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  404711:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404715:	48 29 f9             	sub    %rdi,%rcx
  404718:	48 89 48 20          	mov    %rcx,0x20(%rax)
  40471c:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  404721:	e8 aa ea ff ff       	call   4031d0 <runtime::memory_block_dealloc>
  404726:	48 83 c4 28          	add    $0x28,%rsp
  40472a:	c3                   	ret
  40472b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000404730 <runtime::print_caller_location>:
  404730:	50                   	push   %rax
  404731:	48 89 3c 24          	mov    %rdi,(%rsp)
  404735:	eb 00                	jmp    404737 <runtime::print_caller_location+0x7>
  404737:	48 8b 04 24          	mov    (%rsp),%rax
  40473b:	48 8b 38             	mov    (%rax),%rdi
  40473e:	48 8b 70 08          	mov    0x8(%rax),%rsi
  404742:	e8 89 f7 ff ff       	call   403ed0 <runtime::print_string>
  404747:	bf 28 00 00 00       	mov    $0x28,%edi
  40474c:	e8 ef f9 ff ff       	call   404140 <runtime::print_byte>
  404751:	48 8b 04 24          	mov    (%rsp),%rax
  404755:	48 63 78 10          	movslq 0x10(%rax),%rdi
  404759:	e8 a2 fc ff ff       	call   404400 <runtime::print_u64>
  40475e:	48 8b 04 24          	mov    (%rsp),%rax
  404762:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  404766:	0f 95 c0             	setne  %al
  404769:	24 01                	and    $0x1,%al
  40476b:	3c 00                	cmp    $0x0,%al
  40476d:	74 17                	je     404786 <runtime::print_caller_location+0x56>
  40476f:	bf 3a 00 00 00       	mov    $0x3a,%edi
  404774:	e8 c7 f9 ff ff       	call   404140 <runtime::print_byte>
  404779:	48 8b 04 24          	mov    (%rsp),%rax
  40477d:	48 63 78 14          	movslq 0x14(%rax),%rdi
  404781:	e8 7a fc ff ff       	call   404400 <runtime::print_u64>
  404786:	bf 29 00 00 00       	mov    $0x29,%edi
  40478b:	e8 b0 f9 ff ff       	call   404140 <runtime::print_byte>
  404790:	58                   	pop    %rax
  404791:	c3                   	ret
  404792:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404799:	1f 84 00 00 00 00 00 

00000000004047a0 <runtime::arena_free_all>:
  4047a0:	48 83 ec 28          	sub    $0x28,%rsp
  4047a4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4047a9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4047ae:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4047b3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4047b8:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4047bd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4047c2:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  4047c7:	0f 95 c0             	setne  %al
  4047ca:	24 01                	and    $0x1,%al
  4047cc:	3c 00                	cmp    $0x0,%al
  4047ce:	74 2c                	je     4047fc <runtime::arena_free_all+0x5c>
  4047d0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4047d5:	48 8b 40 10          	mov    0x10(%rax),%rax
  4047d9:	48 83 38 00          	cmpq   $0x0,(%rax)
  4047dd:	0f 95 c0             	setne  %al
  4047e0:	24 01                	and    $0x1,%al
  4047e2:	3c 00                	cmp    $0x0,%al
  4047e4:	74 16                	je     4047fc <runtime::arena_free_all+0x5c>
  4047e6:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4047eb:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4047f0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4047f5:	e8 b6 fe ff ff       	call   4046b0 <runtime::arena_free_last_memory_block>
  4047fa:	eb c1                	jmp    4047bd <runtime::arena_free_all+0x1d>
  4047fc:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404801:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404806:	0f 95 c0             	setne  %al
  404809:	24 01                	and    $0x1,%al
  40480b:	3c 00                	cmp    $0x0,%al
  40480d:	74 32                	je     404841 <runtime::arena_free_all+0xa1>
  40480f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404814:	48 8b 40 10          	mov    0x10(%rax),%rax
  404818:	48 8b 78 18          	mov    0x18(%rax),%rdi
  40481c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404821:	48 8b 40 10          	mov    0x10(%rax),%rax
  404825:	48 8b 50 20          	mov    0x20(%rax),%rdx
  404829:	31 f6                	xor    %esi,%esi
  40482b:	e8 10 c8 ff ff       	call   401040 <memset@plt>
  404830:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404835:	48 8b 40 10          	mov    0x10(%rax),%rax
  404839:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  404840:	00 
  404841:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404846:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  40484d:	00 
  40484e:	48 83 c4 28          	add    $0x28,%rsp
  404852:	c3                   	ret
  404853:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40485a:	84 00 00 00 00 00 

0000000000404860 <runtime::arena_destroy>:
  404860:	48 83 ec 28          	sub    $0x28,%rsp
  404864:	48 89 3c 24          	mov    %rdi,(%rsp)
  404868:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40486d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404872:	48 8b 04 24          	mov    (%rsp),%rax
  404876:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40487b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404880:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404885:	0f 95 c0             	setne  %al
  404888:	24 01                	and    $0x1,%al
  40488a:	3c 00                	cmp    $0x0,%al
  40488c:	74 4e                	je     4048dc <runtime::arena_destroy+0x7c>
  40488e:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  404893:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404898:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40489d:	48 8b 40 10          	mov    0x10(%rax),%rax
  4048a1:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4048a6:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4048ab:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4048b0:	48 8b 09             	mov    (%rcx),%rcx
  4048b3:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4048b7:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4048bc:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4048c1:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  4048c5:	48 8b 48 20          	mov    0x20(%rax),%rcx
  4048c9:	48 29 f9             	sub    %rdi,%rcx
  4048cc:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4048d0:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4048d5:	e8 f6 e8 ff ff       	call   4031d0 <runtime::memory_block_dealloc>
  4048da:	eb 9f                	jmp    40487b <runtime::arena_destroy+0x1b>
  4048dc:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4048e1:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  4048e8:	00 
  4048e9:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4048ee:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  4048f5:	00 
  4048f6:	48 83 c4 28          	add    $0x28,%rsp
  4048fa:	c3                   	ret
  4048fb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000404900 <runtime::arena_allocator_proc>:
  404900:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  404907:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  40490c:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  404913:	00 
  404914:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40491b:	00 
  40491c:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  404923:	00 
  404924:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  40492b:	00 
  40492c:	40 88 f0             	mov    %sil,%al
  40492f:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  404936:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  40493d:	00 
  40493e:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  404945:	00 
  404946:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  40494d:	00 
  40494e:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  404955:	00 
  404956:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  40495d:	00 
  40495e:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  404965:	00 
  404966:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  40496d:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  404972:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  404979:	00 
  40497a:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404981:	00 
  404982:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  404989:	00 
  40498a:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  404991:	00 
  404992:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  404999:	00 
  40499a:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  4049a1:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  4049a8:	00 
  4049a9:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  4049b0:	00 
  4049b1:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  4049b8:	00 
  4049b9:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  4049c0:	00 
  4049c1:	0f 57 c0             	xorps  %xmm0,%xmm0
  4049c4:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  4049cb:	00 
  4049cc:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  4049d3:	00 
  4049d4:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  4049db:	00 
  4049dc:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  4049e3:	00 
  4049e4:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  4049eb:	00 
  4049ec:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  4049f3:	00 
  4049f4:	0f b6 c8             	movzbl %al,%ecx
  4049f7:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4049fc:	2c 07                	sub    $0x7,%al
  4049fe:	0f 87 9a 07 00 00    	ja     40519e <runtime::arena_allocator_proc+0x89e>
  404a04:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  404a09:	48 8b 04 c5 50 80 40 	mov    0x408050(,%rax,8),%rax
  404a10:	00 
  404a11:	ff e0                	jmp    *%rax
  404a13:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404a1a:	00 
  404a1b:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404a22:	00 
  404a23:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404a2a:	00 
  404a2b:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404a32:	00 
  404a33:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404a3a:	00 
  404a3b:	0f 57 c0             	xorps  %xmm0,%xmm0
  404a3e:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  404a45:	00 
  404a46:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  404a4d:	00 
  404a4e:	e8 4d f0 ff ff       	call   403aa0 <runtime::arena_alloc>
  404a53:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404a5a:	00 
  404a5b:	40 88 c7             	mov    %al,%dil
  404a5e:	40 88 f8             	mov    %dil,%al
  404a61:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  404a68:	00 
  404a69:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  404a70:	00 
  404a71:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404a78:	00 
  404a79:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404a80:	00 
  404a81:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404a88:	00 
  404a89:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404a8d:	48 89 11             	mov    %rdx,(%rcx)
  404a90:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404a97:	c3                   	ret
  404a98:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404a9f:	04 
  404aa0:	e9 f9 06 00 00       	jmp    40519e <runtime::arena_allocator_proc+0x89e>
  404aa5:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  404aac:	00 
  404aad:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  404ab4:	00 
  404ab5:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404abc:	00 
  404abd:	e8 de fc ff ff       	call   4047a0 <runtime::arena_free_all>
  404ac2:	e9 d7 06 00 00       	jmp    40519e <runtime::arena_allocator_proc+0x89e>
  404ac7:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404ace:	00 
  404acf:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  404ad6:	00 
  404ad7:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  404ade:	00 00 
  404ae0:	0f 94 c1             	sete   %cl
  404ae3:	80 e1 01             	and    $0x1,%cl
  404ae6:	b0 01                	mov    $0x1,%al
  404ae8:	38 c8                	cmp    %cl,%al
  404aea:	74 25                	je     404b11 <runtime::arena_allocator_proc+0x211>
  404aec:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  404af3:	00 
  404af4:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  404afb:	00 
  404afc:	0f 94 c1             	sete   %cl
  404aff:	80 e1 01             	and    $0x1,%cl
  404b02:	b0 01                	mov    $0x1,%al
  404b04:	38 c8                	cmp    %cl,%al
  404b06:	0f 84 a8 00 00 00    	je     404bb4 <runtime::arena_allocator_proc+0x2b4>
  404b0c:	e9 85 00 00 00       	jmp    404b96 <runtime::arena_allocator_proc+0x296>
  404b11:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404b18:	00 
  404b19:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404b20:	00 
  404b21:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404b28:	00 
  404b29:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404b30:	00 
  404b31:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404b38:	00 
  404b39:	0f 57 c0             	xorps  %xmm0,%xmm0
  404b3c:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  404b43:	00 
  404b44:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  404b4b:	00 
  404b4c:	e8 4f ef ff ff       	call   403aa0 <runtime::arena_alloc>
  404b51:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404b58:	00 
  404b59:	40 88 c7             	mov    %al,%dil
  404b5c:	40 88 f8             	mov    %dil,%al
  404b5f:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  404b66:	00 
  404b67:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  404b6e:	00 
  404b6f:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404b76:	00 
  404b77:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404b7e:	00 
  404b7f:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404b86:	00 
  404b87:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404b8b:	48 89 11             	mov    %rdx,(%rcx)
  404b8e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404b95:	c3                   	ret
  404b96:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  404b9d:	00 00 
  404b9f:	0f 94 c1             	sete   %cl
  404ba2:	80 e1 01             	and    $0x1,%cl
  404ba5:	b0 01                	mov    $0x1,%al
  404ba7:	38 c8                	cmp    %cl,%al
  404ba9:	0f 84 e5 00 00 00    	je     404c94 <runtime::arena_allocator_proc+0x394>
  404baf:	e9 b7 00 00 00       	jmp    404c6b <runtime::arena_allocator_proc+0x36b>
  404bb4:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404bbb:	00 
  404bbc:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  404bc1:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404bc8:	00 
  404bc9:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  404bce:	bf a0 80 40 00       	mov    $0x4080a0,%edi
  404bd3:	31 c0                	xor    %eax,%eax
  404bd5:	41 89 c0             	mov    %eax,%r8d
  404bd8:	be 3e 00 00 00       	mov    $0x3e,%esi
  404bdd:	ba d1 00 00 00       	mov    $0xd1,%edx
  404be2:	b9 13 00 00 00       	mov    $0x13,%ecx
  404be7:	e8 54 eb ff ff       	call   403740 <runtime::multi_pointer_slice_expr_error>
  404bec:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  404bf1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  404bf6:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404bfd:	00 
  404bfe:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  404c05:	00 
  404c06:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  404c0d:	00 
  404c0e:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  404c15:	00 
  404c16:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  404c1d:	00 
  404c1e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404c25:	00 
  404c26:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404c2d:	00 
  404c2e:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404c35:	00 
  404c36:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404c3d:	00 
  404c3e:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404c45:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404c4c:	00 
  404c4d:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404c54:	00 
  404c55:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404c5c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404c60:	48 89 11             	mov    %rdx,(%rcx)
  404c63:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404c6a:	c3                   	ret
  404c6b:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404c72:	00 
  404c73:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  404c7a:	00 
  404c7b:	48 83 e9 01          	sub    $0x1,%rcx
  404c7f:	48 21 c8             	and    %rcx,%rax
  404c82:	48 83 f8 00          	cmp    $0x0,%rax
  404c86:	0f 94 c1             	sete   %cl
  404c89:	80 e1 01             	and    $0x1,%cl
  404c8c:	b0 01                	mov    $0x1,%al
  404c8e:	38 c8                	cmp    %cl,%al
  404c90:	74 54                	je     404ce6 <runtime::arena_allocator_proc+0x3e6>
  404c92:	eb 4d                	jmp    404ce1 <runtime::arena_allocator_proc+0x3e1>
  404c94:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404c9b:	00 
  404c9c:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404ca3:	04 
  404ca4:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404cab:	00 
  404cac:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404cb3:	00 
  404cb4:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404cbb:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404cc2:	00 
  404cc3:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404cca:	00 
  404ccb:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404cd2:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404cd6:	48 89 11             	mov    %rdx,(%rcx)
  404cd9:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404ce0:	c3                   	ret
  404ce1:	e9 94 02 00 00       	jmp    404f7a <runtime::arena_allocator_proc+0x67a>
  404ce6:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  404ced:	00 
  404cee:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  404cf5:	00 
  404cf6:	0f 92 c0             	setb   %al
  404cf9:	24 01                	and    $0x1,%al
  404cfb:	3c 00                	cmp    $0x0,%al
  404cfd:	0f 84 b7 00 00 00    	je     404dba <runtime::arena_allocator_proc+0x4ba>
  404d03:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404d0a:	00 
  404d0b:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404d10:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404d17:	00 
  404d18:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  404d1d:	bf a0 80 40 00       	mov    $0x4080a0,%edi
  404d22:	31 c0                	xor    %eax,%eax
  404d24:	41 89 c0             	mov    %eax,%r8d
  404d27:	be 3e 00 00 00       	mov    $0x3e,%esi
  404d2c:	ba d9 00 00 00       	mov    $0xd9,%edx
  404d31:	b9 14 00 00 00       	mov    $0x14,%ecx
  404d36:	e8 05 ea ff ff       	call   403740 <runtime::multi_pointer_slice_expr_error>
  404d3b:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  404d40:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  404d45:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404d4c:	00 
  404d4d:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  404d54:	00 
  404d55:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  404d5c:	00 
  404d5d:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  404d64:	00 
  404d65:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  404d6c:	00 
  404d6d:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404d74:	00 
  404d75:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404d7c:	00 
  404d7d:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404d84:	00 
  404d85:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404d8c:	00 
  404d8d:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404d94:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404d9b:	00 
  404d9c:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404da3:	00 
  404da4:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404dab:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404daf:	48 89 11             	mov    %rdx,(%rcx)
  404db2:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404db9:	c3                   	ret
  404dba:	eb 00                	jmp    404dbc <runtime::arena_allocator_proc+0x4bc>
  404dbc:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  404dc3:	00 
  404dc4:	48 8b 40 10          	mov    0x10(%rax),%rax
  404dc8:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  404dcf:	00 
  404dd0:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  404dd7:	00 00 
  404dd9:	0f 95 c0             	setne  %al
  404ddc:	24 01                	and    $0x1,%al
  404dde:	3c 00                	cmp    $0x0,%al
  404de0:	0f 84 92 01 00 00    	je     404f78 <runtime::arena_allocator_proc+0x678>
  404de6:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404ded:	00 
  404dee:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404df5:	00 
  404df6:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  404dfa:	48 29 c8             	sub    %rcx,%rax
  404dfd:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  404e04:	00 
  404e05:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404e0c:	00 
  404e0d:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  404e14:	00 
  404e15:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  404e1c:	00 
  404e1d:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404e24:	00 
  404e25:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  404e2c:	00 
  404e2d:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  404e34:	00 
  404e35:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404e3c:	00 
  404e3d:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  404e44:	00 
  404e45:	0f 92 c0             	setb   %al
  404e48:	24 01                	and    $0x1,%al
  404e4a:	3c 00                	cmp    $0x0,%al
  404e4c:	0f 84 24 01 00 00    	je     404f76 <runtime::arena_allocator_proc+0x676>
  404e52:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  404e59:	00 
  404e5a:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404e61:	00 
  404e62:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  404e66:	0f 94 c0             	sete   %al
  404e69:	24 01                	and    $0x1,%al
  404e6b:	3c 00                	cmp    $0x0,%al
  404e6d:	0f 84 03 01 00 00    	je     404f76 <runtime::arena_allocator_proc+0x676>
  404e73:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  404e7a:	00 
  404e7b:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404e82:	00 
  404e83:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  404e87:	0f 96 c0             	setbe  %al
  404e8a:	24 01                	and    $0x1,%al
  404e8c:	3c 00                	cmp    $0x0,%al
  404e8e:	0f 84 e2 00 00 00    	je     404f76 <runtime::arena_allocator_proc+0x676>
  404e94:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404e9b:	00 
  404e9c:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  404ea3:	00 
  404ea4:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404ea8:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404eaf:	00 
  404eb0:	48 8b 40 18          	mov    0x18(%rax),%rax
  404eb4:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404eb9:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  404ec0:	00 
  404ec1:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  404ec6:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  404ecd:	00 
  404ece:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  404ed3:	bf a0 80 40 00       	mov    $0x4080a0,%edi
  404ed8:	be 3e 00 00 00       	mov    $0x3e,%esi
  404edd:	ba e4 00 00 00       	mov    $0xe4,%edx
  404ee2:	b9 17 00 00 00       	mov    $0x17,%ecx
  404ee7:	e8 54 e8 ff ff       	call   403740 <runtime::multi_pointer_slice_expr_error>
  404eec:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404ef1:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404ef6:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404efb:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404f02:	00 
  404f03:	48 01 f2             	add    %rsi,%rdx
  404f06:	48 29 f0             	sub    %rsi,%rax
  404f09:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  404f10:	00 
  404f11:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  404f18:	00 
  404f19:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  404f20:	00 
  404f21:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  404f28:	00 
  404f29:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404f30:	00 
  404f31:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404f38:	00 
  404f39:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404f40:	00 
  404f41:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404f48:	00 
  404f49:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404f50:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404f57:	00 
  404f58:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404f5f:	00 
  404f60:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404f67:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404f6b:	48 89 11             	mov    %rdx,(%rcx)
  404f6e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404f75:	c3                   	ret
  404f76:	eb 00                	jmp    404f78 <runtime::arena_allocator_proc+0x678>
  404f78:	eb 00                	jmp    404f7a <runtime::arena_allocator_proc+0x67a>
  404f7a:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404f81:	00 
  404f82:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404f89:	00 
  404f8a:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404f91:	00 
  404f92:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404f99:	00 
  404f9a:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404fa1:	00 
  404fa2:	0f 57 c0             	xorps  %xmm0,%xmm0
  404fa5:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  404fac:	00 
  404fad:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  404fb4:	00 
  404fb5:	e8 e6 ea ff ff       	call   403aa0 <runtime::arena_alloc>
  404fba:	88 44 24 27          	mov    %al,0x27(%rsp)
  404fbe:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  404fc5:	00 
  404fc6:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  404fcb:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  404fd2:	00 
  404fd3:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404fd8:	3c 00                	cmp    $0x0,%al
  404fda:	74 50                	je     40502c <runtime::arena_allocator_proc+0x72c>
  404fdc:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404fe3:	00 
  404fe4:	8a 44 24 27          	mov    0x27(%rsp),%al
  404fe8:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404fef:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404ff6:	00 
  404ff7:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404ffe:	00 
  404fff:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  405006:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40500d:	00 
  40500e:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  405015:	00 
  405016:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40501d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405021:	48 89 11             	mov    %rdx,(%rcx)
  405024:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40502b:	c3                   	ret
  40502c:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  405031:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  405036:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  40503d:	00 
  40503e:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  405045:	00 
  405046:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  40504d:	00 00 
  40504f:	0f 94 c0             	sete   %al
  405052:	24 01                	and    $0x1,%al
  405054:	3c 00                	cmp    $0x0,%al
  405056:	74 45                	je     40509d <runtime::arena_allocator_proc+0x79d>
  405058:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40505f:	00 
  405060:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  405067:	00 
  405068:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40506f:	00 
  405070:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  405077:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40507e:	00 
  40507f:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  405086:	00 
  405087:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40508e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405092:	48 89 11             	mov    %rdx,(%rcx)
  405095:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40509c:	c3                   	ret
  40509d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4050a4:	00 
  4050a5:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4050aa:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4050b1:	00 
  4050b2:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4050b7:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4050be:	00 
  4050bf:	48 89 04 24          	mov    %rax,(%rsp)
  4050c3:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  4050ca:	00 
  4050cb:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4050d0:	bf a0 80 40 00       	mov    $0x4080a0,%edi
  4050d5:	31 c0                	xor    %eax,%eax
  4050d7:	41 89 c0             	mov    %eax,%r8d
  4050da:	be 3e 00 00 00       	mov    $0x3e,%esi
  4050df:	ba ee 00 00 00       	mov    $0xee,%edx
  4050e4:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  4050e9:	e8 52 e6 ff ff       	call   403740 <runtime::multi_pointer_slice_expr_error>
  4050ee:	48 8b 0c 24          	mov    (%rsp),%rcx
  4050f2:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4050f7:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4050fc:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405101:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  405108:	00 
  405109:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  405110:	00 
  405111:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  405118:	00 
  405119:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  405120:	00 
  405121:	e8 8a ef ff ff       	call   4040b0 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  405126:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40512d:	00 
  40512e:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  405135:	00 
  405136:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40513d:	00 
  40513e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  405145:	00 
  405146:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  40514d:	00 
  40514e:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  405155:	00 
  405156:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40515a:	48 89 08             	mov    %rcx,(%rax)
  40515d:	31 c0                	xor    %eax,%eax
  40515f:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  405166:	c3                   	ret
  405167:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40516e:	00 
  40516f:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  405176:	00 
  405177:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  40517e:	00 00 
  405180:	0f 95 c0             	setne  %al
  405183:	24 01                	and    $0x1,%al
  405185:	3c 00                	cmp    $0x0,%al
  405187:	74 0b                	je     405194 <runtime::arena_allocator_proc+0x894>
  405189:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  405190:	00 
  405191:	c6 00 5d             	movb   $0x5d,(%rax)
  405194:	eb 08                	jmp    40519e <runtime::arena_allocator_proc+0x89e>
  405196:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  40519d:	04 
  40519e:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4051a5:	00 
  4051a6:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4051ad:	00 
  4051ae:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4051b5:	00 
  4051b6:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4051bd:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4051c4:	00 
  4051c5:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4051cc:	00 
  4051cd:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4051d4:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4051d8:	48 89 11             	mov    %rdx,(%rcx)
  4051db:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4051e2:	c3                   	ret
  4051e3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4051ea:	84 00 00 00 00 00 

00000000004051f0 <runtime::memory_equal>:
  4051f0:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  4051f5:	48 89 74 24 b8       	mov    %rsi,-0x48(%rsp)
  4051fa:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  4051ff:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  405204:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405209:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  40520e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  405213:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405218:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40521d:	48 83 f8 00          	cmp    $0x0,%rax
  405221:	0f 94 c1             	sete   %cl
  405224:	80 e1 01             	and    $0x1,%cl
  405227:	b0 01                	mov    $0x1,%al
  405229:	38 c8                	cmp    %cl,%al
  40522b:	74 1b                	je     405248 <runtime::memory_equal+0x58>
  40522d:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405232:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405237:	48 39 c8             	cmp    %rcx,%rax
  40523a:	0f 94 c1             	sete   %cl
  40523d:	80 e1 01             	and    $0x1,%cl
  405240:	b0 01                	mov    $0x1,%al
  405242:	38 c8                	cmp    %cl,%al
  405244:	74 07                	je     40524d <runtime::memory_equal+0x5d>
  405246:	eb 03                	jmp    40524b <runtime::memory_equal+0x5b>
  405248:	b0 01                	mov    $0x1,%al
  40524a:	c3                   	ret
  40524b:	eb 03                	jmp    405250 <runtime::memory_equal+0x60>
  40524d:	b0 01                	mov    $0x1,%al
  40524f:	c3                   	ret
  405250:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  405255:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40525a:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  40525f:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  405264:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405269:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40526e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  405275:	00 00 
  405277:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40527c:	48 3b 44 24 d0       	cmp    -0x30(%rsp),%rax
  405281:	0f 92 c0             	setb   %al
  405284:	24 01                	and    $0x1,%al
  405286:	3c 00                	cmp    $0x0,%al
  405288:	74 38                	je     4052c2 <runtime::memory_equal+0xd2>
  40528a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40528f:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  405294:	8a 04 08             	mov    (%rax,%rcx,1),%al
  405297:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40529c:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4052a1:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  4052a4:	0f 95 c0             	setne  %al
  4052a7:	24 01                	and    $0x1,%al
  4052a9:	3c 00                	cmp    $0x0,%al
  4052ab:	74 03                	je     4052b0 <runtime::memory_equal+0xc0>
  4052ad:	31 c0                	xor    %eax,%eax
  4052af:	c3                   	ret
  4052b0:	eb 00                	jmp    4052b2 <runtime::memory_equal+0xc2>
  4052b2:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4052b7:	48 83 c0 01          	add    $0x1,%rax
  4052bb:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4052c0:	eb b5                	jmp    405277 <runtime::memory_equal+0x87>
  4052c2:	b0 01                	mov    $0x1,%al
  4052c4:	c3                   	ret
  4052c5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4052cc:	00 00 00 00 

00000000004052d0 <runtime::memory_compare>:
  4052d0:	48 83 ec 10          	sub    $0x10,%rsp
  4052d4:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  4052d9:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  4052de:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  4052e3:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  4052e8:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  4052ed:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  4052f2:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4052f7:	48 89 0c 24          	mov    %rcx,(%rsp)
  4052fb:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  405300:	48 39 c8             	cmp    %rcx,%rax
  405303:	0f 94 c1             	sete   %cl
  405306:	80 e1 01             	and    $0x1,%cl
  405309:	b0 01                	mov    $0x1,%al
  40530b:	38 c8                	cmp    %cl,%al
  40530d:	74 17                	je     405326 <runtime::memory_compare+0x56>
  40530f:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  405314:	48 83 f8 00          	cmp    $0x0,%rax
  405318:	0f 94 c1             	sete   %cl
  40531b:	80 e1 01             	and    $0x1,%cl
  40531e:	b0 01                	mov    $0x1,%al
  405320:	38 c8                	cmp    %cl,%al
  405322:	74 20                	je     405344 <runtime::memory_compare+0x74>
  405324:	eb 07                	jmp    40532d <runtime::memory_compare+0x5d>
  405326:	31 c0                	xor    %eax,%eax
  405328:	48 83 c4 10          	add    $0x10,%rsp
  40532c:	c3                   	ret
  40532d:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  405332:	48 83 f8 00          	cmp    $0x0,%rax
  405336:	0f 94 c1             	sete   %cl
  405339:	80 e1 01             	and    $0x1,%cl
  40533c:	b0 01                	mov    $0x1,%al
  40533e:	38 c8                	cmp    %cl,%al
  405340:	74 10                	je     405352 <runtime::memory_compare+0x82>
  405342:	eb 0c                	jmp    405350 <runtime::memory_compare+0x80>
  405344:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40534b:	48 83 c4 10          	add    $0x10,%rsp
  40534f:	c3                   	ret
  405350:	eb 0a                	jmp    40535c <runtime::memory_compare+0x8c>
  405352:	b8 01 00 00 00       	mov    $0x1,%eax
  405357:	48 83 c4 10          	add    $0x10,%rsp
  40535b:	c3                   	ret
  40535c:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405361:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  405366:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  40536b:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  405370:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405375:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40537a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40537f:	48 c1 e8 03          	shr    $0x3,%rax
  405383:	48 83 c0 01          	add    $0x1,%rax
  405387:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40538c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405391:	48 83 e8 01          	sub    $0x1,%rax
  405395:	48 c1 e0 03          	shl    $0x3,%rax
  405399:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40539e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  4053a5:	00 00 
  4053a7:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  4053ad:	0f 92 c0             	setb   %al
  4053b0:	24 01                	and    $0x1,%al
  4053b2:	3c 00                	cmp    $0x0,%al
  4053b4:	74 09                	je     4053bf <runtime::memory_compare+0xef>
  4053b6:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4053bd:	00 00 
  4053bf:	eb 00                	jmp    4053c1 <runtime::memory_compare+0xf1>
  4053c1:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4053c6:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  4053cb:	0f 92 c0             	setb   %al
  4053ce:	24 01                	and    $0x1,%al
  4053d0:	3c 00                	cmp    $0x0,%al
  4053d2:	0f 84 11 01 00 00    	je     4054e9 <runtime::memory_compare+0x219>
  4053d8:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4053dd:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4053e2:	48 c1 e1 03          	shl    $0x3,%rcx
  4053e6:	48 01 c8             	add    %rcx,%rax
  4053e9:	48 8b 00             	mov    (%rax),%rax
  4053ec:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4053f1:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4053f6:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4053fb:	48 c1 e1 03          	shl    $0x3,%rcx
  4053ff:	48 01 c8             	add    %rcx,%rax
  405402:	48 8b 00             	mov    (%rax),%rax
  405405:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40540a:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40540f:	48 33 44 24 b8       	xor    -0x48(%rsp),%rax
  405414:	48 83 f8 00          	cmp    $0x0,%rax
  405418:	0f 95 c0             	setne  %al
  40541b:	24 01                	and    $0x1,%al
  40541d:	3c 00                	cmp    $0x0,%al
  40541f:	0f 84 af 00 00 00    	je     4054d4 <runtime::memory_compare+0x204>
  405425:	eb 00                	jmp    405427 <runtime::memory_compare+0x157>
  405427:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40542c:	48 c1 e0 03          	shl    $0x3,%rax
  405430:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  405435:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40543a:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  40543f:	0f 92 c0             	setb   %al
  405442:	24 01                	and    $0x1,%al
  405444:	3c 00                	cmp    $0x0,%al
  405446:	0f 84 86 00 00 00    	je     4054d2 <runtime::memory_compare+0x202>
  40544c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405451:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  405456:	8a 00                	mov    (%rax),%al
  405458:	88 44 24 af          	mov    %al,-0x51(%rsp)
  40545c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405461:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  405466:	8a 00                	mov    (%rax),%al
  405468:	88 44 24 ae          	mov    %al,-0x52(%rsp)
  40546c:	8a 44 24 af          	mov    -0x51(%rsp),%al
  405470:	32 44 24 ae          	xor    -0x52(%rsp),%al
  405474:	3c 00                	cmp    $0x0,%al
  405476:	0f 95 c0             	setne  %al
  405479:	24 01                	and    $0x1,%al
  40547b:	3c 00                	cmp    $0x0,%al
  40547d:	74 3e                	je     4054bd <runtime::memory_compare+0x1ed>
  40547f:	0f b6 44 24 af       	movzbl -0x51(%rsp),%eax
  405484:	0f b6 4c 24 ae       	movzbl -0x52(%rsp),%ecx
  405489:	48 29 c8             	sub    %rcx,%rax
  40548c:	48 83 f8 00          	cmp    $0x0,%rax
  405490:	0f 9c c0             	setl   %al
  405493:	24 01                	and    $0x1,%al
  405495:	3c 00                	cmp    $0x0,%al
  405497:	74 0e                	je     4054a7 <runtime::memory_compare+0x1d7>
  405499:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4054a0:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  4054a5:	eb 0c                	jmp    4054b3 <runtime::memory_compare+0x1e3>
  4054a7:	b8 01 00 00 00       	mov    $0x1,%eax
  4054ac:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  4054b1:	eb 00                	jmp    4054b3 <runtime::memory_compare+0x1e3>
  4054b3:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  4054b8:	48 83 c4 10          	add    $0x10,%rsp
  4054bc:	c3                   	ret
  4054bd:	eb 00                	jmp    4054bf <runtime::memory_compare+0x1ef>
  4054bf:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4054c4:	48 83 c0 01          	add    $0x1,%rax
  4054c8:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4054cd:	e9 63 ff ff ff       	jmp    405435 <runtime::memory_compare+0x165>
  4054d2:	eb 00                	jmp    4054d4 <runtime::memory_compare+0x204>
  4054d4:	eb 00                	jmp    4054d6 <runtime::memory_compare+0x206>
  4054d6:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4054db:	48 83 c0 01          	add    $0x1,%rax
  4054df:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4054e4:	e9 d8 fe ff ff       	jmp    4053c1 <runtime::memory_compare+0xf1>
  4054e9:	eb 00                	jmp    4054eb <runtime::memory_compare+0x21b>
  4054eb:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4054f0:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4054f5:	0f 92 c0             	setb   %al
  4054f8:	24 01                	and    $0x1,%al
  4054fa:	3c 00                	cmp    $0x0,%al
  4054fc:	0f 84 86 00 00 00    	je     405588 <runtime::memory_compare+0x2b8>
  405502:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405507:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  40550c:	8a 00                	mov    (%rax),%al
  40550e:	88 44 24 ad          	mov    %al,-0x53(%rsp)
  405512:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405517:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  40551c:	8a 00                	mov    (%rax),%al
  40551e:	88 44 24 ac          	mov    %al,-0x54(%rsp)
  405522:	8a 44 24 ad          	mov    -0x53(%rsp),%al
  405526:	32 44 24 ac          	xor    -0x54(%rsp),%al
  40552a:	3c 00                	cmp    $0x0,%al
  40552c:	0f 95 c0             	setne  %al
  40552f:	24 01                	and    $0x1,%al
  405531:	3c 00                	cmp    $0x0,%al
  405533:	74 3e                	je     405573 <runtime::memory_compare+0x2a3>
  405535:	0f b6 44 24 ad       	movzbl -0x53(%rsp),%eax
  40553a:	0f b6 4c 24 ac       	movzbl -0x54(%rsp),%ecx
  40553f:	48 29 c8             	sub    %rcx,%rax
  405542:	48 83 f8 00          	cmp    $0x0,%rax
  405546:	0f 9c c0             	setl   %al
  405549:	24 01                	and    $0x1,%al
  40554b:	3c 00                	cmp    $0x0,%al
  40554d:	74 0e                	je     40555d <runtime::memory_compare+0x28d>
  40554f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405556:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40555b:	eb 0c                	jmp    405569 <runtime::memory_compare+0x299>
  40555d:	b8 01 00 00 00       	mov    $0x1,%eax
  405562:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  405567:	eb 00                	jmp    405569 <runtime::memory_compare+0x299>
  405569:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  40556e:	48 83 c4 10          	add    $0x10,%rsp
  405572:	c3                   	ret
  405573:	eb 00                	jmp    405575 <runtime::memory_compare+0x2a5>
  405575:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40557a:	48 83 c0 01          	add    $0x1,%rax
  40557e:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405583:	e9 63 ff ff ff       	jmp    4054eb <runtime::memory_compare+0x21b>
  405588:	31 c0                	xor    %eax,%eax
  40558a:	48 83 c4 10          	add    $0x10,%rsp
  40558e:	c3                   	ret
  40558f:	90                   	nop

0000000000405590 <runtime::memory_compare_zero>:
  405590:	48 89 7c 24 a0       	mov    %rdi,-0x60(%rsp)
  405595:	48 89 74 24 a8       	mov    %rsi,-0x58(%rsp)
  40559a:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40559f:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  4055a4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4055a9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4055ae:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4055b3:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4055b8:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4055bd:	48 c1 e8 03          	shr    $0x3,%rax
  4055c1:	48 83 c0 01          	add    $0x1,%rax
  4055c5:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4055ca:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4055cf:	48 83 e8 01          	sub    $0x1,%rax
  4055d3:	48 c1 e0 03          	shl    $0x3,%rax
  4055d7:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4055dc:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  4055e3:	00 00 
  4055e5:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  4055eb:	0f 92 c0             	setb   %al
  4055ee:	24 01                	and    $0x1,%al
  4055f0:	3c 00                	cmp    $0x0,%al
  4055f2:	74 09                	je     4055fd <runtime::memory_compare_zero+0x6d>
  4055f4:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4055fb:	00 00 
  4055fd:	eb 00                	jmp    4055ff <runtime::memory_compare_zero+0x6f>
  4055ff:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405604:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  405609:	0f 92 c0             	setb   %al
  40560c:	24 01                	and    $0x1,%al
  40560e:	3c 00                	cmp    $0x0,%al
  405610:	0f 84 d2 00 00 00    	je     4056e8 <runtime::memory_compare_zero+0x158>
  405616:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40561b:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  405620:	48 c1 e1 03          	shl    $0x3,%rcx
  405624:	48 01 c8             	add    %rcx,%rax
  405627:	48 8b 00             	mov    (%rax),%rax
  40562a:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  40562f:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  405634:	48 83 f0 00          	xor    $0x0,%rax
  405638:	48 83 f8 00          	cmp    $0x0,%rax
  40563c:	0f 95 c0             	setne  %al
  40563f:	24 01                	and    $0x1,%al
  405641:	3c 00                	cmp    $0x0,%al
  405643:	0f 84 8a 00 00 00    	je     4056d3 <runtime::memory_compare_zero+0x143>
  405649:	eb 00                	jmp    40564b <runtime::memory_compare_zero+0xbb>
  40564b:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405650:	48 c1 e0 03          	shl    $0x3,%rax
  405654:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405659:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40565e:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  405663:	0f 92 c0             	setb   %al
  405666:	24 01                	and    $0x1,%al
  405668:	3c 00                	cmp    $0x0,%al
  40566a:	74 65                	je     4056d1 <runtime::memory_compare_zero+0x141>
  40566c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405671:	48 03 44 24 b8       	add    -0x48(%rsp),%rax
  405676:	8a 00                	mov    (%rax),%al
  405678:	88 44 24 b7          	mov    %al,-0x49(%rsp)
  40567c:	8a 44 24 b7          	mov    -0x49(%rsp),%al
  405680:	34 00                	xor    $0x0,%al
  405682:	3c 00                	cmp    $0x0,%al
  405684:	0f 95 c0             	setne  %al
  405687:	24 01                	and    $0x1,%al
  405689:	3c 00                	cmp    $0x0,%al
  40568b:	74 32                	je     4056bf <runtime::memory_compare_zero+0x12f>
  40568d:	0f b6 44 24 b7       	movzbl -0x49(%rsp),%eax
  405692:	48 83 f8 00          	cmp    $0x0,%rax
  405696:	0f 9c c0             	setl   %al
  405699:	24 01                	and    $0x1,%al
  40569b:	3c 00                	cmp    $0x0,%al
  40569d:	74 0e                	je     4056ad <runtime::memory_compare_zero+0x11d>
  40569f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4056a6:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  4056ab:	eb 0c                	jmp    4056b9 <runtime::memory_compare_zero+0x129>
  4056ad:	b8 01 00 00 00       	mov    $0x1,%eax
  4056b2:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  4056b7:	eb 00                	jmp    4056b9 <runtime::memory_compare_zero+0x129>
  4056b9:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  4056be:	c3                   	ret
  4056bf:	eb 00                	jmp    4056c1 <runtime::memory_compare_zero+0x131>
  4056c1:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4056c6:	48 83 c0 01          	add    $0x1,%rax
  4056ca:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  4056cf:	eb 88                	jmp    405659 <runtime::memory_compare_zero+0xc9>
  4056d1:	eb 00                	jmp    4056d3 <runtime::memory_compare_zero+0x143>
  4056d3:	eb 00                	jmp    4056d5 <runtime::memory_compare_zero+0x145>
  4056d5:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4056da:	48 83 c0 01          	add    $0x1,%rax
  4056de:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4056e3:	e9 17 ff ff ff       	jmp    4055ff <runtime::memory_compare_zero+0x6f>
  4056e8:	eb 00                	jmp    4056ea <runtime::memory_compare_zero+0x15a>
  4056ea:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4056ef:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4056f4:	0f 92 c0             	setb   %al
  4056f7:	24 01                	and    $0x1,%al
  4056f9:	3c 00                	cmp    $0x0,%al
  4056fb:	74 65                	je     405762 <runtime::memory_compare_zero+0x1d2>
  4056fd:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405702:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  405707:	8a 00                	mov    (%rax),%al
  405709:	88 44 24 b6          	mov    %al,-0x4a(%rsp)
  40570d:	8a 44 24 b6          	mov    -0x4a(%rsp),%al
  405711:	34 00                	xor    $0x0,%al
  405713:	3c 00                	cmp    $0x0,%al
  405715:	0f 95 c0             	setne  %al
  405718:	24 01                	and    $0x1,%al
  40571a:	3c 00                	cmp    $0x0,%al
  40571c:	74 32                	je     405750 <runtime::memory_compare_zero+0x1c0>
  40571e:	0f b6 44 24 b6       	movzbl -0x4a(%rsp),%eax
  405723:	48 83 f8 00          	cmp    $0x0,%rax
  405727:	0f 9c c0             	setl   %al
  40572a:	24 01                	and    $0x1,%al
  40572c:	3c 00                	cmp    $0x0,%al
  40572e:	74 0e                	je     40573e <runtime::memory_compare_zero+0x1ae>
  405730:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405737:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40573c:	eb 0c                	jmp    40574a <runtime::memory_compare_zero+0x1ba>
  40573e:	b8 01 00 00 00       	mov    $0x1,%eax
  405743:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405748:	eb 00                	jmp    40574a <runtime::memory_compare_zero+0x1ba>
  40574a:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  40574f:	c3                   	ret
  405750:	eb 00                	jmp    405752 <runtime::memory_compare_zero+0x1c2>
  405752:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405757:	48 83 c0 01          	add    $0x1,%rax
  40575b:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405760:	eb 88                	jmp    4056ea <runtime::memory_compare_zero+0x15a>
  405762:	31 c0                	xor    %eax,%eax
  405764:	c3                   	ret
  405765:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40576c:	00 00 00 00 

0000000000405770 <runtime::__type_info_of>:
  405770:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  405775:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40577a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40577f:	48 c7 c1 38 83 40 00 	mov    $0x408338,%rcx
  405786:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  40578a:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40578f:	31 c9                	xor    %ecx,%ecx
  405791:	89 ca                	mov    %ecx,%edx
  405793:	48 f7 74 24 f0       	divq   -0x10(%rsp)
  405798:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  40579d:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  4057a4:	00 00 
  4057a6:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4057ad:	00 00 
  4057af:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4057b4:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  4057b9:	0f 83 9f 00 00 00    	jae    40585e <runtime::__type_info_of+0xee>
  4057bf:	48 c7 c0 38 83 40 00 	mov    $0x408338,%rax
  4057c6:	48 8b 00             	mov    (%rax),%rax
  4057c9:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4057ce:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  4057d2:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4057d7:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  4057dd:	0f 95 c0             	setne  %al
  4057e0:	24 01                	and    $0x1,%al
  4057e2:	3c 00                	cmp    $0x0,%al
  4057e4:	74 1d                	je     405803 <runtime::__type_info_of+0x93>
  4057e6:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4057eb:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4057f0:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  4057f4:	0f 94 c0             	sete   %al
  4057f7:	24 01                	and    $0x1,%al
  4057f9:	3c 00                	cmp    $0x0,%al
  4057fb:	74 06                	je     405803 <runtime::__type_info_of+0x93>
  4057fd:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405802:	c3                   	ret
  405803:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405808:	48 83 c0 01          	add    $0x1,%rax
  40580c:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  405811:	0f 92 c0             	setb   %al
  405814:	24 01                	and    $0x1,%al
  405816:	3c 00                	cmp    $0x0,%al
  405818:	74 10                	je     40582a <runtime::__type_info_of+0xba>
  40581a:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40581f:	48 83 c0 01          	add    $0x1,%rax
  405823:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  405828:	eb 09                	jmp    405833 <runtime::__type_info_of+0xc3>
  40582a:	31 c0                	xor    %eax,%eax
  40582c:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  405831:	eb 00                	jmp    405833 <runtime::__type_info_of+0xc3>
  405833:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  405838:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40583d:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405842:	48 83 c0 01          	add    $0x1,%rax
  405846:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40584b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405850:	48 83 c0 01          	add    $0x1,%rax
  405854:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  405859:	e9 51 ff ff ff       	jmp    4057af <runtime::__type_info_of+0x3f>
  40585e:	48 c7 c0 38 83 40 00 	mov    $0x408338,%rax
  405865:	48 8b 00             	mov    (%rax),%rax
  405868:	48 8b 00             	mov    (%rax),%rax
  40586b:	c3                   	ret
  40586c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405870 <runtime::default_logger_proc>:
  405870:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405875:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40587a:	66 44 89 c0          	mov    %r8w,%ax
  40587e:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  405883:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  405888:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40588d:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  405892:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  405897:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40589c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  4058a1:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  4058a6:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  4058ab:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  4058b0:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  4058b5:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  4058ba:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  4058bf:	c3                   	ret

00000000004058c0 <runtime::default_context>:
  4058c0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4058c7:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4058cc:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4058d1:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  4058d6:	31 f6                	xor    %esi,%esi
  4058d8:	ba 70 00 00 00       	mov    $0x70,%edx
  4058dd:	e8 5e b7 ff ff       	call   401040 <memset@plt>
  4058e2:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  4058e7:	e8 24 00 00 00       	call   405910 <runtime::[core.odin]::__init_context>
  4058ec:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4058f1:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  4058f6:	ba 70 00 00 00       	mov    $0x70,%edx
  4058fb:	e8 60 b7 ff ff       	call   401060 <memcpy@plt>
  405900:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405905:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40590c:	c3                   	ret
  40590d:	0f 1f 00             	nopl   (%rax)

0000000000405910 <runtime::[core.odin]::__init_context>:
  405910:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  405915:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40591a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40591f:	48 83 f8 00          	cmp    $0x0,%rax
  405923:	0f 94 c0             	sete   %al
  405926:	24 01                	and    $0x1,%al
  405928:	3c 00                	cmp    $0x0,%al
  40592a:	74 01                	je     40592d <runtime::[core.odin]::__init_context+0x1d>
  40592c:	c3                   	ret
  40592d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405932:	48 c7 c1 d0 23 40 00 	mov    $0x4023d0,%rcx
  405939:	48 89 08             	mov    %rcx,(%rax)
  40593c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405941:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405948:	00 
  405949:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40594e:	48 c7 c1 b0 2f 40 00 	mov    $0x402fb0,%rcx
  405955:	48 89 48 10          	mov    %rcx,0x10(%rax)
  405959:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40595e:	48 c7 c2 c0 ff ff ff 	mov    $0xffffffffffffffc0,%rdx
  405965:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  40596c:	00 00 
  40596e:	48 01 d1             	add    %rdx,%rcx
  405971:	48 89 48 18          	mov    %rcx,0x18(%rax)
  405975:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40597a:	48 c7 c1 c0 59 40 00 	mov    $0x4059c0,%rcx
  405981:	48 89 48 20          	mov    %rcx,0x20(%rax)
  405985:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40598a:	48 c7 c1 70 58 40 00 	mov    $0x405870,%rcx
  405991:	48 89 48 28          	mov    %rcx,0x28(%rax)
  405995:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40599a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  4059a1:	00 
  4059a2:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4059a7:	48 c7 c1 70 2c 40 00 	mov    $0x402c70,%rcx
  4059ae:	48 89 48 48          	mov    %rcx,0x48(%rax)
  4059b2:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4059b7:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  4059be:	00 
  4059bf:	c3                   	ret

00000000004059c0 <runtime::default_assertion_failure_proc>:
  4059c0:	48 83 ec 48          	sub    $0x48,%rsp
  4059c4:	4c 89 04 24          	mov    %r8,(%rsp)
  4059c8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4059cd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4059d2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4059d7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4059dc:	4c 8b 04 24          	mov    (%rsp),%r8
  4059e0:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4059e5:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4059ea:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4059ef:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4059f4:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4059f9:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4059fe:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405a03:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  405a08:	e8 03 00 00 00       	call   405a10 <runtime::default_assertion_contextless_failure_proc>
  405a0d:	0f 1f 00             	nopl   (%rax)

0000000000405a10 <runtime::default_assertion_contextless_failure_proc>:
  405a10:	48 83 ec 48          	sub    $0x48,%rsp
  405a14:	4c 89 04 24          	mov    %r8,(%rsp)
  405a18:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405a1d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405a22:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405a27:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  405a2c:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405a31:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  405a36:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405a3b:	48 8b 3c 24          	mov    (%rsp),%rdi
  405a3f:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405a44:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  405a49:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  405a4e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405a53:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  405a58:	e8 d3 ec ff ff       	call   404730 <runtime::print_caller_location>
  405a5d:	bf 92 82 40 00       	mov    $0x408292,%edi
  405a62:	be 01 00 00 00       	mov    $0x1,%esi
  405a67:	e8 64 e4 ff ff       	call   403ed0 <runtime::print_string>
  405a6c:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405a71:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405a76:	e8 55 e4 ff ff       	call   403ed0 <runtime::print_string>
  405a7b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405a80:	48 83 f8 00          	cmp    $0x0,%rax
  405a84:	0f 9f c0             	setg   %al
  405a87:	24 01                	and    $0x1,%al
  405a89:	3c 00                	cmp    $0x0,%al
  405a8b:	74 1e                	je     405aab <runtime::default_assertion_contextless_failure_proc+0x9b>
  405a8d:	bf 94 82 40 00       	mov    $0x408294,%edi
  405a92:	be 02 00 00 00       	mov    $0x2,%esi
  405a97:	e8 34 e4 ff ff       	call   403ed0 <runtime::print_string>
  405a9c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  405aa1:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  405aa6:	e8 25 e4 ff ff       	call   403ed0 <runtime::print_string>
  405aab:	bf 0a 00 00 00       	mov    $0xa,%edi
  405ab0:	e8 8b e6 ff ff       	call   404140 <runtime::print_byte>
  405ab5:	0f 0b                	ud2
  405ab7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  405abe:	00 00 

0000000000405ac0 <__truncsfhf2>:
  405ac0:	48 83 ec 18          	sub    $0x18,%rsp
  405ac4:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  405aca:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  405ad0:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  405ad6:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  405add:	00 
  405ade:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  405ae5:	00 
  405ae6:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  405aed:	00 
  405aee:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  405af5:	00 
  405af6:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  405afd:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  405b03:	8b 44 24 10          	mov    0x10(%rsp),%eax
  405b07:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  405b0b:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  405b0f:	c1 f9 10             	sar    $0x10,%ecx
  405b12:	b2 01                	mov    $0x1,%dl
  405b14:	31 c0                	xor    %eax,%eax
  405b16:	f6 c2 01             	test   $0x1,%dl
  405b19:	0f 45 c1             	cmovne %ecx,%eax
  405b1c:	25 00 80 00 00       	and    $0x8000,%eax
  405b21:	89 44 24 08          	mov    %eax,0x8(%rsp)
  405b25:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  405b29:	c1 f9 17             	sar    $0x17,%ecx
  405b2c:	b2 01                	mov    $0x1,%dl
  405b2e:	31 c0                	xor    %eax,%eax
  405b30:	f6 c2 01             	test   $0x1,%dl
  405b33:	0f 45 c1             	cmovne %ecx,%eax
  405b36:	25 ff 00 00 00       	and    $0xff,%eax
  405b3b:	83 e8 70             	sub    $0x70,%eax
  405b3e:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405b42:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  405b46:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  405b4b:	89 04 24             	mov    %eax,(%rsp)
  405b4e:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  405b53:	0f 9e c0             	setle  %al
  405b56:	24 01                	and    $0x1,%al
  405b58:	3c 00                	cmp    $0x0,%al
  405b5a:	0f 84 82 00 00 00    	je     405be2 <__truncsfhf2+0x122>
  405b60:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  405b65:	0f 9c c0             	setl   %al
  405b68:	24 01                	and    $0x1,%al
  405b6a:	3c 00                	cmp    $0x0,%al
  405b6c:	74 16                	je     405b84 <__truncsfhf2+0xc4>
  405b6e:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  405b73:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  405b78:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  405b7f:	48 83 c4 18          	add    $0x18,%rsp
  405b83:	c3                   	ret
  405b84:	8b 04 24             	mov    (%rsp),%eax
  405b87:	0d 00 00 80 00       	or     $0x800000,%eax
  405b8c:	ba 01 00 00 00       	mov    $0x1,%edx
  405b91:	2b 54 24 04          	sub    0x4(%rsp),%edx
  405b95:	89 d1                	mov    %edx,%ecx
  405b97:	d3 f8                	sar    %cl,%eax
  405b99:	89 c1                	mov    %eax,%ecx
  405b9b:	31 c0                	xor    %eax,%eax
  405b9d:	83 fa 20             	cmp    $0x20,%edx
  405ba0:	0f 42 c1             	cmovb  %ecx,%eax
  405ba3:	89 04 24             	mov    %eax,(%rsp)
  405ba6:	8b 04 24             	mov    (%rsp),%eax
  405ba9:	25 00 10 00 00       	and    $0x1000,%eax
  405bae:	83 f8 00             	cmp    $0x0,%eax
  405bb1:	0f 95 c0             	setne  %al
  405bb4:	24 01                	and    $0x1,%al
  405bb6:	3c 00                	cmp    $0x0,%al
  405bb8:	74 0b                	je     405bc5 <__truncsfhf2+0x105>
  405bba:	8b 04 24             	mov    (%rsp),%eax
  405bbd:	05 00 20 00 00       	add    $0x2000,%eax
  405bc2:	89 04 24             	mov    %eax,(%rsp)
  405bc5:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405bc9:	8b 0c 24             	mov    (%rsp),%ecx
  405bcc:	c1 e9 0d             	shr    $0xd,%ecx
  405bcf:	09 c8                	or     %ecx,%eax
  405bd1:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  405bd6:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  405bdd:	48 83 c4 18          	add    $0x18,%rsp
  405be1:	c3                   	ret
  405be2:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  405be9:	00 
  405bea:	0f 94 c0             	sete   %al
  405bed:	24 01                	and    $0x1,%al
  405bef:	3c 00                	cmp    $0x0,%al
  405bf1:	74 59                	je     405c4c <__truncsfhf2+0x18c>
  405bf3:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  405bf7:	0f 94 c0             	sete   %al
  405bfa:	24 01                	and    $0x1,%al
  405bfc:	3c 00                	cmp    $0x0,%al
  405bfe:	74 1a                	je     405c1a <__truncsfhf2+0x15a>
  405c00:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405c04:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405c09:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  405c0e:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  405c15:	48 83 c4 18          	add    $0x18,%rsp
  405c19:	c3                   	ret
  405c1a:	8b 04 24             	mov    (%rsp),%eax
  405c1d:	c1 f8 0d             	sar    $0xd,%eax
  405c20:	89 04 24             	mov    %eax,(%rsp)
  405c23:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405c27:	8b 0c 24             	mov    (%rsp),%ecx
  405c2a:	09 c8                	or     %ecx,%eax
  405c2c:	85 c9                	test   %ecx,%ecx
  405c2e:	0f 94 c1             	sete   %cl
  405c31:	0f b6 c9             	movzbl %cl,%ecx
  405c34:	09 c8                	or     %ecx,%eax
  405c36:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405c3b:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  405c40:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  405c47:	48 83 c4 18          	add    $0x18,%rsp
  405c4b:	c3                   	ret
  405c4c:	8b 04 24             	mov    (%rsp),%eax
  405c4f:	25 00 10 00 00       	and    $0x1000,%eax
  405c54:	83 f8 00             	cmp    $0x0,%eax
  405c57:	0f 95 c0             	setne  %al
  405c5a:	24 01                	and    $0x1,%al
  405c5c:	3c 00                	cmp    $0x0,%al
  405c5e:	74 33                	je     405c93 <__truncsfhf2+0x1d3>
  405c60:	8b 04 24             	mov    (%rsp),%eax
  405c63:	05 00 20 00 00       	add    $0x2000,%eax
  405c68:	89 04 24             	mov    %eax,(%rsp)
  405c6b:	8b 04 24             	mov    (%rsp),%eax
  405c6e:	25 00 00 80 00       	and    $0x800000,%eax
  405c73:	83 f8 00             	cmp    $0x0,%eax
  405c76:	0f 95 c0             	setne  %al
  405c79:	24 01                	and    $0x1,%al
  405c7b:	3c 00                	cmp    $0x0,%al
  405c7d:	74 12                	je     405c91 <__truncsfhf2+0x1d1>
  405c7f:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  405c86:	8b 44 24 04          	mov    0x4(%rsp),%eax
  405c8a:	83 c0 01             	add    $0x1,%eax
  405c8d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405c91:	eb 00                	jmp    405c93 <__truncsfhf2+0x1d3>
  405c93:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  405c98:	0f 9f c0             	setg   %al
  405c9b:	24 01                	and    $0x1,%al
  405c9d:	3c 00                	cmp    $0x0,%al
  405c9f:	74 75                	je     405d16 <__truncsfhf2+0x256>
  405ca1:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  405ca8:	00 00 00 
  405cab:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405cb0:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  405cb7:	00 00 
  405cb9:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  405cbf:	0f 9c c0             	setl   %al
  405cc2:	24 01                	and    $0x1,%al
  405cc4:	3c 00                	cmp    $0x0,%al
  405cc6:	74 34                	je     405cfc <__truncsfhf2+0x23c>
  405cc8:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405ccd:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405cd2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405cd7:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  405cdd:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405ce2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405ce7:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405cec:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405cf1:	48 83 c0 01          	add    $0x1,%rax
  405cf5:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  405cfa:	eb bd                	jmp    405cb9 <__truncsfhf2+0x1f9>
  405cfc:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405d00:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405d05:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  405d0a:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  405d11:	48 83 c4 18          	add    $0x18,%rsp
  405d15:	c3                   	ret
  405d16:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405d1a:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  405d1e:	c1 e1 0a             	shl    $0xa,%ecx
  405d21:	09 c8                	or     %ecx,%eax
  405d23:	8b 0c 24             	mov    (%rsp),%ecx
  405d26:	c1 e9 0d             	shr    $0xd,%ecx
  405d29:	09 c8                	or     %ecx,%eax
  405d2b:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  405d30:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  405d37:	48 83 c4 18          	add    $0x18,%rsp
  405d3b:	c3                   	ret
  405d3c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405d40 <__truncdfhf2>:
  405d40:	48 83 ec 18          	sub    $0x18,%rsp
  405d44:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  405d4a:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  405d50:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  405d56:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  405d5a:	e8 61 fd ff ff       	call   405ac0 <__truncsfhf2>
  405d5f:	48 83 c4 18          	add    $0x18,%rsp
  405d63:	c3                   	ret
  405d64:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  405d6b:	00 00 00 00 00 

0000000000405d70 <__gnu_h2f_ieee>:
  405d70:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  405d76:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  405d7c:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  405d83:	00 
  405d84:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  405d8b:	00 
  405d8c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  405d91:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  405d96:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  405d9d:	00 
  405d9e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  405da5:	00 
  405da6:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  405dad:	00 
  405dae:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  405db5:	77 
  405db6:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  405dbd:	47 
  405dbe:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405dc3:	66 25 ff 7f          	and    $0x7fff,%ax
  405dc7:	0f b7 c8             	movzwl %ax,%ecx
  405dca:	c1 e1 0d             	shl    $0xd,%ecx
  405dcd:	b2 01                	mov    $0x1,%dl
  405dcf:	31 c0                	xor    %eax,%eax
  405dd1:	f6 c2 01             	test   $0x1,%dl
  405dd4:	0f 45 c1             	cmovne %ecx,%eax
  405dd7:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405ddb:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  405de1:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  405de7:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  405ded:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405df3:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  405df8:	0f 93 c0             	setae  %al
  405dfb:	24 01                	and    $0x1,%al
  405dfd:	3c 00                	cmp    $0x0,%al
  405dff:	74 0d                	je     405e0e <__gnu_h2f_ieee+0x9e>
  405e01:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405e05:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  405e0a:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405e0e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405e13:	66 25 00 80          	and    $0x8000,%ax
  405e17:	0f b7 c8             	movzwl %ax,%ecx
  405e1a:	c1 e1 10             	shl    $0x10,%ecx
  405e1d:	b2 01                	mov    $0x1,%dl
  405e1f:	31 c0                	xor    %eax,%eax
  405e21:	f6 c2 01             	test   $0x1,%dl
  405e24:	0f 45 c1             	cmovne %ecx,%eax
  405e27:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  405e2b:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405e2f:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405e35:	c3                   	ret
  405e36:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  405e3d:	00 00 00 

0000000000405e40 <__gnu_f2h_ieee>:
  405e40:	50                   	push   %rax
  405e41:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  405e46:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  405e4b:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  405e51:	e8 6a fc ff ff       	call   405ac0 <__truncsfhf2>
  405e56:	58                   	pop    %rax
  405e57:	c3                   	ret
  405e58:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  405e5f:	00 

0000000000405e60 <__extendhfsf2>:
  405e60:	50                   	push   %rax
  405e61:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  405e67:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  405e6d:	0f 28 c8             	movaps %xmm0,%xmm1
  405e70:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  405e77:	00 
  405e78:	e8 f3 fe ff ff       	call   405d70 <__gnu_h2f_ieee>
  405e7d:	58                   	pop    %rax
  405e7e:	c3                   	ret
  405e7f:	90                   	nop

0000000000405e80 <__floattidf>:
  405e80:	53                   	push   %rbx
  405e81:	48 83 ec 10          	sub    $0x10,%rsp
  405e85:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405e8a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  405e8f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405e94:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405e99:	48 89 04 24          	mov    %rax,(%rsp)
  405e9d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405ea2:	48 09 c8             	or     %rcx,%rax
  405ea5:	0f 94 c0             	sete   %al
  405ea8:	24 01                	and    $0x1,%al
  405eaa:	3c 00                	cmp    $0x0,%al
  405eac:	74 09                	je     405eb7 <__floattidf+0x37>
  405eae:	0f 57 c0             	xorps  %xmm0,%xmm0
  405eb1:	48 83 c4 10          	add    $0x10,%rsp
  405eb5:	5b                   	pop    %rbx
  405eb6:	c3                   	ret
  405eb7:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405ebc:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405ec1:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405ec6:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405ecb:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405ed0:	48 c1 f8 3f          	sar    $0x3f,%rax
  405ed4:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405ed9:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405ede:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405ee3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405ee8:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  405eed:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405ef2:	48 31 d0             	xor    %rdx,%rax
  405ef5:	48 31 f1             	xor    %rsi,%rcx
  405ef8:	48 29 f1             	sub    %rsi,%rcx
  405efb:	48 19 d0             	sbb    %rdx,%rax
  405efe:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405f03:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405f08:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  405f0d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  405f12:	48 0f bd c2          	bsr    %rdx,%rax
  405f16:	48 83 f0 3f          	xor    $0x3f,%rax
  405f1a:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  405f1f:	48 0f bd ce          	bsr    %rsi,%rcx
  405f23:	48 83 f1 3f          	xor    $0x3f,%rcx
  405f27:	48 83 c1 40          	add    $0x40,%rcx
  405f2b:	48 85 d2             	test   %rdx,%rdx
  405f2e:	48 0f 45 c8          	cmovne %rax,%rcx
  405f32:	31 c0                	xor    %eax,%eax
  405f34:	ba 80 00 00 00       	mov    $0x80,%edx
  405f39:	48 29 ca             	sub    %rcx,%rdx
  405f3c:	48 89 c1             	mov    %rax,%rcx
  405f3f:	48 19 c9             	sbb    %rcx,%rcx
  405f42:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405f47:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405f4c:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  405f50:	ff c9                	dec    %ecx
  405f52:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  405f56:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  405f5b:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405f60:	ba 35 00 00 00       	mov    $0x35,%edx
  405f65:	48 29 f2             	sub    %rsi,%rdx
  405f68:	48 19 c8             	sbb    %rcx,%rax
  405f6b:	0f 9c c0             	setl   %al
  405f6e:	24 01                	and    $0x1,%al
  405f70:	3c 00                	cmp    $0x0,%al
  405f72:	0f 84 c0 01 00 00    	je     406138 <__floattidf+0x2b8>
  405f78:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405f7d:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  405f82:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405f87:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405f8c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  405f91:	0f 28 0d 88 23 00 00 	movaps 0x2388(%rip),%xmm1        # 408320 <_IO_stdin_used+0x320>
  405f98:	66 0f ef c1          	pxor   %xmm1,%xmm0
  405f9c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  405fa1:	74 17                	je     405fba <__floattidf+0x13a>
  405fa3:	eb 00                	jmp    405fa5 <__floattidf+0x125>
  405fa5:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  405faa:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405faf:	48 83 f0 37          	xor    $0x37,%rax
  405fb3:	48 09 c8             	or     %rcx,%rax
  405fb6:	74 26                	je     405fde <__floattidf+0x15e>
  405fb8:	eb 29                	jmp    405fe3 <__floattidf+0x163>
  405fba:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405fbf:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405fc4:	48 89 d0             	mov    %rdx,%rax
  405fc7:	48 01 c0             	add    %rax,%rax
  405fca:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  405fcf:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405fd4:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405fd9:	e9 d6 00 00 00       	jmp    4060b4 <__floattidf+0x234>
  405fde:	e9 d1 00 00 00       	jmp    4060b4 <__floattidf+0x234>
  405fe3:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405fe8:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  405fed:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405ff2:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405ff7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405ffc:	49 89 fb             	mov    %rdi,%r11
  405fff:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  406003:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  406007:	44 88 db             	mov    %r11b,%bl
  40600a:	88 d9                	mov    %bl,%cl
  40600c:	49 89 f2             	mov    %rsi,%r10
  40600f:	49 d3 ea             	shr    %cl,%r10
  406012:	88 d9                	mov    %bl,%cl
  406014:	49 89 d1             	mov    %rdx,%r9
  406017:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40601b:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  406020:	45 31 c0             	xor    %r8d,%r8d
  406023:	f6 c3 40             	test   $0x40,%bl
  406026:	4d 0f 45 ca          	cmovne %r10,%r9
  40602a:	4d 0f 45 d0          	cmovne %r8,%r10
  40602e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  406035:	48 83 d8 00          	sbb    $0x0,%rax
  406039:	4c 89 c0             	mov    %r8,%rax
  40603c:	49 0f 42 c2          	cmovb  %r10,%rax
  406040:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  406045:	4c 89 c0             	mov    %r8,%rax
  406048:	49 0f 42 c1          	cmovb  %r9,%rax
  40604c:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  406052:	49 29 fb             	sub    %rdi,%r11
  406055:	4c 89 c7             	mov    %r8,%rdi
  406058:	48 19 cf             	sbb    %rcx,%rdi
  40605b:	45 88 d9             	mov    %r11b,%r9b
  40605e:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  406065:	44 88 c9             	mov    %r9b,%cl
  406068:	4c 89 d3             	mov    %r10,%rbx
  40606b:	48 d3 eb             	shr    %cl,%rbx
  40606e:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  406073:	41 f6 c1 40          	test   $0x40,%r9b
  406077:	49 89 d9             	mov    %rbx,%r9
  40607a:	4d 0f 45 c8          	cmovne %r8,%r9
  40607e:	4c 0f 45 d3          	cmovne %rbx,%r10
  406082:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  406089:	48 83 df 00          	sbb    $0x0,%rdi
  40608d:	4c 89 c7             	mov    %r8,%rdi
  406090:	49 0f 42 fa          	cmovb  %r10,%rdi
  406094:	4d 0f 42 c1          	cmovb  %r9,%r8
  406098:	4c 21 c6             	and    %r8,%rsi
  40609b:	48 21 fa             	and    %rdi,%rdx
  40609e:	48 09 f2             	or     %rsi,%rdx
  4060a1:	0f 95 c2             	setne  %dl
  4060a4:	0f b6 d2             	movzbl %dl,%edx
  4060a7:	48 09 d0             	or     %rdx,%rax
  4060aa:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4060af:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4060b4:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4060b9:	89 c1                	mov    %eax,%ecx
  4060bb:	83 e1 04             	and    $0x4,%ecx
  4060be:	c1 e9 02             	shr    $0x2,%ecx
  4060c1:	48 09 c8             	or     %rcx,%rax
  4060c4:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4060c9:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4060ce:	48 83 c0 01          	add    $0x1,%rax
  4060d2:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  4060d8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4060dd:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  4060e2:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4060e7:	48 89 c8             	mov    %rcx,%rax
  4060ea:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  4060ef:	48 c1 f9 02          	sar    $0x2,%rcx
  4060f3:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4060f8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4060fd:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  406101:	24 20                	and    $0x20,%al
  406103:	c0 e8 05             	shr    $0x5,%al
  406106:	24 01                	and    $0x1,%al
  406108:	3c 00                	cmp    $0x0,%al
  40610a:	74 2a                	je     406136 <__floattidf+0x2b6>
  40610c:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  406111:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  406116:	48 89 c8             	mov    %rcx,%rax
  406119:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40611e:	48 d1 f9             	sar    $1,%rcx
  406121:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  406126:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40612b:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40612f:	83 c0 01             	add    $0x1,%eax
  406132:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  406136:	eb 5c                	jmp    406194 <__floattidf+0x314>
  406138:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  40613c:	b9 35 00 00 00       	mov    $0x35,%ecx
  406141:	29 c1                	sub    %eax,%ecx
  406143:	83 e1 7f             	and    $0x7f,%ecx
  406146:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  40614a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40614f:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  406154:	40 88 cf             	mov    %cl,%dil
  406157:	40 88 f9             	mov    %dil,%cl
  40615a:	48 89 c2             	mov    %rax,%rdx
  40615d:	48 d3 e2             	shl    %cl,%rdx
  406160:	40 88 f9             	mov    %dil,%cl
  406163:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  406167:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  40616b:	31 c0                	xor    %eax,%eax
  40616d:	40 f6 c7 40          	test   $0x40,%dil
  406171:	48 0f 45 f2          	cmovne %rdx,%rsi
  406175:	48 0f 45 d0          	cmovne %rax,%rdx
  406179:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  40617f:	48 89 c1             	mov    %rax,%rcx
  406182:	48 0f 42 ce          	cmovb  %rsi,%rcx
  406186:	48 0f 42 c2          	cmovb  %rdx,%rax
  40618a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40618f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406194:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  40619b:	00 00 
  40619d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4061a1:	25 00 00 00 80       	and    $0x80000000,%eax
  4061a6:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  4061aa:	c1 e1 14             	shl    $0x14,%ecx
  4061ad:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  4061b3:	09 c8                	or     %ecx,%eax
  4061b5:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  4061b9:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  4061bf:	09 c8                	or     %ecx,%eax
  4061c1:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  4061c5:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4061c9:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  4061cd:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  4061d3:	48 83 c4 10          	add    $0x10,%rsp
  4061d7:	5b                   	pop    %rbx
  4061d8:	c3                   	ret
  4061d9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004061e0 <__floattidf_unsigned>:
  4061e0:	53                   	push   %rbx
  4061e1:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  4061e6:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4061eb:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  4061f0:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4061f5:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4061fa:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4061ff:	48 09 c8             	or     %rcx,%rax
  406202:	0f 94 c0             	sete   %al
  406205:	24 01                	and    $0x1,%al
  406207:	3c 00                	cmp    $0x0,%al
  406209:	74 05                	je     406210 <__floattidf_unsigned+0x30>
  40620b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40620e:	5b                   	pop    %rbx
  40620f:	c3                   	ret
  406210:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  406215:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40621a:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40621f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406224:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  406229:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40622e:	48 0f bd c2          	bsr    %rdx,%rax
  406232:	48 83 f0 3f          	xor    $0x3f,%rax
  406236:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40623b:	48 0f bd ce          	bsr    %rsi,%rcx
  40623f:	48 83 f1 3f          	xor    $0x3f,%rcx
  406243:	48 83 c1 40          	add    $0x40,%rcx
  406247:	48 85 d2             	test   %rdx,%rdx
  40624a:	48 0f 45 c8          	cmovne %rax,%rcx
  40624e:	31 c0                	xor    %eax,%eax
  406250:	ba 80 00 00 00       	mov    $0x80,%edx
  406255:	48 29 ca             	sub    %rcx,%rdx
  406258:	48 89 c1             	mov    %rax,%rcx
  40625b:	48 19 c9             	sbb    %rcx,%rcx
  40625e:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  406263:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406268:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  40626c:	ff c9                	dec    %ecx
  40626e:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  406272:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  406277:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40627c:	ba 35 00 00 00       	mov    $0x35,%edx
  406281:	48 29 f2             	sub    %rsi,%rdx
  406284:	48 19 c8             	sbb    %rcx,%rax
  406287:	0f 92 c0             	setb   %al
  40628a:	24 01                	and    $0x1,%al
  40628c:	3c 00                	cmp    $0x0,%al
  40628e:	0f 84 c0 01 00 00    	je     406454 <__floattidf_unsigned+0x274>
  406294:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406299:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40629e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4062a3:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  4062a8:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  4062ad:	0f 28 0d 6c 20 00 00 	movaps 0x206c(%rip),%xmm1        # 408320 <_IO_stdin_used+0x320>
  4062b4:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4062b8:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  4062bd:	74 17                	je     4062d6 <__floattidf_unsigned+0xf6>
  4062bf:	eb 00                	jmp    4062c1 <__floattidf_unsigned+0xe1>
  4062c1:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  4062c6:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  4062cb:	48 83 f0 37          	xor    $0x37,%rax
  4062cf:	48 09 c8             	or     %rcx,%rax
  4062d2:	74 26                	je     4062fa <__floattidf_unsigned+0x11a>
  4062d4:	eb 29                	jmp    4062ff <__floattidf_unsigned+0x11f>
  4062d6:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4062db:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4062e0:	48 89 d0             	mov    %rdx,%rax
  4062e3:	48 01 c0             	add    %rax,%rax
  4062e6:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  4062eb:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4062f0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4062f5:	e9 d6 00 00 00       	jmp    4063d0 <__floattidf_unsigned+0x1f0>
  4062fa:	e9 d1 00 00 00       	jmp    4063d0 <__floattidf_unsigned+0x1f0>
  4062ff:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  406304:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  406309:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  40630e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406313:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  406318:	49 89 fb             	mov    %rdi,%r11
  40631b:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  40631f:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  406323:	44 88 db             	mov    %r11b,%bl
  406326:	88 d9                	mov    %bl,%cl
  406328:	49 89 f2             	mov    %rsi,%r10
  40632b:	49 d3 ea             	shr    %cl,%r10
  40632e:	88 d9                	mov    %bl,%cl
  406330:	49 89 d1             	mov    %rdx,%r9
  406333:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  406337:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40633c:	45 31 c0             	xor    %r8d,%r8d
  40633f:	f6 c3 40             	test   $0x40,%bl
  406342:	4d 0f 45 ca          	cmovne %r10,%r9
  406346:	4d 0f 45 d0          	cmovne %r8,%r10
  40634a:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  406351:	48 83 d8 00          	sbb    $0x0,%rax
  406355:	4c 89 c0             	mov    %r8,%rax
  406358:	49 0f 42 c2          	cmovb  %r10,%rax
  40635c:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  406361:	4c 89 c0             	mov    %r8,%rax
  406364:	49 0f 42 c1          	cmovb  %r9,%rax
  406368:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  40636e:	49 29 fb             	sub    %rdi,%r11
  406371:	4c 89 c7             	mov    %r8,%rdi
  406374:	48 19 cf             	sbb    %rcx,%rdi
  406377:	45 88 d9             	mov    %r11b,%r9b
  40637a:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  406381:	44 88 c9             	mov    %r9b,%cl
  406384:	4c 89 d3             	mov    %r10,%rbx
  406387:	48 d3 eb             	shr    %cl,%rbx
  40638a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40638f:	41 f6 c1 40          	test   $0x40,%r9b
  406393:	49 89 d9             	mov    %rbx,%r9
  406396:	4d 0f 45 c8          	cmovne %r8,%r9
  40639a:	4c 0f 45 d3          	cmovne %rbx,%r10
  40639e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  4063a5:	48 83 df 00          	sbb    $0x0,%rdi
  4063a9:	4c 89 c7             	mov    %r8,%rdi
  4063ac:	49 0f 42 fa          	cmovb  %r10,%rdi
  4063b0:	4d 0f 42 c1          	cmovb  %r9,%r8
  4063b4:	4c 21 c6             	and    %r8,%rsi
  4063b7:	48 21 fa             	and    %rdi,%rdx
  4063ba:	48 09 f2             	or     %rsi,%rdx
  4063bd:	0f 95 c2             	setne  %dl
  4063c0:	0f b6 d2             	movzbl %dl,%edx
  4063c3:	48 09 d0             	or     %rdx,%rax
  4063c6:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4063cb:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4063d0:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4063d5:	89 c1                	mov    %eax,%ecx
  4063d7:	83 e1 04             	and    $0x4,%ecx
  4063da:	c1 e9 02             	shr    $0x2,%ecx
  4063dd:	48 09 c8             	or     %rcx,%rax
  4063e0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4063e5:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4063ea:	48 83 c0 01          	add    $0x1,%rax
  4063ee:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  4063f4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4063f9:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4063fe:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406403:	48 89 c8             	mov    %rcx,%rax
  406406:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40640b:	48 c1 e9 02          	shr    $0x2,%rcx
  40640f:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  406414:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406419:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  40641d:	24 20                	and    $0x20,%al
  40641f:	c0 e8 05             	shr    $0x5,%al
  406422:	24 01                	and    $0x1,%al
  406424:	3c 00                	cmp    $0x0,%al
  406426:	74 2a                	je     406452 <__floattidf_unsigned+0x272>
  406428:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40642d:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406432:	48 89 c8             	mov    %rcx,%rax
  406435:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40643a:	48 d1 e9             	shr    $1,%rcx
  40643d:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  406442:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406447:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40644b:	83 c0 01             	add    $0x1,%eax
  40644e:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  406452:	eb 6a                	jmp    4064be <__floattidf_unsigned+0x2de>
  406454:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  406459:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40645e:	31 c0                	xor    %eax,%eax
  406460:	bf 35 00 00 00       	mov    $0x35,%edi
  406465:	48 29 d7             	sub    %rdx,%rdi
  406468:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40646d:	48 19 c8             	sbb    %rcx,%rax
  406470:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  406475:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40647a:	41 88 f8             	mov    %dil,%r8b
  40647d:	44 88 c1             	mov    %r8b,%cl
  406480:	4c 89 ce             	mov    %r9,%rsi
  406483:	48 d3 e6             	shl    %cl,%rsi
  406486:	44 88 c1             	mov    %r8b,%cl
  406489:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  40648d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  406492:	41 f6 c0 40          	test   $0x40,%r8b
  406496:	48 0f 45 d6          	cmovne %rsi,%rdx
  40649a:	48 0f 45 f1          	cmovne %rcx,%rsi
  40649e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  4064a5:	48 83 d8 00          	sbb    $0x0,%rax
  4064a9:	48 89 c8             	mov    %rcx,%rax
  4064ac:	48 0f 42 c6          	cmovb  %rsi,%rax
  4064b0:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4064b4:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4064b9:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4064be:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  4064c5:	00 00 
  4064c7:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  4064cb:	c1 e2 14             	shl    $0x14,%edx
  4064ce:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  4064d2:	25 ff ff 0f 00       	and    $0xfffff,%eax
  4064d7:	89 c1                	mov    %eax,%ecx
  4064d9:	89 d0                	mov    %edx,%eax
  4064db:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  4064e2:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  4064e6:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4064ea:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  4064ee:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  4064f4:	5b                   	pop    %rbx
  4064f5:	c3                   	ret
  4064f6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4064fd:	00 00 00 

0000000000406500 <__umodti3>:
  406500:	48 83 ec 58          	sub    $0x58,%rsp
  406504:	48 89 0c 24          	mov    %rcx,(%rsp)
  406508:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40650d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406512:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406517:	48 8b 0c 24          	mov    (%rsp),%rcx
  40651b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  406520:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  406525:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40652a:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40652f:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  406534:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  406539:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40653e:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  406543:	e8 08 b4 ff ff       	call   401950 <runtime::udivmod128>
  406548:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40654d:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406552:	48 83 c4 58          	add    $0x58,%rsp
  406556:	c3                   	ret
  406557:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40655e:	00 00 

0000000000406560 <__udivmodti4>:
  406560:	48 83 ec 58          	sub    $0x58,%rsp
  406564:	4c 89 04 24          	mov    %r8,(%rsp)
  406568:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40656d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  406572:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  406577:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40657c:	4c 8b 04 24          	mov    (%rsp),%r8
  406580:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406585:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40658a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40658f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406594:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  406599:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40659e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4065a3:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4065a8:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  4065ad:	e8 9e b3 ff ff       	call   401950 <runtime::udivmod128>
  4065b2:	48 83 c4 58          	add    $0x58,%rsp
  4065b6:	c3                   	ret
  4065b7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4065be:	00 00 

00000000004065c0 <__udivti3>:
  4065c0:	48 83 ec 48          	sub    $0x48,%rsp
  4065c4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4065c8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4065cd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4065d2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4065d7:	48 8b 0c 24          	mov    (%rsp),%rcx
  4065db:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4065e0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4065e5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4065ea:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  4065ef:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4065f4:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4065f9:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4065fe:	31 c0                	xor    %eax,%eax
  406600:	41 89 c0             	mov    %eax,%r8d
  406603:	e8 58 ff ff ff       	call   406560 <__udivmodti4>
  406608:	48 83 c4 48          	add    $0x48,%rsp
  40660c:	c3                   	ret
  40660d:	0f 1f 00             	nopl   (%rax)

0000000000406610 <runtime::assert>:
  406610:	48 83 ec 48          	sub    $0x48,%rsp
  406614:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  406619:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40661e:	40 88 f8             	mov    %dil,%al
  406621:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  406625:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40662a:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40662f:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  406633:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  406638:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40663d:	88 44 24 47          	mov    %al,0x47(%rsp)
  406641:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  406646:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40664b:	3c 00                	cmp    $0x0,%al
  40664d:	75 19                	jne    406668 <runtime::assert+0x58>
  40664f:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406654:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  406659:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40665e:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406663:	e8 68 0a 00 00       	call   4070d0 <runtime::assert.internal-0>
  406668:	48 83 c4 48          	add    $0x48,%rsp
  40666c:	c3                   	ret
  40666d:	0f 1f 00             	nopl   (%rax)

0000000000406670 <runtime::heap_allocator_proc.aligned_alloc-0>:
  406670:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  406677:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40667c:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  406681:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  406686:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40668b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406690:	44 88 c0             	mov    %r8b,%al
  406693:	88 44 24 37          	mov    %al,0x37(%rsp)
  406697:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40669e:	00 
  40669f:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4066a4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4066a9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4066ae:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4066b3:	8a 4c 24 37          	mov    0x37(%rsp),%cl
  4066b7:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4066bc:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  4066c3:	00 
  4066c4:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  4066cb:	00 
  4066cc:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  4066d3:	00 
  4066d4:	48 89 bc 24 90 00 00 	mov    %rdi,0x90(%rsp)
  4066db:	00 
  4066dc:	88 8c 24 8f 00 00 00 	mov    %cl,0x8f(%rsp)
  4066e3:	b9 08 00 00 00       	mov    $0x8,%ecx
  4066e8:	48 83 fe 08          	cmp    $0x8,%rsi
  4066ec:	48 0f 4f ce          	cmovg  %rsi,%rcx
  4066f0:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  4066f7:	00 
  4066f8:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  4066ff:	00 
  406700:	48 83 e9 01          	sub    $0x1,%rcx
  406704:	48 83 c1 08          	add    $0x8,%rcx
  406708:	48 01 d1             	add    %rdx,%rcx
  40670b:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  406710:	48 c7 44 24 70 00 00 	movq   $0x0,0x70(%rsp)
  406717:	00 00 
  406719:	48 83 f8 00          	cmp    $0x0,%rax
  40671d:	0f 95 c1             	setne  %cl
  406720:	80 e1 01             	and    $0x1,%cl
  406723:	31 c0                	xor    %eax,%eax
  406725:	80 f9 00             	cmp    $0x0,%cl
  406728:	88 44 24 07          	mov    %al,0x7(%rsp)
  40672c:	74 17                	je     406745 <runtime::heap_allocator_proc.aligned_alloc-0+0xd5>
  40672e:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406733:	48 83 f8 08          	cmp    $0x8,%rax
  406737:	0f 9f c0             	setg   %al
  40673a:	24 01                	and    $0x1,%al
  40673c:	3c 00                	cmp    $0x0,%al
  40673e:	0f 95 c0             	setne  %al
  406741:	88 44 24 07          	mov    %al,0x7(%rsp)
  406745:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40674a:	8a 4c 24 07          	mov    0x7(%rsp),%cl
  40674e:	80 e1 01             	and    $0x1,%cl
  406751:	88 4c 24 6f          	mov    %cl,0x6f(%rsp)
  406755:	48 83 f8 00          	cmp    $0x0,%rax
  406759:	0f 95 c0             	setne  %al
  40675c:	24 01                	and    $0x1,%al
  40675e:	3c 00                	cmp    $0x0,%al
  406760:	74 2b                	je     40678d <runtime::heap_allocator_proc.aligned_alloc-0+0x11d>
  406762:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  406767:	75 24                	jne    40678d <runtime::heap_allocator_proc.aligned_alloc-0+0x11d>
  406769:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40676e:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  406772:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  406777:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  40677c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  406781:	e8 6a db ff ff       	call   4042f0 <runtime::heap_resize>
  406786:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40678b:	eb 16                	jmp    4067a3 <runtime::heap_allocator_proc.aligned_alloc-0+0x133>
  40678d:	8a 44 24 37          	mov    0x37(%rsp),%al
  406791:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  406796:	0f b6 f0             	movzbl %al,%esi
  406799:	e8 22 db ff ff       	call   4042c0 <runtime::heap_alloc>
  40679e:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4067a3:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4067a8:	48 83 c0 08          	add    $0x8,%rax
  4067ac:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4067b1:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4067b6:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4067bb:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4067c0:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4067c5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4067ca:	48 03 84 24 80 00 00 	add    0x80(%rsp),%rax
  4067d1:	00 
  4067d2:	48 83 e8 01          	sub    $0x1,%rax
  4067d6:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  4067dd:	00 
  4067de:	48 83 e9 01          	sub    $0x1,%rcx
  4067e2:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  4067e6:	48 21 c8             	and    %rcx,%rax
  4067e9:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4067ee:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  4067f4:	0f 94 c0             	sete   %al
  4067f7:	24 01                	and    $0x1,%al
  4067f9:	3c 00                	cmp    $0x0,%al
  4067fb:	74 3c                	je     406839 <runtime::heap_allocator_proc.aligned_alloc-0+0x1c9>
  4067fd:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  406802:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  406807:	e8 04 01 00 00       	call   406910 <runtime::heap_allocator_proc.aligned_free-1>
  40680c:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  406811:	48 8b 7c 24 70       	mov    0x70(%rsp),%rdi
  406816:	e8 f5 00 00 00       	call   406910 <runtime::heap_allocator_proc.aligned_free-1>
  40681b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406820:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  406827:	00 
  406828:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40682f:	b0 01                	mov    $0x1,%al
  406831:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  406838:	c3                   	ret
  406839:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40683e:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  406843:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  406848:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40684d:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  406851:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  406856:	74 2f                	je     406887 <runtime::heap_allocator_proc.aligned_alloc-0+0x217>
  406858:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40685d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406862:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406867:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40686c:	48 39 d0             	cmp    %rdx,%rax
  40686f:	48 0f 4c d0          	cmovl  %rax,%rdx
  406873:	e8 08 d0 ff ff       	call   403880 <runtime::mem_copy_non_overlapping>
  406878:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40687d:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  406882:	e8 89 00 00 00       	call   406910 <runtime::heap_allocator_proc.aligned_free-1>
  406887:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40688c:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  406891:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  406896:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  40689d:	00 
  40689e:	48 89 b4 24 d8 00 00 	mov    %rsi,0xd8(%rsp)
  4068a5:	00 
  4068a6:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  4068ad:	00 
  4068ae:	48 8b 94 24 d0 00 00 	mov    0xd0(%rsp),%rdx
  4068b5:	00 
  4068b6:	31 c9                	xor    %ecx,%ecx
  4068b8:	48 85 f6             	test   %rsi,%rsi
  4068bb:	48 0f 49 ce          	cmovns %rsi,%rcx
  4068bf:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  4068c6:	00 
  4068c7:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  4068ce:	00 
  4068cf:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4068d6:	00 
  4068d7:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  4068de:	00 
  4068df:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  4068e6:	00 
  4068e7:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  4068ee:	00 
  4068ef:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4068f6:	00 
  4068f7:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  4068fe:	00 
  4068ff:	48 89 50 08          	mov    %rdx,0x8(%rax)
  406903:	48 89 08             	mov    %rcx,(%rax)
  406906:	31 c0                	xor    %eax,%eax
  406908:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40690f:	c3                   	ret

0000000000406910 <runtime::heap_allocator_proc.aligned_free-1>:
  406910:	48 83 ec 18          	sub    $0x18,%rsp
  406914:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406919:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40691e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406923:	48 83 f8 00          	cmp    $0x0,%rax
  406927:	0f 95 c0             	setne  %al
  40692a:	24 01                	and    $0x1,%al
  40692c:	3c 00                	cmp    $0x0,%al
  40692e:	74 0e                	je     40693e <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  406930:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406935:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  406939:	e8 e2 d9 ff ff       	call   404320 <runtime::heap_free>
  40693e:	48 83 c4 18          	add    $0x18,%rsp
  406942:	c3                   	ret
  406943:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40694a:	84 00 00 00 00 00 

0000000000406950 <runtime::heap_allocator_proc.aligned_resize-2>:
  406950:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  406957:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40695c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406961:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  406966:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40696b:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  406970:	44 88 c0             	mov    %r8b,%al
  406973:	88 44 24 57          	mov    %al,0x57(%rsp)
  406977:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40697e:	00 
  40697f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  406984:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  406989:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  40698d:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  406992:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  406997:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40699c:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4069a3:	00 
  4069a4:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  4069ab:	00 
  4069ac:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  4069b3:	00 
  4069b4:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  4069bb:	00 
  4069bc:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  4069c3:	0f 57 c0             	xorps  %xmm0,%xmm0
  4069c6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4069cd:	00 
  4069ce:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  4069d5:	00 
  4069d6:	48 83 f8 00          	cmp    $0x0,%rax
  4069da:	0f 94 c0             	sete   %al
  4069dd:	24 01                	and    $0x1,%al
  4069df:	3c 00                	cmp    $0x0,%al
  4069e1:	0f 84 80 00 00 00    	je     406a67 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  4069e7:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4069ec:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4069f1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4069f6:	8a 44 24 57          	mov    0x57(%rsp),%al
  4069fa:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  4069ff:	0f 57 c0             	xorps  %xmm0,%xmm0
  406a02:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  406a09:	00 
  406a0a:	48 89 e2             	mov    %rsp,%rdx
  406a0d:	4c 89 02             	mov    %r8,(%rdx)
  406a10:	44 0f b6 c0          	movzbl %al,%r8d
  406a14:	31 c0                	xor    %eax,%eax
  406a16:	89 c2                	mov    %eax,%edx
  406a18:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  406a1f:	00 
  406a20:	e8 4b fc ff ff       	call   406670 <runtime::heap_allocator_proc.aligned_alloc-0>
  406a25:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  406a2a:	40 88 c7             	mov    %al,%dil
  406a2d:	40 88 f8             	mov    %dil,%al
  406a30:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  406a37:	00 
  406a38:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  406a3f:	00 
  406a40:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406a47:	00 
  406a48:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  406a4f:	00 
  406a50:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  406a57:	00 
  406a58:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  406a5c:	48 89 11             	mov    %rdx,(%rcx)
  406a5f:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406a66:	c3                   	ret
  406a67:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  406a6c:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  406a71:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406a76:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406a7b:	8a 44 24 57          	mov    0x57(%rsp),%al
  406a7f:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  406a84:	0f 57 c0             	xorps  %xmm0,%xmm0
  406a87:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  406a8e:	00 
  406a8f:	49 89 e0             	mov    %rsp,%r8
  406a92:	4d 89 08             	mov    %r9,(%r8)
  406a95:	44 0f b6 c0          	movzbl %al,%r8d
  406a99:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  406aa0:	00 
  406aa1:	e8 ca fb ff ff       	call   406670 <runtime::heap_allocator_proc.aligned_alloc-0>
  406aa6:	88 44 24 17          	mov    %al,0x17(%rsp)
  406aaa:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  406ab1:	00 
  406ab2:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406ab7:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  406abe:	00 
  406abf:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  406ac4:	3c 00                	cmp    $0x0,%al
  406ac6:	74 4d                	je     406b15 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  406ac8:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  406acd:	8a 44 24 17          	mov    0x17(%rsp),%al
  406ad1:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406ad8:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  406adf:	00 
  406ae0:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406ae7:	00 
  406ae8:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  406aef:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406af6:	00 
  406af7:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  406afe:	00 
  406aff:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406b06:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  406b0a:	48 89 11             	mov    %rdx,(%rcx)
  406b0d:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406b14:	c3                   	ret
  406b15:	8a 44 24 57          	mov    0x57(%rsp),%al
  406b19:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  406b1e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406b23:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  406b2a:	00 
  406b2b:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  406b32:	00 
  406b33:	3c 00                	cmp    $0x0,%al
  406b35:	0f 84 87 00 00 00    	je     406bc2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  406b3b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406b40:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  406b45:	48 39 c8             	cmp    %rcx,%rax
  406b48:	0f 9f c0             	setg   %al
  406b4b:	24 01                	and    $0x1,%al
  406b4d:	3c 00                	cmp    $0x0,%al
  406b4f:	74 71                	je     406bc2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  406b51:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  406b56:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  406b5d:	00 
  406b5e:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406b63:	48 89 e0             	mov    %rsp,%rax
  406b66:	4c 89 08             	mov    %r9,(%rax)
  406b69:	bf 97 82 40 00       	mov    $0x408297,%edi
  406b6e:	be 30 00 00 00       	mov    $0x30,%esi
  406b73:	ba 4b 00 00 00       	mov    $0x4b,%edx
  406b78:	b9 25 00 00 00       	mov    $0x25,%ecx
  406b7d:	e8 8e cd ff ff       	call   403910 <runtime::slice_expr_error_lo_hi>
  406b82:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406b87:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406b8c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  406b91:	48 89 c6             	mov    %rax,%rsi
  406b94:	48 03 b4 24 e0 00 00 	add    0xe0(%rsp),%rsi
  406b9b:	00 
  406b9c:	48 29 c1             	sub    %rax,%rcx
  406b9f:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  406ba4:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  406ba9:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  406bae:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  406bb3:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  406bb8:	48 29 c2             	sub    %rax,%rdx
  406bbb:	31 f6                	xor    %esi,%esi
  406bbd:	e8 7e a4 ff ff       	call   401040 <memset@plt>
  406bc2:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  406bc7:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  406bce:	00 
  406bcf:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406bd6:	00 
  406bd7:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  406bde:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406be5:	00 
  406be6:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  406bed:	00 
  406bee:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406bf5:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  406bf9:	48 89 11             	mov    %rdx,(%rcx)
  406bfc:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406c03:	c3                   	ret
  406c04:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  406c0b:	00 00 00 00 00 

0000000000406c10 <runtime::bounds_check_error.handle_error-0>:
  406c10:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  406c17:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406c1c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406c21:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406c25:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406c29:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  406c2e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406c33:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406c38:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  406c3d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406c41:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406c45:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406c4a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406c4f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  406c54:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  406c5b:	00 
  406c5c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  406c60:	89 44 24 70          	mov    %eax,0x70(%rsp)
  406c64:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  406c69:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  406c6e:	0f 57 c0             	xorps  %xmm0,%xmm0
  406c71:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406c76:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  406c7b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  406c82:	00 00 
  406c84:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406c89:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  406c8e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  406c95:	00 00 
  406c97:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  406c9c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406ca1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  406ca5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  406ca9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  406cae:	e8 7d da ff ff       	call   404730 <runtime::print_caller_location>
  406cb3:	bf c8 82 40 00       	mov    $0x4082c8,%edi
  406cb8:	be 07 00 00 00       	mov    $0x7,%esi
  406cbd:	e8 0e d2 ff ff       	call   403ed0 <runtime::print_string>
  406cc2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406cc7:	e8 64 d8 ff ff       	call   404530 <runtime::print_i64>
  406ccc:	bf 13 82 40 00       	mov    $0x408213,%edi
  406cd1:	be 15 00 00 00       	mov    $0x15,%esi
  406cd6:	e8 f5 d1 ff ff       	call   403ed0 <runtime::print_string>
  406cdb:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406ce0:	e8 4b d8 ff ff       	call   404530 <runtime::print_i64>
  406ce5:	bf 0a 00 00 00       	mov    $0xa,%edi
  406cea:	e8 51 d4 ff ff       	call   404140 <runtime::print_byte>
  406cef:	e8 2c ac ff ff       	call   401920 <runtime::bounds_trap>
  406cf4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  406cfb:	00 00 00 00 00 

0000000000406d00 <runtime::default_random_generator_proc.read_u64-0>:
  406d00:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  406d05:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406d0a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406d0f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406d14:	48 8b 00             	mov    (%rax),%rax
  406d17:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406d1c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406d21:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  406d28:	f4 51 58 
  406d2b:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  406d31:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406d36:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  406d3a:	48 83 ca 01          	or     $0x1,%rdx
  406d3e:	48 01 d1             	add    %rdx,%rcx
  406d41:	48 89 08             	mov    %rcx,(%rax)
  406d44:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406d49:	48 c1 e9 3b          	shr    $0x3b,%rcx
  406d4d:	b2 01                	mov    $0x1,%dl
  406d4f:	31 c0                	xor    %eax,%eax
  406d51:	f6 c2 01             	test   $0x1,%dl
  406d54:	48 0f 45 c1          	cmovne %rcx,%rax
  406d58:	48 83 c0 05          	add    $0x5,%rax
  406d5c:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  406d61:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  406d68:	75 f1 ae 
  406d6b:	48 0f af c1          	imul   %rcx,%rax
  406d6f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406d74:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406d79:	48 c1 e9 3b          	shr    $0x3b,%rcx
  406d7d:	b2 01                	mov    $0x1,%dl
  406d7f:	31 c0                	xor    %eax,%eax
  406d81:	f6 c2 01             	test   $0x1,%dl
  406d84:	48 0f 45 c1          	cmovne %rcx,%rax
  406d88:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406d8d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406d92:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  406d97:	48 89 d1             	mov    %rdx,%rcx
  406d9a:	48 d3 e8             	shr    %cl,%rax
  406d9d:	48 89 c1             	mov    %rax,%rcx
  406da0:	31 c0                	xor    %eax,%eax
  406da2:	48 83 fa 40          	cmp    $0x40,%rdx
  406da6:	48 0f 42 c1          	cmovb  %rcx,%rax
  406daa:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  406daf:	31 c9                	xor    %ecx,%ecx
  406db1:	89 ce                	mov    %ecx,%esi
  406db3:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  406db8:	48 83 e6 3f          	and    $0x3f,%rsi
  406dbc:	48 89 f1             	mov    %rsi,%rcx
  406dbf:	48 d3 e2             	shl    %cl,%rdx
  406dc2:	31 c9                	xor    %ecx,%ecx
  406dc4:	48 83 fe 40          	cmp    $0x40,%rsi
  406dc8:	48 0f 42 ca          	cmovb  %rdx,%rcx
  406dcc:	48 09 c8             	or     %rcx,%rax
  406dcf:	c3                   	ret

0000000000406dd0 <runtime::default_random_generator_proc.init-1>:
  406dd0:	48 83 ec 28          	sub    $0x28,%rsp
  406dd4:	48 89 3c 24          	mov    %rdi,(%rsp)
  406dd8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  406ddd:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406de2:	48 8b 0c 24          	mov    (%rsp),%rcx
  406de6:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  406deb:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  406df0:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406df5:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  406dfb:	0f 94 c0             	sete   %al
  406dfe:	24 01                	and    $0x1,%al
  406e00:	3c 00                	cmp    $0x0,%al
  406e02:	74 0e                	je     406e12 <runtime::default_random_generator_proc.init-1+0x42>
  406e04:	0f 31                	rdtsc
  406e06:	48 c1 e2 20          	shl    $0x20,%rdx
  406e0a:	48 09 d0             	or     %rdx,%rax
  406e0d:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406e12:	48 8b 3c 24          	mov    (%rsp),%rdi
  406e16:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406e1b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  406e22:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406e27:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  406e2c:	48 d1 e2             	shl    $1,%rdx
  406e2f:	40 b6 01             	mov    $0x1,%sil
  406e32:	31 c9                	xor    %ecx,%ecx
  406e34:	40 f6 c6 01          	test   $0x1,%sil
  406e38:	48 0f 45 ca          	cmovne %rdx,%rcx
  406e3c:	48 83 c9 01          	or     $0x1,%rcx
  406e40:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406e44:	e8 b7 fe ff ff       	call   406d00 <runtime::default_random_generator_proc.read_u64-0>
  406e49:	48 8b 3c 24          	mov    (%rsp),%rdi
  406e4d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406e52:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406e57:	48 03 08             	add    (%rax),%rcx
  406e5a:	48 89 08             	mov    %rcx,(%rax)
  406e5d:	e8 9e fe ff ff       	call   406d00 <runtime::default_random_generator_proc.read_u64-0>
  406e62:	48 83 c4 28          	add    $0x28,%rsp
  406e66:	c3                   	ret
  406e67:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  406e6e:	00 00 

0000000000406e70 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  406e70:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  406e75:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  406e7a:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406e7f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  406e84:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  406e89:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406e8e:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  406e95:	00 00 
  406e97:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  406e9c:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  406ea0:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406ea5:	48 03 4a 20          	add    0x20(%rdx),%rcx
  406ea9:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  406eae:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406eb3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406eb8:	48 83 e8 01          	sub    $0x1,%rax
  406ebc:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  406ec1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406ec6:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  406ecb:	48 83 f8 00          	cmp    $0x0,%rax
  406ecf:	0f 95 c0             	setne  %al
  406ed2:	24 01                	and    $0x1,%al
  406ed4:	3c 00                	cmp    $0x0,%al
  406ed6:	74 17                	je     406eef <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  406ed8:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406edd:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406ee2:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  406ee7:	48 29 c8             	sub    %rcx,%rax
  406eea:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406eef:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406ef4:	c3                   	ret
  406ef5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  406efc:	00 00 00 00 

0000000000406f00 <runtime::arena_alloc.align_forward_uint-0>:
  406f00:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  406f05:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  406f0a:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406f0f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406f14:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406f19:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  406f1e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406f23:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406f28:	48 83 e9 01          	sub    $0x1,%rcx
  406f2c:	48 21 c8             	and    %rcx,%rax
  406f2f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406f34:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  406f3a:	0f 95 c0             	setne  %al
  406f3d:	24 01                	and    $0x1,%al
  406f3f:	3c 00                	cmp    $0x0,%al
  406f41:	74 14                	je     406f57 <runtime::arena_alloc.align_forward_uint-0+0x57>
  406f43:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406f48:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  406f4d:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  406f52:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406f57:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406f5c:	c3                   	ret
  406f5d:	0f 1f 00             	nopl   (%rax)

0000000000406f60 <runtime::matrix_bounds_check_error.handle_error-0>:
  406f60:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  406f67:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406f6c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406f71:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406f75:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406f79:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  406f7e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406f83:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  406f8a:	00 
  406f8b:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406f90:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  406f97:	00 
  406f98:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406f9d:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406fa2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  406fa7:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  406fac:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  406fb1:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406fb5:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406fb9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406fbe:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406fc3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  406fca:	00 
  406fcb:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  406fd2:	00 
  406fd3:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  406fda:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  406fe1:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  406fe8:	00 
  406fe9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  406ff0:	00 
  406ff1:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  406ff6:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  406ffb:	0f 57 c0             	xorps  %xmm0,%xmm0
  406ffe:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  407003:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  407008:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40700f:	00 00 
  407011:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  407016:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40701b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  407022:	00 00 
  407024:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  407029:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40702e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  407032:	89 44 24 54          	mov    %eax,0x54(%rsp)
  407036:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  40703b:	e8 f0 d6 ff ff       	call   404730 <runtime::print_caller_location>
  407040:	bf d0 82 40 00       	mov    $0x4082d0,%edi
  407045:	be 11 00 00 00       	mov    $0x11,%esi
  40704a:	e8 81 ce ff ff       	call   403ed0 <runtime::print_string>
  40704f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  407054:	e8 d7 d4 ff ff       	call   404530 <runtime::print_i64>
  407059:	bf e2 82 40 00       	mov    $0x4082e2,%edi
  40705e:	be 02 00 00 00       	mov    $0x2,%esi
  407063:	e8 68 ce ff ff       	call   403ed0 <runtime::print_string>
  407068:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40706d:	e8 be d4 ff ff       	call   404530 <runtime::print_i64>
  407072:	bf e5 82 40 00       	mov    $0x4082e5,%edi
  407077:	be 16 00 00 00       	mov    $0x16,%esi
  40707c:	e8 4f ce ff ff       	call   403ed0 <runtime::print_string>
  407081:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  407086:	e8 a5 d4 ff ff       	call   404530 <runtime::print_i64>
  40708b:	bf fc 82 40 00       	mov    $0x4082fc,%edi
  407090:	be 06 00 00 00       	mov    $0x6,%esi
  407095:	e8 36 ce ff ff       	call   403ed0 <runtime::print_string>
  40709a:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40709f:	e8 8c d4 ff ff       	call   404530 <runtime::print_i64>
  4070a4:	bf 03 83 40 00       	mov    $0x408303,%edi
  4070a9:	be 01 00 00 00       	mov    $0x1,%esi
  4070ae:	e8 1d ce ff ff       	call   403ed0 <runtime::print_string>
  4070b3:	bf 0a 00 00 00       	mov    $0xa,%edi
  4070b8:	e8 83 d0 ff ff       	call   404140 <runtime::print_byte>
  4070bd:	e8 5e a8 ff ff       	call   401920 <runtime::bounds_trap>
  4070c2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4070c9:	1f 84 00 00 00 00 00 

00000000004070d0 <runtime::assert.internal-0>:
  4070d0:	48 83 ec 38          	sub    $0x38,%rsp
  4070d4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4070d8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4070dd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4070e2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4070e7:	48 8b 04 24          	mov    (%rsp),%rax
  4070eb:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4070f0:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4070f5:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4070fa:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4070ff:	48 8b 40 20          	mov    0x20(%rax),%rax
  407103:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407108:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  40710e:	0f 94 c0             	sete   %al
  407111:	24 01                	and    $0x1,%al
  407113:	3c 00                	cmp    $0x0,%al
  407115:	74 0c                	je     407123 <runtime::assert.internal-0+0x53>
  407117:	48 c7 c0 c0 59 40 00 	mov    $0x4059c0,%rax
  40711e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407123:	4c 8b 0c 24          	mov    (%rsp),%r9
  407127:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40712c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  407131:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  407136:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40713b:	bf 05 83 40 00       	mov    $0x408305,%edi
  407140:	be 11 00 00 00       	mov    $0x11,%esi
  407145:	ff d0                	call   *%rax
  407147:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40714e:	00 00 

0000000000407150 <__$startup_runtime>:
  407150:	eb 00                	jmp    407152 <__$startup_runtime+0x2>
  407152:	c3                   	ret
  407153:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40715a:	84 00 00 00 00 00 

0000000000407160 <__$cleanup_runtime>:
  407160:	50                   	push   %rax
  407161:	48 89 3c 24          	mov    %rdi,(%rsp)
  407165:	eb 00                	jmp    407167 <__$cleanup_runtime+0x7>
  407167:	48 8b 3c 24          	mov    (%rsp),%rdi
  40716b:	e8 80 a7 ff ff       	call   4018f0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  407170:	58                   	pop    %rax
  407171:	c3                   	ret

Disassembly of section .fini:

0000000000407174 <_fini>:
  407174:	f3 0f 1e fa          	endbr64
  407178:	48 83 ec 08          	sub    $0x8,%rsp
  40717c:	48 83 c4 08          	add    $0x8,%rsp
  407180:	c3                   	ret
