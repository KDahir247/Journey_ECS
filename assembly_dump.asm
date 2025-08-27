
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
  4010b8:	48 c7 c7 10 2e 40 00 	mov    $0x402e10,%rdi
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
  401190:	48 83 ec 58          	sub    $0x58,%rsp
  401194:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  401199:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40119e:	e8 a3 00 00 00       	call   401246 <journey::create_world:proc(indice_bit_capacity:$$900,unique_data_capacity:$$30)->(:journey::World)>
  4011a3:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011a8:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4011ad:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4011b2:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4011b7:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4011bc:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  4011c1:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4011c6:	48 8d 7c 24 48       	lea    0x48(%rsp),%rdi
  4011cb:	48 89 3c 24          	mov    %rdi,(%rsp)
  4011cf:	e8 11 01 00 00       	call   4012e5 <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Health::$1,data_storage_index:$$0,indices_capacity:$$100)>
  4011d4:	48 8b 3c 24          	mov    (%rsp),%rdi
  4011d8:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011dd:	e8 78 01 00 00       	call   40135a <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)>
  4011e2:	48 8b 3c 24          	mov    (%rsp),%rdi
  4011e6:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011eb:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4011f0:	e8 3f 02 00 00       	call   401434 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)>
  4011f5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4011fa:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4011ff:	0f 57 c0             	xorps  %xmm0,%xmm0
  401202:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  401207:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  40120e:	00 00 
  401210:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  401217:	00 00 
  401219:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40121e:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401223:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  401228:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40122d:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401232:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401237:	48 8d 7c 24 48       	lea    0x48(%rsp),%rdi
  40123c:	e8 cd 02 00 00       	call   40150e <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)>
  401241:	48 83 c4 58          	add    $0x58,%rsp
  401245:	c3                   	ret

0000000000401246 <journey::create_world:proc(indice_bit_capacity:$$900,unique_data_capacity:$$30)->(:journey::World)>:
  401246:	31 c0                	xor    %eax,%eax
  401248:	41 89 c1             	mov    %eax,%r9d
  40124b:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  401250:	b8 09 00 00 00       	mov    $0x9,%eax
  401255:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40125a:	be 00 10 00 00       	mov    $0x1000,%esi
  40125f:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  401264:	ba 03 00 00 00       	mov    $0x3,%edx
  401269:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  40126e:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401274:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  401279:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401280:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  401285:	4c 89 cf             	mov    %r9,%rdi
  401288:	0f 05                	syscall
  40128a:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  40128f:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  401294:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  401299:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40129e:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  4012a3:	48 89 c1             	mov    %rax,%rcx
  4012a6:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4012ab:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4012b0:	4c 89 cf             	mov    %r9,%rdi
  4012b3:	0f 05                	syscall
  4012b5:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4012ba:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4012bf:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  4012c6:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4012cb:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4012d0:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4012d5:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4012da:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4012df:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4012e4:	c3                   	ret

00000000004012e5 <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Health::$1,data_storage_index:$$0,indices_capacity:$$100)>:
  4012e5:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  4012ea:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4012ef:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4012f4:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4012f9:	48 8b 00             	mov    (%rax),%rax
  4012fc:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401301:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401306:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40130b:	31 c0                	xor    %eax,%eax
  40130d:	41 89 c1             	mov    %eax,%r9d
  401310:	b8 09 00 00 00       	mov    $0x9,%eax
  401315:	be 00 10 00 00       	mov    $0x1000,%esi
  40131a:	ba 03 00 00 00       	mov    $0x3,%edx
  40131f:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401325:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  40132c:	4c 89 cf             	mov    %r9,%rdi
  40132f:	0f 05                	syscall
  401331:	48 89 c1             	mov    %rax,%rcx
  401334:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401339:	48 89 08             	mov    %rcx,(%rax)
  40133c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401341:	48 c7 40 18 08 00 00 	movq   $0x8,0x18(%rax)
  401348:	00 
  401349:	48 c7 40 10 c8 00 00 	movq   $0xc8,0x10(%rax)
  401350:	00 
  401351:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401358:	00 
  401359:	c3                   	ret

000000000040135a <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)>:
  40135a:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  40135f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401364:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401369:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40136e:	48 8b 40 08          	mov    0x8(%rax),%rax
  401372:	48 8b 00             	mov    (%rax),%rax
  401375:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40137a:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40137f:	48 8b 40 08          	mov    0x8(%rax),%rax
  401383:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401388:	48 c1 e9 06          	shr    $0x6,%rcx
  40138c:	48 c1 e1 03          	shl    $0x3,%rcx
  401390:	48 01 c8             	add    %rcx,%rax
  401393:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401398:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40139d:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4013a2:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4013a7:	48 83 c1 14          	add    $0x14,%rcx
  4013ab:	48 83 e1 3f          	and    $0x3f,%rcx
  4013af:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4013b6:	c4 e2 f0 f5 c0       	bzhi   %rcx,%rax,%rax
  4013bb:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4013c0:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4013c5:	48 c7 00 ff ff ff ff 	movq   $0xffffffffffffffff,(%rax)
  4013cc:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4013d1:	48 83 c0 14          	add    $0x14,%rax
  4013d5:	48 c1 e8 06          	shr    $0x6,%rax
  4013d9:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4013de:	48 83 c1 00          	add    $0x0,%rcx
  4013e2:	48 c1 e9 06          	shr    $0x6,%rcx
  4013e6:	48 29 c8             	sub    %rcx,%rax
  4013e9:	48 83 f8 00          	cmp    $0x0,%rax
  4013ed:	75 0d                	jne    4013fc <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)+0xa2>
  4013ef:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4013f4:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4013f9:	48 89 08             	mov    %rcx,(%rax)
  4013fc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401401:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401406:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40140a:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40140f:	48 8b 40 08          	mov    0x8(%rax),%rax
  401413:	48 8b 08             	mov    (%rax),%rcx
  401416:	48 83 c1 14          	add    $0x14,%rcx
  40141a:	48 89 08             	mov    %rcx,(%rax)
  40141d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  401422:	48 83 e8 40          	sub    $0x40,%rax
  401426:	48 b9 00 00 00 00 14 	movabs $0x1400000000,%rcx
  40142d:	00 00 00 
  401430:	48 09 c8             	or     %rcx,%rax
  401433:	c3                   	ret

0000000000401434 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)>:
  401434:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  401439:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40143e:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401443:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401448:	48 8b 40 08          	mov    0x8(%rax),%rax
  40144c:	48 8b 00             	mov    (%rax),%rax
  40144f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401454:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401459:	48 8b 40 08          	mov    0x8(%rax),%rax
  40145d:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401462:	48 c1 e9 06          	shr    $0x6,%rcx
  401466:	48 c1 e1 03          	shl    $0x3,%rcx
  40146a:	48 01 c8             	add    %rcx,%rax
  40146d:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401472:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401477:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40147c:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401481:	48 83 c1 32          	add    $0x32,%rcx
  401485:	48 83 e1 3f          	and    $0x3f,%rcx
  401489:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  401490:	c4 e2 f0 f5 c0       	bzhi   %rcx,%rax,%rax
  401495:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40149a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40149f:	48 c7 00 ff ff ff ff 	movq   $0xffffffffffffffff,(%rax)
  4014a6:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4014ab:	48 83 c0 32          	add    $0x32,%rax
  4014af:	48 c1 e8 06          	shr    $0x6,%rax
  4014b3:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4014b8:	48 83 c1 00          	add    $0x0,%rcx
  4014bc:	48 c1 e9 06          	shr    $0x6,%rcx
  4014c0:	48 29 c8             	sub    %rcx,%rax
  4014c3:	48 83 f8 00          	cmp    $0x0,%rax
  4014c7:	75 0d                	jne    4014d6 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)+0xa2>
  4014c9:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4014ce:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4014d3:	48 89 08             	mov    %rcx,(%rax)
  4014d6:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4014db:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4014e0:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4014e4:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4014e9:	48 8b 40 08          	mov    0x8(%rax),%rax
  4014ed:	48 8b 08             	mov    (%rax),%rcx
  4014f0:	48 83 c1 32          	add    $0x32,%rcx
  4014f4:	48 89 08             	mov    %rcx,(%rax)
  4014f7:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4014fc:	48 83 e8 40          	sub    $0x40,%rax
  401500:	48 b9 00 00 00 00 32 	movabs $0x3200000000,%rcx
  401507:	00 00 00 
  40150a:	48 09 c8             	or     %rcx,%rax
  40150d:	c3                   	ret

000000000040150e <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)>:
  40150e:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  401513:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401518:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40151d:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  401522:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  401527:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  40152c:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  401531:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401536:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40153b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401540:	48 8b 00             	mov    (%rax),%rax
  401543:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401548:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40154d:	48 8b 00             	mov    (%rax),%rax
  401550:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401555:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  401559:	48 c1 e1 04          	shl    $0x4,%rcx
  40155d:	48 01 c8             	add    %rcx,%rax
  401560:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401565:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  40156c:	00 00 
  40156e:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401573:	48 83 e8 02          	sub    $0x2,%rax
  401577:	48 83 f8 00          	cmp    $0x0,%rax
  40157b:	74 4e                	je     4015cb <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)+0xbd>
  40157d:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401582:	48 8b 44 c4 e8       	mov    -0x18(%rsp,%rax,8),%rax
  401587:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40158c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401591:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  401596:	89 c9                	mov    %ecx,%ecx
  401598:	48 89 08             	mov    %rcx,(%rax)
  40159b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4015a0:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4015a5:	48 c1 e9 20          	shr    $0x20,%rcx
  4015a9:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4015ad:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4015b2:	48 83 c0 10          	add    $0x10,%rax
  4015b6:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4015bb:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4015c0:	48 83 c0 01          	add    $0x1,%rax
  4015c4:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4015c9:	eb a3                	jmp    40156e <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)+0x60>
  4015cb:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4015d0:	48 8b 48 08          	mov    0x8(%rax),%rcx
  4015d4:	48 83 c1 02          	add    $0x2,%rcx
  4015d8:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4015dc:	c3                   	ret
  4015dd:	0f 1f 00             	nopl   (%rax)

00000000004015e0 <__$startup_runtime>:
  4015e0:	eb 00                	jmp    4015e2 <__$startup_runtime+0x2>
  4015e2:	c3                   	ret
  4015e3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4015ea:	84 00 00 00 00 00 

00000000004015f0 <__$cleanup_runtime>:
  4015f0:	50                   	push   %rax
  4015f1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4015f5:	eb 00                	jmp    4015f7 <__$cleanup_runtime+0x7>
  4015f7:	48 8b 3c 24          	mov    (%rsp),%rdi
  4015fb:	e8 10 00 00 00       	call   401610 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  401600:	58                   	pop    %rax
  401601:	c3                   	ret
  401602:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  401609:	00 00 00 
  40160c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401610 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  401610:	50                   	push   %rax
  401611:	48 89 3c 24          	mov    %rdi,(%rsp)
  401615:	eb 00                	jmp    401617 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x7>
  401617:	48 8b 34 24          	mov    (%rsp),%rsi
  40161b:	48 c7 c0 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rax
  401622:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  401629:	00 00 
  40162b:	48 01 c7             	add    %rax,%rdi
  40162e:	e8 8d 11 00 00       	call   4027c0 <runtime::default_temp_allocator_destroy>
  401633:	58                   	pop    %rax
  401634:	c3                   	ret
  401635:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40163c:	00 00 00 00 

0000000000401640 <runtime::bounds_trap>:
  401640:	eb 00                	jmp    401642 <runtime::bounds_trap+0x2>
  401642:	0f 0b                	ud2
  401644:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40164b:	00 00 00 00 00 

0000000000401650 <runtime::heap_allocator>:
  401650:	48 c7 c0 f0 20 40 00 	mov    $0x4020f0,%rax
  401657:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40165c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401663:	00 00 
  401665:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40166a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40166f:	c3                   	ret

0000000000401670 <runtime::udivmod128>:
  401670:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  401677:	4c 89 44 24 a8       	mov    %r8,-0x58(%rsp)
  40167c:	48 89 4c 24 b0       	mov    %rcx,-0x50(%rsp)
  401681:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401686:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40168b:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  401690:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401695:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40169a:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40169f:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  4016a4:	48 8b 7c 24 a8       	mov    -0x58(%rsp),%rdi
  4016a9:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4016b0:	00 
  4016b1:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4016b8:	00 
  4016b9:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  4016c0:	00 
  4016c1:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4016c8:	00 
  4016c9:	48 89 bc 24 88 00 00 	mov    %rdi,0x88(%rsp)
  4016d0:	00 
  4016d1:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4016d6:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  4016db:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4016e0:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4016e5:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4016ea:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  4016ef:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4016f4:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4016f9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4016fe:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  401703:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  401708:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40170d:	0f 57 c0             	xorps  %xmm0,%xmm0
  401710:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401715:	0f 57 c0             	xorps  %xmm0,%xmm0
  401718:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  40171d:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  401724:	00 
  401725:	48 83 7c 24 68 00    	cmpq   $0x0,0x68(%rsp)
  40172b:	0f 94 c0             	sete   %al
  40172e:	24 01                	and    $0x1,%al
  401730:	3c 00                	cmp    $0x0,%al
  401732:	0f 84 a1 00 00 00    	je     4017d9 <runtime::udivmod128+0x169>
  401738:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  40173e:	0f 94 c0             	sete   %al
  401741:	24 01                	and    $0x1,%al
  401743:	3c 00                	cmp    $0x0,%al
  401745:	74 5c                	je     4017a3 <runtime::udivmod128+0x133>
  401747:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40174c:	48 83 f8 00          	cmp    $0x0,%rax
  401750:	0f 95 c0             	setne  %al
  401753:	24 01                	and    $0x1,%al
  401755:	3c 00                	cmp    $0x0,%al
  401757:	74 29                	je     401782 <runtime::udivmod128+0x112>
  401759:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40175e:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401763:	31 d2                	xor    %edx,%edx
  401765:	48 f7 f1             	div    %rcx
  401768:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40176d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  401772:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401777:	48 89 08             	mov    %rcx,(%rax)
  40177a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401781:	00 
  401782:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401787:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40178c:	31 d2                	xor    %edx,%edx
  40178e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  401793:	48 f7 f1             	div    %rcx
  401796:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  40179b:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4017a2:	c3                   	ret
  4017a3:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4017a8:	48 83 f8 00          	cmp    $0x0,%rax
  4017ac:	0f 95 c0             	setne  %al
  4017af:	24 01                	and    $0x1,%al
  4017b1:	3c 00                	cmp    $0x0,%al
  4017b3:	74 15                	je     4017ca <runtime::udivmod128+0x15a>
  4017b5:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4017ba:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4017bf:	48 89 08             	mov    %rcx,(%rax)
  4017c2:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4017c9:	00 
  4017ca:	31 c0                	xor    %eax,%eax
  4017cc:	89 c2                	mov    %eax,%edx
  4017ce:	48 89 d0             	mov    %rdx,%rax
  4017d1:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4017d8:	c3                   	ret
  4017d9:	48 83 7c 24 40 00    	cmpq   $0x0,0x40(%rsp)
  4017df:	0f 94 c0             	sete   %al
  4017e2:	24 01                	and    $0x1,%al
  4017e4:	3c 00                	cmp    $0x0,%al
  4017e6:	0f 84 8b 02 00 00    	je     401a77 <runtime::udivmod128+0x407>
  4017ec:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  4017f2:	0f 94 c0             	sete   %al
  4017f5:	24 01                	and    $0x1,%al
  4017f7:	3c 00                	cmp    $0x0,%al
  4017f9:	74 52                	je     40184d <runtime::udivmod128+0x1dd>
  4017fb:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401800:	48 83 f8 00          	cmp    $0x0,%rax
  401804:	0f 95 c0             	setne  %al
  401807:	24 01                	and    $0x1,%al
  401809:	3c 00                	cmp    $0x0,%al
  40180b:	74 1f                	je     40182c <runtime::udivmod128+0x1bc>
  40180d:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401812:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401817:	31 d2                	xor    %edx,%edx
  401819:	48 f7 f1             	div    %rcx
  40181c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401821:	48 89 10             	mov    %rdx,(%rax)
  401824:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40182b:	00 
  40182c:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401831:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401836:	31 d2                	xor    %edx,%edx
  401838:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  40183d:	48 f7 f1             	div    %rcx
  401840:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  401845:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40184c:	c3                   	ret
  40184d:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  401853:	0f 94 c0             	sete   %al
  401856:	24 01                	and    $0x1,%al
  401858:	3c 00                	cmp    $0x0,%al
  40185a:	74 66                	je     4018c2 <runtime::udivmod128+0x252>
  40185c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401861:	48 83 f8 00          	cmp    $0x0,%rax
  401865:	0f 95 c0             	setne  %al
  401868:	24 01                	and    $0x1,%al
  40186a:	3c 00                	cmp    $0x0,%al
  40186c:	74 33                	je     4018a1 <runtime::udivmod128+0x231>
  40186e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401873:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401878:	31 d2                	xor    %edx,%edx
  40187a:	48 f7 f1             	div    %rcx
  40187d:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401882:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401887:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  40188e:	00 00 
  401890:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401895:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40189a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40189e:	48 89 08             	mov    %rcx,(%rax)
  4018a1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4018a6:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4018ab:	31 d2                	xor    %edx,%edx
  4018ad:	48 89 54 24 90       	mov    %rdx,-0x70(%rsp)
  4018b2:	48 f7 f1             	div    %rcx
  4018b5:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  4018ba:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4018c1:	c3                   	ret
  4018c2:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4018c7:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4018cc:	48 83 e9 01          	sub    $0x1,%rcx
  4018d0:	48 21 c8             	and    %rcx,%rax
  4018d3:	48 83 f8 00          	cmp    $0x0,%rax
  4018d7:	0f 94 c0             	sete   %al
  4018da:	24 01                	and    $0x1,%al
  4018dc:	3c 00                	cmp    $0x0,%al
  4018de:	74 7a                	je     40195a <runtime::udivmod128+0x2ea>
  4018e0:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4018e5:	48 83 f8 00          	cmp    $0x0,%rax
  4018e9:	0f 95 c0             	setne  %al
  4018ec:	24 01                	and    $0x1,%al
  4018ee:	3c 00                	cmp    $0x0,%al
  4018f0:	74 35                	je     401927 <runtime::udivmod128+0x2b7>
  4018f2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4018f7:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4018fc:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401901:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  401906:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40190b:	48 ff ca             	dec    %rdx
  40190e:	48 21 d1             	and    %rdx,%rcx
  401911:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401916:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40191b:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401920:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401924:	48 89 08             	mov    %rcx,(%rax)
  401927:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40192c:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401931:	ba 40 00 00 00       	mov    $0x40,%edx
  401936:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  40193b:	88 d1                	mov    %dl,%cl
  40193d:	48 d3 e8             	shr    %cl,%rax
  401940:	48 89 c1             	mov    %rax,%rcx
  401943:	31 c0                	xor    %eax,%eax
  401945:	48 83 ea 40          	sub    $0x40,%rdx
  401949:	89 c2                	mov    %eax,%edx
  40194b:	48 89 d0             	mov    %rdx,%rax
  40194e:	48 0f 42 c1          	cmovb  %rcx,%rax
  401952:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401959:	c3                   	ret
  40195a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40195f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401964:	48 0f bd c1          	bsr    %rcx,%rax
  401968:	48 83 f0 3f          	xor    $0x3f,%rax
  40196c:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401971:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401976:	48 0f bd ca          	bsr    %rdx,%rcx
  40197a:	48 83 f1 3f          	xor    $0x3f,%rcx
  40197e:	29 c8                	sub    %ecx,%eax
  401980:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401984:	83 7c 24 1c 3e       	cmpl   $0x3e,0x1c(%rsp)
  401989:	0f 97 c0             	seta   %al
  40198c:	24 01                	and    $0x1,%al
  40198e:	3c 00                	cmp    $0x0,%al
  401990:	74 37                	je     4019c9 <runtime::udivmod128+0x359>
  401992:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401997:	48 83 f8 00          	cmp    $0x0,%rax
  40199b:	0f 95 c0             	setne  %al
  40199e:	24 01                	and    $0x1,%al
  4019a0:	3c 00                	cmp    $0x0,%al
  4019a2:	74 16                	je     4019ba <runtime::udivmod128+0x34a>
  4019a4:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4019a9:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4019ae:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4019b3:	48 89 10             	mov    %rdx,(%rax)
  4019b6:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4019ba:	31 c0                	xor    %eax,%eax
  4019bc:	89 c2                	mov    %eax,%edx
  4019be:	48 89 d0             	mov    %rdx,%rax
  4019c1:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4019c8:	c3                   	ret
  4019c9:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  4019cd:	83 c0 01             	add    $0x1,%eax
  4019d0:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4019d4:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4019db:	00 00 
  4019dd:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4019e2:	b9 40 00 00 00       	mov    $0x40,%ecx
  4019e7:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  4019eb:	89 c9                	mov    %ecx,%ecx
  4019ed:	89 ca                	mov    %ecx,%edx
  4019ef:	48 89 d1             	mov    %rdx,%rcx
  4019f2:	48 d3 e0             	shl    %cl,%rax
  4019f5:	48 89 c1             	mov    %rax,%rcx
  4019f8:	31 c0                	xor    %eax,%eax
  4019fa:	48 83 fa 40          	cmp    $0x40,%rdx
  4019fe:	48 0f 42 c1          	cmovb  %rcx,%rax
  401a02:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401a07:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401a0c:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401a10:	89 ca                	mov    %ecx,%edx
  401a12:	48 89 d1             	mov    %rdx,%rcx
  401a15:	48 d3 e8             	shr    %cl,%rax
  401a18:	48 89 c1             	mov    %rax,%rcx
  401a1b:	31 c0                	xor    %eax,%eax
  401a1d:	48 83 fa 40          	cmp    $0x40,%rdx
  401a21:	48 0f 42 c1          	cmovb  %rcx,%rax
  401a25:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401a2a:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401a2f:	b9 40 00 00 00       	mov    $0x40,%ecx
  401a34:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401a38:	89 c9                	mov    %ecx,%ecx
  401a3a:	89 ca                	mov    %ecx,%edx
  401a3c:	48 89 d1             	mov    %rdx,%rcx
  401a3f:	48 d3 e0             	shl    %cl,%rax
  401a42:	48 89 c1             	mov    %rax,%rcx
  401a45:	31 c0                	xor    %eax,%eax
  401a47:	48 83 fa 40          	cmp    $0x40,%rdx
  401a4b:	48 0f 42 c1          	cmovb  %rcx,%rax
  401a4f:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401a54:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401a58:	89 ce                	mov    %ecx,%esi
  401a5a:	48 89 f1             	mov    %rsi,%rcx
  401a5d:	48 d3 ea             	shr    %cl,%rdx
  401a60:	31 c9                	xor    %ecx,%ecx
  401a62:	48 83 fe 40          	cmp    $0x40,%rsi
  401a66:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401a6a:	48 09 c8             	or     %rcx,%rax
  401a6d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401a72:	e9 30 04 00 00       	jmp    401ea7 <runtime::udivmod128+0x837>
  401a77:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401a7d:	0f 94 c0             	sete   %al
  401a80:	24 01                	and    $0x1,%al
  401a82:	3c 00                	cmp    $0x0,%al
  401a84:	0f 84 d1 02 00 00    	je     401d5b <runtime::udivmod128+0x6eb>
  401a8a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401a8f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401a94:	48 83 e9 01          	sub    $0x1,%rcx
  401a98:	48 21 c8             	and    %rcx,%rax
  401a9b:	48 83 f8 00          	cmp    $0x0,%rax
  401a9f:	0f 94 c0             	sete   %al
  401aa2:	24 01                	and    $0x1,%al
  401aa4:	3c 00                	cmp    $0x0,%al
  401aa6:	0f 84 de 00 00 00    	je     401b8a <runtime::udivmod128+0x51a>
  401aac:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401ab1:	48 83 f8 00          	cmp    $0x0,%rax
  401ab5:	0f 95 c0             	setne  %al
  401ab8:	24 01                	and    $0x1,%al
  401aba:	3c 00                	cmp    $0x0,%al
  401abc:	74 20                	je     401ade <runtime::udivmod128+0x46e>
  401abe:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401ac3:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401ac8:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401acd:	48 ff ca             	dec    %rdx
  401ad0:	48 21 d1             	and    %rdx,%rcx
  401ad3:	48 89 08             	mov    %rcx,(%rax)
  401ad6:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401add:	00 
  401ade:	48 83 7c 24 40 01    	cmpq   $0x1,0x40(%rsp)
  401ae4:	0f 94 c0             	sete   %al
  401ae7:	24 01                	and    $0x1,%al
  401ae9:	3c 00                	cmp    $0x0,%al
  401aeb:	74 12                	je     401aff <runtime::udivmod128+0x48f>
  401aed:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401af2:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401af7:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401afe:	c3                   	ret
  401aff:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401b04:	b8 40 00 00 00       	mov    $0x40,%eax
  401b09:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  401b0e:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401b12:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401b17:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401b1b:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  401b1f:	88 c1                	mov    %al,%cl
  401b21:	48 d3 ea             	shr    %cl,%rdx
  401b24:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  401b28:	31 c0                	xor    %eax,%eax
  401b2a:	83 e9 40             	sub    $0x40,%ecx
  401b2d:	48 89 c1             	mov    %rax,%rcx
  401b30:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401b34:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  401b39:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  401b3e:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401b43:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  401b47:	40 88 f1             	mov    %sil,%cl
  401b4a:	48 d3 ef             	shr    %cl,%rdi
  401b4d:	83 ee 40             	sub    $0x40,%esi
  401b50:	48 89 c1             	mov    %rax,%rcx
  401b53:	48 0f 42 cf          	cmovb  %rdi,%rcx
  401b57:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  401b5c:	f7 de                	neg    %esi
  401b5e:	40 88 f1             	mov    %sil,%cl
  401b61:	48 d3 e2             	shl    %cl,%rdx
  401b64:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  401b69:	83 ee 40             	sub    $0x40,%esi
  401b6c:	48 0f 42 c2          	cmovb  %rdx,%rax
  401b70:	48 09 c8             	or     %rcx,%rax
  401b73:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401b78:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401b7d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401b82:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401b89:	c3                   	ret
  401b8a:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401b8f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401b94:	48 0f bd c1          	bsr    %rcx,%rax
  401b98:	48 83 f0 3f          	xor    $0x3f,%rax
  401b9c:	83 c0 41             	add    $0x41,%eax
  401b9f:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401ba4:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401ba9:	48 0f bd ca          	bsr    %rdx,%rcx
  401bad:	48 83 f1 3f          	xor    $0x3f,%rcx
  401bb1:	29 c8                	sub    %ecx,%eax
  401bb3:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401bb7:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401bbc:	0f 94 c1             	sete   %cl
  401bbf:	80 e1 01             	and    $0x1,%cl
  401bc2:	b0 01                	mov    $0x1,%al
  401bc4:	38 c8                	cmp    %cl,%al
  401bc6:	74 13                	je     401bdb <runtime::udivmod128+0x56b>
  401bc8:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401bcd:	0f 92 c1             	setb   %cl
  401bd0:	80 e1 01             	and    $0x1,%cl
  401bd3:	b0 01                	mov    $0x1,%al
  401bd5:	38 c8                	cmp    %cl,%al
  401bd7:	74 32                	je     401c0b <runtime::udivmod128+0x59b>
  401bd9:	eb 2b                	jmp    401c06 <runtime::udivmod128+0x596>
  401bdb:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401be2:	00 00 
  401be4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401be9:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401bee:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401bf5:	00 00 
  401bf7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401bfc:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401c01:	e9 50 01 00 00       	jmp    401d56 <runtime::udivmod128+0x6e6>
  401c06:	e9 a3 00 00 00       	jmp    401cae <runtime::udivmod128+0x63e>
  401c0b:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401c12:	00 00 
  401c14:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401c19:	b9 40 00 00 00       	mov    $0x40,%ecx
  401c1e:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401c22:	89 c9                	mov    %ecx,%ecx
  401c24:	89 ca                	mov    %ecx,%edx
  401c26:	48 89 d1             	mov    %rdx,%rcx
  401c29:	48 d3 e0             	shl    %cl,%rax
  401c2c:	48 89 c1             	mov    %rax,%rcx
  401c2f:	31 c0                	xor    %eax,%eax
  401c31:	48 83 fa 40          	cmp    $0x40,%rdx
  401c35:	48 0f 42 c1          	cmovb  %rcx,%rax
  401c39:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401c3e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401c43:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401c47:	89 ca                	mov    %ecx,%edx
  401c49:	48 89 d1             	mov    %rdx,%rcx
  401c4c:	48 d3 e8             	shr    %cl,%rax
  401c4f:	48 89 c1             	mov    %rax,%rcx
  401c52:	31 c0                	xor    %eax,%eax
  401c54:	48 83 fa 40          	cmp    $0x40,%rdx
  401c58:	48 0f 42 c1          	cmovb  %rcx,%rax
  401c5c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401c61:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401c66:	b9 40 00 00 00       	mov    $0x40,%ecx
  401c6b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401c6f:	89 c9                	mov    %ecx,%ecx
  401c71:	89 ca                	mov    %ecx,%edx
  401c73:	48 89 d1             	mov    %rdx,%rcx
  401c76:	48 d3 e0             	shl    %cl,%rax
  401c79:	48 89 c1             	mov    %rax,%rcx
  401c7c:	31 c0                	xor    %eax,%eax
  401c7e:	48 83 fa 40          	cmp    $0x40,%rdx
  401c82:	48 0f 42 c1          	cmovb  %rcx,%rax
  401c86:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401c8b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401c8f:	89 ce                	mov    %ecx,%esi
  401c91:	48 89 f1             	mov    %rsi,%rcx
  401c94:	48 d3 ea             	shr    %cl,%rdx
  401c97:	31 c9                	xor    %ecx,%ecx
  401c99:	48 83 fe 40          	cmp    $0x40,%rsi
  401c9d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401ca1:	48 09 c8             	or     %rcx,%rax
  401ca4:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401ca9:	e9 a8 00 00 00       	jmp    401d56 <runtime::udivmod128+0x6e6>
  401cae:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401cb3:	b9 80 00 00 00       	mov    $0x80,%ecx
  401cb8:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401cbc:	89 c9                	mov    %ecx,%ecx
  401cbe:	89 ca                	mov    %ecx,%edx
  401cc0:	48 89 d1             	mov    %rdx,%rcx
  401cc3:	48 d3 e0             	shl    %cl,%rax
  401cc6:	48 89 c1             	mov    %rax,%rcx
  401cc9:	31 c0                	xor    %eax,%eax
  401ccb:	48 83 fa 40          	cmp    $0x40,%rdx
  401ccf:	48 0f 42 c1          	cmovb  %rcx,%rax
  401cd3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401cd8:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401cdd:	b9 80 00 00 00       	mov    $0x80,%ecx
  401ce2:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401ce6:	89 c9                	mov    %ecx,%ecx
  401ce8:	89 ca                	mov    %ecx,%edx
  401cea:	48 89 d1             	mov    %rdx,%rcx
  401ced:	48 d3 e0             	shl    %cl,%rax
  401cf0:	48 89 c1             	mov    %rax,%rcx
  401cf3:	31 c0                	xor    %eax,%eax
  401cf5:	48 83 fa 40          	cmp    $0x40,%rdx
  401cf9:	48 0f 42 c1          	cmovb  %rcx,%rax
  401cfd:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401d02:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401d06:	83 e9 40             	sub    $0x40,%ecx
  401d09:	89 c9                	mov    %ecx,%ecx
  401d0b:	89 ce                	mov    %ecx,%esi
  401d0d:	48 89 f1             	mov    %rsi,%rcx
  401d10:	48 d3 ea             	shr    %cl,%rdx
  401d13:	31 c9                	xor    %ecx,%ecx
  401d15:	48 83 fe 40          	cmp    $0x40,%rsi
  401d19:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401d1d:	48 09 c8             	or     %rcx,%rax
  401d20:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401d25:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401d2c:	00 00 
  401d2e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401d33:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401d37:	83 e9 40             	sub    $0x40,%ecx
  401d3a:	89 c9                	mov    %ecx,%ecx
  401d3c:	89 ca                	mov    %ecx,%edx
  401d3e:	48 89 d1             	mov    %rdx,%rcx
  401d41:	48 d3 e8             	shr    %cl,%rax
  401d44:	48 89 c1             	mov    %rax,%rcx
  401d47:	31 c0                	xor    %eax,%eax
  401d49:	48 83 fa 40          	cmp    $0x40,%rdx
  401d4d:	48 0f 42 c1          	cmovb  %rcx,%rax
  401d51:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401d56:	e9 4a 01 00 00       	jmp    401ea5 <runtime::udivmod128+0x835>
  401d5b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401d60:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401d65:	48 0f bd c1          	bsr    %rcx,%rax
  401d69:	48 83 f0 3f          	xor    $0x3f,%rax
  401d6d:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401d72:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401d77:	48 0f bd ca          	bsr    %rdx,%rcx
  401d7b:	48 83 f1 3f          	xor    $0x3f,%rcx
  401d7f:	29 c8                	sub    %ecx,%eax
  401d81:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401d85:	83 7c 24 1c 3f       	cmpl   $0x3f,0x1c(%rsp)
  401d8a:	0f 97 c0             	seta   %al
  401d8d:	24 01                	and    $0x1,%al
  401d8f:	3c 00                	cmp    $0x0,%al
  401d91:	74 37                	je     401dca <runtime::udivmod128+0x75a>
  401d93:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401d98:	48 83 f8 00          	cmp    $0x0,%rax
  401d9c:	0f 95 c0             	setne  %al
  401d9f:	24 01                	and    $0x1,%al
  401da1:	3c 00                	cmp    $0x0,%al
  401da3:	74 16                	je     401dbb <runtime::udivmod128+0x74b>
  401da5:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401daa:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  401daf:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401db4:	48 89 10             	mov    %rdx,(%rax)
  401db7:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401dbb:	31 c0                	xor    %eax,%eax
  401dbd:	89 c2                	mov    %eax,%edx
  401dbf:	48 89 d0             	mov    %rdx,%rax
  401dc2:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401dc9:	c3                   	ret
  401dca:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401dce:	83 c0 01             	add    $0x1,%eax
  401dd1:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401dd5:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401ddc:	00 00 
  401dde:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401de3:	0f 94 c0             	sete   %al
  401de6:	24 01                	and    $0x1,%al
  401de8:	3c 00                	cmp    $0x0,%al
  401dea:	74 22                	je     401e0e <runtime::udivmod128+0x79e>
  401dec:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401df1:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401df6:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401dfd:	00 00 
  401dff:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401e04:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401e09:	e9 95 00 00 00       	jmp    401ea3 <runtime::udivmod128+0x833>
  401e0e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401e13:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401e17:	89 ca                	mov    %ecx,%edx
  401e19:	48 89 d1             	mov    %rdx,%rcx
  401e1c:	48 d3 e8             	shr    %cl,%rax
  401e1f:	48 89 c1             	mov    %rax,%rcx
  401e22:	31 c0                	xor    %eax,%eax
  401e24:	48 83 fa 40          	cmp    $0x40,%rdx
  401e28:	48 0f 42 c1          	cmovb  %rcx,%rax
  401e2c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401e31:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401e36:	b9 40 00 00 00       	mov    $0x40,%ecx
  401e3b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401e3f:	89 c9                	mov    %ecx,%ecx
  401e41:	89 ca                	mov    %ecx,%edx
  401e43:	48 89 d1             	mov    %rdx,%rcx
  401e46:	48 d3 e0             	shl    %cl,%rax
  401e49:	48 89 c1             	mov    %rax,%rcx
  401e4c:	31 c0                	xor    %eax,%eax
  401e4e:	48 83 fa 40          	cmp    $0x40,%rdx
  401e52:	48 0f 42 c1          	cmovb  %rcx,%rax
  401e56:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401e5b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401e5f:	89 ce                	mov    %ecx,%esi
  401e61:	48 89 f1             	mov    %rsi,%rcx
  401e64:	48 d3 ea             	shr    %cl,%rdx
  401e67:	31 c9                	xor    %ecx,%ecx
  401e69:	48 83 fe 40          	cmp    $0x40,%rsi
  401e6d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401e71:	48 09 c8             	or     %rcx,%rax
  401e74:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401e79:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401e7e:	b9 40 00 00 00       	mov    $0x40,%ecx
  401e83:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401e87:	89 c9                	mov    %ecx,%ecx
  401e89:	89 ca                	mov    %ecx,%edx
  401e8b:	48 89 d1             	mov    %rdx,%rcx
  401e8e:	48 d3 e0             	shl    %cl,%rax
  401e91:	48 89 c1             	mov    %rax,%rcx
  401e94:	31 c0                	xor    %eax,%eax
  401e96:	48 83 fa 40          	cmp    $0x40,%rdx
  401e9a:	48 0f 42 c1          	cmovb  %rcx,%rax
  401e9e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401ea3:	eb 00                	jmp    401ea5 <runtime::udivmod128+0x835>
  401ea5:	eb 00                	jmp    401ea7 <runtime::udivmod128+0x837>
  401ea7:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  401eae:	00 
  401eaf:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401eb6:	00 00 
  401eb8:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  401ebf:	00 00 
  401ec1:	83 7c 24 1c 00       	cmpl   $0x0,0x1c(%rsp)
  401ec6:	0f 97 c0             	seta   %al
  401ec9:	24 01                	and    $0x1,%al
  401ecb:	3c 00                	cmp    $0x0,%al
  401ecd:	0f 84 eb 00 00 00    	je     401fbe <runtime::udivmod128+0x94e>
  401ed3:	48 8b 74 24 b8       	mov    -0x48(%rsp),%rsi
  401ed8:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  401edd:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401ee2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401ee7:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401eec:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401ef1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401ef6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  401efb:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401f00:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401f05:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401f0a:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  401f0f:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  401f14:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401f19:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401f1e:	48 01 c0             	add    %rax,%rax
  401f21:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  401f25:	48 09 c8             	or     %rcx,%rax
  401f28:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401f2d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401f32:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  401f37:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  401f3c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  401f41:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401f46:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401f4b:	48 f7 d0             	not    %rax
  401f4e:	48 f7 d1             	not    %rcx
  401f51:	48 01 f1             	add    %rsi,%rcx
  401f54:	48 11 d0             	adc    %rdx,%rax
  401f57:	48 c1 f8 3f          	sar    $0x3f,%rax
  401f5b:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401f60:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401f65:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401f69:	83 e0 01             	and    $0x1,%eax
  401f6c:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  401f70:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401f75:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401f7a:	48 21 ca             	and    %rcx,%rdx
  401f7d:	48 21 c6             	and    %rax,%rsi
  401f80:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401f85:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401f8a:	48 29 f1             	sub    %rsi,%rcx
  401f8d:	48 19 d0             	sbb    %rdx,%rax
  401f90:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  401f95:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401f9a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401f9f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  401fa4:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401fa9:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401fae:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401fb2:	83 e8 01             	sub    $0x1,%eax
  401fb5:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401fb9:	e9 03 ff ff ff       	jmp    401ec1 <runtime::udivmod128+0x851>
  401fbe:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401fc3:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401fc8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401fcd:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  401fd2:	48 01 c9             	add    %rcx,%rcx
  401fd5:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  401fd9:	48 09 f1             	or     %rsi,%rcx
  401fdc:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  401fe1:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  401fe6:	48 83 f8 00          	cmp    $0x0,%rax
  401fea:	0f 95 c0             	setne  %al
  401fed:	24 01                	and    $0x1,%al
  401fef:	3c 00                	cmp    $0x0,%al
  401ff1:	74 16                	je     402009 <runtime::udivmod128+0x999>
  401ff3:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401ff8:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  401ffd:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  402002:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402006:	48 89 08             	mov    %rcx,(%rax)
  402009:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40200e:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  402013:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40201a:	c3                   	ret
  40201b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402020 <runtime::stderr_write>:
  402020:	48 83 ec 48          	sub    $0x48,%rsp
  402024:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  402029:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40202e:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  402033:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402038:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40203d:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  402042:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  402047:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40204e:	00 00 
  402050:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  402055:	e8 16 00 00 00       	call   402070 <runtime::[os_specific_linux.odin]::_stderr_write>
  40205a:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40205f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402064:	48 89 11             	mov    %rdx,(%rcx)
  402067:	48 83 c4 48          	add    $0x48,%rsp
  40206b:	c3                   	ret
  40206c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402070 <runtime::[os_specific_linux.odin]::_stderr_write>:
  402070:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  402075:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40207a:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  40207f:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  402084:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  402089:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  40208e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  402093:	b8 01 00 00 00       	mov    $0x1,%eax
  402098:	bf 02 00 00 00       	mov    $0x2,%edi
  40209d:	0f 05                	syscall
  40209f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4020a4:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  4020aa:	0f 9c c0             	setl   %al
  4020ad:	24 01                	and    $0x1,%al
  4020af:	3c 00                	cmp    $0x0,%al
  4020b1:	74 26                	je     4020d9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4020b3:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  4020ba:	ff ff 
  4020bc:	0f 9f c0             	setg   %al
  4020bf:	24 01                	and    $0x1,%al
  4020c1:	3c 00                	cmp    $0x0,%al
  4020c3:	74 14                	je     4020d9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4020c5:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  4020ca:	31 c0                	xor    %eax,%eax
  4020cc:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  4020d1:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  4020d8:	c3                   	ret
  4020d9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4020de:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4020e3:	48 89 08             	mov    %rcx,(%rax)
  4020e6:	31 c0                	xor    %eax,%eax
  4020e8:	c3                   	ret
  4020e9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004020f0 <runtime::heap_allocator_proc>:
  4020f0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4020f7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  4020fc:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  402101:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402106:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40210b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  402110:	40 88 f0             	mov    %sil,%al
  402113:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  402117:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40211e:	00 
  40211f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402124:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40212b:	00 
  40212c:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402131:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  402135:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40213a:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40213f:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402144:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402149:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  40214e:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  402155:	00 
  402156:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  40215d:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  402164:	00 
  402165:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  40216c:	00 
  40216d:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  402174:	00 
  402175:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40217c:	00 
  40217d:	0f b6 c8             	movzbl %al,%ecx
  402180:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  402185:	2c 07                	sub    $0x7,%al
  402187:	0f 87 5f 01 00 00    	ja     4022ec <runtime::heap_allocator_proc+0x1fc>
  40218d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402192:	48 8b 04 c5 b0 76 40 	mov    0x4076b0(,%rax,8),%rax
  402199:	00 
  40219a:	ff e0                	jmp    *%rax
  40219c:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4021a1:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4021a6:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  4021ab:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  4021af:	84 c0                	test   %al,%al
  4021b1:	0f 94 c0             	sete   %al
  4021b4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4021b7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4021be:	00 
  4021bf:	48 89 e1             	mov    %rsp,%rcx
  4021c2:	48 89 11             	mov    %rdx,(%rcx)
  4021c5:	44 0f b6 c0          	movzbl %al,%r8d
  4021c9:	31 c0                	xor    %eax,%eax
  4021cb:	89 c1                	mov    %eax,%ecx
  4021cd:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  4021d4:	00 
  4021d5:	48 89 ca             	mov    %rcx,%rdx
  4021d8:	e8 43 3f 00 00       	call   406120 <runtime::heap_allocator_proc.aligned_alloc-0>
  4021dd:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4021e2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4021e9:	00 
  4021ea:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4021f1:	00 
  4021f2:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4021f6:	48 89 11             	mov    %rdx,(%rcx)
  4021f9:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402200:	c3                   	ret
  402201:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  402206:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40220b:	e8 60 41 00 00       	call   406370 <runtime::heap_allocator_proc.aligned_free-1>
  402210:	e9 d7 00 00 00       	jmp    4022ec <runtime::heap_allocator_proc+0x1fc>
  402215:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40221a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402221:	00 
  402222:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402229:	b0 04                	mov    $0x4,%al
  40222b:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402232:	c3                   	ret
  402233:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402238:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40223d:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402242:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  402247:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  40224c:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  402250:	2c 03                	sub    $0x3,%al
  402252:	0f 94 c0             	sete   %al
  402255:	0f 57 c0             	xorps  %xmm0,%xmm0
  402258:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40225d:	49 89 e0             	mov    %rsp,%r8
  402260:	4d 89 08             	mov    %r9,(%r8)
  402263:	44 0f b6 c0          	movzbl %al,%r8d
  402267:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  40226c:	e8 3f 41 00 00       	call   4063b0 <runtime::heap_allocator_proc.aligned_resize-2>
  402271:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402276:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40227b:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  402280:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402284:	48 89 11             	mov    %rdx,(%rcx)
  402287:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40228e:	c3                   	ret
  40228f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402294:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402299:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  40229f:	0f 95 c0             	setne  %al
  4022a2:	24 01                	and    $0x1,%al
  4022a4:	3c 00                	cmp    $0x0,%al
  4022a6:	74 08                	je     4022b0 <runtime::heap_allocator_proc+0x1c0>
  4022a8:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4022ad:	c6 00 db             	movb   $0xdb,(%rax)
  4022b0:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4022b5:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4022bc:	00 
  4022bd:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4022c4:	31 c0                	xor    %eax,%eax
  4022c6:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4022cd:	c3                   	ret
  4022ce:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4022d3:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4022da:	00 
  4022db:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4022e2:	b0 04                	mov    $0x4,%al
  4022e4:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4022eb:	c3                   	ret
  4022ec:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4022f1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4022f8:	00 
  4022f9:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402300:	31 c0                	xor    %eax,%eax
  402302:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402309:	c3                   	ret
  40230a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402310 <runtime::[internal.odin]::byte_slice>:
  402310:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  402315:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40231a:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  40231f:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  402324:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  402329:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  40232e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402333:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  402338:	31 c0                	xor    %eax,%eax
  40233a:	48 85 d2             	test   %rdx,%rdx
  40233d:	48 0f 49 c2          	cmovns %rdx,%rax
  402341:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  402346:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40234b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  402350:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  402355:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40235a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40235f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  402364:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  402369:	c3                   	ret
  40236a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402370 <runtime::bounds_check_error>:
  402370:	48 83 ec 58          	sub    $0x58,%rsp
  402374:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402379:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40237e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402382:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402386:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40238b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402390:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402395:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40239a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40239e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  4023a2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4023a7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4023ac:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4023b1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  4023b6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  4023ba:	89 54 24 40          	mov    %edx,0x40(%rsp)
  4023be:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4023c3:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4023c8:	48 39 c8             	cmp    %rcx,%rax
  4023cb:	0f 92 c0             	setb   %al
  4023ce:	24 01                	and    $0x1,%al
  4023d0:	3c 00                	cmp    $0x0,%al
  4023d2:	74 05                	je     4023d9 <runtime::bounds_check_error+0x69>
  4023d4:	48 83 c4 58          	add    $0x58,%rsp
  4023d8:	c3                   	ret
  4023d9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4023de:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4023e3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4023e7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4023eb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4023f0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4023f5:	e8 76 42 00 00       	call   406670 <runtime::bounds_check_error.handle_error-0>
  4023fa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402400 <runtime::is_power_of_two_int>:
  402400:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  402405:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40240a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40240f:	48 83 f8 00          	cmp    $0x0,%rax
  402413:	0f 9e c0             	setle  %al
  402416:	24 01                	and    $0x1,%al
  402418:	3c 00                	cmp    $0x0,%al
  40241a:	74 03                	je     40241f <runtime::is_power_of_two_int+0x1f>
  40241c:	31 c0                	xor    %eax,%eax
  40241e:	c3                   	ret
  40241f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  402424:	48 89 c1             	mov    %rax,%rcx
  402427:	48 83 e9 01          	sub    $0x1,%rcx
  40242b:	48 21 c8             	and    %rcx,%rax
  40242e:	48 83 f8 00          	cmp    $0x0,%rax
  402432:	0f 94 c0             	sete   %al
  402435:	24 01                	and    $0x1,%al
  402437:	c3                   	ret
  402438:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40243f:	00 

0000000000402440 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  402440:	48 83 ec 18          	sub    $0x18,%rsp
  402444:	48 89 3c 24          	mov    %rdi,(%rsp)
  402448:	40 88 f0             	mov    %sil,%al
  40244b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40244f:	48 8b 04 24          	mov    (%rsp),%rax
  402453:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  402457:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40245c:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  402460:	48 83 f8 00          	cmp    $0x0,%rax
  402464:	0f 9e c0             	setle  %al
  402467:	24 01                	and    $0x1,%al
  402469:	3c 00                	cmp    $0x0,%al
  40246b:	74 07                	je     402474 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  40246d:	31 c0                	xor    %eax,%eax
  40246f:	48 83 c4 18          	add    $0x18,%rsp
  402473:	c3                   	ret
  402474:	8a 44 24 0e          	mov    0xe(%rsp),%al
  402478:	3c 00                	cmp    $0x0,%al
  40247a:	74 13                	je     40248f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  40247c:	48 8b 34 24          	mov    (%rsp),%rsi
  402480:	bf 01 00 00 00       	mov    $0x1,%edi
  402485:	e8 c6 eb ff ff       	call   401050 <calloc@plt>
  40248a:	48 83 c4 18          	add    $0x18,%rsp
  40248e:	c3                   	ret
  40248f:	48 8b 3c 24          	mov    (%rsp),%rdi
  402493:	e8 d8 eb ff ff       	call   401070 <malloc@plt>
  402498:	48 83 c4 18          	add    $0x18,%rsp
  40249c:	c3                   	ret
  40249d:	0f 1f 00             	nopl   (%rax)

00000000004024a0 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  4024a0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  4024a5:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  4024aa:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  4024af:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4024b4:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4024b9:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4024be:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4024c3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4024c8:	48 01 c2             	add    %rax,%rdx
  4024cb:	0f 92 c0             	setb   %al
  4024ce:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  4024d3:	24 01                	and    $0x1,%al
  4024d5:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  4024d9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4024de:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  4024e3:	0f 94 c0             	sete   %al
  4024e6:	24 01                	and    $0x1,%al
  4024e8:	48 89 11             	mov    %rdx,(%rcx)
  4024eb:	c3                   	ret
  4024ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004024f0 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  4024f0:	48 83 ec 28          	sub    $0x28,%rsp
  4024f4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4024f9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4024fe:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402503:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402508:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40250d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402512:	e8 69 eb ff ff       	call   401080 <realloc@plt>
  402517:	48 83 c4 28          	add    $0x28,%rsp
  40251b:	c3                   	ret
  40251c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402520 <runtime::memory_block_alloc>:
  402520:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  402527:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  40252c:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  402531:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  402536:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40253b:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  402540:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402545:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  40254c:	00 
  40254d:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402552:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  402557:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  40255c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  402561:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  402566:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40256b:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  402570:	48 89 b4 24 f0 00 00 	mov    %rsi,0xf0(%rsp)
  402577:	00 
  402578:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  40257f:	00 
  402580:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  402587:	00 
  402588:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40258d:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  402594:	00 
  402595:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40259a:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  4025a1:	00 
  4025a2:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4025a9:	00 
  4025aa:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  4025b1:	00 00 00 00 00 
  4025b6:	c6 84 24 d7 00 00 00 	movb   $0x0,0xd7(%rsp)
  4025bd:	00 
  4025be:	48 89 c1             	mov    %rax,%rcx
  4025c1:	48 83 e9 31          	sub    $0x31,%rcx
  4025c5:	b9 30 00 00 00       	mov    $0x30,%ecx
  4025ca:	48 0f 43 c8          	cmovae %rax,%rcx
  4025ce:	48 01 ca             	add    %rcx,%rdx
  4025d1:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  4025d8:	00 
  4025d9:	48 89 8c 24 c0 00 00 	mov    %rcx,0xc0(%rsp)
  4025e0:	00 
  4025e1:	48 89 c1             	mov    %rax,%rcx
  4025e4:	48 83 e9 10          	sub    $0x10,%rcx
  4025e8:	b9 10 00 00 00       	mov    $0x10,%ecx
  4025ed:	48 0f 4c c1          	cmovl  %rcx,%rax
  4025f1:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4025f8:	00 
  4025f9:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  402600:	00 
  402601:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  402608:	00 
  402609:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  402610:	00 
  402611:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  402618:	00 
  402619:	0f 57 c0             	xorps  %xmm0,%xmm0
  40261c:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402623:	00 
  402624:	48 89 e0             	mov    %rsp,%rax
  402627:	4c 89 08             	mov    %r9,(%rax)
  40262a:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  402631:	00 
  402632:	e8 f9 13 00 00       	call   403a30 <runtime::mem_alloc>
  402637:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  40263b:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  402642:	00 
  402643:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402648:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  40264f:	00 
  402650:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  402655:	3c 00                	cmp    $0x0,%al
  402657:	74 39                	je     402692 <runtime::memory_block_alloc+0x172>
  402659:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40265e:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  402662:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402669:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402670:	00 
  402671:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  402678:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  40267f:	00 
  402680:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402687:	48 89 11             	mov    %rdx,(%rcx)
  40268a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402691:	c3                   	ret
  402692:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  402697:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40269c:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4026a1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4026a6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4026ab:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4026b0:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4026b7:	00 
  4026b8:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4026bd:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4026c4:	00 
  4026c5:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4026ca:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  4026d1:	00 
  4026d2:	48 01 f0             	add    %rsi,%rax
  4026d5:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4026da:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4026df:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4026e4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4026eb:	00 
  4026ec:	48 89 50 10          	mov    %rdx,0x10(%rax)
  4026f0:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4026f4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4026fb:	00 
  4026fc:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  402703:	00 
  402704:	48 03 8c 24 c0 00 00 	add    0xc0(%rsp),%rcx
  40270b:	00 
  40270c:	48 89 48 18          	mov    %rcx,0x18(%rax)
  402710:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402717:	00 
  402718:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40271d:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402724:	00 
  402725:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  402729:	48 29 d1             	sub    %rdx,%rcx
  40272c:	48 89 48 28          	mov    %rcx,0x28(%rax)
  402730:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402737:	00 
  402738:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  40273d:	0f 94 c0             	sete   %al
  402740:	24 01                	and    $0x1,%al
  402742:	0f b6 f8             	movzbl %al,%edi
  402745:	be 30 77 40 00       	mov    $0x407730,%esi
  40274a:	b9 a0 77 40 00       	mov    $0x4077a0,%ecx
  40274f:	ba 0f 00 00 00       	mov    $0xf,%edx
  402754:	e8 67 39 00 00       	call   4060c0 <runtime::assert>
  402759:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  40275e:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402765:	00 
  402766:	48 83 38 00          	cmpq   $0x0,(%rax)
  40276a:	0f 94 c0             	sete   %al
  40276d:	24 01                	and    $0x1,%al
  40276f:	0f b6 f8             	movzbl %al,%edi
  402772:	be c8 77 40 00       	mov    $0x4077c8,%esi
  402777:	b9 e0 77 40 00       	mov    $0x4077e0,%ecx
  40277c:	ba 11 00 00 00       	mov    $0x11,%edx
  402781:	e8 3a 39 00 00       	call   4060c0 <runtime::assert>
  402786:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40278b:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402792:	00 
  402793:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  40279a:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  4027a1:	00 
  4027a2:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  4027a9:	48 89 11             	mov    %rdx,(%rcx)
  4027ac:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  4027b3:	c3                   	ret
  4027b4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4027bb:	00 00 00 00 00 

00000000004027c0 <runtime::default_temp_allocator_destroy>:
  4027c0:	48 83 ec 18          	sub    $0x18,%rsp
  4027c4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4027c8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4027cd:	48 8b 04 24          	mov    (%rsp),%rax
  4027d1:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4027d6:	48 83 f8 00          	cmp    $0x0,%rax
  4027da:	0f 95 c0             	setne  %al
  4027dd:	24 01                	and    $0x1,%al
  4027df:	3c 00                	cmp    $0x0,%al
  4027e1:	74 29                	je     40280c <runtime::default_temp_allocator_destroy+0x4c>
  4027e3:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4027e8:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4027ed:	48 be 70 78 40 00 00 	movabs $0x407870,%rsi
  4027f4:	00 00 00 
  4027f7:	e8 14 1b 00 00       	call   404310 <runtime::arena_destroy>
  4027fc:	48 8b 3c 24          	mov    (%rsp),%rdi
  402800:	31 f6                	xor    %esi,%esi
  402802:	ba 38 00 00 00       	mov    $0x38,%edx
  402807:	e8 34 e8 ff ff       	call   401040 <memset@plt>
  40280c:	48 83 c4 18          	add    $0x18,%rsp
  402810:	c3                   	ret
  402811:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402818:	0f 1f 84 00 00 00 00 
  40281f:	00 

0000000000402820 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  402820:	48 83 ec 18          	sub    $0x18,%rsp
  402824:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402829:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40282e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402833:	e8 f8 e7 ff ff       	call   401030 <free@plt>
  402838:	48 83 c4 18          	add    $0x18,%rsp
  40283c:	c3                   	ret
  40283d:	0f 1f 00             	nopl   (%rax)

0000000000402840 <runtime::default_random_generator_proc>:
  402840:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402847:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40284c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402851:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  402856:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40285b:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402860:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402865:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40286a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40286f:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402876:	00 
  402877:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  40287c:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  402881:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  402886:	48 83 f8 00          	cmp    $0x0,%rax
  40288a:	0f 94 c0             	sete   %al
  40288d:	24 01                	and    $0x1,%al
  40288f:	3c 00                	cmp    $0x0,%al
  402891:	74 1a                	je     4028ad <runtime::default_random_generator_proc+0x6d>
  402893:	48 c7 c1 e8 ff ff ff 	mov    $0xffffffffffffffe8,%rcx
  40289a:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  4028a1:	00 00 
  4028a3:	48 01 c8             	add    %rcx,%rax
  4028a6:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4028ab:	eb 0a                	jmp    4028b7 <runtime::default_random_generator_proc+0x77>
  4028ad:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4028b2:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4028b7:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4028bc:	48 85 c0             	test   %rax,%rax
  4028bf:	74 27                	je     4028e8 <runtime::default_random_generator_proc+0xa8>
  4028c1:	eb 00                	jmp    4028c3 <runtime::default_random_generator_proc+0x83>
  4028c3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4028c8:	48 83 e8 01          	sub    $0x1,%rax
  4028cc:	0f 84 17 01 00 00    	je     4029e9 <runtime::default_random_generator_proc+0x1a9>
  4028d2:	eb 00                	jmp    4028d4 <runtime::default_random_generator_proc+0x94>
  4028d4:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4028d9:	48 83 e8 02          	sub    $0x2,%rax
  4028dd:	0f 84 40 01 00 00    	je     402a23 <runtime::default_random_generator_proc+0x1e3>
  4028e3:	e9 6b 01 00 00       	jmp    402a53 <runtime::default_random_generator_proc+0x213>
  4028e8:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4028ed:	48 83 38 00          	cmpq   $0x0,(%rax)
  4028f1:	0f 94 c0             	sete   %al
  4028f4:	24 01                	and    $0x1,%al
  4028f6:	3c 00                	cmp    $0x0,%al
  4028f8:	74 21                	je     40291b <runtime::default_random_generator_proc+0xdb>
  4028fa:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4028ff:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  402904:	0f 94 c0             	sete   %al
  402907:	24 01                	and    $0x1,%al
  402909:	3c 00                	cmp    $0x0,%al
  40290b:	74 0e                	je     40291b <runtime::default_random_generator_proc+0xdb>
  40290d:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402912:	31 c0                	xor    %eax,%eax
  402914:	89 c6                	mov    %eax,%esi
  402916:	e8 15 3f 00 00       	call   406830 <runtime::default_random_generator_proc.init-1>
  40291b:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402920:	48 83 e8 08          	sub    $0x8,%rax
  402924:	75 26                	jne    40294c <runtime::default_random_generator_proc+0x10c>
  402926:	eb 00                	jmp    402928 <runtime::default_random_generator_proc+0xe8>
  402928:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  40292d:	e8 2e 3e 00 00       	call   406760 <runtime::default_random_generator_proc.read_u64-0>
  402932:	48 89 c1             	mov    %rax,%rcx
  402935:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40293a:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  40293f:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402944:	48 89 08             	mov    %rcx,(%rax)
  402947:	e9 9b 00 00 00       	jmp    4029e7 <runtime::default_random_generator_proc+0x1a7>
  40294c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402951:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  402956:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  40295d:	00 00 
  40295f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402964:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  40296b:	ff ff 
  40296d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402972:	48 83 c0 01          	add    $0x1,%rax
  402976:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40297b:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  402980:	7d 63                	jge    4029e5 <runtime::default_random_generator_proc+0x1a5>
  402982:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402987:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40298c:	48 01 c8             	add    %rcx,%rax
  40298f:	48 89 04 24          	mov    %rax,(%rsp)
  402993:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  402998:	0f 94 c0             	sete   %al
  40299b:	24 01                	and    $0x1,%al
  40299d:	3c 00                	cmp    $0x0,%al
  40299f:	74 14                	je     4029b5 <runtime::default_random_generator_proc+0x175>
  4029a1:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4029a6:	e8 b5 3d 00 00       	call   406760 <runtime::default_random_generator_proc.read_u64-0>
  4029ab:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4029b0:	c6 44 24 57 07       	movb   $0x7,0x57(%rsp)
  4029b5:	48 8b 04 24          	mov    (%rsp),%rax
  4029b9:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4029be:	88 08                	mov    %cl,(%rax)
  4029c0:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4029c5:	48 c1 e9 08          	shr    $0x8,%rcx
  4029c9:	b2 01                	mov    $0x1,%dl
  4029cb:	31 c0                	xor    %eax,%eax
  4029cd:	f6 c2 01             	test   $0x1,%dl
  4029d0:	48 0f 45 c1          	cmovne %rcx,%rax
  4029d4:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4029d9:	8a 44 24 57          	mov    0x57(%rsp),%al
  4029dd:	2c 01                	sub    $0x1,%al
  4029df:	88 44 24 57          	mov    %al,0x57(%rsp)
  4029e3:	eb 88                	jmp    40296d <runtime::default_random_generator_proc+0x12d>
  4029e5:	eb 00                	jmp    4029e7 <runtime::default_random_generator_proc+0x1a7>
  4029e7:	eb 6a                	jmp    402a53 <runtime::default_random_generator_proc+0x213>
  4029e9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4029ee:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4029f3:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4029fa:	00 00 
  4029fc:	b8 08 00 00 00       	mov    $0x8,%eax
  402a01:	48 39 d0             	cmp    %rdx,%rax
  402a04:	48 0f 4c d0          	cmovl  %rax,%rdx
  402a08:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402a0d:	e8 5e 09 00 00       	call   403370 <runtime::mem_copy_non_overlapping>
  402a12:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402a17:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  402a1c:	e8 0f 3e 00 00       	call   406830 <runtime::default_random_generator_proc.init-1>
  402a21:	eb 30                	jmp    402a53 <runtime::default_random_generator_proc+0x213>
  402a23:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402a28:	48 83 f8 04          	cmp    $0x4,%rax
  402a2c:	0f 95 c0             	setne  %al
  402a2f:	24 01                	and    $0x1,%al
  402a31:	3c 00                	cmp    $0x0,%al
  402a33:	74 08                	je     402a3d <runtime::default_random_generator_proc+0x1fd>
  402a35:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402a3c:	c3                   	ret
  402a3d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402a42:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402a47:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402a4c:	8b 08                	mov    (%rax),%ecx
  402a4e:	83 c9 0a             	or     $0xa,%ecx
  402a51:	89 08                	mov    %ecx,(%rax)
  402a53:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402a5a:	c3                   	ret
  402a5b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402a60 <runtime::slice_handle_error>:
  402a60:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402a67:	4c 89 0c 24          	mov    %r9,(%rsp)
  402a6b:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  402a70:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  402a74:	89 54 24 14          	mov    %edx,0x14(%rsp)
  402a78:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402a7d:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  402a82:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  402a89:	00 
  402a8a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402a8f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402a94:	4c 8b 04 24          	mov    (%rsp),%r8
  402a98:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  402a9d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  402aa1:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  402aa5:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402aaa:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  402aaf:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402ab4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  402abb:	00 
  402abc:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402ac0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402ac4:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  402ac9:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  402ace:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402ad3:	0f 57 c0             	xorps  %xmm0,%xmm0
  402ad6:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402adb:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402ae0:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402ae7:	00 00 
  402ae9:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402aee:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402af3:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402afa:	00 00 
  402afc:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  402b01:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402b06:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  402b0a:	89 44 24 44          	mov    %eax,0x44(%rsp)
  402b0e:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402b13:	e8 c8 16 00 00       	call   4041e0 <runtime::print_caller_location>
  402b18:	bf 99 78 40 00       	mov    $0x407899,%edi
  402b1d:	be 17 00 00 00       	mov    $0x17,%esi
  402b22:	e8 99 0e 00 00       	call   4039c0 <runtime::print_string>
  402b27:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402b2c:	e8 af 14 00 00       	call   403fe0 <runtime::print_i64>
  402b31:	bf b1 78 40 00       	mov    $0x4078b1,%edi
  402b36:	be 01 00 00 00       	mov    $0x1,%esi
  402b3b:	e8 80 0e 00 00       	call   4039c0 <runtime::print_string>
  402b40:	48 8b 3c 24          	mov    (%rsp),%rdi
  402b44:	e8 97 14 00 00       	call   403fe0 <runtime::print_i64>
  402b49:	bf b3 78 40 00       	mov    $0x4078b3,%edi
  402b4e:	be 15 00 00 00       	mov    $0x15,%esi
  402b53:	e8 68 0e 00 00       	call   4039c0 <runtime::print_string>
  402b58:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402b5d:	e8 7e 14 00 00       	call   403fe0 <runtime::print_i64>
  402b62:	bf 0a 00 00 00       	mov    $0xa,%edi
  402b67:	e8 84 10 00 00       	call   403bf0 <runtime::print_byte>
  402b6c:	e8 cf ea ff ff       	call   401640 <runtime::bounds_trap>
  402b71:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402b78:	0f 1f 84 00 00 00 00 
  402b7f:	00 

0000000000402b80 <runtime::default_temp_allocator_proc>:
  402b80:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  402b87:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  402b8c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  402b91:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402b96:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  402b9b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  402ba0:	40 88 f0             	mov    %sil,%al
  402ba3:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  402ba7:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  402bae:	00 
  402baf:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402bb4:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  402bbb:	00 
  402bbc:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  402bc1:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  402bc8:	00 
  402bc9:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402bce:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  402bd3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402bd8:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  402bdd:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  402be2:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  402be6:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  402beb:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  402bf0:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  402bf5:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  402bfc:	00 
  402bfd:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  402c04:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  402c0b:	00 
  402c0c:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  402c13:	00 
  402c14:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  402c1b:	00 
  402c1c:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  402c23:	00 
  402c24:	0f 57 c0             	xorps  %xmm0,%xmm0
  402c27:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402c2e:	00 
  402c2f:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  402c36:	00 
  402c37:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  402c3e:	00 
  402c3f:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  402c46:	00 
  402c47:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  402c4e:	00 
  402c4f:	48 89 e6             	mov    %rsp,%rsi
  402c52:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  402c56:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  402c5d:	00 
  402c5e:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  402c62:	4c 89 16             	mov    %r10,(%rsi)
  402c65:	0f b6 f0             	movzbl %al,%esi
  402c68:	e8 43 17 00 00       	call   4043b0 <runtime::arena_allocator_proc>
  402c6d:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402c72:	40 88 c7             	mov    %al,%dil
  402c75:	40 88 f8             	mov    %dil,%al
  402c78:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  402c7f:	00 
  402c80:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  402c87:	00 
  402c88:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402c8f:	00 
  402c90:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402c97:	00 
  402c98:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  402c9f:	00 
  402ca0:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402ca4:	48 89 11             	mov    %rdx,(%rcx)
  402ca7:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402cae:	c3                   	ret
  402caf:	90                   	nop

0000000000402cb0 <runtime::multi_pointer_slice_handle_error>:
  402cb0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402cb7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402cbc:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402cc1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402cc5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402cc9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402cce:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402cd3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402cd8:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402cdd:	8b 44 24 18          	mov    0x18(%rsp),%eax
  402ce1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402ce5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402cea:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402cef:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402cf4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  402cfb:	00 
  402cfc:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402d00:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402d04:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  402d09:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  402d0e:	0f 57 c0             	xorps  %xmm0,%xmm0
  402d11:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402d16:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402d1b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402d22:	00 00 
  402d24:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402d29:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402d2e:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402d35:	00 00 
  402d37:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  402d3c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402d41:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  402d45:	89 44 24 44          	mov    %eax,0x44(%rsp)
  402d49:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402d4e:	e8 8d 14 00 00       	call   4041e0 <runtime::print_caller_location>
  402d53:	bf 99 78 40 00       	mov    $0x407899,%edi
  402d58:	be 17 00 00 00       	mov    $0x17,%esi
  402d5d:	e8 5e 0c 00 00       	call   4039c0 <runtime::print_string>
  402d62:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402d67:	e8 74 12 00 00       	call   403fe0 <runtime::print_i64>
  402d6c:	bf b1 78 40 00       	mov    $0x4078b1,%edi
  402d71:	be 01 00 00 00       	mov    $0x1,%esi
  402d76:	e8 45 0c 00 00       	call   4039c0 <runtime::print_string>
  402d7b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402d80:	e8 5b 12 00 00       	call   403fe0 <runtime::print_i64>
  402d85:	bf 0a 00 00 00       	mov    $0xa,%edi
  402d8a:	e8 61 0e 00 00       	call   403bf0 <runtime::print_byte>
  402d8f:	e8 ac e8 ff ff       	call   401640 <runtime::bounds_trap>
  402d94:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402d9b:	00 00 00 00 00 

0000000000402da0 <runtime::memory_block_dealloc>:
  402da0:	48 83 ec 38          	sub    $0x38,%rsp
  402da4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402da9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402dae:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402db3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402db8:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  402dbd:	48 83 f8 00          	cmp    $0x0,%rax
  402dc1:	0f 95 c0             	setne  %al
  402dc4:	24 01                	and    $0x1,%al
  402dc6:	3c 00                	cmp    $0x0,%al
  402dc8:	74 35                	je     402dff <runtime::memory_block_dealloc+0x5f>
  402dca:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  402dcf:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  402dd4:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402dd9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402dde:	48 8b 42 08          	mov    0x8(%rdx),%rax
  402de2:	48 8b 52 10          	mov    0x10(%rdx),%rdx
  402de6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  402deb:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402df0:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402df5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402dfa:	e8 f1 0f 00 00       	call   403df0 <runtime::mem_free>
  402dff:	48 83 c4 38          	add    $0x38,%rsp
  402e03:	c3                   	ret
  402e04:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402e0b:	00 00 00 00 00 

0000000000402e10 <main>:
  402e10:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  402e17:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  402e1b:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402e20:	8b 44 24 14          	mov    0x14(%rsp),%eax
  402e24:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402e29:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  402e30:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  402e37:	00 
  402e38:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  402e3f:	00 
  402e40:	48 89 0c 24          	mov    %rcx,(%rsp)
  402e44:	4c 63 c8             	movslq %eax,%r9
  402e47:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402e4c:	bf c9 78 40 00       	mov    $0x4078c9,%edi
  402e51:	31 c0                	xor    %eax,%eax
  402e53:	41 89 c0             	mov    %eax,%r8d
  402e56:	be 2c 00 00 00       	mov    $0x2c,%esi
  402e5b:	ba 36 00 00 00       	mov    $0x36,%edx
  402e60:	b9 11 00 00 00       	mov    $0x11,%ecx
  402e65:	e8 c6 03 00 00       	call   403230 <runtime::multi_pointer_slice_expr_error>
  402e6a:	48 8b 0c 24          	mov    (%rsp),%rcx
  402e6e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402e73:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  402e7a:	00 
  402e7b:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  402e82:	00 
  402e83:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  402e8a:	00 
  402e8b:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  402e92:	00 
  402e93:	48 c7 c0 60 a0 40 00 	mov    $0x40a060,%rax
  402e9a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402e9e:	48 89 08             	mov    %rcx,(%rax)
  402ea1:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402ea8:	00 
  402ea9:	31 f6                	xor    %esi,%esi
  402eab:	ba 70 00 00 00       	mov    $0x70,%edx
  402eb0:	e8 8b e1 ff ff       	call   401040 <memset@plt>
  402eb5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402ebc:	00 
  402ebd:	e8 fe 24 00 00       	call   4053c0 <runtime::[core.odin]::__init_context>
  402ec2:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  402ec7:	31 f6                	xor    %esi,%esi
  402ec9:	ba 70 00 00 00       	mov    $0x70,%edx
  402ece:	e8 6d e1 ff ff       	call   401040 <memset@plt>
  402ed3:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  402ed8:	e8 93 24 00 00       	call   405370 <runtime::default_context>
  402edd:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  402ee2:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  402ee7:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  402eec:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  402ef1:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  402ef6:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  402efb:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  402f02:	00 
  402f03:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  402f0a:	00 
  402f0b:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  402f12:	00 
  402f13:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  402f1a:	00 
  402f1b:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  402f22:	00 
  402f23:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  402f2a:	00 
  402f2b:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  402f32:	00 
  402f33:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  402f3a:	00 
  402f3b:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402f42:	00 
  402f43:	e8 98 e6 ff ff       	call   4015e0 <__$startup_runtime>
  402f48:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402f4f:	00 
  402f50:	e8 3b e2 ff ff       	call   401190 <journey::main>
  402f55:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  402f5c:	00 
  402f5d:	e8 8e e6 ff ff       	call   4015f0 <__$cleanup_runtime>
  402f62:	31 c0                	xor    %eax,%eax
  402f64:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  402f6b:	c3                   	ret
  402f6c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402f70 <runtime::alloc_from_memory_block>:
  402f70:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  402f77:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402f7c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  402f81:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  402f86:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402f8b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402f90:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  402f95:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402f9a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  402fa1:	00 
  402fa2:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  402fa9:	00 
  402faa:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  402fb1:	00 
  402fb2:	0f 57 c0             	xorps  %xmm0,%xmm0
  402fb5:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402fbc:	00 
  402fbd:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  402fc4:	00 
  402fc5:	48 83 f8 00          	cmp    $0x0,%rax
  402fc9:	0f 94 c0             	sete   %al
  402fcc:	24 01                	and    $0x1,%al
  402fce:	3c 00                	cmp    $0x0,%al
  402fd0:	74 3e                	je     403010 <runtime::alloc_from_memory_block+0xa0>
  402fd2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402fd7:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  402fde:	00 00 00 00 00 
  402fe3:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  402fea:	00 00 00 00 00 
  402fef:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  402ff6:	01 
  402ff7:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  402ffe:	00 
  402fff:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  403006:	b0 01                	mov    $0x1,%al
  403008:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40300f:	c3                   	ret
  403010:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403015:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40301a:	e8 b1 38 00 00       	call   4068d0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  40301f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403024:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40302b:	00 
  40302c:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  403033:	00 
  403034:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40303b:	00 00 00 00 00 
  403040:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  403047:	00 
  403048:	e8 53 f4 ff ff       	call   4024a0 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  40304d:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  403054:	00 
  403055:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40305a:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  40305e:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  403063:	75 4a                	jne    4030af <runtime::alloc_from_memory_block+0x13f>
  403065:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40306a:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  403071:	01 
  403072:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  403079:	00 
  40307a:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403081:	00 
  403082:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  403089:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  403090:	00 
  403091:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403098:	00 
  403099:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  4030a0:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4030a4:	48 89 11             	mov    %rdx,(%rcx)
  4030a7:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  4030ae:	c3                   	ret
  4030af:	eb 00                	jmp    4030b1 <runtime::alloc_from_memory_block+0x141>
  4030b1:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4030b8:	00 
  4030b9:	48 8b 78 20          	mov    0x20(%rax),%rdi
  4030bd:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4030c2:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  4030c9:	00 00 
  4030cb:	48 8d 54 24 60       	lea    0x60(%rsp),%rdx
  4030d0:	e8 cb f3 ff ff       	call   4024a0 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  4030d5:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4030da:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4030df:	88 44 24 47          	mov    %al,0x47(%rsp)
  4030e3:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  4030e8:	74 1a                	je     403104 <runtime::alloc_from_memory_block+0x194>
  4030ea:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4030ef:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4030f6:	00 
  4030f7:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  4030fb:	0f 97 c0             	seta   %al
  4030fe:	24 01                	and    $0x1,%al
  403100:	3c 00                	cmp    $0x0,%al
  403102:	74 4a                	je     40314e <runtime::alloc_from_memory_block+0x1de>
  403104:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403109:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  403110:	01 
  403111:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  403118:	00 
  403119:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403120:	00 
  403121:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  403128:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40312f:	00 
  403130:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403137:	00 
  403138:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  40313f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403143:	48 89 11             	mov    %rdx,(%rcx)
  403146:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40314d:	c3                   	ret
  40314e:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403153:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  40315a:	00 
  40315b:	48 8b 41 18          	mov    0x18(%rcx),%rax
  40315f:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  403163:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40316a:	00 
  40316b:	48 01 d1             	add    %rdx,%rcx
  40316e:	48 01 c8             	add    %rcx,%rax
  403171:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403176:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40317b:	48 89 04 24          	mov    %rax,(%rsp)
  40317f:	bf 40 77 40 00       	mov    $0x407740,%edi
  403184:	31 c0                	xor    %eax,%eax
  403186:	41 89 c0             	mov    %eax,%r8d
  403189:	be 3e 00 00 00       	mov    $0x3e,%esi
  40318e:	ba 55 00 00 00       	mov    $0x55,%edx
  403193:	b9 31 00 00 00       	mov    $0x31,%ecx
  403198:	e8 93 00 00 00       	call   403230 <runtime::multi_pointer_slice_expr_error>
  40319d:	48 8b 14 24          	mov    (%rsp),%rdx
  4031a1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4031a6:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4031ab:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4031b0:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4031b5:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4031ba:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4031bf:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  4031c6:	00 
  4031c7:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4031ce:	00 
  4031cf:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4031d6:	00 
  4031d7:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4031dc:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4031e0:	48 01 f2             	add    %rsi,%rdx
  4031e3:	48 89 50 20          	mov    %rdx,0x20(%rax)
  4031e7:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4031ee:	00 
  4031ef:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4031f6:	00 
  4031f7:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  4031fe:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  403205:	00 
  403206:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  40320d:	00 
  40320e:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  403215:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403219:	48 89 11             	mov    %rdx,(%rcx)
  40321c:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  403223:	c3                   	ret
  403224:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40322b:	00 00 00 00 00 

0000000000403230 <runtime::multi_pointer_slice_expr_error>:
  403230:	48 83 ec 58          	sub    $0x58,%rsp
  403234:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403239:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40323e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403242:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403246:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40324b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403250:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403255:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40325a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40325e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403262:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403267:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40326c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403271:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403276:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40327a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40327e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403283:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403288:	48 39 c8             	cmp    %rcx,%rax
  40328b:	0f 9e c0             	setle  %al
  40328e:	24 01                	and    $0x1,%al
  403290:	3c 00                	cmp    $0x0,%al
  403292:	74 05                	je     403299 <runtime::multi_pointer_slice_expr_error+0x69>
  403294:	48 83 c4 58          	add    $0x58,%rsp
  403298:	c3                   	ret
  403299:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40329e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4032a3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4032a7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4032ab:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4032b0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4032b5:	e8 f6 f9 ff ff       	call   402cb0 <runtime::multi_pointer_slice_handle_error>
  4032ba:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004032c0 <runtime::slice_expr_error_hi>:
  4032c0:	48 83 ec 58          	sub    $0x58,%rsp
  4032c4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4032c9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4032ce:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4032d2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4032d6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4032db:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4032e0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4032e5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4032ea:	8b 54 24 18          	mov    0x18(%rsp),%edx
  4032ee:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  4032f2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4032f7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4032fc:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403301:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403306:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40330a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40330e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403313:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403318:	31 c0                	xor    %eax,%eax
  40331a:	48 39 c8             	cmp    %rcx,%rax
  40331d:	0f 9e c0             	setle  %al
  403320:	24 01                	and    $0x1,%al
  403322:	3c 00                	cmp    $0x0,%al
  403324:	74 1b                	je     403341 <runtime::slice_expr_error_hi+0x81>
  403326:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40332b:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403330:	48 39 c8             	cmp    %rcx,%rax
  403333:	0f 9e c0             	setle  %al
  403336:	24 01                	and    $0x1,%al
  403338:	3c 00                	cmp    $0x0,%al
  40333a:	74 05                	je     403341 <runtime::slice_expr_error_hi+0x81>
  40333c:	48 83 c4 58          	add    $0x58,%rsp
  403340:	c3                   	ret
  403341:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403346:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40334a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40334e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403353:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403358:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40335d:	48 89 e0             	mov    %rsp,%rax
  403360:	4c 89 00             	mov    %r8,(%rax)
  403363:	31 c0                	xor    %eax,%eax
  403365:	41 89 c0             	mov    %eax,%r8d
  403368:	e8 f3 f6 ff ff       	call   402a60 <runtime::slice_handle_error>
  40336d:	0f 1f 00             	nopl   (%rax)

0000000000403370 <runtime::mem_copy_non_overlapping>:
  403370:	48 83 ec 38          	sub    $0x38,%rsp
  403374:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403379:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40337e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403383:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403388:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40338d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403392:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403397:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40339c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4033a1:	48 83 f8 00          	cmp    $0x0,%rax
  4033a5:	0f 95 c0             	setne  %al
  4033a8:	24 01                	and    $0x1,%al
  4033aa:	3c 00                	cmp    $0x0,%al
  4033ac:	74 3c                	je     4033ea <runtime::mem_copy_non_overlapping+0x7a>
  4033ae:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4033b3:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4033b8:	48 39 c8             	cmp    %rcx,%rax
  4033bb:	0f 95 c0             	setne  %al
  4033be:	24 01                	and    $0x1,%al
  4033c0:	3c 00                	cmp    $0x0,%al
  4033c2:	74 26                	je     4033ea <runtime::mem_copy_non_overlapping+0x7a>
  4033c4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4033c9:	48 83 f8 00          	cmp    $0x0,%rax
  4033cd:	0f 9f c0             	setg   %al
  4033d0:	24 01                	and    $0x1,%al
  4033d2:	3c 00                	cmp    $0x0,%al
  4033d4:	74 14                	je     4033ea <runtime::mem_copy_non_overlapping+0x7a>
  4033d6:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4033db:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4033e0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4033e5:	e8 76 dc ff ff       	call   401060 <memcpy@plt>
  4033ea:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4033ef:	48 83 c4 38          	add    $0x38,%rsp
  4033f3:	c3                   	ret
  4033f4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4033fb:	00 00 00 00 00 

0000000000403400 <runtime::slice_expr_error_lo_hi>:
  403400:	48 83 ec 68          	sub    $0x68,%rsp
  403404:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403409:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40340e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403412:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403416:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40341b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403420:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403425:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40342a:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40342f:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403434:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403439:	8b 74 24 18          	mov    0x18(%rsp),%esi
  40343d:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  403441:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403446:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  40344b:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  403450:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  403455:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  403459:	89 74 24 50          	mov    %esi,0x50(%rsp)
  40345d:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  403462:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  403467:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40346c:	31 c0                	xor    %eax,%eax
  40346e:	48 39 c8             	cmp    %rcx,%rax
  403471:	0f 9e c0             	setle  %al
  403474:	24 01                	and    $0x1,%al
  403476:	3c 00                	cmp    $0x0,%al
  403478:	74 47                	je     4034c1 <runtime::slice_expr_error_lo_hi+0xc1>
  40347a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40347f:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403484:	48 39 c8             	cmp    %rcx,%rax
  403487:	0f 9e c0             	setle  %al
  40348a:	24 01                	and    $0x1,%al
  40348c:	3c 00                	cmp    $0x0,%al
  40348e:	74 31                	je     4034c1 <runtime::slice_expr_error_lo_hi+0xc1>
  403490:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403495:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40349a:	48 39 c8             	cmp    %rcx,%rax
  40349d:	0f 9e c0             	setle  %al
  4034a0:	24 01                	and    $0x1,%al
  4034a2:	3c 00                	cmp    $0x0,%al
  4034a4:	74 1b                	je     4034c1 <runtime::slice_expr_error_lo_hi+0xc1>
  4034a6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4034ab:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4034b0:	48 39 c8             	cmp    %rcx,%rax
  4034b3:	0f 9e c0             	setle  %al
  4034b6:	24 01                	and    $0x1,%al
  4034b8:	3c 00                	cmp    $0x0,%al
  4034ba:	74 05                	je     4034c1 <runtime::slice_expr_error_lo_hi+0xc1>
  4034bc:	48 83 c4 68          	add    $0x68,%rsp
  4034c0:	c3                   	ret
  4034c1:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4034c6:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4034cb:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4034cf:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4034d3:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4034d8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4034dd:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  4034e2:	48 89 e0             	mov    %rsp,%rax
  4034e5:	4c 89 10             	mov    %r10,(%rax)
  4034e8:	e8 73 f5 ff ff       	call   402a60 <runtime::slice_handle_error>
  4034ed:	0f 1f 00             	nopl   (%rax)

00000000004034f0 <runtime::memset>:
  4034f0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4034f5:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  4034f9:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  4034fe:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  403503:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  403508:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  40350c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  403511:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  403515:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40351a:	48 83 f8 00          	cmp    $0x0,%rax
  40351e:	0f 95 c0             	setne  %al
  403521:	24 01                	and    $0x1,%al
  403523:	3c 00                	cmp    $0x0,%al
  403525:	74 63                	je     40358a <runtime::memset+0x9a>
  403527:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40352c:	48 83 f8 00          	cmp    $0x0,%rax
  403530:	0f 95 c0             	setne  %al
  403533:	24 01                	and    $0x1,%al
  403535:	3c 00                	cmp    $0x0,%al
  403537:	74 51                	je     40358a <runtime::memset+0x9a>
  403539:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40353e:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  403542:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  403546:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40354b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  403552:	00 00 
  403554:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  403559:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  40355e:	0f 9c c0             	setl   %al
  403561:	24 01                	and    $0x1,%al
  403563:	3c 00                	cmp    $0x0,%al
  403565:	74 21                	je     403588 <runtime::memset+0x98>
  403567:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40356c:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  403571:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  403575:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  403578:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40357d:	48 83 c0 01          	add    $0x1,%rax
  403581:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  403586:	eb cc                	jmp    403554 <runtime::memset+0x64>
  403588:	eb 00                	jmp    40358a <runtime::memset+0x9a>
  40358a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40358f:	c3                   	ret

0000000000403590 <runtime::arena_alloc>:
  403590:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  403597:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40359c:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  4035a1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4035a6:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4035ab:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4035b0:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  4035b5:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4035ba:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  4035bf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4035c4:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4035c9:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  4035ce:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  4035d5:	00 
  4035d6:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  4035dd:	00 
  4035de:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4035e5:	00 
  4035e6:	0f 57 c0             	xorps  %xmm0,%xmm0
  4035e9:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  4035f0:	00 
  4035f1:	c6 84 24 0f 01 00 00 	movb   $0x0,0x10f(%rsp)
  4035f8:	00 
  4035f9:	48 89 c2             	mov    %rax,%rdx
  4035fc:	48 83 ea 01          	sub    $0x1,%rdx
  403600:	48 21 d0             	and    %rdx,%rax
  403603:	48 83 f8 00          	cmp    $0x0,%rax
  403607:	0f 94 c0             	sete   %al
  40360a:	24 01                	and    $0x1,%al
  40360c:	0f b6 f8             	movzbl %al,%edi
  40360f:	be f6 78 40 00       	mov    $0x4078f6,%esi
  403614:	ba 1a 00 00 00       	mov    $0x1a,%edx
  403619:	e8 a2 2a 00 00       	call   4060c0 <runtime::assert>
  40361e:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403623:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  40362a:	00 
  40362b:	48 83 bc 24 00 01 00 	cmpq   $0x0,0x100(%rsp)
  403632:	00 00 
  403634:	0f 94 c0             	sete   %al
  403637:	24 01                	and    $0x1,%al
  403639:	3c 00                	cmp    $0x0,%al
  40363b:	74 42                	je     40367f <runtime::arena_alloc+0xef>
  40363d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403642:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403649:	00 
  40364a:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403651:	00 
  403652:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403659:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403660:	00 
  403661:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403668:	00 
  403669:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403670:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403674:	48 89 11             	mov    %rdx,(%rcx)
  403677:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40367e:	c3                   	ret
  40367f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403686:	00 
  403687:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  40368c:	0f 94 c0             	sete   %al
  40368f:	24 01                	and    $0x1,%al
  403691:	3c 00                	cmp    $0x0,%al
  403693:	74 09                	je     40369e <runtime::arena_alloc+0x10e>
  403695:	31 c0                	xor    %eax,%eax
  403697:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40369c:	eb 15                	jmp    4036b3 <runtime::arena_alloc+0x123>
  40369e:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4036a5:	00 
  4036a6:	48 8b 40 10          	mov    0x10(%rax),%rax
  4036aa:	48 8b 40 20          	mov    0x20(%rax),%rax
  4036ae:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4036b3:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  4036b8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4036bd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4036c2:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  4036c9:	00 
  4036ca:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4036d1:	00 
  4036d2:	48 8b 78 10          	mov    0x10(%rax),%rdi
  4036d6:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  4036dd:	00 
  4036de:	0f 57 c0             	xorps  %xmm0,%xmm0
  4036e1:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4036e8:	00 
  4036e9:	48 8d 8c 24 e0 00 00 	lea    0xe0(%rsp),%rcx
  4036f0:	00 
  4036f1:	e8 7a f8 ff ff       	call   402f70 <runtime::alloc_from_memory_block>
  4036f6:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  4036fd:	00 
  4036fe:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  403705:	00 
  403706:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  40370d:	00 
  40370e:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403715:	00 
  403716:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  40371d:	80 bc 24 0f 01 00 00 	cmpb   $0x1,0x10f(%rsp)
  403724:	01 
  403725:	0f 94 c0             	sete   %al
  403728:	24 01                	and    $0x1,%al
  40372a:	3c 00                	cmp    $0x0,%al
  40372c:	0f 84 19 02 00 00    	je     40394b <runtime::arena_alloc+0x3bb>
  403732:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403739:	00 
  40373a:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  40373f:	0f 94 c0             	sete   %al
  403742:	24 01                	and    $0x1,%al
  403744:	3c 00                	cmp    $0x0,%al
  403746:	74 10                	je     403758 <runtime::arena_alloc+0x1c8>
  403748:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40374f:	00 
  403750:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  403757:	00 
  403758:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40375d:	48 8b bc 24 00 01 00 	mov    0x100(%rsp),%rdi
  403764:	00 
  403765:	e8 f6 31 00 00       	call   406960 <runtime::arena_alloc.align_forward_uint-0>
  40376a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  403771:	00 
  403772:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403779:	00 
  40377a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403781:	00 
  403782:	48 8b 40 28          	mov    0x28(%rax),%rax
  403786:	48 39 c1             	cmp    %rax,%rcx
  403789:	48 0f 47 c1          	cmova  %rcx,%rax
  40378d:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  403794:	00 
  403795:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40379c:	00 
  40379d:	48 83 38 00          	cmpq   $0x0,(%rax)
  4037a1:	0f 94 c0             	sete   %al
  4037a4:	24 01                	and    $0x1,%al
  4037a6:	3c 00                	cmp    $0x0,%al
  4037a8:	74 46                	je     4037f0 <runtime::arena_alloc+0x260>
  4037aa:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  4037af:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4037b6:	00 
  4037b7:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4037bc:	e8 8f de ff ff       	call   401650 <runtime::heap_allocator>
  4037c1:	48 89 c1             	mov    %rax,%rcx
  4037c4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4037c9:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  4037d0:	00 
  4037d1:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  4037d8:	00 
  4037d9:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4037e0:	00 
  4037e1:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  4037e8:	00 
  4037e9:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4037ed:	48 89 08             	mov    %rcx,(%rax)
  4037f0:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  4037f5:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4037fa:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  4037ff:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403806:	00 
  403807:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  40380e:	00 
  40380f:	48 8b 38             	mov    (%rax),%rdi
  403812:	48 8b 70 08          	mov    0x8(%rax),%rsi
  403816:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  40381d:	00 00 00 00 00 
  403822:	48 89 e0             	mov    %rsp,%rax
  403825:	4c 89 08             	mov    %r9,(%rax)
  403828:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  40382f:	00 
  403830:	e8 eb ec ff ff       	call   402520 <runtime::memory_block_alloc>
  403835:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403839:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  403840:	00 
  403841:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  403846:	3c 00                	cmp    $0x0,%al
  403848:	74 4d                	je     403897 <runtime::arena_alloc+0x307>
  40384a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40384f:	8a 44 24 0f          	mov    0xf(%rsp),%al
  403853:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  40385a:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403861:	00 
  403862:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403869:	00 
  40386a:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403871:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403878:	00 
  403879:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403880:	00 
  403881:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403888:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40388c:	48 89 11             	mov    %rdx,(%rcx)
  40388f:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403896:	c3                   	ret
  403897:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40389c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4038a1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4038a6:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4038ad:	00 
  4038ae:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4038b5:	00 
  4038b6:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  4038bd:	00 
  4038be:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  4038c2:	48 89 08             	mov    %rcx,(%rax)
  4038c5:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4038cc:	00 
  4038cd:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  4038d4:	00 
  4038d5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4038d9:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4038e0:	00 
  4038e1:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  4038e8:	00 
  4038e9:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  4038ed:	48 8b 48 20          	mov    0x20(%rax),%rcx
  4038f1:	48 01 f1             	add    %rsi,%rcx
  4038f4:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4038f8:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  4038ff:	00 00 00 00 00 
  403904:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40390b:	00 
  40390c:	48 8b 78 10          	mov    0x10(%rax),%rdi
  403910:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  403917:	00 
  403918:	0f 57 c0             	xorps  %xmm0,%xmm0
  40391b:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  403920:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  403925:	e8 46 f6 ff ff       	call   402f70 <runtime::alloc_from_memory_block>
  40392a:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40392f:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  403934:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  40393b:	00 
  40393c:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403943:	00 
  403944:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  40394b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403950:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403957:	00 
  403958:	48 8b 70 10          	mov    0x10(%rax),%rsi
  40395c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  403960:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  403964:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  40396b:	00 
  40396c:	48 29 fe             	sub    %rdi,%rsi
  40396f:	48 01 f2             	add    %rsi,%rdx
  403972:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403976:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  40397d:	00 
  40397e:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403985:	00 
  403986:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  40398d:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403994:	00 
  403995:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  40399c:	00 
  40399d:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4039a4:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4039a8:	48 89 11             	mov    %rdx,(%rcx)
  4039ab:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  4039b2:	c3                   	ret
  4039b3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4039ba:	84 00 00 00 00 00 

00000000004039c0 <runtime::print_string>:
  4039c0:	48 83 ec 58          	sub    $0x58,%rsp
  4039c4:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4039c9:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4039ce:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4039d3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4039d8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4039dd:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  4039e2:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  4039e9:	00 00 
  4039eb:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4039f0:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4039f5:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4039fa:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4039ff:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403a06:	00 00 
  403a08:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403a0d:	e8 0e e6 ff ff       	call   402020 <runtime::stderr_write>
  403a12:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403a17:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403a1c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403a21:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403a26:	48 83 c4 58          	add    $0x58,%rsp
  403a2a:	c3                   	ret
  403a2b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403a30 <runtime::mem_alloc>:
  403a30:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  403a37:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  403a3c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  403a41:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403a46:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403a4b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403a50:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  403a55:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403a5c:	00 
  403a5d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403a62:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403a67:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403a6c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403a71:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403a76:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403a7d:	00 
  403a7e:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  403a85:	00 
  403a86:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  403a8d:	00 
  403a8e:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403a95:	00 
  403a96:	e8 65 e9 ff ff       	call   402400 <runtime::is_power_of_two_int>
  403a9b:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403aa0:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403aa5:	0f b6 f8             	movzbl %al,%edi
  403aa8:	be 11 79 40 00       	mov    $0x407911,%esi
  403aad:	ba 20 00 00 00       	mov    $0x20,%edx
  403ab2:	e8 09 26 00 00       	call   4060c0 <runtime::assert>
  403ab7:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403abc:	48 83 f8 00          	cmp    $0x0,%rax
  403ac0:	0f 94 c0             	sete   %al
  403ac3:	24 01                	and    $0x1,%al
  403ac5:	3c 00                	cmp    $0x0,%al
  403ac7:	75 12                	jne    403adb <runtime::mem_alloc+0xab>
  403ac9:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  403ad0:	00 00 
  403ad2:	0f 94 c0             	sete   %al
  403ad5:	24 01                	and    $0x1,%al
  403ad7:	3c 00                	cmp    $0x0,%al
  403ad9:	74 1e                	je     403af9 <runtime::mem_alloc+0xc9>
  403adb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403ae0:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  403ae7:	00 
  403ae8:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  403aef:	31 c0                	xor    %eax,%eax
  403af1:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403af8:	c3                   	ret
  403af9:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403afe:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403b03:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403b08:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  403b0d:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403b14:	00 
  403b15:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  403b1c:	00 
  403b1d:	0f 57 c0             	xorps  %xmm0,%xmm0
  403b20:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  403b25:	48 89 e6             	mov    %rsp,%rsi
  403b28:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  403b2c:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  403b31:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  403b35:	4c 89 06             	mov    %r8,(%rsi)
  403b38:	31 f6                	xor    %esi,%esi
  403b3a:	41 89 f1             	mov    %esi,%r9d
  403b3d:	4d 89 c8             	mov    %r9,%r8
  403b40:	ff d0                	call   *%rax
  403b42:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403b47:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  403b4c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  403b51:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403b55:	48 89 11             	mov    %rdx,(%rcx)
  403b58:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403b5f:	c3                   	ret

0000000000403b60 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  403b60:	48 83 ec 48          	sub    $0x48,%rsp
  403b64:	48 89 0c 24          	mov    %rcx,(%rsp)
  403b68:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  403b6d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403b72:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403b77:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  403b7c:	48 8b 04 24          	mov    (%rsp),%rax
  403b80:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403b85:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403b8a:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403b8f:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403b94:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403b99:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  403b9e:	48 39 c1             	cmp    %rax,%rcx
  403ba1:	48 0f 4c c1          	cmovl  %rcx,%rax
  403ba5:	31 c9                	xor    %ecx,%ecx
  403ba7:	48 39 c1             	cmp    %rax,%rcx
  403baa:	48 0f 4f c1          	cmovg  %rcx,%rax
  403bae:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403bb3:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  403bb9:	0f 9f c0             	setg   %al
  403bbc:	24 01                	and    $0x1,%al
  403bbe:	3c 00                	cmp    $0x0,%al
  403bc0:	74 18                	je     403bda <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)+0x7a>
  403bc2:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403bc7:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403bcc:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  403bd1:	48 c1 e2 00          	shl    $0x0,%rdx
  403bd5:	e8 b6 d4 ff ff       	call   401090 <memmove@plt>
  403bda:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403bdf:	48 83 c4 48          	add    $0x48,%rsp
  403be3:	c3                   	ret
  403be4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403beb:	00 00 00 00 00 

0000000000403bf0 <runtime::print_byte>:
  403bf0:	48 83 ec 68          	sub    $0x68,%rsp
  403bf4:	40 88 f8             	mov    %dil,%al
  403bf7:	88 44 24 07          	mov    %al,0x7(%rsp)
  403bfb:	8a 54 24 07          	mov    0x7(%rsp),%dl
  403bff:	88 54 24 67          	mov    %dl,0x67(%rsp)
  403c03:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  403c0a:	00 00 
  403c0c:	0f 57 c0             	xorps  %xmm0,%xmm0
  403c0f:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403c14:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  403c19:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  403c1e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403c23:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  403c2a:	00 00 
  403c2c:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403c31:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403c36:	88 11                	mov    %dl,(%rcx)
  403c38:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403c3d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403c42:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  403c47:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403c4c:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  403c53:	00 00 
  403c55:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  403c5a:	e8 c1 e3 ff ff       	call   402020 <runtime::stderr_write>
  403c5f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403c64:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403c69:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  403c6e:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403c73:	48 83 c4 68          	add    $0x68,%rsp
  403c77:	c3                   	ret
  403c78:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  403c7f:	00 

0000000000403c80 <runtime::matrix_bounds_check_error>:
  403c80:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403c87:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  403c8c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403c91:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  403c95:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  403c99:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403c9e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403ca3:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  403caa:	00 
  403cab:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403cb0:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403cb7:	00 
  403cb8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403cbd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403cc2:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403cc7:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  403ccc:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403cd1:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  403cd5:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  403cda:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  403cdf:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  403ce4:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  403ce9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  403cf0:	00 
  403cf1:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  403cf6:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  403cfa:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  403cff:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  403d04:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  403d09:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  403d0e:	48 39 c8             	cmp    %rcx,%rax
  403d11:	0f 92 c0             	setb   %al
  403d14:	24 01                	and    $0x1,%al
  403d16:	3c 00                	cmp    $0x0,%al
  403d18:	74 1e                	je     403d38 <runtime::matrix_bounds_check_error+0xb8>
  403d1a:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403d1f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403d24:	48 39 c8             	cmp    %rcx,%rax
  403d27:	0f 92 c0             	setb   %al
  403d2a:	24 01                	and    $0x1,%al
  403d2c:	3c 00                	cmp    $0x0,%al
  403d2e:	74 08                	je     403d38 <runtime::matrix_bounds_check_error+0xb8>
  403d30:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  403d37:	c3                   	ret
  403d38:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  403d3d:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403d42:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  403d46:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  403d4a:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  403d4f:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  403d54:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  403d59:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  403d5e:	48 89 e0             	mov    %rsp,%rax
  403d61:	4c 89 58 08          	mov    %r11,0x8(%rax)
  403d65:	4c 89 10             	mov    %r10,(%rax)
  403d68:	e8 53 2c 00 00       	call   4069c0 <runtime::matrix_bounds_check_error.handle_error-0>
  403d6d:	0f 1f 00             	nopl   (%rax)

0000000000403d70 <runtime::heap_alloc>:
  403d70:	48 83 ec 18          	sub    $0x18,%rsp
  403d74:	48 89 3c 24          	mov    %rdi,(%rsp)
  403d78:	40 88 f0             	mov    %sil,%al
  403d7b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  403d7f:	8a 44 24 0e          	mov    0xe(%rsp),%al
  403d83:	48 8b 3c 24          	mov    (%rsp),%rdi
  403d87:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403d8c:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403d90:	0f b6 f0             	movzbl %al,%esi
  403d93:	e8 a8 e6 ff ff       	call   402440 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  403d98:	48 83 c4 18          	add    $0x18,%rsp
  403d9c:	c3                   	ret
  403d9d:	0f 1f 00             	nopl   (%rax)

0000000000403da0 <runtime::heap_resize>:
  403da0:	48 83 ec 28          	sub    $0x28,%rsp
  403da4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403da9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403dae:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403db3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403db8:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  403dbd:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403dc2:	e8 29 e7 ff ff       	call   4024f0 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  403dc7:	48 83 c4 28          	add    $0x28,%rsp
  403dcb:	c3                   	ret
  403dcc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403dd0 <runtime::heap_free>:
  403dd0:	48 83 ec 18          	sub    $0x18,%rsp
  403dd4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403dd9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403dde:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403de3:	e8 38 ea ff ff       	call   402820 <runtime::[heap_allocator_unix.odin]::_heap_free>
  403de8:	48 83 c4 18          	add    $0x18,%rsp
  403dec:	c3                   	ret
  403ded:	0f 1f 00             	nopl   (%rax)

0000000000403df0 <runtime::mem_free>:
  403df0:	53                   	push   %rbx
  403df1:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  403df8:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  403dfd:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  403e02:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403e07:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403e0c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403e11:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403e16:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  403e1b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  403e20:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  403e27:	00 
  403e28:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  403e2d:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  403e32:	48 83 f8 00          	cmp    $0x0,%rax
  403e36:	0f 94 c0             	sete   %al
  403e39:	24 01                	and    $0x1,%al
  403e3b:	3c 00                	cmp    $0x0,%al
  403e3d:	75 0f                	jne    403e4e <runtime::mem_free+0x5e>
  403e3f:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  403e45:	0f 94 c0             	sete   %al
  403e48:	24 01                	and    $0x1,%al
  403e4a:	3c 00                	cmp    $0x0,%al
  403e4c:	74 0b                	je     403e59 <runtime::mem_free+0x69>
  403e4e:	31 c0                	xor    %eax,%eax
  403e50:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  403e57:	5b                   	pop    %rbx
  403e58:	c3                   	ret
  403e59:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  403e5e:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  403e63:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403e68:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  403e6d:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  403e72:	0f 57 c0             	xorps  %xmm0,%xmm0
  403e75:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  403e7a:	be 01 00 00 00       	mov    $0x1,%esi
  403e7f:	31 c9                	xor    %ecx,%ecx
  403e81:	41 89 c9             	mov    %ecx,%r9d
  403e84:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  403e89:	4c 89 ca             	mov    %r9,%rdx
  403e8c:	4c 89 c9             	mov    %r9,%rcx
  403e8f:	48 89 1c 24          	mov    %rbx,(%rsp)
  403e93:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  403e98:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  403e9d:	ff d0                	call   *%rax
  403e9f:	88 44 24 47          	mov    %al,0x47(%rsp)
  403ea3:	8a 44 24 47          	mov    0x47(%rsp),%al
  403ea7:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  403eae:	5b                   	pop    %rbx
  403eaf:	c3                   	ret

0000000000403eb0 <runtime::print_u64>:
  403eb0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403eb7:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403ebc:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403ec1:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403ec8:	00 
  403ec9:	48 8d 7c 24 5f       	lea    0x5f(%rsp),%rdi
  403ece:	31 f6                	xor    %esi,%esi
  403ed0:	ba 81 00 00 00       	mov    $0x81,%edx
  403ed5:	e8 66 d1 ff ff       	call   401040 <memset@plt>
  403eda:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403edf:	48 c7 44 24 50 81 00 	movq   $0x81,0x50(%rsp)
  403ee6:	00 00 
  403ee8:	48 c7 44 24 48 0a 00 	movq   $0xa,0x48(%rsp)
  403eef:	00 00 
  403ef1:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403ef6:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403efb:	48 3b 44 24 48       	cmp    0x48(%rsp),%rax
  403f00:	0f 93 c0             	setae  %al
  403f03:	24 01                	and    $0x1,%al
  403f05:	3c 00                	cmp    $0x0,%al
  403f07:	74 50                	je     403f59 <runtime::print_u64+0xa9>
  403f09:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403f0e:	48 83 e8 01          	sub    $0x1,%rax
  403f12:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403f17:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403f1c:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403f21:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403f28:	48 8b 08             	mov    (%rax),%rcx
  403f2b:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403f30:	31 d2                	xor    %edx,%edx
  403f32:	48 f7 74 24 48       	divq   0x48(%rsp)
  403f37:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403f3c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403f3f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  403f43:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403f48:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403f4d:	31 d2                	xor    %edx,%edx
  403f4f:	48 f7 f1             	div    %rcx
  403f52:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403f57:	eb 9d                	jmp    403ef6 <runtime::print_u64+0x46>
  403f59:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403f5e:	48 ff c8             	dec    %rax
  403f61:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403f66:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  403f6b:	48 89 04 24          	mov    %rax,(%rsp)
  403f6f:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  403f76:	48 8b 08             	mov    (%rax),%rcx
  403f79:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403f7e:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403f83:	31 d2                	xor    %edx,%edx
  403f85:	48 f7 f6             	div    %rsi
  403f88:	48 8b 04 24          	mov    (%rsp),%rax
  403f8c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  403f8f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  403f93:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  403f98:	48 8d 4c 14 5f       	lea    0x5f(%rsp,%rdx,1),%rcx
  403f9d:	b8 81 00 00 00       	mov    $0x81,%eax
  403fa2:	48 29 d0             	sub    %rdx,%rax
  403fa5:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403faa:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403faf:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403fb4:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403fb9:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403fc0:	00 00 
  403fc2:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403fc7:	e8 54 e0 ff ff       	call   402020 <runtime::stderr_write>
  403fcc:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  403fd3:	c3                   	ret
  403fd4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403fdb:	00 00 00 00 00 

0000000000403fe0 <runtime::print_i64>:
  403fe0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  403fe7:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403fec:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403ff1:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  403ff8:	00 
  403ff9:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404000:	00 
  404001:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  404008:	00 00 
  40400a:	0f 9c c0             	setl   %al
  40400d:	24 01                	and    $0x1,%al
  40400f:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  404016:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40401d:	00 
  40401e:	31 c9                	xor    %ecx,%ecx
  404020:	48 29 c1             	sub    %rax,%rcx
  404023:	48 83 f8 00          	cmp    $0x0,%rax
  404027:	48 0f 4c c1          	cmovl  %rcx,%rax
  40402b:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404032:	00 
  404033:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  404038:	31 f6                	xor    %esi,%esi
  40403a:	ba 81 00 00 00       	mov    $0x81,%edx
  40403f:	e8 fc cf ff ff       	call   401040 <memset@plt>
  404044:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  40404b:	00 00 
  40404d:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  404054:	00 0a 
  404056:	0f 9d c0             	setge  %al
  404059:	24 01                	and    $0x1,%al
  40405b:	3c 00                	cmp    $0x0,%al
  40405d:	74 5c                	je     4040bb <runtime::print_i64+0xdb>
  40405f:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404064:	48 83 e8 01          	sub    $0x1,%rax
  404068:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40406d:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404072:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  404077:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  40407e:	48 8b 08             	mov    (%rax),%rcx
  404081:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404088:	00 
  404089:	be 0a 00 00 00       	mov    $0xa,%esi
  40408e:	48 99                	cqto
  404090:	48 f7 fe             	idiv   %rsi
  404093:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404098:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  40409b:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  40409f:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4040a6:	00 
  4040a7:	b9 0a 00 00 00       	mov    $0xa,%ecx
  4040ac:	48 99                	cqto
  4040ae:	48 f7 f9             	idiv   %rcx
  4040b1:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4040b8:	00 
  4040b9:	eb 92                	jmp    40404d <runtime::print_i64+0x6d>
  4040bb:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4040c0:	48 83 e8 01          	sub    $0x1,%rax
  4040c4:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4040c9:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4040ce:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4040d3:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  4040da:	48 8b 08             	mov    (%rax),%rcx
  4040dd:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4040e4:	00 
  4040e5:	be 0a 00 00 00       	mov    $0xa,%esi
  4040ea:	48 99                	cqto
  4040ec:	48 f7 fe             	idiv   %rsi
  4040ef:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4040f4:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  4040f7:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  4040fb:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  404102:	00 
  404103:	74 18                	je     40411d <runtime::print_i64+0x13d>
  404105:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40410a:	48 83 e8 01          	sub    $0x1,%rax
  40410e:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404113:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404118:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  40411d:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  404122:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  404127:	b8 81 00 00 00       	mov    $0x81,%eax
  40412c:	48 29 d0             	sub    %rdx,%rax
  40412f:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  404134:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404139:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40413e:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  404143:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40414a:	00 00 
  40414c:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  404151:	e8 ca de ff ff       	call   402020 <runtime::stderr_write>
  404156:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40415d:	c3                   	ret
  40415e:	66 90                	xchg   %ax,%ax

0000000000404160 <runtime::arena_free_last_memory_block>:
  404160:	48 83 ec 28          	sub    $0x28,%rsp
  404164:	48 89 3c 24          	mov    %rdi,(%rsp)
  404168:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40416d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404172:	48 8b 04 24          	mov    (%rsp),%rax
  404176:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40417b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404180:	48 8b 40 10          	mov    0x10(%rax),%rax
  404184:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404189:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  40418f:	0f 95 c0             	setne  %al
  404192:	24 01                	and    $0x1,%al
  404194:	3c 00                	cmp    $0x0,%al
  404196:	74 3e                	je     4041d6 <runtime::arena_free_last_memory_block+0x76>
  404198:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40419d:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4041a2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4041a7:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4041ac:	48 8b 09             	mov    (%rcx),%rcx
  4041af:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4041b3:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4041b8:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4041bd:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  4041c1:	48 8b 48 20          	mov    0x20(%rax),%rcx
  4041c5:	48 29 f9             	sub    %rdi,%rcx
  4041c8:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4041cc:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4041d1:	e8 ca eb ff ff       	call   402da0 <runtime::memory_block_dealloc>
  4041d6:	48 83 c4 28          	add    $0x28,%rsp
  4041da:	c3                   	ret
  4041db:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004041e0 <runtime::print_caller_location>:
  4041e0:	50                   	push   %rax
  4041e1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4041e5:	eb 00                	jmp    4041e7 <runtime::print_caller_location+0x7>
  4041e7:	48 8b 04 24          	mov    (%rsp),%rax
  4041eb:	48 8b 38             	mov    (%rax),%rdi
  4041ee:	48 8b 70 08          	mov    0x8(%rax),%rsi
  4041f2:	e8 c9 f7 ff ff       	call   4039c0 <runtime::print_string>
  4041f7:	bf 28 00 00 00       	mov    $0x28,%edi
  4041fc:	e8 ef f9 ff ff       	call   403bf0 <runtime::print_byte>
  404201:	48 8b 04 24          	mov    (%rsp),%rax
  404205:	48 63 78 10          	movslq 0x10(%rax),%rdi
  404209:	e8 a2 fc ff ff       	call   403eb0 <runtime::print_u64>
  40420e:	48 8b 04 24          	mov    (%rsp),%rax
  404212:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  404216:	0f 95 c0             	setne  %al
  404219:	24 01                	and    $0x1,%al
  40421b:	3c 00                	cmp    $0x0,%al
  40421d:	74 17                	je     404236 <runtime::print_caller_location+0x56>
  40421f:	bf 3a 00 00 00       	mov    $0x3a,%edi
  404224:	e8 c7 f9 ff ff       	call   403bf0 <runtime::print_byte>
  404229:	48 8b 04 24          	mov    (%rsp),%rax
  40422d:	48 63 78 14          	movslq 0x14(%rax),%rdi
  404231:	e8 7a fc ff ff       	call   403eb0 <runtime::print_u64>
  404236:	bf 29 00 00 00       	mov    $0x29,%edi
  40423b:	e8 b0 f9 ff ff       	call   403bf0 <runtime::print_byte>
  404240:	58                   	pop    %rax
  404241:	c3                   	ret
  404242:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404249:	1f 84 00 00 00 00 00 

0000000000404250 <runtime::arena_free_all>:
  404250:	48 83 ec 28          	sub    $0x28,%rsp
  404254:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404259:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40425e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  404263:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404268:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40426d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404272:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404277:	0f 95 c0             	setne  %al
  40427a:	24 01                	and    $0x1,%al
  40427c:	3c 00                	cmp    $0x0,%al
  40427e:	74 2c                	je     4042ac <runtime::arena_free_all+0x5c>
  404280:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404285:	48 8b 40 10          	mov    0x10(%rax),%rax
  404289:	48 83 38 00          	cmpq   $0x0,(%rax)
  40428d:	0f 95 c0             	setne  %al
  404290:	24 01                	and    $0x1,%al
  404292:	3c 00                	cmp    $0x0,%al
  404294:	74 16                	je     4042ac <runtime::arena_free_all+0x5c>
  404296:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40429b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4042a0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4042a5:	e8 b6 fe ff ff       	call   404160 <runtime::arena_free_last_memory_block>
  4042aa:	eb c1                	jmp    40426d <runtime::arena_free_all+0x1d>
  4042ac:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4042b1:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  4042b6:	0f 95 c0             	setne  %al
  4042b9:	24 01                	and    $0x1,%al
  4042bb:	3c 00                	cmp    $0x0,%al
  4042bd:	74 32                	je     4042f1 <runtime::arena_free_all+0xa1>
  4042bf:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4042c4:	48 8b 40 10          	mov    0x10(%rax),%rax
  4042c8:	48 8b 78 18          	mov    0x18(%rax),%rdi
  4042cc:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4042d1:	48 8b 40 10          	mov    0x10(%rax),%rax
  4042d5:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4042d9:	31 f6                	xor    %esi,%esi
  4042db:	e8 60 cd ff ff       	call   401040 <memset@plt>
  4042e0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4042e5:	48 8b 40 10          	mov    0x10(%rax),%rax
  4042e9:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  4042f0:	00 
  4042f1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4042f6:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  4042fd:	00 
  4042fe:	48 83 c4 28          	add    $0x28,%rsp
  404302:	c3                   	ret
  404303:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40430a:	84 00 00 00 00 00 

0000000000404310 <runtime::arena_destroy>:
  404310:	48 83 ec 28          	sub    $0x28,%rsp
  404314:	48 89 3c 24          	mov    %rdi,(%rsp)
  404318:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40431d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404322:	48 8b 04 24          	mov    (%rsp),%rax
  404326:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40432b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404330:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404335:	0f 95 c0             	setne  %al
  404338:	24 01                	and    $0x1,%al
  40433a:	3c 00                	cmp    $0x0,%al
  40433c:	74 4e                	je     40438c <runtime::arena_destroy+0x7c>
  40433e:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  404343:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404348:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40434d:	48 8b 40 10          	mov    0x10(%rax),%rax
  404351:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404356:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40435b:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404360:	48 8b 09             	mov    (%rcx),%rcx
  404363:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404367:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40436c:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404371:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  404375:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404379:	48 29 f9             	sub    %rdi,%rcx
  40437c:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404380:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  404385:	e8 16 ea ff ff       	call   402da0 <runtime::memory_block_dealloc>
  40438a:	eb 9f                	jmp    40432b <runtime::arena_destroy+0x1b>
  40438c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404391:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  404398:	00 
  404399:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40439e:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  4043a5:	00 
  4043a6:	48 83 c4 28          	add    $0x28,%rsp
  4043aa:	c3                   	ret
  4043ab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004043b0 <runtime::arena_allocator_proc>:
  4043b0:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  4043b7:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  4043bc:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  4043c3:	00 
  4043c4:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  4043cb:	00 
  4043cc:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  4043d3:	00 
  4043d4:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  4043db:	00 
  4043dc:	40 88 f0             	mov    %sil,%al
  4043df:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  4043e6:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  4043ed:	00 
  4043ee:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4043f5:	00 
  4043f6:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  4043fd:	00 
  4043fe:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  404405:	00 
  404406:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  40440d:	00 
  40440e:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  404415:	00 
  404416:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  40441d:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  404422:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  404429:	00 
  40442a:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404431:	00 
  404432:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  404439:	00 
  40443a:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  404441:	00 
  404442:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  404449:	00 
  40444a:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  404451:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  404458:	00 
  404459:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  404460:	00 
  404461:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  404468:	00 
  404469:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  404470:	00 
  404471:	0f 57 c0             	xorps  %xmm0,%xmm0
  404474:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  40447b:	00 
  40447c:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404483:	00 
  404484:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  40448b:	00 
  40448c:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  404493:	00 
  404494:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  40449b:	00 
  40449c:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  4044a3:	00 
  4044a4:	0f b6 c8             	movzbl %al,%ecx
  4044a7:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4044ac:	2c 07                	sub    $0x7,%al
  4044ae:	0f 87 9a 07 00 00    	ja     404c4e <runtime::arena_allocator_proc+0x89e>
  4044b4:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4044b9:	48 8b 04 c5 f0 76 40 	mov    0x4076f0(,%rax,8),%rax
  4044c0:	00 
  4044c1:	ff e0                	jmp    *%rax
  4044c3:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  4044ca:	00 
  4044cb:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4044d2:	00 
  4044d3:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4044da:	00 
  4044db:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4044e2:	00 
  4044e3:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4044ea:	00 
  4044eb:	0f 57 c0             	xorps  %xmm0,%xmm0
  4044ee:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  4044f5:	00 
  4044f6:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  4044fd:	00 
  4044fe:	e8 8d f0 ff ff       	call   403590 <runtime::arena_alloc>
  404503:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40450a:	00 
  40450b:	40 88 c7             	mov    %al,%dil
  40450e:	40 88 f8             	mov    %dil,%al
  404511:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  404518:	00 
  404519:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  404520:	00 
  404521:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404528:	00 
  404529:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404530:	00 
  404531:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404538:	00 
  404539:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40453d:	48 89 11             	mov    %rdx,(%rcx)
  404540:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404547:	c3                   	ret
  404548:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  40454f:	04 
  404550:	e9 f9 06 00 00       	jmp    404c4e <runtime::arena_allocator_proc+0x89e>
  404555:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  40455c:	00 
  40455d:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  404564:	00 
  404565:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  40456c:	00 
  40456d:	e8 de fc ff ff       	call   404250 <runtime::arena_free_all>
  404572:	e9 d7 06 00 00       	jmp    404c4e <runtime::arena_allocator_proc+0x89e>
  404577:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40457e:	00 
  40457f:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  404586:	00 
  404587:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  40458e:	00 00 
  404590:	0f 94 c1             	sete   %cl
  404593:	80 e1 01             	and    $0x1,%cl
  404596:	b0 01                	mov    $0x1,%al
  404598:	38 c8                	cmp    %cl,%al
  40459a:	74 25                	je     4045c1 <runtime::arena_allocator_proc+0x211>
  40459c:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  4045a3:	00 
  4045a4:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  4045ab:	00 
  4045ac:	0f 94 c1             	sete   %cl
  4045af:	80 e1 01             	and    $0x1,%cl
  4045b2:	b0 01                	mov    $0x1,%al
  4045b4:	38 c8                	cmp    %cl,%al
  4045b6:	0f 84 a8 00 00 00    	je     404664 <runtime::arena_allocator_proc+0x2b4>
  4045bc:	e9 85 00 00 00       	jmp    404646 <runtime::arena_allocator_proc+0x296>
  4045c1:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  4045c8:	00 
  4045c9:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4045d0:	00 
  4045d1:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4045d8:	00 
  4045d9:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4045e0:	00 
  4045e1:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4045e8:	00 
  4045e9:	0f 57 c0             	xorps  %xmm0,%xmm0
  4045ec:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  4045f3:	00 
  4045f4:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  4045fb:	00 
  4045fc:	e8 8f ef ff ff       	call   403590 <runtime::arena_alloc>
  404601:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404608:	00 
  404609:	40 88 c7             	mov    %al,%dil
  40460c:	40 88 f8             	mov    %dil,%al
  40460f:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  404616:	00 
  404617:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  40461e:	00 
  40461f:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404626:	00 
  404627:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40462e:	00 
  40462f:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404636:	00 
  404637:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40463b:	48 89 11             	mov    %rdx,(%rcx)
  40463e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404645:	c3                   	ret
  404646:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  40464d:	00 00 
  40464f:	0f 94 c1             	sete   %cl
  404652:	80 e1 01             	and    $0x1,%cl
  404655:	b0 01                	mov    $0x1,%al
  404657:	38 c8                	cmp    %cl,%al
  404659:	0f 84 e5 00 00 00    	je     404744 <runtime::arena_allocator_proc+0x394>
  40465f:	e9 b7 00 00 00       	jmp    40471b <runtime::arena_allocator_proc+0x36b>
  404664:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40466b:	00 
  40466c:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  404671:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404678:	00 
  404679:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  40467e:	bf 40 77 40 00       	mov    $0x407740,%edi
  404683:	31 c0                	xor    %eax,%eax
  404685:	41 89 c0             	mov    %eax,%r8d
  404688:	be 3e 00 00 00       	mov    $0x3e,%esi
  40468d:	ba d1 00 00 00       	mov    $0xd1,%edx
  404692:	b9 13 00 00 00       	mov    $0x13,%ecx
  404697:	e8 94 eb ff ff       	call   403230 <runtime::multi_pointer_slice_expr_error>
  40469c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4046a1:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4046a6:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4046ad:	00 
  4046ae:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  4046b5:	00 
  4046b6:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  4046bd:	00 
  4046be:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  4046c5:	00 
  4046c6:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  4046cd:	00 
  4046ce:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4046d5:	00 
  4046d6:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4046dd:	00 
  4046de:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4046e5:	00 
  4046e6:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4046ed:	00 
  4046ee:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4046f5:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4046fc:	00 
  4046fd:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404704:	00 
  404705:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40470c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404710:	48 89 11             	mov    %rdx,(%rcx)
  404713:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40471a:	c3                   	ret
  40471b:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404722:	00 
  404723:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  40472a:	00 
  40472b:	48 83 e9 01          	sub    $0x1,%rcx
  40472f:	48 21 c8             	and    %rcx,%rax
  404732:	48 83 f8 00          	cmp    $0x0,%rax
  404736:	0f 94 c1             	sete   %cl
  404739:	80 e1 01             	and    $0x1,%cl
  40473c:	b0 01                	mov    $0x1,%al
  40473e:	38 c8                	cmp    %cl,%al
  404740:	74 54                	je     404796 <runtime::arena_allocator_proc+0x3e6>
  404742:	eb 4d                	jmp    404791 <runtime::arena_allocator_proc+0x3e1>
  404744:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40474b:	00 
  40474c:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404753:	04 
  404754:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  40475b:	00 
  40475c:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404763:	00 
  404764:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  40476b:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404772:	00 
  404773:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  40477a:	00 
  40477b:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404782:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404786:	48 89 11             	mov    %rdx,(%rcx)
  404789:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404790:	c3                   	ret
  404791:	e9 94 02 00 00       	jmp    404a2a <runtime::arena_allocator_proc+0x67a>
  404796:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  40479d:	00 
  40479e:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  4047a5:	00 
  4047a6:	0f 92 c0             	setb   %al
  4047a9:	24 01                	and    $0x1,%al
  4047ab:	3c 00                	cmp    $0x0,%al
  4047ad:	0f 84 b7 00 00 00    	je     40486a <runtime::arena_allocator_proc+0x4ba>
  4047b3:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4047ba:	00 
  4047bb:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4047c0:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  4047c7:	00 
  4047c8:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  4047cd:	bf 40 77 40 00       	mov    $0x407740,%edi
  4047d2:	31 c0                	xor    %eax,%eax
  4047d4:	41 89 c0             	mov    %eax,%r8d
  4047d7:	be 3e 00 00 00       	mov    $0x3e,%esi
  4047dc:	ba d9 00 00 00       	mov    $0xd9,%edx
  4047e1:	b9 14 00 00 00       	mov    $0x14,%ecx
  4047e6:	e8 45 ea ff ff       	call   403230 <runtime::multi_pointer_slice_expr_error>
  4047eb:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4047f0:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4047f5:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4047fc:	00 
  4047fd:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  404804:	00 
  404805:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  40480c:	00 
  40480d:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  404814:	00 
  404815:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  40481c:	00 
  40481d:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404824:	00 
  404825:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  40482c:	00 
  40482d:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404834:	00 
  404835:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  40483c:	00 
  40483d:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404844:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  40484b:	00 
  40484c:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404853:	00 
  404854:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  40485b:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40485f:	48 89 11             	mov    %rdx,(%rcx)
  404862:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404869:	c3                   	ret
  40486a:	eb 00                	jmp    40486c <runtime::arena_allocator_proc+0x4bc>
  40486c:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  404873:	00 
  404874:	48 8b 40 10          	mov    0x10(%rax),%rax
  404878:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40487f:	00 
  404880:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  404887:	00 00 
  404889:	0f 95 c0             	setne  %al
  40488c:	24 01                	and    $0x1,%al
  40488e:	3c 00                	cmp    $0x0,%al
  404890:	0f 84 92 01 00 00    	je     404a28 <runtime::arena_allocator_proc+0x678>
  404896:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40489d:	00 
  40489e:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  4048a5:	00 
  4048a6:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  4048aa:	48 29 c8             	sub    %rcx,%rax
  4048ad:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  4048b4:	00 
  4048b5:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  4048bc:	00 
  4048bd:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  4048c4:	00 
  4048c5:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  4048cc:	00 
  4048cd:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  4048d4:	00 
  4048d5:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  4048dc:	00 
  4048dd:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  4048e4:	00 
  4048e5:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  4048ec:	00 
  4048ed:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  4048f4:	00 
  4048f5:	0f 92 c0             	setb   %al
  4048f8:	24 01                	and    $0x1,%al
  4048fa:	3c 00                	cmp    $0x0,%al
  4048fc:	0f 84 24 01 00 00    	je     404a26 <runtime::arena_allocator_proc+0x676>
  404902:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  404909:	00 
  40490a:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404911:	00 
  404912:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  404916:	0f 94 c0             	sete   %al
  404919:	24 01                	and    $0x1,%al
  40491b:	3c 00                	cmp    $0x0,%al
  40491d:	0f 84 03 01 00 00    	je     404a26 <runtime::arena_allocator_proc+0x676>
  404923:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  40492a:	00 
  40492b:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404932:	00 
  404933:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  404937:	0f 96 c0             	setbe  %al
  40493a:	24 01                	and    $0x1,%al
  40493c:	3c 00                	cmp    $0x0,%al
  40493e:	0f 84 e2 00 00 00    	je     404a26 <runtime::arena_allocator_proc+0x676>
  404944:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40494b:	00 
  40494c:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  404953:	00 
  404954:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404958:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40495f:	00 
  404960:	48 8b 40 18          	mov    0x18(%rax),%rax
  404964:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404969:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  404970:	00 
  404971:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  404976:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  40497d:	00 
  40497e:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  404983:	bf 40 77 40 00       	mov    $0x407740,%edi
  404988:	be 3e 00 00 00       	mov    $0x3e,%esi
  40498d:	ba e4 00 00 00       	mov    $0xe4,%edx
  404992:	b9 17 00 00 00       	mov    $0x17,%ecx
  404997:	e8 94 e8 ff ff       	call   403230 <runtime::multi_pointer_slice_expr_error>
  40499c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4049a1:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4049a6:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4049ab:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4049b2:	00 
  4049b3:	48 01 f2             	add    %rsi,%rdx
  4049b6:	48 29 f0             	sub    %rsi,%rax
  4049b9:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  4049c0:	00 
  4049c1:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4049c8:	00 
  4049c9:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  4049d0:	00 
  4049d1:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  4049d8:	00 
  4049d9:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4049e0:	00 
  4049e1:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4049e8:	00 
  4049e9:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4049f0:	00 
  4049f1:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4049f8:	00 
  4049f9:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404a00:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404a07:	00 
  404a08:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404a0f:	00 
  404a10:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404a17:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404a1b:	48 89 11             	mov    %rdx,(%rcx)
  404a1e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404a25:	c3                   	ret
  404a26:	eb 00                	jmp    404a28 <runtime::arena_allocator_proc+0x678>
  404a28:	eb 00                	jmp    404a2a <runtime::arena_allocator_proc+0x67a>
  404a2a:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404a31:	00 
  404a32:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404a39:	00 
  404a3a:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404a41:	00 
  404a42:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404a49:	00 
  404a4a:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404a51:	00 
  404a52:	0f 57 c0             	xorps  %xmm0,%xmm0
  404a55:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  404a5c:	00 
  404a5d:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  404a64:	00 
  404a65:	e8 26 eb ff ff       	call   403590 <runtime::arena_alloc>
  404a6a:	88 44 24 27          	mov    %al,0x27(%rsp)
  404a6e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  404a75:	00 
  404a76:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  404a7b:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  404a82:	00 
  404a83:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404a88:	3c 00                	cmp    $0x0,%al
  404a8a:	74 50                	je     404adc <runtime::arena_allocator_proc+0x72c>
  404a8c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404a93:	00 
  404a94:	8a 44 24 27          	mov    0x27(%rsp),%al
  404a98:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404a9f:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404aa6:	00 
  404aa7:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404aae:	00 
  404aaf:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404ab6:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404abd:	00 
  404abe:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404ac5:	00 
  404ac6:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404acd:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404ad1:	48 89 11             	mov    %rdx,(%rcx)
  404ad4:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404adb:	c3                   	ret
  404adc:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404ae1:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  404ae6:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  404aed:	00 
  404aee:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404af5:	00 
  404af6:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  404afd:	00 00 
  404aff:	0f 94 c0             	sete   %al
  404b02:	24 01                	and    $0x1,%al
  404b04:	3c 00                	cmp    $0x0,%al
  404b06:	74 45                	je     404b4d <runtime::arena_allocator_proc+0x79d>
  404b08:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404b0f:	00 
  404b10:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404b17:	00 
  404b18:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404b1f:	00 
  404b20:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404b27:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404b2e:	00 
  404b2f:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404b36:	00 
  404b37:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404b3e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404b42:	48 89 11             	mov    %rdx,(%rcx)
  404b45:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404b4c:	c3                   	ret
  404b4d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404b54:	00 
  404b55:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  404b5a:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  404b61:	00 
  404b62:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404b67:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404b6e:	00 
  404b6f:	48 89 04 24          	mov    %rax,(%rsp)
  404b73:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  404b7a:	00 
  404b7b:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  404b80:	bf 40 77 40 00       	mov    $0x407740,%edi
  404b85:	31 c0                	xor    %eax,%eax
  404b87:	41 89 c0             	mov    %eax,%r8d
  404b8a:	be 3e 00 00 00       	mov    $0x3e,%esi
  404b8f:	ba ee 00 00 00       	mov    $0xee,%edx
  404b94:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  404b99:	e8 92 e6 ff ff       	call   403230 <runtime::multi_pointer_slice_expr_error>
  404b9e:	48 8b 0c 24          	mov    (%rsp),%rcx
  404ba2:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404ba7:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  404bac:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404bb1:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  404bb8:	00 
  404bb9:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404bc0:	00 
  404bc1:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  404bc8:	00 
  404bc9:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  404bd0:	00 
  404bd1:	e8 8a ef ff ff       	call   403b60 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  404bd6:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  404bdd:	00 
  404bde:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  404be5:	00 
  404be6:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  404bed:	00 
  404bee:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404bf5:	00 
  404bf6:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  404bfd:	00 
  404bfe:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404c05:	00 
  404c06:	48 89 50 08          	mov    %rdx,0x8(%rax)
  404c0a:	48 89 08             	mov    %rcx,(%rax)
  404c0d:	31 c0                	xor    %eax,%eax
  404c0f:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404c16:	c3                   	ret
  404c17:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404c1e:	00 
  404c1f:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  404c26:	00 
  404c27:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  404c2e:	00 00 
  404c30:	0f 95 c0             	setne  %al
  404c33:	24 01                	and    $0x1,%al
  404c35:	3c 00                	cmp    $0x0,%al
  404c37:	74 0b                	je     404c44 <runtime::arena_allocator_proc+0x894>
  404c39:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404c40:	00 
  404c41:	c6 00 5d             	movb   $0x5d,(%rax)
  404c44:	eb 08                	jmp    404c4e <runtime::arena_allocator_proc+0x89e>
  404c46:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404c4d:	04 
  404c4e:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404c55:	00 
  404c56:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404c5d:	00 
  404c5e:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404c65:	00 
  404c66:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404c6d:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404c74:	00 
  404c75:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404c7c:	00 
  404c7d:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404c84:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404c88:	48 89 11             	mov    %rdx,(%rcx)
  404c8b:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404c92:	c3                   	ret
  404c93:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404c9a:	84 00 00 00 00 00 

0000000000404ca0 <runtime::memory_equal>:
  404ca0:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  404ca5:	48 89 74 24 b8       	mov    %rsi,-0x48(%rsp)
  404caa:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  404caf:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404cb4:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404cb9:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  404cbe:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404cc3:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404cc8:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  404ccd:	48 83 f8 00          	cmp    $0x0,%rax
  404cd1:	0f 94 c1             	sete   %cl
  404cd4:	80 e1 01             	and    $0x1,%cl
  404cd7:	b0 01                	mov    $0x1,%al
  404cd9:	38 c8                	cmp    %cl,%al
  404cdb:	74 1b                	je     404cf8 <runtime::memory_equal+0x58>
  404cdd:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404ce2:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404ce7:	48 39 c8             	cmp    %rcx,%rax
  404cea:	0f 94 c1             	sete   %cl
  404ced:	80 e1 01             	and    $0x1,%cl
  404cf0:	b0 01                	mov    $0x1,%al
  404cf2:	38 c8                	cmp    %cl,%al
  404cf4:	74 07                	je     404cfd <runtime::memory_equal+0x5d>
  404cf6:	eb 03                	jmp    404cfb <runtime::memory_equal+0x5b>
  404cf8:	b0 01                	mov    $0x1,%al
  404cfa:	c3                   	ret
  404cfb:	eb 03                	jmp    404d00 <runtime::memory_equal+0x60>
  404cfd:	b0 01                	mov    $0x1,%al
  404cff:	c3                   	ret
  404d00:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404d05:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404d0a:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  404d0f:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  404d14:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  404d19:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404d1e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  404d25:	00 00 
  404d27:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404d2c:	48 3b 44 24 d0       	cmp    -0x30(%rsp),%rax
  404d31:	0f 92 c0             	setb   %al
  404d34:	24 01                	and    $0x1,%al
  404d36:	3c 00                	cmp    $0x0,%al
  404d38:	74 38                	je     404d72 <runtime::memory_equal+0xd2>
  404d3a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404d3f:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404d44:	8a 04 08             	mov    (%rax,%rcx,1),%al
  404d47:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  404d4c:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  404d51:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  404d54:	0f 95 c0             	setne  %al
  404d57:	24 01                	and    $0x1,%al
  404d59:	3c 00                	cmp    $0x0,%al
  404d5b:	74 03                	je     404d60 <runtime::memory_equal+0xc0>
  404d5d:	31 c0                	xor    %eax,%eax
  404d5f:	c3                   	ret
  404d60:	eb 00                	jmp    404d62 <runtime::memory_equal+0xc2>
  404d62:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404d67:	48 83 c0 01          	add    $0x1,%rax
  404d6b:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404d70:	eb b5                	jmp    404d27 <runtime::memory_equal+0x87>
  404d72:	b0 01                	mov    $0x1,%al
  404d74:	c3                   	ret
  404d75:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  404d7c:	00 00 00 00 

0000000000404d80 <runtime::memory_compare>:
  404d80:	48 83 ec 10          	sub    $0x10,%rsp
  404d84:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  404d89:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  404d8e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  404d93:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404d98:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  404d9d:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  404da2:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  404da7:	48 89 0c 24          	mov    %rcx,(%rsp)
  404dab:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404db0:	48 39 c8             	cmp    %rcx,%rax
  404db3:	0f 94 c1             	sete   %cl
  404db6:	80 e1 01             	and    $0x1,%cl
  404db9:	b0 01                	mov    $0x1,%al
  404dbb:	38 c8                	cmp    %cl,%al
  404dbd:	74 17                	je     404dd6 <runtime::memory_compare+0x56>
  404dbf:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  404dc4:	48 83 f8 00          	cmp    $0x0,%rax
  404dc8:	0f 94 c1             	sete   %cl
  404dcb:	80 e1 01             	and    $0x1,%cl
  404dce:	b0 01                	mov    $0x1,%al
  404dd0:	38 c8                	cmp    %cl,%al
  404dd2:	74 20                	je     404df4 <runtime::memory_compare+0x74>
  404dd4:	eb 07                	jmp    404ddd <runtime::memory_compare+0x5d>
  404dd6:	31 c0                	xor    %eax,%eax
  404dd8:	48 83 c4 10          	add    $0x10,%rsp
  404ddc:	c3                   	ret
  404ddd:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  404de2:	48 83 f8 00          	cmp    $0x0,%rax
  404de6:	0f 94 c1             	sete   %cl
  404de9:	80 e1 01             	and    $0x1,%cl
  404dec:	b0 01                	mov    $0x1,%al
  404dee:	38 c8                	cmp    %cl,%al
  404df0:	74 10                	je     404e02 <runtime::memory_compare+0x82>
  404df2:	eb 0c                	jmp    404e00 <runtime::memory_compare+0x80>
  404df4:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404dfb:	48 83 c4 10          	add    $0x10,%rsp
  404dff:	c3                   	ret
  404e00:	eb 0a                	jmp    404e0c <runtime::memory_compare+0x8c>
  404e02:	b8 01 00 00 00       	mov    $0x1,%eax
  404e07:	48 83 c4 10          	add    $0x10,%rsp
  404e0b:	c3                   	ret
  404e0c:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  404e11:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  404e16:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  404e1b:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  404e20:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  404e25:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  404e2a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  404e2f:	48 c1 e8 03          	shr    $0x3,%rax
  404e33:	48 83 c0 01          	add    $0x1,%rax
  404e37:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  404e3c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  404e41:	48 83 e8 01          	sub    $0x1,%rax
  404e45:	48 c1 e0 03          	shl    $0x3,%rax
  404e49:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404e4e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  404e55:	00 00 
  404e57:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  404e5d:	0f 92 c0             	setb   %al
  404e60:	24 01                	and    $0x1,%al
  404e62:	3c 00                	cmp    $0x0,%al
  404e64:	74 09                	je     404e6f <runtime::memory_compare+0xef>
  404e66:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404e6d:	00 00 
  404e6f:	eb 00                	jmp    404e71 <runtime::memory_compare+0xf1>
  404e71:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404e76:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  404e7b:	0f 92 c0             	setb   %al
  404e7e:	24 01                	and    $0x1,%al
  404e80:	3c 00                	cmp    $0x0,%al
  404e82:	0f 84 11 01 00 00    	je     404f99 <runtime::memory_compare+0x219>
  404e88:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404e8d:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404e92:	48 c1 e1 03          	shl    $0x3,%rcx
  404e96:	48 01 c8             	add    %rcx,%rax
  404e99:	48 8b 00             	mov    (%rax),%rax
  404e9c:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  404ea1:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404ea6:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  404eab:	48 c1 e1 03          	shl    $0x3,%rcx
  404eaf:	48 01 c8             	add    %rcx,%rax
  404eb2:	48 8b 00             	mov    (%rax),%rax
  404eb5:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  404eba:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404ebf:	48 33 44 24 b8       	xor    -0x48(%rsp),%rax
  404ec4:	48 83 f8 00          	cmp    $0x0,%rax
  404ec8:	0f 95 c0             	setne  %al
  404ecb:	24 01                	and    $0x1,%al
  404ecd:	3c 00                	cmp    $0x0,%al
  404ecf:	0f 84 af 00 00 00    	je     404f84 <runtime::memory_compare+0x204>
  404ed5:	eb 00                	jmp    404ed7 <runtime::memory_compare+0x157>
  404ed7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404edc:	48 c1 e0 03          	shl    $0x3,%rax
  404ee0:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  404ee5:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404eea:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404eef:	0f 92 c0             	setb   %al
  404ef2:	24 01                	and    $0x1,%al
  404ef4:	3c 00                	cmp    $0x0,%al
  404ef6:	0f 84 86 00 00 00    	je     404f82 <runtime::memory_compare+0x202>
  404efc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404f01:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  404f06:	8a 00                	mov    (%rax),%al
  404f08:	88 44 24 af          	mov    %al,-0x51(%rsp)
  404f0c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404f11:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  404f16:	8a 00                	mov    (%rax),%al
  404f18:	88 44 24 ae          	mov    %al,-0x52(%rsp)
  404f1c:	8a 44 24 af          	mov    -0x51(%rsp),%al
  404f20:	32 44 24 ae          	xor    -0x52(%rsp),%al
  404f24:	3c 00                	cmp    $0x0,%al
  404f26:	0f 95 c0             	setne  %al
  404f29:	24 01                	and    $0x1,%al
  404f2b:	3c 00                	cmp    $0x0,%al
  404f2d:	74 3e                	je     404f6d <runtime::memory_compare+0x1ed>
  404f2f:	0f b6 44 24 af       	movzbl -0x51(%rsp),%eax
  404f34:	0f b6 4c 24 ae       	movzbl -0x52(%rsp),%ecx
  404f39:	48 29 c8             	sub    %rcx,%rax
  404f3c:	48 83 f8 00          	cmp    $0x0,%rax
  404f40:	0f 9c c0             	setl   %al
  404f43:	24 01                	and    $0x1,%al
  404f45:	3c 00                	cmp    $0x0,%al
  404f47:	74 0e                	je     404f57 <runtime::memory_compare+0x1d7>
  404f49:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  404f50:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  404f55:	eb 0c                	jmp    404f63 <runtime::memory_compare+0x1e3>
  404f57:	b8 01 00 00 00       	mov    $0x1,%eax
  404f5c:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  404f61:	eb 00                	jmp    404f63 <runtime::memory_compare+0x1e3>
  404f63:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  404f68:	48 83 c4 10          	add    $0x10,%rsp
  404f6c:	c3                   	ret
  404f6d:	eb 00                	jmp    404f6f <runtime::memory_compare+0x1ef>
  404f6f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404f74:	48 83 c0 01          	add    $0x1,%rax
  404f78:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  404f7d:	e9 63 ff ff ff       	jmp    404ee5 <runtime::memory_compare+0x165>
  404f82:	eb 00                	jmp    404f84 <runtime::memory_compare+0x204>
  404f84:	eb 00                	jmp    404f86 <runtime::memory_compare+0x206>
  404f86:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404f8b:	48 83 c0 01          	add    $0x1,%rax
  404f8f:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  404f94:	e9 d8 fe ff ff       	jmp    404e71 <runtime::memory_compare+0xf1>
  404f99:	eb 00                	jmp    404f9b <runtime::memory_compare+0x21b>
  404f9b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404fa0:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  404fa5:	0f 92 c0             	setb   %al
  404fa8:	24 01                	and    $0x1,%al
  404faa:	3c 00                	cmp    $0x0,%al
  404fac:	0f 84 86 00 00 00    	je     405038 <runtime::memory_compare+0x2b8>
  404fb2:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404fb7:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404fbc:	8a 00                	mov    (%rax),%al
  404fbe:	88 44 24 ad          	mov    %al,-0x53(%rsp)
  404fc2:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404fc7:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  404fcc:	8a 00                	mov    (%rax),%al
  404fce:	88 44 24 ac          	mov    %al,-0x54(%rsp)
  404fd2:	8a 44 24 ad          	mov    -0x53(%rsp),%al
  404fd6:	32 44 24 ac          	xor    -0x54(%rsp),%al
  404fda:	3c 00                	cmp    $0x0,%al
  404fdc:	0f 95 c0             	setne  %al
  404fdf:	24 01                	and    $0x1,%al
  404fe1:	3c 00                	cmp    $0x0,%al
  404fe3:	74 3e                	je     405023 <runtime::memory_compare+0x2a3>
  404fe5:	0f b6 44 24 ad       	movzbl -0x53(%rsp),%eax
  404fea:	0f b6 4c 24 ac       	movzbl -0x54(%rsp),%ecx
  404fef:	48 29 c8             	sub    %rcx,%rax
  404ff2:	48 83 f8 00          	cmp    $0x0,%rax
  404ff6:	0f 9c c0             	setl   %al
  404ff9:	24 01                	and    $0x1,%al
  404ffb:	3c 00                	cmp    $0x0,%al
  404ffd:	74 0e                	je     40500d <runtime::memory_compare+0x28d>
  404fff:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405006:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40500b:	eb 0c                	jmp    405019 <runtime::memory_compare+0x299>
  40500d:	b8 01 00 00 00       	mov    $0x1,%eax
  405012:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  405017:	eb 00                	jmp    405019 <runtime::memory_compare+0x299>
  405019:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  40501e:	48 83 c4 10          	add    $0x10,%rsp
  405022:	c3                   	ret
  405023:	eb 00                	jmp    405025 <runtime::memory_compare+0x2a5>
  405025:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40502a:	48 83 c0 01          	add    $0x1,%rax
  40502e:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405033:	e9 63 ff ff ff       	jmp    404f9b <runtime::memory_compare+0x21b>
  405038:	31 c0                	xor    %eax,%eax
  40503a:	48 83 c4 10          	add    $0x10,%rsp
  40503e:	c3                   	ret
  40503f:	90                   	nop

0000000000405040 <runtime::memory_compare_zero>:
  405040:	48 89 7c 24 a0       	mov    %rdi,-0x60(%rsp)
  405045:	48 89 74 24 a8       	mov    %rsi,-0x58(%rsp)
  40504a:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40504f:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  405054:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405059:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40505e:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405063:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405068:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40506d:	48 c1 e8 03          	shr    $0x3,%rax
  405071:	48 83 c0 01          	add    $0x1,%rax
  405075:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40507a:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40507f:	48 83 e8 01          	sub    $0x1,%rax
  405083:	48 c1 e0 03          	shl    $0x3,%rax
  405087:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40508c:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  405093:	00 00 
  405095:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  40509b:	0f 92 c0             	setb   %al
  40509e:	24 01                	and    $0x1,%al
  4050a0:	3c 00                	cmp    $0x0,%al
  4050a2:	74 09                	je     4050ad <runtime::memory_compare_zero+0x6d>
  4050a4:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4050ab:	00 00 
  4050ad:	eb 00                	jmp    4050af <runtime::memory_compare_zero+0x6f>
  4050af:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4050b4:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  4050b9:	0f 92 c0             	setb   %al
  4050bc:	24 01                	and    $0x1,%al
  4050be:	3c 00                	cmp    $0x0,%al
  4050c0:	0f 84 d2 00 00 00    	je     405198 <runtime::memory_compare_zero+0x158>
  4050c6:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4050cb:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4050d0:	48 c1 e1 03          	shl    $0x3,%rcx
  4050d4:	48 01 c8             	add    %rcx,%rax
  4050d7:	48 8b 00             	mov    (%rax),%rax
  4050da:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4050df:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4050e4:	48 83 f0 00          	xor    $0x0,%rax
  4050e8:	48 83 f8 00          	cmp    $0x0,%rax
  4050ec:	0f 95 c0             	setne  %al
  4050ef:	24 01                	and    $0x1,%al
  4050f1:	3c 00                	cmp    $0x0,%al
  4050f3:	0f 84 8a 00 00 00    	je     405183 <runtime::memory_compare_zero+0x143>
  4050f9:	eb 00                	jmp    4050fb <runtime::memory_compare_zero+0xbb>
  4050fb:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405100:	48 c1 e0 03          	shl    $0x3,%rax
  405104:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405109:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40510e:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  405113:	0f 92 c0             	setb   %al
  405116:	24 01                	and    $0x1,%al
  405118:	3c 00                	cmp    $0x0,%al
  40511a:	74 65                	je     405181 <runtime::memory_compare_zero+0x141>
  40511c:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405121:	48 03 44 24 b8       	add    -0x48(%rsp),%rax
  405126:	8a 00                	mov    (%rax),%al
  405128:	88 44 24 b7          	mov    %al,-0x49(%rsp)
  40512c:	8a 44 24 b7          	mov    -0x49(%rsp),%al
  405130:	34 00                	xor    $0x0,%al
  405132:	3c 00                	cmp    $0x0,%al
  405134:	0f 95 c0             	setne  %al
  405137:	24 01                	and    $0x1,%al
  405139:	3c 00                	cmp    $0x0,%al
  40513b:	74 32                	je     40516f <runtime::memory_compare_zero+0x12f>
  40513d:	0f b6 44 24 b7       	movzbl -0x49(%rsp),%eax
  405142:	48 83 f8 00          	cmp    $0x0,%rax
  405146:	0f 9c c0             	setl   %al
  405149:	24 01                	and    $0x1,%al
  40514b:	3c 00                	cmp    $0x0,%al
  40514d:	74 0e                	je     40515d <runtime::memory_compare_zero+0x11d>
  40514f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405156:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40515b:	eb 0c                	jmp    405169 <runtime::memory_compare_zero+0x129>
  40515d:	b8 01 00 00 00       	mov    $0x1,%eax
  405162:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405167:	eb 00                	jmp    405169 <runtime::memory_compare_zero+0x129>
  405169:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40516e:	c3                   	ret
  40516f:	eb 00                	jmp    405171 <runtime::memory_compare_zero+0x131>
  405171:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405176:	48 83 c0 01          	add    $0x1,%rax
  40517a:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40517f:	eb 88                	jmp    405109 <runtime::memory_compare_zero+0xc9>
  405181:	eb 00                	jmp    405183 <runtime::memory_compare_zero+0x143>
  405183:	eb 00                	jmp    405185 <runtime::memory_compare_zero+0x145>
  405185:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40518a:	48 83 c0 01          	add    $0x1,%rax
  40518e:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  405193:	e9 17 ff ff ff       	jmp    4050af <runtime::memory_compare_zero+0x6f>
  405198:	eb 00                	jmp    40519a <runtime::memory_compare_zero+0x15a>
  40519a:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40519f:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4051a4:	0f 92 c0             	setb   %al
  4051a7:	24 01                	and    $0x1,%al
  4051a9:	3c 00                	cmp    $0x0,%al
  4051ab:	74 65                	je     405212 <runtime::memory_compare_zero+0x1d2>
  4051ad:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4051b2:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  4051b7:	8a 00                	mov    (%rax),%al
  4051b9:	88 44 24 b6          	mov    %al,-0x4a(%rsp)
  4051bd:	8a 44 24 b6          	mov    -0x4a(%rsp),%al
  4051c1:	34 00                	xor    $0x0,%al
  4051c3:	3c 00                	cmp    $0x0,%al
  4051c5:	0f 95 c0             	setne  %al
  4051c8:	24 01                	and    $0x1,%al
  4051ca:	3c 00                	cmp    $0x0,%al
  4051cc:	74 32                	je     405200 <runtime::memory_compare_zero+0x1c0>
  4051ce:	0f b6 44 24 b6       	movzbl -0x4a(%rsp),%eax
  4051d3:	48 83 f8 00          	cmp    $0x0,%rax
  4051d7:	0f 9c c0             	setl   %al
  4051da:	24 01                	and    $0x1,%al
  4051dc:	3c 00                	cmp    $0x0,%al
  4051de:	74 0e                	je     4051ee <runtime::memory_compare_zero+0x1ae>
  4051e0:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4051e7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  4051ec:	eb 0c                	jmp    4051fa <runtime::memory_compare_zero+0x1ba>
  4051ee:	b8 01 00 00 00       	mov    $0x1,%eax
  4051f3:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  4051f8:	eb 00                	jmp    4051fa <runtime::memory_compare_zero+0x1ba>
  4051fa:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  4051ff:	c3                   	ret
  405200:	eb 00                	jmp    405202 <runtime::memory_compare_zero+0x1c2>
  405202:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405207:	48 83 c0 01          	add    $0x1,%rax
  40520b:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405210:	eb 88                	jmp    40519a <runtime::memory_compare_zero+0x15a>
  405212:	31 c0                	xor    %eax,%eax
  405214:	c3                   	ret
  405215:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40521c:	00 00 00 00 

0000000000405220 <runtime::__type_info_of>:
  405220:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  405225:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40522a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40522f:	48 c7 c1 80 71 40 00 	mov    $0x407180,%rcx
  405236:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  40523a:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40523f:	31 c9                	xor    %ecx,%ecx
  405241:	89 ca                	mov    %ecx,%edx
  405243:	48 f7 74 24 f0       	divq   -0x10(%rsp)
  405248:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  40524d:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  405254:	00 00 
  405256:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  40525d:	00 00 
  40525f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405264:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  405269:	0f 83 9f 00 00 00    	jae    40530e <runtime::__type_info_of+0xee>
  40526f:	48 c7 c0 80 71 40 00 	mov    $0x407180,%rax
  405276:	48 8b 00             	mov    (%rax),%rax
  405279:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40527e:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  405282:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405287:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  40528d:	0f 95 c0             	setne  %al
  405290:	24 01                	and    $0x1,%al
  405292:	3c 00                	cmp    $0x0,%al
  405294:	74 1d                	je     4052b3 <runtime::__type_info_of+0x93>
  405296:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  40529b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4052a0:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  4052a4:	0f 94 c0             	sete   %al
  4052a7:	24 01                	and    $0x1,%al
  4052a9:	3c 00                	cmp    $0x0,%al
  4052ab:	74 06                	je     4052b3 <runtime::__type_info_of+0x93>
  4052ad:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4052b2:	c3                   	ret
  4052b3:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4052b8:	48 83 c0 01          	add    $0x1,%rax
  4052bc:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  4052c1:	0f 92 c0             	setb   %al
  4052c4:	24 01                	and    $0x1,%al
  4052c6:	3c 00                	cmp    $0x0,%al
  4052c8:	74 10                	je     4052da <runtime::__type_info_of+0xba>
  4052ca:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4052cf:	48 83 c0 01          	add    $0x1,%rax
  4052d3:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4052d8:	eb 09                	jmp    4052e3 <runtime::__type_info_of+0xc3>
  4052da:	31 c0                	xor    %eax,%eax
  4052dc:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4052e1:	eb 00                	jmp    4052e3 <runtime::__type_info_of+0xc3>
  4052e3:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4052e8:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4052ed:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4052f2:	48 83 c0 01          	add    $0x1,%rax
  4052f6:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4052fb:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405300:	48 83 c0 01          	add    $0x1,%rax
  405304:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  405309:	e9 51 ff ff ff       	jmp    40525f <runtime::__type_info_of+0x3f>
  40530e:	48 c7 c0 80 71 40 00 	mov    $0x407180,%rax
  405315:	48 8b 00             	mov    (%rax),%rax
  405318:	48 8b 00             	mov    (%rax),%rax
  40531b:	c3                   	ret
  40531c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405320 <runtime::default_logger_proc>:
  405320:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405325:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40532a:	66 44 89 c0          	mov    %r8w,%ax
  40532e:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  405333:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  405338:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40533d:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  405342:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  405347:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40534c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  405351:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  405356:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  40535b:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  405360:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  405365:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40536a:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  40536f:	c3                   	ret

0000000000405370 <runtime::default_context>:
  405370:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  405377:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40537c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  405381:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405386:	31 f6                	xor    %esi,%esi
  405388:	ba 70 00 00 00       	mov    $0x70,%edx
  40538d:	e8 ae bc ff ff       	call   401040 <memset@plt>
  405392:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405397:	e8 24 00 00 00       	call   4053c0 <runtime::[core.odin]::__init_context>
  40539c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4053a1:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  4053a6:	ba 70 00 00 00       	mov    $0x70,%edx
  4053ab:	e8 b0 bc ff ff       	call   401060 <memcpy@plt>
  4053b0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4053b5:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4053bc:	c3                   	ret
  4053bd:	0f 1f 00             	nopl   (%rax)

00000000004053c0 <runtime::[core.odin]::__init_context>:
  4053c0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  4053c5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4053ca:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4053cf:	48 83 f8 00          	cmp    $0x0,%rax
  4053d3:	0f 94 c0             	sete   %al
  4053d6:	24 01                	and    $0x1,%al
  4053d8:	3c 00                	cmp    $0x0,%al
  4053da:	74 01                	je     4053dd <runtime::[core.odin]::__init_context+0x1d>
  4053dc:	c3                   	ret
  4053dd:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4053e2:	48 c7 c1 f0 20 40 00 	mov    $0x4020f0,%rcx
  4053e9:	48 89 08             	mov    %rcx,(%rax)
  4053ec:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4053f1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4053f8:	00 
  4053f9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4053fe:	48 c7 c1 80 2b 40 00 	mov    $0x402b80,%rcx
  405405:	48 89 48 10          	mov    %rcx,0x10(%rax)
  405409:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40540e:	48 c7 c2 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rdx
  405415:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  40541c:	00 00 
  40541e:	48 01 d1             	add    %rdx,%rcx
  405421:	48 89 48 18          	mov    %rcx,0x18(%rax)
  405425:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40542a:	48 c7 c1 70 54 40 00 	mov    $0x405470,%rcx
  405431:	48 89 48 20          	mov    %rcx,0x20(%rax)
  405435:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40543a:	48 c7 c1 20 53 40 00 	mov    $0x405320,%rcx
  405441:	48 89 48 28          	mov    %rcx,0x28(%rax)
  405445:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40544a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  405451:	00 
  405452:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405457:	48 c7 c1 40 28 40 00 	mov    $0x402840,%rcx
  40545e:	48 89 48 48          	mov    %rcx,0x48(%rax)
  405462:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405467:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  40546e:	00 
  40546f:	c3                   	ret

0000000000405470 <runtime::default_assertion_failure_proc>:
  405470:	48 83 ec 48          	sub    $0x48,%rsp
  405474:	4c 89 04 24          	mov    %r8,(%rsp)
  405478:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40547d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405482:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405487:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40548c:	4c 8b 04 24          	mov    (%rsp),%r8
  405490:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405495:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40549a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40549f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4054a4:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4054a9:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4054ae:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4054b3:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4054b8:	e8 03 00 00 00       	call   4054c0 <runtime::default_assertion_contextless_failure_proc>
  4054bd:	0f 1f 00             	nopl   (%rax)

00000000004054c0 <runtime::default_assertion_contextless_failure_proc>:
  4054c0:	48 83 ec 48          	sub    $0x48,%rsp
  4054c4:	4c 89 04 24          	mov    %r8,(%rsp)
  4054c8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4054cd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4054d2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4054d7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4054dc:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4054e1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4054e6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4054eb:	48 8b 3c 24          	mov    (%rsp),%rdi
  4054ef:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4054f4:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4054f9:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4054fe:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405503:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  405508:	e8 d3 ec ff ff       	call   4041e0 <runtime::print_caller_location>
  40550d:	bf 32 79 40 00       	mov    $0x407932,%edi
  405512:	be 01 00 00 00       	mov    $0x1,%esi
  405517:	e8 a4 e4 ff ff       	call   4039c0 <runtime::print_string>
  40551c:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405521:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  405526:	e8 95 e4 ff ff       	call   4039c0 <runtime::print_string>
  40552b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405530:	48 83 f8 00          	cmp    $0x0,%rax
  405534:	0f 9f c0             	setg   %al
  405537:	24 01                	and    $0x1,%al
  405539:	3c 00                	cmp    $0x0,%al
  40553b:	74 1e                	je     40555b <runtime::default_assertion_contextless_failure_proc+0x9b>
  40553d:	bf 34 79 40 00       	mov    $0x407934,%edi
  405542:	be 02 00 00 00       	mov    $0x2,%esi
  405547:	e8 74 e4 ff ff       	call   4039c0 <runtime::print_string>
  40554c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  405551:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  405556:	e8 65 e4 ff ff       	call   4039c0 <runtime::print_string>
  40555b:	bf 0a 00 00 00       	mov    $0xa,%edi
  405560:	e8 8b e6 ff ff       	call   403bf0 <runtime::print_byte>
  405565:	0f 0b                	ud2
  405567:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40556e:	00 00 

0000000000405570 <__truncsfhf2>:
  405570:	48 83 ec 18          	sub    $0x18,%rsp
  405574:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  40557a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  405580:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  405586:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40558d:	00 
  40558e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  405595:	00 
  405596:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40559d:	00 
  40559e:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  4055a5:	00 
  4055a6:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  4055ad:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  4055b3:	8b 44 24 10          	mov    0x10(%rsp),%eax
  4055b7:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  4055bb:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4055bf:	c1 f9 10             	sar    $0x10,%ecx
  4055c2:	b2 01                	mov    $0x1,%dl
  4055c4:	31 c0                	xor    %eax,%eax
  4055c6:	f6 c2 01             	test   $0x1,%dl
  4055c9:	0f 45 c1             	cmovne %ecx,%eax
  4055cc:	25 00 80 00 00       	and    $0x8000,%eax
  4055d1:	89 44 24 08          	mov    %eax,0x8(%rsp)
  4055d5:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4055d9:	c1 f9 17             	sar    $0x17,%ecx
  4055dc:	b2 01                	mov    $0x1,%dl
  4055de:	31 c0                	xor    %eax,%eax
  4055e0:	f6 c2 01             	test   $0x1,%dl
  4055e3:	0f 45 c1             	cmovne %ecx,%eax
  4055e6:	25 ff 00 00 00       	and    $0xff,%eax
  4055eb:	83 e8 70             	sub    $0x70,%eax
  4055ee:	89 44 24 04          	mov    %eax,0x4(%rsp)
  4055f2:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  4055f6:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  4055fb:	89 04 24             	mov    %eax,(%rsp)
  4055fe:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  405603:	0f 9e c0             	setle  %al
  405606:	24 01                	and    $0x1,%al
  405608:	3c 00                	cmp    $0x0,%al
  40560a:	0f 84 82 00 00 00    	je     405692 <__truncsfhf2+0x122>
  405610:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  405615:	0f 9c c0             	setl   %al
  405618:	24 01                	and    $0x1,%al
  40561a:	3c 00                	cmp    $0x0,%al
  40561c:	74 16                	je     405634 <__truncsfhf2+0xc4>
  40561e:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  405623:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  405628:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  40562f:	48 83 c4 18          	add    $0x18,%rsp
  405633:	c3                   	ret
  405634:	8b 04 24             	mov    (%rsp),%eax
  405637:	0d 00 00 80 00       	or     $0x800000,%eax
  40563c:	ba 01 00 00 00       	mov    $0x1,%edx
  405641:	2b 54 24 04          	sub    0x4(%rsp),%edx
  405645:	89 d1                	mov    %edx,%ecx
  405647:	d3 f8                	sar    %cl,%eax
  405649:	89 c1                	mov    %eax,%ecx
  40564b:	31 c0                	xor    %eax,%eax
  40564d:	83 fa 20             	cmp    $0x20,%edx
  405650:	0f 42 c1             	cmovb  %ecx,%eax
  405653:	89 04 24             	mov    %eax,(%rsp)
  405656:	8b 04 24             	mov    (%rsp),%eax
  405659:	25 00 10 00 00       	and    $0x1000,%eax
  40565e:	83 f8 00             	cmp    $0x0,%eax
  405661:	0f 95 c0             	setne  %al
  405664:	24 01                	and    $0x1,%al
  405666:	3c 00                	cmp    $0x0,%al
  405668:	74 0b                	je     405675 <__truncsfhf2+0x105>
  40566a:	8b 04 24             	mov    (%rsp),%eax
  40566d:	05 00 20 00 00       	add    $0x2000,%eax
  405672:	89 04 24             	mov    %eax,(%rsp)
  405675:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405679:	8b 0c 24             	mov    (%rsp),%ecx
  40567c:	c1 e9 0d             	shr    $0xd,%ecx
  40567f:	09 c8                	or     %ecx,%eax
  405681:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  405686:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40568d:	48 83 c4 18          	add    $0x18,%rsp
  405691:	c3                   	ret
  405692:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  405699:	00 
  40569a:	0f 94 c0             	sete   %al
  40569d:	24 01                	and    $0x1,%al
  40569f:	3c 00                	cmp    $0x0,%al
  4056a1:	74 59                	je     4056fc <__truncsfhf2+0x18c>
  4056a3:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  4056a7:	0f 94 c0             	sete   %al
  4056aa:	24 01                	and    $0x1,%al
  4056ac:	3c 00                	cmp    $0x0,%al
  4056ae:	74 1a                	je     4056ca <__truncsfhf2+0x15a>
  4056b0:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4056b4:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4056b9:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  4056be:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  4056c5:	48 83 c4 18          	add    $0x18,%rsp
  4056c9:	c3                   	ret
  4056ca:	8b 04 24             	mov    (%rsp),%eax
  4056cd:	c1 f8 0d             	sar    $0xd,%eax
  4056d0:	89 04 24             	mov    %eax,(%rsp)
  4056d3:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4056d7:	8b 0c 24             	mov    (%rsp),%ecx
  4056da:	09 c8                	or     %ecx,%eax
  4056dc:	85 c9                	test   %ecx,%ecx
  4056de:	0f 94 c1             	sete   %cl
  4056e1:	0f b6 c9             	movzbl %cl,%ecx
  4056e4:	09 c8                	or     %ecx,%eax
  4056e6:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4056eb:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  4056f0:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  4056f7:	48 83 c4 18          	add    $0x18,%rsp
  4056fb:	c3                   	ret
  4056fc:	8b 04 24             	mov    (%rsp),%eax
  4056ff:	25 00 10 00 00       	and    $0x1000,%eax
  405704:	83 f8 00             	cmp    $0x0,%eax
  405707:	0f 95 c0             	setne  %al
  40570a:	24 01                	and    $0x1,%al
  40570c:	3c 00                	cmp    $0x0,%al
  40570e:	74 33                	je     405743 <__truncsfhf2+0x1d3>
  405710:	8b 04 24             	mov    (%rsp),%eax
  405713:	05 00 20 00 00       	add    $0x2000,%eax
  405718:	89 04 24             	mov    %eax,(%rsp)
  40571b:	8b 04 24             	mov    (%rsp),%eax
  40571e:	25 00 00 80 00       	and    $0x800000,%eax
  405723:	83 f8 00             	cmp    $0x0,%eax
  405726:	0f 95 c0             	setne  %al
  405729:	24 01                	and    $0x1,%al
  40572b:	3c 00                	cmp    $0x0,%al
  40572d:	74 12                	je     405741 <__truncsfhf2+0x1d1>
  40572f:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  405736:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40573a:	83 c0 01             	add    $0x1,%eax
  40573d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405741:	eb 00                	jmp    405743 <__truncsfhf2+0x1d3>
  405743:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  405748:	0f 9f c0             	setg   %al
  40574b:	24 01                	and    $0x1,%al
  40574d:	3c 00                	cmp    $0x0,%al
  40574f:	74 75                	je     4057c6 <__truncsfhf2+0x256>
  405751:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  405758:	00 00 00 
  40575b:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405760:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  405767:	00 00 
  405769:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  40576f:	0f 9c c0             	setl   %al
  405772:	24 01                	and    $0x1,%al
  405774:	3c 00                	cmp    $0x0,%al
  405776:	74 34                	je     4057ac <__truncsfhf2+0x23c>
  405778:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40577d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405782:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405787:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  40578d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405792:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405797:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40579c:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4057a1:	48 83 c0 01          	add    $0x1,%rax
  4057a5:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4057aa:	eb bd                	jmp    405769 <__truncsfhf2+0x1f9>
  4057ac:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4057b0:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4057b5:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  4057ba:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  4057c1:	48 83 c4 18          	add    $0x18,%rsp
  4057c5:	c3                   	ret
  4057c6:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4057ca:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  4057ce:	c1 e1 0a             	shl    $0xa,%ecx
  4057d1:	09 c8                	or     %ecx,%eax
  4057d3:	8b 0c 24             	mov    (%rsp),%ecx
  4057d6:	c1 e9 0d             	shr    $0xd,%ecx
  4057d9:	09 c8                	or     %ecx,%eax
  4057db:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  4057e0:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  4057e7:	48 83 c4 18          	add    $0x18,%rsp
  4057eb:	c3                   	ret
  4057ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004057f0 <__truncdfhf2>:
  4057f0:	48 83 ec 18          	sub    $0x18,%rsp
  4057f4:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  4057fa:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  405800:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  405806:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  40580a:	e8 61 fd ff ff       	call   405570 <__truncsfhf2>
  40580f:	48 83 c4 18          	add    $0x18,%rsp
  405813:	c3                   	ret
  405814:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40581b:	00 00 00 00 00 

0000000000405820 <__gnu_h2f_ieee>:
  405820:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  405826:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  40582c:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  405833:	00 
  405834:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  40583b:	00 
  40583c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  405841:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  405846:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  40584d:	00 
  40584e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  405855:	00 
  405856:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  40585d:	00 
  40585e:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  405865:	77 
  405866:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  40586d:	47 
  40586e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405873:	66 25 ff 7f          	and    $0x7fff,%ax
  405877:	0f b7 c8             	movzwl %ax,%ecx
  40587a:	c1 e1 0d             	shl    $0xd,%ecx
  40587d:	b2 01                	mov    $0x1,%dl
  40587f:	31 c0                	xor    %eax,%eax
  405881:	f6 c2 01             	test   $0x1,%dl
  405884:	0f 45 c1             	cmovne %ecx,%eax
  405887:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40588b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  405891:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  405897:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  40589d:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  4058a3:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  4058a8:	0f 93 c0             	setae  %al
  4058ab:	24 01                	and    $0x1,%al
  4058ad:	3c 00                	cmp    $0x0,%al
  4058af:	74 0d                	je     4058be <__gnu_h2f_ieee+0x9e>
  4058b1:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4058b5:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  4058ba:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4058be:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  4058c3:	66 25 00 80          	and    $0x8000,%ax
  4058c7:	0f b7 c8             	movzwl %ax,%ecx
  4058ca:	c1 e1 10             	shl    $0x10,%ecx
  4058cd:	b2 01                	mov    $0x1,%dl
  4058cf:	31 c0                	xor    %eax,%eax
  4058d1:	f6 c2 01             	test   $0x1,%dl
  4058d4:	0f 45 c1             	cmovne %ecx,%eax
  4058d7:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  4058db:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4058df:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  4058e5:	c3                   	ret
  4058e6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4058ed:	00 00 00 

00000000004058f0 <__gnu_f2h_ieee>:
  4058f0:	50                   	push   %rax
  4058f1:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  4058f6:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  4058fb:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  405901:	e8 6a fc ff ff       	call   405570 <__truncsfhf2>
  405906:	58                   	pop    %rax
  405907:	c3                   	ret
  405908:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40590f:	00 

0000000000405910 <__extendhfsf2>:
  405910:	50                   	push   %rax
  405911:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  405917:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  40591d:	0f 28 c8             	movaps %xmm0,%xmm1
  405920:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  405927:	00 
  405928:	e8 f3 fe ff ff       	call   405820 <__gnu_h2f_ieee>
  40592d:	58                   	pop    %rax
  40592e:	c3                   	ret
  40592f:	90                   	nop

0000000000405930 <__floattidf>:
  405930:	53                   	push   %rbx
  405931:	48 83 ec 10          	sub    $0x10,%rsp
  405935:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40593a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40593f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405944:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405949:	48 89 04 24          	mov    %rax,(%rsp)
  40594d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405952:	48 09 c8             	or     %rcx,%rax
  405955:	0f 94 c0             	sete   %al
  405958:	24 01                	and    $0x1,%al
  40595a:	3c 00                	cmp    $0x0,%al
  40595c:	74 09                	je     405967 <__floattidf+0x37>
  40595e:	0f 57 c0             	xorps  %xmm0,%xmm0
  405961:	48 83 c4 10          	add    $0x10,%rsp
  405965:	5b                   	pop    %rbx
  405966:	c3                   	ret
  405967:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40596c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405971:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405976:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40597b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405980:	48 c1 f8 3f          	sar    $0x3f,%rax
  405984:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405989:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40598e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405993:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405998:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40599d:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4059a2:	48 31 d0             	xor    %rdx,%rax
  4059a5:	48 31 f1             	xor    %rsi,%rcx
  4059a8:	48 29 f1             	sub    %rsi,%rcx
  4059ab:	48 19 d0             	sbb    %rdx,%rax
  4059ae:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4059b3:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4059b8:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  4059bd:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4059c2:	48 0f bd c2          	bsr    %rdx,%rax
  4059c6:	48 83 f0 3f          	xor    $0x3f,%rax
  4059ca:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  4059cf:	48 0f bd ce          	bsr    %rsi,%rcx
  4059d3:	48 83 f1 3f          	xor    $0x3f,%rcx
  4059d7:	48 83 c1 40          	add    $0x40,%rcx
  4059db:	48 85 d2             	test   %rdx,%rdx
  4059de:	48 0f 45 c8          	cmovne %rax,%rcx
  4059e2:	31 c0                	xor    %eax,%eax
  4059e4:	ba 80 00 00 00       	mov    $0x80,%edx
  4059e9:	48 29 ca             	sub    %rcx,%rdx
  4059ec:	48 89 c1             	mov    %rax,%rcx
  4059ef:	48 19 c9             	sbb    %rcx,%rcx
  4059f2:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4059f7:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  4059fc:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  405a00:	ff c9                	dec    %ecx
  405a02:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  405a06:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  405a0b:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405a10:	ba 35 00 00 00       	mov    $0x35,%edx
  405a15:	48 29 f2             	sub    %rsi,%rdx
  405a18:	48 19 c8             	sbb    %rcx,%rax
  405a1b:	0f 9c c0             	setl   %al
  405a1e:	24 01                	and    $0x1,%al
  405a20:	3c 00                	cmp    $0x0,%al
  405a22:	0f 84 c0 01 00 00    	je     405be8 <__floattidf+0x2b8>
  405a28:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405a2d:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  405a32:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405a37:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405a3c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  405a41:	0f 28 0d 78 1f 00 00 	movaps 0x1f78(%rip),%xmm1        # 4079c0 <runtime::type_table+0x840>
  405a48:	66 0f ef c1          	pxor   %xmm1,%xmm0
  405a4c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  405a51:	74 17                	je     405a6a <__floattidf+0x13a>
  405a53:	eb 00                	jmp    405a55 <__floattidf+0x125>
  405a55:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  405a5a:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405a5f:	48 83 f0 37          	xor    $0x37,%rax
  405a63:	48 09 c8             	or     %rcx,%rax
  405a66:	74 26                	je     405a8e <__floattidf+0x15e>
  405a68:	eb 29                	jmp    405a93 <__floattidf+0x163>
  405a6a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405a6f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405a74:	48 89 d0             	mov    %rdx,%rax
  405a77:	48 01 c0             	add    %rax,%rax
  405a7a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  405a7f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405a84:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405a89:	e9 d6 00 00 00       	jmp    405b64 <__floattidf+0x234>
  405a8e:	e9 d1 00 00 00       	jmp    405b64 <__floattidf+0x234>
  405a93:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405a98:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  405a9d:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405aa2:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405aa7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405aac:	49 89 fb             	mov    %rdi,%r11
  405aaf:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405ab3:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405ab7:	44 88 db             	mov    %r11b,%bl
  405aba:	88 d9                	mov    %bl,%cl
  405abc:	49 89 f2             	mov    %rsi,%r10
  405abf:	49 d3 ea             	shr    %cl,%r10
  405ac2:	88 d9                	mov    %bl,%cl
  405ac4:	49 89 d1             	mov    %rdx,%r9
  405ac7:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  405acb:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  405ad0:	45 31 c0             	xor    %r8d,%r8d
  405ad3:	f6 c3 40             	test   $0x40,%bl
  405ad6:	4d 0f 45 ca          	cmovne %r10,%r9
  405ada:	4d 0f 45 d0          	cmovne %r8,%r10
  405ade:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405ae5:	48 83 d8 00          	sbb    $0x0,%rax
  405ae9:	4c 89 c0             	mov    %r8,%rax
  405aec:	49 0f 42 c2          	cmovb  %r10,%rax
  405af0:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405af5:	4c 89 c0             	mov    %r8,%rax
  405af8:	49 0f 42 c1          	cmovb  %r9,%rax
  405afc:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  405b02:	49 29 fb             	sub    %rdi,%r11
  405b05:	4c 89 c7             	mov    %r8,%rdi
  405b08:	48 19 cf             	sbb    %rcx,%rdi
  405b0b:	45 88 d9             	mov    %r11b,%r9b
  405b0e:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  405b15:	44 88 c9             	mov    %r9b,%cl
  405b18:	4c 89 d3             	mov    %r10,%rbx
  405b1b:	48 d3 eb             	shr    %cl,%rbx
  405b1e:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  405b23:	41 f6 c1 40          	test   $0x40,%r9b
  405b27:	49 89 d9             	mov    %rbx,%r9
  405b2a:	4d 0f 45 c8          	cmovne %r8,%r9
  405b2e:	4c 0f 45 d3          	cmovne %rbx,%r10
  405b32:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405b39:	48 83 df 00          	sbb    $0x0,%rdi
  405b3d:	4c 89 c7             	mov    %r8,%rdi
  405b40:	49 0f 42 fa          	cmovb  %r10,%rdi
  405b44:	4d 0f 42 c1          	cmovb  %r9,%r8
  405b48:	4c 21 c6             	and    %r8,%rsi
  405b4b:	48 21 fa             	and    %rdi,%rdx
  405b4e:	48 09 f2             	or     %rsi,%rdx
  405b51:	0f 95 c2             	setne  %dl
  405b54:	0f b6 d2             	movzbl %dl,%edx
  405b57:	48 09 d0             	or     %rdx,%rax
  405b5a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405b5f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405b64:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405b69:	89 c1                	mov    %eax,%ecx
  405b6b:	83 e1 04             	and    $0x4,%ecx
  405b6e:	c1 e9 02             	shr    $0x2,%ecx
  405b71:	48 09 c8             	or     %rcx,%rax
  405b74:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405b79:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405b7e:	48 83 c0 01          	add    $0x1,%rax
  405b82:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  405b88:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405b8d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405b92:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405b97:	48 89 c8             	mov    %rcx,%rax
  405b9a:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  405b9f:	48 c1 f9 02          	sar    $0x2,%rcx
  405ba3:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405ba8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405bad:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  405bb1:	24 20                	and    $0x20,%al
  405bb3:	c0 e8 05             	shr    $0x5,%al
  405bb6:	24 01                	and    $0x1,%al
  405bb8:	3c 00                	cmp    $0x0,%al
  405bba:	74 2a                	je     405be6 <__floattidf+0x2b6>
  405bbc:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405bc1:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405bc6:	48 89 c8             	mov    %rcx,%rax
  405bc9:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  405bce:	48 d1 f9             	sar    $1,%rcx
  405bd1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405bd6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405bdb:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  405bdf:	83 c0 01             	add    $0x1,%eax
  405be2:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405be6:	eb 5c                	jmp    405c44 <__floattidf+0x314>
  405be8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  405bec:	b9 35 00 00 00       	mov    $0x35,%ecx
  405bf1:	29 c1                	sub    %eax,%ecx
  405bf3:	83 e1 7f             	and    $0x7f,%ecx
  405bf6:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  405bfa:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405bff:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  405c04:	40 88 cf             	mov    %cl,%dil
  405c07:	40 88 f9             	mov    %dil,%cl
  405c0a:	48 89 c2             	mov    %rax,%rdx
  405c0d:	48 d3 e2             	shl    %cl,%rdx
  405c10:	40 88 f9             	mov    %dil,%cl
  405c13:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  405c17:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  405c1b:	31 c0                	xor    %eax,%eax
  405c1d:	40 f6 c7 40          	test   $0x40,%dil
  405c21:	48 0f 45 f2          	cmovne %rdx,%rsi
  405c25:	48 0f 45 d0          	cmovne %rax,%rdx
  405c29:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  405c2f:	48 89 c1             	mov    %rax,%rcx
  405c32:	48 0f 42 ce          	cmovb  %rsi,%rcx
  405c36:	48 0f 42 c2          	cmovb  %rdx,%rax
  405c3a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405c3f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405c44:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  405c4b:	00 00 
  405c4d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  405c51:	25 00 00 00 80       	and    $0x80000000,%eax
  405c56:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  405c5a:	c1 e1 14             	shl    $0x14,%ecx
  405c5d:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  405c63:	09 c8                	or     %ecx,%eax
  405c65:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  405c69:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  405c6f:	09 c8                	or     %ecx,%eax
  405c71:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405c75:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405c79:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  405c7d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405c83:	48 83 c4 10          	add    $0x10,%rsp
  405c87:	5b                   	pop    %rbx
  405c88:	c3                   	ret
  405c89:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000405c90 <__floattidf_unsigned>:
  405c90:	53                   	push   %rbx
  405c91:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405c96:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  405c9b:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405ca0:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405ca5:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405caa:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405caf:	48 09 c8             	or     %rcx,%rax
  405cb2:	0f 94 c0             	sete   %al
  405cb5:	24 01                	and    $0x1,%al
  405cb7:	3c 00                	cmp    $0x0,%al
  405cb9:	74 05                	je     405cc0 <__floattidf_unsigned+0x30>
  405cbb:	0f 57 c0             	xorps  %xmm0,%xmm0
  405cbe:	5b                   	pop    %rbx
  405cbf:	c3                   	ret
  405cc0:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405cc5:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405cca:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  405ccf:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405cd4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  405cd9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405cde:	48 0f bd c2          	bsr    %rdx,%rax
  405ce2:	48 83 f0 3f          	xor    $0x3f,%rax
  405ce6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  405ceb:	48 0f bd ce          	bsr    %rsi,%rcx
  405cef:	48 83 f1 3f          	xor    $0x3f,%rcx
  405cf3:	48 83 c1 40          	add    $0x40,%rcx
  405cf7:	48 85 d2             	test   %rdx,%rdx
  405cfa:	48 0f 45 c8          	cmovne %rax,%rcx
  405cfe:	31 c0                	xor    %eax,%eax
  405d00:	ba 80 00 00 00       	mov    $0x80,%edx
  405d05:	48 29 ca             	sub    %rcx,%rdx
  405d08:	48 89 c1             	mov    %rax,%rcx
  405d0b:	48 19 c9             	sbb    %rcx,%rcx
  405d0e:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405d13:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405d18:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  405d1c:	ff c9                	dec    %ecx
  405d1e:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  405d22:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  405d27:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405d2c:	ba 35 00 00 00       	mov    $0x35,%edx
  405d31:	48 29 f2             	sub    %rsi,%rdx
  405d34:	48 19 c8             	sbb    %rcx,%rax
  405d37:	0f 92 c0             	setb   %al
  405d3a:	24 01                	and    $0x1,%al
  405d3c:	3c 00                	cmp    $0x0,%al
  405d3e:	0f 84 c0 01 00 00    	je     405f04 <__floattidf_unsigned+0x274>
  405d44:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405d49:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  405d4e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405d53:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405d58:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  405d5d:	0f 28 0d 5c 1c 00 00 	movaps 0x1c5c(%rip),%xmm1        # 4079c0 <runtime::type_table+0x840>
  405d64:	66 0f ef c1          	pxor   %xmm1,%xmm0
  405d68:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  405d6d:	74 17                	je     405d86 <__floattidf_unsigned+0xf6>
  405d6f:	eb 00                	jmp    405d71 <__floattidf_unsigned+0xe1>
  405d71:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  405d76:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405d7b:	48 83 f0 37          	xor    $0x37,%rax
  405d7f:	48 09 c8             	or     %rcx,%rax
  405d82:	74 26                	je     405daa <__floattidf_unsigned+0x11a>
  405d84:	eb 29                	jmp    405daf <__floattidf_unsigned+0x11f>
  405d86:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405d8b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405d90:	48 89 d0             	mov    %rdx,%rax
  405d93:	48 01 c0             	add    %rax,%rax
  405d96:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  405d9b:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405da0:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405da5:	e9 d6 00 00 00       	jmp    405e80 <__floattidf_unsigned+0x1f0>
  405daa:	e9 d1 00 00 00       	jmp    405e80 <__floattidf_unsigned+0x1f0>
  405daf:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405db4:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  405db9:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405dbe:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405dc3:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405dc8:	49 89 fb             	mov    %rdi,%r11
  405dcb:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405dcf:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405dd3:	44 88 db             	mov    %r11b,%bl
  405dd6:	88 d9                	mov    %bl,%cl
  405dd8:	49 89 f2             	mov    %rsi,%r10
  405ddb:	49 d3 ea             	shr    %cl,%r10
  405dde:	88 d9                	mov    %bl,%cl
  405de0:	49 89 d1             	mov    %rdx,%r9
  405de3:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  405de7:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  405dec:	45 31 c0             	xor    %r8d,%r8d
  405def:	f6 c3 40             	test   $0x40,%bl
  405df2:	4d 0f 45 ca          	cmovne %r10,%r9
  405df6:	4d 0f 45 d0          	cmovne %r8,%r10
  405dfa:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405e01:	48 83 d8 00          	sbb    $0x0,%rax
  405e05:	4c 89 c0             	mov    %r8,%rax
  405e08:	49 0f 42 c2          	cmovb  %r10,%rax
  405e0c:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405e11:	4c 89 c0             	mov    %r8,%rax
  405e14:	49 0f 42 c1          	cmovb  %r9,%rax
  405e18:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  405e1e:	49 29 fb             	sub    %rdi,%r11
  405e21:	4c 89 c7             	mov    %r8,%rdi
  405e24:	48 19 cf             	sbb    %rcx,%rdi
  405e27:	45 88 d9             	mov    %r11b,%r9b
  405e2a:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  405e31:	44 88 c9             	mov    %r9b,%cl
  405e34:	4c 89 d3             	mov    %r10,%rbx
  405e37:	48 d3 eb             	shr    %cl,%rbx
  405e3a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  405e3f:	41 f6 c1 40          	test   $0x40,%r9b
  405e43:	49 89 d9             	mov    %rbx,%r9
  405e46:	4d 0f 45 c8          	cmovne %r8,%r9
  405e4a:	4c 0f 45 d3          	cmovne %rbx,%r10
  405e4e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405e55:	48 83 df 00          	sbb    $0x0,%rdi
  405e59:	4c 89 c7             	mov    %r8,%rdi
  405e5c:	49 0f 42 fa          	cmovb  %r10,%rdi
  405e60:	4d 0f 42 c1          	cmovb  %r9,%r8
  405e64:	4c 21 c6             	and    %r8,%rsi
  405e67:	48 21 fa             	and    %rdi,%rdx
  405e6a:	48 09 f2             	or     %rsi,%rdx
  405e6d:	0f 95 c2             	setne  %dl
  405e70:	0f b6 d2             	movzbl %dl,%edx
  405e73:	48 09 d0             	or     %rdx,%rax
  405e76:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405e7b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405e80:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405e85:	89 c1                	mov    %eax,%ecx
  405e87:	83 e1 04             	and    $0x4,%ecx
  405e8a:	c1 e9 02             	shr    $0x2,%ecx
  405e8d:	48 09 c8             	or     %rcx,%rax
  405e90:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405e95:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  405e9a:	48 83 c0 01          	add    $0x1,%rax
  405e9e:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  405ea4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405ea9:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405eae:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405eb3:	48 89 c8             	mov    %rcx,%rax
  405eb6:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  405ebb:	48 c1 e9 02          	shr    $0x2,%rcx
  405ebf:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405ec4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405ec9:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  405ecd:	24 20                	and    $0x20,%al
  405ecf:	c0 e8 05             	shr    $0x5,%al
  405ed2:	24 01                	and    $0x1,%al
  405ed4:	3c 00                	cmp    $0x0,%al
  405ed6:	74 2a                	je     405f02 <__floattidf_unsigned+0x272>
  405ed8:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  405edd:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  405ee2:	48 89 c8             	mov    %rcx,%rax
  405ee5:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  405eea:	48 d1 e9             	shr    $1,%rcx
  405eed:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405ef2:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405ef7:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  405efb:	83 c0 01             	add    $0x1,%eax
  405efe:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405f02:	eb 6a                	jmp    405f6e <__floattidf_unsigned+0x2de>
  405f04:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  405f09:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405f0e:	31 c0                	xor    %eax,%eax
  405f10:	bf 35 00 00 00       	mov    $0x35,%edi
  405f15:	48 29 d7             	sub    %rdx,%rdi
  405f18:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  405f1d:	48 19 c8             	sbb    %rcx,%rax
  405f20:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  405f25:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405f2a:	41 88 f8             	mov    %dil,%r8b
  405f2d:	44 88 c1             	mov    %r8b,%cl
  405f30:	4c 89 ce             	mov    %r9,%rsi
  405f33:	48 d3 e6             	shl    %cl,%rsi
  405f36:	44 88 c1             	mov    %r8b,%cl
  405f39:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  405f3d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  405f42:	41 f6 c0 40          	test   $0x40,%r8b
  405f46:	48 0f 45 d6          	cmovne %rsi,%rdx
  405f4a:	48 0f 45 f1          	cmovne %rcx,%rsi
  405f4e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  405f55:	48 83 d8 00          	sbb    $0x0,%rax
  405f59:	48 89 c8             	mov    %rcx,%rax
  405f5c:	48 0f 42 c6          	cmovb  %rsi,%rax
  405f60:	48 0f 42 ca          	cmovb  %rdx,%rcx
  405f64:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405f69:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405f6e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  405f75:	00 00 
  405f77:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  405f7b:	c1 e2 14             	shl    $0x14,%edx
  405f7e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  405f82:	25 ff ff 0f 00       	and    $0xfffff,%eax
  405f87:	89 c1                	mov    %eax,%ecx
  405f89:	89 d0                	mov    %edx,%eax
  405f8b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  405f92:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405f96:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  405f9a:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  405f9e:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405fa4:	5b                   	pop    %rbx
  405fa5:	c3                   	ret
  405fa6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  405fad:	00 00 00 

0000000000405fb0 <__umodti3>:
  405fb0:	48 83 ec 58          	sub    $0x58,%rsp
  405fb4:	48 89 0c 24          	mov    %rcx,(%rsp)
  405fb8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  405fbd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  405fc2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  405fc7:	48 8b 0c 24          	mov    (%rsp),%rcx
  405fcb:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  405fd0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405fd5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405fda:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  405fdf:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  405fe4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  405fe9:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  405fee:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  405ff3:	e8 78 b6 ff ff       	call   401670 <runtime::udivmod128>
  405ff8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405ffd:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406002:	48 83 c4 58          	add    $0x58,%rsp
  406006:	c3                   	ret
  406007:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40600e:	00 00 

0000000000406010 <__udivmodti4>:
  406010:	48 83 ec 58          	sub    $0x58,%rsp
  406014:	4c 89 04 24          	mov    %r8,(%rsp)
  406018:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40601d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  406022:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  406027:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40602c:	4c 8b 04 24          	mov    (%rsp),%r8
  406030:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406035:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40603a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40603f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406044:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  406049:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40604e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  406053:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406058:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40605d:	e8 0e b6 ff ff       	call   401670 <runtime::udivmod128>
  406062:	48 83 c4 58          	add    $0x58,%rsp
  406066:	c3                   	ret
  406067:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40606e:	00 00 

0000000000406070 <__udivti3>:
  406070:	48 83 ec 48          	sub    $0x48,%rsp
  406074:	48 89 0c 24          	mov    %rcx,(%rsp)
  406078:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40607d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406082:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406087:	48 8b 0c 24          	mov    (%rsp),%rcx
  40608b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  406090:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  406095:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40609a:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40609f:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4060a4:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4060a9:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4060ae:	31 c0                	xor    %eax,%eax
  4060b0:	41 89 c0             	mov    %eax,%r8d
  4060b3:	e8 58 ff ff ff       	call   406010 <__udivmodti4>
  4060b8:	48 83 c4 48          	add    $0x48,%rsp
  4060bc:	c3                   	ret
  4060bd:	0f 1f 00             	nopl   (%rax)

00000000004060c0 <runtime::assert>:
  4060c0:	48 83 ec 48          	sub    $0x48,%rsp
  4060c4:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  4060c9:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4060ce:	40 88 f8             	mov    %dil,%al
  4060d1:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  4060d5:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4060da:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  4060df:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  4060e3:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4060e8:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4060ed:	88 44 24 47          	mov    %al,0x47(%rsp)
  4060f1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4060f6:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4060fb:	3c 00                	cmp    $0x0,%al
  4060fd:	75 19                	jne    406118 <runtime::assert+0x58>
  4060ff:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406104:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  406109:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40610e:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406113:	e8 18 0a 00 00       	call   406b30 <runtime::assert.internal-0>
  406118:	48 83 c4 48          	add    $0x48,%rsp
  40611c:	c3                   	ret
  40611d:	0f 1f 00             	nopl   (%rax)

0000000000406120 <runtime::heap_allocator_proc.aligned_alloc-0>:
  406120:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  406127:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40612c:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406131:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  406136:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40613b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  406140:	44 88 c0             	mov    %r8b,%al
  406143:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  406147:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40614e:	00 
  40614f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  406154:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406159:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40615e:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  406163:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  406167:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40616c:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  406173:	00 
  406174:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40617b:	00 
  40617c:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  406183:	00 
  406184:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  40618b:	00 
  40618c:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  406193:	b9 08 00 00 00       	mov    $0x8,%ecx
  406198:	48 83 fe 08          	cmp    $0x8,%rsi
  40619c:	48 0f 4f ce          	cmovg  %rsi,%rcx
  4061a0:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  4061a7:	00 
  4061a8:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  4061af:	00 
  4061b0:	48 83 e9 01          	sub    $0x1,%rcx
  4061b4:	48 83 c1 08          	add    $0x8,%rcx
  4061b8:	48 01 d1             	add    %rdx,%rcx
  4061bb:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  4061c2:	00 
  4061c3:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  4061ca:	00 00 
  4061cc:	48 83 f8 00          	cmp    $0x0,%rax
  4061d0:	0f 95 c1             	setne  %cl
  4061d3:	80 e1 01             	and    $0x1,%cl
  4061d6:	31 c0                	xor    %eax,%eax
  4061d8:	80 f9 00             	cmp    $0x0,%cl
  4061db:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4061df:	74 17                	je     4061f8 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  4061e1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4061e6:	48 83 f8 08          	cmp    $0x8,%rax
  4061ea:	0f 9f c0             	setg   %al
  4061ed:	24 01                	and    $0x1,%al
  4061ef:	3c 00                	cmp    $0x0,%al
  4061f1:	0f 95 c0             	setne  %al
  4061f4:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4061f8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4061fd:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  406201:	80 e1 01             	and    $0x1,%cl
  406204:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  406208:	48 83 f8 00          	cmp    $0x0,%rax
  40620c:	0f 95 c0             	setne  %al
  40620f:	24 01                	and    $0x1,%al
  406211:	3c 00                	cmp    $0x0,%al
  406213:	74 2e                	je     406243 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  406215:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40621a:	75 27                	jne    406243 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  40621c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406221:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  406225:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40622a:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40622f:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  406236:	00 
  406237:	e8 64 db ff ff       	call   403da0 <runtime::heap_resize>
  40623c:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  406241:	eb 19                	jmp    40625c <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  406243:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  406247:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  40624e:	00 
  40624f:	0f b6 f0             	movzbl %al,%esi
  406252:	e8 19 db ff ff       	call   403d70 <runtime::heap_alloc>
  406257:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40625c:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  406261:	48 83 c0 08          	add    $0x8,%rax
  406265:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40626a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40626f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  406274:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  406279:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40627e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  406283:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  40628a:	00 
  40628b:	48 83 e8 01          	sub    $0x1,%rax
  40628f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  406296:	00 
  406297:	48 83 e9 01          	sub    $0x1,%rcx
  40629b:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  40629f:	48 21 c8             	and    %rcx,%rax
  4062a2:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4062a7:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  4062ad:	0f 94 c0             	sete   %al
  4062b0:	24 01                	and    $0x1,%al
  4062b2:	3c 00                	cmp    $0x0,%al
  4062b4:	74 3c                	je     4062f2 <runtime::heap_allocator_proc.aligned_alloc-0+0x1d2>
  4062b6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4062bb:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4062c0:	e8 ab 00 00 00       	call   406370 <runtime::heap_allocator_proc.aligned_free-1>
  4062c5:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4062ca:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  4062cf:	e8 9c 00 00 00       	call   406370 <runtime::heap_allocator_proc.aligned_free-1>
  4062d4:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4062d9:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4062e0:	00 
  4062e1:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4062e8:	b0 01                	mov    $0x1,%al
  4062ea:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4062f1:	c3                   	ret
  4062f2:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4062f7:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4062fc:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  406301:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  406306:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  40630a:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40630f:	74 2f                	je     406340 <runtime::heap_allocator_proc.aligned_alloc-0+0x220>
  406311:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406316:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40631b:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  406320:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  406325:	48 39 d0             	cmp    %rdx,%rax
  406328:	48 0f 4c d0          	cmovl  %rax,%rdx
  40632c:	e8 3f d0 ff ff       	call   403370 <runtime::mem_copy_non_overlapping>
  406331:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406336:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40633b:	e8 30 00 00 00       	call   406370 <runtime::heap_allocator_proc.aligned_free-1>
  406340:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406345:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  40634a:	e8 c1 bf ff ff       	call   402310 <runtime::[internal.odin]::byte_slice>
  40634f:	48 89 c1             	mov    %rax,%rcx
  406352:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406357:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40635b:	48 89 08             	mov    %rcx,(%rax)
  40635e:	31 c0                	xor    %eax,%eax
  406360:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  406367:	c3                   	ret
  406368:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40636f:	00 

0000000000406370 <runtime::heap_allocator_proc.aligned_free-1>:
  406370:	48 83 ec 18          	sub    $0x18,%rsp
  406374:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406379:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40637e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406383:	48 83 f8 00          	cmp    $0x0,%rax
  406387:	0f 95 c0             	setne  %al
  40638a:	24 01                	and    $0x1,%al
  40638c:	3c 00                	cmp    $0x0,%al
  40638e:	74 0e                	je     40639e <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  406390:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406395:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  406399:	e8 32 da ff ff       	call   403dd0 <runtime::heap_free>
  40639e:	48 83 c4 18          	add    $0x18,%rsp
  4063a2:	c3                   	ret
  4063a3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4063aa:	84 00 00 00 00 00 

00000000004063b0 <runtime::heap_allocator_proc.aligned_resize-2>:
  4063b0:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  4063b7:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  4063bc:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4063c1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4063c6:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  4063cb:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  4063d0:	44 88 c0             	mov    %r8b,%al
  4063d3:	88 44 24 57          	mov    %al,0x57(%rsp)
  4063d7:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4063de:	00 
  4063df:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4063e4:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4063e9:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  4063ed:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4063f2:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4063f7:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4063fc:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  406403:	00 
  406404:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  40640b:	00 
  40640c:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  406413:	00 
  406414:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  40641b:	00 
  40641c:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  406423:	0f 57 c0             	xorps  %xmm0,%xmm0
  406426:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  40642d:	00 
  40642e:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  406435:	00 
  406436:	48 83 f8 00          	cmp    $0x0,%rax
  40643a:	0f 94 c0             	sete   %al
  40643d:	24 01                	and    $0x1,%al
  40643f:	3c 00                	cmp    $0x0,%al
  406441:	0f 84 80 00 00 00    	je     4064c7 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  406447:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40644c:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406451:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406456:	8a 44 24 57          	mov    0x57(%rsp),%al
  40645a:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  40645f:	0f 57 c0             	xorps  %xmm0,%xmm0
  406462:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  406469:	00 
  40646a:	48 89 e2             	mov    %rsp,%rdx
  40646d:	4c 89 02             	mov    %r8,(%rdx)
  406470:	44 0f b6 c0          	movzbl %al,%r8d
  406474:	31 c0                	xor    %eax,%eax
  406476:	89 c2                	mov    %eax,%edx
  406478:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  40647f:	00 
  406480:	e8 9b fc ff ff       	call   406120 <runtime::heap_allocator_proc.aligned_alloc-0>
  406485:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40648a:	40 88 c7             	mov    %al,%dil
  40648d:	40 88 f8             	mov    %dil,%al
  406490:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  406497:	00 
  406498:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  40649f:	00 
  4064a0:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  4064a7:	00 
  4064a8:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4064af:	00 
  4064b0:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  4064b7:	00 
  4064b8:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4064bc:	48 89 11             	mov    %rdx,(%rcx)
  4064bf:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  4064c6:	c3                   	ret
  4064c7:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4064cc:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4064d1:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4064d6:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4064db:	8a 44 24 57          	mov    0x57(%rsp),%al
  4064df:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  4064e4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4064e7:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4064ee:	00 
  4064ef:	49 89 e0             	mov    %rsp,%r8
  4064f2:	4d 89 08             	mov    %r9,(%r8)
  4064f5:	44 0f b6 c0          	movzbl %al,%r8d
  4064f9:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  406500:	00 
  406501:	e8 1a fc ff ff       	call   406120 <runtime::heap_allocator_proc.aligned_alloc-0>
  406506:	88 44 24 17          	mov    %al,0x17(%rsp)
  40650a:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  406511:	00 
  406512:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406517:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40651e:	00 
  40651f:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  406524:	3c 00                	cmp    $0x0,%al
  406526:	74 4d                	je     406575 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  406528:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40652d:	8a 44 24 17          	mov    0x17(%rsp),%al
  406531:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406538:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40653f:	00 
  406540:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406547:	00 
  406548:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40654f:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406556:	00 
  406557:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40655e:	00 
  40655f:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406566:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40656a:	48 89 11             	mov    %rdx,(%rcx)
  40656d:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406574:	c3                   	ret
  406575:	8a 44 24 57          	mov    0x57(%rsp),%al
  406579:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40657e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406583:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40658a:	00 
  40658b:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  406592:	00 
  406593:	3c 00                	cmp    $0x0,%al
  406595:	0f 84 87 00 00 00    	je     406622 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  40659b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4065a0:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4065a5:	48 39 c8             	cmp    %rcx,%rax
  4065a8:	0f 9f c0             	setg   %al
  4065ab:	24 01                	and    $0x1,%al
  4065ad:	3c 00                	cmp    $0x0,%al
  4065af:	74 71                	je     406622 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  4065b1:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  4065b6:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  4065bd:	00 
  4065be:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4065c3:	48 89 e0             	mov    %rsp,%rax
  4065c6:	4c 89 08             	mov    %r9,(%rax)
  4065c9:	bf 37 79 40 00       	mov    $0x407937,%edi
  4065ce:	be 30 00 00 00       	mov    $0x30,%esi
  4065d3:	ba 4b 00 00 00       	mov    $0x4b,%edx
  4065d8:	b9 25 00 00 00       	mov    $0x25,%ecx
  4065dd:	e8 1e ce ff ff       	call   403400 <runtime::slice_expr_error_lo_hi>
  4065e2:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4065e7:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4065ec:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4065f1:	48 89 c6             	mov    %rax,%rsi
  4065f4:	48 03 b4 24 e0 00 00 	add    0xe0(%rsp),%rsi
  4065fb:	00 
  4065fc:	48 29 c1             	sub    %rax,%rcx
  4065ff:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  406604:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  406609:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40660e:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  406613:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  406618:	48 29 c2             	sub    %rax,%rdx
  40661b:	31 f6                	xor    %esi,%esi
  40661d:	e8 1e aa ff ff       	call   401040 <memset@plt>
  406622:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  406627:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40662e:	00 
  40662f:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406636:	00 
  406637:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40663e:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406645:	00 
  406646:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40664d:	00 
  40664e:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406655:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  406659:	48 89 11             	mov    %rdx,(%rcx)
  40665c:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406663:	c3                   	ret
  406664:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40666b:	00 00 00 00 00 

0000000000406670 <runtime::bounds_check_error.handle_error-0>:
  406670:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  406677:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40667c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406681:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406685:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406689:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40668e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406693:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406698:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40669d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  4066a1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4066a5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4066aa:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4066af:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4066b4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  4066bb:	00 
  4066bc:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  4066c0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  4066c4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  4066c9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  4066ce:	0f 57 c0             	xorps  %xmm0,%xmm0
  4066d1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4066d6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4066db:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4066e2:	00 00 
  4066e4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4066e9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4066ee:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4066f5:	00 00 
  4066f7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4066fc:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406701:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  406705:	89 44 24 44          	mov    %eax,0x44(%rsp)
  406709:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40670e:	e8 cd da ff ff       	call   4041e0 <runtime::print_caller_location>
  406713:	bf 68 79 40 00       	mov    $0x407968,%edi
  406718:	be 07 00 00 00       	mov    $0x7,%esi
  40671d:	e8 9e d2 ff ff       	call   4039c0 <runtime::print_string>
  406722:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406727:	e8 b4 d8 ff ff       	call   403fe0 <runtime::print_i64>
  40672c:	bf b3 78 40 00       	mov    $0x4078b3,%edi
  406731:	be 15 00 00 00       	mov    $0x15,%esi
  406736:	e8 85 d2 ff ff       	call   4039c0 <runtime::print_string>
  40673b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406740:	e8 9b d8 ff ff       	call   403fe0 <runtime::print_i64>
  406745:	bf 0a 00 00 00       	mov    $0xa,%edi
  40674a:	e8 a1 d4 ff ff       	call   403bf0 <runtime::print_byte>
  40674f:	e8 ec ae ff ff       	call   401640 <runtime::bounds_trap>
  406754:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40675b:	00 00 00 00 00 

0000000000406760 <runtime::default_random_generator_proc.read_u64-0>:
  406760:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  406765:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40676a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40676f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406774:	48 8b 00             	mov    (%rax),%rax
  406777:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40677c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406781:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  406788:	f4 51 58 
  40678b:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  406791:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406796:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  40679a:	48 83 ca 01          	or     $0x1,%rdx
  40679e:	48 01 d1             	add    %rdx,%rcx
  4067a1:	48 89 08             	mov    %rcx,(%rax)
  4067a4:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4067a9:	48 c1 e9 3b          	shr    $0x3b,%rcx
  4067ad:	b2 01                	mov    $0x1,%dl
  4067af:	31 c0                	xor    %eax,%eax
  4067b1:	f6 c2 01             	test   $0x1,%dl
  4067b4:	48 0f 45 c1          	cmovne %rcx,%rax
  4067b8:	48 83 c0 05          	add    $0x5,%rax
  4067bc:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  4067c1:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  4067c8:	75 f1 ae 
  4067cb:	48 0f af c1          	imul   %rcx,%rax
  4067cf:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4067d4:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4067d9:	48 c1 e9 3b          	shr    $0x3b,%rcx
  4067dd:	b2 01                	mov    $0x1,%dl
  4067df:	31 c0                	xor    %eax,%eax
  4067e1:	f6 c2 01             	test   $0x1,%dl
  4067e4:	48 0f 45 c1          	cmovne %rcx,%rax
  4067e8:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4067ed:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4067f2:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4067f7:	48 89 d1             	mov    %rdx,%rcx
  4067fa:	48 d3 e8             	shr    %cl,%rax
  4067fd:	48 89 c1             	mov    %rax,%rcx
  406800:	31 c0                	xor    %eax,%eax
  406802:	48 83 fa 40          	cmp    $0x40,%rdx
  406806:	48 0f 42 c1          	cmovb  %rcx,%rax
  40680a:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40680f:	31 c9                	xor    %ecx,%ecx
  406811:	89 ce                	mov    %ecx,%esi
  406813:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  406818:	48 83 e6 3f          	and    $0x3f,%rsi
  40681c:	48 89 f1             	mov    %rsi,%rcx
  40681f:	48 d3 e2             	shl    %cl,%rdx
  406822:	31 c9                	xor    %ecx,%ecx
  406824:	48 83 fe 40          	cmp    $0x40,%rsi
  406828:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40682c:	48 09 c8             	or     %rcx,%rax
  40682f:	c3                   	ret

0000000000406830 <runtime::default_random_generator_proc.init-1>:
  406830:	48 83 ec 28          	sub    $0x28,%rsp
  406834:	48 89 3c 24          	mov    %rdi,(%rsp)
  406838:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40683d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406842:	48 8b 0c 24          	mov    (%rsp),%rcx
  406846:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40684b:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  406850:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406855:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  40685b:	0f 94 c0             	sete   %al
  40685e:	24 01                	and    $0x1,%al
  406860:	3c 00                	cmp    $0x0,%al
  406862:	74 0e                	je     406872 <runtime::default_random_generator_proc.init-1+0x42>
  406864:	0f 31                	rdtsc
  406866:	48 c1 e2 20          	shl    $0x20,%rdx
  40686a:	48 09 d0             	or     %rdx,%rax
  40686d:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406872:	48 8b 3c 24          	mov    (%rsp),%rdi
  406876:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40687b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  406882:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406887:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40688c:	48 d1 e2             	shl    $1,%rdx
  40688f:	40 b6 01             	mov    $0x1,%sil
  406892:	31 c9                	xor    %ecx,%ecx
  406894:	40 f6 c6 01          	test   $0x1,%sil
  406898:	48 0f 45 ca          	cmovne %rdx,%rcx
  40689c:	48 83 c9 01          	or     $0x1,%rcx
  4068a0:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4068a4:	e8 b7 fe ff ff       	call   406760 <runtime::default_random_generator_proc.read_u64-0>
  4068a9:	48 8b 3c 24          	mov    (%rsp),%rdi
  4068ad:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4068b2:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4068b7:	48 03 08             	add    (%rax),%rcx
  4068ba:	48 89 08             	mov    %rcx,(%rax)
  4068bd:	e8 9e fe ff ff       	call   406760 <runtime::default_random_generator_proc.read_u64-0>
  4068c2:	48 83 c4 28          	add    $0x28,%rsp
  4068c6:	c3                   	ret
  4068c7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4068ce:	00 00 

00000000004068d0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  4068d0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  4068d5:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  4068da:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4068df:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4068e4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4068e9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4068ee:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4068f5:	00 00 
  4068f7:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  4068fc:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  406900:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406905:	48 03 4a 20          	add    0x20(%rdx),%rcx
  406909:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40690e:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406913:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406918:	48 83 e8 01          	sub    $0x1,%rax
  40691c:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  406921:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406926:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  40692b:	48 83 f8 00          	cmp    $0x0,%rax
  40692f:	0f 95 c0             	setne  %al
  406932:	24 01                	and    $0x1,%al
  406934:	3c 00                	cmp    $0x0,%al
  406936:	74 17                	je     40694f <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  406938:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40693d:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406942:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  406947:	48 29 c8             	sub    %rcx,%rax
  40694a:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40694f:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406954:	c3                   	ret
  406955:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40695c:	00 00 00 00 

0000000000406960 <runtime::arena_alloc.align_forward_uint-0>:
  406960:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  406965:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40696a:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40696f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406974:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406979:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40697e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406983:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406988:	48 83 e9 01          	sub    $0x1,%rcx
  40698c:	48 21 c8             	and    %rcx,%rax
  40698f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406994:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  40699a:	0f 95 c0             	setne  %al
  40699d:	24 01                	and    $0x1,%al
  40699f:	3c 00                	cmp    $0x0,%al
  4069a1:	74 14                	je     4069b7 <runtime::arena_alloc.align_forward_uint-0+0x57>
  4069a3:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4069a8:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  4069ad:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  4069b2:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4069b7:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4069bc:	c3                   	ret
  4069bd:	0f 1f 00             	nopl   (%rax)

00000000004069c0 <runtime::matrix_bounds_check_error.handle_error-0>:
  4069c0:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  4069c7:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4069cc:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4069d1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4069d5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4069d9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4069de:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4069e3:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  4069ea:	00 
  4069eb:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4069f0:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4069f7:	00 
  4069f8:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4069fd:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406a02:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  406a07:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  406a0c:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  406a11:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406a15:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406a19:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406a1e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406a23:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  406a2a:	00 
  406a2b:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  406a32:	00 
  406a33:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  406a3a:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  406a41:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  406a48:	00 
  406a49:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  406a50:	00 
  406a51:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  406a56:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  406a5b:	0f 57 c0             	xorps  %xmm0,%xmm0
  406a5e:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406a63:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406a68:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406a6f:	00 00 
  406a71:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406a76:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406a7b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406a82:	00 00 
  406a84:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  406a89:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  406a8e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  406a92:	89 44 24 54          	mov    %eax,0x54(%rsp)
  406a96:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  406a9b:	e8 40 d7 ff ff       	call   4041e0 <runtime::print_caller_location>
  406aa0:	bf 70 79 40 00       	mov    $0x407970,%edi
  406aa5:	be 11 00 00 00       	mov    $0x11,%esi
  406aaa:	e8 11 cf ff ff       	call   4039c0 <runtime::print_string>
  406aaf:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406ab4:	e8 27 d5 ff ff       	call   403fe0 <runtime::print_i64>
  406ab9:	bf 82 79 40 00       	mov    $0x407982,%edi
  406abe:	be 02 00 00 00       	mov    $0x2,%esi
  406ac3:	e8 f8 ce ff ff       	call   4039c0 <runtime::print_string>
  406ac8:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406acd:	e8 0e d5 ff ff       	call   403fe0 <runtime::print_i64>
  406ad2:	bf 85 79 40 00       	mov    $0x407985,%edi
  406ad7:	be 16 00 00 00       	mov    $0x16,%esi
  406adc:	e8 df ce ff ff       	call   4039c0 <runtime::print_string>
  406ae1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406ae6:	e8 f5 d4 ff ff       	call   403fe0 <runtime::print_i64>
  406aeb:	bf 9c 79 40 00       	mov    $0x40799c,%edi
  406af0:	be 06 00 00 00       	mov    $0x6,%esi
  406af5:	e8 c6 ce ff ff       	call   4039c0 <runtime::print_string>
  406afa:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406aff:	e8 dc d4 ff ff       	call   403fe0 <runtime::print_i64>
  406b04:	bf a3 79 40 00       	mov    $0x4079a3,%edi
  406b09:	be 01 00 00 00       	mov    $0x1,%esi
  406b0e:	e8 ad ce ff ff       	call   4039c0 <runtime::print_string>
  406b13:	bf 0a 00 00 00       	mov    $0xa,%edi
  406b18:	e8 d3 d0 ff ff       	call   403bf0 <runtime::print_byte>
  406b1d:	e8 1e ab ff ff       	call   401640 <runtime::bounds_trap>
  406b22:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  406b29:	1f 84 00 00 00 00 00 

0000000000406b30 <runtime::assert.internal-0>:
  406b30:	48 83 ec 38          	sub    $0x38,%rsp
  406b34:	48 89 0c 24          	mov    %rcx,(%rsp)
  406b38:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  406b3d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406b42:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406b47:	48 8b 04 24          	mov    (%rsp),%rax
  406b4b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406b50:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406b55:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  406b5a:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406b5f:	48 8b 40 20          	mov    0x20(%rax),%rax
  406b63:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406b68:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  406b6e:	0f 94 c0             	sete   %al
  406b71:	24 01                	and    $0x1,%al
  406b73:	3c 00                	cmp    $0x0,%al
  406b75:	74 0c                	je     406b83 <runtime::assert.internal-0+0x53>
  406b77:	48 c7 c0 70 54 40 00 	mov    $0x405470,%rax
  406b7e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406b83:	4c 8b 0c 24          	mov    (%rsp),%r9
  406b87:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  406b8c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406b91:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406b96:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406b9b:	bf a5 79 40 00       	mov    $0x4079a5,%edi
  406ba0:	be 11 00 00 00       	mov    $0x11,%esi
  406ba5:	ff d0                	call   *%rax

Disassembly of section .fini:

0000000000406ba8 <_fini>:
  406ba8:	f3 0f 1e fa          	endbr64
  406bac:	48 83 ec 08          	sub    $0x8,%rsp
  406bb0:	48 83 c4 08          	add    $0x8,%rsp
  406bb4:	c3                   	ret
