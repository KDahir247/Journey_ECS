
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
  4010b8:	48 c7 c7 e0 30 40 00 	mov    $0x4030e0,%rdi
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
  401190:	48 83 ec 68          	sub    $0x68,%rsp
  401194:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  401199:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40119e:	e8 1d 01 00 00       	call   4012c0 <journey::create_world:proc(indice_bit_capacity:$$900,unique_data_capacity:$$30)->(:journey::World)>
  4011a3:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011a8:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  4011ad:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4011b2:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4011b7:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4011bc:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  4011c1:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4011c6:	48 8d 7c 24 58       	lea    0x58(%rsp),%rdi
  4011cb:	48 89 3c 24          	mov    %rdi,(%rsp)
  4011cf:	e8 8b 01 00 00       	call   40135f <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Health::$1,data_storage_index:$$0,indices_capacity:$$100)>
  4011d4:	48 8b 3c 24          	mov    (%rsp),%rdi
  4011d8:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011dd:	e8 f2 01 00 00       	call   4013d4 <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Position::$1,data_storage_index:$$1,indices_capacity:$$200)>
  4011e2:	48 8b 3c 24          	mov    (%rsp),%rdi
  4011e6:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011eb:	e8 5d 02 00 00       	call   40144d <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Position::$1,data_storage_index:$$2,indices_capacity:$$300)>
  4011f0:	48 8b 3c 24          	mov    (%rsp),%rdi
  4011f4:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011f9:	e8 c8 02 00 00       	call   4014c6 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)>
  4011fe:	48 8b 3c 24          	mov    (%rsp),%rdi
  401202:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  401207:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40120c:	e8 8f 03 00 00       	call   4015a0 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)>
  401211:	48 8b 3c 24          	mov    (%rsp),%rdi
  401215:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40121a:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40121f:	0f 57 c0             	xorps  %xmm0,%xmm0
  401222:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  401227:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  40122e:	00 00 
  401230:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  401237:	00 00 
  401239:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40123e:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401243:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  401248:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40124d:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401252:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401257:	e8 1e 04 00 00       	call   40167a <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)>
  40125c:	48 8b 3c 24          	mov    (%rsp),%rdi
  401260:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401265:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  40126c:	00 00 
  40126e:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  401275:	00 00 
  401277:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40127c:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  401281:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  401286:	e8 be 04 00 00       	call   401749 <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$1,indices:[1]journey::QWORD)>
  40128b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401290:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  401297:	00 00 
  401299:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
  4012a0:	00 00 
  4012a2:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4012a7:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4012ac:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4012b1:	48 8d 7c 24 58       	lea    0x58(%rsp),%rdi
  4012b6:	e8 52 05 00 00       	call   40180d <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$2,indices:[1]journey::QWORD)>
  4012bb:	48 83 c4 68          	add    $0x68,%rsp
  4012bf:	c3                   	ret

00000000004012c0 <journey::create_world:proc(indice_bit_capacity:$$900,unique_data_capacity:$$30)->(:journey::World)>:
  4012c0:	31 c0                	xor    %eax,%eax
  4012c2:	41 89 c1             	mov    %eax,%r9d
  4012c5:	4c 89 4c 24 b0       	mov    %r9,-0x50(%rsp)
  4012ca:	b8 09 00 00 00       	mov    $0x9,%eax
  4012cf:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4012d4:	be 00 10 00 00       	mov    $0x1000,%esi
  4012d9:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  4012de:	ba 03 00 00 00       	mov    $0x3,%edx
  4012e3:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  4012e8:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4012ee:	4c 89 54 24 c0       	mov    %r10,-0x40(%rsp)
  4012f3:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4012fa:	4c 89 44 24 b8       	mov    %r8,-0x48(%rsp)
  4012ff:	4c 89 cf             	mov    %r9,%rdi
  401302:	0f 05                	syscall
  401304:	4c 8b 4c 24 b0       	mov    -0x50(%rsp),%r9
  401309:	4c 8b 44 24 b8       	mov    -0x48(%rsp),%r8
  40130e:	4c 8b 54 24 c0       	mov    -0x40(%rsp),%r10
  401313:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401318:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  40131d:	48 89 c1             	mov    %rax,%rcx
  401320:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401325:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40132a:	4c 89 cf             	mov    %r9,%rdi
  40132d:	0f 05                	syscall
  40132f:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  401334:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401339:	48 c7 00 40 00 00 00 	movq   $0x40,(%rax)
  401340:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401345:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40134a:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40134f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401354:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401359:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40135e:	c3                   	ret

000000000040135f <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Health::$1,data_storage_index:$$0,indices_capacity:$$100)>:
  40135f:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  401364:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  401369:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40136e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401373:	48 8b 00             	mov    (%rax),%rax
  401376:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40137b:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401380:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401385:	31 c0                	xor    %eax,%eax
  401387:	41 89 c1             	mov    %eax,%r9d
  40138a:	b8 09 00 00 00       	mov    $0x9,%eax
  40138f:	be 00 10 00 00       	mov    $0x1000,%esi
  401394:	ba 03 00 00 00       	mov    $0x3,%edx
  401399:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  40139f:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4013a6:	4c 89 cf             	mov    %r9,%rdi
  4013a9:	0f 05                	syscall
  4013ab:	48 89 c1             	mov    %rax,%rcx
  4013ae:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4013b3:	48 89 08             	mov    %rcx,(%rax)
  4013b6:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4013bb:	48 c7 40 18 04 00 00 	movq   $0x4,0x18(%rax)
  4013c2:	00 
  4013c3:	48 c7 40 10 c8 00 00 	movq   $0xc8,0x10(%rax)
  4013ca:	00 
  4013cb:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4013d2:	00 
  4013d3:	c3                   	ret

00000000004013d4 <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Position::$1,data_storage_index:$$1,indices_capacity:$$200)>:
  4013d4:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  4013d9:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4013de:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4013e3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4013e8:	48 8b 00             	mov    (%rax),%rax
  4013eb:	48 83 c0 20          	add    $0x20,%rax
  4013ef:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4013f4:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4013f9:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4013fe:	31 c0                	xor    %eax,%eax
  401400:	41 89 c1             	mov    %eax,%r9d
  401403:	b8 09 00 00 00       	mov    $0x9,%eax
  401408:	be 00 10 00 00       	mov    $0x1000,%esi
  40140d:	ba 03 00 00 00       	mov    $0x3,%edx
  401412:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401418:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  40141f:	4c 89 cf             	mov    %r9,%rdi
  401422:	0f 05                	syscall
  401424:	48 89 c1             	mov    %rax,%rcx
  401427:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40142c:	48 89 08             	mov    %rcx,(%rax)
  40142f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401434:	48 c7 40 18 08 00 00 	movq   $0x8,0x18(%rax)
  40143b:	00 
  40143c:	48 c7 40 10 90 01 00 	movq   $0x190,0x10(%rax)
  401443:	00 
  401444:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40144b:	00 
  40144c:	c3                   	ret

000000000040144d <journey::register_data_storage:proc(world:^journey::World,data_typeid:$journey::main::Position::$1,data_storage_index:$$2,indices_capacity:$$300)>:
  40144d:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  401452:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  401457:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40145c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401461:	48 8b 00             	mov    (%rax),%rax
  401464:	48 83 c0 40          	add    $0x40,%rax
  401468:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40146d:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401472:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401477:	31 c0                	xor    %eax,%eax
  401479:	41 89 c1             	mov    %eax,%r9d
  40147c:	b8 09 00 00 00       	mov    $0x9,%eax
  401481:	be 00 10 00 00       	mov    $0x1000,%esi
  401486:	ba 03 00 00 00       	mov    $0x3,%edx
  40148b:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  401491:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  401498:	4c 89 cf             	mov    %r9,%rdi
  40149b:	0f 05                	syscall
  40149d:	48 89 c1             	mov    %rax,%rcx
  4014a0:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4014a5:	48 89 08             	mov    %rcx,(%rax)
  4014a8:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4014ad:	48 c7 40 18 08 00 00 	movq   $0x8,0x18(%rax)
  4014b4:	00 
  4014b5:	48 c7 40 10 58 02 00 	movq   $0x258,0x10(%rax)
  4014bc:	00 
  4014bd:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4014c4:	00 
  4014c5:	c3                   	ret

00000000004014c6 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)>:
  4014c6:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  4014cb:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4014d0:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4014d5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4014da:	48 8b 40 08          	mov    0x8(%rax),%rax
  4014de:	48 8b 00             	mov    (%rax),%rax
  4014e1:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4014e6:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4014eb:	48 8b 40 08          	mov    0x8(%rax),%rax
  4014ef:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4014f4:	48 c1 e9 06          	shr    $0x6,%rcx
  4014f8:	48 c1 e1 03          	shl    $0x3,%rcx
  4014fc:	48 01 c8             	add    %rcx,%rax
  4014ff:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401504:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401509:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40150e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401513:	48 83 c1 14          	add    $0x14,%rcx
  401517:	48 83 e1 3f          	and    $0x3f,%rcx
  40151b:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  401522:	c4 e2 f0 f5 c0       	bzhi   %rcx,%rax,%rax
  401527:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40152c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401531:	48 c7 00 ff ff ff ff 	movq   $0xffffffffffffffff,(%rax)
  401538:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40153d:	48 83 c0 14          	add    $0x14,%rax
  401541:	48 c1 e8 06          	shr    $0x6,%rax
  401545:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40154a:	48 83 c1 00          	add    $0x0,%rcx
  40154e:	48 c1 e9 06          	shr    $0x6,%rcx
  401552:	48 29 c8             	sub    %rcx,%rax
  401555:	48 83 f8 00          	cmp    $0x0,%rax
  401559:	75 0d                	jne    401568 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$20)->(:journey::QWORD)+0xa2>
  40155b:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401560:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401565:	48 89 08             	mov    %rcx,(%rax)
  401568:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40156d:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401572:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401576:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40157b:	48 8b 40 08          	mov    0x8(%rax),%rax
  40157f:	48 8b 08             	mov    (%rax),%rcx
  401582:	48 83 c1 14          	add    $0x14,%rcx
  401586:	48 89 08             	mov    %rcx,(%rax)
  401589:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40158e:	48 83 e8 40          	sub    $0x40,%rax
  401592:	48 b9 00 00 00 00 14 	movabs $0x1400000000,%rcx
  401599:	00 00 00 
  40159c:	48 09 c8             	or     %rcx,%rax
  40159f:	c3                   	ret

00000000004015a0 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)>:
  4015a0:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  4015a5:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4015aa:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4015af:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4015b4:	48 8b 40 08          	mov    0x8(%rax),%rax
  4015b8:	48 8b 00             	mov    (%rax),%rax
  4015bb:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4015c0:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4015c5:	48 8b 40 08          	mov    0x8(%rax),%rax
  4015c9:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4015ce:	48 c1 e9 06          	shr    $0x6,%rcx
  4015d2:	48 c1 e1 03          	shl    $0x3,%rcx
  4015d6:	48 01 c8             	add    %rcx,%rax
  4015d9:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4015de:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4015e3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4015e8:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4015ed:	48 83 c1 32          	add    $0x32,%rcx
  4015f1:	48 83 e1 3f          	and    $0x3f,%rcx
  4015f5:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4015fc:	c4 e2 f0 f5 c0       	bzhi   %rcx,%rax,%rax
  401601:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401606:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40160b:	48 c7 00 ff ff ff ff 	movq   $0xffffffffffffffff,(%rax)
  401612:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  401617:	48 83 c0 32          	add    $0x32,%rax
  40161b:	48 c1 e8 06          	shr    $0x6,%rax
  40161f:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  401624:	48 83 c1 00          	add    $0x0,%rcx
  401628:	48 c1 e9 06          	shr    $0x6,%rcx
  40162c:	48 29 c8             	sub    %rcx,%rax
  40162f:	48 83 f8 00          	cmp    $0x0,%rax
  401633:	75 0d                	jne    401642 <journey::create_indices:proc(world:^journey::World,bits_to_use:$$50)->(:journey::QWORD)+0xa2>
  401635:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40163a:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40163f:	48 89 08             	mov    %rcx,(%rax)
  401642:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  401647:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40164c:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401650:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401655:	48 8b 40 08          	mov    0x8(%rax),%rax
  401659:	48 8b 08             	mov    (%rax),%rcx
  40165c:	48 83 c1 32          	add    $0x32,%rcx
  401660:	48 89 08             	mov    %rcx,(%rax)
  401663:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  401668:	48 83 e8 40          	sub    $0x40,%rax
  40166c:	48 b9 00 00 00 00 32 	movabs $0x3200000000,%rcx
  401673:	00 00 00 
  401676:	48 09 c8             	or     %rcx,%rax
  401679:	c3                   	ret

000000000040167a <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)>:
  40167a:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  40167f:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401684:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  401689:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40168e:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  401693:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  401698:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  40169d:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4016a2:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4016a7:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4016ac:	48 8b 00             	mov    (%rax),%rax
  4016af:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4016b4:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4016b9:	48 8b 00             	mov    (%rax),%rax
  4016bc:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  4016c1:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  4016c5:	48 c1 e1 04          	shl    $0x4,%rcx
  4016c9:	48 01 c8             	add    %rcx,%rax
  4016cc:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4016d1:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  4016d8:	00 00 
  4016da:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4016df:	48 83 e8 02          	sub    $0x2,%rax
  4016e3:	48 83 f8 00          	cmp    $0x0,%rax
  4016e7:	74 4e                	je     401737 <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)+0xbd>
  4016e9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4016ee:	48 8b 44 c4 e8       	mov    -0x18(%rsp,%rax,8),%rax
  4016f3:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4016f8:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4016fd:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  401702:	89 c9                	mov    %ecx,%ecx
  401704:	48 89 08             	mov    %rcx,(%rax)
  401707:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40170c:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  401711:	48 c1 e9 20          	shr    $0x20,%rcx
  401715:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401719:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40171e:	48 83 c0 10          	add    $0x10,%rax
  401722:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401727:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40172c:	48 83 c0 01          	add    $0x1,%rax
  401730:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  401735:	eb a3                	jmp    4016da <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$0,indices:[2]journey::QWORD)+0x60>
  401737:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40173c:	48 8b 48 08          	mov    0x8(%rax),%rcx
  401740:	48 83 c1 02          	add    $0x2,%rcx
  401744:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401748:	c3                   	ret

0000000000401749 <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$1,indices:[1]journey::QWORD)>:
  401749:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40174e:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  401753:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  401758:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40175d:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  401762:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  401767:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40176c:	48 8b 00             	mov    (%rax),%rax
  40176f:	48 83 c0 20          	add    $0x20,%rax
  401773:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  401778:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40177d:	48 8b 00             	mov    (%rax),%rax
  401780:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401785:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  401789:	48 c1 e1 04          	shl    $0x4,%rcx
  40178d:	48 01 c8             	add    %rcx,%rax
  401790:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401795:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  40179c:	00 00 
  40179e:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4017a3:	48 83 e8 01          	sub    $0x1,%rax
  4017a7:	48 83 f8 00          	cmp    $0x0,%rax
  4017ab:	74 4e                	je     4017fb <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$1,indices:[1]journey::QWORD)+0xb2>
  4017ad:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4017b2:	48 8b 44 c4 e8       	mov    -0x18(%rsp,%rax,8),%rax
  4017b7:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  4017bc:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4017c1:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4017c6:	89 c9                	mov    %ecx,%ecx
  4017c8:	48 89 08             	mov    %rcx,(%rax)
  4017cb:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4017d0:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4017d5:	48 c1 e9 20          	shr    $0x20,%rcx
  4017d9:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4017dd:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4017e2:	48 83 c0 10          	add    $0x10,%rax
  4017e6:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4017eb:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4017f0:	48 83 c0 01          	add    $0x1,%rax
  4017f4:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4017f9:	eb a3                	jmp    40179e <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$1,indices:[1]journey::QWORD)+0x55>
  4017fb:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401800:	48 8b 48 08          	mov    0x8(%rax),%rcx
  401804:	48 83 c1 01          	add    $0x1,%rcx
  401808:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40180c:	c3                   	ret

000000000040180d <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$2,indices:[1]journey::QWORD)>:
  40180d:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  401812:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  401817:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40181c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  401821:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  401826:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40182b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  401830:	48 8b 00             	mov    (%rax),%rax
  401833:	48 83 c0 40          	add    $0x40,%rax
  401837:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40183c:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  401841:	48 8b 00             	mov    (%rax),%rax
  401844:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  401849:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  40184d:	48 c1 e1 04          	shl    $0x4,%rcx
  401851:	48 01 c8             	add    %rcx,%rax
  401854:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  401859:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  401860:	00 00 
  401862:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401867:	48 83 e8 01          	sub    $0x1,%rax
  40186b:	48 83 f8 00          	cmp    $0x0,%rax
  40186f:	74 4e                	je     4018bf <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$2,indices:[1]journey::QWORD)+0xb2>
  401871:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  401876:	48 8b 44 c4 e8       	mov    -0x18(%rsp,%rax,8),%rax
  40187b:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  401880:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401885:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  40188a:	89 c9                	mov    %ecx,%ecx
  40188c:	48 89 08             	mov    %rcx,(%rax)
  40188f:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  401894:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  401899:	48 c1 e9 20          	shr    $0x20,%rcx
  40189d:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4018a1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4018a6:	48 83 c0 10          	add    $0x10,%rax
  4018aa:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4018af:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4018b4:	48 83 c0 01          	add    $0x1,%rax
  4018b8:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4018bd:	eb a3                	jmp    401862 <journey::bind_indices_to_data:proc(world:^journey::World,storage_index:$$2,indices:[1]journey::QWORD)+0x55>
  4018bf:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4018c4:	48 8b 48 08          	mov    0x8(%rax),%rcx
  4018c8:	48 83 c1 01          	add    $0x1,%rcx
  4018cc:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4018d0:	c3                   	ret
  4018d1:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4018d8:	00 00 00 
  4018db:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004018e0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  4018e0:	50                   	push   %rax
  4018e1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4018e5:	eb 00                	jmp    4018e7 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x7>
  4018e7:	48 8b 34 24          	mov    (%rsp),%rsi
  4018eb:	48 c7 c0 c0 ff ff ff 	mov    $0xffffffffffffffc0,%rax
  4018f2:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  4018f9:	00 00 
  4018fb:	48 01 c7             	add    %rax,%rdi
  4018fe:	e8 8d 11 00 00       	call   402a90 <runtime::default_temp_allocator_destroy>
  401903:	58                   	pop    %rax
  401904:	c3                   	ret
  401905:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40190c:	00 00 00 00 

0000000000401910 <runtime::bounds_trap>:
  401910:	eb 00                	jmp    401912 <runtime::bounds_trap+0x2>
  401912:	0f 0b                	ud2
  401914:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40191b:	00 00 00 00 00 

0000000000401920 <runtime::heap_allocator>:
  401920:	48 c7 c0 c0 23 40 00 	mov    $0x4023c0,%rax
  401927:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40192c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  401933:	00 00 
  401935:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40193a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40193f:	c3                   	ret

0000000000401940 <runtime::udivmod128>:
  401940:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  401947:	4c 89 44 24 a8       	mov    %r8,-0x58(%rsp)
  40194c:	48 89 4c 24 b0       	mov    %rcx,-0x50(%rsp)
  401951:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  401956:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40195b:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  401960:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  401965:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40196a:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40196f:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  401974:	48 8b 7c 24 a8       	mov    -0x58(%rsp),%rdi
  401979:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  401980:	00 
  401981:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  401988:	00 
  401989:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  401990:	00 
  401991:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  401998:	00 
  401999:	48 89 bc 24 88 00 00 	mov    %rdi,0x88(%rsp)
  4019a0:	00 
  4019a1:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  4019a6:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  4019ab:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4019b0:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4019b5:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4019ba:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  4019bf:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4019c4:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4019c9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4019ce:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  4019d3:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4019d8:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4019dd:	0f 57 c0             	xorps  %xmm0,%xmm0
  4019e0:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4019e5:	0f 57 c0             	xorps  %xmm0,%xmm0
  4019e8:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  4019ed:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  4019f4:	00 
  4019f5:	48 83 7c 24 68 00    	cmpq   $0x0,0x68(%rsp)
  4019fb:	0f 94 c0             	sete   %al
  4019fe:	24 01                	and    $0x1,%al
  401a00:	3c 00                	cmp    $0x0,%al
  401a02:	0f 84 a1 00 00 00    	je     401aa9 <runtime::udivmod128+0x169>
  401a08:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401a0e:	0f 94 c0             	sete   %al
  401a11:	24 01                	and    $0x1,%al
  401a13:	3c 00                	cmp    $0x0,%al
  401a15:	74 5c                	je     401a73 <runtime::udivmod128+0x133>
  401a17:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a1c:	48 83 f8 00          	cmp    $0x0,%rax
  401a20:	0f 95 c0             	setne  %al
  401a23:	24 01                	and    $0x1,%al
  401a25:	3c 00                	cmp    $0x0,%al
  401a27:	74 29                	je     401a52 <runtime::udivmod128+0x112>
  401a29:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401a2e:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401a33:	31 d2                	xor    %edx,%edx
  401a35:	48 f7 f1             	div    %rcx
  401a38:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a3d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  401a42:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401a47:	48 89 08             	mov    %rcx,(%rax)
  401a4a:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401a51:	00 
  401a52:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401a57:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401a5c:	31 d2                	xor    %edx,%edx
  401a5e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  401a63:	48 f7 f1             	div    %rcx
  401a66:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  401a6b:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401a72:	c3                   	ret
  401a73:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a78:	48 83 f8 00          	cmp    $0x0,%rax
  401a7c:	0f 95 c0             	setne  %al
  401a7f:	24 01                	and    $0x1,%al
  401a81:	3c 00                	cmp    $0x0,%al
  401a83:	74 15                	je     401a9a <runtime::udivmod128+0x15a>
  401a85:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401a8a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401a8f:	48 89 08             	mov    %rcx,(%rax)
  401a92:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401a99:	00 
  401a9a:	31 c0                	xor    %eax,%eax
  401a9c:	89 c2                	mov    %eax,%edx
  401a9e:	48 89 d0             	mov    %rdx,%rax
  401aa1:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401aa8:	c3                   	ret
  401aa9:	48 83 7c 24 40 00    	cmpq   $0x0,0x40(%rsp)
  401aaf:	0f 94 c0             	sete   %al
  401ab2:	24 01                	and    $0x1,%al
  401ab4:	3c 00                	cmp    $0x0,%al
  401ab6:	0f 84 8b 02 00 00    	je     401d47 <runtime::udivmod128+0x407>
  401abc:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401ac2:	0f 94 c0             	sete   %al
  401ac5:	24 01                	and    $0x1,%al
  401ac7:	3c 00                	cmp    $0x0,%al
  401ac9:	74 52                	je     401b1d <runtime::udivmod128+0x1dd>
  401acb:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401ad0:	48 83 f8 00          	cmp    $0x0,%rax
  401ad4:	0f 95 c0             	setne  %al
  401ad7:	24 01                	and    $0x1,%al
  401ad9:	3c 00                	cmp    $0x0,%al
  401adb:	74 1f                	je     401afc <runtime::udivmod128+0x1bc>
  401add:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401ae2:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401ae7:	31 d2                	xor    %edx,%edx
  401ae9:	48 f7 f1             	div    %rcx
  401aec:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401af1:	48 89 10             	mov    %rdx,(%rax)
  401af4:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401afb:	00 
  401afc:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b01:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401b06:	31 d2                	xor    %edx,%edx
  401b08:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  401b0d:	48 f7 f1             	div    %rcx
  401b10:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  401b15:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401b1c:	c3                   	ret
  401b1d:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  401b23:	0f 94 c0             	sete   %al
  401b26:	24 01                	and    $0x1,%al
  401b28:	3c 00                	cmp    $0x0,%al
  401b2a:	74 66                	je     401b92 <runtime::udivmod128+0x252>
  401b2c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b31:	48 83 f8 00          	cmp    $0x0,%rax
  401b35:	0f 95 c0             	setne  %al
  401b38:	24 01                	and    $0x1,%al
  401b3a:	3c 00                	cmp    $0x0,%al
  401b3c:	74 33                	je     401b71 <runtime::udivmod128+0x231>
  401b3e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b43:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401b48:	31 d2                	xor    %edx,%edx
  401b4a:	48 f7 f1             	div    %rcx
  401b4d:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401b52:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  401b57:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  401b5e:	00 00 
  401b60:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401b65:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401b6a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401b6e:	48 89 08             	mov    %rcx,(%rax)
  401b71:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401b76:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401b7b:	31 d2                	xor    %edx,%edx
  401b7d:	48 89 54 24 90       	mov    %rdx,-0x70(%rsp)
  401b82:	48 f7 f1             	div    %rcx
  401b85:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  401b8a:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401b91:	c3                   	ret
  401b92:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  401b97:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401b9c:	48 83 e9 01          	sub    $0x1,%rcx
  401ba0:	48 21 c8             	and    %rcx,%rax
  401ba3:	48 83 f8 00          	cmp    $0x0,%rax
  401ba7:	0f 94 c0             	sete   %al
  401baa:	24 01                	and    $0x1,%al
  401bac:	3c 00                	cmp    $0x0,%al
  401bae:	74 7a                	je     401c2a <runtime::udivmod128+0x2ea>
  401bb0:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401bb5:	48 83 f8 00          	cmp    $0x0,%rax
  401bb9:	0f 95 c0             	setne  %al
  401bbc:	24 01                	and    $0x1,%al
  401bbe:	3c 00                	cmp    $0x0,%al
  401bc0:	74 35                	je     401bf7 <runtime::udivmod128+0x2b7>
  401bc2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401bc7:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401bcc:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  401bd1:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  401bd6:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  401bdb:	48 ff ca             	dec    %rdx
  401bde:	48 21 d1             	and    %rdx,%rcx
  401be1:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  401be6:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  401beb:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401bf0:	48 89 50 08          	mov    %rdx,0x8(%rax)
  401bf4:	48 89 08             	mov    %rcx,(%rax)
  401bf7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401bfc:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401c01:	ba 40 00 00 00       	mov    $0x40,%edx
  401c06:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  401c0b:	88 d1                	mov    %dl,%cl
  401c0d:	48 d3 e8             	shr    %cl,%rax
  401c10:	48 89 c1             	mov    %rax,%rcx
  401c13:	31 c0                	xor    %eax,%eax
  401c15:	48 83 ea 40          	sub    $0x40,%rdx
  401c19:	89 c2                	mov    %eax,%edx
  401c1b:	48 89 d0             	mov    %rdx,%rax
  401c1e:	48 0f 42 c1          	cmovb  %rcx,%rax
  401c22:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401c29:	c3                   	ret
  401c2a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401c2f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401c34:	48 0f bd c1          	bsr    %rcx,%rax
  401c38:	48 83 f0 3f          	xor    $0x3f,%rax
  401c3c:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401c41:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401c46:	48 0f bd ca          	bsr    %rdx,%rcx
  401c4a:	48 83 f1 3f          	xor    $0x3f,%rcx
  401c4e:	29 c8                	sub    %ecx,%eax
  401c50:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401c54:	83 7c 24 1c 3e       	cmpl   $0x3e,0x1c(%rsp)
  401c59:	0f 97 c0             	seta   %al
  401c5c:	24 01                	and    $0x1,%al
  401c5e:	3c 00                	cmp    $0x0,%al
  401c60:	74 37                	je     401c99 <runtime::udivmod128+0x359>
  401c62:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401c67:	48 83 f8 00          	cmp    $0x0,%rax
  401c6b:	0f 95 c0             	setne  %al
  401c6e:	24 01                	and    $0x1,%al
  401c70:	3c 00                	cmp    $0x0,%al
  401c72:	74 16                	je     401c8a <runtime::udivmod128+0x34a>
  401c74:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401c79:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  401c7e:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  401c83:	48 89 10             	mov    %rdx,(%rax)
  401c86:	48 89 48 08          	mov    %rcx,0x8(%rax)
  401c8a:	31 c0                	xor    %eax,%eax
  401c8c:	89 c2                	mov    %eax,%edx
  401c8e:	48 89 d0             	mov    %rdx,%rax
  401c91:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401c98:	c3                   	ret
  401c99:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401c9d:	83 c0 01             	add    $0x1,%eax
  401ca0:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401ca4:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401cab:	00 00 
  401cad:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401cb2:	b9 40 00 00 00       	mov    $0x40,%ecx
  401cb7:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401cbb:	89 c9                	mov    %ecx,%ecx
  401cbd:	89 ca                	mov    %ecx,%edx
  401cbf:	48 89 d1             	mov    %rdx,%rcx
  401cc2:	48 d3 e0             	shl    %cl,%rax
  401cc5:	48 89 c1             	mov    %rax,%rcx
  401cc8:	31 c0                	xor    %eax,%eax
  401cca:	48 83 fa 40          	cmp    $0x40,%rdx
  401cce:	48 0f 42 c1          	cmovb  %rcx,%rax
  401cd2:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401cd7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401cdc:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401ce0:	89 ca                	mov    %ecx,%edx
  401ce2:	48 89 d1             	mov    %rdx,%rcx
  401ce5:	48 d3 e8             	shr    %cl,%rax
  401ce8:	48 89 c1             	mov    %rax,%rcx
  401ceb:	31 c0                	xor    %eax,%eax
  401ced:	48 83 fa 40          	cmp    $0x40,%rdx
  401cf1:	48 0f 42 c1          	cmovb  %rcx,%rax
  401cf5:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401cfa:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401cff:	b9 40 00 00 00       	mov    $0x40,%ecx
  401d04:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401d08:	89 c9                	mov    %ecx,%ecx
  401d0a:	89 ca                	mov    %ecx,%edx
  401d0c:	48 89 d1             	mov    %rdx,%rcx
  401d0f:	48 d3 e0             	shl    %cl,%rax
  401d12:	48 89 c1             	mov    %rax,%rcx
  401d15:	31 c0                	xor    %eax,%eax
  401d17:	48 83 fa 40          	cmp    $0x40,%rdx
  401d1b:	48 0f 42 c1          	cmovb  %rcx,%rax
  401d1f:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401d24:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401d28:	89 ce                	mov    %ecx,%esi
  401d2a:	48 89 f1             	mov    %rsi,%rcx
  401d2d:	48 d3 ea             	shr    %cl,%rdx
  401d30:	31 c9                	xor    %ecx,%ecx
  401d32:	48 83 fe 40          	cmp    $0x40,%rsi
  401d36:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401d3a:	48 09 c8             	or     %rcx,%rax
  401d3d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401d42:	e9 30 04 00 00       	jmp    402177 <runtime::udivmod128+0x837>
  401d47:	48 83 7c 24 48 00    	cmpq   $0x0,0x48(%rsp)
  401d4d:	0f 94 c0             	sete   %al
  401d50:	24 01                	and    $0x1,%al
  401d52:	3c 00                	cmp    $0x0,%al
  401d54:	0f 84 d1 02 00 00    	je     40202b <runtime::udivmod128+0x6eb>
  401d5a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401d5f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401d64:	48 83 e9 01          	sub    $0x1,%rcx
  401d68:	48 21 c8             	and    %rcx,%rax
  401d6b:	48 83 f8 00          	cmp    $0x0,%rax
  401d6f:	0f 94 c0             	sete   %al
  401d72:	24 01                	and    $0x1,%al
  401d74:	3c 00                	cmp    $0x0,%al
  401d76:	0f 84 de 00 00 00    	je     401e5a <runtime::udivmod128+0x51a>
  401d7c:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401d81:	48 83 f8 00          	cmp    $0x0,%rax
  401d85:	0f 95 c0             	setne  %al
  401d88:	24 01                	and    $0x1,%al
  401d8a:	3c 00                	cmp    $0x0,%al
  401d8c:	74 20                	je     401dae <runtime::udivmod128+0x46e>
  401d8e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  401d93:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  401d98:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401d9d:	48 ff ca             	dec    %rdx
  401da0:	48 21 d1             	and    %rdx,%rcx
  401da3:	48 89 08             	mov    %rcx,(%rax)
  401da6:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  401dad:	00 
  401dae:	48 83 7c 24 40 01    	cmpq   $0x1,0x40(%rsp)
  401db4:	0f 94 c0             	sete   %al
  401db7:	24 01                	and    $0x1,%al
  401db9:	3c 00                	cmp    $0x0,%al
  401dbb:	74 12                	je     401dcf <runtime::udivmod128+0x48f>
  401dbd:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  401dc2:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  401dc7:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401dce:	c3                   	ret
  401dcf:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401dd4:	b8 40 00 00 00       	mov    $0x40,%eax
  401dd9:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  401dde:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401de2:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401de7:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  401deb:	89 44 24 84          	mov    %eax,-0x7c(%rsp)
  401def:	88 c1                	mov    %al,%cl
  401df1:	48 d3 ea             	shr    %cl,%rdx
  401df4:	8b 4c 24 84          	mov    -0x7c(%rsp),%ecx
  401df8:	31 c0                	xor    %eax,%eax
  401dfa:	83 e9 40             	sub    $0x40,%ecx
  401dfd:	48 89 c1             	mov    %rax,%rcx
  401e00:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401e04:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  401e09:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  401e0e:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401e13:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  401e17:	40 88 f1             	mov    %sil,%cl
  401e1a:	48 d3 ef             	shr    %cl,%rdi
  401e1d:	83 ee 40             	sub    $0x40,%esi
  401e20:	48 89 c1             	mov    %rax,%rcx
  401e23:	48 0f 42 cf          	cmovb  %rdi,%rcx
  401e27:	48 89 4c 24 88       	mov    %rcx,-0x78(%rsp)
  401e2c:	f7 de                	neg    %esi
  401e2e:	40 88 f1             	mov    %sil,%cl
  401e31:	48 d3 e2             	shl    %cl,%rdx
  401e34:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  401e39:	83 ee 40             	sub    $0x40,%esi
  401e3c:	48 0f 42 c2          	cmovb  %rdx,%rax
  401e40:	48 09 c8             	or     %rcx,%rax
  401e43:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401e48:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401e4d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  401e52:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  401e59:	c3                   	ret
  401e5a:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401e5f:	b8 7f 00 00 00       	mov    $0x7f,%eax
  401e64:	48 0f bd c1          	bsr    %rcx,%rax
  401e68:	48 83 f0 3f          	xor    $0x3f,%rax
  401e6c:	83 c0 41             	add    $0x41,%eax
  401e6f:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  401e74:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  401e79:	48 0f bd ca          	bsr    %rdx,%rcx
  401e7d:	48 83 f1 3f          	xor    $0x3f,%rcx
  401e81:	29 c8                	sub    %ecx,%eax
  401e83:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  401e87:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401e8c:	0f 94 c1             	sete   %cl
  401e8f:	80 e1 01             	and    $0x1,%cl
  401e92:	b0 01                	mov    $0x1,%al
  401e94:	38 c8                	cmp    %cl,%al
  401e96:	74 13                	je     401eab <runtime::udivmod128+0x56b>
  401e98:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  401e9d:	0f 92 c1             	setb   %cl
  401ea0:	80 e1 01             	and    $0x1,%cl
  401ea3:	b0 01                	mov    $0x1,%al
  401ea5:	38 c8                	cmp    %cl,%al
  401ea7:	74 32                	je     401edb <runtime::udivmod128+0x59b>
  401ea9:	eb 2b                	jmp    401ed6 <runtime::udivmod128+0x596>
  401eab:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401eb2:	00 00 
  401eb4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401eb9:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401ebe:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401ec5:	00 00 
  401ec7:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401ecc:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401ed1:	e9 50 01 00 00       	jmp    402026 <runtime::udivmod128+0x6e6>
  401ed6:	e9 a3 00 00 00       	jmp    401f7e <runtime::udivmod128+0x63e>
  401edb:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  401ee2:	00 00 
  401ee4:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401ee9:	b9 40 00 00 00       	mov    $0x40,%ecx
  401eee:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401ef2:	89 c9                	mov    %ecx,%ecx
  401ef4:	89 ca                	mov    %ecx,%edx
  401ef6:	48 89 d1             	mov    %rdx,%rcx
  401ef9:	48 d3 e0             	shl    %cl,%rax
  401efc:	48 89 c1             	mov    %rax,%rcx
  401eff:	31 c0                	xor    %eax,%eax
  401f01:	48 83 fa 40          	cmp    $0x40,%rdx
  401f05:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f09:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401f0e:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401f13:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401f17:	89 ca                	mov    %ecx,%edx
  401f19:	48 89 d1             	mov    %rdx,%rcx
  401f1c:	48 d3 e8             	shr    %cl,%rax
  401f1f:	48 89 c1             	mov    %rax,%rcx
  401f22:	31 c0                	xor    %eax,%eax
  401f24:	48 83 fa 40          	cmp    $0x40,%rdx
  401f28:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f2c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401f31:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401f36:	b9 40 00 00 00       	mov    $0x40,%ecx
  401f3b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401f3f:	89 c9                	mov    %ecx,%ecx
  401f41:	89 ca                	mov    %ecx,%edx
  401f43:	48 89 d1             	mov    %rdx,%rcx
  401f46:	48 d3 e0             	shl    %cl,%rax
  401f49:	48 89 c1             	mov    %rax,%rcx
  401f4c:	31 c0                	xor    %eax,%eax
  401f4e:	48 83 fa 40          	cmp    $0x40,%rdx
  401f52:	48 0f 42 c1          	cmovb  %rcx,%rax
  401f56:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401f5b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401f5f:	89 ce                	mov    %ecx,%esi
  401f61:	48 89 f1             	mov    %rsi,%rcx
  401f64:	48 d3 ea             	shr    %cl,%rdx
  401f67:	31 c9                	xor    %ecx,%ecx
  401f69:	48 83 fe 40          	cmp    $0x40,%rsi
  401f6d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401f71:	48 09 c8             	or     %rcx,%rax
  401f74:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401f79:	e9 a8 00 00 00       	jmp    402026 <runtime::udivmod128+0x6e6>
  401f7e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  401f83:	b9 80 00 00 00       	mov    $0x80,%ecx
  401f88:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401f8c:	89 c9                	mov    %ecx,%ecx
  401f8e:	89 ca                	mov    %ecx,%edx
  401f90:	48 89 d1             	mov    %rdx,%rcx
  401f93:	48 d3 e0             	shl    %cl,%rax
  401f96:	48 89 c1             	mov    %rax,%rcx
  401f99:	31 c0                	xor    %eax,%eax
  401f9b:	48 83 fa 40          	cmp    $0x40,%rdx
  401f9f:	48 0f 42 c1          	cmovb  %rcx,%rax
  401fa3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401fa8:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  401fad:	b9 80 00 00 00       	mov    $0x80,%ecx
  401fb2:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  401fb6:	89 c9                	mov    %ecx,%ecx
  401fb8:	89 ca                	mov    %ecx,%edx
  401fba:	48 89 d1             	mov    %rdx,%rcx
  401fbd:	48 d3 e0             	shl    %cl,%rax
  401fc0:	48 89 c1             	mov    %rax,%rcx
  401fc3:	31 c0                	xor    %eax,%eax
  401fc5:	48 83 fa 40          	cmp    $0x40,%rdx
  401fc9:	48 0f 42 c1          	cmovb  %rcx,%rax
  401fcd:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401fd2:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401fd6:	83 e9 40             	sub    $0x40,%ecx
  401fd9:	89 c9                	mov    %ecx,%ecx
  401fdb:	89 ce                	mov    %ecx,%esi
  401fdd:	48 89 f1             	mov    %rsi,%rcx
  401fe0:	48 d3 ea             	shr    %cl,%rdx
  401fe3:	31 c9                	xor    %ecx,%ecx
  401fe5:	48 83 fe 40          	cmp    $0x40,%rsi
  401fe9:	48 0f 42 ca          	cmovb  %rdx,%rcx
  401fed:	48 09 c8             	or     %rcx,%rax
  401ff0:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401ff5:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  401ffc:	00 00 
  401ffe:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402003:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402007:	83 e9 40             	sub    $0x40,%ecx
  40200a:	89 c9                	mov    %ecx,%ecx
  40200c:	89 ca                	mov    %ecx,%edx
  40200e:	48 89 d1             	mov    %rdx,%rcx
  402011:	48 d3 e8             	shr    %cl,%rax
  402014:	48 89 c1             	mov    %rax,%rcx
  402017:	31 c0                	xor    %eax,%eax
  402019:	48 83 fa 40          	cmp    $0x40,%rdx
  40201d:	48 0f 42 c1          	cmovb  %rcx,%rax
  402021:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402026:	e9 4a 01 00 00       	jmp    402175 <runtime::udivmod128+0x835>
  40202b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402030:	b8 7f 00 00 00       	mov    $0x7f,%eax
  402035:	48 0f bd c1          	bsr    %rcx,%rax
  402039:	48 83 f0 3f          	xor    $0x3f,%rax
  40203d:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  402042:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  402047:	48 0f bd ca          	bsr    %rdx,%rcx
  40204b:	48 83 f1 3f          	xor    $0x3f,%rcx
  40204f:	29 c8                	sub    %ecx,%eax
  402051:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  402055:	83 7c 24 1c 3f       	cmpl   $0x3f,0x1c(%rsp)
  40205a:	0f 97 c0             	seta   %al
  40205d:	24 01                	and    $0x1,%al
  40205f:	3c 00                	cmp    $0x0,%al
  402061:	74 37                	je     40209a <runtime::udivmod128+0x75a>
  402063:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  402068:	48 83 f8 00          	cmp    $0x0,%rax
  40206c:	0f 95 c0             	setne  %al
  40206f:	24 01                	and    $0x1,%al
  402071:	3c 00                	cmp    $0x0,%al
  402073:	74 16                	je     40208b <runtime::udivmod128+0x74b>
  402075:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40207a:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  40207f:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  402084:	48 89 10             	mov    %rdx,(%rax)
  402087:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40208b:	31 c0                	xor    %eax,%eax
  40208d:	89 c2                	mov    %eax,%edx
  40208f:	48 89 d0             	mov    %rdx,%rax
  402092:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  402099:	c3                   	ret
  40209a:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  40209e:	83 c0 01             	add    $0x1,%eax
  4020a1:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4020a5:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4020ac:	00 00 
  4020ae:	83 7c 24 1c 40       	cmpl   $0x40,0x1c(%rsp)
  4020b3:	0f 94 c0             	sete   %al
  4020b6:	24 01                	and    $0x1,%al
  4020b8:	3c 00                	cmp    $0x0,%al
  4020ba:	74 22                	je     4020de <runtime::udivmod128+0x79e>
  4020bc:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  4020c1:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4020c6:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  4020cd:	00 00 
  4020cf:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4020d4:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4020d9:	e9 95 00 00 00       	jmp    402173 <runtime::udivmod128+0x833>
  4020de:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  4020e3:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4020e7:	89 ca                	mov    %ecx,%edx
  4020e9:	48 89 d1             	mov    %rdx,%rcx
  4020ec:	48 d3 e8             	shr    %cl,%rax
  4020ef:	48 89 c1             	mov    %rax,%rcx
  4020f2:	31 c0                	xor    %eax,%eax
  4020f4:	48 83 fa 40          	cmp    $0x40,%rdx
  4020f8:	48 0f 42 c1          	cmovb  %rcx,%rax
  4020fc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402101:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402106:	b9 40 00 00 00       	mov    $0x40,%ecx
  40210b:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  40210f:	89 c9                	mov    %ecx,%ecx
  402111:	89 ca                	mov    %ecx,%edx
  402113:	48 89 d1             	mov    %rdx,%rcx
  402116:	48 d3 e0             	shl    %cl,%rax
  402119:	48 89 c1             	mov    %rax,%rcx
  40211c:	31 c0                	xor    %eax,%eax
  40211e:	48 83 fa 40          	cmp    $0x40,%rdx
  402122:	48 0f 42 c1          	cmovb  %rcx,%rax
  402126:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40212b:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40212f:	89 ce                	mov    %ecx,%esi
  402131:	48 89 f1             	mov    %rsi,%rcx
  402134:	48 d3 ea             	shr    %cl,%rdx
  402137:	31 c9                	xor    %ecx,%ecx
  402139:	48 83 fe 40          	cmp    $0x40,%rsi
  40213d:	48 0f 42 ca          	cmovb  %rdx,%rcx
  402141:	48 09 c8             	or     %rcx,%rax
  402144:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  402149:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40214e:	b9 40 00 00 00       	mov    $0x40,%ecx
  402153:	2b 4c 24 1c          	sub    0x1c(%rsp),%ecx
  402157:	89 c9                	mov    %ecx,%ecx
  402159:	89 ca                	mov    %ecx,%edx
  40215b:	48 89 d1             	mov    %rdx,%rcx
  40215e:	48 d3 e0             	shl    %cl,%rax
  402161:	48 89 c1             	mov    %rax,%rcx
  402164:	31 c0                	xor    %eax,%eax
  402166:	48 83 fa 40          	cmp    $0x40,%rdx
  40216a:	48 0f 42 c1          	cmovb  %rcx,%rax
  40216e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402173:	eb 00                	jmp    402175 <runtime::udivmod128+0x835>
  402175:	eb 00                	jmp    402177 <runtime::udivmod128+0x837>
  402177:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  40217e:	00 
  40217f:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  402186:	00 00 
  402188:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  40218f:	00 00 
  402191:	83 7c 24 1c 00       	cmpl   $0x0,0x1c(%rsp)
  402196:	0f 97 c0             	seta   %al
  402199:	24 01                	and    $0x1,%al
  40219b:	3c 00                	cmp    $0x0,%al
  40219d:	0f 84 eb 00 00 00    	je     40228e <runtime::udivmod128+0x94e>
  4021a3:	48 8b 74 24 b8       	mov    -0x48(%rsp),%rsi
  4021a8:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  4021ad:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4021b2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4021b7:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  4021bc:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4021c1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4021c6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4021cb:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  4021d0:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4021d5:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4021da:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4021df:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  4021e4:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4021e9:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4021ee:	48 01 c0             	add    %rax,%rax
  4021f1:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4021f5:	48 09 c8             	or     %rcx,%rax
  4021f8:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4021fd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402202:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  402207:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40220c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  402211:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  402216:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40221b:	48 f7 d0             	not    %rax
  40221e:	48 f7 d1             	not    %rcx
  402221:	48 01 f1             	add    %rsi,%rcx
  402224:	48 11 d0             	adc    %rdx,%rax
  402227:	48 c1 f8 3f          	sar    $0x3f,%rax
  40222b:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402230:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  402235:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  402239:	83 e0 01             	and    $0x1,%eax
  40223c:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  402240:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  402245:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40224a:	48 21 ca             	and    %rcx,%rdx
  40224d:	48 21 c6             	and    %rax,%rsi
  402250:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  402255:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40225a:	48 29 f1             	sub    %rsi,%rcx
  40225d:	48 19 d0             	sbb    %rdx,%rax
  402260:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  402265:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40226a:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40226f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  402274:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  402279:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40227e:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  402282:	83 e8 01             	sub    $0x1,%eax
  402285:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  402289:	e9 03 ff ff ff       	jmp    402191 <runtime::udivmod128+0x851>
  40228e:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  402293:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  402298:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40229d:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  4022a2:	48 01 c9             	add    %rcx,%rcx
  4022a5:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  4022a9:	48 09 f1             	or     %rsi,%rcx
  4022ac:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  4022b1:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  4022b6:	48 83 f8 00          	cmp    $0x0,%rax
  4022ba:	0f 95 c0             	setne  %al
  4022bd:	24 01                	and    $0x1,%al
  4022bf:	3c 00                	cmp    $0x0,%al
  4022c1:	74 16                	je     4022d9 <runtime::udivmod128+0x999>
  4022c3:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  4022c8:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4022cd:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4022d2:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4022d6:	48 89 08             	mov    %rcx,(%rax)
  4022d9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4022de:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  4022e3:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4022ea:	c3                   	ret
  4022eb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004022f0 <runtime::stderr_write>:
  4022f0:	48 83 ec 48          	sub    $0x48,%rsp
  4022f4:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4022f9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4022fe:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  402303:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402308:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40230d:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  402312:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  402317:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40231e:	00 00 
  402320:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  402325:	e8 16 00 00 00       	call   402340 <runtime::[os_specific_linux.odin]::_stderr_write>
  40232a:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40232f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402334:	48 89 11             	mov    %rdx,(%rcx)
  402337:	48 83 c4 48          	add    $0x48,%rsp
  40233b:	c3                   	ret
  40233c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402340 <runtime::[os_specific_linux.odin]::_stderr_write>:
  402340:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  402345:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40234a:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  40234f:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  402354:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  402359:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  40235e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  402363:	b8 01 00 00 00       	mov    $0x1,%eax
  402368:	bf 02 00 00 00       	mov    $0x2,%edi
  40236d:	0f 05                	syscall
  40236f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402374:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  40237a:	0f 9c c0             	setl   %al
  40237d:	24 01                	and    $0x1,%al
  40237f:	3c 00                	cmp    $0x0,%al
  402381:	74 26                	je     4023a9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  402383:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  40238a:	ff ff 
  40238c:	0f 9f c0             	setg   %al
  40238f:	24 01                	and    $0x1,%al
  402391:	3c 00                	cmp    $0x0,%al
  402393:	74 14                	je     4023a9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  402395:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  40239a:	31 c0                	xor    %eax,%eax
  40239c:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  4023a1:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  4023a8:	c3                   	ret
  4023a9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4023ae:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4023b3:	48 89 08             	mov    %rcx,(%rax)
  4023b6:	31 c0                	xor    %eax,%eax
  4023b8:	c3                   	ret
  4023b9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004023c0 <runtime::heap_allocator_proc>:
  4023c0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4023c7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  4023cc:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  4023d1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4023d6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4023db:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  4023e0:	40 88 f0             	mov    %sil,%al
  4023e3:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  4023e7:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  4023ee:	00 
  4023ef:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4023f4:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  4023fb:	00 
  4023fc:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402401:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  402405:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40240a:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40240f:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402414:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402419:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  40241e:	4c 89 84 24 e0 00 00 	mov    %r8,0xe0(%rsp)
  402425:	00 
  402426:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  40242d:	48 89 bc 24 d0 00 00 	mov    %rdi,0xd0(%rsp)
  402434:	00 
  402435:	48 89 b4 24 c8 00 00 	mov    %rsi,0xc8(%rsp)
  40243c:	00 
  40243d:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  402444:	00 
  402445:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40244c:	00 
  40244d:	0f b6 c8             	movzbl %al,%ecx
  402450:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  402455:	2c 07                	sub    $0x7,%al
  402457:	0f 87 5f 01 00 00    	ja     4025bc <runtime::heap_allocator_proc+0x1fc>
  40245d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402462:	48 8b 04 c5 10 70 40 	mov    0x407010(,%rax,8),%rax
  402469:	00 
  40246a:	ff e0                	jmp    *%rax
  40246c:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402471:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402476:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40247b:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  40247f:	84 c0                	test   %al,%al
  402481:	0f 94 c0             	sete   %al
  402484:	0f 57 c0             	xorps  %xmm0,%xmm0
  402487:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40248e:	00 
  40248f:	48 89 e1             	mov    %rsp,%rcx
  402492:	48 89 11             	mov    %rdx,(%rcx)
  402495:	44 0f b6 c0          	movzbl %al,%r8d
  402499:	31 c0                	xor    %eax,%eax
  40249b:	89 c1                	mov    %eax,%ecx
  40249d:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  4024a4:	00 
  4024a5:	48 89 ca             	mov    %rcx,%rdx
  4024a8:	e8 43 3f 00 00       	call   4063f0 <runtime::heap_allocator_proc.aligned_alloc-0>
  4024ad:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4024b2:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4024b9:	00 
  4024ba:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4024c1:	00 
  4024c2:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4024c6:	48 89 11             	mov    %rdx,(%rcx)
  4024c9:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4024d0:	c3                   	ret
  4024d1:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4024d6:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4024db:	e8 60 41 00 00       	call   406640 <runtime::heap_allocator_proc.aligned_free-1>
  4024e0:	e9 d7 00 00 00       	jmp    4025bc <runtime::heap_allocator_proc+0x1fc>
  4024e5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4024ea:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4024f1:	00 
  4024f2:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4024f9:	b0 04                	mov    $0x4,%al
  4024fb:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402502:	c3                   	ret
  402503:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402508:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40250d:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  402512:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  402517:	4c 8b 4c 24 40       	mov    0x40(%rsp),%r9
  40251c:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  402520:	2c 03                	sub    $0x3,%al
  402522:	0f 94 c0             	sete   %al
  402525:	0f 57 c0             	xorps  %xmm0,%xmm0
  402528:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40252d:	49 89 e0             	mov    %rsp,%r8
  402530:	4d 89 08             	mov    %r9,(%r8)
  402533:	44 0f b6 c0          	movzbl %al,%r8d
  402537:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  40253c:	e8 3f 41 00 00       	call   406680 <runtime::heap_allocator_proc.aligned_resize-2>
  402541:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402546:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  40254b:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  402550:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402554:	48 89 11             	mov    %rdx,(%rcx)
  402557:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40255e:	c3                   	ret
  40255f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402564:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402569:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  40256f:	0f 95 c0             	setne  %al
  402572:	24 01                	and    $0x1,%al
  402574:	3c 00                	cmp    $0x0,%al
  402576:	74 08                	je     402580 <runtime::heap_allocator_proc+0x1c0>
  402578:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40257d:	c6 00 db             	movb   $0xdb,(%rax)
  402580:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402585:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  40258c:	00 
  40258d:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402594:	31 c0                	xor    %eax,%eax
  402596:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40259d:	c3                   	ret
  40259e:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4025a3:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4025aa:	00 
  4025ab:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4025b2:	b0 04                	mov    $0x4,%al
  4025b4:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4025bb:	c3                   	ret
  4025bc:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4025c1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4025c8:	00 
  4025c9:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4025d0:	31 c0                	xor    %eax,%eax
  4025d2:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4025d9:	c3                   	ret
  4025da:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004025e0 <runtime::[internal.odin]::byte_slice>:
  4025e0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4025e5:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  4025ea:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  4025ef:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4025f4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4025f9:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  4025fe:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  402603:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  402608:	31 c0                	xor    %eax,%eax
  40260a:	48 85 d2             	test   %rdx,%rdx
  40260d:	48 0f 49 c2          	cmovns %rdx,%rax
  402611:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  402616:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40261b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  402620:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  402625:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40262a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40262f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  402634:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  402639:	c3                   	ret
  40263a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402640 <runtime::bounds_check_error>:
  402640:	48 83 ec 58          	sub    $0x58,%rsp
  402644:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402649:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40264e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402652:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402656:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40265b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402660:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402665:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40266a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40266e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  402672:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  402677:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40267c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  402681:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  402686:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40268a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40268e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402693:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402698:	48 39 c8             	cmp    %rcx,%rax
  40269b:	0f 92 c0             	setb   %al
  40269e:	24 01                	and    $0x1,%al
  4026a0:	3c 00                	cmp    $0x0,%al
  4026a2:	74 05                	je     4026a9 <runtime::bounds_check_error+0x69>
  4026a4:	48 83 c4 58          	add    $0x58,%rsp
  4026a8:	c3                   	ret
  4026a9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4026ae:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4026b3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4026b7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4026bb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4026c0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4026c5:	e8 76 42 00 00       	call   406940 <runtime::bounds_check_error.handle_error-0>
  4026ca:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004026d0 <runtime::is_power_of_two_int>:
  4026d0:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  4026d5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4026da:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4026df:	48 83 f8 00          	cmp    $0x0,%rax
  4026e3:	0f 9e c0             	setle  %al
  4026e6:	24 01                	and    $0x1,%al
  4026e8:	3c 00                	cmp    $0x0,%al
  4026ea:	74 03                	je     4026ef <runtime::is_power_of_two_int+0x1f>
  4026ec:	31 c0                	xor    %eax,%eax
  4026ee:	c3                   	ret
  4026ef:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4026f4:	48 89 c1             	mov    %rax,%rcx
  4026f7:	48 83 e9 01          	sub    $0x1,%rcx
  4026fb:	48 21 c8             	and    %rcx,%rax
  4026fe:	48 83 f8 00          	cmp    $0x0,%rax
  402702:	0f 94 c0             	sete   %al
  402705:	24 01                	and    $0x1,%al
  402707:	c3                   	ret
  402708:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40270f:	00 

0000000000402710 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  402710:	48 83 ec 18          	sub    $0x18,%rsp
  402714:	48 89 3c 24          	mov    %rdi,(%rsp)
  402718:	40 88 f0             	mov    %sil,%al
  40271b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40271f:	48 8b 04 24          	mov    (%rsp),%rax
  402723:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  402727:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40272c:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  402730:	48 83 f8 00          	cmp    $0x0,%rax
  402734:	0f 9e c0             	setle  %al
  402737:	24 01                	and    $0x1,%al
  402739:	3c 00                	cmp    $0x0,%al
  40273b:	74 07                	je     402744 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  40273d:	31 c0                	xor    %eax,%eax
  40273f:	48 83 c4 18          	add    $0x18,%rsp
  402743:	c3                   	ret
  402744:	8a 44 24 0e          	mov    0xe(%rsp),%al
  402748:	3c 00                	cmp    $0x0,%al
  40274a:	74 13                	je     40275f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  40274c:	48 8b 34 24          	mov    (%rsp),%rsi
  402750:	bf 01 00 00 00       	mov    $0x1,%edi
  402755:	e8 f6 e8 ff ff       	call   401050 <calloc@plt>
  40275a:	48 83 c4 18          	add    $0x18,%rsp
  40275e:	c3                   	ret
  40275f:	48 8b 3c 24          	mov    (%rsp),%rdi
  402763:	e8 08 e9 ff ff       	call   401070 <malloc@plt>
  402768:	48 83 c4 18          	add    $0x18,%rsp
  40276c:	c3                   	ret
  40276d:	0f 1f 00             	nopl   (%rax)

0000000000402770 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  402770:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  402775:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  40277a:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  40277f:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  402784:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  402789:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40278e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  402793:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  402798:	48 01 c2             	add    %rax,%rdx
  40279b:	0f 92 c0             	setb   %al
  40279e:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  4027a3:	24 01                	and    $0x1,%al
  4027a5:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  4027a9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4027ae:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  4027b3:	0f 94 c0             	sete   %al
  4027b6:	24 01                	and    $0x1,%al
  4027b8:	48 89 11             	mov    %rdx,(%rcx)
  4027bb:	c3                   	ret
  4027bc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004027c0 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  4027c0:	48 83 ec 28          	sub    $0x28,%rsp
  4027c4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4027c9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4027ce:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4027d3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4027d8:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4027dd:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4027e2:	e8 99 e8 ff ff       	call   401080 <realloc@plt>
  4027e7:	48 83 c4 28          	add    $0x28,%rsp
  4027eb:	c3                   	ret
  4027ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004027f0 <runtime::memory_block_alloc>:
  4027f0:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  4027f7:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  4027fc:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  402801:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  402806:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40280b:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  402810:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402815:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  40281c:	00 
  40281d:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402822:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  402827:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  40282c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  402831:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  402836:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40283b:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  402840:	48 89 b4 24 f0 00 00 	mov    %rsi,0xf0(%rsp)
  402847:	00 
  402848:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  40284f:	00 
  402850:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  402857:	00 
  402858:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40285d:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  402864:	00 
  402865:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40286a:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  402871:	00 
  402872:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  402879:	00 
  40287a:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  402881:	00 00 00 00 00 
  402886:	c6 84 24 d7 00 00 00 	movb   $0x0,0xd7(%rsp)
  40288d:	00 
  40288e:	48 89 c1             	mov    %rax,%rcx
  402891:	48 83 e9 31          	sub    $0x31,%rcx
  402895:	b9 30 00 00 00       	mov    $0x30,%ecx
  40289a:	48 0f 43 c8          	cmovae %rax,%rcx
  40289e:	48 01 ca             	add    %rcx,%rdx
  4028a1:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  4028a8:	00 
  4028a9:	48 89 8c 24 c0 00 00 	mov    %rcx,0xc0(%rsp)
  4028b0:	00 
  4028b1:	48 89 c1             	mov    %rax,%rcx
  4028b4:	48 83 e9 10          	sub    $0x10,%rcx
  4028b8:	b9 10 00 00 00       	mov    $0x10,%ecx
  4028bd:	48 0f 4c c1          	cmovl  %rcx,%rax
  4028c1:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4028c8:	00 
  4028c9:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  4028d0:	00 
  4028d1:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  4028d8:	00 
  4028d9:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  4028e0:	00 
  4028e1:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  4028e8:	00 
  4028e9:	0f 57 c0             	xorps  %xmm0,%xmm0
  4028ec:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  4028f3:	00 
  4028f4:	48 89 e0             	mov    %rsp,%rax
  4028f7:	4c 89 08             	mov    %r9,(%rax)
  4028fa:	4c 8d 8c 24 a0 00 00 	lea    0xa0(%rsp),%r9
  402901:	00 
  402902:	e8 f9 13 00 00       	call   403d00 <runtime::mem_alloc>
  402907:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  40290b:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  402912:	00 
  402913:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  402918:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  40291f:	00 
  402920:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  402925:	3c 00                	cmp    $0x0,%al
  402927:	74 39                	je     402962 <runtime::memory_block_alloc+0x172>
  402929:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40292e:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  402932:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402939:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402940:	00 
  402941:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  402948:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  40294f:	00 
  402950:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402957:	48 89 11             	mov    %rdx,(%rcx)
  40295a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402961:	c3                   	ret
  402962:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  402967:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40296c:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  402971:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402976:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40297b:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  402980:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402987:	00 
  402988:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40298d:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  402994:	00 
  402995:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40299a:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  4029a1:	00 
  4029a2:	48 01 f0             	add    %rsi,%rax
  4029a5:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4029aa:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4029af:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4029b4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4029bb:	00 
  4029bc:	48 89 50 10          	mov    %rdx,0x10(%rax)
  4029c0:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4029c4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4029cb:	00 
  4029cc:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  4029d3:	00 
  4029d4:	48 03 8c 24 c0 00 00 	add    0xc0(%rsp),%rcx
  4029db:	00 
  4029dc:	48 89 48 18          	mov    %rcx,0x18(%rax)
  4029e0:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4029e7:	00 
  4029e8:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4029ed:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  4029f4:	00 
  4029f5:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  4029f9:	48 29 d1             	sub    %rdx,%rcx
  4029fc:	48 89 48 28          	mov    %rcx,0x28(%rax)
  402a00:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402a07:	00 
  402a08:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  402a0d:	0f 94 c0             	sete   %al
  402a10:	24 01                	and    $0x1,%al
  402a12:	0f b6 f8             	movzbl %al,%edi
  402a15:	be 90 70 40 00       	mov    $0x407090,%esi
  402a1a:	b9 00 71 40 00       	mov    $0x407100,%ecx
  402a1f:	ba 0f 00 00 00       	mov    $0xf,%edx
  402a24:	e8 67 39 00 00       	call   406390 <runtime::assert>
  402a29:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  402a2e:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  402a35:	00 
  402a36:	48 83 38 00          	cmpq   $0x0,(%rax)
  402a3a:	0f 94 c0             	sete   %al
  402a3d:	24 01                	and    $0x1,%al
  402a3f:	0f b6 f8             	movzbl %al,%edi
  402a42:	be 28 71 40 00       	mov    $0x407128,%esi
  402a47:	b9 40 71 40 00       	mov    $0x407140,%ecx
  402a4c:	ba 11 00 00 00       	mov    $0x11,%edx
  402a51:	e8 3a 39 00 00       	call   406390 <runtime::assert>
  402a56:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  402a5b:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  402a62:	00 
  402a63:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  402a6a:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  402a71:	00 
  402a72:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  402a79:	48 89 11             	mov    %rdx,(%rcx)
  402a7c:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  402a83:	c3                   	ret
  402a84:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  402a8b:	00 00 00 00 00 

0000000000402a90 <runtime::default_temp_allocator_destroy>:
  402a90:	48 83 ec 18          	sub    $0x18,%rsp
  402a94:	48 89 3c 24          	mov    %rdi,(%rsp)
  402a98:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402a9d:	48 8b 04 24          	mov    (%rsp),%rax
  402aa1:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402aa6:	48 83 f8 00          	cmp    $0x0,%rax
  402aaa:	0f 95 c0             	setne  %al
  402aad:	24 01                	and    $0x1,%al
  402aaf:	3c 00                	cmp    $0x0,%al
  402ab1:	74 29                	je     402adc <runtime::default_temp_allocator_destroy+0x4c>
  402ab3:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  402ab8:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402abd:	48 be d0 71 40 00 00 	movabs $0x4071d0,%rsi
  402ac4:	00 00 00 
  402ac7:	e8 14 1b 00 00       	call   4045e0 <runtime::arena_destroy>
  402acc:	48 8b 3c 24          	mov    (%rsp),%rdi
  402ad0:	31 f6                	xor    %esi,%esi
  402ad2:	ba 38 00 00 00       	mov    $0x38,%edx
  402ad7:	e8 64 e5 ff ff       	call   401040 <memset@plt>
  402adc:	48 83 c4 18          	add    $0x18,%rsp
  402ae0:	c3                   	ret
  402ae1:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402ae8:	0f 1f 84 00 00 00 00 
  402aef:	00 

0000000000402af0 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  402af0:	48 83 ec 18          	sub    $0x18,%rsp
  402af4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402af9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402afe:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402b03:	e8 28 e5 ff ff       	call   401030 <free@plt>
  402b08:	48 83 c4 18          	add    $0x18,%rsp
  402b0c:	c3                   	ret
  402b0d:	0f 1f 00             	nopl   (%rax)

0000000000402b10 <runtime::default_random_generator_proc>:
  402b10:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402b17:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  402b1c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  402b21:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  402b26:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  402b2b:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402b30:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  402b35:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402b3a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  402b3f:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402b46:	00 
  402b47:	48 89 74 24 78       	mov    %rsi,0x78(%rsp)
  402b4c:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  402b51:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  402b56:	48 83 f8 00          	cmp    $0x0,%rax
  402b5a:	0f 94 c0             	sete   %al
  402b5d:	24 01                	and    $0x1,%al
  402b5f:	3c 00                	cmp    $0x0,%al
  402b61:	74 1a                	je     402b7d <runtime::default_random_generator_proc+0x6d>
  402b63:	48 c7 c1 b0 ff ff ff 	mov    $0xffffffffffffffb0,%rcx
  402b6a:	64 48 8b 04 25 00 00 	mov    %fs:0x0,%rax
  402b71:	00 00 
  402b73:	48 01 c8             	add    %rcx,%rax
  402b76:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402b7b:	eb 0a                	jmp    402b87 <runtime::default_random_generator_proc+0x77>
  402b7d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  402b82:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402b87:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402b8c:	48 85 c0             	test   %rax,%rax
  402b8f:	74 27                	je     402bb8 <runtime::default_random_generator_proc+0xa8>
  402b91:	eb 00                	jmp    402b93 <runtime::default_random_generator_proc+0x83>
  402b93:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402b98:	48 83 e8 01          	sub    $0x1,%rax
  402b9c:	0f 84 17 01 00 00    	je     402cb9 <runtime::default_random_generator_proc+0x1a9>
  402ba2:	eb 00                	jmp    402ba4 <runtime::default_random_generator_proc+0x94>
  402ba4:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  402ba9:	48 83 e8 02          	sub    $0x2,%rax
  402bad:	0f 84 40 01 00 00    	je     402cf3 <runtime::default_random_generator_proc+0x1e3>
  402bb3:	e9 6b 01 00 00       	jmp    402d23 <runtime::default_random_generator_proc+0x213>
  402bb8:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402bbd:	48 83 38 00          	cmpq   $0x0,(%rax)
  402bc1:	0f 94 c0             	sete   %al
  402bc4:	24 01                	and    $0x1,%al
  402bc6:	3c 00                	cmp    $0x0,%al
  402bc8:	74 21                	je     402beb <runtime::default_random_generator_proc+0xdb>
  402bca:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402bcf:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  402bd4:	0f 94 c0             	sete   %al
  402bd7:	24 01                	and    $0x1,%al
  402bd9:	3c 00                	cmp    $0x0,%al
  402bdb:	74 0e                	je     402beb <runtime::default_random_generator_proc+0xdb>
  402bdd:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402be2:	31 c0                	xor    %eax,%eax
  402be4:	89 c6                	mov    %eax,%esi
  402be6:	e8 15 3f 00 00       	call   406b00 <runtime::default_random_generator_proc.init-1>
  402beb:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402bf0:	48 83 e8 08          	sub    $0x8,%rax
  402bf4:	75 26                	jne    402c1c <runtime::default_random_generator_proc+0x10c>
  402bf6:	eb 00                	jmp    402bf8 <runtime::default_random_generator_proc+0xe8>
  402bf8:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402bfd:	e8 2e 3e 00 00       	call   406a30 <runtime::default_random_generator_proc.read_u64-0>
  402c02:	48 89 c1             	mov    %rax,%rcx
  402c05:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c0a:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  402c0f:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402c14:	48 89 08             	mov    %rcx,(%rax)
  402c17:	e9 9b 00 00 00       	jmp    402cb7 <runtime::default_random_generator_proc+0x1a7>
  402c1c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402c21:	c6 44 24 57 00       	movb   $0x0,0x57(%rsp)
  402c26:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  402c2d:	00 00 
  402c2f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  402c34:	48 c7 44 24 38 ff ff 	movq   $0xffffffffffffffff,0x38(%rsp)
  402c3b:	ff ff 
  402c3d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  402c42:	48 83 c0 01          	add    $0x1,%rax
  402c46:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  402c4b:	48 3b 44 24 40       	cmp    0x40(%rsp),%rax
  402c50:	7d 63                	jge    402cb5 <runtime::default_random_generator_proc+0x1a5>
  402c52:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402c57:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  402c5c:	48 01 c8             	add    %rcx,%rax
  402c5f:	48 89 04 24          	mov    %rax,(%rsp)
  402c63:	80 7c 24 57 00       	cmpb   $0x0,0x57(%rsp)
  402c68:	0f 94 c0             	sete   %al
  402c6b:	24 01                	and    $0x1,%al
  402c6d:	3c 00                	cmp    $0x0,%al
  402c6f:	74 14                	je     402c85 <runtime::default_random_generator_proc+0x175>
  402c71:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402c76:	e8 b5 3d 00 00       	call   406a30 <runtime::default_random_generator_proc.read_u64-0>
  402c7b:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402c80:	c6 44 24 57 07       	movb   $0x7,0x57(%rsp)
  402c85:	48 8b 04 24          	mov    (%rsp),%rax
  402c89:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402c8e:	88 08                	mov    %cl,(%rax)
  402c90:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  402c95:	48 c1 e9 08          	shr    $0x8,%rcx
  402c99:	b2 01                	mov    $0x1,%dl
  402c9b:	31 c0                	xor    %eax,%eax
  402c9d:	f6 c2 01             	test   $0x1,%dl
  402ca0:	48 0f 45 c1          	cmovne %rcx,%rax
  402ca4:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402ca9:	8a 44 24 57          	mov    0x57(%rsp),%al
  402cad:	2c 01                	sub    $0x1,%al
  402caf:	88 44 24 57          	mov    %al,0x57(%rsp)
  402cb3:	eb 88                	jmp    402c3d <runtime::default_random_generator_proc+0x12d>
  402cb5:	eb 00                	jmp    402cb7 <runtime::default_random_generator_proc+0x1a7>
  402cb7:	eb 6a                	jmp    402d23 <runtime::default_random_generator_proc+0x213>
  402cb9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402cbe:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  402cc3:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  402cca:	00 00 
  402ccc:	b8 08 00 00 00       	mov    $0x8,%eax
  402cd1:	48 39 d0             	cmp    %rdx,%rax
  402cd4:	48 0f 4c d0          	cmovl  %rax,%rdx
  402cd8:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402cdd:	e8 5e 09 00 00       	call   403640 <runtime::mem_copy_non_overlapping>
  402ce2:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  402ce7:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  402cec:	e8 0f 3e 00 00       	call   406b00 <runtime::default_random_generator_proc.init-1>
  402cf1:	eb 30                	jmp    402d23 <runtime::default_random_generator_proc+0x213>
  402cf3:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  402cf8:	48 83 f8 04          	cmp    $0x4,%rax
  402cfc:	0f 95 c0             	setne  %al
  402cff:	24 01                	and    $0x1,%al
  402d01:	3c 00                	cmp    $0x0,%al
  402d03:	74 08                	je     402d0d <runtime::default_random_generator_proc+0x1fd>
  402d05:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402d0c:	c3                   	ret
  402d0d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  402d12:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402d17:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  402d1c:	8b 08                	mov    (%rax),%ecx
  402d1e:	83 c9 0a             	or     $0xa,%ecx
  402d21:	89 08                	mov    %ecx,(%rax)
  402d23:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  402d2a:	c3                   	ret
  402d2b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000402d30 <runtime::slice_handle_error>:
  402d30:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402d37:	4c 89 0c 24          	mov    %r9,(%rsp)
  402d3b:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  402d40:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  402d44:	89 54 24 14          	mov    %edx,0x14(%rsp)
  402d48:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  402d4d:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  402d52:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  402d59:	00 
  402d5a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  402d5f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402d64:	4c 8b 04 24          	mov    (%rsp),%r8
  402d68:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  402d6d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  402d71:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  402d75:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  402d7a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  402d7f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402d84:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  402d8b:	00 
  402d8c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402d90:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402d94:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  402d99:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  402d9e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  402da3:	0f 57 c0             	xorps  %xmm0,%xmm0
  402da6:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402dab:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402db0:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402db7:	00 00 
  402db9:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402dbe:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402dc3:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402dca:	00 00 
  402dcc:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  402dd1:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402dd6:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  402dda:	89 44 24 44          	mov    %eax,0x44(%rsp)
  402dde:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  402de3:	e8 c8 16 00 00       	call   4044b0 <runtime::print_caller_location>
  402de8:	bf f9 71 40 00       	mov    $0x4071f9,%edi
  402ded:	be 17 00 00 00       	mov    $0x17,%esi
  402df2:	e8 99 0e 00 00       	call   403c90 <runtime::print_string>
  402df7:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402dfc:	e8 af 14 00 00       	call   4042b0 <runtime::print_i64>
  402e01:	bf 11 72 40 00       	mov    $0x407211,%edi
  402e06:	be 01 00 00 00       	mov    $0x1,%esi
  402e0b:	e8 80 0e 00 00       	call   403c90 <runtime::print_string>
  402e10:	48 8b 3c 24          	mov    (%rsp),%rdi
  402e14:	e8 97 14 00 00       	call   4042b0 <runtime::print_i64>
  402e19:	bf 13 72 40 00       	mov    $0x407213,%edi
  402e1e:	be 15 00 00 00       	mov    $0x15,%esi
  402e23:	e8 68 0e 00 00       	call   403c90 <runtime::print_string>
  402e28:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  402e2d:	e8 7e 14 00 00       	call   4042b0 <runtime::print_i64>
  402e32:	bf 0a 00 00 00       	mov    $0xa,%edi
  402e37:	e8 84 10 00 00       	call   403ec0 <runtime::print_byte>
  402e3c:	e8 cf ea ff ff       	call   401910 <runtime::bounds_trap>
  402e41:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  402e48:	0f 1f 84 00 00 00 00 
  402e4f:	00 

0000000000402e50 <runtime::default_temp_allocator_proc>:
  402e50:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  402e57:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  402e5c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  402e61:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  402e66:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  402e6b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  402e70:	40 88 f0             	mov    %sil,%al
  402e73:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  402e77:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  402e7e:	00 
  402e7f:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402e84:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  402e8b:	00 
  402e8c:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  402e91:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  402e98:	00 
  402e99:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402e9e:	4c 8b 4c 24 20       	mov    0x20(%rsp),%r9
  402ea3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  402ea8:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  402ead:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  402eb2:	8a 44 24 4f          	mov    0x4f(%rsp),%al
  402eb6:	4c 8b 54 24 60       	mov    0x60(%rsp),%r10
  402ebb:	4c 8b 5c 24 50       	mov    0x50(%rsp),%r11
  402ec0:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  402ec5:	48 89 b4 24 e0 00 00 	mov    %rsi,0xe0(%rsp)
  402ecc:	00 
  402ecd:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  402ed4:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  402edb:	00 
  402edc:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  402ee3:	00 
  402ee4:	4c 89 84 24 c0 00 00 	mov    %r8,0xc0(%rsp)
  402eeb:	00 
  402eec:	4c 89 8c 24 b8 00 00 	mov    %r9,0xb8(%rsp)
  402ef3:	00 
  402ef4:	0f 57 c0             	xorps  %xmm0,%xmm0
  402ef7:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  402efe:	00 
  402eff:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  402f06:	00 
  402f07:	48 89 b4 24 90 00 00 	mov    %rsi,0x90(%rsp)
  402f0e:	00 
  402f0f:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  402f16:	00 
  402f17:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  402f1e:	00 
  402f1f:	48 89 e6             	mov    %rsp,%rsi
  402f22:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  402f26:	4c 8d 9c 24 80 00 00 	lea    0x80(%rsp),%r11
  402f2d:	00 
  402f2e:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  402f32:	4c 89 16             	mov    %r10,(%rsi)
  402f35:	0f b6 f0             	movzbl %al,%esi
  402f38:	e8 43 17 00 00       	call   404680 <runtime::arena_allocator_proc>
  402f3d:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  402f42:	40 88 c7             	mov    %al,%dil
  402f45:	40 88 f8             	mov    %dil,%al
  402f48:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  402f4f:	00 
  402f50:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  402f57:	00 
  402f58:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  402f5f:	00 
  402f60:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  402f67:	00 
  402f68:	40 88 bc 24 9f 00 00 	mov    %dil,0x9f(%rsp)
  402f6f:	00 
  402f70:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  402f74:	48 89 11             	mov    %rdx,(%rcx)
  402f77:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  402f7e:	c3                   	ret
  402f7f:	90                   	nop

0000000000402f80 <runtime::multi_pointer_slice_handle_error>:
  402f80:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  402f87:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  402f8c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402f91:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402f95:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402f99:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  402f9e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402fa3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402fa8:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  402fad:	8b 44 24 18          	mov    0x18(%rsp),%eax
  402fb1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  402fb5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  402fba:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  402fbf:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  402fc4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  402fcb:	00 
  402fcc:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  402fd0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  402fd4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  402fd9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  402fde:	0f 57 c0             	xorps  %xmm0,%xmm0
  402fe1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402fe6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402feb:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402ff2:	00 00 
  402ff4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402ff9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  402ffe:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  403005:	00 00 
  403007:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40300c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403011:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  403015:	89 44 24 44          	mov    %eax,0x44(%rsp)
  403019:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40301e:	e8 8d 14 00 00       	call   4044b0 <runtime::print_caller_location>
  403023:	bf f9 71 40 00       	mov    $0x4071f9,%edi
  403028:	be 17 00 00 00       	mov    $0x17,%esi
  40302d:	e8 5e 0c 00 00       	call   403c90 <runtime::print_string>
  403032:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  403037:	e8 74 12 00 00       	call   4042b0 <runtime::print_i64>
  40303c:	bf 11 72 40 00       	mov    $0x407211,%edi
  403041:	be 01 00 00 00       	mov    $0x1,%esi
  403046:	e8 45 0c 00 00       	call   403c90 <runtime::print_string>
  40304b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403050:	e8 5b 12 00 00       	call   4042b0 <runtime::print_i64>
  403055:	bf 0a 00 00 00       	mov    $0xa,%edi
  40305a:	e8 61 0e 00 00       	call   403ec0 <runtime::print_byte>
  40305f:	e8 ac e8 ff ff       	call   401910 <runtime::bounds_trap>
  403064:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40306b:	00 00 00 00 00 

0000000000403070 <runtime::memory_block_dealloc>:
  403070:	48 83 ec 38          	sub    $0x38,%rsp
  403074:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403079:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40307e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403083:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403088:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40308d:	48 83 f8 00          	cmp    $0x0,%rax
  403091:	0f 95 c0             	setne  %al
  403094:	24 01                	and    $0x1,%al
  403096:	3c 00                	cmp    $0x0,%al
  403098:	74 35                	je     4030cf <runtime::memory_block_dealloc+0x5f>
  40309a:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  40309f:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4030a4:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4030a9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4030ae:	48 8b 42 08          	mov    0x8(%rdx),%rax
  4030b2:	48 8b 52 10          	mov    0x10(%rdx),%rdx
  4030b6:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4030bb:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4030c0:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4030c5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4030ca:	e8 f1 0f 00 00       	call   4040c0 <runtime::mem_free>
  4030cf:	48 83 c4 38          	add    $0x38,%rsp
  4030d3:	c3                   	ret
  4030d4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4030db:	00 00 00 00 00 

00000000004030e0 <main>:
  4030e0:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  4030e7:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  4030eb:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4030f0:	8b 44 24 14          	mov    0x14(%rsp),%eax
  4030f4:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4030f9:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  403100:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  403107:	00 
  403108:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  40310f:	00 
  403110:	48 89 0c 24          	mov    %rcx,(%rsp)
  403114:	4c 63 c8             	movslq %eax,%r9
  403117:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40311c:	bf 29 72 40 00       	mov    $0x407229,%edi
  403121:	31 c0                	xor    %eax,%eax
  403123:	41 89 c0             	mov    %eax,%r8d
  403126:	be 2c 00 00 00       	mov    $0x2c,%esi
  40312b:	ba 36 00 00 00       	mov    $0x36,%edx
  403130:	b9 11 00 00 00       	mov    $0x11,%ecx
  403135:	e8 c6 03 00 00       	call   403500 <runtime::multi_pointer_slice_expr_error>
  40313a:	48 8b 0c 24          	mov    (%rsp),%rcx
  40313e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403143:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  40314a:	00 
  40314b:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  403152:	00 
  403153:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  40315a:	00 
  40315b:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403162:	00 
  403163:	48 c7 c0 60 a0 40 00 	mov    $0x40a060,%rax
  40316a:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40316e:	48 89 08             	mov    %rcx,(%rax)
  403171:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  403178:	00 
  403179:	31 f6                	xor    %esi,%esi
  40317b:	ba 70 00 00 00       	mov    $0x70,%edx
  403180:	e8 bb de ff ff       	call   401040 <memset@plt>
  403185:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40318c:	00 
  40318d:	e8 fe 24 00 00       	call   405690 <runtime::[core.odin]::__init_context>
  403192:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  403197:	31 f6                	xor    %esi,%esi
  403199:	ba 70 00 00 00       	mov    $0x70,%edx
  40319e:	e8 9d de ff ff       	call   401040 <memset@plt>
  4031a3:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  4031a8:	e8 93 24 00 00       	call   405640 <runtime::default_context>
  4031ad:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  4031b2:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  4031b7:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  4031bc:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  4031c1:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  4031c6:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  4031cb:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  4031d2:	00 
  4031d3:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  4031da:	00 
  4031db:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  4031e2:	00 
  4031e3:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  4031ea:	00 
  4031eb:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  4031f2:	00 
  4031f3:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  4031fa:	00 
  4031fb:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  403202:	00 
  403203:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  40320a:	00 
  40320b:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  403212:	00 
  403213:	e8 68 3c 00 00       	call   406e80 <__$startup_runtime>
  403218:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40321f:	00 
  403220:	e8 6b df ff ff       	call   401190 <journey::main>
  403225:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  40322c:	00 
  40322d:	e8 5e 3c 00 00       	call   406e90 <__$cleanup_runtime>
  403232:	31 c0                	xor    %eax,%eax
  403234:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  40323b:	c3                   	ret
  40323c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403240 <runtime::alloc_from_memory_block>:
  403240:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  403247:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40324c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403251:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403256:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40325b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403260:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403265:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40326a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  403271:	00 
  403272:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  403279:	00 
  40327a:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  403281:	00 
  403282:	0f 57 c0             	xorps  %xmm0,%xmm0
  403285:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40328c:	00 
  40328d:	c6 84 24 9f 00 00 00 	movb   $0x0,0x9f(%rsp)
  403294:	00 
  403295:	48 83 f8 00          	cmp    $0x0,%rax
  403299:	0f 94 c0             	sete   %al
  40329c:	24 01                	and    $0x1,%al
  40329e:	3c 00                	cmp    $0x0,%al
  4032a0:	74 3e                	je     4032e0 <runtime::alloc_from_memory_block+0xa0>
  4032a2:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4032a7:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  4032ae:	00 00 00 00 00 
  4032b3:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  4032ba:	00 00 00 00 00 
  4032bf:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  4032c6:	01 
  4032c7:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4032ce:	00 
  4032cf:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4032d6:	b0 01                	mov    $0x1,%al
  4032d8:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  4032df:	c3                   	ret
  4032e0:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4032e5:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4032ea:	e8 b1 38 00 00       	call   406ba0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  4032ef:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4032f4:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4032fb:	00 
  4032fc:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  403303:	00 
  403304:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40330b:	00 00 00 00 00 
  403310:	48 8d 94 24 88 00 00 	lea    0x88(%rsp),%rdx
  403317:	00 
  403318:	e8 53 f4 ff ff       	call   402770 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  40331d:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  403324:	00 
  403325:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40332a:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  40332e:	80 7c 24 6f 00       	cmpb   $0x0,0x6f(%rsp)
  403333:	75 4a                	jne    40337f <runtime::alloc_from_memory_block+0x13f>
  403335:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40333a:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  403341:	01 
  403342:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  403349:	00 
  40334a:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  403351:	00 
  403352:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  403359:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  403360:	00 
  403361:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403368:	00 
  403369:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  403370:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403374:	48 89 11             	mov    %rdx,(%rcx)
  403377:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40337e:	c3                   	ret
  40337f:	eb 00                	jmp    403381 <runtime::alloc_from_memory_block+0x141>
  403381:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  403388:	00 
  403389:	48 8b 78 20          	mov    0x20(%rax),%rdi
  40338d:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  403392:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  403399:	00 00 
  40339b:	48 8d 54 24 60       	lea    0x60(%rsp),%rdx
  4033a0:	e8 cb f3 ff ff       	call   402770 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  4033a5:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  4033aa:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4033af:	88 44 24 47          	mov    %al,0x47(%rsp)
  4033b3:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  4033b8:	74 1a                	je     4033d4 <runtime::alloc_from_memory_block+0x194>
  4033ba:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4033bf:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4033c6:	00 
  4033c7:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  4033cb:	0f 97 c0             	seta   %al
  4033ce:	24 01                	and    $0x1,%al
  4033d0:	3c 00                	cmp    $0x0,%al
  4033d2:	74 4a                	je     40341e <runtime::alloc_from_memory_block+0x1de>
  4033d4:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4033d9:	c6 84 24 9f 00 00 00 	movb   $0x1,0x9f(%rsp)
  4033e0:	01 
  4033e1:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4033e8:	00 
  4033e9:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4033f0:	00 
  4033f1:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  4033f8:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4033ff:	00 
  403400:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403407:	00 
  403408:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  40340f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403413:	48 89 11             	mov    %rdx,(%rcx)
  403416:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40341d:	c3                   	ret
  40341e:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403423:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  40342a:	00 
  40342b:	48 8b 41 18          	mov    0x18(%rcx),%rax
  40342f:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  403433:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40343a:	00 
  40343b:	48 01 d1             	add    %rdx,%rcx
  40343e:	48 01 c8             	add    %rcx,%rax
  403441:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403446:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40344b:	48 89 04 24          	mov    %rax,(%rsp)
  40344f:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  403454:	31 c0                	xor    %eax,%eax
  403456:	41 89 c0             	mov    %eax,%r8d
  403459:	be 3e 00 00 00       	mov    $0x3e,%esi
  40345e:	ba 55 00 00 00       	mov    $0x55,%edx
  403463:	b9 31 00 00 00       	mov    $0x31,%ecx
  403468:	e8 93 00 00 00       	call   403500 <runtime::multi_pointer_slice_expr_error>
  40346d:	48 8b 14 24          	mov    (%rsp),%rdx
  403471:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403476:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40347b:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  403480:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403485:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40348a:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40348f:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  403496:	00 
  403497:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40349e:	00 
  40349f:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4034a6:	00 
  4034a7:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4034ac:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4034b0:	48 01 f2             	add    %rsi,%rdx
  4034b3:	48 89 50 20          	mov    %rdx,0x20(%rax)
  4034b7:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  4034be:	00 
  4034bf:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4034c6:	00 
  4034c7:	8a 84 24 9f 00 00 00 	mov    0x9f(%rsp),%al
  4034ce:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  4034d5:	00 
  4034d6:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4034dd:	00 
  4034de:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  4034e5:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4034e9:	48 89 11             	mov    %rdx,(%rcx)
  4034ec:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  4034f3:	c3                   	ret
  4034f4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4034fb:	00 00 00 00 00 

0000000000403500 <runtime::multi_pointer_slice_expr_error>:
  403500:	48 83 ec 58          	sub    $0x58,%rsp
  403504:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403509:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40350e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  403512:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  403516:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40351b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  403520:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403525:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40352a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  40352e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  403532:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  403537:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40353c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403541:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  403546:	89 74 24 44          	mov    %esi,0x44(%rsp)
  40354a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  40354e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  403553:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  403558:	48 39 c8             	cmp    %rcx,%rax
  40355b:	0f 9e c0             	setle  %al
  40355e:	24 01                	and    $0x1,%al
  403560:	3c 00                	cmp    $0x0,%al
  403562:	74 05                	je     403569 <runtime::multi_pointer_slice_expr_error+0x69>
  403564:	48 83 c4 58          	add    $0x58,%rsp
  403568:	c3                   	ret
  403569:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  40356e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  403573:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  403577:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40357b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403580:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403585:	e8 f6 f9 ff ff       	call   402f80 <runtime::multi_pointer_slice_handle_error>
  40358a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000403590 <runtime::slice_expr_error_hi>:
  403590:	48 83 ec 58          	sub    $0x58,%rsp
  403594:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  403599:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40359e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4035a2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4035a6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4035ab:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4035b0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4035b5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4035ba:	8b 54 24 18          	mov    0x18(%rsp),%edx
  4035be:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  4035c2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4035c7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4035cc:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4035d1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  4035d6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  4035da:	89 54 24 40          	mov    %edx,0x40(%rsp)
  4035de:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4035e3:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4035e8:	31 c0                	xor    %eax,%eax
  4035ea:	48 39 c8             	cmp    %rcx,%rax
  4035ed:	0f 9e c0             	setle  %al
  4035f0:	24 01                	and    $0x1,%al
  4035f2:	3c 00                	cmp    $0x0,%al
  4035f4:	74 1b                	je     403611 <runtime::slice_expr_error_hi+0x81>
  4035f6:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4035fb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403600:	48 39 c8             	cmp    %rcx,%rax
  403603:	0f 9e c0             	setle  %al
  403606:	24 01                	and    $0x1,%al
  403608:	3c 00                	cmp    $0x0,%al
  40360a:	74 05                	je     403611 <runtime::slice_expr_error_hi+0x81>
  40360c:	48 83 c4 58          	add    $0x58,%rsp
  403610:	c3                   	ret
  403611:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  403616:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40361a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  40361e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  403623:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  403628:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40362d:	48 89 e0             	mov    %rsp,%rax
  403630:	4c 89 00             	mov    %r8,(%rax)
  403633:	31 c0                	xor    %eax,%eax
  403635:	41 89 c0             	mov    %eax,%r8d
  403638:	e8 f3 f6 ff ff       	call   402d30 <runtime::slice_handle_error>
  40363d:	0f 1f 00             	nopl   (%rax)

0000000000403640 <runtime::mem_copy_non_overlapping>:
  403640:	48 83 ec 38          	sub    $0x38,%rsp
  403644:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403649:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40364e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  403653:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403658:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40365d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403662:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403667:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40366c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  403671:	48 83 f8 00          	cmp    $0x0,%rax
  403675:	0f 95 c0             	setne  %al
  403678:	24 01                	and    $0x1,%al
  40367a:	3c 00                	cmp    $0x0,%al
  40367c:	74 3c                	je     4036ba <runtime::mem_copy_non_overlapping+0x7a>
  40367e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  403683:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  403688:	48 39 c8             	cmp    %rcx,%rax
  40368b:	0f 95 c0             	setne  %al
  40368e:	24 01                	and    $0x1,%al
  403690:	3c 00                	cmp    $0x0,%al
  403692:	74 26                	je     4036ba <runtime::mem_copy_non_overlapping+0x7a>
  403694:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403699:	48 83 f8 00          	cmp    $0x0,%rax
  40369d:	0f 9f c0             	setg   %al
  4036a0:	24 01                	and    $0x1,%al
  4036a2:	3c 00                	cmp    $0x0,%al
  4036a4:	74 14                	je     4036ba <runtime::mem_copy_non_overlapping+0x7a>
  4036a6:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4036ab:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4036b0:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4036b5:	e8 a6 d9 ff ff       	call   401060 <memcpy@plt>
  4036ba:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4036bf:	48 83 c4 38          	add    $0x38,%rsp
  4036c3:	c3                   	ret
  4036c4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4036cb:	00 00 00 00 00 

00000000004036d0 <runtime::slice_expr_error_lo_hi>:
  4036d0:	48 83 ec 68          	sub    $0x68,%rsp
  4036d4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4036d9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4036de:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  4036e2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  4036e6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4036eb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4036f0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4036f5:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4036fa:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4036ff:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403704:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403709:	8b 74 24 18          	mov    0x18(%rsp),%esi
  40370d:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  403711:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  403716:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  40371b:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  403720:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  403725:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  403729:	89 74 24 50          	mov    %esi,0x50(%rsp)
  40372d:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  403732:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  403737:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40373c:	31 c0                	xor    %eax,%eax
  40373e:	48 39 c8             	cmp    %rcx,%rax
  403741:	0f 9e c0             	setle  %al
  403744:	24 01                	and    $0x1,%al
  403746:	3c 00                	cmp    $0x0,%al
  403748:	74 47                	je     403791 <runtime::slice_expr_error_lo_hi+0xc1>
  40374a:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40374f:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403754:	48 39 c8             	cmp    %rcx,%rax
  403757:	0f 9e c0             	setle  %al
  40375a:	24 01                	and    $0x1,%al
  40375c:	3c 00                	cmp    $0x0,%al
  40375e:	74 31                	je     403791 <runtime::slice_expr_error_lo_hi+0xc1>
  403760:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403765:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40376a:	48 39 c8             	cmp    %rcx,%rax
  40376d:	0f 9e c0             	setle  %al
  403770:	24 01                	and    $0x1,%al
  403772:	3c 00                	cmp    $0x0,%al
  403774:	74 1b                	je     403791 <runtime::slice_expr_error_lo_hi+0xc1>
  403776:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40377b:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403780:	48 39 c8             	cmp    %rcx,%rax
  403783:	0f 9e c0             	setle  %al
  403786:	24 01                	and    $0x1,%al
  403788:	3c 00                	cmp    $0x0,%al
  40378a:	74 05                	je     403791 <runtime::slice_expr_error_lo_hi+0xc1>
  40378c:	48 83 c4 68          	add    $0x68,%rsp
  403790:	c3                   	ret
  403791:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  403796:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40379b:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  40379f:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4037a3:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4037a8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4037ad:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  4037b2:	48 89 e0             	mov    %rsp,%rax
  4037b5:	4c 89 10             	mov    %r10,(%rax)
  4037b8:	e8 73 f5 ff ff       	call   402d30 <runtime::slice_handle_error>
  4037bd:	0f 1f 00             	nopl   (%rax)

00000000004037c0 <runtime::memset>:
  4037c0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4037c5:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  4037c9:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  4037ce:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4037d3:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4037d8:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  4037dc:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4037e1:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  4037e5:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4037ea:	48 83 f8 00          	cmp    $0x0,%rax
  4037ee:	0f 95 c0             	setne  %al
  4037f1:	24 01                	and    $0x1,%al
  4037f3:	3c 00                	cmp    $0x0,%al
  4037f5:	74 63                	je     40385a <runtime::memset+0x9a>
  4037f7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4037fc:	48 83 f8 00          	cmp    $0x0,%rax
  403800:	0f 95 c0             	setne  %al
  403803:	24 01                	and    $0x1,%al
  403805:	3c 00                	cmp    $0x0,%al
  403807:	74 51                	je     40385a <runtime::memset+0x9a>
  403809:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40380e:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  403812:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  403816:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40381b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  403822:	00 00 
  403824:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  403829:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  40382e:	0f 9c c0             	setl   %al
  403831:	24 01                	and    $0x1,%al
  403833:	3c 00                	cmp    $0x0,%al
  403835:	74 21                	je     403858 <runtime::memset+0x98>
  403837:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40383c:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  403841:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  403845:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  403848:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40384d:	48 83 c0 01          	add    $0x1,%rax
  403851:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  403856:	eb cc                	jmp    403824 <runtime::memset+0x64>
  403858:	eb 00                	jmp    40385a <runtime::memset+0x9a>
  40385a:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40385f:	c3                   	ret

0000000000403860 <runtime::arena_alloc>:
  403860:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  403867:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40386c:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403871:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  403876:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40387b:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  403880:	4c 89 4c 24 50       	mov    %r9,0x50(%rsp)
  403885:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40388a:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  40388f:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403894:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403899:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40389e:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  4038a5:	00 
  4038a6:	48 89 94 24 28 01 00 	mov    %rdx,0x128(%rsp)
  4038ad:	00 
  4038ae:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4038b5:	00 
  4038b6:	0f 57 c0             	xorps  %xmm0,%xmm0
  4038b9:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  4038c0:	00 
  4038c1:	c6 84 24 0f 01 00 00 	movb   $0x0,0x10f(%rsp)
  4038c8:	00 
  4038c9:	48 89 c2             	mov    %rax,%rdx
  4038cc:	48 83 ea 01          	sub    $0x1,%rdx
  4038d0:	48 21 d0             	and    %rdx,%rax
  4038d3:	48 83 f8 00          	cmp    $0x0,%rax
  4038d7:	0f 94 c0             	sete   %al
  4038da:	24 01                	and    $0x1,%al
  4038dc:	0f b6 f8             	movzbl %al,%edi
  4038df:	be 56 72 40 00       	mov    $0x407256,%esi
  4038e4:	ba 1a 00 00 00       	mov    $0x1a,%edx
  4038e9:	e8 a2 2a 00 00       	call   406390 <runtime::assert>
  4038ee:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4038f3:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  4038fa:	00 
  4038fb:	48 83 bc 24 00 01 00 	cmpq   $0x0,0x100(%rsp)
  403902:	00 00 
  403904:	0f 94 c0             	sete   %al
  403907:	24 01                	and    $0x1,%al
  403909:	3c 00                	cmp    $0x0,%al
  40390b:	74 42                	je     40394f <runtime::arena_alloc+0xef>
  40390d:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403912:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403919:	00 
  40391a:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403921:	00 
  403922:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403929:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403930:	00 
  403931:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403938:	00 
  403939:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403940:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403944:	48 89 11             	mov    %rdx,(%rcx)
  403947:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40394e:	c3                   	ret
  40394f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403956:	00 
  403957:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  40395c:	0f 94 c0             	sete   %al
  40395f:	24 01                	and    $0x1,%al
  403961:	3c 00                	cmp    $0x0,%al
  403963:	74 09                	je     40396e <runtime::arena_alloc+0x10e>
  403965:	31 c0                	xor    %eax,%eax
  403967:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40396c:	eb 15                	jmp    403983 <runtime::arena_alloc+0x123>
  40396e:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403975:	00 
  403976:	48 8b 40 10          	mov    0x10(%rax),%rax
  40397a:	48 8b 40 20          	mov    0x20(%rax),%rax
  40397e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403983:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403988:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40398d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403992:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  403999:	00 
  40399a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4039a1:	00 
  4039a2:	48 8b 78 10          	mov    0x10(%rax),%rdi
  4039a6:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  4039ad:	00 
  4039ae:	0f 57 c0             	xorps  %xmm0,%xmm0
  4039b1:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4039b8:	00 
  4039b9:	48 8d 8c 24 e0 00 00 	lea    0xe0(%rsp),%rcx
  4039c0:	00 
  4039c1:	e8 7a f8 ff ff       	call   403240 <runtime::alloc_from_memory_block>
  4039c6:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  4039cd:	00 
  4039ce:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  4039d5:	00 
  4039d6:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  4039dd:	00 
  4039de:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  4039e5:	00 
  4039e6:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  4039ed:	80 bc 24 0f 01 00 00 	cmpb   $0x1,0x10f(%rsp)
  4039f4:	01 
  4039f5:	0f 94 c0             	sete   %al
  4039f8:	24 01                	and    $0x1,%al
  4039fa:	3c 00                	cmp    $0x0,%al
  4039fc:	0f 84 19 02 00 00    	je     403c1b <runtime::arena_alloc+0x3bb>
  403a02:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403a09:	00 
  403a0a:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  403a0f:	0f 94 c0             	sete   %al
  403a12:	24 01                	and    $0x1,%al
  403a14:	3c 00                	cmp    $0x0,%al
  403a16:	74 10                	je     403a28 <runtime::arena_alloc+0x1c8>
  403a18:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403a1f:	00 
  403a20:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  403a27:	00 
  403a28:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403a2d:	48 8b bc 24 00 01 00 	mov    0x100(%rsp),%rdi
  403a34:	00 
  403a35:	e8 f6 31 00 00       	call   406c30 <runtime::arena_alloc.align_forward_uint-0>
  403a3a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  403a41:	00 
  403a42:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  403a49:	00 
  403a4a:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403a51:	00 
  403a52:	48 8b 40 28          	mov    0x28(%rax),%rax
  403a56:	48 39 c1             	cmp    %rax,%rcx
  403a59:	48 0f 47 c1          	cmova  %rcx,%rax
  403a5d:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  403a64:	00 
  403a65:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403a6c:	00 
  403a6d:	48 83 38 00          	cmpq   $0x0,(%rax)
  403a71:	0f 94 c0             	sete   %al
  403a74:	24 01                	and    $0x1,%al
  403a76:	3c 00                	cmp    $0x0,%al
  403a78:	74 46                	je     403ac0 <runtime::arena_alloc+0x260>
  403a7a:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  403a7f:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403a86:	00 
  403a87:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  403a8c:	e8 8f de ff ff       	call   401920 <runtime::heap_allocator>
  403a91:	48 89 c1             	mov    %rax,%rcx
  403a94:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403a99:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  403aa0:	00 
  403aa1:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  403aa8:	00 
  403aa9:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403ab0:	00 
  403ab1:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  403ab8:	00 
  403ab9:	48 89 50 08          	mov    %rdx,0x8(%rax)
  403abd:	48 89 08             	mov    %rcx,(%rax)
  403ac0:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  403ac5:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  403aca:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  403acf:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403ad6:	00 
  403ad7:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  403ade:	00 
  403adf:	48 8b 38             	mov    (%rax),%rdi
  403ae2:	48 8b 70 08          	mov    0x8(%rax),%rsi
  403ae6:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  403aed:	00 00 00 00 00 
  403af2:	48 89 e0             	mov    %rsp,%rax
  403af5:	4c 89 08             	mov    %r9,(%rax)
  403af8:	4c 8d 8c 24 98 00 00 	lea    0x98(%rsp),%r9
  403aff:	00 
  403b00:	e8 eb ec ff ff       	call   4027f0 <runtime::memory_block_alloc>
  403b05:	88 44 24 0f          	mov    %al,0xf(%rsp)
  403b09:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  403b10:	00 
  403b11:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  403b16:	3c 00                	cmp    $0x0,%al
  403b18:	74 4d                	je     403b67 <runtime::arena_alloc+0x307>
  403b1a:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403b1f:	8a 44 24 0f          	mov    0xf(%rsp),%al
  403b23:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403b2a:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403b31:	00 
  403b32:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403b39:	00 
  403b3a:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403b41:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403b48:	00 
  403b49:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403b50:	00 
  403b51:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403b58:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403b5c:	48 89 11             	mov    %rdx,(%rcx)
  403b5f:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403b66:	c3                   	ret
  403b67:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403b6c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403b71:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403b76:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403b7d:	00 
  403b7e:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403b85:	00 
  403b86:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  403b8d:	00 
  403b8e:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  403b92:	48 89 08             	mov    %rcx,(%rax)
  403b95:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403b9c:	00 
  403b9d:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403ba4:	00 
  403ba5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  403ba9:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403bb0:	00 
  403bb1:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  403bb8:	00 
  403bb9:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  403bbd:	48 8b 48 20          	mov    0x20(%rax),%rcx
  403bc1:	48 01 f1             	add    %rsi,%rcx
  403bc4:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403bc8:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  403bcf:	00 00 00 00 00 
  403bd4:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403bdb:	00 
  403bdc:	48 8b 78 10          	mov    0x10(%rax),%rdi
  403be0:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  403be7:	00 
  403be8:	0f 57 c0             	xorps  %xmm0,%xmm0
  403beb:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  403bf0:	48 8d 4c 24 70       	lea    0x70(%rsp),%rcx
  403bf5:	e8 46 f6 ff ff       	call   403240 <runtime::alloc_from_memory_block>
  403bfa:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  403bff:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  403c04:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  403c0b:	00 
  403c0c:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  403c13:	00 
  403c14:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403c1b:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403c20:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  403c27:	00 
  403c28:	48 8b 70 10          	mov    0x10(%rax),%rsi
  403c2c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  403c30:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  403c34:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  403c3b:	00 
  403c3c:	48 29 fe             	sub    %rdi,%rsi
  403c3f:	48 01 f2             	add    %rsi,%rdx
  403c42:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403c46:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  403c4d:	00 
  403c4e:	48 8b b4 24 18 01 00 	mov    0x118(%rsp),%rsi
  403c55:	00 
  403c56:	8a 84 24 0f 01 00 00 	mov    0x10f(%rsp),%al
  403c5d:	48 89 b4 24 18 01 00 	mov    %rsi,0x118(%rsp)
  403c64:	00 
  403c65:	48 89 94 24 10 01 00 	mov    %rdx,0x110(%rsp)
  403c6c:	00 
  403c6d:	88 84 24 0f 01 00 00 	mov    %al,0x10f(%rsp)
  403c74:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403c78:	48 89 11             	mov    %rdx,(%rcx)
  403c7b:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  403c82:	c3                   	ret
  403c83:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  403c8a:	84 00 00 00 00 00 

0000000000403c90 <runtime::print_string>:
  403c90:	48 83 ec 58          	sub    $0x58,%rsp
  403c94:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  403c99:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403c9e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  403ca3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403ca8:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403cad:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  403cb2:	48 c7 44 24 40 00 00 	movq   $0x0,0x40(%rsp)
  403cb9:	00 00 
  403cbb:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  403cc0:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403cc5:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403cca:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  403ccf:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  403cd6:	00 00 
  403cd8:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  403cdd:	e8 0e e6 ff ff       	call   4022f0 <runtime::stderr_write>
  403ce2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403ce7:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403cec:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  403cf1:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403cf6:	48 83 c4 58          	add    $0x58,%rsp
  403cfa:	c3                   	ret
  403cfb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000403d00 <runtime::mem_alloc>:
  403d00:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  403d07:	4c 89 4c 24 20       	mov    %r9,0x20(%rsp)
  403d0c:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  403d11:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403d16:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403d1b:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403d20:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  403d25:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403d2c:	00 
  403d2d:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403d32:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403d37:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  403d3c:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  403d41:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403d46:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  403d4d:	00 
  403d4e:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  403d55:	00 
  403d56:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  403d5d:	00 
  403d5e:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403d65:	00 
  403d66:	e8 65 e9 ff ff       	call   4026d0 <runtime::is_power_of_two_int>
  403d6b:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403d70:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  403d75:	0f b6 f8             	movzbl %al,%edi
  403d78:	be 71 72 40 00       	mov    $0x407271,%esi
  403d7d:	ba 20 00 00 00       	mov    $0x20,%edx
  403d82:	e8 09 26 00 00       	call   406390 <runtime::assert>
  403d87:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  403d8c:	48 83 f8 00          	cmp    $0x0,%rax
  403d90:	0f 94 c0             	sete   %al
  403d93:	24 01                	and    $0x1,%al
  403d95:	3c 00                	cmp    $0x0,%al
  403d97:	75 12                	jne    403dab <runtime::mem_alloc+0xab>
  403d99:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  403da0:	00 00 
  403da2:	0f 94 c0             	sete   %al
  403da5:	24 01                	and    $0x1,%al
  403da7:	3c 00                	cmp    $0x0,%al
  403da9:	74 1e                	je     403dc9 <runtime::mem_alloc+0xc9>
  403dab:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403db0:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  403db7:	00 
  403db8:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  403dbf:	31 c0                	xor    %eax,%eax
  403dc1:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403dc8:	c3                   	ret
  403dc9:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403dce:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  403dd3:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  403dd8:	4c 8b 4c 24 50       	mov    0x50(%rsp),%r9
  403ddd:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  403de4:	00 
  403de5:	48 8b bc 24 88 00 00 	mov    0x88(%rsp),%rdi
  403dec:	00 
  403ded:	0f 57 c0             	xorps  %xmm0,%xmm0
  403df0:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  403df5:	48 89 e6             	mov    %rsp,%rsi
  403df8:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  403dfc:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  403e01:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  403e05:	4c 89 06             	mov    %r8,(%rsi)
  403e08:	31 f6                	xor    %esi,%esi
  403e0a:	41 89 f1             	mov    %esi,%r9d
  403e0d:	4d 89 c8             	mov    %r9,%r8
  403e10:	ff d0                	call   *%rax
  403e12:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403e17:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  403e1c:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  403e21:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  403e25:	48 89 11             	mov    %rdx,(%rcx)
  403e28:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  403e2f:	c3                   	ret

0000000000403e30 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  403e30:	48 83 ec 48          	sub    $0x48,%rsp
  403e34:	48 89 0c 24          	mov    %rcx,(%rsp)
  403e38:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  403e3d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403e42:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  403e47:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  403e4c:	48 8b 04 24          	mov    (%rsp),%rax
  403e50:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  403e55:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403e5a:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  403e5f:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403e64:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  403e69:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  403e6e:	48 39 c1             	cmp    %rax,%rcx
  403e71:	48 0f 4c c1          	cmovl  %rcx,%rax
  403e75:	31 c9                	xor    %ecx,%ecx
  403e77:	48 39 c1             	cmp    %rax,%rcx
  403e7a:	48 0f 4f c1          	cmovg  %rcx,%rax
  403e7e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403e83:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  403e89:	0f 9f c0             	setg   %al
  403e8c:	24 01                	and    $0x1,%al
  403e8e:	3c 00                	cmp    $0x0,%al
  403e90:	74 18                	je     403eaa <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)+0x7a>
  403e92:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  403e97:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403e9c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  403ea1:	48 c1 e2 00          	shl    $0x0,%rdx
  403ea5:	e8 e6 d1 ff ff       	call   401090 <memmove@plt>
  403eaa:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403eaf:	48 83 c4 48          	add    $0x48,%rsp
  403eb3:	c3                   	ret
  403eb4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  403ebb:	00 00 00 00 00 

0000000000403ec0 <runtime::print_byte>:
  403ec0:	48 83 ec 68          	sub    $0x68,%rsp
  403ec4:	40 88 f8             	mov    %dil,%al
  403ec7:	88 44 24 07          	mov    %al,0x7(%rsp)
  403ecb:	8a 54 24 07          	mov    0x7(%rsp),%dl
  403ecf:	88 54 24 67          	mov    %dl,0x67(%rsp)
  403ed3:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  403eda:	00 00 
  403edc:	0f 57 c0             	xorps  %xmm0,%xmm0
  403edf:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  403ee4:	c6 44 24 30 00       	movb   $0x0,0x30(%rsp)
  403ee9:	48 8d 44 24 30       	lea    0x30(%rsp),%rax
  403eee:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  403ef3:	48 c7 44 24 28 01 00 	movq   $0x1,0x28(%rsp)
  403efa:	00 00 
  403efc:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  403f01:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403f06:	88 11                	mov    %dl,(%rcx)
  403f08:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403f0d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403f12:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  403f17:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403f1c:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  403f23:	00 00 
  403f25:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  403f2a:	e8 c1 e3 ff ff       	call   4022f0 <runtime::stderr_write>
  403f2f:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403f34:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403f39:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  403f3e:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403f43:	48 83 c4 68          	add    $0x68,%rsp
  403f47:	c3                   	ret
  403f48:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  403f4f:	00 

0000000000403f50 <runtime::matrix_bounds_check_error>:
  403f50:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  403f57:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  403f5c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403f61:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  403f65:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  403f69:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  403f6e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  403f73:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  403f7a:	00 
  403f7b:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  403f80:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403f87:	00 
  403f88:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403f8d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403f92:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  403f97:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  403f9c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403fa1:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  403fa5:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  403faa:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  403faf:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  403fb4:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  403fb9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  403fc0:	00 
  403fc1:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  403fc6:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  403fca:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  403fcf:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  403fd4:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  403fd9:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  403fde:	48 39 c8             	cmp    %rcx,%rax
  403fe1:	0f 92 c0             	setb   %al
  403fe4:	24 01                	and    $0x1,%al
  403fe6:	3c 00                	cmp    $0x0,%al
  403fe8:	74 1e                	je     404008 <runtime::matrix_bounds_check_error+0xb8>
  403fea:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403fef:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  403ff4:	48 39 c8             	cmp    %rcx,%rax
  403ff7:	0f 92 c0             	setb   %al
  403ffa:	24 01                	and    $0x1,%al
  403ffc:	3c 00                	cmp    $0x0,%al
  403ffe:	74 08                	je     404008 <runtime::matrix_bounds_check_error+0xb8>
  404000:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  404007:	c3                   	ret
  404008:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40400d:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  404012:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  404016:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  40401a:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40401f:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  404024:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  404029:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  40402e:	48 89 e0             	mov    %rsp,%rax
  404031:	4c 89 58 08          	mov    %r11,0x8(%rax)
  404035:	4c 89 10             	mov    %r10,(%rax)
  404038:	e8 53 2c 00 00       	call   406c90 <runtime::matrix_bounds_check_error.handle_error-0>
  40403d:	0f 1f 00             	nopl   (%rax)

0000000000404040 <runtime::heap_alloc>:
  404040:	48 83 ec 18          	sub    $0x18,%rsp
  404044:	48 89 3c 24          	mov    %rdi,(%rsp)
  404048:	40 88 f0             	mov    %sil,%al
  40404b:	88 44 24 0e          	mov    %al,0xe(%rsp)
  40404f:	8a 44 24 0e          	mov    0xe(%rsp),%al
  404053:	48 8b 3c 24          	mov    (%rsp),%rdi
  404057:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40405c:	88 44 24 0f          	mov    %al,0xf(%rsp)
  404060:	0f b6 f0             	movzbl %al,%esi
  404063:	e8 a8 e6 ff ff       	call   402710 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  404068:	48 83 c4 18          	add    $0x18,%rsp
  40406c:	c3                   	ret
  40406d:	0f 1f 00             	nopl   (%rax)

0000000000404070 <runtime::heap_resize>:
  404070:	48 83 ec 28          	sub    $0x28,%rsp
  404074:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404079:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40407e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404083:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  404088:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40408d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  404092:	e8 29 e7 ff ff       	call   4027c0 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  404097:	48 83 c4 28          	add    $0x28,%rsp
  40409b:	c3                   	ret
  40409c:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004040a0 <runtime::heap_free>:
  4040a0:	48 83 ec 18          	sub    $0x18,%rsp
  4040a4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4040a9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4040ae:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4040b3:	e8 38 ea ff ff       	call   402af0 <runtime::[heap_allocator_unix.odin]::_heap_free>
  4040b8:	48 83 c4 18          	add    $0x18,%rsp
  4040bc:	c3                   	ret
  4040bd:	0f 1f 00             	nopl   (%rax)

00000000004040c0 <runtime::mem_free>:
  4040c0:	53                   	push   %rbx
  4040c1:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  4040c8:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  4040cd:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4040d2:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4040d7:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4040dc:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4040e1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4040e6:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4040eb:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4040f0:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  4040f7:	00 
  4040f8:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  4040fd:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  404102:	48 83 f8 00          	cmp    $0x0,%rax
  404106:	0f 94 c0             	sete   %al
  404109:	24 01                	and    $0x1,%al
  40410b:	3c 00                	cmp    $0x0,%al
  40410d:	75 0f                	jne    40411e <runtime::mem_free+0x5e>
  40410f:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  404115:	0f 94 c0             	sete   %al
  404118:	24 01                	and    $0x1,%al
  40411a:	3c 00                	cmp    $0x0,%al
  40411c:	74 0b                	je     404129 <runtime::mem_free+0x69>
  40411e:	31 c0                	xor    %eax,%eax
  404120:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  404127:	5b                   	pop    %rbx
  404128:	c3                   	ret
  404129:	4c 8b 54 24 18       	mov    0x18(%rsp),%r10
  40412e:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
  404133:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  404138:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40413d:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  404142:	0f 57 c0             	xorps  %xmm0,%xmm0
  404145:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  40414a:	be 01 00 00 00       	mov    $0x1,%esi
  40414f:	31 c9                	xor    %ecx,%ecx
  404151:	41 89 c9             	mov    %ecx,%r9d
  404154:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  404159:	4c 89 ca             	mov    %r9,%rdx
  40415c:	4c 89 c9             	mov    %r9,%rcx
  40415f:	48 89 1c 24          	mov    %rbx,(%rsp)
  404163:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  404168:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  40416d:	ff d0                	call   *%rax
  40416f:	88 44 24 47          	mov    %al,0x47(%rsp)
  404173:	8a 44 24 47          	mov    0x47(%rsp),%al
  404177:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40417e:	5b                   	pop    %rbx
  40417f:	c3                   	ret

0000000000404180 <runtime::print_u64>:
  404180:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  404187:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40418c:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404191:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404198:	00 
  404199:	48 8d 7c 24 5f       	lea    0x5f(%rsp),%rdi
  40419e:	31 f6                	xor    %esi,%esi
  4041a0:	ba 81 00 00 00       	mov    $0x81,%edx
  4041a5:	e8 96 ce ff ff       	call   401040 <memset@plt>
  4041aa:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4041af:	48 c7 44 24 50 81 00 	movq   $0x81,0x50(%rsp)
  4041b6:	00 00 
  4041b8:	48 c7 44 24 48 0a 00 	movq   $0xa,0x48(%rsp)
  4041bf:	00 00 
  4041c1:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4041c6:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4041cb:	48 3b 44 24 48       	cmp    0x48(%rsp),%rax
  4041d0:	0f 93 c0             	setae  %al
  4041d3:	24 01                	and    $0x1,%al
  4041d5:	3c 00                	cmp    $0x0,%al
  4041d7:	74 50                	je     404229 <runtime::print_u64+0xa9>
  4041d9:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4041de:	48 83 e8 01          	sub    $0x1,%rax
  4041e2:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4041e7:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4041ec:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4041f1:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  4041f8:	48 8b 08             	mov    (%rax),%rcx
  4041fb:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404200:	31 d2                	xor    %edx,%edx
  404202:	48 f7 74 24 48       	divq   0x48(%rsp)
  404207:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40420c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  40420f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  404213:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  404218:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40421d:	31 d2                	xor    %edx,%edx
  40421f:	48 f7 f1             	div    %rcx
  404222:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404227:	eb 9d                	jmp    4041c6 <runtime::print_u64+0x46>
  404229:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40422e:	48 ff c8             	dec    %rax
  404231:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404236:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40423b:	48 89 04 24          	mov    %rax,(%rsp)
  40423f:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  404246:	48 8b 08             	mov    (%rax),%rcx
  404249:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40424e:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404253:	31 d2                	xor    %edx,%edx
  404255:	48 f7 f6             	div    %rsi
  404258:	48 8b 04 24          	mov    (%rsp),%rax
  40425c:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  40425f:	88 4c 04 5f          	mov    %cl,0x5f(%rsp,%rax,1)
  404263:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  404268:	48 8d 4c 14 5f       	lea    0x5f(%rsp,%rdx,1),%rcx
  40426d:	b8 81 00 00 00       	mov    $0x81,%eax
  404272:	48 29 d0             	sub    %rdx,%rax
  404275:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40427a:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40427f:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  404284:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  404289:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  404290:	00 00 
  404292:	48 8d 54 24 28       	lea    0x28(%rsp),%rdx
  404297:	e8 54 e0 ff ff       	call   4022f0 <runtime::stderr_write>
  40429c:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4042a3:	c3                   	ret
  4042a4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  4042ab:	00 00 00 00 00 

00000000004042b0 <runtime::print_i64>:
  4042b0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4042b7:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4042bc:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4042c1:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4042c8:	00 
  4042c9:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4042d0:	00 
  4042d1:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  4042d8:	00 00 
  4042da:	0f 9c c0             	setl   %al
  4042dd:	24 01                	and    $0x1,%al
  4042df:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  4042e6:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4042ed:	00 
  4042ee:	31 c9                	xor    %ecx,%ecx
  4042f0:	48 29 c1             	sub    %rax,%rcx
  4042f3:	48 83 f8 00          	cmp    $0x0,%rax
  4042f7:	48 0f 4c c1          	cmovl  %rcx,%rax
  4042fb:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404302:	00 
  404303:	48 8d 7c 24 56       	lea    0x56(%rsp),%rdi
  404308:	31 f6                	xor    %esi,%esi
  40430a:	ba 81 00 00 00       	mov    $0x81,%edx
  40430f:	e8 2c cd ff ff       	call   401040 <memset@plt>
  404314:	48 c7 44 24 48 81 00 	movq   $0x81,0x48(%rsp)
  40431b:	00 00 
  40431d:	48 83 bc 24 d8 00 00 	cmpq   $0xa,0xd8(%rsp)
  404324:	00 0a 
  404326:	0f 9d c0             	setge  %al
  404329:	24 01                	and    $0x1,%al
  40432b:	3c 00                	cmp    $0x0,%al
  40432d:	74 5c                	je     40438b <runtime::print_i64+0xdb>
  40432f:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404334:	48 83 e8 01          	sub    $0x1,%rax
  404338:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40433d:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404342:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  404347:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  40434e:	48 8b 08             	mov    (%rax),%rcx
  404351:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404358:	00 
  404359:	be 0a 00 00 00       	mov    $0xa,%esi
  40435e:	48 99                	cqto
  404360:	48 f7 fe             	idiv   %rsi
  404363:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404368:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  40436b:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  40436f:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404376:	00 
  404377:	b9 0a 00 00 00       	mov    $0xa,%ecx
  40437c:	48 99                	cqto
  40437e:	48 f7 f9             	idiv   %rcx
  404381:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404388:	00 
  404389:	eb 92                	jmp    40431d <runtime::print_i64+0x6d>
  40438b:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404390:	48 83 e8 01          	sub    $0x1,%rax
  404394:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404399:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40439e:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4043a3:	48 c7 c0 48 a0 40 00 	mov    $0x40a048,%rax
  4043aa:	48 8b 08             	mov    (%rax),%rcx
  4043ad:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4043b4:	00 
  4043b5:	be 0a 00 00 00       	mov    $0xa,%esi
  4043ba:	48 99                	cqto
  4043bc:	48 f7 fe             	idiv   %rsi
  4043bf:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4043c4:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  4043c7:	88 4c 04 56          	mov    %cl,0x56(%rsp,%rax,1)
  4043cb:	80 bc 24 d7 00 00 00 	cmpb   $0x0,0xd7(%rsp)
  4043d2:	00 
  4043d3:	74 18                	je     4043ed <runtime::print_i64+0x13d>
  4043d5:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4043da:	48 83 e8 01          	sub    $0x1,%rax
  4043de:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4043e3:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4043e8:	c6 44 04 56 2d       	movb   $0x2d,0x56(%rsp,%rax,1)
  4043ed:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4043f2:	48 8d 4c 14 56       	lea    0x56(%rsp,%rdx,1),%rcx
  4043f7:	b8 81 00 00 00       	mov    $0x81,%eax
  4043fc:	48 29 d0             	sub    %rdx,%rax
  4043ff:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  404404:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404409:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40440e:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  404413:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40441a:	00 00 
  40441c:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  404421:	e8 ca de ff ff       	call   4022f0 <runtime::stderr_write>
  404426:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40442d:	c3                   	ret
  40442e:	66 90                	xchg   %ax,%ax

0000000000404430 <runtime::arena_free_last_memory_block>:
  404430:	48 83 ec 28          	sub    $0x28,%rsp
  404434:	48 89 3c 24          	mov    %rdi,(%rsp)
  404438:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40443d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404442:	48 8b 04 24          	mov    (%rsp),%rax
  404446:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40444b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404450:	48 8b 40 10          	mov    0x10(%rax),%rax
  404454:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404459:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  40445f:	0f 95 c0             	setne  %al
  404462:	24 01                	and    $0x1,%al
  404464:	3c 00                	cmp    $0x0,%al
  404466:	74 3e                	je     4044a6 <runtime::arena_free_last_memory_block+0x76>
  404468:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40446d:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404472:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404477:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40447c:	48 8b 09             	mov    (%rcx),%rcx
  40447f:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404483:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404488:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40448d:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  404491:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404495:	48 29 f9             	sub    %rdi,%rcx
  404498:	48 89 48 20          	mov    %rcx,0x20(%rax)
  40449c:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4044a1:	e8 ca eb ff ff       	call   403070 <runtime::memory_block_dealloc>
  4044a6:	48 83 c4 28          	add    $0x28,%rsp
  4044aa:	c3                   	ret
  4044ab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004044b0 <runtime::print_caller_location>:
  4044b0:	50                   	push   %rax
  4044b1:	48 89 3c 24          	mov    %rdi,(%rsp)
  4044b5:	eb 00                	jmp    4044b7 <runtime::print_caller_location+0x7>
  4044b7:	48 8b 04 24          	mov    (%rsp),%rax
  4044bb:	48 8b 38             	mov    (%rax),%rdi
  4044be:	48 8b 70 08          	mov    0x8(%rax),%rsi
  4044c2:	e8 c9 f7 ff ff       	call   403c90 <runtime::print_string>
  4044c7:	bf 28 00 00 00       	mov    $0x28,%edi
  4044cc:	e8 ef f9 ff ff       	call   403ec0 <runtime::print_byte>
  4044d1:	48 8b 04 24          	mov    (%rsp),%rax
  4044d5:	48 63 78 10          	movslq 0x10(%rax),%rdi
  4044d9:	e8 a2 fc ff ff       	call   404180 <runtime::print_u64>
  4044de:	48 8b 04 24          	mov    (%rsp),%rax
  4044e2:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  4044e6:	0f 95 c0             	setne  %al
  4044e9:	24 01                	and    $0x1,%al
  4044eb:	3c 00                	cmp    $0x0,%al
  4044ed:	74 17                	je     404506 <runtime::print_caller_location+0x56>
  4044ef:	bf 3a 00 00 00       	mov    $0x3a,%edi
  4044f4:	e8 c7 f9 ff ff       	call   403ec0 <runtime::print_byte>
  4044f9:	48 8b 04 24          	mov    (%rsp),%rax
  4044fd:	48 63 78 14          	movslq 0x14(%rax),%rdi
  404501:	e8 7a fc ff ff       	call   404180 <runtime::print_u64>
  404506:	bf 29 00 00 00       	mov    $0x29,%edi
  40450b:	e8 b0 f9 ff ff       	call   403ec0 <runtime::print_byte>
  404510:	58                   	pop    %rax
  404511:	c3                   	ret
  404512:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404519:	1f 84 00 00 00 00 00 

0000000000404520 <runtime::arena_free_all>:
  404520:	48 83 ec 28          	sub    $0x28,%rsp
  404524:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  404529:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40452e:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  404533:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404538:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40453d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404542:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404547:	0f 95 c0             	setne  %al
  40454a:	24 01                	and    $0x1,%al
  40454c:	3c 00                	cmp    $0x0,%al
  40454e:	74 2c                	je     40457c <runtime::arena_free_all+0x5c>
  404550:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404555:	48 8b 40 10          	mov    0x10(%rax),%rax
  404559:	48 83 38 00          	cmpq   $0x0,(%rax)
  40455d:	0f 95 c0             	setne  %al
  404560:	24 01                	and    $0x1,%al
  404562:	3c 00                	cmp    $0x0,%al
  404564:	74 16                	je     40457c <runtime::arena_free_all+0x5c>
  404566:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40456b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  404570:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  404575:	e8 b6 fe ff ff       	call   404430 <runtime::arena_free_last_memory_block>
  40457a:	eb c1                	jmp    40453d <runtime::arena_free_all+0x1d>
  40457c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404581:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404586:	0f 95 c0             	setne  %al
  404589:	24 01                	and    $0x1,%al
  40458b:	3c 00                	cmp    $0x0,%al
  40458d:	74 32                	je     4045c1 <runtime::arena_free_all+0xa1>
  40458f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404594:	48 8b 40 10          	mov    0x10(%rax),%rax
  404598:	48 8b 78 18          	mov    0x18(%rax),%rdi
  40459c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4045a1:	48 8b 40 10          	mov    0x10(%rax),%rax
  4045a5:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4045a9:	31 f6                	xor    %esi,%esi
  4045ab:	e8 90 ca ff ff       	call   401040 <memset@plt>
  4045b0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4045b5:	48 8b 40 10          	mov    0x10(%rax),%rax
  4045b9:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  4045c0:	00 
  4045c1:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4045c6:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  4045cd:	00 
  4045ce:	48 83 c4 28          	add    $0x28,%rsp
  4045d2:	c3                   	ret
  4045d3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4045da:	84 00 00 00 00 00 

00000000004045e0 <runtime::arena_destroy>:
  4045e0:	48 83 ec 28          	sub    $0x28,%rsp
  4045e4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4045e8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4045ed:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4045f2:	48 8b 04 24          	mov    (%rsp),%rax
  4045f6:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4045fb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404600:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  404605:	0f 95 c0             	setne  %al
  404608:	24 01                	and    $0x1,%al
  40460a:	3c 00                	cmp    $0x0,%al
  40460c:	74 4e                	je     40465c <runtime::arena_destroy+0x7c>
  40460e:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  404613:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404618:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40461d:	48 8b 40 10          	mov    0x10(%rax),%rax
  404621:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404626:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40462b:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404630:	48 8b 09             	mov    (%rcx),%rcx
  404633:	48 89 48 10          	mov    %rcx,0x10(%rax)
  404637:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40463c:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404641:	48 8b 79 28          	mov    0x28(%rcx),%rdi
  404645:	48 8b 48 20          	mov    0x20(%rax),%rcx
  404649:	48 29 f9             	sub    %rdi,%rcx
  40464c:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404650:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  404655:	e8 16 ea ff ff       	call   403070 <runtime::memory_block_dealloc>
  40465a:	eb 9f                	jmp    4045fb <runtime::arena_destroy+0x1b>
  40465c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  404661:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  404668:	00 
  404669:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40466e:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  404675:	00 
  404676:	48 83 c4 28          	add    $0x28,%rsp
  40467a:	c3                   	ret
  40467b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000404680 <runtime::arena_allocator_proc>:
  404680:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  404687:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  40468c:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  404693:	00 
  404694:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40469b:	00 
  40469c:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  4046a3:	00 
  4046a4:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  4046ab:	00 
  4046ac:	40 88 f0             	mov    %sil,%al
  4046af:	88 84 24 a7 00 00 00 	mov    %al,0xa7(%rsp)
  4046b6:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  4046bd:	00 
  4046be:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  4046c5:	00 
  4046c6:	48 8b 84 24 48 02 00 	mov    0x248(%rsp),%rax
  4046cd:	00 
  4046ce:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  4046d5:	00 
  4046d6:	48 8b 84 24 40 02 00 	mov    0x240(%rsp),%rax
  4046dd:	00 
  4046de:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  4046e5:	00 
  4046e6:	8a 84 24 a7 00 00 00 	mov    0xa7(%rsp),%al
  4046ed:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  4046f2:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  4046f9:	00 
  4046fa:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  404701:	00 
  404702:	48 8b bc 24 98 00 00 	mov    0x98(%rsp),%rdi
  404709:	00 
  40470a:	4c 8b 84 24 80 00 00 	mov    0x80(%rsp),%r8
  404711:	00 
  404712:	48 89 bc 24 30 02 00 	mov    %rdi,0x230(%rsp)
  404719:	00 
  40471a:	88 84 24 2f 02 00 00 	mov    %al,0x22f(%rsp)
  404721:	48 89 b4 24 20 02 00 	mov    %rsi,0x220(%rsp)
  404728:	00 
  404729:	48 89 94 24 18 02 00 	mov    %rdx,0x218(%rsp)
  404730:	00 
  404731:	4c 89 84 24 10 02 00 	mov    %r8,0x210(%rsp)
  404738:	00 
  404739:	48 89 8c 24 08 02 00 	mov    %rcx,0x208(%rsp)
  404740:	00 
  404741:	0f 57 c0             	xorps  %xmm0,%xmm0
  404744:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  40474b:	00 
  40474c:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404753:	00 
  404754:	48 89 bc 24 e0 01 00 	mov    %rdi,0x1e0(%rsp)
  40475b:	00 
  40475c:	48 89 b4 24 d8 01 00 	mov    %rsi,0x1d8(%rsp)
  404763:	00 
  404764:	48 89 94 24 d0 01 00 	mov    %rdx,0x1d0(%rsp)
  40476b:	00 
  40476c:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  404773:	00 
  404774:	0f b6 c8             	movzbl %al,%ecx
  404777:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40477c:	2c 07                	sub    $0x7,%al
  40477e:	0f 87 9a 07 00 00    	ja     404f1e <runtime::arena_allocator_proc+0x89e>
  404784:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  404789:	48 8b 04 c5 50 70 40 	mov    0x407050(,%rax,8),%rax
  404790:	00 
  404791:	ff e0                	jmp    *%rax
  404793:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  40479a:	00 
  40479b:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4047a2:	00 
  4047a3:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4047aa:	00 
  4047ab:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4047b2:	00 
  4047b3:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4047ba:	00 
  4047bb:	0f 57 c0             	xorps  %xmm0,%xmm0
  4047be:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  4047c5:	00 
  4047c6:	4c 8d 84 24 b0 01 00 	lea    0x1b0(%rsp),%r8
  4047cd:	00 
  4047ce:	e8 8d f0 ff ff       	call   403860 <runtime::arena_alloc>
  4047d3:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4047da:	00 
  4047db:	40 88 c7             	mov    %al,%dil
  4047de:	40 88 f8             	mov    %dil,%al
  4047e1:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4047e8:	00 
  4047e9:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4047f0:	00 
  4047f1:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4047f8:	00 
  4047f9:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404800:	00 
  404801:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404808:	00 
  404809:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40480d:	48 89 11             	mov    %rdx,(%rcx)
  404810:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404817:	c3                   	ret
  404818:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  40481f:	04 
  404820:	e9 f9 06 00 00       	jmp    404f1e <runtime::arena_allocator_proc+0x89e>
  404825:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  40482c:	00 
  40482d:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  404834:	00 
  404835:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  40483c:	00 
  40483d:	e8 de fc ff ff       	call   404520 <runtime::arena_free_all>
  404842:	e9 d7 06 00 00       	jmp    404f1e <runtime::arena_allocator_proc+0x89e>
  404847:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40484e:	00 
  40484f:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  404856:	00 
  404857:	48 83 bc 24 90 01 00 	cmpq   $0x0,0x190(%rsp)
  40485e:	00 00 
  404860:	0f 94 c1             	sete   %cl
  404863:	80 e1 01             	and    $0x1,%cl
  404866:	b0 01                	mov    $0x1,%al
  404868:	38 c8                	cmp    %cl,%al
  40486a:	74 25                	je     404891 <runtime::arena_allocator_proc+0x211>
  40486c:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  404873:	00 
  404874:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  40487b:	00 
  40487c:	0f 94 c1             	sete   %cl
  40487f:	80 e1 01             	and    $0x1,%cl
  404882:	b0 01                	mov    $0x1,%al
  404884:	38 c8                	cmp    %cl,%al
  404886:	0f 84 a8 00 00 00    	je     404934 <runtime::arena_allocator_proc+0x2b4>
  40488c:	e9 85 00 00 00       	jmp    404916 <runtime::arena_allocator_proc+0x296>
  404891:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404898:	00 
  404899:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4048a0:	00 
  4048a1:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  4048a8:	00 
  4048a9:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  4048b0:	00 
  4048b1:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  4048b8:	00 
  4048b9:	0f 57 c0             	xorps  %xmm0,%xmm0
  4048bc:	0f 29 84 24 80 01 00 	movaps %xmm0,0x180(%rsp)
  4048c3:	00 
  4048c4:	4c 8d 84 24 80 01 00 	lea    0x180(%rsp),%r8
  4048cb:	00 
  4048cc:	e8 8f ef ff ff       	call   403860 <runtime::arena_alloc>
  4048d1:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  4048d8:	00 
  4048d9:	40 88 c7             	mov    %al,%dil
  4048dc:	40 88 f8             	mov    %dil,%al
  4048df:	48 8b 94 24 80 01 00 	mov    0x180(%rsp),%rdx
  4048e6:	00 
  4048e7:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  4048ee:	00 
  4048ef:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4048f6:	00 
  4048f7:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4048fe:	00 
  4048ff:	40 88 bc 24 ef 01 00 	mov    %dil,0x1ef(%rsp)
  404906:	00 
  404907:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40490b:	48 89 11             	mov    %rdx,(%rcx)
  40490e:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404915:	c3                   	ret
  404916:	48 83 bc 24 d8 01 00 	cmpq   $0x0,0x1d8(%rsp)
  40491d:	00 00 
  40491f:	0f 94 c1             	sete   %cl
  404922:	80 e1 01             	and    $0x1,%cl
  404925:	b0 01                	mov    $0x1,%al
  404927:	38 c8                	cmp    %cl,%al
  404929:	0f 84 e5 00 00 00    	je     404a14 <runtime::arena_allocator_proc+0x394>
  40492f:	e9 b7 00 00 00       	jmp    4049eb <runtime::arena_allocator_proc+0x36b>
  404934:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40493b:	00 
  40493c:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  404941:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404948:	00 
  404949:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  40494e:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404953:	31 c0                	xor    %eax,%eax
  404955:	41 89 c0             	mov    %eax,%r8d
  404958:	be 3e 00 00 00       	mov    $0x3e,%esi
  40495d:	ba d1 00 00 00       	mov    $0xd1,%edx
  404962:	b9 13 00 00 00       	mov    $0x13,%ecx
  404967:	e8 94 eb ff ff       	call   403500 <runtime::multi_pointer_slice_expr_error>
  40496c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  404971:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  404976:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40497d:	00 
  40497e:	48 89 94 24 58 01 00 	mov    %rdx,0x158(%rsp)
  404985:	00 
  404986:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  40498d:	00 
  40498e:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  404995:	00 
  404996:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  40499d:	00 
  40499e:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  4049a5:	00 
  4049a6:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  4049ad:	00 
  4049ae:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  4049b5:	00 
  4049b6:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  4049bd:	00 
  4049be:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  4049c5:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  4049cc:	00 
  4049cd:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  4049d4:	00 
  4049d5:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4049dc:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4049e0:	48 89 11             	mov    %rdx,(%rcx)
  4049e3:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  4049ea:	c3                   	ret
  4049eb:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  4049f2:	00 
  4049f3:	48 8b 8c 24 d0 01 00 	mov    0x1d0(%rsp),%rcx
  4049fa:	00 
  4049fb:	48 83 e9 01          	sub    $0x1,%rcx
  4049ff:	48 21 c8             	and    %rcx,%rax
  404a02:	48 83 f8 00          	cmp    $0x0,%rax
  404a06:	0f 94 c1             	sete   %cl
  404a09:	80 e1 01             	and    $0x1,%cl
  404a0c:	b0 01                	mov    $0x1,%al
  404a0e:	38 c8                	cmp    %cl,%al
  404a10:	74 54                	je     404a66 <runtime::arena_allocator_proc+0x3e6>
  404a12:	eb 4d                	jmp    404a61 <runtime::arena_allocator_proc+0x3e1>
  404a14:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404a1b:	00 
  404a1c:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404a23:	04 
  404a24:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404a2b:	00 
  404a2c:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404a33:	00 
  404a34:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404a3b:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404a42:	00 
  404a43:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404a4a:	00 
  404a4b:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404a52:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404a56:	48 89 11             	mov    %rdx,(%rcx)
  404a59:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404a60:	c3                   	ret
  404a61:	e9 94 02 00 00       	jmp    404cfa <runtime::arena_allocator_proc+0x67a>
  404a66:	48 8b 84 24 d8 01 00 	mov    0x1d8(%rsp),%rax
  404a6d:	00 
  404a6e:	48 3b 84 24 c8 01 00 	cmp    0x1c8(%rsp),%rax
  404a75:	00 
  404a76:	0f 92 c0             	setb   %al
  404a79:	24 01                	and    $0x1,%al
  404a7b:	3c 00                	cmp    $0x0,%al
  404a7d:	0f 84 b7 00 00 00    	je     404b3a <runtime::arena_allocator_proc+0x4ba>
  404a83:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404a8a:	00 
  404a8b:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  404a90:	4c 8b 8c 24 d8 01 00 	mov    0x1d8(%rsp),%r9
  404a97:	00 
  404a98:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  404a9d:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404aa2:	31 c0                	xor    %eax,%eax
  404aa4:	41 89 c0             	mov    %eax,%r8d
  404aa7:	be 3e 00 00 00       	mov    $0x3e,%esi
  404aac:	ba d9 00 00 00       	mov    $0xd9,%edx
  404ab1:	b9 14 00 00 00       	mov    $0x14,%ecx
  404ab6:	e8 45 ea ff ff       	call   403500 <runtime::multi_pointer_slice_expr_error>
  404abb:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  404ac0:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  404ac5:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404acc:	00 
  404acd:	48 89 94 24 48 01 00 	mov    %rdx,0x148(%rsp)
  404ad4:	00 
  404ad5:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  404adc:	00 
  404add:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  404ae4:	00 
  404ae5:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  404aec:	00 
  404aed:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404af4:	00 
  404af5:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404afc:	00 
  404afd:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404b04:	00 
  404b05:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404b0c:	00 
  404b0d:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404b14:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404b1b:	00 
  404b1c:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404b23:	00 
  404b24:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404b2b:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404b2f:	48 89 11             	mov    %rdx,(%rcx)
  404b32:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404b39:	c3                   	ret
  404b3a:	eb 00                	jmp    404b3c <runtime::arena_allocator_proc+0x4bc>
  404b3c:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  404b43:	00 
  404b44:	48 8b 40 10          	mov    0x10(%rax),%rax
  404b48:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  404b4f:	00 
  404b50:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  404b57:	00 00 
  404b59:	0f 95 c0             	setne  %al
  404b5c:	24 01                	and    $0x1,%al
  404b5e:	3c 00                	cmp    $0x0,%al
  404b60:	0f 84 92 01 00 00    	je     404cf8 <runtime::arena_allocator_proc+0x678>
  404b66:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404b6d:	00 
  404b6e:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404b75:	00 
  404b76:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  404b7a:	48 29 c8             	sub    %rcx,%rax
  404b7d:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  404b84:	00 
  404b85:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404b8c:	00 
  404b8d:	48 03 84 24 c8 01 00 	add    0x1c8(%rsp),%rax
  404b94:	00 
  404b95:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  404b9c:	00 
  404b9d:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404ba4:	00 
  404ba5:	48 03 84 24 d8 01 00 	add    0x1d8(%rsp),%rax
  404bac:	00 
  404bad:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  404bb4:	00 
  404bb5:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  404bbc:	00 
  404bbd:	48 3b 84 24 30 01 00 	cmp    0x130(%rsp),%rax
  404bc4:	00 
  404bc5:	0f 92 c0             	setb   %al
  404bc8:	24 01                	and    $0x1,%al
  404bca:	3c 00                	cmp    $0x0,%al
  404bcc:	0f 84 24 01 00 00    	je     404cf6 <runtime::arena_allocator_proc+0x676>
  404bd2:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  404bd9:	00 
  404bda:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404be1:	00 
  404be2:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  404be6:	0f 94 c0             	sete   %al
  404be9:	24 01                	and    $0x1,%al
  404beb:	3c 00                	cmp    $0x0,%al
  404bed:	0f 84 03 01 00 00    	je     404cf6 <runtime::arena_allocator_proc+0x676>
  404bf3:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  404bfa:	00 
  404bfb:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  404c02:	00 
  404c03:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  404c07:	0f 96 c0             	setbe  %al
  404c0a:	24 01                	and    $0x1,%al
  404c0c:	3c 00                	cmp    $0x0,%al
  404c0e:	0f 84 e2 00 00 00    	je     404cf6 <runtime::arena_allocator_proc+0x676>
  404c14:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404c1b:	00 
  404c1c:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  404c23:	00 
  404c24:	48 89 48 20          	mov    %rcx,0x20(%rax)
  404c28:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  404c2f:	00 
  404c30:	48 8b 40 18          	mov    0x18(%rax),%rax
  404c34:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404c39:	4c 8b 84 24 38 01 00 	mov    0x138(%rsp),%r8
  404c40:	00 
  404c41:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  404c46:	4c 8b 8c 24 28 01 00 	mov    0x128(%rsp),%r9
  404c4d:	00 
  404c4e:	4c 89 4c 24 40       	mov    %r9,0x40(%rsp)
  404c53:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404c58:	be 3e 00 00 00       	mov    $0x3e,%esi
  404c5d:	ba e4 00 00 00       	mov    $0xe4,%edx
  404c62:	b9 17 00 00 00       	mov    $0x17,%ecx
  404c67:	e8 94 e8 ff ff       	call   403500 <runtime::multi_pointer_slice_expr_error>
  404c6c:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  404c71:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404c76:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  404c7b:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404c82:	00 
  404c83:	48 01 f2             	add    %rsi,%rdx
  404c86:	48 29 f0             	sub    %rsi,%rax
  404c89:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  404c90:	00 
  404c91:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  404c98:	00 
  404c99:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  404ca0:	00 
  404ca1:	48 8b 94 24 20 01 00 	mov    0x120(%rsp),%rdx
  404ca8:	00 
  404ca9:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404cb0:	00 
  404cb1:	48 89 84 24 f0 01 00 	mov    %rax,0x1f0(%rsp)
  404cb8:	00 
  404cb9:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404cc0:	00 
  404cc1:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404cc8:	00 
  404cc9:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404cd0:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404cd7:	00 
  404cd8:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404cdf:	00 
  404ce0:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404ce7:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404ceb:	48 89 11             	mov    %rdx,(%rcx)
  404cee:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404cf5:	c3                   	ret
  404cf6:	eb 00                	jmp    404cf8 <runtime::arena_allocator_proc+0x678>
  404cf8:	eb 00                	jmp    404cfa <runtime::arena_allocator_proc+0x67a>
  404cfa:	4c 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%r9
  404d01:	00 
  404d02:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  404d09:	00 
  404d0a:	48 8b bc 24 e0 01 00 	mov    0x1e0(%rsp),%rdi
  404d11:	00 
  404d12:	48 8b b4 24 d8 01 00 	mov    0x1d8(%rsp),%rsi
  404d19:	00 
  404d1a:	48 8b 94 24 d0 01 00 	mov    0x1d0(%rsp),%rdx
  404d21:	00 
  404d22:	0f 57 c0             	xorps  %xmm0,%xmm0
  404d25:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  404d2c:	00 
  404d2d:	4c 8d 84 24 00 01 00 	lea    0x100(%rsp),%r8
  404d34:	00 
  404d35:	e8 26 eb ff ff       	call   403860 <runtime::arena_alloc>
  404d3a:	88 44 24 27          	mov    %al,0x27(%rsp)
  404d3e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  404d45:	00 
  404d46:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  404d4b:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  404d52:	00 
  404d53:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404d58:	3c 00                	cmp    $0x0,%al
  404d5a:	74 50                	je     404dac <runtime::arena_allocator_proc+0x72c>
  404d5c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404d63:	00 
  404d64:	8a 44 24 27          	mov    0x27(%rsp),%al
  404d68:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404d6f:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404d76:	00 
  404d77:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404d7e:	00 
  404d7f:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404d86:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404d8d:	00 
  404d8e:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404d95:	00 
  404d96:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404d9d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404da1:	48 89 11             	mov    %rdx,(%rcx)
  404da4:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404dab:	c3                   	ret
  404dac:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404db1:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  404db6:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  404dbd:	00 
  404dbe:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404dc5:	00 
  404dc6:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  404dcd:	00 00 
  404dcf:	0f 94 c0             	sete   %al
  404dd2:	24 01                	and    $0x1,%al
  404dd4:	3c 00                	cmp    $0x0,%al
  404dd6:	74 45                	je     404e1d <runtime::arena_allocator_proc+0x79d>
  404dd8:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404ddf:	00 
  404de0:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404de7:	00 
  404de8:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404def:	00 
  404df0:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404df7:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404dfe:	00 
  404dff:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404e06:	00 
  404e07:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404e0e:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404e12:	48 89 11             	mov    %rdx,(%rcx)
  404e15:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404e1c:	c3                   	ret
  404e1d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404e24:	00 
  404e25:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  404e2a:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  404e31:	00 
  404e32:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  404e37:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  404e3e:	00 
  404e3f:	48 89 04 24          	mov    %rax,(%rsp)
  404e43:	4c 8b 8c 24 c8 01 00 	mov    0x1c8(%rsp),%r9
  404e4a:	00 
  404e4b:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  404e50:	bf a0 70 40 00       	mov    $0x4070a0,%edi
  404e55:	31 c0                	xor    %eax,%eax
  404e57:	41 89 c0             	mov    %eax,%r8d
  404e5a:	be 3e 00 00 00       	mov    $0x3e,%esi
  404e5f:	ba ee 00 00 00       	mov    $0xee,%edx
  404e64:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  404e69:	e8 92 e6 ff ff       	call   403500 <runtime::multi_pointer_slice_expr_error>
  404e6e:	48 8b 0c 24          	mov    (%rsp),%rcx
  404e72:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404e77:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  404e7c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  404e81:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  404e88:	00 
  404e89:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404e90:	00 
  404e91:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  404e98:	00 
  404e99:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  404ea0:	00 
  404ea1:	e8 8a ef ff ff       	call   403e30 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  404ea6:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  404ead:	00 
  404eae:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  404eb5:	00 
  404eb6:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  404ebd:	00 
  404ebe:	48 89 94 24 f8 01 00 	mov    %rdx,0x1f8(%rsp)
  404ec5:	00 
  404ec6:	48 89 8c 24 f0 01 00 	mov    %rcx,0x1f0(%rsp)
  404ecd:	00 
  404ece:	c6 84 24 ef 01 00 00 	movb   $0x0,0x1ef(%rsp)
  404ed5:	00 
  404ed6:	48 89 50 08          	mov    %rdx,0x8(%rax)
  404eda:	48 89 08             	mov    %rcx,(%rax)
  404edd:	31 c0                	xor    %eax,%eax
  404edf:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404ee6:	c3                   	ret
  404ee7:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  404eee:	00 
  404eef:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  404ef6:	00 
  404ef7:	48 83 bc 24 c0 00 00 	cmpq   $0x0,0xc0(%rsp)
  404efe:	00 00 
  404f00:	0f 95 c0             	setne  %al
  404f03:	24 01                	and    $0x1,%al
  404f05:	3c 00                	cmp    $0x0,%al
  404f07:	74 0b                	je     404f14 <runtime::arena_allocator_proc+0x894>
  404f09:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404f10:	00 
  404f11:	c6 00 5d             	movb   $0x5d,(%rax)
  404f14:	eb 08                	jmp    404f1e <runtime::arena_allocator_proc+0x89e>
  404f16:	c6 84 24 ef 01 00 00 	movb   $0x4,0x1ef(%rsp)
  404f1d:	04 
  404f1e:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  404f25:	00 
  404f26:	48 8b 94 24 f0 01 00 	mov    0x1f0(%rsp),%rdx
  404f2d:	00 
  404f2e:	48 8b b4 24 f8 01 00 	mov    0x1f8(%rsp),%rsi
  404f35:	00 
  404f36:	8a 84 24 ef 01 00 00 	mov    0x1ef(%rsp),%al
  404f3d:	48 89 b4 24 f8 01 00 	mov    %rsi,0x1f8(%rsp)
  404f44:	00 
  404f45:	48 89 94 24 f0 01 00 	mov    %rdx,0x1f0(%rsp)
  404f4c:	00 
  404f4d:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  404f54:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  404f58:	48 89 11             	mov    %rdx,(%rcx)
  404f5b:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  404f62:	c3                   	ret
  404f63:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  404f6a:	84 00 00 00 00 00 

0000000000404f70 <runtime::memory_equal>:
  404f70:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  404f75:	48 89 74 24 b8       	mov    %rsi,-0x48(%rsp)
  404f7a:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  404f7f:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404f84:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404f89:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  404f8e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  404f93:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  404f98:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  404f9d:	48 83 f8 00          	cmp    $0x0,%rax
  404fa1:	0f 94 c1             	sete   %cl
  404fa4:	80 e1 01             	and    $0x1,%cl
  404fa7:	b0 01                	mov    $0x1,%al
  404fa9:	38 c8                	cmp    %cl,%al
  404fab:	74 1b                	je     404fc8 <runtime::memory_equal+0x58>
  404fad:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  404fb2:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404fb7:	48 39 c8             	cmp    %rcx,%rax
  404fba:	0f 94 c1             	sete   %cl
  404fbd:	80 e1 01             	and    $0x1,%cl
  404fc0:	b0 01                	mov    $0x1,%al
  404fc2:	38 c8                	cmp    %cl,%al
  404fc4:	74 07                	je     404fcd <runtime::memory_equal+0x5d>
  404fc6:	eb 03                	jmp    404fcb <runtime::memory_equal+0x5b>
  404fc8:	b0 01                	mov    $0x1,%al
  404fca:	c3                   	ret
  404fcb:	eb 03                	jmp    404fd0 <runtime::memory_equal+0x60>
  404fcd:	b0 01                	mov    $0x1,%al
  404fcf:	c3                   	ret
  404fd0:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  404fd5:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  404fda:	48 8b 54 24 b0       	mov    -0x50(%rsp),%rdx
  404fdf:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  404fe4:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  404fe9:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  404fee:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  404ff5:	00 00 
  404ff7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  404ffc:	48 3b 44 24 d0       	cmp    -0x30(%rsp),%rax
  405001:	0f 92 c0             	setb   %al
  405004:	24 01                	and    $0x1,%al
  405006:	3c 00                	cmp    $0x0,%al
  405008:	74 38                	je     405042 <runtime::memory_equal+0xd2>
  40500a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40500f:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  405014:	8a 04 08             	mov    (%rax,%rcx,1),%al
  405017:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40501c:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  405021:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  405024:	0f 95 c0             	setne  %al
  405027:	24 01                	and    $0x1,%al
  405029:	3c 00                	cmp    $0x0,%al
  40502b:	74 03                	je     405030 <runtime::memory_equal+0xc0>
  40502d:	31 c0                	xor    %eax,%eax
  40502f:	c3                   	ret
  405030:	eb 00                	jmp    405032 <runtime::memory_equal+0xc2>
  405032:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405037:	48 83 c0 01          	add    $0x1,%rax
  40503b:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  405040:	eb b5                	jmp    404ff7 <runtime::memory_equal+0x87>
  405042:	b0 01                	mov    $0x1,%al
  405044:	c3                   	ret
  405045:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40504c:	00 00 00 00 

0000000000405050 <runtime::memory_compare>:
  405050:	48 83 ec 10          	sub    $0x10,%rsp
  405054:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  405059:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  40505e:	48 89 54 24 a0       	mov    %rdx,-0x60(%rsp)
  405063:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  405068:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40506d:	48 8b 54 24 a0       	mov    -0x60(%rsp),%rdx
  405072:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  405077:	48 89 0c 24          	mov    %rcx,(%rsp)
  40507b:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  405080:	48 39 c8             	cmp    %rcx,%rax
  405083:	0f 94 c1             	sete   %cl
  405086:	80 e1 01             	and    $0x1,%cl
  405089:	b0 01                	mov    $0x1,%al
  40508b:	38 c8                	cmp    %cl,%al
  40508d:	74 17                	je     4050a6 <runtime::memory_compare+0x56>
  40508f:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  405094:	48 83 f8 00          	cmp    $0x0,%rax
  405098:	0f 94 c1             	sete   %cl
  40509b:	80 e1 01             	and    $0x1,%cl
  40509e:	b0 01                	mov    $0x1,%al
  4050a0:	38 c8                	cmp    %cl,%al
  4050a2:	74 20                	je     4050c4 <runtime::memory_compare+0x74>
  4050a4:	eb 07                	jmp    4050ad <runtime::memory_compare+0x5d>
  4050a6:	31 c0                	xor    %eax,%eax
  4050a8:	48 83 c4 10          	add    $0x10,%rsp
  4050ac:	c3                   	ret
  4050ad:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  4050b2:	48 83 f8 00          	cmp    $0x0,%rax
  4050b6:	0f 94 c1             	sete   %cl
  4050b9:	80 e1 01             	and    $0x1,%cl
  4050bc:	b0 01                	mov    $0x1,%al
  4050be:	38 c8                	cmp    %cl,%al
  4050c0:	74 10                	je     4050d2 <runtime::memory_compare+0x82>
  4050c2:	eb 0c                	jmp    4050d0 <runtime::memory_compare+0x80>
  4050c4:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4050cb:	48 83 c4 10          	add    $0x10,%rsp
  4050cf:	c3                   	ret
  4050d0:	eb 0a                	jmp    4050dc <runtime::memory_compare+0x8c>
  4050d2:	b8 01 00 00 00       	mov    $0x1,%eax
  4050d7:	48 83 c4 10          	add    $0x10,%rsp
  4050db:	c3                   	ret
  4050dc:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  4050e1:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  4050e6:	48 8b 54 24 90       	mov    -0x70(%rsp),%rdx
  4050eb:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  4050f0:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4050f5:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4050fa:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4050ff:	48 c1 e8 03          	shr    $0x3,%rax
  405103:	48 83 c0 01          	add    $0x1,%rax
  405107:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40510c:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405111:	48 83 e8 01          	sub    $0x1,%rax
  405115:	48 c1 e0 03          	shl    $0x3,%rax
  405119:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40511e:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  405125:	00 00 
  405127:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  40512d:	0f 92 c0             	setb   %al
  405130:	24 01                	and    $0x1,%al
  405132:	3c 00                	cmp    $0x0,%al
  405134:	74 09                	je     40513f <runtime::memory_compare+0xef>
  405136:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  40513d:	00 00 
  40513f:	eb 00                	jmp    405141 <runtime::memory_compare+0xf1>
  405141:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405146:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  40514b:	0f 92 c0             	setb   %al
  40514e:	24 01                	and    $0x1,%al
  405150:	3c 00                	cmp    $0x0,%al
  405152:	0f 84 11 01 00 00    	je     405269 <runtime::memory_compare+0x219>
  405158:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40515d:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  405162:	48 c1 e1 03          	shl    $0x3,%rcx
  405166:	48 01 c8             	add    %rcx,%rax
  405169:	48 8b 00             	mov    (%rax),%rax
  40516c:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  405171:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405176:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  40517b:	48 c1 e1 03          	shl    $0x3,%rcx
  40517f:	48 01 c8             	add    %rcx,%rax
  405182:	48 8b 00             	mov    (%rax),%rax
  405185:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40518a:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40518f:	48 33 44 24 b8       	xor    -0x48(%rsp),%rax
  405194:	48 83 f8 00          	cmp    $0x0,%rax
  405198:	0f 95 c0             	setne  %al
  40519b:	24 01                	and    $0x1,%al
  40519d:	3c 00                	cmp    $0x0,%al
  40519f:	0f 84 af 00 00 00    	je     405254 <runtime::memory_compare+0x204>
  4051a5:	eb 00                	jmp    4051a7 <runtime::memory_compare+0x157>
  4051a7:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4051ac:	48 c1 e0 03          	shl    $0x3,%rax
  4051b0:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4051b5:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4051ba:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4051bf:	0f 92 c0             	setb   %al
  4051c2:	24 01                	and    $0x1,%al
  4051c4:	3c 00                	cmp    $0x0,%al
  4051c6:	0f 84 86 00 00 00    	je     405252 <runtime::memory_compare+0x202>
  4051cc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4051d1:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  4051d6:	8a 00                	mov    (%rax),%al
  4051d8:	88 44 24 af          	mov    %al,-0x51(%rsp)
  4051dc:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4051e1:	48 03 44 24 b0       	add    -0x50(%rsp),%rax
  4051e6:	8a 00                	mov    (%rax),%al
  4051e8:	88 44 24 ae          	mov    %al,-0x52(%rsp)
  4051ec:	8a 44 24 af          	mov    -0x51(%rsp),%al
  4051f0:	32 44 24 ae          	xor    -0x52(%rsp),%al
  4051f4:	3c 00                	cmp    $0x0,%al
  4051f6:	0f 95 c0             	setne  %al
  4051f9:	24 01                	and    $0x1,%al
  4051fb:	3c 00                	cmp    $0x0,%al
  4051fd:	74 3e                	je     40523d <runtime::memory_compare+0x1ed>
  4051ff:	0f b6 44 24 af       	movzbl -0x51(%rsp),%eax
  405204:	0f b6 4c 24 ae       	movzbl -0x52(%rsp),%ecx
  405209:	48 29 c8             	sub    %rcx,%rax
  40520c:	48 83 f8 00          	cmp    $0x0,%rax
  405210:	0f 9c c0             	setl   %al
  405213:	24 01                	and    $0x1,%al
  405215:	3c 00                	cmp    $0x0,%al
  405217:	74 0e                	je     405227 <runtime::memory_compare+0x1d7>
  405219:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405220:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  405225:	eb 0c                	jmp    405233 <runtime::memory_compare+0x1e3>
  405227:	b8 01 00 00 00       	mov    $0x1,%eax
  40522c:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  405231:	eb 00                	jmp    405233 <runtime::memory_compare+0x1e3>
  405233:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  405238:	48 83 c4 10          	add    $0x10,%rsp
  40523c:	c3                   	ret
  40523d:	eb 00                	jmp    40523f <runtime::memory_compare+0x1ef>
  40523f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405244:	48 83 c0 01          	add    $0x1,%rax
  405248:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40524d:	e9 63 ff ff ff       	jmp    4051b5 <runtime::memory_compare+0x165>
  405252:	eb 00                	jmp    405254 <runtime::memory_compare+0x204>
  405254:	eb 00                	jmp    405256 <runtime::memory_compare+0x206>
  405256:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40525b:	48 83 c0 01          	add    $0x1,%rax
  40525f:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  405264:	e9 d8 fe ff ff       	jmp    405141 <runtime::memory_compare+0xf1>
  405269:	eb 00                	jmp    40526b <runtime::memory_compare+0x21b>
  40526b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405270:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  405275:	0f 92 c0             	setb   %al
  405278:	24 01                	and    $0x1,%al
  40527a:	3c 00                	cmp    $0x0,%al
  40527c:	0f 84 86 00 00 00    	je     405308 <runtime::memory_compare+0x2b8>
  405282:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405287:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  40528c:	8a 00                	mov    (%rax),%al
  40528e:	88 44 24 ad          	mov    %al,-0x53(%rsp)
  405292:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405297:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  40529c:	8a 00                	mov    (%rax),%al
  40529e:	88 44 24 ac          	mov    %al,-0x54(%rsp)
  4052a2:	8a 44 24 ad          	mov    -0x53(%rsp),%al
  4052a6:	32 44 24 ac          	xor    -0x54(%rsp),%al
  4052aa:	3c 00                	cmp    $0x0,%al
  4052ac:	0f 95 c0             	setne  %al
  4052af:	24 01                	and    $0x1,%al
  4052b1:	3c 00                	cmp    $0x0,%al
  4052b3:	74 3e                	je     4052f3 <runtime::memory_compare+0x2a3>
  4052b5:	0f b6 44 24 ad       	movzbl -0x53(%rsp),%eax
  4052ba:	0f b6 4c 24 ac       	movzbl -0x54(%rsp),%ecx
  4052bf:	48 29 c8             	sub    %rcx,%rax
  4052c2:	48 83 f8 00          	cmp    $0x0,%rax
  4052c6:	0f 9c c0             	setl   %al
  4052c9:	24 01                	and    $0x1,%al
  4052cb:	3c 00                	cmp    $0x0,%al
  4052cd:	74 0e                	je     4052dd <runtime::memory_compare+0x28d>
  4052cf:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4052d6:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  4052db:	eb 0c                	jmp    4052e9 <runtime::memory_compare+0x299>
  4052dd:	b8 01 00 00 00       	mov    $0x1,%eax
  4052e2:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  4052e7:	eb 00                	jmp    4052e9 <runtime::memory_compare+0x299>
  4052e9:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  4052ee:	48 83 c4 10          	add    $0x10,%rsp
  4052f2:	c3                   	ret
  4052f3:	eb 00                	jmp    4052f5 <runtime::memory_compare+0x2a5>
  4052f5:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4052fa:	48 83 c0 01          	add    $0x1,%rax
  4052fe:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405303:	e9 63 ff ff ff       	jmp    40526b <runtime::memory_compare+0x21b>
  405308:	31 c0                	xor    %eax,%eax
  40530a:	48 83 c4 10          	add    $0x10,%rsp
  40530e:	c3                   	ret
  40530f:	90                   	nop

0000000000405310 <runtime::memory_compare_zero>:
  405310:	48 89 7c 24 a0       	mov    %rdi,-0x60(%rsp)
  405315:	48 89 74 24 a8       	mov    %rsi,-0x58(%rsp)
  40531a:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40531f:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  405324:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405329:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40532e:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  405333:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405338:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40533d:	48 c1 e8 03          	shr    $0x3,%rax
  405341:	48 83 c0 01          	add    $0x1,%rax
  405345:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40534a:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40534f:	48 83 e8 01          	sub    $0x1,%rax
  405353:	48 c1 e0 03          	shl    $0x3,%rax
  405357:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40535c:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  405363:	00 00 
  405365:	48 83 7c 24 e0 08    	cmpq   $0x8,-0x20(%rsp)
  40536b:	0f 92 c0             	setb   %al
  40536e:	24 01                	and    $0x1,%al
  405370:	3c 00                	cmp    $0x0,%al
  405372:	74 09                	je     40537d <runtime::memory_compare_zero+0x6d>
  405374:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  40537b:	00 00 
  40537d:	eb 00                	jmp    40537f <runtime::memory_compare_zero+0x6f>
  40537f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  405384:	48 3b 44 24 d8       	cmp    -0x28(%rsp),%rax
  405389:	0f 92 c0             	setb   %al
  40538c:	24 01                	and    $0x1,%al
  40538e:	3c 00                	cmp    $0x0,%al
  405390:	0f 84 d2 00 00 00    	je     405468 <runtime::memory_compare_zero+0x158>
  405396:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40539b:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  4053a0:	48 c1 e1 03          	shl    $0x3,%rcx
  4053a4:	48 01 c8             	add    %rcx,%rax
  4053a7:	48 8b 00             	mov    (%rax),%rax
  4053aa:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4053af:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4053b4:	48 83 f0 00          	xor    $0x0,%rax
  4053b8:	48 83 f8 00          	cmp    $0x0,%rax
  4053bc:	0f 95 c0             	setne  %al
  4053bf:	24 01                	and    $0x1,%al
  4053c1:	3c 00                	cmp    $0x0,%al
  4053c3:	0f 84 8a 00 00 00    	je     405453 <runtime::memory_compare_zero+0x143>
  4053c9:	eb 00                	jmp    4053cb <runtime::memory_compare_zero+0xbb>
  4053cb:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4053d0:	48 c1 e0 03          	shl    $0x3,%rax
  4053d4:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  4053d9:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4053de:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  4053e3:	0f 92 c0             	setb   %al
  4053e6:	24 01                	and    $0x1,%al
  4053e8:	3c 00                	cmp    $0x0,%al
  4053ea:	74 65                	je     405451 <runtime::memory_compare_zero+0x141>
  4053ec:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4053f1:	48 03 44 24 b8       	add    -0x48(%rsp),%rax
  4053f6:	8a 00                	mov    (%rax),%al
  4053f8:	88 44 24 b7          	mov    %al,-0x49(%rsp)
  4053fc:	8a 44 24 b7          	mov    -0x49(%rsp),%al
  405400:	34 00                	xor    $0x0,%al
  405402:	3c 00                	cmp    $0x0,%al
  405404:	0f 95 c0             	setne  %al
  405407:	24 01                	and    $0x1,%al
  405409:	3c 00                	cmp    $0x0,%al
  40540b:	74 32                	je     40543f <runtime::memory_compare_zero+0x12f>
  40540d:	0f b6 44 24 b7       	movzbl -0x49(%rsp),%eax
  405412:	48 83 f8 00          	cmp    $0x0,%rax
  405416:	0f 9c c0             	setl   %al
  405419:	24 01                	and    $0x1,%al
  40541b:	3c 00                	cmp    $0x0,%al
  40541d:	74 0e                	je     40542d <runtime::memory_compare_zero+0x11d>
  40541f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  405426:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40542b:	eb 0c                	jmp    405439 <runtime::memory_compare_zero+0x129>
  40542d:	b8 01 00 00 00       	mov    $0x1,%eax
  405432:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405437:	eb 00                	jmp    405439 <runtime::memory_compare_zero+0x129>
  405439:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40543e:	c3                   	ret
  40543f:	eb 00                	jmp    405441 <runtime::memory_compare_zero+0x131>
  405441:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405446:	48 83 c0 01          	add    $0x1,%rax
  40544a:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40544f:	eb 88                	jmp    4053d9 <runtime::memory_compare_zero+0xc9>
  405451:	eb 00                	jmp    405453 <runtime::memory_compare_zero+0x143>
  405453:	eb 00                	jmp    405455 <runtime::memory_compare_zero+0x145>
  405455:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40545a:	48 83 c0 01          	add    $0x1,%rax
  40545e:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  405463:	e9 17 ff ff ff       	jmp    40537f <runtime::memory_compare_zero+0x6f>
  405468:	eb 00                	jmp    40546a <runtime::memory_compare_zero+0x15a>
  40546a:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40546f:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  405474:	0f 92 c0             	setb   %al
  405477:	24 01                	and    $0x1,%al
  405479:	3c 00                	cmp    $0x0,%al
  40547b:	74 65                	je     4054e2 <runtime::memory_compare_zero+0x1d2>
  40547d:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405482:	48 03 44 24 d0       	add    -0x30(%rsp),%rax
  405487:	8a 00                	mov    (%rax),%al
  405489:	88 44 24 b6          	mov    %al,-0x4a(%rsp)
  40548d:	8a 44 24 b6          	mov    -0x4a(%rsp),%al
  405491:	34 00                	xor    $0x0,%al
  405493:	3c 00                	cmp    $0x0,%al
  405495:	0f 95 c0             	setne  %al
  405498:	24 01                	and    $0x1,%al
  40549a:	3c 00                	cmp    $0x0,%al
  40549c:	74 32                	je     4054d0 <runtime::memory_compare_zero+0x1c0>
  40549e:	0f b6 44 24 b6       	movzbl -0x4a(%rsp),%eax
  4054a3:	48 83 f8 00          	cmp    $0x0,%rax
  4054a7:	0f 9c c0             	setl   %al
  4054aa:	24 01                	and    $0x1,%al
  4054ac:	3c 00                	cmp    $0x0,%al
  4054ae:	74 0e                	je     4054be <runtime::memory_compare_zero+0x1ae>
  4054b0:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  4054b7:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  4054bc:	eb 0c                	jmp    4054ca <runtime::memory_compare_zero+0x1ba>
  4054be:	b8 01 00 00 00       	mov    $0x1,%eax
  4054c3:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  4054c8:	eb 00                	jmp    4054ca <runtime::memory_compare_zero+0x1ba>
  4054ca:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  4054cf:	c3                   	ret
  4054d0:	eb 00                	jmp    4054d2 <runtime::memory_compare_zero+0x1c2>
  4054d2:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4054d7:	48 83 c0 01          	add    $0x1,%rax
  4054db:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4054e0:	eb 88                	jmp    40546a <runtime::memory_compare_zero+0x15a>
  4054e2:	31 c0                	xor    %eax,%eax
  4054e4:	c3                   	ret
  4054e5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4054ec:	00 00 00 00 

00000000004054f0 <runtime::__type_info_of>:
  4054f0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  4054f5:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4054fa:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4054ff:	48 c7 c1 40 75 40 00 	mov    $0x407540,%rcx
  405506:	48 8b 49 08          	mov    0x8(%rcx),%rcx
  40550a:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40550f:	31 c9                	xor    %ecx,%ecx
  405511:	89 ca                	mov    %ecx,%edx
  405513:	48 f7 74 24 f0       	divq   -0x10(%rsp)
  405518:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  40551d:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  405524:	00 00 
  405526:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  40552d:	00 00 
  40552f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405534:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  405539:	0f 83 9f 00 00 00    	jae    4055de <runtime::__type_info_of+0xee>
  40553f:	48 c7 c0 40 75 40 00 	mov    $0x407540,%rax
  405546:	48 8b 00             	mov    (%rax),%rax
  405549:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40554e:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  405552:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  405557:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  40555d:	0f 95 c0             	setne  %al
  405560:	24 01                	and    $0x1,%al
  405562:	3c 00                	cmp    $0x0,%al
  405564:	74 1d                	je     405583 <runtime::__type_info_of+0x93>
  405566:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  40556b:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405570:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  405574:	0f 94 c0             	sete   %al
  405577:	24 01                	and    $0x1,%al
  405579:	3c 00                	cmp    $0x0,%al
  40557b:	74 06                	je     405583 <runtime::__type_info_of+0x93>
  40557d:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405582:	c3                   	ret
  405583:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  405588:	48 83 c0 01          	add    $0x1,%rax
  40558c:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  405591:	0f 92 c0             	setb   %al
  405594:	24 01                	and    $0x1,%al
  405596:	3c 00                	cmp    $0x0,%al
  405598:	74 10                	je     4055aa <runtime::__type_info_of+0xba>
  40559a:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40559f:	48 83 c0 01          	add    $0x1,%rax
  4055a3:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4055a8:	eb 09                	jmp    4055b3 <runtime::__type_info_of+0xc3>
  4055aa:	31 c0                	xor    %eax,%eax
  4055ac:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4055b1:	eb 00                	jmp    4055b3 <runtime::__type_info_of+0xc3>
  4055b3:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  4055b8:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4055bd:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4055c2:	48 83 c0 01          	add    $0x1,%rax
  4055c6:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4055cb:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4055d0:	48 83 c0 01          	add    $0x1,%rax
  4055d4:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4055d9:	e9 51 ff ff ff       	jmp    40552f <runtime::__type_info_of+0x3f>
  4055de:	48 c7 c0 40 75 40 00 	mov    $0x407540,%rax
  4055e5:	48 8b 00             	mov    (%rax),%rax
  4055e8:	48 8b 00             	mov    (%rax),%rax
  4055eb:	c3                   	ret
  4055ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004055f0 <runtime::default_logger_proc>:
  4055f0:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  4055f5:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4055fa:	66 44 89 c0          	mov    %r8w,%ax
  4055fe:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  405603:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  405608:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40560d:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  405612:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  405617:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40561c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  405621:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  405626:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  40562b:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  405630:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  405635:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40563a:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  40563f:	c3                   	ret

0000000000405640 <runtime::default_context>:
  405640:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  405647:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40564c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  405651:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405656:	31 f6                	xor    %esi,%esi
  405658:	ba 70 00 00 00       	mov    $0x70,%edx
  40565d:	e8 de b9 ff ff       	call   401040 <memset@plt>
  405662:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  405667:	e8 24 00 00 00       	call   405690 <runtime::[core.odin]::__init_context>
  40566c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  405671:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  405676:	ba 70 00 00 00       	mov    $0x70,%edx
  40567b:	e8 e0 b9 ff ff       	call   401060 <memcpy@plt>
  405680:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405685:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40568c:	c3                   	ret
  40568d:	0f 1f 00             	nopl   (%rax)

0000000000405690 <runtime::[core.odin]::__init_context>:
  405690:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  405695:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40569a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40569f:	48 83 f8 00          	cmp    $0x0,%rax
  4056a3:	0f 94 c0             	sete   %al
  4056a6:	24 01                	and    $0x1,%al
  4056a8:	3c 00                	cmp    $0x0,%al
  4056aa:	74 01                	je     4056ad <runtime::[core.odin]::__init_context+0x1d>
  4056ac:	c3                   	ret
  4056ad:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4056b2:	48 c7 c1 c0 23 40 00 	mov    $0x4023c0,%rcx
  4056b9:	48 89 08             	mov    %rcx,(%rax)
  4056bc:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4056c1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4056c8:	00 
  4056c9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4056ce:	48 c7 c1 50 2e 40 00 	mov    $0x402e50,%rcx
  4056d5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4056d9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4056de:	48 c7 c2 c0 ff ff ff 	mov    $0xffffffffffffffc0,%rdx
  4056e5:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  4056ec:	00 00 
  4056ee:	48 01 d1             	add    %rdx,%rcx
  4056f1:	48 89 48 18          	mov    %rcx,0x18(%rax)
  4056f5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4056fa:	48 c7 c1 40 57 40 00 	mov    $0x405740,%rcx
  405701:	48 89 48 20          	mov    %rcx,0x20(%rax)
  405705:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40570a:	48 c7 c1 f0 55 40 00 	mov    $0x4055f0,%rcx
  405711:	48 89 48 28          	mov    %rcx,0x28(%rax)
  405715:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40571a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  405721:	00 
  405722:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405727:	48 c7 c1 10 2b 40 00 	mov    $0x402b10,%rcx
  40572e:	48 89 48 48          	mov    %rcx,0x48(%rax)
  405732:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405737:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  40573e:	00 
  40573f:	c3                   	ret

0000000000405740 <runtime::default_assertion_failure_proc>:
  405740:	48 83 ec 48          	sub    $0x48,%rsp
  405744:	4c 89 04 24          	mov    %r8,(%rsp)
  405748:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40574d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  405752:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405757:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40575c:	4c 8b 04 24          	mov    (%rsp),%r8
  405760:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  405765:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40576a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40576f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405774:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  405779:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40577e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  405783:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  405788:	e8 03 00 00 00       	call   405790 <runtime::default_assertion_contextless_failure_proc>
  40578d:	0f 1f 00             	nopl   (%rax)

0000000000405790 <runtime::default_assertion_contextless_failure_proc>:
  405790:	48 83 ec 48          	sub    $0x48,%rsp
  405794:	4c 89 04 24          	mov    %r8,(%rsp)
  405798:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40579d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4057a2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4057a7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4057ac:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4057b1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4057b6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4057bb:	48 8b 3c 24          	mov    (%rsp),%rdi
  4057bf:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4057c4:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4057c9:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4057ce:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4057d3:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4057d8:	e8 d3 ec ff ff       	call   4044b0 <runtime::print_caller_location>
  4057dd:	bf 92 72 40 00       	mov    $0x407292,%edi
  4057e2:	be 01 00 00 00       	mov    $0x1,%esi
  4057e7:	e8 a4 e4 ff ff       	call   403c90 <runtime::print_string>
  4057ec:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4057f1:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4057f6:	e8 95 e4 ff ff       	call   403c90 <runtime::print_string>
  4057fb:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  405800:	48 83 f8 00          	cmp    $0x0,%rax
  405804:	0f 9f c0             	setg   %al
  405807:	24 01                	and    $0x1,%al
  405809:	3c 00                	cmp    $0x0,%al
  40580b:	74 1e                	je     40582b <runtime::default_assertion_contextless_failure_proc+0x9b>
  40580d:	bf 94 72 40 00       	mov    $0x407294,%edi
  405812:	be 02 00 00 00       	mov    $0x2,%esi
  405817:	e8 74 e4 ff ff       	call   403c90 <runtime::print_string>
  40581c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  405821:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  405826:	e8 65 e4 ff ff       	call   403c90 <runtime::print_string>
  40582b:	bf 0a 00 00 00       	mov    $0xa,%edi
  405830:	e8 8b e6 ff ff       	call   403ec0 <runtime::print_byte>
  405835:	0f 0b                	ud2
  405837:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40583e:	00 00 

0000000000405840 <__truncsfhf2>:
  405840:	48 83 ec 18          	sub    $0x18,%rsp
  405844:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  40584a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  405850:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  405856:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40585d:	00 
  40585e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  405865:	00 
  405866:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40586d:	00 
  40586e:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  405875:	00 
  405876:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  40587d:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  405883:	8b 44 24 10          	mov    0x10(%rsp),%eax
  405887:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  40588b:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40588f:	c1 f9 10             	sar    $0x10,%ecx
  405892:	b2 01                	mov    $0x1,%dl
  405894:	31 c0                	xor    %eax,%eax
  405896:	f6 c2 01             	test   $0x1,%dl
  405899:	0f 45 c1             	cmovne %ecx,%eax
  40589c:	25 00 80 00 00       	and    $0x8000,%eax
  4058a1:	89 44 24 08          	mov    %eax,0x8(%rsp)
  4058a5:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  4058a9:	c1 f9 17             	sar    $0x17,%ecx
  4058ac:	b2 01                	mov    $0x1,%dl
  4058ae:	31 c0                	xor    %eax,%eax
  4058b0:	f6 c2 01             	test   $0x1,%dl
  4058b3:	0f 45 c1             	cmovne %ecx,%eax
  4058b6:	25 ff 00 00 00       	and    $0xff,%eax
  4058bb:	83 e8 70             	sub    $0x70,%eax
  4058be:	89 44 24 04          	mov    %eax,0x4(%rsp)
  4058c2:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  4058c6:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  4058cb:	89 04 24             	mov    %eax,(%rsp)
  4058ce:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  4058d3:	0f 9e c0             	setle  %al
  4058d6:	24 01                	and    $0x1,%al
  4058d8:	3c 00                	cmp    $0x0,%al
  4058da:	0f 84 82 00 00 00    	je     405962 <__truncsfhf2+0x122>
  4058e0:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  4058e5:	0f 9c c0             	setl   %al
  4058e8:	24 01                	and    $0x1,%al
  4058ea:	3c 00                	cmp    $0x0,%al
  4058ec:	74 16                	je     405904 <__truncsfhf2+0xc4>
  4058ee:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  4058f3:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  4058f8:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  4058ff:	48 83 c4 18          	add    $0x18,%rsp
  405903:	c3                   	ret
  405904:	8b 04 24             	mov    (%rsp),%eax
  405907:	0d 00 00 80 00       	or     $0x800000,%eax
  40590c:	ba 01 00 00 00       	mov    $0x1,%edx
  405911:	2b 54 24 04          	sub    0x4(%rsp),%edx
  405915:	89 d1                	mov    %edx,%ecx
  405917:	d3 f8                	sar    %cl,%eax
  405919:	89 c1                	mov    %eax,%ecx
  40591b:	31 c0                	xor    %eax,%eax
  40591d:	83 fa 20             	cmp    $0x20,%edx
  405920:	0f 42 c1             	cmovb  %ecx,%eax
  405923:	89 04 24             	mov    %eax,(%rsp)
  405926:	8b 04 24             	mov    (%rsp),%eax
  405929:	25 00 10 00 00       	and    $0x1000,%eax
  40592e:	83 f8 00             	cmp    $0x0,%eax
  405931:	0f 95 c0             	setne  %al
  405934:	24 01                	and    $0x1,%al
  405936:	3c 00                	cmp    $0x0,%al
  405938:	74 0b                	je     405945 <__truncsfhf2+0x105>
  40593a:	8b 04 24             	mov    (%rsp),%eax
  40593d:	05 00 20 00 00       	add    $0x2000,%eax
  405942:	89 04 24             	mov    %eax,(%rsp)
  405945:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405949:	8b 0c 24             	mov    (%rsp),%ecx
  40594c:	c1 e9 0d             	shr    $0xd,%ecx
  40594f:	09 c8                	or     %ecx,%eax
  405951:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  405956:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40595d:	48 83 c4 18          	add    $0x18,%rsp
  405961:	c3                   	ret
  405962:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  405969:	00 
  40596a:	0f 94 c0             	sete   %al
  40596d:	24 01                	and    $0x1,%al
  40596f:	3c 00                	cmp    $0x0,%al
  405971:	74 59                	je     4059cc <__truncsfhf2+0x18c>
  405973:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  405977:	0f 94 c0             	sete   %al
  40597a:	24 01                	and    $0x1,%al
  40597c:	3c 00                	cmp    $0x0,%al
  40597e:	74 1a                	je     40599a <__truncsfhf2+0x15a>
  405980:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405984:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405989:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  40598e:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  405995:	48 83 c4 18          	add    $0x18,%rsp
  405999:	c3                   	ret
  40599a:	8b 04 24             	mov    (%rsp),%eax
  40599d:	c1 f8 0d             	sar    $0xd,%eax
  4059a0:	89 04 24             	mov    %eax,(%rsp)
  4059a3:	8b 44 24 08          	mov    0x8(%rsp),%eax
  4059a7:	8b 0c 24             	mov    (%rsp),%ecx
  4059aa:	09 c8                	or     %ecx,%eax
  4059ac:	85 c9                	test   %ecx,%ecx
  4059ae:	0f 94 c1             	sete   %cl
  4059b1:	0f b6 c9             	movzbl %cl,%ecx
  4059b4:	09 c8                	or     %ecx,%eax
  4059b6:	0d 00 7c 00 00       	or     $0x7c00,%eax
  4059bb:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  4059c0:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  4059c7:	48 83 c4 18          	add    $0x18,%rsp
  4059cb:	c3                   	ret
  4059cc:	8b 04 24             	mov    (%rsp),%eax
  4059cf:	25 00 10 00 00       	and    $0x1000,%eax
  4059d4:	83 f8 00             	cmp    $0x0,%eax
  4059d7:	0f 95 c0             	setne  %al
  4059da:	24 01                	and    $0x1,%al
  4059dc:	3c 00                	cmp    $0x0,%al
  4059de:	74 33                	je     405a13 <__truncsfhf2+0x1d3>
  4059e0:	8b 04 24             	mov    (%rsp),%eax
  4059e3:	05 00 20 00 00       	add    $0x2000,%eax
  4059e8:	89 04 24             	mov    %eax,(%rsp)
  4059eb:	8b 04 24             	mov    (%rsp),%eax
  4059ee:	25 00 00 80 00       	and    $0x800000,%eax
  4059f3:	83 f8 00             	cmp    $0x0,%eax
  4059f6:	0f 95 c0             	setne  %al
  4059f9:	24 01                	and    $0x1,%al
  4059fb:	3c 00                	cmp    $0x0,%al
  4059fd:	74 12                	je     405a11 <__truncsfhf2+0x1d1>
  4059ff:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  405a06:	8b 44 24 04          	mov    0x4(%rsp),%eax
  405a0a:	83 c0 01             	add    $0x1,%eax
  405a0d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  405a11:	eb 00                	jmp    405a13 <__truncsfhf2+0x1d3>
  405a13:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  405a18:	0f 9f c0             	setg   %al
  405a1b:	24 01                	and    $0x1,%al
  405a1d:	3c 00                	cmp    $0x0,%al
  405a1f:	74 75                	je     405a96 <__truncsfhf2+0x256>
  405a21:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  405a28:	00 00 00 
  405a2b:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405a30:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  405a37:	00 00 
  405a39:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  405a3f:	0f 9c c0             	setl   %al
  405a42:	24 01                	and    $0x1,%al
  405a44:	3c 00                	cmp    $0x0,%al
  405a46:	74 34                	je     405a7c <__truncsfhf2+0x23c>
  405a48:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405a4d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405a52:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405a57:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  405a5d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405a62:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  405a67:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  405a6c:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405a71:	48 83 c0 01          	add    $0x1,%rax
  405a75:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  405a7a:	eb bd                	jmp    405a39 <__truncsfhf2+0x1f9>
  405a7c:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405a80:	0d 00 7c 00 00       	or     $0x7c00,%eax
  405a85:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  405a8a:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  405a91:	48 83 c4 18          	add    $0x18,%rsp
  405a95:	c3                   	ret
  405a96:	8b 44 24 08          	mov    0x8(%rsp),%eax
  405a9a:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  405a9e:	c1 e1 0a             	shl    $0xa,%ecx
  405aa1:	09 c8                	or     %ecx,%eax
  405aa3:	8b 0c 24             	mov    (%rsp),%ecx
  405aa6:	c1 e9 0d             	shr    $0xd,%ecx
  405aa9:	09 c8                	or     %ecx,%eax
  405aab:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  405ab0:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  405ab7:	48 83 c4 18          	add    $0x18,%rsp
  405abb:	c3                   	ret
  405abc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405ac0 <__truncdfhf2>:
  405ac0:	48 83 ec 18          	sub    $0x18,%rsp
  405ac4:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  405aca:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  405ad0:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  405ad6:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  405ada:	e8 61 fd ff ff       	call   405840 <__truncsfhf2>
  405adf:	48 83 c4 18          	add    $0x18,%rsp
  405ae3:	c3                   	ret
  405ae4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  405aeb:	00 00 00 00 00 

0000000000405af0 <__gnu_h2f_ieee>:
  405af0:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  405af6:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  405afc:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  405b03:	00 
  405b04:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  405b0b:	00 
  405b0c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  405b11:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  405b16:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  405b1d:	00 
  405b1e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  405b25:	00 
  405b26:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  405b2d:	00 
  405b2e:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  405b35:	77 
  405b36:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  405b3d:	47 
  405b3e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405b43:	66 25 ff 7f          	and    $0x7fff,%ax
  405b47:	0f b7 c8             	movzwl %ax,%ecx
  405b4a:	c1 e1 0d             	shl    $0xd,%ecx
  405b4d:	b2 01                	mov    $0x1,%dl
  405b4f:	31 c0                	xor    %eax,%eax
  405b51:	f6 c2 01             	test   $0x1,%dl
  405b54:	0f 45 c1             	cmovne %ecx,%eax
  405b57:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405b5b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  405b61:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  405b67:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  405b6d:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405b73:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  405b78:	0f 93 c0             	setae  %al
  405b7b:	24 01                	and    $0x1,%al
  405b7d:	3c 00                	cmp    $0x0,%al
  405b7f:	74 0d                	je     405b8e <__gnu_h2f_ieee+0x9e>
  405b81:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405b85:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  405b8a:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405b8e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  405b93:	66 25 00 80          	and    $0x8000,%ax
  405b97:	0f b7 c8             	movzwl %ax,%ecx
  405b9a:	c1 e1 10             	shl    $0x10,%ecx
  405b9d:	b2 01                	mov    $0x1,%dl
  405b9f:	31 c0                	xor    %eax,%eax
  405ba1:	f6 c2 01             	test   $0x1,%dl
  405ba4:	0f 45 c1             	cmovne %ecx,%eax
  405ba7:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  405bab:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  405baf:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  405bb5:	c3                   	ret
  405bb6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  405bbd:	00 00 00 

0000000000405bc0 <__gnu_f2h_ieee>:
  405bc0:	50                   	push   %rax
  405bc1:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  405bc6:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  405bcb:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  405bd1:	e8 6a fc ff ff       	call   405840 <__truncsfhf2>
  405bd6:	58                   	pop    %rax
  405bd7:	c3                   	ret
  405bd8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  405bdf:	00 

0000000000405be0 <__extendhfsf2>:
  405be0:	50                   	push   %rax
  405be1:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  405be7:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  405bed:	0f 28 c8             	movaps %xmm0,%xmm1
  405bf0:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  405bf7:	00 
  405bf8:	e8 f3 fe ff ff       	call   405af0 <__gnu_h2f_ieee>
  405bfd:	58                   	pop    %rax
  405bfe:	c3                   	ret
  405bff:	90                   	nop

0000000000405c00 <__floattidf>:
  405c00:	53                   	push   %rbx
  405c01:	48 83 ec 10          	sub    $0x10,%rsp
  405c05:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405c0a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  405c0f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405c14:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405c19:	48 89 04 24          	mov    %rax,(%rsp)
  405c1d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  405c22:	48 09 c8             	or     %rcx,%rax
  405c25:	0f 94 c0             	sete   %al
  405c28:	24 01                	and    $0x1,%al
  405c2a:	3c 00                	cmp    $0x0,%al
  405c2c:	74 09                	je     405c37 <__floattidf+0x37>
  405c2e:	0f 57 c0             	xorps  %xmm0,%xmm0
  405c31:	48 83 c4 10          	add    $0x10,%rsp
  405c35:	5b                   	pop    %rbx
  405c36:	c3                   	ret
  405c37:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405c3c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405c41:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405c46:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405c4b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405c50:	48 c1 f8 3f          	sar    $0x3f,%rax
  405c54:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405c59:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  405c5e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  405c63:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  405c68:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  405c6d:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405c72:	48 31 d0             	xor    %rdx,%rax
  405c75:	48 31 f1             	xor    %rsi,%rcx
  405c78:	48 29 f1             	sub    %rsi,%rcx
  405c7b:	48 19 d0             	sbb    %rdx,%rax
  405c7e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  405c83:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  405c88:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  405c8d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  405c92:	48 0f bd c2          	bsr    %rdx,%rax
  405c96:	48 83 f0 3f          	xor    $0x3f,%rax
  405c9a:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  405c9f:	48 0f bd ce          	bsr    %rsi,%rcx
  405ca3:	48 83 f1 3f          	xor    $0x3f,%rcx
  405ca7:	48 83 c1 40          	add    $0x40,%rcx
  405cab:	48 85 d2             	test   %rdx,%rdx
  405cae:	48 0f 45 c8          	cmovne %rax,%rcx
  405cb2:	31 c0                	xor    %eax,%eax
  405cb4:	ba 80 00 00 00       	mov    $0x80,%edx
  405cb9:	48 29 ca             	sub    %rcx,%rdx
  405cbc:	48 89 c1             	mov    %rax,%rcx
  405cbf:	48 19 c9             	sbb    %rcx,%rcx
  405cc2:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405cc7:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405ccc:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  405cd0:	ff c9                	dec    %ecx
  405cd2:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  405cd6:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  405cdb:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405ce0:	ba 35 00 00 00       	mov    $0x35,%edx
  405ce5:	48 29 f2             	sub    %rsi,%rdx
  405ce8:	48 19 c8             	sbb    %rcx,%rax
  405ceb:	0f 9c c0             	setl   %al
  405cee:	24 01                	and    $0x1,%al
  405cf0:	3c 00                	cmp    $0x0,%al
  405cf2:	0f 84 c0 01 00 00    	je     405eb8 <__floattidf+0x2b8>
  405cf8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  405cfd:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  405d02:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405d07:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  405d0c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  405d11:	0f 28 0d 08 16 00 00 	movaps 0x1608(%rip),%xmm1        # 407320 <_IO_stdin_used+0x320>
  405d18:	66 0f ef c1          	pxor   %xmm1,%xmm0
  405d1c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  405d21:	74 17                	je     405d3a <__floattidf+0x13a>
  405d23:	eb 00                	jmp    405d25 <__floattidf+0x125>
  405d25:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  405d2a:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  405d2f:	48 83 f0 37          	xor    $0x37,%rax
  405d33:	48 09 c8             	or     %rcx,%rax
  405d36:	74 26                	je     405d5e <__floattidf+0x15e>
  405d38:	eb 29                	jmp    405d63 <__floattidf+0x163>
  405d3a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405d3f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405d44:	48 89 d0             	mov    %rdx,%rax
  405d47:	48 01 c0             	add    %rax,%rax
  405d4a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  405d4f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405d54:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405d59:	e9 d6 00 00 00       	jmp    405e34 <__floattidf+0x234>
  405d5e:	e9 d1 00 00 00       	jmp    405e34 <__floattidf+0x234>
  405d63:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405d68:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  405d6d:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  405d72:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  405d77:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  405d7c:	49 89 fb             	mov    %rdi,%r11
  405d7f:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  405d83:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  405d87:	44 88 db             	mov    %r11b,%bl
  405d8a:	88 d9                	mov    %bl,%cl
  405d8c:	49 89 f2             	mov    %rsi,%r10
  405d8f:	49 d3 ea             	shr    %cl,%r10
  405d92:	88 d9                	mov    %bl,%cl
  405d94:	49 89 d1             	mov    %rdx,%r9
  405d97:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  405d9b:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  405da0:	45 31 c0             	xor    %r8d,%r8d
  405da3:	f6 c3 40             	test   $0x40,%bl
  405da6:	4d 0f 45 ca          	cmovne %r10,%r9
  405daa:	4d 0f 45 d0          	cmovne %r8,%r10
  405dae:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405db5:	48 83 d8 00          	sbb    $0x0,%rax
  405db9:	4c 89 c0             	mov    %r8,%rax
  405dbc:	49 0f 42 c2          	cmovb  %r10,%rax
  405dc0:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  405dc5:	4c 89 c0             	mov    %r8,%rax
  405dc8:	49 0f 42 c1          	cmovb  %r9,%rax
  405dcc:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  405dd2:	49 29 fb             	sub    %rdi,%r11
  405dd5:	4c 89 c7             	mov    %r8,%rdi
  405dd8:	48 19 cf             	sbb    %rcx,%rdi
  405ddb:	45 88 d9             	mov    %r11b,%r9b
  405dde:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  405de5:	44 88 c9             	mov    %r9b,%cl
  405de8:	4c 89 d3             	mov    %r10,%rbx
  405deb:	48 d3 eb             	shr    %cl,%rbx
  405dee:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  405df3:	41 f6 c1 40          	test   $0x40,%r9b
  405df7:	49 89 d9             	mov    %rbx,%r9
  405dfa:	4d 0f 45 c8          	cmovne %r8,%r9
  405dfe:	4c 0f 45 d3          	cmovne %rbx,%r10
  405e02:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  405e09:	48 83 df 00          	sbb    $0x0,%rdi
  405e0d:	4c 89 c7             	mov    %r8,%rdi
  405e10:	49 0f 42 fa          	cmovb  %r10,%rdi
  405e14:	4d 0f 42 c1          	cmovb  %r9,%r8
  405e18:	4c 21 c6             	and    %r8,%rsi
  405e1b:	48 21 fa             	and    %rdi,%rdx
  405e1e:	48 09 f2             	or     %rsi,%rdx
  405e21:	0f 95 c2             	setne  %dl
  405e24:	0f b6 d2             	movzbl %dl,%edx
  405e27:	48 09 d0             	or     %rdx,%rax
  405e2a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405e2f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405e34:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405e39:	89 c1                	mov    %eax,%ecx
  405e3b:	83 e1 04             	and    $0x4,%ecx
  405e3e:	c1 e9 02             	shr    $0x2,%ecx
  405e41:	48 09 c8             	or     %rcx,%rax
  405e44:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405e49:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405e4e:	48 83 c0 01          	add    $0x1,%rax
  405e52:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  405e58:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405e5d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405e62:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405e67:	48 89 c8             	mov    %rcx,%rax
  405e6a:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  405e6f:	48 c1 f9 02          	sar    $0x2,%rcx
  405e73:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405e78:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405e7d:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  405e81:	24 20                	and    $0x20,%al
  405e83:	c0 e8 05             	shr    $0x5,%al
  405e86:	24 01                	and    $0x1,%al
  405e88:	3c 00                	cmp    $0x0,%al
  405e8a:	74 2a                	je     405eb6 <__floattidf+0x2b6>
  405e8c:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  405e91:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  405e96:	48 89 c8             	mov    %rcx,%rax
  405e99:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  405e9e:	48 d1 f9             	sar    $1,%rcx
  405ea1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405ea6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405eab:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  405eaf:	83 c0 01             	add    $0x1,%eax
  405eb2:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  405eb6:	eb 5c                	jmp    405f14 <__floattidf+0x314>
  405eb8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  405ebc:	b9 35 00 00 00       	mov    $0x35,%ecx
  405ec1:	29 c1                	sub    %eax,%ecx
  405ec3:	83 e1 7f             	and    $0x7f,%ecx
  405ec6:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  405eca:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  405ecf:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  405ed4:	40 88 cf             	mov    %cl,%dil
  405ed7:	40 88 f9             	mov    %dil,%cl
  405eda:	48 89 c2             	mov    %rax,%rdx
  405edd:	48 d3 e2             	shl    %cl,%rdx
  405ee0:	40 88 f9             	mov    %dil,%cl
  405ee3:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  405ee7:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  405eeb:	31 c0                	xor    %eax,%eax
  405eed:	40 f6 c7 40          	test   $0x40,%dil
  405ef1:	48 0f 45 f2          	cmovne %rdx,%rsi
  405ef5:	48 0f 45 d0          	cmovne %rax,%rdx
  405ef9:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  405eff:	48 89 c1             	mov    %rax,%rcx
  405f02:	48 0f 42 ce          	cmovb  %rsi,%rcx
  405f06:	48 0f 42 c2          	cmovb  %rdx,%rax
  405f0a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405f0f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405f14:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  405f1b:	00 00 
  405f1d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  405f21:	25 00 00 00 80       	and    $0x80000000,%eax
  405f26:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  405f2a:	c1 e1 14             	shl    $0x14,%ecx
  405f2d:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  405f33:	09 c8                	or     %ecx,%eax
  405f35:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  405f39:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  405f3f:	09 c8                	or     %ecx,%eax
  405f41:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  405f45:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  405f49:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  405f4d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  405f53:	48 83 c4 10          	add    $0x10,%rsp
  405f57:	5b                   	pop    %rbx
  405f58:	c3                   	ret
  405f59:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000405f60 <__floattidf_unsigned>:
  405f60:	53                   	push   %rbx
  405f61:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  405f66:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  405f6b:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  405f70:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  405f75:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  405f7a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  405f7f:	48 09 c8             	or     %rcx,%rax
  405f82:	0f 94 c0             	sete   %al
  405f85:	24 01                	and    $0x1,%al
  405f87:	3c 00                	cmp    $0x0,%al
  405f89:	74 05                	je     405f90 <__floattidf_unsigned+0x30>
  405f8b:	0f 57 c0             	xorps  %xmm0,%xmm0
  405f8e:	5b                   	pop    %rbx
  405f8f:	c3                   	ret
  405f90:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  405f95:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  405f9a:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  405f9f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  405fa4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  405fa9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  405fae:	48 0f bd c2          	bsr    %rdx,%rax
  405fb2:	48 83 f0 3f          	xor    $0x3f,%rax
  405fb6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  405fbb:	48 0f bd ce          	bsr    %rsi,%rcx
  405fbf:	48 83 f1 3f          	xor    $0x3f,%rcx
  405fc3:	48 83 c1 40          	add    $0x40,%rcx
  405fc7:	48 85 d2             	test   %rdx,%rdx
  405fca:	48 0f 45 c8          	cmovne %rax,%rcx
  405fce:	31 c0                	xor    %eax,%eax
  405fd0:	ba 80 00 00 00       	mov    $0x80,%edx
  405fd5:	48 29 ca             	sub    %rcx,%rdx
  405fd8:	48 89 c1             	mov    %rax,%rcx
  405fdb:	48 19 c9             	sbb    %rcx,%rcx
  405fde:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  405fe3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  405fe8:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  405fec:	ff c9                	dec    %ecx
  405fee:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  405ff2:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  405ff7:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  405ffc:	ba 35 00 00 00       	mov    $0x35,%edx
  406001:	48 29 f2             	sub    %rsi,%rdx
  406004:	48 19 c8             	sbb    %rcx,%rax
  406007:	0f 92 c0             	setb   %al
  40600a:	24 01                	and    $0x1,%al
  40600c:	3c 00                	cmp    $0x0,%al
  40600e:	0f 84 c0 01 00 00    	je     4061d4 <__floattidf_unsigned+0x274>
  406014:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406019:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40601e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406023:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  406028:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40602d:	0f 28 0d ec 12 00 00 	movaps 0x12ec(%rip),%xmm1        # 407320 <_IO_stdin_used+0x320>
  406034:	66 0f ef c1          	pxor   %xmm1,%xmm0
  406038:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  40603d:	74 17                	je     406056 <__floattidf_unsigned+0xf6>
  40603f:	eb 00                	jmp    406041 <__floattidf_unsigned+0xe1>
  406041:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  406046:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40604b:	48 83 f0 37          	xor    $0x37,%rax
  40604f:	48 09 c8             	or     %rcx,%rax
  406052:	74 26                	je     40607a <__floattidf_unsigned+0x11a>
  406054:	eb 29                	jmp    40607f <__floattidf_unsigned+0x11f>
  406056:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40605b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406060:	48 89 d0             	mov    %rdx,%rax
  406063:	48 01 c0             	add    %rax,%rax
  406066:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40606b:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  406070:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406075:	e9 d6 00 00 00       	jmp    406150 <__floattidf_unsigned+0x1f0>
  40607a:	e9 d1 00 00 00       	jmp    406150 <__floattidf_unsigned+0x1f0>
  40607f:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  406084:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  406089:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  40608e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406093:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  406098:	49 89 fb             	mov    %rdi,%r11
  40609b:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  40609f:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  4060a3:	44 88 db             	mov    %r11b,%bl
  4060a6:	88 d9                	mov    %bl,%cl
  4060a8:	49 89 f2             	mov    %rsi,%r10
  4060ab:	49 d3 ea             	shr    %cl,%r10
  4060ae:	88 d9                	mov    %bl,%cl
  4060b0:	49 89 d1             	mov    %rdx,%r9
  4060b3:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  4060b7:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  4060bc:	45 31 c0             	xor    %r8d,%r8d
  4060bf:	f6 c3 40             	test   $0x40,%bl
  4060c2:	4d 0f 45 ca          	cmovne %r10,%r9
  4060c6:	4d 0f 45 d0          	cmovne %r8,%r10
  4060ca:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  4060d1:	48 83 d8 00          	sbb    $0x0,%rax
  4060d5:	4c 89 c0             	mov    %r8,%rax
  4060d8:	49 0f 42 c2          	cmovb  %r10,%rax
  4060dc:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  4060e1:	4c 89 c0             	mov    %r8,%rax
  4060e4:	49 0f 42 c1          	cmovb  %r9,%rax
  4060e8:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  4060ee:	49 29 fb             	sub    %rdi,%r11
  4060f1:	4c 89 c7             	mov    %r8,%rdi
  4060f4:	48 19 cf             	sbb    %rcx,%rdi
  4060f7:	45 88 d9             	mov    %r11b,%r9b
  4060fa:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  406101:	44 88 c9             	mov    %r9b,%cl
  406104:	4c 89 d3             	mov    %r10,%rbx
  406107:	48 d3 eb             	shr    %cl,%rbx
  40610a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40610f:	41 f6 c1 40          	test   $0x40,%r9b
  406113:	49 89 d9             	mov    %rbx,%r9
  406116:	4d 0f 45 c8          	cmovne %r8,%r9
  40611a:	4c 0f 45 d3          	cmovne %rbx,%r10
  40611e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  406125:	48 83 df 00          	sbb    $0x0,%rdi
  406129:	4c 89 c7             	mov    %r8,%rdi
  40612c:	49 0f 42 fa          	cmovb  %r10,%rdi
  406130:	4d 0f 42 c1          	cmovb  %r9,%r8
  406134:	4c 21 c6             	and    %r8,%rsi
  406137:	48 21 fa             	and    %rdi,%rdx
  40613a:	48 09 f2             	or     %rsi,%rdx
  40613d:	0f 95 c2             	setne  %dl
  406140:	0f b6 d2             	movzbl %dl,%edx
  406143:	48 09 d0             	or     %rdx,%rax
  406146:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40614b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406150:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  406155:	89 c1                	mov    %eax,%ecx
  406157:	83 e1 04             	and    $0x4,%ecx
  40615a:	c1 e9 02             	shr    $0x2,%ecx
  40615d:	48 09 c8             	or     %rcx,%rax
  406160:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406165:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40616a:	48 83 c0 01          	add    $0x1,%rax
  40616e:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  406174:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406179:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40617e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406183:	48 89 c8             	mov    %rcx,%rax
  406186:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40618b:	48 c1 e9 02          	shr    $0x2,%rcx
  40618f:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  406194:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406199:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  40619d:	24 20                	and    $0x20,%al
  40619f:	c0 e8 05             	shr    $0x5,%al
  4061a2:	24 01                	and    $0x1,%al
  4061a4:	3c 00                	cmp    $0x0,%al
  4061a6:	74 2a                	je     4061d2 <__floattidf_unsigned+0x272>
  4061a8:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  4061ad:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4061b2:	48 89 c8             	mov    %rcx,%rax
  4061b5:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  4061ba:	48 d1 e9             	shr    $1,%rcx
  4061bd:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4061c2:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4061c7:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  4061cb:	83 c0 01             	add    $0x1,%eax
  4061ce:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  4061d2:	eb 6a                	jmp    40623e <__floattidf_unsigned+0x2de>
  4061d4:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  4061d9:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4061de:	31 c0                	xor    %eax,%eax
  4061e0:	bf 35 00 00 00       	mov    $0x35,%edi
  4061e5:	48 29 d7             	sub    %rdx,%rdi
  4061e8:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  4061ed:	48 19 c8             	sbb    %rcx,%rax
  4061f0:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  4061f5:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  4061fa:	41 88 f8             	mov    %dil,%r8b
  4061fd:	44 88 c1             	mov    %r8b,%cl
  406200:	4c 89 ce             	mov    %r9,%rsi
  406203:	48 d3 e6             	shl    %cl,%rsi
  406206:	44 88 c1             	mov    %r8b,%cl
  406209:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  40620d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  406212:	41 f6 c0 40          	test   $0x40,%r8b
  406216:	48 0f 45 d6          	cmovne %rsi,%rdx
  40621a:	48 0f 45 f1          	cmovne %rcx,%rsi
  40621e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  406225:	48 83 d8 00          	sbb    $0x0,%rax
  406229:	48 89 c8             	mov    %rcx,%rax
  40622c:	48 0f 42 c6          	cmovb  %rsi,%rax
  406230:	48 0f 42 ca          	cmovb  %rdx,%rcx
  406234:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  406239:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40623e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  406245:	00 00 
  406247:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  40624b:	c1 e2 14             	shl    $0x14,%edx
  40624e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  406252:	25 ff ff 0f 00       	and    $0xfffff,%eax
  406257:	89 c1                	mov    %eax,%ecx
  406259:	89 d0                	mov    %edx,%eax
  40625b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  406262:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  406266:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  40626a:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40626e:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  406274:	5b                   	pop    %rbx
  406275:	c3                   	ret
  406276:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40627d:	00 00 00 

0000000000406280 <__umodti3>:
  406280:	48 83 ec 58          	sub    $0x58,%rsp
  406284:	48 89 0c 24          	mov    %rcx,(%rsp)
  406288:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40628d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406292:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406297:	48 8b 0c 24          	mov    (%rsp),%rcx
  40629b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  4062a0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  4062a5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4062aa:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  4062af:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  4062b4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4062b9:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4062be:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  4062c3:	e8 78 b6 ff ff       	call   401940 <runtime::udivmod128>
  4062c8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4062cd:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4062d2:	48 83 c4 58          	add    $0x58,%rsp
  4062d6:	c3                   	ret
  4062d7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4062de:	00 00 

00000000004062e0 <__udivmodti4>:
  4062e0:	48 83 ec 58          	sub    $0x58,%rsp
  4062e4:	4c 89 04 24          	mov    %r8,(%rsp)
  4062e8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4062ed:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4062f2:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4062f7:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  4062fc:	4c 8b 04 24          	mov    (%rsp),%r8
  406300:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406305:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40630a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40630f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406314:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  406319:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40631e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  406323:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406328:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40632d:	e8 0e b6 ff ff       	call   401940 <runtime::udivmod128>
  406332:	48 83 c4 58          	add    $0x58,%rsp
  406336:	c3                   	ret
  406337:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40633e:	00 00 

0000000000406340 <__udivti3>:
  406340:	48 83 ec 48          	sub    $0x48,%rsp
  406344:	48 89 0c 24          	mov    %rcx,(%rsp)
  406348:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40634d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406352:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406357:	48 8b 0c 24          	mov    (%rsp),%rcx
  40635b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  406360:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  406365:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40636a:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40636f:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  406374:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  406379:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40637e:	31 c0                	xor    %eax,%eax
  406380:	41 89 c0             	mov    %eax,%r8d
  406383:	e8 58 ff ff ff       	call   4062e0 <__udivmodti4>
  406388:	48 83 c4 48          	add    $0x48,%rsp
  40638c:	c3                   	ret
  40638d:	0f 1f 00             	nopl   (%rax)

0000000000406390 <runtime::assert>:
  406390:	48 83 ec 48          	sub    $0x48,%rsp
  406394:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  406399:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40639e:	40 88 f8             	mov    %dil,%al
  4063a1:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  4063a5:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4063aa:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  4063af:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  4063b3:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4063b8:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4063bd:	88 44 24 47          	mov    %al,0x47(%rsp)
  4063c1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4063c6:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4063cb:	3c 00                	cmp    $0x0,%al
  4063cd:	75 19                	jne    4063e8 <runtime::assert+0x58>
  4063cf:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4063d4:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4063d9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4063de:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4063e3:	e8 18 0a 00 00       	call   406e00 <runtime::assert.internal-0>
  4063e8:	48 83 c4 48          	add    $0x48,%rsp
  4063ec:	c3                   	ret
  4063ed:	0f 1f 00             	nopl   (%rax)

00000000004063f0 <runtime::heap_allocator_proc.aligned_alloc-0>:
  4063f0:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  4063f7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  4063fc:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  406401:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  406406:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40640b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  406410:	44 88 c0             	mov    %r8b,%al
  406413:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  406417:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40641e:	00 
  40641f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  406424:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406429:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40642e:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  406433:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  406437:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40643c:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  406443:	00 
  406444:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40644b:	00 
  40644c:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  406453:	00 
  406454:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  40645b:	00 
  40645c:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  406463:	b9 08 00 00 00       	mov    $0x8,%ecx
  406468:	48 83 fe 08          	cmp    $0x8,%rsi
  40646c:	48 0f 4f ce          	cmovg  %rsi,%rcx
  406470:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  406477:	00 
  406478:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40647f:	00 
  406480:	48 83 e9 01          	sub    $0x1,%rcx
  406484:	48 83 c1 08          	add    $0x8,%rcx
  406488:	48 01 d1             	add    %rdx,%rcx
  40648b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  406492:	00 
  406493:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  40649a:	00 00 
  40649c:	48 83 f8 00          	cmp    $0x0,%rax
  4064a0:	0f 95 c1             	setne  %cl
  4064a3:	80 e1 01             	and    $0x1,%cl
  4064a6:	31 c0                	xor    %eax,%eax
  4064a8:	80 f9 00             	cmp    $0x0,%cl
  4064ab:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4064af:	74 17                	je     4064c8 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  4064b1:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4064b6:	48 83 f8 08          	cmp    $0x8,%rax
  4064ba:	0f 9f c0             	setg   %al
  4064bd:	24 01                	and    $0x1,%al
  4064bf:	3c 00                	cmp    $0x0,%al
  4064c1:	0f 95 c0             	setne  %al
  4064c4:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4064c8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4064cd:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  4064d1:	80 e1 01             	and    $0x1,%cl
  4064d4:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  4064d8:	48 83 f8 00          	cmp    $0x0,%rax
  4064dc:	0f 95 c0             	setne  %al
  4064df:	24 01                	and    $0x1,%al
  4064e1:	3c 00                	cmp    $0x0,%al
  4064e3:	74 2e                	je     406513 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  4064e5:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  4064ea:	75 27                	jne    406513 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  4064ec:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4064f1:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  4064f5:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4064fa:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  4064ff:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  406506:	00 
  406507:	e8 64 db ff ff       	call   404070 <runtime::heap_resize>
  40650c:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  406511:	eb 19                	jmp    40652c <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  406513:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  406517:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  40651e:	00 
  40651f:	0f b6 f0             	movzbl %al,%esi
  406522:	e8 19 db ff ff       	call   404040 <runtime::heap_alloc>
  406527:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40652c:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  406531:	48 83 c0 08          	add    $0x8,%rax
  406535:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40653a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40653f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  406544:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  406549:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40654e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  406553:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  40655a:	00 
  40655b:	48 83 e8 01          	sub    $0x1,%rax
  40655f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  406566:	00 
  406567:	48 83 e9 01          	sub    $0x1,%rcx
  40656b:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  40656f:	48 21 c8             	and    %rcx,%rax
  406572:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  406577:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  40657d:	0f 94 c0             	sete   %al
  406580:	24 01                	and    $0x1,%al
  406582:	3c 00                	cmp    $0x0,%al
  406584:	74 3c                	je     4065c2 <runtime::heap_allocator_proc.aligned_alloc-0+0x1d2>
  406586:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40658b:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406590:	e8 ab 00 00 00       	call   406640 <runtime::heap_allocator_proc.aligned_free-1>
  406595:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40659a:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  40659f:	e8 9c 00 00 00       	call   406640 <runtime::heap_allocator_proc.aligned_free-1>
  4065a4:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4065a9:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4065b0:	00 
  4065b1:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4065b8:	b0 01                	mov    $0x1,%al
  4065ba:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4065c1:	c3                   	ret
  4065c2:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4065c7:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4065cc:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4065d1:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  4065d6:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  4065da:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  4065df:	74 2f                	je     406610 <runtime::heap_allocator_proc.aligned_alloc-0+0x220>
  4065e1:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4065e6:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4065eb:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4065f0:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  4065f5:	48 39 d0             	cmp    %rdx,%rax
  4065f8:	48 0f 4c d0          	cmovl  %rax,%rdx
  4065fc:	e8 3f d0 ff ff       	call   403640 <runtime::mem_copy_non_overlapping>
  406601:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  406606:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40660b:	e8 30 00 00 00       	call   406640 <runtime::heap_allocator_proc.aligned_free-1>
  406610:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406615:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  40661a:	e8 c1 bf ff ff       	call   4025e0 <runtime::[internal.odin]::byte_slice>
  40661f:	48 89 c1             	mov    %rax,%rcx
  406622:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406627:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40662b:	48 89 08             	mov    %rcx,(%rax)
  40662e:	31 c0                	xor    %eax,%eax
  406630:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  406637:	c3                   	ret
  406638:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40663f:	00 

0000000000406640 <runtime::heap_allocator_proc.aligned_free-1>:
  406640:	48 83 ec 18          	sub    $0x18,%rsp
  406644:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  406649:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40664e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406653:	48 83 f8 00          	cmp    $0x0,%rax
  406657:	0f 95 c0             	setne  %al
  40665a:	24 01                	and    $0x1,%al
  40665c:	3c 00                	cmp    $0x0,%al
  40665e:	74 0e                	je     40666e <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  406660:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406665:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  406669:	e8 32 da ff ff       	call   4040a0 <runtime::heap_free>
  40666e:	48 83 c4 18          	add    $0x18,%rsp
  406672:	c3                   	ret
  406673:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40667a:	84 00 00 00 00 00 

0000000000406680 <runtime::heap_allocator_proc.aligned_resize-2>:
  406680:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  406687:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40668c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406691:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  406696:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40669b:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  4066a0:	44 88 c0             	mov    %r8b,%al
  4066a3:	88 44 24 57          	mov    %al,0x57(%rsp)
  4066a7:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4066ae:	00 
  4066af:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4066b4:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4066b9:	8a 4c 24 57          	mov    0x57(%rsp),%cl
  4066bd:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4066c2:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  4066c7:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4066cc:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4066d3:	00 
  4066d4:	48 89 bc 24 08 01 00 	mov    %rdi,0x108(%rsp)
  4066db:	00 
  4066dc:	48 89 b4 24 00 01 00 	mov    %rsi,0x100(%rsp)
  4066e3:	00 
  4066e4:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  4066eb:	00 
  4066ec:	88 8c 24 f7 00 00 00 	mov    %cl,0xf7(%rsp)
  4066f3:	0f 57 c0             	xorps  %xmm0,%xmm0
  4066f6:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  4066fd:	00 
  4066fe:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  406705:	00 
  406706:	48 83 f8 00          	cmp    $0x0,%rax
  40670a:	0f 94 c0             	sete   %al
  40670d:	24 01                	and    $0x1,%al
  40670f:	3c 00                	cmp    $0x0,%al
  406711:	0f 84 80 00 00 00    	je     406797 <runtime::heap_allocator_proc.aligned_resize-2+0x117>
  406717:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40671c:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406721:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406726:	8a 44 24 57          	mov    0x57(%rsp),%al
  40672a:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  40672f:	0f 57 c0             	xorps  %xmm0,%xmm0
  406732:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  406739:	00 
  40673a:	48 89 e2             	mov    %rsp,%rdx
  40673d:	4c 89 02             	mov    %r8,(%rdx)
  406740:	44 0f b6 c0          	movzbl %al,%r8d
  406744:	31 c0                	xor    %eax,%eax
  406746:	89 c2                	mov    %eax,%edx
  406748:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  40674f:	00 
  406750:	e8 9b fc ff ff       	call   4063f0 <runtime::heap_allocator_proc.aligned_alloc-0>
  406755:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40675a:	40 88 c7             	mov    %al,%dil
  40675d:	40 88 f8             	mov    %dil,%al
  406760:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  406767:	00 
  406768:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  40676f:	00 
  406770:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406777:	00 
  406778:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40677f:	00 
  406780:	40 88 bc 24 df 00 00 	mov    %dil,0xdf(%rsp)
  406787:	00 
  406788:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40678c:	48 89 11             	mov    %rdx,(%rcx)
  40678f:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406796:	c3                   	ret
  406797:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40679c:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  4067a1:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4067a6:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  4067ab:	8a 44 24 57          	mov    0x57(%rsp),%al
  4067af:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  4067b4:	0f 57 c0             	xorps  %xmm0,%xmm0
  4067b7:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  4067be:	00 
  4067bf:	49 89 e0             	mov    %rsp,%r8
  4067c2:	4d 89 08             	mov    %r9,(%r8)
  4067c5:	44 0f b6 c0          	movzbl %al,%r8d
  4067c9:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  4067d0:	00 
  4067d1:	e8 1a fc ff ff       	call   4063f0 <runtime::heap_allocator_proc.aligned_alloc-0>
  4067d6:	88 44 24 17          	mov    %al,0x17(%rsp)
  4067da:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  4067e1:	00 
  4067e2:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4067e7:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  4067ee:	00 
  4067ef:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4067f4:	3c 00                	cmp    $0x0,%al
  4067f6:	74 4d                	je     406845 <runtime::heap_allocator_proc.aligned_resize-2+0x1c5>
  4067f8:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4067fd:	8a 44 24 17          	mov    0x17(%rsp),%al
  406801:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406808:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40680f:	00 
  406810:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406817:	00 
  406818:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40681f:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406826:	00 
  406827:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40682e:	00 
  40682f:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406836:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40683a:	48 89 11             	mov    %rdx,(%rcx)
  40683d:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406844:	c3                   	ret
  406845:	8a 44 24 57          	mov    0x57(%rsp),%al
  406849:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40684e:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406853:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40685a:	00 
  40685b:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  406862:	00 
  406863:	3c 00                	cmp    $0x0,%al
  406865:	0f 84 87 00 00 00    	je     4068f2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  40686b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406870:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  406875:	48 39 c8             	cmp    %rcx,%rax
  406878:	0f 9f c0             	setg   %al
  40687b:	24 01                	and    $0x1,%al
  40687d:	3c 00                	cmp    $0x0,%al
  40687f:	74 71                	je     4068f2 <runtime::heap_allocator_proc.aligned_resize-2+0x272>
  406881:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  406886:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  40688d:	00 
  40688e:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406893:	48 89 e0             	mov    %rsp,%rax
  406896:	4c 89 08             	mov    %r9,(%rax)
  406899:	bf 97 72 40 00       	mov    $0x407297,%edi
  40689e:	be 30 00 00 00       	mov    $0x30,%esi
  4068a3:	ba 4b 00 00 00       	mov    $0x4b,%edx
  4068a8:	b9 25 00 00 00       	mov    $0x25,%ecx
  4068ad:	e8 1e ce ff ff       	call   4036d0 <runtime::slice_expr_error_lo_hi>
  4068b2:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4068b7:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4068bc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4068c1:	48 89 c6             	mov    %rax,%rsi
  4068c4:	48 03 b4 24 e0 00 00 	add    0xe0(%rsp),%rsi
  4068cb:	00 
  4068cc:	48 29 c1             	sub    %rax,%rcx
  4068cf:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  4068d4:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  4068d9:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  4068de:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  4068e3:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  4068e8:	48 29 c2             	sub    %rax,%rdx
  4068eb:	31 f6                	xor    %esi,%esi
  4068ed:	e8 4e a7 ff ff       	call   401040 <memset@plt>
  4068f2:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4068f7:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  4068fe:	00 
  4068ff:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  406906:	00 
  406907:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  40690e:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  406915:	00 
  406916:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40691d:	00 
  40691e:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  406925:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  406929:	48 89 11             	mov    %rdx,(%rcx)
  40692c:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  406933:	c3                   	ret
  406934:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40693b:	00 00 00 00 00 

0000000000406940 <runtime::bounds_check_error.handle_error-0>:
  406940:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  406947:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40694c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406951:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406955:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406959:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40695e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406963:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406968:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  40696d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406971:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406975:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40697a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40697f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  406984:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40698b:	00 
  40698c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  406990:	89 44 24 70          	mov    %eax,0x70(%rsp)
  406994:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  406999:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40699e:	0f 57 c0             	xorps  %xmm0,%xmm0
  4069a1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4069a6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4069ab:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4069b2:	00 00 
  4069b4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  4069b9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4069be:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4069c5:	00 00 
  4069c7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4069cc:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4069d1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  4069d5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  4069d9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  4069de:	e8 cd da ff ff       	call   4044b0 <runtime::print_caller_location>
  4069e3:	bf c8 72 40 00       	mov    $0x4072c8,%edi
  4069e8:	be 07 00 00 00       	mov    $0x7,%esi
  4069ed:	e8 9e d2 ff ff       	call   403c90 <runtime::print_string>
  4069f2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4069f7:	e8 b4 d8 ff ff       	call   4042b0 <runtime::print_i64>
  4069fc:	bf 13 72 40 00       	mov    $0x407213,%edi
  406a01:	be 15 00 00 00       	mov    $0x15,%esi
  406a06:	e8 85 d2 ff ff       	call   403c90 <runtime::print_string>
  406a0b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406a10:	e8 9b d8 ff ff       	call   4042b0 <runtime::print_i64>
  406a15:	bf 0a 00 00 00       	mov    $0xa,%edi
  406a1a:	e8 a1 d4 ff ff       	call   403ec0 <runtime::print_byte>
  406a1f:	e8 ec ae ff ff       	call   401910 <runtime::bounds_trap>
  406a24:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  406a2b:	00 00 00 00 00 

0000000000406a30 <runtime::default_random_generator_proc.read_u64-0>:
  406a30:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  406a35:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406a3a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406a3f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406a44:	48 8b 00             	mov    (%rax),%rax
  406a47:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406a4c:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  406a51:	48 b9 2d 7f 95 4c 2d 	movabs $0x5851f42d4c957f2d,%rcx
  406a58:	f4 51 58 
  406a5b:	48 0f af 4c 24 f0    	imul   -0x10(%rsp),%rcx
  406a61:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406a66:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  406a6a:	48 83 ca 01          	or     $0x1,%rdx
  406a6e:	48 01 d1             	add    %rdx,%rcx
  406a71:	48 89 08             	mov    %rcx,(%rax)
  406a74:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406a79:	48 c1 e9 3b          	shr    $0x3b,%rcx
  406a7d:	b2 01                	mov    $0x1,%dl
  406a7f:	31 c0                	xor    %eax,%eax
  406a81:	f6 c2 01             	test   $0x1,%dl
  406a84:	48 0f 45 c1          	cmovne %rcx,%rax
  406a88:	48 83 c0 05          	add    $0x5,%rax
  406a8c:	48 33 44 24 f0       	xor    -0x10(%rsp),%rax
  406a91:	48 b9 d9 f2 8e 10 02 	movabs $0xaef17502108ef2d9,%rcx
  406a98:	75 f1 ae 
  406a9b:	48 0f af c1          	imul   %rcx,%rax
  406a9f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406aa4:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  406aa9:	48 c1 e9 3b          	shr    $0x3b,%rcx
  406aad:	b2 01                	mov    $0x1,%dl
  406aaf:	31 c0                	xor    %eax,%eax
  406ab1:	f6 c2 01             	test   $0x1,%dl
  406ab4:	48 0f 45 c1          	cmovne %rcx,%rax
  406ab8:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406abd:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406ac2:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  406ac7:	48 89 d1             	mov    %rdx,%rcx
  406aca:	48 d3 e8             	shr    %cl,%rax
  406acd:	48 89 c1             	mov    %rax,%rcx
  406ad0:	31 c0                	xor    %eax,%eax
  406ad2:	48 83 fa 40          	cmp    $0x40,%rdx
  406ad6:	48 0f 42 c1          	cmovb  %rcx,%rax
  406ada:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  406adf:	31 c9                	xor    %ecx,%ecx
  406ae1:	89 ce                	mov    %ecx,%esi
  406ae3:	48 2b 74 24 e0       	sub    -0x20(%rsp),%rsi
  406ae8:	48 83 e6 3f          	and    $0x3f,%rsi
  406aec:	48 89 f1             	mov    %rsi,%rcx
  406aef:	48 d3 e2             	shl    %cl,%rdx
  406af2:	31 c9                	xor    %ecx,%ecx
  406af4:	48 83 fe 40          	cmp    $0x40,%rsi
  406af8:	48 0f 42 ca          	cmovb  %rdx,%rcx
  406afc:	48 09 c8             	or     %rcx,%rax
  406aff:	c3                   	ret

0000000000406b00 <runtime::default_random_generator_proc.init-1>:
  406b00:	48 83 ec 28          	sub    $0x28,%rsp
  406b04:	48 89 3c 24          	mov    %rdi,(%rsp)
  406b08:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  406b0d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406b12:	48 8b 0c 24          	mov    (%rsp),%rcx
  406b16:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  406b1b:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  406b20:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406b25:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
  406b2b:	0f 94 c0             	sete   %al
  406b2e:	24 01                	and    $0x1,%al
  406b30:	3c 00                	cmp    $0x0,%al
  406b32:	74 0e                	je     406b42 <runtime::default_random_generator_proc.init-1+0x42>
  406b34:	0f 31                	rdtsc
  406b36:	48 c1 e2 20          	shl    $0x20,%rdx
  406b3a:	48 09 d0             	or     %rdx,%rax
  406b3d:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406b42:	48 8b 3c 24          	mov    (%rsp),%rdi
  406b46:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406b4b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  406b52:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406b57:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  406b5c:	48 d1 e2             	shl    $1,%rdx
  406b5f:	40 b6 01             	mov    $0x1,%sil
  406b62:	31 c9                	xor    %ecx,%ecx
  406b64:	40 f6 c6 01          	test   $0x1,%sil
  406b68:	48 0f 45 ca          	cmovne %rdx,%rcx
  406b6c:	48 83 c9 01          	or     $0x1,%rcx
  406b70:	48 89 48 08          	mov    %rcx,0x8(%rax)
  406b74:	e8 b7 fe ff ff       	call   406a30 <runtime::default_random_generator_proc.read_u64-0>
  406b79:	48 8b 3c 24          	mov    (%rsp),%rdi
  406b7d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406b82:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406b87:	48 03 08             	add    (%rax),%rcx
  406b8a:	48 89 08             	mov    %rcx,(%rax)
  406b8d:	e8 9e fe ff ff       	call   406a30 <runtime::default_random_generator_proc.read_u64-0>
  406b92:	48 83 c4 28          	add    $0x28,%rsp
  406b96:	c3                   	ret
  406b97:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  406b9e:	00 00 

0000000000406ba0 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  406ba0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  406ba5:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  406baa:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406baf:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  406bb4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  406bb9:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  406bbe:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  406bc5:	00 00 
  406bc7:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  406bcc:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  406bd0:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  406bd5:	48 03 4a 20          	add    0x20(%rdx),%rcx
  406bd9:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  406bde:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  406be3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406be8:	48 83 e8 01          	sub    $0x1,%rax
  406bec:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  406bf1:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406bf6:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  406bfb:	48 83 f8 00          	cmp    $0x0,%rax
  406bff:	0f 95 c0             	setne  %al
  406c02:	24 01                	and    $0x1,%al
  406c04:	3c 00                	cmp    $0x0,%al
  406c06:	74 17                	je     406c1f <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  406c08:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406c0d:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406c12:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  406c17:	48 29 c8             	sub    %rcx,%rax
  406c1a:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406c1f:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406c24:	c3                   	ret
  406c25:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  406c2c:	00 00 00 00 

0000000000406c30 <runtime::arena_alloc.align_forward_uint-0>:
  406c30:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  406c35:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  406c3a:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  406c3f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406c44:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406c49:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  406c4e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406c53:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406c58:	48 83 e9 01          	sub    $0x1,%rcx
  406c5c:	48 21 c8             	and    %rcx,%rax
  406c5f:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  406c64:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  406c6a:	0f 95 c0             	setne  %al
  406c6d:	24 01                	and    $0x1,%al
  406c6f:	3c 00                	cmp    $0x0,%al
  406c71:	74 14                	je     406c87 <runtime::arena_alloc.align_forward_uint-0+0x57>
  406c73:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406c78:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  406c7d:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  406c82:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  406c87:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406c8c:	c3                   	ret
  406c8d:	0f 1f 00             	nopl   (%rax)

0000000000406c90 <runtime::matrix_bounds_check_error.handle_error-0>:
  406c90:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  406c97:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  406c9c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  406ca1:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  406ca5:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  406ca9:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  406cae:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  406cb3:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  406cba:	00 
  406cbb:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406cc0:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  406cc7:	00 
  406cc8:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406ccd:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406cd2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  406cd7:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  406cdc:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  406ce1:	8b 44 24 18          	mov    0x18(%rsp),%eax
  406ce5:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  406ce9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  406cee:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  406cf3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  406cfa:	00 
  406cfb:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  406d02:	00 
  406d03:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  406d0a:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  406d11:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  406d18:	00 
  406d19:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  406d20:	00 
  406d21:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  406d26:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  406d2b:	0f 57 c0             	xorps  %xmm0,%xmm0
  406d2e:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406d33:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406d38:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406d3f:	00 00 
  406d41:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  406d46:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  406d4b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  406d52:	00 00 
  406d54:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  406d59:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  406d5e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  406d62:	89 44 24 54          	mov    %eax,0x54(%rsp)
  406d66:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  406d6b:	e8 40 d7 ff ff       	call   4044b0 <runtime::print_caller_location>
  406d70:	bf d0 72 40 00       	mov    $0x4072d0,%edi
  406d75:	be 11 00 00 00       	mov    $0x11,%esi
  406d7a:	e8 11 cf ff ff       	call   403c90 <runtime::print_string>
  406d7f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  406d84:	e8 27 d5 ff ff       	call   4042b0 <runtime::print_i64>
  406d89:	bf e2 72 40 00       	mov    $0x4072e2,%edi
  406d8e:	be 02 00 00 00       	mov    $0x2,%esi
  406d93:	e8 f8 ce ff ff       	call   403c90 <runtime::print_string>
  406d98:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  406d9d:	e8 0e d5 ff ff       	call   4042b0 <runtime::print_i64>
  406da2:	bf e5 72 40 00       	mov    $0x4072e5,%edi
  406da7:	be 16 00 00 00       	mov    $0x16,%esi
  406dac:	e8 df ce ff ff       	call   403c90 <runtime::print_string>
  406db1:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  406db6:	e8 f5 d4 ff ff       	call   4042b0 <runtime::print_i64>
  406dbb:	bf fc 72 40 00       	mov    $0x4072fc,%edi
  406dc0:	be 06 00 00 00       	mov    $0x6,%esi
  406dc5:	e8 c6 ce ff ff       	call   403c90 <runtime::print_string>
  406dca:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406dcf:	e8 dc d4 ff ff       	call   4042b0 <runtime::print_i64>
  406dd4:	bf 03 73 40 00       	mov    $0x407303,%edi
  406dd9:	be 01 00 00 00       	mov    $0x1,%esi
  406dde:	e8 ad ce ff ff       	call   403c90 <runtime::print_string>
  406de3:	bf 0a 00 00 00       	mov    $0xa,%edi
  406de8:	e8 d3 d0 ff ff       	call   403ec0 <runtime::print_byte>
  406ded:	e8 1e ab ff ff       	call   401910 <runtime::bounds_trap>
  406df2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  406df9:	1f 84 00 00 00 00 00 

0000000000406e00 <runtime::assert.internal-0>:
  406e00:	48 83 ec 38          	sub    $0x38,%rsp
  406e04:	48 89 0c 24          	mov    %rcx,(%rsp)
  406e08:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  406e0d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  406e12:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406e17:	48 8b 04 24          	mov    (%rsp),%rax
  406e1b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406e20:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406e25:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  406e2a:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  406e2f:	48 8b 40 20          	mov    0x20(%rax),%rax
  406e33:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406e38:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  406e3e:	0f 94 c0             	sete   %al
  406e41:	24 01                	and    $0x1,%al
  406e43:	3c 00                	cmp    $0x0,%al
  406e45:	74 0c                	je     406e53 <runtime::assert.internal-0+0x53>
  406e47:	48 c7 c0 40 57 40 00 	mov    $0x405740,%rax
  406e4e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  406e53:	4c 8b 0c 24          	mov    (%rsp),%r9
  406e57:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  406e5c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406e61:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406e66:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  406e6b:	bf 05 73 40 00       	mov    $0x407305,%edi
  406e70:	be 11 00 00 00       	mov    $0x11,%esi
  406e75:	ff d0                	call   *%rax
  406e77:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  406e7e:	00 00 

0000000000406e80 <__$startup_runtime>:
  406e80:	eb 00                	jmp    406e82 <__$startup_runtime+0x2>
  406e82:	c3                   	ret
  406e83:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  406e8a:	84 00 00 00 00 00 

0000000000406e90 <__$cleanup_runtime>:
  406e90:	50                   	push   %rax
  406e91:	48 89 3c 24          	mov    %rdi,(%rsp)
  406e95:	eb 00                	jmp    406e97 <__$cleanup_runtime+0x7>
  406e97:	48 8b 3c 24          	mov    (%rsp),%rdi
  406e9b:	e8 40 aa ff ff       	call   4018e0 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  406ea0:	58                   	pop    %rax
  406ea1:	c3                   	ret

Disassembly of section .fini:

0000000000406ea4 <_fini>:
  406ea4:	f3 0f 1e fa          	endbr64
  406ea8:	48 83 ec 08          	sub    $0x8,%rsp
  406eac:	48 83 c4 08          	add    $0x8,%rsp
  406eb0:	c3                   	ret
