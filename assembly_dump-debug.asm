
/home/khalid/Documents/GitHub/Journey_ECS/main-debug.bin:     file format elf64-x86-64


Disassembly of section .init:

0000000000401000 <_init>:
  401000:	f3 0f 1e fa          	endbr64
  401004:	48 83 ec 08          	sub    $0x8,%rsp
  401008:	48 8b 05 c9 2f 01 00 	mov    0x12fc9(%rip),%rax        # 413fd8 <__gmon_start__>
  40100f:	48 85 c0             	test   %rax,%rax
  401012:	74 02                	je     401016 <_init+0x16>
  401014:	ff d0                	call   *%rax
  401016:	48 83 c4 08          	add    $0x8,%rsp
  40101a:	c3                   	ret

Disassembly of section .plt:

0000000000401020 <free@plt-0x10>:
  401020:	ff 35 ca 2f 01 00    	push   0x12fca(%rip)        # 413ff0 <_GLOBAL_OFFSET_TABLE_+0x8>
  401026:	ff 25 cc 2f 01 00    	jmp    *0x12fcc(%rip)        # 413ff8 <_GLOBAL_OFFSET_TABLE_+0x10>
  40102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401030 <free@plt>:
  401030:	ff 25 ca 2f 01 00    	jmp    *0x12fca(%rip)        # 414000 <free@GLIBC_2.2.5>
  401036:	68 00 00 00 00       	push   $0x0
  40103b:	e9 e0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401040 <memset@plt>:
  401040:	ff 25 c2 2f 01 00    	jmp    *0x12fc2(%rip)        # 414008 <memset@GLIBC_2.2.5>
  401046:	68 01 00 00 00       	push   $0x1
  40104b:	e9 d0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401050 <calloc@plt>:
  401050:	ff 25 ba 2f 01 00    	jmp    *0x12fba(%rip)        # 414010 <calloc@GLIBC_2.2.5>
  401056:	68 02 00 00 00       	push   $0x2
  40105b:	e9 c0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401060 <memcpy@plt>:
  401060:	ff 25 b2 2f 01 00    	jmp    *0x12fb2(%rip)        # 414018 <memcpy@GLIBC_2.14>
  401066:	68 03 00 00 00       	push   $0x3
  40106b:	e9 b0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401070 <malloc@plt>:
  401070:	ff 25 aa 2f 01 00    	jmp    *0x12faa(%rip)        # 414020 <malloc@GLIBC_2.2.5>
  401076:	68 04 00 00 00       	push   $0x4
  40107b:	e9 a0 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401080 <realloc@plt>:
  401080:	ff 25 a2 2f 01 00    	jmp    *0x12fa2(%rip)        # 414028 <realloc@GLIBC_2.2.5>
  401086:	68 05 00 00 00       	push   $0x5
  40108b:	e9 90 ff ff ff       	jmp    401020 <_init+0x20>

0000000000401090 <memmove@plt>:
  401090:	ff 25 9a 2f 01 00    	jmp    *0x12f9a(%rip)        # 414030 <memmove@GLIBC_2.2.5>
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
  4010b8:	48 c7 c7 50 39 40 00 	mov    $0x403950,%rdi
  4010bf:	ff 15 03 2f 01 00    	call   *0x12f03(%rip)        # 413fc8 <__libc_start_main@GLIBC_2.34>
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
  4010e0:	b8 68 40 41 00       	mov    $0x414068,%eax
  4010e5:	48 3d 68 40 41 00    	cmp    $0x414068,%rax
  4010eb:	74 13                	je     401100 <deregister_tm_clones+0x20>
  4010ed:	48 8b 05 dc 2e 01 00 	mov    0x12edc(%rip),%rax        # 413fd0 <_ITM_deregisterTMCloneTable>
  4010f4:	48 85 c0             	test   %rax,%rax
  4010f7:	74 07                	je     401100 <deregister_tm_clones+0x20>
  4010f9:	bf 68 40 41 00       	mov    $0x414068,%edi
  4010fe:	ff e0                	jmp    *%rax
  401100:	c3                   	ret
  401101:	0f 1f 40 00          	nopl   0x0(%rax)
  401105:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40110c:	00 00 00 00 

0000000000401110 <register_tm_clones>:
  401110:	be 68 40 41 00       	mov    $0x414068,%esi
  401115:	48 81 ee 68 40 41 00 	sub    $0x414068,%rsi
  40111c:	48 89 f0             	mov    %rsi,%rax
  40111f:	48 c1 ee 3f          	shr    $0x3f,%rsi
  401123:	48 c1 f8 03          	sar    $0x3,%rax
  401127:	48 01 c6             	add    %rax,%rsi
  40112a:	48 d1 fe             	sar    $1,%rsi
  40112d:	74 19                	je     401148 <register_tm_clones+0x38>
  40112f:	48 8b 05 aa 2e 01 00 	mov    0x12eaa(%rip),%rax        # 413fe0 <_ITM_registerTMCloneTable>
  401136:	48 85 c0             	test   %rax,%rax
  401139:	74 0d                	je     401148 <register_tm_clones+0x38>
  40113b:	bf 68 40 41 00       	mov    $0x414068,%edi
  401140:	ff e0                	jmp    *%rax
  401142:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
  401148:	c3                   	ret
  401149:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401150 <__do_global_dtors_aux>:
  401150:	f3 0f 1e fa          	endbr64
  401154:	80 3d 0d 2f 01 00 00 	cmpb   $0x0,0x12f0d(%rip)        # 414068 <__TMC_END__>
  40115b:	75 13                	jne    401170 <__do_global_dtors_aux+0x20>
  40115d:	55                   	push   %rbp
  40115e:	48 89 e5             	mov    %rsp,%rbp
  401161:	e8 7a ff ff ff       	call   4010e0 <deregister_tm_clones>
  401166:	c6 05 fb 2e 01 00 01 	movb   $0x1,0x12efb(%rip)        # 414068 <__TMC_END__>
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

0000000000401190 <runtime::copy_slice_raw>:
  401190:	48 83 ec 58          	sub    $0x58,%rsp
  401194:	48 89 3c 24          	mov    %rdi,(%rsp)
  401198:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40119d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  4011a2:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  4011a7:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  4011ac:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4011b1:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4011b6:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4011bb:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011c0:	48 8b 3c 24          	mov    (%rsp),%rdi
  4011c4:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  4011c9:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  4011ce:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4011d3:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4011d8:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4011dd:	48 39 c1             	cmp    %rax,%rcx
  4011e0:	48 0f 4c c1          	cmovl  %rcx,%rax
  4011e4:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4011e9:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
  4011ef:	0f 9f c0             	setg   %al
  4011f2:	24 01                	and    $0x1,%al
  4011f4:	3c 00                	cmp    $0x0,%al
  4011f6:	74 19                	je     401211 <runtime::copy_slice_raw+0x81>
  4011f8:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  4011fd:	48 8b 3c 24          	mov    (%rsp),%rdi
  401201:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  401206:	48 0f af 54 24 28    	imul   0x28(%rsp),%rdx
  40120c:	e8 7f fe ff ff       	call   401090 <memmove@plt>
  401211:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401216:	48 83 c4 58          	add    $0x58,%rsp
  40121a:	c3                   	ret
  40121b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401220 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>:
  401220:	48 83 ec 48          	sub    $0x48,%rsp
  401224:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  401229:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40122e:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  401233:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  401238:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40123d:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401242:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401247:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40124c:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  401251:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  401256:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40125b:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  401260:	41 b8 01 00 00 00    	mov    $0x1,%r8d
  401266:	e8 25 ff ff ff       	call   401190 <runtime::copy_slice_raw>
  40126b:	48 83 c4 48          	add    $0x48,%rsp
  40126f:	c3                   	ret

0000000000401270 <runtime::copy_from_string:proc"contextless"(dst:[]u8,src:string)->(:int)>:
  401270:	48 83 ec 48          	sub    $0x48,%rsp
  401274:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  401279:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40127e:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  401283:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  401288:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40128d:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401292:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401297:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40129c:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4012a1:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4012a6:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4012ab:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  4012b0:	41 b8 01 00 00 00    	mov    $0x1,%r8d
  4012b6:	e8 d5 fe ff ff       	call   401190 <runtime::copy_slice_raw>
  4012bb:	48 83 c4 48          	add    $0x48,%rsp
  4012bf:	c3                   	ret

00000000004012c0 <runtime::delete_string>:
  4012c0:	48 83 ec 68          	sub    $0x68,%rsp
  4012c4:	4c 89 0c 24          	mov    %r9,(%rsp)
  4012c8:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  4012cd:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4012d2:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4012d7:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4012dc:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4012e1:	4c 8b 0c 24          	mov    (%rsp),%r9
  4012e5:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  4012ea:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4012ef:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4012f4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4012f9:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4012fe:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  401303:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  401308:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40130d:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401312:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401317:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40131c:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  401321:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401326:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40132b:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  401330:	e8 1b b6 00 00       	call   40c950 <runtime::mem_free_with_size>
  401335:	48 83 c4 68          	add    $0x68,%rsp
  401339:	c3                   	ret
  40133a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401340 <runtime::delete_slice:proc(array:[]u8,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>:
  401340:	48 83 ec 68          	sub    $0x68,%rsp
  401344:	4c 89 0c 24          	mov    %r9,(%rsp)
  401348:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40134d:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  401352:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  401357:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40135c:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  401361:	4c 8b 0c 24          	mov    (%rsp),%r9
  401365:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40136a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40136f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401374:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401379:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40137e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  401383:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  401388:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40138d:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401392:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401397:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40139c:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4013a1:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4013a6:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4013ab:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4013b0:	e8 9b b5 00 00       	call   40c950 <runtime::mem_free_with_size>
  4013b5:	48 83 c4 68          	add    $0x68,%rsp
  4013b9:	c3                   	ret
  4013ba:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004013c0 <runtime::delete_slice:proc(array:[]string,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>:
  4013c0:	48 83 ec 68          	sub    $0x68,%rsp
  4013c4:	4c 89 0c 24          	mov    %r9,(%rsp)
  4013c8:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  4013cd:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4013d2:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4013d7:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4013dc:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4013e1:	4c 8b 0c 24          	mov    (%rsp),%r9
  4013e5:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  4013ea:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4013ef:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4013f4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4013f9:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4013fe:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  401403:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  401408:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40140d:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401412:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401417:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40141c:	48 c1 e6 04          	shl    $0x4,%rsi
  401420:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  401425:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40142a:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40142f:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  401434:	e8 17 b5 00 00       	call   40c950 <runtime::mem_free_with_size>
  401439:	48 83 c4 68          	add    $0x68,%rsp
  40143d:	c3                   	ret
  40143e:	66 90                	xchg   %ax,%ax

0000000000401440 <runtime::_make_aligned_type_erased>:
  401440:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  401447:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40144c:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  401451:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  401456:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40145b:	4c 89 4c 24 38       	mov    %r9,0x38(%rsp)
  401460:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  401465:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  40146c:	00 
  40146d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  401472:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  401479:	00 
  40147a:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40147f:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  401484:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401489:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40148e:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  401493:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  401498:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40149d:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  4014a2:	4c 89 8c 24 d0 00 00 	mov    %r9,0xd0(%rsp)
  4014a9:	00 
  4014aa:	4c 89 84 24 c8 00 00 	mov    %r8,0xc8(%rsp)
  4014b1:	00 
  4014b2:	48 89 b4 24 c0 00 00 	mov    %rsi,0xc0(%rsp)
  4014b9:	00 
  4014ba:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  4014c1:	00 
  4014c2:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  4014c9:	00 
  4014ca:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4014d1:	00 
  4014d2:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  4014d9:	00 
  4014da:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4014df:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4014e6:	00 
  4014e7:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4014ec:	e8 4f 0a 00 00       	call   401f40 <runtime::make_slice_error_loc>
  4014f1:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4014f6:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4014fb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401500:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401505:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40150a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40150f:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  401514:	48 0f af fa          	imul   %rdx,%rdi
  401518:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  40151f:	00 
  401520:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  401527:	00 
  401528:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40152f:	00 
  401530:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  401537:	00 
  401538:	0f 57 c0             	xorps  %xmm0,%xmm0
  40153b:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  401542:	00 
  401543:	48 89 e0             	mov    %rsp,%rax
  401546:	4c 89 08             	mov    %r9,(%rax)
  401549:	4c 8d 8c 24 80 00 00 	lea    0x80(%rsp),%r9
  401550:	00 
  401551:	e8 da b0 00 00       	call   40c630 <runtime::mem_alloc_bytes>
  401556:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  40155d:	00 
  40155e:	48 8b 94 24 88 00 00 	mov    0x88(%rsp),%rdx
  401565:	00 
  401566:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40156b:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  401570:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  401574:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  40157a:	0f 94 c0             	sete   %al
  40157d:	24 01                	and    $0x1,%al
  40157f:	3c 00                	cmp    $0x0,%al
  401581:	74 1e                	je     4015a1 <runtime::_make_aligned_type_erased+0x161>
  401583:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  401588:	48 83 f8 00          	cmp    $0x0,%rax
  40158c:	0f 95 c0             	setne  %al
  40158f:	24 01                	and    $0x1,%al
  401591:	3c 00                	cmp    $0x0,%al
  401593:	74 0c                	je     4015a1 <runtime::_make_aligned_type_erased+0x161>
  401595:	8a 44 24 6f          	mov    0x6f(%rsp),%al
  401599:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  4015a0:	c3                   	ret
  4015a1:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4015a6:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4015ab:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4015b0:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4015b5:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  4015ba:	48 89 32             	mov    %rsi,(%rdx)
  4015bd:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4015c2:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  4015c7:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4015cb:	8a 44 24 6f          	mov    0x6f(%rsp),%al
  4015cf:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  4015d6:	c3                   	ret
  4015d7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4015de:	00 00 

00000000004015e0 <runtime::make_slice:proc(T:$[]u8,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]u8,err:runtime::Allocator_Error)>:
  4015e0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4015e7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  4015ec:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  4015f1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4015f6:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4015fb:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  401600:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  401605:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40160a:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40160f:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401614:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  401619:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40161e:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  401625:	00 
  401626:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  40162b:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  401630:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  401635:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  40163a:	0f 57 c0             	xorps  %xmm0,%xmm0
  40163d:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  401642:	c6 44 24 5f 00       	movb   $0x0,0x5f(%rsp)
  401647:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  40164c:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401651:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  401656:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40165b:	48 89 e0             	mov    %rsp,%rax
  40165e:	48 89 70 08          	mov    %rsi,0x8(%rax)
  401662:	48 89 08             	mov    %rcx,(%rax)
  401665:	48 8d 7c 24 60       	lea    0x60(%rsp),%rdi
  40166a:	b9 01 00 00 00       	mov    $0x1,%ecx
  40166f:	48 89 ce             	mov    %rcx,%rsi
  401672:	e8 c9 fd ff ff       	call   401440 <runtime::_make_aligned_type_erased>
  401677:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40167c:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  401680:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  401685:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  40168a:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40168e:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  401693:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  401698:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  40169c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4016a0:	48 89 11             	mov    %rdx,(%rcx)
  4016a3:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  4016aa:	c3                   	ret
  4016ab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004016b0 <runtime::assert>:
  4016b0:	48 83 ec 48          	sub    $0x48,%rsp
  4016b4:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  4016b9:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  4016be:	40 88 f8             	mov    %dil,%al
  4016c1:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  4016c5:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  4016ca:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  4016cf:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  4016d3:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4016d8:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4016dd:	88 44 24 47          	mov    %al,0x47(%rsp)
  4016e1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4016e6:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4016eb:	3c 00                	cmp    $0x0,%al
  4016ed:	75 19                	jne    401708 <runtime::assert+0x58>
  4016ef:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4016f4:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4016f9:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4016fe:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401703:	e8 c8 00 00 00       	call   4017d0 <runtime::assert.internal-0>
  401708:	48 83 c4 48          	add    $0x48,%rsp
  40170c:	c3                   	ret
  40170d:	0f 1f 00             	nopl   (%rax)

0000000000401710 <runtime::panic>:
  401710:	48 83 ec 38          	sub    $0x38,%rsp
  401714:	48 89 0c 24          	mov    %rcx,(%rsp)
  401718:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40171d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  401722:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  401727:	48 8b 04 24          	mov    (%rsp),%rax
  40172b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401730:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401735:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40173a:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40173f:	48 8b 40 20          	mov    0x20(%rax),%rax
  401743:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401748:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  40174e:	0f 94 c0             	sete   %al
  401751:	24 01                	and    $0x1,%al
  401753:	3c 00                	cmp    $0x0,%al
  401755:	74 0c                	je     401763 <runtime::panic+0x53>
  401757:	48 c7 c0 30 47 40 00 	mov    $0x404730,%rax
  40175e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401763:	4c 8b 0c 24          	mov    (%rsp),%r9
  401767:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40176c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401771:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401776:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40177b:	bf 04 f0 40 00       	mov    $0x40f004,%edi
  401780:	be 05 00 00 00       	mov    $0x5,%esi
  401785:	ff d0                	call   *%rax
  401787:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40178e:	00 00 

0000000000401790 <runtime::panic_contextless>:
  401790:	48 83 ec 28          	sub    $0x28,%rsp
  401794:	48 89 14 24          	mov    %rdx,(%rsp)
  401798:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40179d:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4017a2:	4c 8b 04 24          	mov    (%rsp),%r8
  4017a6:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4017ab:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4017b0:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4017b5:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  4017ba:	bf 04 f0 40 00       	mov    $0x40f004,%edi
  4017bf:	be 05 00 00 00       	mov    $0x5,%esi
  4017c4:	e8 b7 2f 00 00       	call   404780 <runtime::default_assertion_contextless_failure_proc>
  4017c9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004017d0 <runtime::assert.internal-0>:
  4017d0:	48 83 ec 38          	sub    $0x38,%rsp
  4017d4:	48 89 0c 24          	mov    %rcx,(%rsp)
  4017d8:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4017dd:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4017e2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4017e7:	48 8b 04 24          	mov    (%rsp),%rax
  4017eb:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4017f0:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4017f5:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  4017fa:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4017ff:	48 8b 40 20          	mov    0x20(%rax),%rax
  401803:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401808:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
  40180e:	0f 94 c0             	sete   %al
  401811:	24 01                	and    $0x1,%al
  401813:	3c 00                	cmp    $0x0,%al
  401815:	74 0c                	je     401823 <runtime::assert.internal-0+0x53>
  401817:	48 c7 c0 30 47 40 00 	mov    $0x404730,%rax
  40181e:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  401823:	4c 8b 0c 24          	mov    (%rsp),%r9
  401827:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  40182c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401831:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  401836:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40183b:	bf 0a f0 40 00       	mov    $0x40f00a,%edi
  401840:	be 11 00 00 00       	mov    $0x11,%esi
  401845:	ff d0                	call   *%rax
  401847:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40184e:	00 00 

0000000000401850 <runtime::add_thread_local_cleaner>:
  401850:	48 83 ec 38          	sub    $0x38,%rsp
  401854:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  401859:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40185e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401863:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401868:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40186d:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401872:	48 c7 44 24 20 08 00 	movq   $0x8,0x20(%rsp)
  401879:	00 00 
  40187b:	48 c7 44 24 18 ff ff 	movq   $0xffffffffffffffff,0x18(%rsp)
  401882:	ff ff 
  401884:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401889:	48 83 c0 01          	add    $0x1,%rax
  40188d:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  401892:	48 83 f8 08          	cmp    $0x8,%rax
  401896:	7d 41                	jge    4018d9 <runtime::add_thread_local_cleaner+0x89>
  401898:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40189d:	48 c7 c0 b8 41 41 00 	mov    $0x4141b8,%rax
  4018a4:	48 c1 e1 04          	shl    $0x4,%rcx
  4018a8:	48 01 c8             	add    %rcx,%rax
  4018ab:	48 89 04 24          	mov    %rax,(%rsp)
  4018af:	48 83 78 08 00       	cmpq   $0x0,0x8(%rax)
  4018b4:	0f 94 c0             	sete   %al
  4018b7:	24 01                	and    $0x1,%al
  4018b9:	3c 00                	cmp    $0x0,%al
  4018bb:	74 1a                	je     4018d7 <runtime::add_thread_local_cleaner+0x87>
  4018bd:	48 8b 04 24          	mov    (%rsp),%rax
  4018c1:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4018c6:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4018cb:	48 89 10             	mov    %rdx,(%rax)
  4018ce:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4018d2:	48 83 c4 38          	add    $0x38,%rsp
  4018d6:	c3                   	ret
  4018d7:	eb ab                	jmp    401884 <runtime::add_thread_local_cleaner+0x34>
  4018d9:	bf 20 f0 40 00       	mov    $0x40f020,%edi
  4018de:	ba b0 f0 40 00       	mov    $0x40f0b0,%edx
  4018e3:	be 37 00 00 00       	mov    $0x37,%esi
  4018e8:	e8 a3 fe ff ff       	call   401790 <runtime::panic_contextless>
  4018ed:	0f 1f 00             	nopl   (%rax)

00000000004018f0 <runtime::stderr_write>:
  4018f0:	48 83 ec 38          	sub    $0x38,%rsp
  4018f4:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  4018f9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4018fe:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  401903:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  401908:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40190d:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  401912:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  401917:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  40191e:	00 00 
  401920:	48 8d 54 24 20       	lea    0x20(%rsp),%rdx
  401925:	e8 56 29 00 00       	call   404280 <runtime::[os_specific_linux.odin]::_stderr_write>
  40192a:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40192f:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  401934:	48 89 11             	mov    %rdx,(%rcx)
  401937:	48 83 c4 38          	add    $0x38,%rsp
  40193b:	c3                   	ret
  40193c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401940 <runtime::rand_bytes>:
  401940:	48 83 ec 28          	sub    $0x28,%rsp
  401944:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  401949:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  40194e:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  401953:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401958:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40195d:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  401962:	e8 99 29 00 00       	call   404300 <runtime::[os_specific_linux.odin]::_rand_bytes>
  401967:	48 83 c4 28          	add    $0x28,%rsp
  40196b:	c3                   	ret
  40196c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401970 <runtime::bounds_trap>:
  401970:	eb 00                	jmp    401972 <runtime::bounds_trap+0x2>
  401972:	0f 0b                	ud2
  401974:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40197b:	00 00 00 00 00 

0000000000401980 <runtime::bounds_check_error>:
  401980:	48 83 ec 58          	sub    $0x58,%rsp
  401984:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  401989:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40198e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  401992:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  401996:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40199b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  4019a0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4019a5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4019aa:	8b 54 24 18          	mov    0x18(%rsp),%edx
  4019ae:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  4019b2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4019b7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  4019bc:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  4019c1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  4019c6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  4019ca:	89 54 24 40          	mov    %edx,0x40(%rsp)
  4019ce:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4019d3:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4019d8:	48 39 c8             	cmp    %rcx,%rax
  4019db:	0f 92 c0             	setb   %al
  4019de:	24 01                	and    $0x1,%al
  4019e0:	3c 00                	cmp    $0x0,%al
  4019e2:	74 05                	je     4019e9 <runtime::bounds_check_error+0x69>
  4019e4:	48 83 c4 58          	add    $0x58,%rsp
  4019e8:	c3                   	ret
  4019e9:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4019ee:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  4019f3:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4019f7:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  4019fb:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401a00:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401a05:	e8 76 05 00 00       	call   401f80 <runtime::bounds_check_error.handle_error-0>
  401a0a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401a10 <runtime::slice_handle_error>:
  401a10:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  401a17:	4c 89 0c 24          	mov    %r9,(%rsp)
  401a1b:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  401a20:	89 4c 24 10          	mov    %ecx,0x10(%rsp)
  401a24:	89 54 24 14          	mov    %edx,0x14(%rsp)
  401a28:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  401a2d:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  401a32:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  401a39:	00 
  401a3a:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  401a3f:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401a44:	4c 8b 04 24          	mov    (%rsp),%r8
  401a48:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  401a4d:	8b 44 24 10          	mov    0x10(%rsp),%eax
  401a51:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  401a55:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  401a5a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  401a5f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  401a64:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  401a6b:	00 
  401a6c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  401a70:	89 44 24 70          	mov    %eax,0x70(%rsp)
  401a74:	4c 89 4c 24 68       	mov    %r9,0x68(%rsp)
  401a79:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  401a7e:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  401a83:	0f 57 c0             	xorps  %xmm0,%xmm0
  401a86:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  401a8b:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401a90:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  401a97:	00 00 
  401a99:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  401a9e:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401aa3:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  401aaa:	00 00 
  401aac:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  401ab1:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  401ab6:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  401aba:	89 44 24 44          	mov    %eax,0x44(%rsp)
  401abe:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  401ac3:	e8 b8 51 00 00       	call   406c80 <runtime::print_caller_location>
  401ac8:	bf d9 f0 40 00       	mov    $0x40f0d9,%edi
  401acd:	be 17 00 00 00       	mov    $0x17,%esi
  401ad2:	e8 e9 4d 00 00       	call   4068c0 <runtime::print_string>
  401ad7:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401adc:	e8 2f 50 00 00       	call   406b10 <runtime::print_i64>
  401ae1:	bf f1 f0 40 00       	mov    $0x40f0f1,%edi
  401ae6:	be 01 00 00 00       	mov    $0x1,%esi
  401aeb:	e8 d0 4d 00 00       	call   4068c0 <runtime::print_string>
  401af0:	48 8b 3c 24          	mov    (%rsp),%rdi
  401af4:	e8 17 50 00 00       	call   406b10 <runtime::print_i64>
  401af9:	bf f3 f0 40 00       	mov    $0x40f0f3,%edi
  401afe:	be 15 00 00 00       	mov    $0x15,%esi
  401b03:	e8 b8 4d 00 00       	call   4068c0 <runtime::print_string>
  401b08:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401b0d:	e8 fe 4f 00 00       	call   406b10 <runtime::print_i64>
  401b12:	bf 0a 00 00 00       	mov    $0xa,%edi
  401b17:	e8 14 4e 00 00       	call   406930 <runtime::print_byte>
  401b1c:	e8 4f fe ff ff       	call   401970 <runtime::bounds_trap>
  401b21:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  401b28:	0f 1f 84 00 00 00 00 
  401b2f:	00 

0000000000401b30 <runtime::multi_pointer_slice_handle_error>:
  401b30:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  401b37:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  401b3c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  401b41:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  401b45:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  401b49:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  401b4e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  401b53:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401b58:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  401b5d:	8b 44 24 18          	mov    0x18(%rsp),%eax
  401b61:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401b65:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401b6a:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401b6f:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  401b74:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  401b7b:	00 
  401b7c:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  401b80:	89 44 24 70          	mov    %eax,0x70(%rsp)
  401b84:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  401b89:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  401b8e:	0f 57 c0             	xorps  %xmm0,%xmm0
  401b91:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  401b96:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401b9b:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  401ba2:	00 00 
  401ba4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  401ba9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401bae:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  401bb5:	00 00 
  401bb7:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  401bbc:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  401bc1:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  401bc5:	89 44 24 44          	mov    %eax,0x44(%rsp)
  401bc9:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  401bce:	e8 ad 50 00 00       	call   406c80 <runtime::print_caller_location>
  401bd3:	bf d9 f0 40 00       	mov    $0x40f0d9,%edi
  401bd8:	be 17 00 00 00       	mov    $0x17,%esi
  401bdd:	e8 de 4c 00 00       	call   4068c0 <runtime::print_string>
  401be2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  401be7:	e8 24 4f 00 00       	call   406b10 <runtime::print_i64>
  401bec:	bf f1 f0 40 00       	mov    $0x40f0f1,%edi
  401bf1:	be 01 00 00 00       	mov    $0x1,%esi
  401bf6:	e8 c5 4c 00 00       	call   4068c0 <runtime::print_string>
  401bfb:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401c00:	e8 0b 4f 00 00       	call   406b10 <runtime::print_i64>
  401c05:	bf 0a 00 00 00       	mov    $0xa,%edi
  401c0a:	e8 21 4d 00 00       	call   406930 <runtime::print_byte>
  401c0f:	e8 5c fd ff ff       	call   401970 <runtime::bounds_trap>
  401c14:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  401c1b:	00 00 00 00 00 

0000000000401c20 <runtime::multi_pointer_slice_expr_error>:
  401c20:	48 83 ec 58          	sub    $0x58,%rsp
  401c24:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  401c29:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  401c2e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  401c32:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  401c36:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  401c3b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  401c40:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401c45:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401c4a:	8b 54 24 18          	mov    0x18(%rsp),%edx
  401c4e:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  401c52:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  401c57:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  401c5c:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  401c61:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  401c66:	89 74 24 44          	mov    %esi,0x44(%rsp)
  401c6a:	89 54 24 40          	mov    %edx,0x40(%rsp)
  401c6e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401c73:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  401c78:	48 39 c8             	cmp    %rcx,%rax
  401c7b:	0f 9e c0             	setle  %al
  401c7e:	24 01                	and    $0x1,%al
  401c80:	3c 00                	cmp    $0x0,%al
  401c82:	74 05                	je     401c89 <runtime::multi_pointer_slice_expr_error+0x69>
  401c84:	48 83 c4 58          	add    $0x58,%rsp
  401c88:	c3                   	ret
  401c89:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  401c8e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  401c93:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  401c97:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  401c9b:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401ca0:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401ca5:	e8 86 fe ff ff       	call   401b30 <runtime::multi_pointer_slice_handle_error>
  401caa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401cb0 <runtime::slice_expr_error_hi>:
  401cb0:	48 83 ec 58          	sub    $0x58,%rsp
  401cb4:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  401cb9:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  401cbe:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  401cc2:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  401cc6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  401ccb:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  401cd0:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401cd5:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401cda:	8b 54 24 18          	mov    0x18(%rsp),%edx
  401cde:	8b 74 24 1c          	mov    0x1c(%rsp),%esi
  401ce2:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  401ce7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  401cec:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  401cf1:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  401cf6:	89 74 24 44          	mov    %esi,0x44(%rsp)
  401cfa:	89 54 24 40          	mov    %edx,0x40(%rsp)
  401cfe:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  401d03:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401d08:	31 c0                	xor    %eax,%eax
  401d0a:	48 39 c8             	cmp    %rcx,%rax
  401d0d:	0f 9e c0             	setle  %al
  401d10:	24 01                	and    $0x1,%al
  401d12:	3c 00                	cmp    $0x0,%al
  401d14:	74 1b                	je     401d31 <runtime::slice_expr_error_hi+0x81>
  401d16:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401d1b:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401d20:	48 39 c8             	cmp    %rcx,%rax
  401d23:	0f 9e c0             	setle  %al
  401d26:	24 01                	and    $0x1,%al
  401d28:	3c 00                	cmp    $0x0,%al
  401d2a:	74 05                	je     401d31 <runtime::slice_expr_error_hi+0x81>
  401d2c:	48 83 c4 58          	add    $0x58,%rsp
  401d30:	c3                   	ret
  401d31:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  401d36:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  401d3a:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  401d3e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401d43:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401d48:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  401d4d:	48 89 e0             	mov    %rsp,%rax
  401d50:	4c 89 00             	mov    %r8,(%rax)
  401d53:	31 c0                	xor    %eax,%eax
  401d55:	41 89 c0             	mov    %eax,%r8d
  401d58:	e8 b3 fc ff ff       	call   401a10 <runtime::slice_handle_error>
  401d5d:	0f 1f 00             	nopl   (%rax)

0000000000401d60 <runtime::slice_expr_error_lo_hi>:
  401d60:	48 83 ec 68          	sub    $0x68,%rsp
  401d64:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  401d69:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  401d6e:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  401d72:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  401d76:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  401d7b:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  401d80:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  401d85:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  401d8a:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  401d8f:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  401d94:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  401d99:	8b 74 24 18          	mov    0x18(%rsp),%esi
  401d9d:	8b 7c 24 1c          	mov    0x1c(%rsp),%edi
  401da1:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  401da6:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  401dab:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  401db0:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  401db5:	89 7c 24 54          	mov    %edi,0x54(%rsp)
  401db9:	89 74 24 50          	mov    %esi,0x50(%rsp)
  401dbd:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  401dc2:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  401dc7:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  401dcc:	31 c0                	xor    %eax,%eax
  401dce:	48 39 c8             	cmp    %rcx,%rax
  401dd1:	0f 9e c0             	setle  %al
  401dd4:	24 01                	and    $0x1,%al
  401dd6:	3c 00                	cmp    $0x0,%al
  401dd8:	74 47                	je     401e21 <runtime::slice_expr_error_lo_hi+0xc1>
  401dda:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401ddf:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401de4:	48 39 c8             	cmp    %rcx,%rax
  401de7:	0f 9e c0             	setle  %al
  401dea:	24 01                	and    $0x1,%al
  401dec:	3c 00                	cmp    $0x0,%al
  401dee:	74 31                	je     401e21 <runtime::slice_expr_error_lo_hi+0xc1>
  401df0:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  401df5:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401dfa:	48 39 c8             	cmp    %rcx,%rax
  401dfd:	0f 9e c0             	setle  %al
  401e00:	24 01                	and    $0x1,%al
  401e02:	3c 00                	cmp    $0x0,%al
  401e04:	74 1b                	je     401e21 <runtime::slice_expr_error_lo_hi+0xc1>
  401e06:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  401e0b:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  401e10:	48 39 c8             	cmp    %rcx,%rax
  401e13:	0f 9e c0             	setle  %al
  401e16:	24 01                	and    $0x1,%al
  401e18:	3c 00                	cmp    $0x0,%al
  401e1a:	74 05                	je     401e21 <runtime::slice_expr_error_lo_hi+0xc1>
  401e1c:	48 83 c4 68          	add    $0x68,%rsp
  401e20:	c3                   	ret
  401e21:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  401e26:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  401e2b:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  401e2f:	8b 54 24 1c          	mov    0x1c(%rsp),%edx
  401e33:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401e38:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  401e3d:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  401e42:	48 89 e0             	mov    %rsp,%rax
  401e45:	4c 89 10             	mov    %r10,(%rax)
  401e48:	e8 c3 fb ff ff       	call   401a10 <runtime::slice_handle_error>
  401e4d:	0f 1f 00             	nopl   (%rax)

0000000000401e50 <runtime::matrix_bounds_check_error>:
  401e50:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  401e57:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  401e5c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  401e61:	89 4c 24 28          	mov    %ecx,0x28(%rsp)
  401e65:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  401e69:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  401e6e:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  401e73:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  401e7a:	00 
  401e7b:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  401e80:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  401e87:	00 
  401e88:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  401e8d:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  401e92:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  401e97:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  401e9c:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  401ea1:	8b 7c 24 28          	mov    0x28(%rsp),%edi
  401ea5:	44 8b 44 24 2c       	mov    0x2c(%rsp),%r8d
  401eaa:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  401eaf:	4c 8b 54 24 38       	mov    0x38(%rsp),%r10
  401eb4:	4c 89 54 24 78       	mov    %r10,0x78(%rsp)
  401eb9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  401ec0:	00 
  401ec1:	44 89 44 24 74       	mov    %r8d,0x74(%rsp)
  401ec6:	89 7c 24 70          	mov    %edi,0x70(%rsp)
  401eca:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  401ecf:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  401ed4:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  401ed9:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  401ede:	48 39 c8             	cmp    %rcx,%rax
  401ee1:	0f 92 c0             	setb   %al
  401ee4:	24 01                	and    $0x1,%al
  401ee6:	3c 00                	cmp    $0x0,%al
  401ee8:	74 1e                	je     401f08 <runtime::matrix_bounds_check_error+0xb8>
  401eea:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  401eef:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  401ef4:	48 39 c8             	cmp    %rcx,%rax
  401ef7:	0f 92 c0             	setb   %al
  401efa:	24 01                	and    $0x1,%al
  401efc:	3c 00                	cmp    $0x0,%al
  401efe:	74 08                	je     401f08 <runtime::matrix_bounds_check_error+0xb8>
  401f00:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  401f07:	c3                   	ret
  401f08:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  401f0d:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  401f12:	8b 4c 24 28          	mov    0x28(%rsp),%ecx
  401f16:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  401f1a:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  401f1f:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  401f24:	4c 8b 54 24 48       	mov    0x48(%rsp),%r10
  401f29:	4c 8b 5c 24 40       	mov    0x40(%rsp),%r11
  401f2e:	48 89 e0             	mov    %rsp,%rax
  401f31:	4c 89 58 08          	mov    %r11,0x8(%rax)
  401f35:	4c 89 10             	mov    %r10,(%rax)
  401f38:	e8 33 01 00 00       	call   402070 <runtime::matrix_bounds_check_error.handle_error-0>
  401f3d:	0f 1f 00             	nopl   (%rax)

0000000000401f40 <runtime::make_slice_error_loc>:
  401f40:	48 83 ec 18          	sub    $0x18,%rsp
  401f44:	48 89 3c 24          	mov    %rdi,(%rsp)
  401f48:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  401f4d:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  401f52:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  401f57:	31 c0                	xor    %eax,%eax
  401f59:	48 39 c8             	cmp    %rcx,%rax
  401f5c:	0f 9e c0             	setle  %al
  401f5f:	24 01                	and    $0x1,%al
  401f61:	3c 00                	cmp    $0x0,%al
  401f63:	74 05                	je     401f6a <runtime::make_slice_error_loc+0x2a>
  401f65:	48 83 c4 18          	add    $0x18,%rsp
  401f69:	c3                   	ret
  401f6a:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  401f6f:	48 8b 3c 24          	mov    (%rsp),%rdi
  401f73:	e8 68 02 00 00       	call   4021e0 <runtime::make_slice_error_loc.handle_error-0>
  401f78:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  401f7f:	00 

0000000000401f80 <runtime::bounds_check_error.handle_error-0>:
  401f80:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  401f87:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  401f8c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  401f91:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  401f95:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  401f99:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  401f9e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  401fa3:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  401fa8:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  401fad:	8b 44 24 18          	mov    0x18(%rsp),%eax
  401fb1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  401fb5:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  401fba:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  401fbf:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  401fc4:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  401fcb:	00 
  401fcc:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  401fd0:	89 44 24 70          	mov    %eax,0x70(%rsp)
  401fd4:	4c 89 44 24 68       	mov    %r8,0x68(%rsp)
  401fd9:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  401fde:	0f 57 c0             	xorps  %xmm0,%xmm0
  401fe1:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  401fe6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401feb:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  401ff2:	00 00 
  401ff4:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  401ff9:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  401ffe:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  402005:	00 00 
  402007:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40200c:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  402011:	89 4c 24 40          	mov    %ecx,0x40(%rsp)
  402015:	89 44 24 44          	mov    %eax,0x44(%rsp)
  402019:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40201e:	e8 5d 4c 00 00       	call   406c80 <runtime::print_caller_location>
  402023:	bf 09 f1 40 00       	mov    $0x40f109,%edi
  402028:	be 07 00 00 00       	mov    $0x7,%esi
  40202d:	e8 8e 48 00 00       	call   4068c0 <runtime::print_string>
  402032:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402037:	e8 d4 4a 00 00       	call   406b10 <runtime::print_i64>
  40203c:	bf f3 f0 40 00       	mov    $0x40f0f3,%edi
  402041:	be 15 00 00 00       	mov    $0x15,%esi
  402046:	e8 75 48 00 00       	call   4068c0 <runtime::print_string>
  40204b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402050:	e8 bb 4a 00 00       	call   406b10 <runtime::print_i64>
  402055:	bf 0a 00 00 00       	mov    $0xa,%edi
  40205a:	e8 d1 48 00 00       	call   406930 <runtime::print_byte>
  40205f:	e8 0c f9 ff ff       	call   401970 <runtime::bounds_trap>
  402064:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40206b:	00 00 00 00 00 

0000000000402070 <runtime::matrix_bounds_check_error.handle_error-0>:
  402070:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  402077:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40207c:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  402081:	89 4c 24 18          	mov    %ecx,0x18(%rsp)
  402085:	89 54 24 1c          	mov    %edx,0x1c(%rsp)
  402089:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40208e:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  402093:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  40209a:	00 
  40209b:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  4020a0:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  4020a7:	00 
  4020a8:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4020ad:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4020b2:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  4020b7:	4c 8b 4c 24 08       	mov    0x8(%rsp),%r9
  4020bc:	4c 8b 54 24 10       	mov    0x10(%rsp),%r10
  4020c1:	8b 44 24 18          	mov    0x18(%rsp),%eax
  4020c5:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  4020c9:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  4020ce:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4020d3:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  4020da:	00 
  4020db:	48 89 b4 24 a0 00 00 	mov    %rsi,0xa0(%rsp)
  4020e2:	00 
  4020e3:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  4020ea:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  4020f1:	4c 89 94 24 88 00 00 	mov    %r10,0x88(%rsp)
  4020f8:	00 
  4020f9:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  402100:	00 
  402101:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  402106:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  40210b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40210e:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  402113:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  402118:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40211f:	00 00 
  402121:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  402126:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40212b:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  402132:	00 00 
  402134:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  402139:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40213e:	89 4c 24 50          	mov    %ecx,0x50(%rsp)
  402142:	89 44 24 54          	mov    %eax,0x54(%rsp)
  402146:	48 8d 7c 24 40       	lea    0x40(%rsp),%rdi
  40214b:	e8 30 4b 00 00       	call   406c80 <runtime::print_caller_location>
  402150:	bf 11 f1 40 00       	mov    $0x40f111,%edi
  402155:	be 11 00 00 00       	mov    $0x11,%esi
  40215a:	e8 61 47 00 00       	call   4068c0 <runtime::print_string>
  40215f:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402164:	e8 a7 49 00 00       	call   406b10 <runtime::print_i64>
  402169:	bf 23 f1 40 00       	mov    $0x40f123,%edi
  40216e:	be 02 00 00 00       	mov    $0x2,%esi
  402173:	e8 48 47 00 00       	call   4068c0 <runtime::print_string>
  402178:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40217d:	e8 8e 49 00 00       	call   406b10 <runtime::print_i64>
  402182:	bf 26 f1 40 00       	mov    $0x40f126,%edi
  402187:	be 16 00 00 00       	mov    $0x16,%esi
  40218c:	e8 2f 47 00 00       	call   4068c0 <runtime::print_string>
  402191:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  402196:	e8 75 49 00 00       	call   406b10 <runtime::print_i64>
  40219b:	bf 3d f1 40 00       	mov    $0x40f13d,%edi
  4021a0:	be 06 00 00 00       	mov    $0x6,%esi
  4021a5:	e8 16 47 00 00       	call   4068c0 <runtime::print_string>
  4021aa:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  4021af:	e8 5c 49 00 00       	call   406b10 <runtime::print_i64>
  4021b4:	bf 44 f1 40 00       	mov    $0x40f144,%edi
  4021b9:	be 01 00 00 00       	mov    $0x1,%esi
  4021be:	e8 fd 46 00 00       	call   4068c0 <runtime::print_string>
  4021c3:	bf 0a 00 00 00       	mov    $0xa,%edi
  4021c8:	e8 63 47 00 00       	call   406930 <runtime::print_byte>
  4021cd:	e8 9e f7 ff ff       	call   401970 <runtime::bounds_trap>
  4021d2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4021d9:	1f 84 00 00 00 00 00 

00000000004021e0 <runtime::make_slice_error_loc.handle_error-0>:
  4021e0:	48 83 ec 18          	sub    $0x18,%rsp
  4021e4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4021e8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4021ed:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4021f2:	48 8b 3c 24          	mov    (%rsp),%rdi
  4021f6:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4021fb:	e8 80 4a 00 00       	call   406c80 <runtime::print_caller_location>
  402200:	bf 46 f1 40 00       	mov    $0x40f146,%edi
  402205:	be 20 00 00 00       	mov    $0x20,%esi
  40220a:	e8 b1 46 00 00       	call   4068c0 <runtime::print_string>
  40220f:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402214:	e8 f7 48 00 00       	call   406b10 <runtime::print_i64>
  402219:	bf 0a 00 00 00       	mov    $0xa,%edi
  40221e:	e8 0d 47 00 00       	call   406930 <runtime::print_byte>
  402223:	e8 48 f7 ff ff       	call   401970 <runtime::bounds_trap>
  402228:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40222f:	00 

0000000000402230 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>:
  402230:	50                   	push   %rax
  402231:	eb 00                	jmp    402233 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini+0x3>
  402233:	48 c7 c0 58 ff ff ff 	mov    $0xffffffffffffff58,%rax
  40223a:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  402241:	00 00 
  402243:	48 01 c7             	add    %rax,%rdi
  402246:	e8 05 00 00 00       	call   402250 <runtime::default_temp_allocator_destroy>
  40224b:	58                   	pop    %rax
  40224c:	c3                   	ret
  40224d:	0f 1f 00             	nopl   (%rax)

0000000000402250 <runtime::default_temp_allocator_destroy>:
  402250:	48 83 ec 18          	sub    $0x18,%rsp
  402254:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  402259:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40225e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  402263:	48 83 f8 00          	cmp    $0x0,%rax
  402267:	0f 95 c0             	setne  %al
  40226a:	24 01                	and    $0x1,%al
  40226c:	3c 00                	cmp    $0x0,%al
  40226e:	74 25                	je     402295 <runtime::default_temp_allocator_destroy+0x45>
  402270:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  402275:	48 be d0 f1 40 00 00 	movabs $0x40f1d0,%rsi
  40227c:	00 00 00 
  40227f:	e8 fc 5c 00 00       	call   407f80 <runtime::arena_destroy>
  402284:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402289:	31 f6                	xor    %esi,%esi
  40228b:	ba 38 00 00 00       	mov    $0x38,%edx
  402290:	e8 ab ed ff ff       	call   401040 <memset@plt>
  402295:	48 83 c4 18          	add    $0x18,%rsp
  402299:	c3                   	ret
  40229a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004022a0 <runtime::default_temp_allocator_proc>:
  4022a0:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  4022a7:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  4022ac:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  4022b1:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4022b6:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4022bb:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4022c0:	40 88 f0             	mov    %sil,%al
  4022c3:	88 44 24 47          	mov    %al,0x47(%rsp)
  4022c7:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4022ce:	00 
  4022cf:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4022d4:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4022db:	00 
  4022dc:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4022e1:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  4022e8:	00 
  4022e9:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  4022ee:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  4022f3:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  4022f8:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4022fd:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  402302:	8a 44 24 47          	mov    0x47(%rsp),%al
  402306:	4c 8b 54 24 58       	mov    0x58(%rsp),%r10
  40230b:	4c 8b 5c 24 48       	mov    0x48(%rsp),%r11
  402310:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  402315:	48 89 b4 24 c0 00 00 	mov    %rsi,0xc0(%rsp)
  40231c:	00 
  40231d:	88 84 24 bf 00 00 00 	mov    %al,0xbf(%rsp)
  402324:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  40232b:	00 
  40232c:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  402333:	00 
  402334:	4c 89 84 24 a0 00 00 	mov    %r8,0xa0(%rsp)
  40233b:	00 
  40233c:	4c 89 8c 24 98 00 00 	mov    %r9,0x98(%rsp)
  402343:	00 
  402344:	0f 57 c0             	xorps  %xmm0,%xmm0
  402347:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40234e:	00 
  40234f:	c6 44 24 7f 00       	movb   $0x0,0x7f(%rsp)
  402354:	48 89 74 24 70       	mov    %rsi,0x70(%rsp)
  402359:	48 8b 7c 24 70       	mov    0x70(%rsp),%rdi
  40235e:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  402363:	48 89 e6             	mov    %rsp,%rsi
  402366:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  40236a:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  40236f:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  402373:	4c 89 16             	mov    %r10,(%rsi)
  402376:	0f b6 f0             	movzbl %al,%esi
  402379:	e8 a2 5c 00 00       	call   408020 <runtime::arena_allocator_proc>
  40237e:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  402383:	40 88 c7             	mov    %al,%dil
  402386:	40 88 f8             	mov    %dil,%al
  402389:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40238e:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  402393:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  40239a:	00 
  40239b:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  4023a2:	00 
  4023a3:	40 88 7c 24 7f       	mov    %dil,0x7f(%rsp)
  4023a8:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4023ac:	48 89 11             	mov    %rdx,(%rcx)
  4023af:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  4023b6:	c3                   	ret
  4023b7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4023be:	00 00 

00000000004023c0 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128>:
  4023c0:	48 81 ec 18 05 00 00 	sub    $0x518,%rsp
  4023c7:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  4023cc:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4023d1:	48 89 84 24 10 05 00 	mov    %rax,0x510(%rsp)
  4023d8:	00 
  4023d9:	48 8b 84 24 10 05 00 	mov    0x510(%rsp),%rax
  4023e0:	00 
  4023e1:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4023e6:	48 89 e0             	mov    %rsp,%rax
  4023e9:	48 c7 00 00 04 00 00 	movq   $0x400,(%rax)
  4023f0:	bf b0 f2 40 00       	mov    $0x40f2b0,%edi
  4023f5:	be 40 00 00 00       	mov    $0x40,%esi
  4023fa:	ba 36 00 00 00       	mov    $0x36,%edx
  4023ff:	b9 24 00 00 00       	mov    $0x24,%ecx
  402404:	41 b8 e0 03 00 00    	mov    $0x3e0,%r8d
  40240a:	41 b9 00 04 00 00    	mov    $0x400,%r9d
  402410:	e8 4b f9 ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  402415:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40241a:	48 05 e0 03 00 00    	add    $0x3e0,%rax
  402420:	48 89 84 24 00 05 00 	mov    %rax,0x500(%rsp)
  402427:	00 
  402428:	48 c7 84 24 08 05 00 	movq   $0x20,0x508(%rsp)
  40242f:	00 20 00 00 00 
  402434:	48 8b 84 24 00 05 00 	mov    0x500(%rsp),%rax
  40243b:	00 
  40243c:	48 89 84 24 f8 04 00 	mov    %rax,0x4f8(%rsp)
  402443:	00 
  402444:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  40244b:	00 
  40244c:	8b 00                	mov    (%rax),%eax
  40244e:	89 84 24 f4 04 00 00 	mov    %eax,0x4f4(%rsp)
  402455:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  40245c:	00 
  40245d:	8b 40 04             	mov    0x4(%rax),%eax
  402460:	89 84 24 f0 04 00 00 	mov    %eax,0x4f0(%rsp)
  402467:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  40246e:	00 
  40246f:	8b 40 08             	mov    0x8(%rax),%eax
  402472:	89 84 24 ec 04 00 00 	mov    %eax,0x4ec(%rsp)
  402479:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  402480:	00 
  402481:	8b 40 0c             	mov    0xc(%rax),%eax
  402484:	89 84 24 e8 04 00 00 	mov    %eax,0x4e8(%rsp)
  40248b:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  402492:	00 
  402493:	8b 40 10             	mov    0x10(%rax),%eax
  402496:	89 84 24 e4 04 00 00 	mov    %eax,0x4e4(%rsp)
  40249d:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  4024a4:	00 
  4024a5:	8b 40 14             	mov    0x14(%rax),%eax
  4024a8:	89 84 24 e0 04 00 00 	mov    %eax,0x4e0(%rsp)
  4024af:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  4024b6:	00 
  4024b7:	8b 40 18             	mov    0x18(%rax),%eax
  4024ba:	89 84 24 dc 04 00 00 	mov    %eax,0x4dc(%rsp)
  4024c1:	48 8b 84 24 f8 04 00 	mov    0x4f8(%rsp),%rax
  4024c8:	00 
  4024c9:	8b 40 1c             	mov    0x1c(%rax),%eax
  4024cc:	89 84 24 d8 04 00 00 	mov    %eax,0x4d8(%rsp)
  4024d3:	0f 57 c0             	xorps  %xmm0,%xmm0
  4024d6:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  4024db:	0f 29 84 24 c0 04 00 	movaps %xmm0,0x4c0(%rsp)
  4024e2:	00 
  4024e3:	66 0f 6e 8c 24 f4 04 	movd   0x4f4(%rsp),%xmm1
  4024ea:	00 00 
  4024ec:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  4024f1:	0f 29 8c 24 c0 04 00 	movaps %xmm1,0x4c0(%rsp)
  4024f8:	00 
  4024f9:	0f 28 8c 24 c0 04 00 	movaps 0x4c0(%rsp),%xmm1
  402500:	00 
  402501:	0f 29 8c 24 b0 04 00 	movaps %xmm1,0x4b0(%rsp)
  402508:	00 
  402509:	0f 29 84 24 a0 04 00 	movaps %xmm0,0x4a0(%rsp)
  402510:	00 
  402511:	66 0f 6e 8c 24 f0 04 	movd   0x4f0(%rsp),%xmm1
  402518:	00 00 
  40251a:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  40251f:	0f 29 8c 24 a0 04 00 	movaps %xmm1,0x4a0(%rsp)
  402526:	00 
  402527:	0f 28 8c 24 a0 04 00 	movaps 0x4a0(%rsp),%xmm1
  40252e:	00 
  40252f:	0f 29 8c 24 90 04 00 	movaps %xmm1,0x490(%rsp)
  402536:	00 
  402537:	0f 29 84 24 80 04 00 	movaps %xmm0,0x480(%rsp)
  40253e:	00 
  40253f:	66 0f 6e 8c 24 ec 04 	movd   0x4ec(%rsp),%xmm1
  402546:	00 00 
  402548:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  40254d:	0f 29 8c 24 80 04 00 	movaps %xmm1,0x480(%rsp)
  402554:	00 
  402555:	0f 28 8c 24 80 04 00 	movaps 0x480(%rsp),%xmm1
  40255c:	00 
  40255d:	0f 29 8c 24 70 04 00 	movaps %xmm1,0x470(%rsp)
  402564:	00 
  402565:	0f 29 84 24 60 04 00 	movaps %xmm0,0x460(%rsp)
  40256c:	00 
  40256d:	66 0f 6e 8c 24 e8 04 	movd   0x4e8(%rsp),%xmm1
  402574:	00 00 
  402576:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  40257b:	0f 29 8c 24 60 04 00 	movaps %xmm1,0x460(%rsp)
  402582:	00 
  402583:	0f 28 8c 24 60 04 00 	movaps 0x460(%rsp),%xmm1
  40258a:	00 
  40258b:	0f 29 8c 24 50 04 00 	movaps %xmm1,0x450(%rsp)
  402592:	00 
  402593:	0f 29 84 24 40 04 00 	movaps %xmm0,0x440(%rsp)
  40259a:	00 
  40259b:	66 0f 6e 8c 24 e4 04 	movd   0x4e4(%rsp),%xmm1
  4025a2:	00 00 
  4025a4:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  4025a9:	0f 29 8c 24 40 04 00 	movaps %xmm1,0x440(%rsp)
  4025b0:	00 
  4025b1:	0f 28 8c 24 40 04 00 	movaps 0x440(%rsp),%xmm1
  4025b8:	00 
  4025b9:	0f 29 8c 24 30 04 00 	movaps %xmm1,0x430(%rsp)
  4025c0:	00 
  4025c1:	0f 29 84 24 20 04 00 	movaps %xmm0,0x420(%rsp)
  4025c8:	00 
  4025c9:	66 0f 6e 8c 24 e0 04 	movd   0x4e0(%rsp),%xmm1
  4025d0:	00 00 
  4025d2:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  4025d7:	0f 29 8c 24 20 04 00 	movaps %xmm1,0x420(%rsp)
  4025de:	00 
  4025df:	0f 28 8c 24 20 04 00 	movaps 0x420(%rsp),%xmm1
  4025e6:	00 
  4025e7:	0f 29 8c 24 10 04 00 	movaps %xmm1,0x410(%rsp)
  4025ee:	00 
  4025ef:	0f 29 84 24 00 04 00 	movaps %xmm0,0x400(%rsp)
  4025f6:	00 
  4025f7:	66 0f 6e 8c 24 dc 04 	movd   0x4dc(%rsp),%xmm1
  4025fe:	00 00 
  402600:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  402605:	0f 29 8c 24 00 04 00 	movaps %xmm1,0x400(%rsp)
  40260c:	00 
  40260d:	0f 28 8c 24 00 04 00 	movaps 0x400(%rsp),%xmm1
  402614:	00 
  402615:	0f 29 8c 24 f0 03 00 	movaps %xmm1,0x3f0(%rsp)
  40261c:	00 
  40261d:	0f 29 84 24 e0 03 00 	movaps %xmm0,0x3e0(%rsp)
  402624:	00 
  402625:	66 0f 6e 8c 24 d8 04 	movd   0x4d8(%rsp),%xmm1
  40262c:	00 00 
  40262e:	66 0f 70 c9 00       	pshufd $0x0,%xmm1,%xmm1
  402633:	0f 29 8c 24 e0 03 00 	movaps %xmm1,0x3e0(%rsp)
  40263a:	00 
  40263b:	0f 28 8c 24 e0 03 00 	movaps 0x3e0(%rsp),%xmm1
  402642:	00 
  402643:	0f 29 8c 24 d0 03 00 	movaps %xmm1,0x3d0(%rsp)
  40264a:	00 
  40264b:	0f 28 0d ae cb 00 00 	movaps 0xcbae(%rip),%xmm1        # 40f200 <_IO_stdin_used+0x200>
  402652:	0f 29 8c 24 c0 03 00 	movaps %xmm1,0x3c0(%rsp)
  402659:	00 
  40265a:	0f 29 84 24 b0 03 00 	movaps %xmm0,0x3b0(%rsp)
  402661:	00 
  402662:	0f 29 84 24 a0 03 00 	movaps %xmm0,0x3a0(%rsp)
  402669:	00 
  40266a:	0f 29 84 24 90 03 00 	movaps %xmm0,0x390(%rsp)
  402671:	00 
  402672:	48 8b 84 24 10 05 00 	mov    0x510(%rsp),%rax
  402679:	00 
  40267a:	48 89 84 24 80 03 00 	mov    %rax,0x380(%rsp)
  402681:	00 
  402682:	48 c7 84 24 88 03 00 	movq   $0x400,0x388(%rsp)
  402689:	00 00 04 00 00 
  40268e:	48 8b 84 24 80 03 00 	mov    0x380(%rsp),%rax
  402695:	00 
  402696:	48 89 84 24 78 03 00 	mov    %rax,0x378(%rsp)
  40269d:	00 
  40269e:	48 c7 c0 30 30 40 00 	mov    $0x403030,%rax
  4026a5:	48 89 84 24 70 03 00 	mov    %rax,0x370(%rsp)
  4026ac:	00 
  4026ad:	48 c7 84 24 68 03 00 	movq   $0x0,0x368(%rsp)
  4026b4:	00 00 00 00 00 
  4026b9:	48 c7 84 24 60 03 00 	movq   $0x0,0x360(%rsp)
  4026c0:	00 00 00 00 00 
  4026c5:	48 83 bc 24 68 03 00 	cmpq   $0x4,0x368(%rsp)
  4026cc:	00 04 
  4026ce:	0f 8d 54 09 00 00    	jge    403028 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128+0xc68>
  4026d4:	0f 28 05 35 cb 00 00 	movaps 0xcb35(%rip),%xmm0        # 40f210 <_IO_stdin_used+0x210>
  4026db:	0f 29 84 24 50 03 00 	movaps %xmm0,0x350(%rsp)
  4026e2:	00 
  4026e3:	0f 28 05 36 cb 00 00 	movaps 0xcb36(%rip),%xmm0        # 40f220 <_IO_stdin_used+0x220>
  4026ea:	0f 29 84 24 40 03 00 	movaps %xmm0,0x340(%rsp)
  4026f1:	00 
  4026f2:	0f 28 05 37 cb 00 00 	movaps 0xcb37(%rip),%xmm0        # 40f230 <_IO_stdin_used+0x230>
  4026f9:	0f 29 84 24 30 03 00 	movaps %xmm0,0x330(%rsp)
  402700:	00 
  402701:	0f 28 05 38 cb 00 00 	movaps 0xcb38(%rip),%xmm0        # 40f240 <_IO_stdin_used+0x240>
  402708:	0f 29 84 24 20 03 00 	movaps %xmm0,0x320(%rsp)
  40270f:	00 
  402710:	66 0f 6f 9c 24 b0 04 	movdqa 0x4b0(%rsp),%xmm3
  402717:	00 00 
  402719:	66 0f 6f 94 24 90 04 	movdqa 0x490(%rsp),%xmm2
  402720:	00 00 
  402722:	66 0f 6f 8c 24 70 04 	movdqa 0x470(%rsp),%xmm1
  402729:	00 00 
  40272b:	66 0f 6f 84 24 50 04 	movdqa 0x450(%rsp),%xmm0
  402732:	00 00 
  402734:	66 0f 7f 9c 24 10 03 	movdqa %xmm3,0x310(%rsp)
  40273b:	00 00 
  40273d:	66 0f 7f 94 24 00 03 	movdqa %xmm2,0x300(%rsp)
  402744:	00 00 
  402746:	66 0f 7f 8c 24 f0 02 	movdqa %xmm1,0x2f0(%rsp)
  40274d:	00 00 
  40274f:	66 0f 7f 84 24 e0 02 	movdqa %xmm0,0x2e0(%rsp)
  402756:	00 00 
  402758:	66 0f 6f 9c 24 30 04 	movdqa 0x430(%rsp),%xmm3
  40275f:	00 00 
  402761:	66 0f 6f 94 24 10 04 	movdqa 0x410(%rsp),%xmm2
  402768:	00 00 
  40276a:	66 0f 6f 8c 24 f0 03 	movdqa 0x3f0(%rsp),%xmm1
  402771:	00 00 
  402773:	66 0f 6f 84 24 d0 03 	movdqa 0x3d0(%rsp),%xmm0
  40277a:	00 00 
  40277c:	66 0f 7f 9c 24 d0 02 	movdqa %xmm3,0x2d0(%rsp)
  402783:	00 00 
  402785:	66 0f 7f 94 24 c0 02 	movdqa %xmm2,0x2c0(%rsp)
  40278c:	00 00 
  40278e:	66 0f 7f 8c 24 b0 02 	movdqa %xmm1,0x2b0(%rsp)
  402795:	00 00 
  402797:	66 0f 7f 84 24 a0 02 	movdqa %xmm0,0x2a0(%rsp)
  40279e:	00 00 
  4027a0:	66 0f 6f 9c 24 c0 03 	movdqa 0x3c0(%rsp),%xmm3
  4027a7:	00 00 
  4027a9:	66 0f 6f 94 24 b0 03 	movdqa 0x3b0(%rsp),%xmm2
  4027b0:	00 00 
  4027b2:	66 0f 6f 8c 24 a0 03 	movdqa 0x3a0(%rsp),%xmm1
  4027b9:	00 00 
  4027bb:	66 0f 6f 84 24 90 03 	movdqa 0x390(%rsp),%xmm0
  4027c2:	00 00 
  4027c4:	66 0f 7f 9c 24 90 02 	movdqa %xmm3,0x290(%rsp)
  4027cb:	00 00 
  4027cd:	66 0f 7f 94 24 80 02 	movdqa %xmm2,0x280(%rsp)
  4027d4:	00 00 
  4027d6:	66 0f 7f 8c 24 70 02 	movdqa %xmm1,0x270(%rsp)
  4027dd:	00 00 
  4027df:	66 0f 7f 84 24 60 02 	movdqa %xmm0,0x260(%rsp)
  4027e6:	00 00 
  4027e8:	48 c7 84 24 58 02 00 	movq   $0x8,0x258(%rsp)
  4027ef:	00 08 00 00 00 
  4027f4:	48 83 bc 24 58 02 00 	cmpq   $0x0,0x258(%rsp)
  4027fb:	00 00 
  4027fd:	0f 9f c0             	setg   %al
  402800:	24 01                	and    $0x1,%al
  402802:	3c 00                	cmp    $0x0,%al
  402804:	0f 84 db 04 00 00    	je     402ce5 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128+0x925>
  40280a:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  402811:	00 
  402812:	0f 28 84 24 50 03 00 	movaps 0x350(%rsp),%xmm0
  402819:	00 
  40281a:	0f 28 8c 24 10 03 00 	movaps 0x310(%rsp),%xmm1
  402821:	00 
  402822:	0f 28 94 24 d0 02 00 	movaps 0x2d0(%rsp),%xmm2
  402829:	00 
  40282a:	0f 28 9c 24 90 02 00 	movaps 0x290(%rsp),%xmm3
  402831:	00 
  402832:	0f 57 e4             	xorps  %xmm4,%xmm4
  402835:	0f 29 64 24 10       	movaps %xmm4,0x10(%rsp)
  40283a:	0f 29 a4 24 40 02 00 	movaps %xmm4,0x240(%rsp)
  402841:	00 
  402842:	0f 29 a4 24 30 02 00 	movaps %xmm4,0x230(%rsp)
  402849:	00 
  40284a:	0f 29 a4 24 20 02 00 	movaps %xmm4,0x220(%rsp)
  402851:	00 
  402852:	48 8d bc 24 40 02 00 	lea    0x240(%rsp),%rdi
  402859:	00 
  40285a:	48 8d b4 24 30 02 00 	lea    0x230(%rsp),%rsi
  402861:	00 
  402862:	48 8d 94 24 20 02 00 	lea    0x220(%rsp),%rdx
  402869:	00 
  40286a:	ff d0                	call   *%rax
  40286c:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  402871:	0f 28 9c 24 40 02 00 	movaps 0x240(%rsp),%xmm3
  402878:	00 
  402879:	0f 28 94 24 30 02 00 	movaps 0x230(%rsp),%xmm2
  402880:	00 
  402881:	0f 28 8c 24 20 02 00 	movaps 0x220(%rsp),%xmm1
  402888:	00 
  402889:	0f 29 9c 24 50 03 00 	movaps %xmm3,0x350(%rsp)
  402890:	00 
  402891:	0f 29 94 24 10 03 00 	movaps %xmm2,0x310(%rsp)
  402898:	00 
  402899:	0f 29 8c 24 d0 02 00 	movaps %xmm1,0x2d0(%rsp)
  4028a0:	00 
  4028a1:	0f 29 84 24 90 02 00 	movaps %xmm0,0x290(%rsp)
  4028a8:	00 
  4028a9:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  4028b0:	00 
  4028b1:	0f 28 84 24 40 03 00 	movaps 0x340(%rsp),%xmm0
  4028b8:	00 
  4028b9:	0f 28 8c 24 00 03 00 	movaps 0x300(%rsp),%xmm1
  4028c0:	00 
  4028c1:	0f 28 94 24 c0 02 00 	movaps 0x2c0(%rsp),%xmm2
  4028c8:	00 
  4028c9:	0f 28 9c 24 80 02 00 	movaps 0x280(%rsp),%xmm3
  4028d0:	00 
  4028d1:	0f 29 a4 24 10 02 00 	movaps %xmm4,0x210(%rsp)
  4028d8:	00 
  4028d9:	0f 29 a4 24 00 02 00 	movaps %xmm4,0x200(%rsp)
  4028e0:	00 
  4028e1:	0f 29 a4 24 f0 01 00 	movaps %xmm4,0x1f0(%rsp)
  4028e8:	00 
  4028e9:	48 8d bc 24 10 02 00 	lea    0x210(%rsp),%rdi
  4028f0:	00 
  4028f1:	48 8d b4 24 00 02 00 	lea    0x200(%rsp),%rsi
  4028f8:	00 
  4028f9:	48 8d 94 24 f0 01 00 	lea    0x1f0(%rsp),%rdx
  402900:	00 
  402901:	ff d0                	call   *%rax
  402903:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  402908:	0f 28 9c 24 10 02 00 	movaps 0x210(%rsp),%xmm3
  40290f:	00 
  402910:	0f 28 94 24 00 02 00 	movaps 0x200(%rsp),%xmm2
  402917:	00 
  402918:	0f 28 8c 24 f0 01 00 	movaps 0x1f0(%rsp),%xmm1
  40291f:	00 
  402920:	0f 29 9c 24 40 03 00 	movaps %xmm3,0x340(%rsp)
  402927:	00 
  402928:	0f 29 94 24 00 03 00 	movaps %xmm2,0x300(%rsp)
  40292f:	00 
  402930:	0f 29 8c 24 c0 02 00 	movaps %xmm1,0x2c0(%rsp)
  402937:	00 
  402938:	0f 29 84 24 80 02 00 	movaps %xmm0,0x280(%rsp)
  40293f:	00 
  402940:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  402947:	00 
  402948:	0f 28 84 24 30 03 00 	movaps 0x330(%rsp),%xmm0
  40294f:	00 
  402950:	0f 28 8c 24 f0 02 00 	movaps 0x2f0(%rsp),%xmm1
  402957:	00 
  402958:	0f 28 94 24 b0 02 00 	movaps 0x2b0(%rsp),%xmm2
  40295f:	00 
  402960:	0f 28 9c 24 70 02 00 	movaps 0x270(%rsp),%xmm3
  402967:	00 
  402968:	0f 29 a4 24 e0 01 00 	movaps %xmm4,0x1e0(%rsp)
  40296f:	00 
  402970:	0f 29 a4 24 d0 01 00 	movaps %xmm4,0x1d0(%rsp)
  402977:	00 
  402978:	0f 29 a4 24 c0 01 00 	movaps %xmm4,0x1c0(%rsp)
  40297f:	00 
  402980:	48 8d bc 24 e0 01 00 	lea    0x1e0(%rsp),%rdi
  402987:	00 
  402988:	48 8d b4 24 d0 01 00 	lea    0x1d0(%rsp),%rsi
  40298f:	00 
  402990:	48 8d 94 24 c0 01 00 	lea    0x1c0(%rsp),%rdx
  402997:	00 
  402998:	ff d0                	call   *%rax
  40299a:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  40299f:	0f 28 9c 24 e0 01 00 	movaps 0x1e0(%rsp),%xmm3
  4029a6:	00 
  4029a7:	0f 28 94 24 d0 01 00 	movaps 0x1d0(%rsp),%xmm2
  4029ae:	00 
  4029af:	0f 28 8c 24 c0 01 00 	movaps 0x1c0(%rsp),%xmm1
  4029b6:	00 
  4029b7:	0f 29 9c 24 30 03 00 	movaps %xmm3,0x330(%rsp)
  4029be:	00 
  4029bf:	0f 29 94 24 f0 02 00 	movaps %xmm2,0x2f0(%rsp)
  4029c6:	00 
  4029c7:	0f 29 8c 24 b0 02 00 	movaps %xmm1,0x2b0(%rsp)
  4029ce:	00 
  4029cf:	0f 29 84 24 70 02 00 	movaps %xmm0,0x270(%rsp)
  4029d6:	00 
  4029d7:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  4029de:	00 
  4029df:	0f 28 84 24 20 03 00 	movaps 0x320(%rsp),%xmm0
  4029e6:	00 
  4029e7:	0f 28 8c 24 e0 02 00 	movaps 0x2e0(%rsp),%xmm1
  4029ee:	00 
  4029ef:	0f 28 94 24 a0 02 00 	movaps 0x2a0(%rsp),%xmm2
  4029f6:	00 
  4029f7:	0f 28 9c 24 60 02 00 	movaps 0x260(%rsp),%xmm3
  4029fe:	00 
  4029ff:	0f 29 a4 24 b0 01 00 	movaps %xmm4,0x1b0(%rsp)
  402a06:	00 
  402a07:	0f 29 a4 24 a0 01 00 	movaps %xmm4,0x1a0(%rsp)
  402a0e:	00 
  402a0f:	0f 29 a4 24 90 01 00 	movaps %xmm4,0x190(%rsp)
  402a16:	00 
  402a17:	48 8d bc 24 b0 01 00 	lea    0x1b0(%rsp),%rdi
  402a1e:	00 
  402a1f:	48 8d b4 24 a0 01 00 	lea    0x1a0(%rsp),%rsi
  402a26:	00 
  402a27:	48 8d 94 24 90 01 00 	lea    0x190(%rsp),%rdx
  402a2e:	00 
  402a2f:	ff d0                	call   *%rax
  402a31:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  402a36:	0f 28 9c 24 b0 01 00 	movaps 0x1b0(%rsp),%xmm3
  402a3d:	00 
  402a3e:	0f 28 94 24 a0 01 00 	movaps 0x1a0(%rsp),%xmm2
  402a45:	00 
  402a46:	0f 28 8c 24 90 01 00 	movaps 0x190(%rsp),%xmm1
  402a4d:	00 
  402a4e:	0f 29 9c 24 20 03 00 	movaps %xmm3,0x320(%rsp)
  402a55:	00 
  402a56:	0f 29 94 24 e0 02 00 	movaps %xmm2,0x2e0(%rsp)
  402a5d:	00 
  402a5e:	0f 29 8c 24 a0 02 00 	movaps %xmm1,0x2a0(%rsp)
  402a65:	00 
  402a66:	0f 29 84 24 60 02 00 	movaps %xmm0,0x260(%rsp)
  402a6d:	00 
  402a6e:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  402a75:	00 
  402a76:	0f 28 84 24 50 03 00 	movaps 0x350(%rsp),%xmm0
  402a7d:	00 
  402a7e:	0f 28 8c 24 00 03 00 	movaps 0x300(%rsp),%xmm1
  402a85:	00 
  402a86:	0f 28 94 24 b0 02 00 	movaps 0x2b0(%rsp),%xmm2
  402a8d:	00 
  402a8e:	0f 28 9c 24 60 02 00 	movaps 0x260(%rsp),%xmm3
  402a95:	00 
  402a96:	0f 29 a4 24 80 01 00 	movaps %xmm4,0x180(%rsp)
  402a9d:	00 
  402a9e:	0f 29 a4 24 70 01 00 	movaps %xmm4,0x170(%rsp)
  402aa5:	00 
  402aa6:	0f 29 a4 24 60 01 00 	movaps %xmm4,0x160(%rsp)
  402aad:	00 
  402aae:	48 8d bc 24 80 01 00 	lea    0x180(%rsp),%rdi
  402ab5:	00 
  402ab6:	48 8d b4 24 70 01 00 	lea    0x170(%rsp),%rsi
  402abd:	00 
  402abe:	48 8d 94 24 60 01 00 	lea    0x160(%rsp),%rdx
  402ac5:	00 
  402ac6:	ff d0                	call   *%rax
  402ac8:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  402acd:	0f 28 9c 24 80 01 00 	movaps 0x180(%rsp),%xmm3
  402ad4:	00 
  402ad5:	0f 28 94 24 70 01 00 	movaps 0x170(%rsp),%xmm2
  402adc:	00 
  402add:	0f 28 8c 24 60 01 00 	movaps 0x160(%rsp),%xmm1
  402ae4:	00 
  402ae5:	0f 29 9c 24 50 03 00 	movaps %xmm3,0x350(%rsp)
  402aec:	00 
  402aed:	0f 29 94 24 00 03 00 	movaps %xmm2,0x300(%rsp)
  402af4:	00 
  402af5:	0f 29 8c 24 b0 02 00 	movaps %xmm1,0x2b0(%rsp)
  402afc:	00 
  402afd:	0f 29 84 24 60 02 00 	movaps %xmm0,0x260(%rsp)
  402b04:	00 
  402b05:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  402b0c:	00 
  402b0d:	0f 28 84 24 40 03 00 	movaps 0x340(%rsp),%xmm0
  402b14:	00 
  402b15:	0f 28 8c 24 f0 02 00 	movaps 0x2f0(%rsp),%xmm1
  402b1c:	00 
  402b1d:	0f 28 94 24 a0 02 00 	movaps 0x2a0(%rsp),%xmm2
  402b24:	00 
  402b25:	0f 28 9c 24 90 02 00 	movaps 0x290(%rsp),%xmm3
  402b2c:	00 
  402b2d:	0f 29 a4 24 50 01 00 	movaps %xmm4,0x150(%rsp)
  402b34:	00 
  402b35:	0f 29 a4 24 40 01 00 	movaps %xmm4,0x140(%rsp)
  402b3c:	00 
  402b3d:	0f 29 a4 24 30 01 00 	movaps %xmm4,0x130(%rsp)
  402b44:	00 
  402b45:	48 8d bc 24 50 01 00 	lea    0x150(%rsp),%rdi
  402b4c:	00 
  402b4d:	48 8d b4 24 40 01 00 	lea    0x140(%rsp),%rsi
  402b54:	00 
  402b55:	48 8d 94 24 30 01 00 	lea    0x130(%rsp),%rdx
  402b5c:	00 
  402b5d:	ff d0                	call   *%rax
  402b5f:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  402b64:	0f 28 9c 24 50 01 00 	movaps 0x150(%rsp),%xmm3
  402b6b:	00 
  402b6c:	0f 28 94 24 40 01 00 	movaps 0x140(%rsp),%xmm2
  402b73:	00 
  402b74:	0f 28 8c 24 30 01 00 	movaps 0x130(%rsp),%xmm1
  402b7b:	00 
  402b7c:	0f 29 9c 24 40 03 00 	movaps %xmm3,0x340(%rsp)
  402b83:	00 
  402b84:	0f 29 94 24 f0 02 00 	movaps %xmm2,0x2f0(%rsp)
  402b8b:	00 
  402b8c:	0f 29 8c 24 a0 02 00 	movaps %xmm1,0x2a0(%rsp)
  402b93:	00 
  402b94:	0f 29 84 24 90 02 00 	movaps %xmm0,0x290(%rsp)
  402b9b:	00 
  402b9c:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  402ba3:	00 
  402ba4:	0f 28 84 24 30 03 00 	movaps 0x330(%rsp),%xmm0
  402bab:	00 
  402bac:	0f 28 8c 24 e0 02 00 	movaps 0x2e0(%rsp),%xmm1
  402bb3:	00 
  402bb4:	0f 28 94 24 d0 02 00 	movaps 0x2d0(%rsp),%xmm2
  402bbb:	00 
  402bbc:	0f 28 9c 24 80 02 00 	movaps 0x280(%rsp),%xmm3
  402bc3:	00 
  402bc4:	0f 29 a4 24 20 01 00 	movaps %xmm4,0x120(%rsp)
  402bcb:	00 
  402bcc:	0f 29 a4 24 10 01 00 	movaps %xmm4,0x110(%rsp)
  402bd3:	00 
  402bd4:	0f 29 a4 24 00 01 00 	movaps %xmm4,0x100(%rsp)
  402bdb:	00 
  402bdc:	48 8d bc 24 20 01 00 	lea    0x120(%rsp),%rdi
  402be3:	00 
  402be4:	48 8d b4 24 10 01 00 	lea    0x110(%rsp),%rsi
  402beb:	00 
  402bec:	48 8d 94 24 00 01 00 	lea    0x100(%rsp),%rdx
  402bf3:	00 
  402bf4:	ff d0                	call   *%rax
  402bf6:	0f 28 64 24 10       	movaps 0x10(%rsp),%xmm4
  402bfb:	0f 28 9c 24 20 01 00 	movaps 0x120(%rsp),%xmm3
  402c02:	00 
  402c03:	0f 28 94 24 10 01 00 	movaps 0x110(%rsp),%xmm2
  402c0a:	00 
  402c0b:	0f 28 8c 24 00 01 00 	movaps 0x100(%rsp),%xmm1
  402c12:	00 
  402c13:	0f 29 9c 24 30 03 00 	movaps %xmm3,0x330(%rsp)
  402c1a:	00 
  402c1b:	0f 29 94 24 e0 02 00 	movaps %xmm2,0x2e0(%rsp)
  402c22:	00 
  402c23:	0f 29 8c 24 d0 02 00 	movaps %xmm1,0x2d0(%rsp)
  402c2a:	00 
  402c2b:	0f 29 84 24 80 02 00 	movaps %xmm0,0x280(%rsp)
  402c32:	00 
  402c33:	48 8b 84 24 70 03 00 	mov    0x370(%rsp),%rax
  402c3a:	00 
  402c3b:	0f 28 84 24 20 03 00 	movaps 0x320(%rsp),%xmm0
  402c42:	00 
  402c43:	0f 28 8c 24 10 03 00 	movaps 0x310(%rsp),%xmm1
  402c4a:	00 
  402c4b:	0f 28 94 24 c0 02 00 	movaps 0x2c0(%rsp),%xmm2
  402c52:	00 
  402c53:	0f 28 9c 24 70 02 00 	movaps 0x270(%rsp),%xmm3
  402c5a:	00 
  402c5b:	0f 29 a4 24 f0 00 00 	movaps %xmm4,0xf0(%rsp)
  402c62:	00 
  402c63:	0f 29 a4 24 e0 00 00 	movaps %xmm4,0xe0(%rsp)
  402c6a:	00 
  402c6b:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  402c72:	00 
  402c73:	48 8d bc 24 f0 00 00 	lea    0xf0(%rsp),%rdi
  402c7a:	00 
  402c7b:	48 8d b4 24 e0 00 00 	lea    0xe0(%rsp),%rsi
  402c82:	00 
  402c83:	48 8d 94 24 d0 00 00 	lea    0xd0(%rsp),%rdx
  402c8a:	00 
  402c8b:	ff d0                	call   *%rax
  402c8d:	66 0f 6f 9c 24 f0 00 	movdqa 0xf0(%rsp),%xmm3
  402c94:	00 00 
  402c96:	66 0f 6f 94 24 e0 00 	movdqa 0xe0(%rsp),%xmm2
  402c9d:	00 00 
  402c9f:	66 0f 6f 8c 24 d0 00 	movdqa 0xd0(%rsp),%xmm1
  402ca6:	00 00 
  402ca8:	66 0f 7f 9c 24 20 03 	movdqa %xmm3,0x320(%rsp)
  402caf:	00 00 
  402cb1:	66 0f 7f 94 24 10 03 	movdqa %xmm2,0x310(%rsp)
  402cb8:	00 00 
  402cba:	66 0f 7f 8c 24 c0 02 	movdqa %xmm1,0x2c0(%rsp)
  402cc1:	00 00 
  402cc3:	66 0f 7f 84 24 70 02 	movdqa %xmm0,0x270(%rsp)
  402cca:	00 00 
  402ccc:	48 8b 84 24 58 02 00 	mov    0x258(%rsp),%rax
  402cd3:	00 
  402cd4:	48 83 e8 02          	sub    $0x2,%rax
  402cd8:	48 89 84 24 58 02 00 	mov    %rax,0x258(%rsp)
  402cdf:	00 
  402ce0:	e9 0f fb ff ff       	jmp    4027f4 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128+0x434>
  402ce5:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402cec:	00 
  402ced:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  402cf4:	00 
  402cf5:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  402cfc:	00 
  402cfd:	0f 28 84 24 50 03 00 	movaps 0x350(%rsp),%xmm0
  402d04:	00 
  402d05:	0f 11 00             	movups %xmm0,(%rax)
  402d08:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402d0f:	00 
  402d10:	48 83 c0 10          	add    $0x10,%rax
  402d14:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  402d1b:	00 
  402d1c:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  402d23:	00 
  402d24:	0f 28 84 24 40 03 00 	movaps 0x340(%rsp),%xmm0
  402d2b:	00 
  402d2c:	0f 11 00             	movups %xmm0,(%rax)
  402d2f:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402d36:	00 
  402d37:	48 83 c0 20          	add    $0x20,%rax
  402d3b:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  402d42:	00 
  402d43:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  402d4a:	00 
  402d4b:	0f 28 84 24 30 03 00 	movaps 0x330(%rsp),%xmm0
  402d52:	00 
  402d53:	0f 11 00             	movups %xmm0,(%rax)
  402d56:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402d5d:	00 
  402d5e:	48 83 c0 30          	add    $0x30,%rax
  402d62:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  402d69:	00 
  402d6a:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  402d71:	00 
  402d72:	0f 28 84 24 20 03 00 	movaps 0x320(%rsp),%xmm0
  402d79:	00 
  402d7a:	0f 11 00             	movups %xmm0,(%rax)
  402d7d:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402d84:	00 
  402d85:	48 83 c0 40          	add    $0x40,%rax
  402d89:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  402d90:	00 
  402d91:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  402d98:	00 
  402d99:	0f 28 84 24 10 03 00 	movaps 0x310(%rsp),%xmm0
  402da0:	00 
  402da1:	0f 28 8c 24 b0 04 00 	movaps 0x4b0(%rsp),%xmm1
  402da8:	00 
  402da9:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402dad:	0f 11 00             	movups %xmm0,(%rax)
  402db0:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402db7:	00 
  402db8:	48 83 c0 50          	add    $0x50,%rax
  402dbc:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  402dc3:	00 
  402dc4:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  402dcb:	00 
  402dcc:	0f 28 84 24 00 03 00 	movaps 0x300(%rsp),%xmm0
  402dd3:	00 
  402dd4:	0f 28 8c 24 90 04 00 	movaps 0x490(%rsp),%xmm1
  402ddb:	00 
  402ddc:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402de0:	0f 11 00             	movups %xmm0,(%rax)
  402de3:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402dea:	00 
  402deb:	48 83 c0 60          	add    $0x60,%rax
  402def:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  402df6:	00 
  402df7:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  402dfe:	00 
  402dff:	0f 28 84 24 f0 02 00 	movaps 0x2f0(%rsp),%xmm0
  402e06:	00 
  402e07:	0f 28 8c 24 70 04 00 	movaps 0x470(%rsp),%xmm1
  402e0e:	00 
  402e0f:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402e13:	0f 11 00             	movups %xmm0,(%rax)
  402e16:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402e1d:	00 
  402e1e:	48 83 c0 70          	add    $0x70,%rax
  402e22:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  402e29:	00 
  402e2a:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  402e31:	00 
  402e32:	0f 28 84 24 e0 02 00 	movaps 0x2e0(%rsp),%xmm0
  402e39:	00 
  402e3a:	0f 28 8c 24 50 04 00 	movaps 0x450(%rsp),%xmm1
  402e41:	00 
  402e42:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402e46:	0f 11 00             	movups %xmm0,(%rax)
  402e49:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402e50:	00 
  402e51:	48 83 e8 80          	sub    $0xffffffffffffff80,%rax
  402e55:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  402e5c:	00 
  402e5d:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  402e64:	00 
  402e65:	0f 28 84 24 d0 02 00 	movaps 0x2d0(%rsp),%xmm0
  402e6c:	00 
  402e6d:	0f 28 8c 24 30 04 00 	movaps 0x430(%rsp),%xmm1
  402e74:	00 
  402e75:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402e79:	0f 11 00             	movups %xmm0,(%rax)
  402e7c:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402e83:	00 
  402e84:	48 05 90 00 00 00    	add    $0x90,%rax
  402e8a:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  402e91:	00 
  402e92:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  402e99:	00 
  402e9a:	0f 28 84 24 c0 02 00 	movaps 0x2c0(%rsp),%xmm0
  402ea1:	00 
  402ea2:	0f 28 8c 24 10 04 00 	movaps 0x410(%rsp),%xmm1
  402ea9:	00 
  402eaa:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402eae:	0f 11 00             	movups %xmm0,(%rax)
  402eb1:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402eb8:	00 
  402eb9:	48 05 a0 00 00 00    	add    $0xa0,%rax
  402ebf:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  402ec4:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  402ec9:	0f 28 84 24 b0 02 00 	movaps 0x2b0(%rsp),%xmm0
  402ed0:	00 
  402ed1:	0f 28 8c 24 f0 03 00 	movaps 0x3f0(%rsp),%xmm1
  402ed8:	00 
  402ed9:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402edd:	0f 11 00             	movups %xmm0,(%rax)
  402ee0:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402ee7:	00 
  402ee8:	48 05 b0 00 00 00    	add    $0xb0,%rax
  402eee:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  402ef3:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  402ef8:	0f 28 84 24 a0 02 00 	movaps 0x2a0(%rsp),%xmm0
  402eff:	00 
  402f00:	0f 28 8c 24 d0 03 00 	movaps 0x3d0(%rsp),%xmm1
  402f07:	00 
  402f08:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402f0c:	0f 11 00             	movups %xmm0,(%rax)
  402f0f:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402f16:	00 
  402f17:	48 05 c0 00 00 00    	add    $0xc0,%rax
  402f1d:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  402f22:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  402f27:	0f 28 84 24 90 02 00 	movaps 0x290(%rsp),%xmm0
  402f2e:	00 
  402f2f:	0f 11 00             	movups %xmm0,(%rax)
  402f32:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402f39:	00 
  402f3a:	48 05 d0 00 00 00    	add    $0xd0,%rax
  402f40:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  402f45:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  402f4a:	0f 28 84 24 80 02 00 	movaps 0x280(%rsp),%xmm0
  402f51:	00 
  402f52:	0f 28 8c 24 b0 03 00 	movaps 0x3b0(%rsp),%xmm1
  402f59:	00 
  402f5a:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402f5e:	0f 11 00             	movups %xmm0,(%rax)
  402f61:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402f68:	00 
  402f69:	48 05 e0 00 00 00    	add    $0xe0,%rax
  402f6f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  402f74:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  402f79:	0f 28 84 24 70 02 00 	movaps 0x270(%rsp),%xmm0
  402f80:	00 
  402f81:	0f 28 8c 24 a0 03 00 	movaps 0x3a0(%rsp),%xmm1
  402f88:	00 
  402f89:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402f8d:	0f 11 00             	movups %xmm0,(%rax)
  402f90:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402f97:	00 
  402f98:	48 05 f0 00 00 00    	add    $0xf0,%rax
  402f9e:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  402fa3:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  402fa8:	0f 28 84 24 60 02 00 	movaps 0x260(%rsp),%xmm0
  402faf:	00 
  402fb0:	0f 28 8c 24 90 03 00 	movaps 0x390(%rsp),%xmm1
  402fb7:	00 
  402fb8:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402fbc:	0f 11 00             	movups %xmm0,(%rax)
  402fbf:	0f 28 84 24 c0 03 00 	movaps 0x3c0(%rsp),%xmm0
  402fc6:	00 
  402fc7:	0f 28 0d 82 c2 00 00 	movaps 0xc282(%rip),%xmm1        # 40f250 <_IO_stdin_used+0x250>
  402fce:	66 0f fe c1          	paddd  %xmm1,%xmm0
  402fd2:	66 0f 7f 84 24 c0 03 	movdqa %xmm0,0x3c0(%rsp)
  402fd9:	00 00 
  402fdb:	48 8b 84 24 78 03 00 	mov    0x378(%rsp),%rax
  402fe2:	00 
  402fe3:	48 05 00 01 00 00    	add    $0x100,%rax
  402fe9:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  402fee:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  402ff3:	48 89 84 24 78 03 00 	mov    %rax,0x378(%rsp)
  402ffa:	00 
  402ffb:	48 8b 84 24 68 03 00 	mov    0x368(%rsp),%rax
  403002:	00 
  403003:	48 83 c0 01          	add    $0x1,%rax
  403007:	48 89 84 24 68 03 00 	mov    %rax,0x368(%rsp)
  40300e:	00 
  40300f:	48 8b 84 24 60 03 00 	mov    0x360(%rsp),%rax
  403016:	00 
  403017:	48 83 c0 01          	add    $0x1,%rax
  40301b:	48 89 84 24 60 03 00 	mov    %rax,0x360(%rsp)
  403022:	00 
  403023:	e9 9d f6 ff ff       	jmp    4026c5 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128+0x305>
  403028:	48 81 c4 18 05 00 00 	add    $0x518,%rsp
  40302f:	c3                   	ret

0000000000403030 <_proclit$anon-1>:
  403030:	48 83 ec 68          	sub    $0x68,%rsp
  403034:	48 89 54 24 88       	mov    %rdx,-0x78(%rsp)
  403039:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  40303e:	48 89 7c 24 98       	mov    %rdi,-0x68(%rsp)
  403043:	0f 29 5c 24 a0       	movaps %xmm3,-0x60(%rsp)
  403048:	0f 29 54 24 b0       	movaps %xmm2,-0x50(%rsp)
  40304d:	0f 29 4c 24 c0       	movaps %xmm1,-0x40(%rsp)
  403052:	0f 29 44 24 d0       	movaps %xmm0,-0x30(%rsp)
  403057:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40305c:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  403061:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  403066:	0f 28 44 24 a0       	movaps -0x60(%rsp),%xmm0
  40306b:	0f 28 4c 24 b0       	movaps -0x50(%rsp),%xmm1
  403070:	0f 28 54 24 c0       	movaps -0x40(%rsp),%xmm2
  403075:	0f 28 5c 24 d0       	movaps -0x30(%rsp),%xmm3
  40307a:	0f 29 5c 24 50       	movaps %xmm3,0x50(%rsp)
  40307f:	0f 29 54 24 40       	movaps %xmm2,0x40(%rsp)
  403084:	0f 29 4c 24 30       	movaps %xmm1,0x30(%rsp)
  403089:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  40308e:	0f 29 5c 24 10       	movaps %xmm3,0x10(%rsp)
  403093:	0f 29 14 24          	movaps %xmm2,(%rsp)
  403097:	0f 29 4c 24 f0       	movaps %xmm1,-0x10(%rsp)
  40309c:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  4030a1:	0f 28 44 24 10       	movaps 0x10(%rsp),%xmm0
  4030a6:	0f 28 0c 24          	movaps (%rsp),%xmm1
  4030aa:	66 0f fe c1          	paddd  %xmm1,%xmm0
  4030ae:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  4030b3:	0f 28 44 24 e0       	movaps -0x20(%rsp),%xmm0
  4030b8:	0f 28 4c 24 10       	movaps 0x10(%rsp),%xmm1
  4030bd:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4030c1:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  4030c6:	0f 28 44 24 e0       	movaps -0x20(%rsp),%xmm0
  4030cb:	0f 28 0d 8e c1 00 00 	movaps 0xc18e(%rip),%xmm1        # 40f260 <_IO_stdin_used+0x260>
  4030d2:	66 0f 38 00 c1       	pshufb %xmm1,%xmm0
  4030d7:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  4030dc:	0f 28 44 24 f0       	movaps -0x10(%rsp),%xmm0
  4030e1:	0f 28 4c 24 e0       	movaps -0x20(%rsp),%xmm1
  4030e6:	66 0f fe c1          	paddd  %xmm1,%xmm0
  4030ea:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  4030ef:	0f 28 04 24          	movaps (%rsp),%xmm0
  4030f3:	0f 28 4c 24 f0       	movaps -0x10(%rsp),%xmm1
  4030f8:	66 0f ef c1          	pxor   %xmm1,%xmm0
  4030fc:	0f 29 04 24          	movaps %xmm0,(%rsp)
  403100:	0f 28 04 24          	movaps (%rsp),%xmm0
  403104:	0f 28 c8             	movaps %xmm0,%xmm1
  403107:	66 0f 72 d1 14       	psrld  $0x14,%xmm1
  40310c:	66 0f 72 f0 0c       	pslld  $0xc,%xmm0
  403111:	66 0f eb c1          	por    %xmm1,%xmm0
  403115:	0f 29 04 24          	movaps %xmm0,(%rsp)
  403119:	0f 28 44 24 10       	movaps 0x10(%rsp),%xmm0
  40311e:	0f 28 0c 24          	movaps (%rsp),%xmm1
  403122:	66 0f fe c1          	paddd  %xmm1,%xmm0
  403126:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  40312b:	0f 28 44 24 e0       	movaps -0x20(%rsp),%xmm0
  403130:	0f 28 4c 24 10       	movaps 0x10(%rsp),%xmm1
  403135:	66 0f ef c1          	pxor   %xmm1,%xmm0
  403139:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  40313e:	0f 28 44 24 e0       	movaps -0x20(%rsp),%xmm0
  403143:	0f 28 0d 26 c1 00 00 	movaps 0xc126(%rip),%xmm1        # 40f270 <_IO_stdin_used+0x270>
  40314a:	66 0f 38 00 c1       	pshufb %xmm1,%xmm0
  40314f:	0f 29 44 24 e0       	movaps %xmm0,-0x20(%rsp)
  403154:	0f 28 44 24 f0       	movaps -0x10(%rsp),%xmm0
  403159:	0f 28 4c 24 e0       	movaps -0x20(%rsp),%xmm1
  40315e:	66 0f fe c1          	paddd  %xmm1,%xmm0
  403162:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  403167:	0f 28 04 24          	movaps (%rsp),%xmm0
  40316b:	0f 28 4c 24 f0       	movaps -0x10(%rsp),%xmm1
  403170:	66 0f ef c1          	pxor   %xmm1,%xmm0
  403174:	0f 29 04 24          	movaps %xmm0,(%rsp)
  403178:	0f 28 0c 24          	movaps (%rsp),%xmm1
  40317c:	0f 28 c1             	movaps %xmm1,%xmm0
  40317f:	66 0f 72 f0 07       	pslld  $0x7,%xmm0
  403184:	66 0f 72 d1 19       	psrld  $0x19,%xmm1
  403189:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40318d:	66 0f 7f 04 24       	movdqa %xmm0,(%rsp)
  403192:	66 0f 6f 5c 24 10    	movdqa 0x10(%rsp),%xmm3
  403198:	66 0f 6f 14 24       	movdqa (%rsp),%xmm2
  40319d:	66 0f 6f 4c 24 f0    	movdqa -0x10(%rsp),%xmm1
  4031a3:	66 0f 6f 44 24 e0    	movdqa -0x20(%rsp),%xmm0
  4031a9:	66 0f 7f 1a          	movdqa %xmm3,(%rdx)
  4031ad:	66 0f 7f 11          	movdqa %xmm2,(%rcx)
  4031b1:	66 0f 7f 08          	movdqa %xmm1,(%rax)
  4031b5:	48 83 c4 68          	add    $0x68,%rsp
  4031b9:	c3                   	ret
  4031ba:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004031c0 <runtime::[heap_allocator_unix.odin]::_heap_alloc>:
  4031c0:	48 83 ec 18          	sub    $0x18,%rsp
  4031c4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4031c8:	40 88 f0             	mov    %sil,%al
  4031cb:	88 44 24 0e          	mov    %al,0xe(%rsp)
  4031cf:	48 8b 04 24          	mov    (%rsp),%rax
  4031d3:	8a 4c 24 0e          	mov    0xe(%rsp),%cl
  4031d7:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4031dc:	88 4c 24 0f          	mov    %cl,0xf(%rsp)
  4031e0:	48 83 f8 00          	cmp    $0x0,%rax
  4031e4:	0f 9e c0             	setle  %al
  4031e7:	24 01                	and    $0x1,%al
  4031e9:	3c 00                	cmp    $0x0,%al
  4031eb:	74 07                	je     4031f4 <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x34>
  4031ed:	31 c0                	xor    %eax,%eax
  4031ef:	48 83 c4 18          	add    $0x18,%rsp
  4031f3:	c3                   	ret
  4031f4:	8a 44 24 0e          	mov    0xe(%rsp),%al
  4031f8:	3c 00                	cmp    $0x0,%al
  4031fa:	74 13                	je     40320f <runtime::[heap_allocator_unix.odin]::_heap_alloc+0x4f>
  4031fc:	48 8b 34 24          	mov    (%rsp),%rsi
  403200:	bf 01 00 00 00       	mov    $0x1,%edi
  403205:	e8 46 de ff ff       	call   401050 <calloc@plt>
  40320a:	48 83 c4 18          	add    $0x18,%rsp
  40320e:	c3                   	ret
  40320f:	48 8b 3c 24          	mov    (%rsp),%rdi
  403213:	e8 58 de ff ff       	call   401070 <malloc@plt>
  403218:	48 83 c4 18          	add    $0x18,%rsp
  40321c:	c3                   	ret
  40321d:	0f 1f 00             	nopl   (%rax)

0000000000403220 <runtime::[heap_allocator_unix.odin]::_heap_resize>:
  403220:	48 83 ec 28          	sub    $0x28,%rsp
  403224:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403229:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40322e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403233:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  403238:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40323d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403242:	e8 39 de ff ff       	call   401080 <realloc@plt>
  403247:	48 83 c4 28          	add    $0x28,%rsp
  40324b:	c3                   	ret
  40324c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403250 <runtime::[heap_allocator_unix.odin]::_heap_free>:
  403250:	48 83 ec 18          	sub    $0x18,%rsp
  403254:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  403259:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40325e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  403263:	e8 c8 dd ff ff       	call   401030 <free@plt>
  403268:	48 83 c4 18          	add    $0x18,%rsp
  40326c:	c3                   	ret
  40326d:	0f 1f 00             	nopl   (%rax)

0000000000403270 <journey::main>:
  403270:	55                   	push   %rbp
  403271:	48 89 e5             	mov    %rsp,%rbp
  403274:	41 57                	push   %r15
  403276:	41 56                	push   %r14
  403278:	41 55                	push   %r13
  40327a:	41 54                	push   %r12
  40327c:	53                   	push   %rbx
  40327d:	48 83 e4 c0          	and    $0xffffffffffffffc0,%rsp
  403281:	48 81 ec 80 02 00 00 	sub    $0x280,%rsp
  403288:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  40328d:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  403292:	c7 84 24 44 02 00 00 	movl   $0x40e00000,0x244(%rsp)
  403299:	00 00 e0 40 
  40329d:	c7 84 24 40 02 00 00 	movl   $0x40a00000,0x240(%rsp)
  4032a4:	00 00 a0 40 
  4032a8:	c7 84 24 3c 02 00 00 	movl   $0x42c80000,0x23c(%rsp)
  4032af:	00 00 c8 42 
  4032b3:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  4032ba:	00 
  4032bb:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4032c0:	e8 b4 02 00 00       	call   403579 <journey::init_world:proc(world:^journey::World(THREAD_COUNT:$$4),unique_data_capacity:$$28)>
  4032c5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4032ca:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4032cf:	e8 8f 03 00 00       	call   403663 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)>
  4032d4:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4032d9:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4032de:	e8 3b 04 00 00       	call   40371e <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$2,start:$$0,end:$$1)>
  4032e3:	48 8b 7c 24 48       	mov    0x48(%rsp),%rdi
  4032e8:	f3 0f 10 84 24 40 02 	movss  0x240(%rsp),%xmm0
  4032ef:	00 00 
  4032f1:	e8 9a 04 00 00       	call   403790 <journey::build_sym_predicate:proc(data_type:$journey::main::Position::$1,field_name:$$"x",cmp_op:$$2,val:f32,comb_op:$$2)->(:journey::SystemPredicate)>
  4032f6:	48 8b 7c 24 48       	mov    0x48(%rsp),%rdi
  4032fb:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  403302:	00 
  403303:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  40330a:	00 
  40330b:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403312:	00 
  403313:	66 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%cx
  40331a:	00 
  40331b:	8a 94 24 ba 00 00 00 	mov    0xba(%rsp),%dl
  403322:	40 8a b4 24 bb 00 00 	mov    0xbb(%rsp),%sil
  403329:	00 
  40332a:	44 8a 84 24 bc 00 00 	mov    0xbc(%rsp),%r8b
  403331:	00 
  403332:	44 8a 8c 24 bd 00 00 	mov    0xbd(%rsp),%r9b
  403339:	00 
  40333a:	44 8a 94 24 be 00 00 	mov    0xbe(%rsp),%r10b
  403341:	00 
  403342:	44 8a 9c 24 bf 00 00 	mov    0xbf(%rsp),%r11b
  403349:	00 
  40334a:	0f 57 c0             	xorps  %xmm0,%xmm0
  40334d:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  403352:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  403359:	00 
  40335a:	44 88 9c 24 af 00 00 	mov    %r11b,0xaf(%rsp)
  403361:	00 
  403362:	44 88 94 24 ae 00 00 	mov    %r10b,0xae(%rsp)
  403369:	00 
  40336a:	44 88 8c 24 ad 00 00 	mov    %r9b,0xad(%rsp)
  403371:	00 
  403372:	44 88 84 24 ac 00 00 	mov    %r8b,0xac(%rsp)
  403379:	00 
  40337a:	40 88 b4 24 ab 00 00 	mov    %sil,0xab(%rsp)
  403381:	00 
  403382:	88 94 24 aa 00 00 00 	mov    %dl,0xaa(%rsp)
  403389:	66 89 8c 24 a8 00 00 	mov    %cx,0xa8(%rsp)
  403390:	00 
  403391:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  403398:	00 
  403399:	f3 0f 10 84 24 44 02 	movss  0x244(%rsp),%xmm0
  4033a0:	00 00 
  4033a2:	e8 39 04 00 00       	call   4037e0 <journey::build_sym_predicate:proc(data_type:$journey::main::Position::$1,field_name:$$"y",cmp_op:$$2,val:f32,comb_op:$$0)->(:journey::SystemPredicate)>
  4033a7:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  4033ac:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  4033b3:	00 
  4033b4:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4033bb:	00 
  4033bc:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  4033c3:	00 
  4033c4:	66 8b 8c 24 98 00 00 	mov    0x98(%rsp),%cx
  4033cb:	00 
  4033cc:	8a 94 24 9a 00 00 00 	mov    0x9a(%rsp),%dl
  4033d3:	40 8a b4 24 9b 00 00 	mov    0x9b(%rsp),%sil
  4033da:	00 
  4033db:	40 8a bc 24 9c 00 00 	mov    0x9c(%rsp),%dil
  4033e2:	00 
  4033e3:	44 8a 84 24 9d 00 00 	mov    0x9d(%rsp),%r8b
  4033ea:	00 
  4033eb:	44 8a 8c 24 9e 00 00 	mov    0x9e(%rsp),%r9b
  4033f2:	00 
  4033f3:	44 8a 94 24 9f 00 00 	mov    0x9f(%rsp),%r10b
  4033fa:	00 
  4033fb:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  403402:	00 
  403403:	44 88 94 24 8f 00 00 	mov    %r10b,0x8f(%rsp)
  40340a:	00 
  40340b:	44 88 8c 24 8e 00 00 	mov    %r9b,0x8e(%rsp)
  403412:	00 
  403413:	44 88 84 24 8d 00 00 	mov    %r8b,0x8d(%rsp)
  40341a:	00 
  40341b:	40 88 bc 24 8c 00 00 	mov    %dil,0x8c(%rsp)
  403422:	00 
  403423:	40 88 b4 24 8b 00 00 	mov    %sil,0x8b(%rsp)
  40342a:	00 
  40342b:	88 94 24 8a 00 00 00 	mov    %dl,0x8a(%rsp)
  403432:	66 89 8c 24 88 00 00 	mov    %cx,0x88(%rsp)
  403439:	00 
  40343a:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  403441:	00 
  403442:	48 8b 9c 24 a0 00 00 	mov    0xa0(%rsp),%rbx
  403449:	00 
  40344a:	66 44 8b b4 24 a8 00 	mov    0xa8(%rsp),%r14w
  403451:	00 00 
  403453:	44 8a bc 24 aa 00 00 	mov    0xaa(%rsp),%r15b
  40345a:	00 
  40345b:	44 8a a4 24 ab 00 00 	mov    0xab(%rsp),%r12b
  403462:	00 
  403463:	44 8a ac 24 ac 00 00 	mov    0xac(%rsp),%r13b
  40346a:	00 
  40346b:	44 8a 8c 24 ad 00 00 	mov    0xad(%rsp),%r9b
  403472:	00 
  403473:	8a 84 24 ae 00 00 00 	mov    0xae(%rsp),%al
  40347a:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  40347e:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  403485:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  40348c:	00 
  40348d:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403492:	66 8b 8c 24 88 00 00 	mov    0x88(%rsp),%cx
  403499:	00 
  40349a:	8a 94 24 8a 00 00 00 	mov    0x8a(%rsp),%dl
  4034a1:	40 8a b4 24 8b 00 00 	mov    0x8b(%rsp),%sil
  4034a8:	00 
  4034a9:	40 8a bc 24 8c 00 00 	mov    0x8c(%rsp),%dil
  4034b0:	00 
  4034b1:	44 8a 84 24 8d 00 00 	mov    0x8d(%rsp),%r8b
  4034b8:	00 
  4034b9:	44 8a 94 24 8e 00 00 	mov    0x8e(%rsp),%r10b
  4034c0:	00 
  4034c1:	44 8a 9c 24 8f 00 00 	mov    0x8f(%rsp),%r11b
  4034c8:	00 
  4034c9:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  4034ce:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  4034d3:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  4034d8:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  4034dc:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  4034e0:	88 44 24 5e          	mov    %al,0x5e(%rsp)
  4034e4:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4034e9:	44 88 4c 24 5d       	mov    %r9b,0x5d(%rsp)
  4034ee:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  4034f3:	44 88 6c 24 5c       	mov    %r13b,0x5c(%rsp)
  4034f8:	44 88 64 24 5b       	mov    %r12b,0x5b(%rsp)
  4034fd:	44 88 7c 24 5a       	mov    %r15b,0x5a(%rsp)
  403502:	66 44 89 74 24 58    	mov    %r14w,0x58(%rsp)
  403508:	48 89 5c 24 50       	mov    %rbx,0x50(%rsp)
  40350d:	44 88 5c 24 6f       	mov    %r11b,0x6f(%rsp)
  403512:	44 88 54 24 6e       	mov    %r10b,0x6e(%rsp)
  403517:	44 88 44 24 6d       	mov    %r8b,0x6d(%rsp)
  40351c:	40 88 7c 24 6c       	mov    %dil,0x6c(%rsp)
  403521:	40 88 74 24 6b       	mov    %sil,0x6b(%rsp)
  403526:	88 54 24 6a          	mov    %dl,0x6a(%rsp)
  40352a:	66 89 4c 24 68       	mov    %cx,0x68(%rsp)
  40352f:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  403534:	48 8d 44 24 50       	lea    0x50(%rsp),%rax
  403539:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40353e:	48 c7 44 24 78 02 00 	movq   $0x2,0x78(%rsp)
  403545:	00 00 
  403547:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40354c:	4c 8b 44 24 78       	mov    0x78(%rsp),%r8
  403551:	48 c7 c2 c0 38 40 00 	mov    $0x4038c0,%rdx
  403558:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  40355f:	00 
  403560:	be 01 00 00 00       	mov    $0x1,%esi
  403565:	e8 c1 02 00 00       	call   40382b <journey::run_0:proc(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>
  40356a:	48 8d 65 d8          	lea    -0x28(%rbp),%rsp
  40356e:	5b                   	pop    %rbx
  40356f:	41 5c                	pop    %r12
  403571:	41 5d                	pop    %r13
  403573:	41 5e                	pop    %r14
  403575:	41 5f                	pop    %r15
  403577:	5d                   	pop    %rbp
  403578:	c3                   	ret

0000000000403579 <journey::init_world:proc(world:^journey::World(THREAD_COUNT:$$4),unique_data_capacity:$$28)>:
  403579:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  40357e:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  403583:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  403588:	31 c0                	xor    %eax,%eax
  40358a:	41 89 c1             	mov    %eax,%r9d
  40358d:	b8 09 00 00 00       	mov    $0x9,%eax
  403592:	be 00 e0 01 00       	mov    $0x1e000,%esi
  403597:	ba 03 00 00 00       	mov    $0x3,%edx
  40359c:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  4035a2:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  4035a9:	4c 89 cf             	mov    %r9,%rdi
  4035ac:	0f 05                	syscall
  4035ae:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4035b3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4035b8:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4035bd:	48 89 08             	mov    %rcx,(%rax)
  4035c0:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4035c5:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4035ca:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  4035d1:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4035d5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4035da:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  4035df:	48 81 c1 00 20 00 00 	add    $0x2000,%rcx
  4035e6:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4035ea:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4035ef:	48 c7 40 18 1c 00 00 	movq   $0x1c,0x18(%rax)
  4035f6:	00 
  4035f7:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4035fc:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  403601:	b8 ba 00 00 00       	mov    $0xba,%eax
  403606:	0f 05                	syscall
  403608:	48 89 c1             	mov    %rax,%rcx
  40360b:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  403610:	48 89 48 20          	mov    %rcx,0x20(%rax)
  403614:	31 c0                	xor    %eax,%eax
  403616:	41 89 c1             	mov    %eax,%r9d
  403619:	b8 09 00 00 00       	mov    $0x9,%eax
  40361e:	be 00 10 02 00       	mov    $0x21000,%esi
  403623:	ba 03 00 00 00       	mov    $0x3,%edx
  403628:	41 ba 21 00 00 00    	mov    $0x21,%r10d
  40362e:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  403635:	4c 89 cf             	mov    %r9,%rdi
  403638:	0f 05                	syscall
  40363a:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40363f:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403644:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  403649:	48 89 48 28          	mov    %rcx,0x28(%rax)
  40364d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403652:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  403657:	48 81 c1 00 10 00 00 	add    $0x1000,%rcx
  40365e:	48 89 48 30          	mov    %rcx,0x30(%rax)
  403662:	c3                   	ret

0000000000403663 <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)>:
  403663:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  403668:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40366d:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  403672:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403677:	48 8b 00             	mov    (%rax),%rax
  40367a:	c7 40 1c 11 33 00 00 	movl   $0x3311,0x1c(%rax)
  403681:	c7 40 18 35 01 00 00 	movl   $0x135,0x18(%rax)
  403688:	c7 40 14 15 00 00 00 	movl   $0x15,0x14(%rax)
  40368f:	c7 40 10 08 00 00 00 	movl   $0x8,0x10(%rax)
  403696:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40369b:	48 8b 40 08          	mov    0x8(%rax),%rax
  40369f:	c6 40 03 ef          	movb   $0xef,0x3(%rax)
  4036a3:	c6 40 02 0f          	movb   $0xf,0x2(%rax)
  4036a7:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4036ac:	48 8b 40 10          	mov    0x10(%rax),%rax
  4036b0:	48 05 00 10 00 00    	add    $0x1000,%rax
  4036b6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4036bb:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4036c2:	00 00 
  4036c4:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  4036cb:	00 00 
  4036cd:	48 83 7c 24 e8 04    	cmpq   $0x4,-0x18(%rsp)
  4036d3:	73 3a                	jae    40370f <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)+0xac>
  4036d5:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4036da:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4036df:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4036e4:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4036e9:	48 c7 04 c8 ff ff ff 	movq   $0xffffffffffffffff,(%rax,%rcx,8)
  4036f0:	ff 
  4036f1:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4036f6:	48 83 c0 01          	add    $0x1,%rax
  4036fa:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4036ff:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  403704:	48 83 c0 01          	add    $0x1,%rax
  403708:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40370d:	eb be                	jmp    4036cd <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$1,start:$$21,end:$$300)+0x6a>
  40370f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  403714:	b9 ff ff ff ff       	mov    $0xffffffff,%ecx
  403719:	48 89 48 20          	mov    %rcx,0x20(%rax)
  40371d:	c3                   	ret

000000000040371e <journey::register_columnar:proc(world:^journey::World(THREAD_COUNT:$$4),data_typeid:$journey::main::Position::$1,table_index:$$2,start:$$0,end:$$1)>:
  40371e:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  403723:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  403728:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40372d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403732:	48 8b 00             	mov    (%rax),%rax
  403735:	c7 40 2c 11 33 00 00 	movl   $0x3311,0x2c(%rax)
  40373c:	c7 40 28 20 00 00 00 	movl   $0x20,0x28(%rax)
  403743:	c7 40 24 00 00 00 00 	movl   $0x0,0x24(%rax)
  40374a:	c7 40 20 08 00 00 00 	movl   $0x8,0x20(%rax)
  403751:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403756:	48 8b 40 08          	mov    0x8(%rax),%rax
  40375a:	c6 40 05 fe          	movb   $0xfe,0x5(%rax)
  40375e:	c6 40 04 00          	movb   $0x0,0x4(%rax)
  403762:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  403767:	48 8b 40 10          	mov    0x10(%rax),%rax
  40376b:	48 05 00 20 00 00    	add    $0x2000,%rax
  403771:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  403776:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40377b:	b9 ff ff ff ff       	mov    $0xffffffff,%ecx
  403780:	48 89 08             	mov    %rcx,(%rax)
  403783:	c3                   	ret
  403784:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40378b:	00 00 00 00 00 

0000000000403790 <journey::build_sym_predicate:proc(data_type:$journey::main::Position::$1,field_name:$$"x",cmp_op:$$2,val:f32,comb_op:$$2)->(:journey::SystemPredicate)>:
  403790:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  403796:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  40379c:	f3 0f 11 44 24 fc    	movss  %xmm0,-0x4(%rsp)
  4037a2:	c6 44 24 f7 00       	movb   $0x0,-0x9(%rsp)
  4037a7:	c6 44 24 f6 00       	movb   $0x0,-0xa(%rsp)
  4037ac:	c6 44 24 f5 00       	movb   $0x0,-0xb(%rsp)
  4037b1:	c6 44 24 f4 00       	movb   $0x0,-0xc(%rsp)
  4037b6:	c6 44 24 f3 00       	movb   $0x0,-0xd(%rsp)
  4037bb:	c6 44 24 f2 00       	movb   $0x0,-0xe(%rsp)
  4037c0:	66 c7 44 24 f0 00 00 	movw   $0x0,-0x10(%rsp)
  4037c7:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  4037ce:	00 00 
  4037d0:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4037d5:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  4037da:	c3                   	ret
  4037db:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004037e0 <journey::build_sym_predicate:proc(data_type:$journey::main::Position::$1,field_name:$$"y",cmp_op:$$2,val:f32,comb_op:$$0)->(:journey::SystemPredicate)>:
  4037e0:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  4037e6:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  4037ec:	f3 0f 11 44 24 fc    	movss  %xmm0,-0x4(%rsp)
  4037f2:	c6 44 24 f7 00       	movb   $0x0,-0x9(%rsp)
  4037f7:	c6 44 24 f6 00       	movb   $0x0,-0xa(%rsp)
  4037fc:	c6 44 24 f5 00       	movb   $0x0,-0xb(%rsp)
  403801:	c6 44 24 f4 00       	movb   $0x0,-0xc(%rsp)
  403806:	c6 44 24 f3 00       	movb   $0x0,-0xd(%rsp)
  40380b:	c6 44 24 f2 00       	movb   $0x0,-0xe(%rsp)
  403810:	66 c7 44 24 f0 00 00 	movw   $0x0,-0x10(%rsp)
  403817:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  40381e:	00 00 
  403820:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  403825:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40382a:	c3                   	ret

000000000040382b <journey::run_0:proc(world:^journey::World(THREAD_COUNT:$$4),table_index:$$1,thread_index:journey::QWORD,sysm:journey::sysm_proc,predicates:..journey::SystemPredicate)>:
  40382b:	48 83 ec 78          	sub    $0x78,%rsp
  40382f:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  403834:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  403839:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40383e:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  403843:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  403848:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40384d:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403852:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  403857:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40385c:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  403861:	48 89 7c 24 70       	mov    %rdi,0x70(%rsp)
  403866:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  40386b:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  403870:	48 89 54 24 58       	mov    %rdx,0x58(%rsp)
  403875:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  40387a:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40387f:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  403883:	48 81 c1 40 10 00 00 	add    $0x1040,%rcx
  40388a:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40388f:	48 c7 44 24 48 c0 0f 	movq   $0xfc0,0x48(%rsp)
  403896:	00 00 
  403898:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40389d:	31 c9                	xor    %ecx,%ecx
  40389f:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  4038a3:	89 4c 24 30          	mov    %ecx,0x30(%rsp)
  4038a7:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  4038ac:	ff d0                	call   *%rax
  4038ae:	48 83 c4 78          	add    $0x78,%rsp
  4038b2:	c3                   	ret
  4038b3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4038ba:	84 00 00 00 00 00 

00000000004038c0 <journey::main.readjust_npc_position-0>:
  4038c0:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4038c5:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  4038ca:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4038cf:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4038d4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4038d9:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  4038de:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4038e3:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  4038ea:	00 00 
  4038ec:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  4038f3:	00 00 
  4038f5:	48 83 7c 24 d8 01    	cmpq   $0x1,-0x28(%rsp)
  4038fb:	7d 46                	jge    403943 <journey::main.readjust_npc_position-0+0x83>
  4038fd:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  403902:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  403907:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40390c:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  403911:	48 c1 e1 06          	shl    $0x6,%rcx
  403915:	0f 28 05 64 b9 00 00 	movaps 0xb964(%rip),%xmm0        # 40f280 <_IO_stdin_used+0x280>
  40391c:	0f 29 44 08 10       	movaps %xmm0,0x10(%rax,%rcx,1)
  403921:	0f 29 04 08          	movaps %xmm0,(%rax,%rcx,1)
  403925:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40392a:	48 83 c0 01          	add    $0x1,%rax
  40392e:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  403933:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  403938:	48 83 c0 01          	add    $0x1,%rax
  40393c:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  403941:	eb b2                	jmp    4038f5 <journey::main.readjust_npc_position-0+0x35>
  403943:	c3                   	ret
  403944:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40394b:	00 00 00 
  40394e:	66 90                	xchg   %ax,%ax

0000000000403950 <main>:
  403950:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  403957:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  40395b:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403960:	8b 44 24 14          	mov    0x14(%rsp),%eax
  403964:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  403969:	89 84 24 24 01 00 00 	mov    %eax,0x124(%rsp)
  403970:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  403977:	00 
  403978:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  40397f:	00 
  403980:	48 89 0c 24          	mov    %rcx,(%rsp)
  403984:	4c 63 c8             	movslq %eax,%r9
  403987:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40398c:	bf f5 f2 40 00       	mov    $0x40f2f5,%edi
  403991:	31 c0                	xor    %eax,%eax
  403993:	41 89 c0             	mov    %eax,%r8d
  403996:	be 2a 00 00 00       	mov    $0x2a,%esi
  40399b:	ba 36 00 00 00       	mov    $0x36,%edx
  4039a0:	b9 11 00 00 00       	mov    $0x11,%ecx
  4039a5:	e8 76 e2 ff ff       	call   401c20 <runtime::multi_pointer_slice_expr_error>
  4039aa:	48 8b 0c 24          	mov    (%rsp),%rcx
  4039ae:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4039b3:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  4039ba:	00 
  4039bb:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  4039c2:	00 
  4039c3:	48 8b 8c 24 08 01 00 	mov    0x108(%rsp),%rcx
  4039ca:	00 
  4039cb:	48 8b 94 24 10 01 00 	mov    0x110(%rsp),%rdx
  4039d2:	00 
  4039d3:	48 c7 c0 a8 41 41 00 	mov    $0x4141a8,%rax
  4039da:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4039de:	48 89 08             	mov    %rcx,(%rax)
  4039e1:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4039e8:	00 
  4039e9:	31 f6                	xor    %esi,%esi
  4039eb:	ba 70 00 00 00       	mov    $0x70,%edx
  4039f0:	e8 4b d6 ff ff       	call   401040 <memset@plt>
  4039f5:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  4039fc:	00 
  4039fd:	e8 7e 0c 00 00       	call   404680 <runtime::[core.odin]::__init_context>
  403a02:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  403a07:	31 f6                	xor    %esi,%esi
  403a09:	ba 70 00 00 00       	mov    $0x70,%edx
  403a0e:	e8 2d d6 ff ff       	call   401040 <memset@plt>
  403a13:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
  403a18:	e8 13 0c 00 00       	call   404630 <runtime::default_context>
  403a1d:	0f 10 44 24 20       	movups 0x20(%rsp),%xmm0
  403a22:	0f 10 4c 24 30       	movups 0x30(%rsp),%xmm1
  403a27:	0f 10 54 24 40       	movups 0x40(%rsp),%xmm2
  403a2c:	0f 10 5c 24 50       	movups 0x50(%rsp),%xmm3
  403a31:	0f 10 64 24 60       	movups 0x60(%rsp),%xmm4
  403a36:	0f 10 6c 24 70       	movups 0x70(%rsp),%xmm5
  403a3b:	0f 10 b4 24 80 00 00 	movups 0x80(%rsp),%xmm6
  403a42:	00 
  403a43:	0f 29 b4 24 f0 00 00 	movaps %xmm6,0xf0(%rsp)
  403a4a:	00 
  403a4b:	0f 29 ac 24 e0 00 00 	movaps %xmm5,0xe0(%rsp)
  403a52:	00 
  403a53:	0f 29 a4 24 d0 00 00 	movaps %xmm4,0xd0(%rsp)
  403a5a:	00 
  403a5b:	0f 29 9c 24 c0 00 00 	movaps %xmm3,0xc0(%rsp)
  403a62:	00 
  403a63:	0f 29 94 24 b0 00 00 	movaps %xmm2,0xb0(%rsp)
  403a6a:	00 
  403a6b:	0f 29 8c 24 a0 00 00 	movaps %xmm1,0xa0(%rsp)
  403a72:	00 
  403a73:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  403a7a:	00 
  403a7b:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  403a82:	00 
  403a83:	e8 f8 89 00 00       	call   40c480 <__$startup_runtime>
  403a88:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  403a8f:	00 
  403a90:	e8 db f7 ff ff       	call   403270 <journey::main>
  403a95:	48 8d bc 24 90 00 00 	lea    0x90(%rsp),%rdi
  403a9c:	00 
  403a9d:	e8 3e 8a 00 00       	call   40c4e0 <__$cleanup_runtime>
  403aa2:	31 c0                	xor    %eax,%eax
  403aa4:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  403aab:	c3                   	ret
  403aac:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000403ab0 <runtime::default_random_generator_proc>:
  403ab0:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  403ab7:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  403abc:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  403ac1:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  403ac6:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  403acb:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  403ad0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403ad5:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403ada:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  403adf:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  403ae4:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  403aeb:	00 
  403aec:	48 89 b4 24 08 01 00 	mov    %rsi,0x108(%rsp)
  403af3:	00 
  403af4:	48 89 94 24 00 01 00 	mov    %rdx,0x100(%rsp)
  403afb:	00 
  403afc:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  403b03:	00 
  403b04:	48 c7 c2 48 fb ff ff 	mov    $0xfffffffffffffb48,%rdx
  403b0b:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  403b12:	00 00 
  403b14:	48 01 d1             	add    %rdx,%rcx
  403b17:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  403b1e:	00 
  403b1f:	48 83 f8 00          	cmp    $0x0,%rax
  403b23:	0f 95 c0             	setne  %al
  403b26:	24 01                	and    $0x1,%al
  403b28:	3c 00                	cmp    $0x0,%al
  403b2a:	74 0d                	je     403b39 <runtime::default_random_generator_proc+0x89>
  403b2c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  403b31:	48 89 84 24 f0 00 00 	mov    %rax,0xf0(%rsp)
  403b38:	00 
  403b39:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403b40:	00 
  403b41:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  403b46:	48 89 e0             	mov    %rsp,%rax
  403b49:	48 c7 00 00 04 00 00 	movq   $0x400,(%rax)
  403b50:	bf 20 f3 40 00       	mov    $0x40f320,%edi
  403b55:	be 38 00 00 00       	mov    $0x38,%esi
  403b5a:	ba 43 00 00 00       	mov    $0x43,%edx
  403b5f:	b9 15 00 00 00       	mov    $0x15,%ecx
  403b64:	41 b8 e0 03 00 00    	mov    $0x3e0,%r8d
  403b6a:	41 b9 00 04 00 00    	mov    $0x400,%r9d
  403b70:	e8 eb e1 ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  403b75:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  403b7a:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403b7f:	48 81 c1 e0 03 00 00 	add    $0x3e0,%rcx
  403b86:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  403b8d:	00 
  403b8e:	48 c7 84 24 e8 00 00 	movq   $0x20,0xe8(%rsp)
  403b95:	00 20 00 00 00 
  403b9a:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  403ba1:	00 
  403ba2:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  403ba9:	00 
  403baa:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  403bb1:	00 
  403bb2:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  403bb9:	00 
  403bba:	48 85 c0             	test   %rax,%rax
  403bbd:	74 27                	je     403be6 <runtime::default_random_generator_proc+0x136>
  403bbf:	eb 00                	jmp    403bc1 <runtime::default_random_generator_proc+0x111>
  403bc1:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403bc6:	48 83 e8 01          	sub    $0x1,%rax
  403bca:	0f 84 c5 03 00 00    	je     403f95 <runtime::default_random_generator_proc+0x4e5>
  403bd0:	eb 00                	jmp    403bd2 <runtime::default_random_generator_proc+0x122>
  403bd2:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  403bd7:	48 83 e8 02          	sub    $0x2,%rax
  403bdb:	0f 84 32 04 00 00    	je     404013 <runtime::default_random_generator_proc+0x563>
  403be1:	e9 5d 04 00 00       	jmp    404043 <runtime::default_random_generator_proc+0x593>
  403be6:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403bed:	00 
  403bee:	80 b8 08 04 00 00 00 	cmpb   $0x0,0x408(%rax)
  403bf5:	75 37                	jne    403c2e <runtime::default_random_generator_proc+0x17e>
  403bf7:	48 8b bc 24 d0 00 00 	mov    0xd0(%rsp),%rdi
  403bfe:	00 
  403bff:	48 8b b4 24 d8 00 00 	mov    0xd8(%rsp),%rsi
  403c06:	00 
  403c07:	e8 34 dd ff ff       	call   401940 <runtime::rand_bytes>
  403c0c:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403c13:	00 
  403c14:	48 c7 80 00 04 00 00 	movq   $0x3e0,0x400(%rax)
  403c1b:	e0 03 00 00 
  403c1f:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403c26:	00 
  403c27:	c6 80 08 04 00 00 01 	movb   $0x1,0x408(%rax)
  403c2e:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  403c33:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403c3a:	00 
  403c3b:	48 81 b8 00 04 00 00 	cmpq   $0x3e0,0x400(%rax)
  403c42:	e0 03 00 00 
  403c46:	0f 9e c0             	setle  %al
  403c49:	24 01                	and    $0x1,%al
  403c4b:	0f b6 f8             	movzbl %al,%edi
  403c4e:	be 59 f3 40 00       	mov    $0x40f359,%esi
  403c53:	b9 a0 f3 40 00       	mov    $0x40f3a0,%ecx
  403c58:	ba 26 00 00 00       	mov    $0x26,%edx
  403c5d:	e8 4e da ff ff       	call   4016b0 <runtime::assert>
  403c62:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403c69:	00 
  403c6a:	48 81 b8 00 04 00 00 	cmpq   $0x3e0,0x400(%rax)
  403c71:	e0 03 00 00 
  403c75:	0f 9d c0             	setge  %al
  403c78:	24 01                	and    $0x1,%al
  403c7a:	3c 00                	cmp    $0x0,%al
  403c7c:	74 12                	je     403c90 <runtime::default_random_generator_proc+0x1e0>
  403c7e:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403c83:	48 8b bc 24 f0 00 00 	mov    0xf0(%rsp),%rdi
  403c8a:	00 
  403c8b:	e8 c0 03 00 00       	call   404050 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>
  403c90:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  403c95:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403c9c:	00 
  403c9d:	48 81 b8 00 04 00 00 	cmpq   $0x3d8,0x400(%rax)
  403ca4:	d8 03 00 00 
  403ca8:	0f 9e c0             	setle  %al
  403cab:	24 01                	and    $0x1,%al
  403cad:	0f b6 f8             	movzbl %al,%edi
  403cb0:	be c8 f3 40 00       	mov    $0x40f3c8,%esi
  403cb5:	b9 00 f4 40 00       	mov    $0x40f400,%ecx
  403cba:	ba 36 00 00 00       	mov    $0x36,%edx
  403cbf:	e8 ec d9 ff ff       	call   4016b0 <runtime::assert>
  403cc4:	4c 8b 44 24 10       	mov    0x10(%rsp),%r8
  403cc9:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403cd0:	00 
  403cd1:	48 8b 80 00 04 00 00 	mov    0x400(%rax),%rax
  403cd8:	b9 08 00 00 00       	mov    $0x8,%ecx
  403cdd:	48 99                	cqto
  403cdf:	48 f7 f9             	idiv   %rcx
  403ce2:	48 83 fa 00          	cmp    $0x0,%rdx
  403ce6:	0f 94 c0             	sete   %al
  403ce9:	24 01                	and    $0x1,%al
  403ceb:	0f b6 f8             	movzbl %al,%edi
  403cee:	be 28 f4 40 00       	mov    $0x40f428,%esi
  403cf3:	b9 70 f4 40 00       	mov    $0x40f470,%ecx
  403cf8:	ba 3d 00 00 00       	mov    $0x3d,%edx
  403cfd:	e8 ae d9 ff ff       	call   4016b0 <runtime::assert>
  403d02:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403d07:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  403d0e:	00 
  403d0f:	48 83 bc 24 c8 00 00 	cmpq   $0x8,0xc8(%rsp)
  403d16:	00 08 
  403d18:	0f 94 c0             	sete   %al
  403d1b:	24 01                	and    $0x1,%al
  403d1d:	3c 00                	cmp    $0x0,%al
  403d1f:	0f 84 86 00 00 00    	je     403dab <runtime::default_random_generator_proc+0x2fb>
  403d25:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  403d2a:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  403d31:	00 
  403d32:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  403d39:	00 
  403d3a:	48 8b b1 00 04 00 00 	mov    0x400(%rcx),%rsi
  403d41:	48 01 f2             	add    %rsi,%rdx
  403d44:	b9 00 04 00 00       	mov    $0x400,%ecx
  403d49:	48 29 f1             	sub    %rsi,%rcx
  403d4c:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  403d53:	00 
  403d54:	48 89 8c 24 c0 00 00 	mov    %rcx,0xc0(%rsp)
  403d5b:	00 
  403d5c:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  403d63:	00 
  403d64:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  403d6b:	00 
  403d6c:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  403d73:	00 
  403d74:	48 8b 09             	mov    (%rcx),%rcx
  403d77:	48 89 08             	mov    %rcx,(%rax)
  403d7a:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  403d81:	00 
  403d82:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  403d89:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403d90:	00 
  403d91:	48 8b 88 00 04 00 00 	mov    0x400(%rax),%rcx
  403d98:	48 83 c1 08          	add    $0x8,%rcx
  403d9c:	48 89 88 00 04 00 00 	mov    %rcx,0x400(%rax)
  403da3:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  403daa:	c3                   	ret
  403dab:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403db0:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  403db5:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  403dbc:	00 
  403dbd:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  403dc4:	00 
  403dc5:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  403dcc:	00 
  403dcd:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  403dd4:	00 
  403dd5:	48 83 bc 24 98 00 00 	cmpq   $0x0,0x98(%rsp)
  403ddc:	00 00 
  403dde:	0f 9f c0             	setg   %al
  403de1:	24 01                	and    $0x1,%al
  403de3:	3c 00                	cmp    $0x0,%al
  403de5:	0f 84 a5 01 00 00    	je     403f90 <runtime::default_random_generator_proc+0x4e0>
  403deb:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  403df2:	00 
  403df3:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403dfa:	00 
  403dfb:	48 8b 90 00 04 00 00 	mov    0x400(%rax),%rdx
  403e02:	b8 e0 03 00 00       	mov    $0x3e0,%eax
  403e07:	48 29 d0             	sub    %rdx,%rax
  403e0a:	48 89 ca             	mov    %rcx,%rdx
  403e0d:	48 29 c2             	sub    %rax,%rdx
  403e10:	48 0f 4c c1          	cmovl  %rcx,%rax
  403e14:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  403e1b:	00 
  403e1c:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403e23:	00 
  403e24:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403e2b:	00 
  403e2c:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  403e33:	00 
  403e34:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  403e3b:	00 
  403e3c:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  403e43:	00 
  403e44:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  403e4b:	00 
  403e4c:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  403e53:	00 
  403e54:	48 8b 91 00 04 00 00 	mov    0x400(%rcx),%rdx
  403e5b:	48 01 d1             	add    %rdx,%rcx
  403e5e:	b8 00 04 00 00       	mov    $0x400,%eax
  403e63:	48 29 d0             	sub    %rdx,%rax
  403e66:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  403e6b:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  403e70:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  403e75:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  403e7a:	e8 a1 d3 ff ff       	call   401220 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  403e7f:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  403e86:	00 
  403e87:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  403e8e:	00 
  403e8f:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  403e96:	00 
  403e97:	48 01 d1             	add    %rdx,%rcx
  403e9a:	48 29 d0             	sub    %rdx,%rax
  403e9d:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  403ea2:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  403ea7:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  403eac:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  403eb1:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  403eb8:	00 
  403eb9:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  403ec0:	00 
  403ec1:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  403ec8:	00 
  403ec9:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  403ed0:	00 
  403ed1:	48 29 c8             	sub    %rcx,%rax
  403ed4:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  403edb:	00 
  403edc:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  403ee3:	00 
  403ee4:	48 83 c0 07          	add    $0x7,%rax
  403ee8:	b9 08 00 00 00       	mov    $0x8,%ecx
  403eed:	48 99                	cqto
  403eef:	48 f7 f9             	idiv   %rcx
  403ef2:	48 c1 e0 03          	shl    $0x3,%rax
  403ef6:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  403efb:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403f02:	00 
  403f03:	48 8b 80 00 04 00 00 	mov    0x400(%rax),%rax
  403f0a:	48 03 44 24 58       	add    0x58(%rsp),%rax
  403f0f:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  403f14:	48 81 7c 24 50 e0 03 	cmpq   $0x3e0,0x50(%rsp)
  403f1b:	00 00 
  403f1d:	0f 9c c0             	setl   %al
  403f20:	24 01                	and    $0x1,%al
  403f22:	3c 00                	cmp    $0x0,%al
  403f24:	74 53                	je     403f79 <runtime::default_random_generator_proc+0x4c9>
  403f26:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  403f2d:	00 
  403f2e:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403f35:	00 
  403f36:	48 8b 90 00 04 00 00 	mov    0x400(%rax),%rdx
  403f3d:	48 01 d1             	add    %rdx,%rcx
  403f40:	b8 00 04 00 00       	mov    $0x400,%eax
  403f45:	48 29 d0             	sub    %rdx,%rax
  403f48:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  403f4d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  403f52:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  403f57:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  403f5c:	31 f6                	xor    %esi,%esi
  403f5e:	e8 dd d0 ff ff       	call   401040 <memset@plt>
  403f63:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403f6a:	00 
  403f6b:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  403f70:	48 89 88 00 04 00 00 	mov    %rcx,0x400(%rax)
  403f77:	eb 12                	jmp    403f8b <runtime::default_random_generator_proc+0x4db>
  403f79:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  403f7e:	48 8b bc 24 f0 00 00 	mov    0xf0(%rsp),%rdi
  403f85:	00 
  403f86:	e8 c5 00 00 00       	call   404050 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>
  403f8b:	e9 45 fe ff ff       	jmp    403dd5 <runtime::default_random_generator_proc+0x325>
  403f90:	e9 ae 00 00 00       	jmp    404043 <runtime::default_random_generator_proc+0x593>
  403f95:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  403f9a:	48 83 f8 00          	cmp    $0x0,%rax
  403f9e:	0f 94 c0             	sete   %al
  403fa1:	24 01                	and    $0x1,%al
  403fa3:	3c 00                	cmp    $0x0,%al
  403fa5:	74 17                	je     403fbe <runtime::default_random_generator_proc+0x50e>
  403fa7:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403fae:	00 
  403faf:	c6 80 08 04 00 00 00 	movb   $0x0,0x408(%rax)
  403fb6:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  403fbd:	c3                   	ret
  403fbe:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  403fc3:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  403fc8:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  403fcf:	00 
  403fd0:	0f 57 c0             	xorps  %xmm0,%xmm0
  403fd3:	0f 11 40 10          	movups %xmm0,0x10(%rax)
  403fd7:	0f 11 00             	movups %xmm0,(%rax)
  403fda:	48 8b bc 24 d0 00 00 	mov    0xd0(%rsp),%rdi
  403fe1:	00 
  403fe2:	48 8b b4 24 d8 00 00 	mov    0xd8(%rsp),%rsi
  403fe9:	00 
  403fea:	e8 31 d2 ff ff       	call   401220 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  403fef:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  403ff6:	00 
  403ff7:	c6 80 08 04 00 00 01 	movb   $0x1,0x408(%rax)
  403ffe:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  404005:	00 
  404006:	48 c7 80 00 04 00 00 	movq   $0x3e0,0x400(%rax)
  40400d:	e0 03 00 00 
  404011:	eb 30                	jmp    404043 <runtime::default_random_generator_proc+0x593>
  404013:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  404018:	48 83 f8 04          	cmp    $0x4,%rax
  40401c:	0f 95 c0             	setne  %al
  40401f:	24 01                	and    $0x1,%al
  404021:	3c 00                	cmp    $0x0,%al
  404023:	74 08                	je     40402d <runtime::default_random_generator_proc+0x57d>
  404025:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40402c:	c3                   	ret
  40402d:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  404032:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404037:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40403c:	8b 08                	mov    (%rax),%ecx
  40403e:	83 c9 0b             	or     $0xb,%ecx
  404041:	89 08                	mov    %ecx,(%rax)
  404043:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40404a:	c3                   	ret
  40404b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000404050 <runtime::[random_generator_chacha8.odin]::chacha8rand_refill>:
  404050:	48 83 ec 18          	sub    $0x18,%rsp
  404054:	48 89 3c 24          	mov    %rdi,(%rsp)
  404058:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40405d:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  404062:	48 8b 04 24          	mov    (%rsp),%rax
  404066:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40406b:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404070:	80 b8 08 04 00 00 01 	cmpb   $0x1,0x408(%rax)
  404077:	0f 94 c0             	sete   %al
  40407a:	24 01                	and    $0x1,%al
  40407c:	0f b6 f8             	movzbl %al,%edi
  40407f:	be 98 f4 40 00       	mov    $0x40f498,%esi
  404084:	b9 d0 f4 40 00       	mov    $0x40f4d0,%ecx
  404089:	ba 20 00 00 00       	mov    $0x20,%edx
  40408e:	e8 1d d6 ff ff       	call   4016b0 <runtime::assert>
  404093:	48 8b 3c 24          	mov    (%rsp),%rdi
  404097:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40409c:	e8 1f e3 ff ff       	call   4023c0 <runtime::[random_generator_chacha8_simd128.odin]::chacha8rand_refill_simd128>
  4040a1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4040a6:	48 c7 80 00 04 00 00 	movq   $0x0,0x400(%rax)
  4040ad:	00 00 00 00 
  4040b1:	48 83 c4 18          	add    $0x18,%rsp
  4040b5:	c3                   	ret
  4040b6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4040bd:	00 00 00 

00000000004040c0 <io::query_utility>:
  4040c0:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  4040c5:	48 89 74 24 e0       	mov    %rsi,-0x20(%rsp)
  4040ca:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4040cf:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4040d4:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  4040d9:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  4040e0:	00 00 
  4040e2:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  4040e9:	00 
  4040ea:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4040ef:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  4040f6:	00 
  4040f7:	48 89 08             	mov    %rcx,(%rax)
  4040fa:	31 c0                	xor    %eax,%eax
  4040fc:	c3                   	ret
  4040fd:	0f 1f 00             	nopl   (%rax)

0000000000404100 <runtime::nil_allocator_proc>:
  404100:	48 83 ec 78          	sub    $0x78,%rsp
  404104:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  404109:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40410e:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  404113:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  404118:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40411d:	40 88 f0             	mov    %sil,%al
  404120:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  404124:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  40412b:	00 
  40412c:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  404131:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  404135:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40413a:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40413f:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  404144:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  404149:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  40414e:	4c 89 44 24 70       	mov    %r8,0x70(%rsp)
  404153:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  404157:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40415c:	48 89 74 24 58       	mov    %rsi,0x58(%rsp)
  404161:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  404166:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40416b:	0f b6 c8             	movzbl %al,%ecx
  40416e:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  404173:	2c 07                	sub    $0x7,%al
  404175:	0f 87 c9 00 00 00    	ja     404244 <runtime::nil_allocator_proc+0x144>
  40417b:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  404180:	48 8b 04 c5 f8 f4 40 	mov    0x40f4f8(,%rax,8),%rax
  404187:	00 
  404188:	ff e0                	jmp    *%rax
  40418a:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40418f:	31 f6                	xor    %esi,%esi
  404191:	ba 10 00 00 00       	mov    $0x10,%edx
  404196:	e8 a5 ce ff ff       	call   401040 <memset@plt>
  40419b:	b0 01                	mov    $0x1,%al
  40419d:	48 83 c4 78          	add    $0x78,%rsp
  4041a1:	c3                   	ret
  4041a2:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4041a7:	31 f6                	xor    %esi,%esi
  4041a9:	ba 10 00 00 00       	mov    $0x10,%edx
  4041ae:	e8 8d ce ff ff       	call   401040 <memset@plt>
  4041b3:	31 c0                	xor    %eax,%eax
  4041b5:	48 83 c4 78          	add    $0x78,%rsp
  4041b9:	c3                   	ret
  4041ba:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4041bf:	31 f6                	xor    %esi,%esi
  4041c1:	ba 10 00 00 00       	mov    $0x10,%edx
  4041c6:	e8 75 ce ff ff       	call   401040 <memset@plt>
  4041cb:	b0 04                	mov    $0x4,%al
  4041cd:	48 83 c4 78          	add    $0x78,%rsp
  4041d1:	c3                   	ret
  4041d2:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4041d7:	48 83 f8 00          	cmp    $0x0,%rax
  4041db:	0f 94 c0             	sete   %al
  4041de:	24 01                	and    $0x1,%al
  4041e0:	3c 00                	cmp    $0x0,%al
  4041e2:	74 18                	je     4041fc <runtime::nil_allocator_proc+0xfc>
  4041e4:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  4041e9:	31 f6                	xor    %esi,%esi
  4041eb:	ba 10 00 00 00       	mov    $0x10,%edx
  4041f0:	e8 4b ce ff ff       	call   401040 <memset@plt>
  4041f5:	31 c0                	xor    %eax,%eax
  4041f7:	48 83 c4 78          	add    $0x78,%rsp
  4041fb:	c3                   	ret
  4041fc:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  404201:	31 f6                	xor    %esi,%esi
  404203:	ba 10 00 00 00       	mov    $0x10,%edx
  404208:	e8 33 ce ff ff       	call   401040 <memset@plt>
  40420d:	b0 01                	mov    $0x1,%al
  40420f:	48 83 c4 78          	add    $0x78,%rsp
  404213:	c3                   	ret
  404214:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  404219:	31 f6                	xor    %esi,%esi
  40421b:	ba 10 00 00 00       	mov    $0x10,%edx
  404220:	e8 1b ce ff ff       	call   401040 <memset@plt>
  404225:	b0 04                	mov    $0x4,%al
  404227:	48 83 c4 78          	add    $0x78,%rsp
  40422b:	c3                   	ret
  40422c:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  404231:	31 f6                	xor    %esi,%esi
  404233:	ba 10 00 00 00       	mov    $0x10,%edx
  404238:	e8 03 ce ff ff       	call   401040 <memset@plt>
  40423d:	b0 04                	mov    $0x4,%al
  40423f:	48 83 c4 78          	add    $0x78,%rsp
  404243:	c3                   	ret
  404244:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  404249:	31 f6                	xor    %esi,%esi
  40424b:	ba 10 00 00 00       	mov    $0x10,%edx
  404250:	e8 eb cd ff ff       	call   401040 <memset@plt>
  404255:	31 c0                	xor    %eax,%eax
  404257:	48 83 c4 78          	add    $0x78,%rsp
  40425b:	c3                   	ret
  40425c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000404260 <runtime::nil_allocator>:
  404260:	48 c7 c0 00 41 40 00 	mov    $0x404100,%rax
  404267:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40426c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  404273:	00 00 
  404275:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40427a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40427f:	c3                   	ret

0000000000404280 <runtime::[os_specific_linux.odin]::_stderr_write>:
  404280:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  404285:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  40428a:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  40428f:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  404294:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  404299:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  40429e:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4042a3:	b8 01 00 00 00       	mov    $0x1,%eax
  4042a8:	bf 02 00 00 00       	mov    $0x2,%edi
  4042ad:	0f 05                	syscall
  4042af:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4042b4:	48 83 7c 24 e8 00    	cmpq   $0x0,-0x18(%rsp)
  4042ba:	0f 9c c0             	setl   %al
  4042bd:	24 01                	and    $0x1,%al
  4042bf:	3c 00                	cmp    $0x0,%al
  4042c1:	74 26                	je     4042e9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4042c3:	48 81 7c 24 e8 00 f0 	cmpq   $0xfffffffffffff000,-0x18(%rsp)
  4042ca:	ff ff 
  4042cc:	0f 9f c0             	setg   %al
  4042cf:	24 01                	and    $0x1,%al
  4042d1:	3c 00                	cmp    $0x0,%al
  4042d3:	74 14                	je     4042e9 <runtime::[os_specific_linux.odin]::_stderr_write+0x69>
  4042d5:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  4042da:	31 c0                	xor    %eax,%eax
  4042dc:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  4042e1:	48 c7 01 00 00 00 00 	movq   $0x0,(%rcx)
  4042e8:	c3                   	ret
  4042e9:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4042ee:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  4042f3:	48 89 08             	mov    %rcx,(%rax)
  4042f6:	31 c0                	xor    %eax,%eax
  4042f8:	c3                   	ret
  4042f9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000404300 <runtime::[os_specific_linux.odin]::_rand_bytes>:
  404300:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
  404307:	48 89 74 24 30       	mov    %rsi,0x30(%rsp)
  40430c:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  404311:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  404316:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40431b:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  404322:	00 
  404323:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  40432a:	00 
  40432b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  404332:	00 
  404333:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  404338:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40433f:	00 
  404340:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  404345:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  40434b:	0f 9f c0             	setg   %al
  40434e:	24 01                	and    $0x1,%al
  404350:	3c 00                	cmp    $0x0,%al
  404352:	0f 84 55 01 00 00    	je     4044ad <runtime::[os_specific_linux.odin]::_rand_bytes+0x1ad>
  404358:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40435d:	48 89 c8             	mov    %rcx,%rax
  404360:	48 2d ff ff ff 01    	sub    $0x1ffffff,%rax
  404366:	b8 ff ff ff 01       	mov    $0x1ffffff,%eax
  40436b:	48 0f 4c c1          	cmovl  %rcx,%rax
  40436f:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  404374:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  404379:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40437e:	4c 8b 8c 24 80 00 00 	mov    0x80(%rsp),%r9
  404385:	00 
  404386:	bf 40 f5 40 00       	mov    $0x40f540,%edi
  40438b:	be 31 00 00 00       	mov    $0x31,%esi
  404390:	ba 37 00 00 00       	mov    $0x37,%edx
  404395:	b9 44 00 00 00       	mov    $0x44,%ecx
  40439a:	e8 11 d9 ff ff       	call   401cb0 <runtime::slice_expr_error_hi>
  40439f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4043a4:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  4043a9:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  4043ae:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  4043b3:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  4043b8:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  4043bd:	31 c0                	xor    %eax,%eax
  4043bf:	89 c2                	mov    %eax,%edx
  4043c1:	b8 3e 01 00 00       	mov    $0x13e,%eax
  4043c6:	0f 05                	syscall
  4043c8:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4043cd:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4043d2:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4043d7:	48 83 e8 da          	sub    $0xffffffffffffffda,%rax
  4043db:	74 14                	je     4043f1 <runtime::[os_specific_linux.odin]::_rand_bytes+0xf1>
  4043dd:	eb 00                	jmp    4043df <runtime::[os_specific_linux.odin]::_rand_bytes+0xdf>
  4043df:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  4043e4:	48 83 e8 fc          	sub    $0xfffffffffffffffc,%rax
  4043e8:	75 1b                	jne    404405 <runtime::[os_specific_linux.odin]::_rand_bytes+0x105>
  4043ea:	eb 00                	jmp    4043ec <runtime::[os_specific_linux.odin]::_rand_bytes+0xec>
  4043ec:	e9 54 ff ff ff       	jmp    404345 <runtime::[os_specific_linux.odin]::_rand_bytes+0x45>
  4043f1:	bf 72 f5 40 00       	mov    $0x40f572,%edi
  4043f6:	ba b0 f5 40 00       	mov    $0x40f5b0,%edx
  4043fb:	be 2f 00 00 00       	mov    $0x2f,%esi
  404400:	e8 8b d3 ff ff       	call   401790 <runtime::panic_contextless>
  404405:	48 83 7c 24 50 00    	cmpq   $0x0,0x50(%rsp)
  40440b:	0f 9c c0             	setl   %al
  40440e:	24 01                	and    $0x1,%al
  404410:	3c 00                	cmp    $0x0,%al
  404412:	74 14                	je     404428 <runtime::[os_specific_linux.odin]::_rand_bytes+0x128>
  404414:	bf d8 f5 40 00       	mov    $0x40f5d8,%edi
  404419:	ba 00 f6 40 00       	mov    $0x40f600,%edx
  40441e:	be 1e 00 00 00       	mov    $0x1e,%esi
  404423:	e8 68 d3 ff ff       	call   401790 <runtime::panic_contextless>
  404428:	eb 00                	jmp    40442a <runtime::[os_specific_linux.odin]::_rand_bytes+0x12a>
  40442a:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40442f:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  404434:	48 29 c8             	sub    %rcx,%rax
  404437:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40443c:	4c 8b 44 24 50       	mov    0x50(%rsp),%r8
  404441:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  404446:	4c 8b 8c 24 80 00 00 	mov    0x80(%rsp),%r9
  40444d:	00 
  40444e:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  404453:	48 89 e0             	mov    %rsp,%rax
  404456:	4c 89 08             	mov    %r9,(%rax)
  404459:	bf 40 f5 40 00       	mov    $0x40f540,%edi
  40445e:	be 31 00 00 00       	mov    $0x31,%esi
  404463:	ba 4a 00 00 00       	mov    $0x4a,%edx
  404468:	b9 0c 00 00 00       	mov    $0xc,%ecx
  40446d:	e8 ee d8 ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  404472:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404477:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40447c:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  404481:	48 01 d1             	add    %rdx,%rcx
  404484:	48 29 d0             	sub    %rdx,%rax
  404487:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40448c:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  404491:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404496:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40449b:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  4044a2:	00 
  4044a3:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  4044a8:	e9 98 fe ff ff       	jmp    404345 <runtime::[os_specific_linux.odin]::_rand_bytes+0x45>
  4044ad:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  4044b4:	c3                   	ret
  4044b5:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  4044bc:	00 00 00 
  4044bf:	90                   	nop

00000000004044c0 <runtime::__type_info_of>:
  4044c0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  4044c5:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4044ca:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4044cf:	48 c7 c0 c0 fc 40 00 	mov    $0x40fcc0,%rax
  4044d6:	48 8b 40 08          	mov    0x8(%rax),%rax
  4044da:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4044df:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4044e4:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  4044e9:	48 83 f8 00          	cmp    $0x0,%rax
  4044ed:	74 16                	je     404505 <runtime::__type_info_of+0x45>
  4044ef:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  4044f4:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  4044f9:	31 d2                	xor    %edx,%edx
  4044fb:	48 f7 f1             	div    %rcx
  4044fe:	48 89 54 24 b8       	mov    %rdx,-0x48(%rsp)
  404503:	eb 02                	jmp    404507 <runtime::__type_info_of+0x47>
  404505:	0f 0b                	ud2
  404507:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40450c:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  404511:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  404518:	00 00 
  40451a:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  404521:	00 00 
  404523:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  404528:	48 39 44 24 e0       	cmp    %rax,-0x20(%rsp)
  40452d:	0f 83 9f 00 00 00    	jae    4045d2 <runtime::__type_info_of+0x112>
  404533:	48 c7 c0 c0 fc 40 00 	mov    $0x40fcc0,%rax
  40453a:	48 8b 00             	mov    (%rax),%rax
  40453d:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  404542:	48 8b 04 c8          	mov    (%rax,%rcx,8),%rax
  404546:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  40454b:	48 83 7c 24 d0 00    	cmpq   $0x0,-0x30(%rsp)
  404551:	0f 95 c0             	setne  %al
  404554:	24 01                	and    $0x1,%al
  404556:	3c 00                	cmp    $0x0,%al
  404558:	74 1d                	je     404577 <runtime::__type_info_of+0xb7>
  40455a:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  40455f:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404564:	48 39 48 18          	cmp    %rcx,0x18(%rax)
  404568:	0f 94 c0             	sete   %al
  40456b:	24 01                	and    $0x1,%al
  40456d:	3c 00                	cmp    $0x0,%al
  40456f:	74 06                	je     404577 <runtime::__type_info_of+0xb7>
  404571:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  404576:	c3                   	ret
  404577:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40457c:	48 83 c0 01          	add    $0x1,%rax
  404580:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  404585:	0f 92 c0             	setb   %al
  404588:	24 01                	and    $0x1,%al
  40458a:	3c 00                	cmp    $0x0,%al
  40458c:	74 10                	je     40459e <runtime::__type_info_of+0xde>
  40458e:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  404593:	48 83 c0 01          	add    $0x1,%rax
  404597:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40459c:	eb 09                	jmp    4045a7 <runtime::__type_info_of+0xe7>
  40459e:	31 c0                	xor    %eax,%eax
  4045a0:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  4045a5:	eb 00                	jmp    4045a7 <runtime::__type_info_of+0xe7>
  4045a7:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  4045ac:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4045b1:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  4045b6:	48 83 c0 01          	add    $0x1,%rax
  4045ba:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4045bf:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4045c4:	48 83 c0 01          	add    $0x1,%rax
  4045c8:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  4045cd:	e9 51 ff ff ff       	jmp    404523 <runtime::__type_info_of+0x63>
  4045d2:	48 c7 c0 c0 fc 40 00 	mov    $0x40fcc0,%rax
  4045d9:	48 8b 00             	mov    (%rax),%rax
  4045dc:	48 8b 00             	mov    (%rax),%rax
  4045df:	c3                   	ret

00000000004045e0 <runtime::default_logger_proc>:
  4045e0:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  4045e5:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  4045ea:	66 44 89 c0          	mov    %r8w,%ax
  4045ee:	66 89 44 24 c6       	mov    %ax,-0x3a(%rsp)
  4045f3:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  4045f8:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  4045fd:	66 8b 44 24 c6       	mov    -0x3a(%rsp),%ax
  404602:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  404607:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  40460c:	48 8b 74 24 b0       	mov    -0x50(%rsp),%rsi
  404611:	48 8b 7c 24 b8       	mov    -0x48(%rsp),%rdi
  404616:	48 89 7c 24 f8       	mov    %rdi,-0x8(%rsp)
  40461b:	48 89 74 24 f0       	mov    %rsi,-0x10(%rsp)
  404620:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  404625:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40462a:	66 89 44 24 de       	mov    %ax,-0x22(%rsp)
  40462f:	c3                   	ret

0000000000404630 <runtime::default_context>:
  404630:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  404637:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40463c:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  404641:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  404646:	31 f6                	xor    %esi,%esi
  404648:	ba 70 00 00 00       	mov    $0x70,%edx
  40464d:	e8 ee c9 ff ff       	call   401040 <memset@plt>
  404652:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
  404657:	e8 24 00 00 00       	call   404680 <runtime::[core.odin]::__init_context>
  40465c:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  404661:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
  404666:	ba 70 00 00 00       	mov    $0x70,%edx
  40466b:	e8 f0 c9 ff ff       	call   401060 <memcpy@plt>
  404670:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404675:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40467c:	c3                   	ret
  40467d:	0f 1f 00             	nopl   (%rax)

0000000000404680 <runtime::[core.odin]::__init_context>:
  404680:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  404685:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40468a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40468f:	48 83 f8 00          	cmp    $0x0,%rax
  404693:	0f 94 c0             	sete   %al
  404696:	24 01                	and    $0x1,%al
  404698:	3c 00                	cmp    $0x0,%al
  40469a:	74 01                	je     40469d <runtime::[core.odin]::__init_context+0x1d>
  40469c:	c3                   	ret
  40469d:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4046a2:	48 c7 c1 b0 52 40 00 	mov    $0x4052b0,%rcx
  4046a9:	48 89 08             	mov    %rcx,(%rax)
  4046ac:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4046b1:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  4046b8:	00 
  4046b9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4046be:	48 c7 c1 a0 22 40 00 	mov    $0x4022a0,%rcx
  4046c5:	48 89 48 10          	mov    %rcx,0x10(%rax)
  4046c9:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4046ce:	48 c7 c2 58 ff ff ff 	mov    $0xffffffffffffff58,%rdx
  4046d5:	64 48 8b 0c 25 00 00 	mov    %fs:0x0,%rcx
  4046dc:	00 00 
  4046de:	48 01 d1             	add    %rdx,%rcx
  4046e1:	48 89 48 18          	mov    %rcx,0x18(%rax)
  4046e5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4046ea:	48 c7 c1 30 47 40 00 	mov    $0x404730,%rcx
  4046f1:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4046f5:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  4046fa:	48 c7 c1 e0 45 40 00 	mov    $0x4045e0,%rcx
  404701:	48 89 48 28          	mov    %rcx,0x28(%rax)
  404705:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40470a:	48 c7 40 30 00 00 00 	movq   $0x0,0x30(%rax)
  404711:	00 
  404712:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404717:	48 c7 c1 b0 3a 40 00 	mov    $0x403ab0,%rcx
  40471e:	48 89 48 48          	mov    %rcx,0x48(%rax)
  404722:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  404727:	48 c7 40 50 00 00 00 	movq   $0x0,0x50(%rax)
  40472e:	00 
  40472f:	c3                   	ret

0000000000404730 <runtime::default_assertion_failure_proc>:
  404730:	48 83 ec 48          	sub    $0x48,%rsp
  404734:	4c 89 04 24          	mov    %r8,(%rsp)
  404738:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40473d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404742:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  404747:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40474c:	4c 8b 04 24          	mov    (%rsp),%r8
  404750:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  404755:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40475a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40475f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  404764:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  404769:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40476e:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  404773:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  404778:	e8 03 00 00 00       	call   404780 <runtime::default_assertion_contextless_failure_proc>
  40477d:	0f 1f 00             	nopl   (%rax)

0000000000404780 <runtime::default_assertion_contextless_failure_proc>:
  404780:	48 83 ec 48          	sub    $0x48,%rsp
  404784:	4c 89 04 24          	mov    %r8,(%rsp)
  404788:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40478d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404792:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  404797:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40479c:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4047a1:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4047a6:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4047ab:	48 8b 3c 24          	mov    (%rsp),%rdi
  4047af:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4047b4:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4047b9:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4047be:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  4047c3:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4047c8:	e8 b3 24 00 00       	call   406c80 <runtime::print_caller_location>
  4047cd:	bf 28 f6 40 00       	mov    $0x40f628,%edi
  4047d2:	be 01 00 00 00       	mov    $0x1,%esi
  4047d7:	e8 e4 20 00 00       	call   4068c0 <runtime::print_string>
  4047dc:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4047e1:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4047e6:	e8 d5 20 00 00       	call   4068c0 <runtime::print_string>
  4047eb:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4047f0:	48 83 f8 00          	cmp    $0x0,%rax
  4047f4:	0f 9f c0             	setg   %al
  4047f7:	24 01                	and    $0x1,%al
  4047f9:	3c 00                	cmp    $0x0,%al
  4047fb:	74 1e                	je     40481b <runtime::default_assertion_contextless_failure_proc+0x9b>
  4047fd:	bf 2a f6 40 00       	mov    $0x40f62a,%edi
  404802:	be 02 00 00 00       	mov    $0x2,%esi
  404807:	e8 b4 20 00 00       	call   4068c0 <runtime::print_string>
  40480c:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  404811:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  404816:	e8 a5 20 00 00       	call   4068c0 <runtime::print_string>
  40481b:	bf 0a 00 00 00       	mov    $0xa,%edi
  404820:	e8 0b 21 00 00       	call   406930 <runtime::print_byte>
  404825:	0f 0b                	ud2
  404827:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40482e:	00 00 

0000000000404830 <strconv::is_integer_negative>:
  404830:	48 83 ec 58          	sub    $0x58,%rsp
  404834:	4c 89 04 24          	mov    %r8,(%rsp)
  404838:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40483d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  404842:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  404847:	40 88 f0             	mov    %sil,%al
  40484a:	88 44 24 27          	mov    %al,0x27(%rsp)
  40484e:	8a 44 24 27          	mov    0x27(%rsp),%al
  404852:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  404857:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40485c:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  404861:	88 44 24 4f          	mov    %al,0x4f(%rsp)
  404865:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40486a:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  404871:	00 00 
  404873:	c6 44 24 37 00       	movb   $0x0,0x37(%rsp)
  404878:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40487d:	3c 00                	cmp    $0x0,%al
  40487f:	0f 84 1e 01 00 00    	je     4049a3 <strconv::is_integer_negative+0x173>
  404885:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40488a:	48 83 e8 08          	sub    $0x8,%rax
  40488e:	74 30                	je     4048c0 <strconv::is_integer_negative+0x90>
  404890:	eb 00                	jmp    404892 <strconv::is_integer_negative+0x62>
  404892:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  404897:	48 83 e8 10          	sub    $0x10,%rax
  40489b:	74 57                	je     4048f4 <strconv::is_integer_negative+0xc4>
  40489d:	eb 00                	jmp    40489f <strconv::is_integer_negative+0x6f>
  40489f:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4048a4:	48 83 e8 20          	sub    $0x20,%rax
  4048a8:	74 7d                	je     404927 <strconv::is_integer_negative+0xf7>
  4048aa:	eb 00                	jmp    4048ac <strconv::is_integer_negative+0x7c>
  4048ac:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4048b1:	48 83 e8 40          	sub    $0x40,%rax
  4048b5:	0f 84 9c 00 00 00    	je     404957 <strconv::is_integer_negative+0x127>
  4048bb:	e9 c9 00 00 00       	jmp    404989 <strconv::is_integer_negative+0x159>
  4048c0:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4048c5:	88 44 24 36          	mov    %al,0x36(%rsp)
  4048c9:	80 7c 24 36 00       	cmpb   $0x0,0x36(%rsp)
  4048ce:	0f 9c c0             	setl   %al
  4048d1:	24 01                	and    $0x1,%al
  4048d3:	88 44 24 37          	mov    %al,0x37(%rsp)
  4048d7:	48 0f be 44 24 36    	movsbq 0x36(%rsp),%rax
  4048dd:	31 c9                	xor    %ecx,%ecx
  4048df:	48 29 c1             	sub    %rax,%rcx
  4048e2:	48 83 f8 00          	cmp    $0x0,%rax
  4048e6:	48 0f 4c c1          	cmovl  %rcx,%rax
  4048ea:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4048ef:	e9 ad 00 00 00       	jmp    4049a1 <strconv::is_integer_negative+0x171>
  4048f4:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4048f9:	66 89 44 24 34       	mov    %ax,0x34(%rsp)
  4048fe:	66 83 7c 24 34 00    	cmpw   $0x0,0x34(%rsp)
  404904:	0f 9c c0             	setl   %al
  404907:	24 01                	and    $0x1,%al
  404909:	88 44 24 37          	mov    %al,0x37(%rsp)
  40490d:	48 0f bf 44 24 34    	movswq 0x34(%rsp),%rax
  404913:	31 c9                	xor    %ecx,%ecx
  404915:	48 29 c1             	sub    %rax,%rcx
  404918:	48 83 f8 00          	cmp    $0x0,%rax
  40491c:	48 0f 4c c1          	cmovl  %rcx,%rax
  404920:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404925:	eb 7a                	jmp    4049a1 <strconv::is_integer_negative+0x171>
  404927:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40492c:	89 44 24 30          	mov    %eax,0x30(%rsp)
  404930:	83 7c 24 30 00       	cmpl   $0x0,0x30(%rsp)
  404935:	0f 9c c0             	setl   %al
  404938:	24 01                	and    $0x1,%al
  40493a:	88 44 24 37          	mov    %al,0x37(%rsp)
  40493e:	48 63 44 24 30       	movslq 0x30(%rsp),%rax
  404943:	31 c9                	xor    %ecx,%ecx
  404945:	48 29 c1             	sub    %rax,%rcx
  404948:	48 83 f8 00          	cmp    $0x0,%rax
  40494c:	48 0f 4c c1          	cmovl  %rcx,%rax
  404950:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404955:	eb 4a                	jmp    4049a1 <strconv::is_integer_negative+0x171>
  404957:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40495c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  404961:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
  404967:	0f 9c c0             	setl   %al
  40496a:	24 01                	and    $0x1,%al
  40496c:	88 44 24 37          	mov    %al,0x37(%rsp)
  404970:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  404975:	31 c9                	xor    %ecx,%ecx
  404977:	48 29 c1             	sub    %rax,%rcx
  40497a:	48 83 f8 00          	cmp    $0x0,%rax
  40497e:	48 0f 4c c1          	cmovl  %rcx,%rax
  404982:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  404987:	eb 18                	jmp    4049a1 <strconv::is_integer_negative+0x171>
  404989:	48 8b 0c 24          	mov    (%rsp),%rcx
  40498d:	bf a8 f6 40 00       	mov    $0x40f6a8,%edi
  404992:	ba 10 f7 40 00       	mov    $0x40f710,%edx
  404997:	be 29 00 00 00       	mov    $0x29,%esi
  40499c:	e8 6f cd ff ff       	call   401710 <runtime::panic>
  4049a1:	eb 00                	jmp    4049a3 <strconv::is_integer_negative+0x173>
  4049a3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4049a8:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  4049ad:	8a 44 24 37          	mov    0x37(%rsp),%al
  4049b1:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4049b6:	88 44 24 37          	mov    %al,0x37(%rsp)
  4049ba:	48 89 11             	mov    %rdx,(%rcx)
  4049bd:	48 83 c4 58          	add    $0x58,%rsp
  4049c1:	c3                   	ret
  4049c2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4049c9:	1f 84 00 00 00 00 00 

00000000004049d0 <strconv::write_bits>:
  4049d0:	48 81 ec 68 02 00 00 	sub    $0x268,%rsp
  4049d7:	4c 89 8c 24 e8 00 00 	mov    %r9,0xe8(%rsp)
  4049de:	00 
  4049df:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  4049e6:	00 
  4049e7:	48 89 94 24 f8 00 00 	mov    %rdx,0xf8(%rsp)
  4049ee:	00 
  4049ef:	44 88 c0             	mov    %r8b,%al
  4049f2:	88 84 24 07 01 00 00 	mov    %al,0x107(%rsp)
  4049f9:	48 89 b4 24 08 01 00 	mov    %rsi,0x108(%rsp)
  404a00:	00 
  404a01:	48 89 bc 24 10 01 00 	mov    %rdi,0x110(%rsp)
  404a08:	00 
  404a09:	48 8b 84 24 88 02 00 	mov    0x288(%rsp),%rax
  404a10:	00 
  404a11:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  404a18:	00 
  404a19:	8a 84 24 80 02 00 00 	mov    0x280(%rsp),%al
  404a20:	88 84 24 27 01 00 00 	mov    %al,0x127(%rsp)
  404a27:	48 8d 84 24 70 02 00 	lea    0x270(%rsp),%rax
  404a2e:	00 
  404a2f:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  404a36:	00 
  404a37:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  404a3e:	00 
  404a3f:	8a 8c 24 27 01 00 00 	mov    0x127(%rsp),%cl
  404a46:	48 8b 94 24 28 01 00 	mov    0x128(%rsp),%rdx
  404a4d:	00 
  404a4e:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  404a55:	00 
  404a56:	40 8a bc 24 07 01 00 	mov    0x107(%rsp),%dil
  404a5d:	00 
  404a5e:	4c 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%r8
  404a65:	00 
  404a66:	4c 8b 8c 24 08 01 00 	mov    0x108(%rsp),%r9
  404a6d:	00 
  404a6e:	4c 8b 94 24 10 01 00 	mov    0x110(%rsp),%r10
  404a75:	00 
  404a76:	4c 89 94 24 58 02 00 	mov    %r10,0x258(%rsp)
  404a7d:	00 
  404a7e:	4c 89 8c 24 60 02 00 	mov    %r9,0x260(%rsp)
  404a85:	00 
  404a86:	4c 89 84 24 50 02 00 	mov    %r8,0x250(%rsp)
  404a8d:	00 
  404a8e:	48 89 84 24 48 02 00 	mov    %rax,0x248(%rsp)
  404a95:	00 
  404a96:	40 88 bc 24 47 02 00 	mov    %dil,0x247(%rsp)
  404a9d:	00 
  404a9e:	48 89 b4 24 38 02 00 	mov    %rsi,0x238(%rsp)
  404aa5:	00 
  404aa6:	48 8b 32             	mov    (%rdx),%rsi
  404aa9:	48 89 b4 24 28 02 00 	mov    %rsi,0x228(%rsp)
  404ab0:	00 
  404ab1:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  404ab5:	48 89 94 24 30 02 00 	mov    %rdx,0x230(%rsp)
  404abc:	00 
  404abd:	88 8c 24 27 02 00 00 	mov    %cl,0x227(%rsp)
  404ac4:	48 83 f8 02          	cmp    $0x2,%rax
  404ac8:	0f 9c c0             	setl   %al
  404acb:	24 01                	and    $0x1,%al
  404acd:	3c 00                	cmp    $0x0,%al
  404acf:	75 15                	jne    404ae6 <strconv::write_bits+0x116>
  404ad1:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  404ad8:	00 
  404ad9:	48 83 f8 20          	cmp    $0x20,%rax
  404add:	0f 9f c0             	setg   %al
  404ae0:	24 01                	and    $0x1,%al
  404ae2:	3c 00                	cmp    $0x0,%al
  404ae4:	74 1c                	je     404b02 <strconv::write_bits+0x132>
  404ae6:	48 8b 8c 24 18 01 00 	mov    0x118(%rsp),%rcx
  404aed:	00 
  404aee:	bf 38 f7 40 00       	mov    $0x40f738,%edi
  404af3:	ba 70 f7 40 00       	mov    $0x40f770,%edx
  404af8:	be 2a 00 00 00       	mov    $0x2a,%esi
  404afd:	e8 0e cc ff ff       	call   401710 <runtime::panic>
  404b02:	48 8d bc 24 a6 01 00 	lea    0x1a6(%rsp),%rdi
  404b09:	00 
  404b0a:	31 f6                	xor    %esi,%esi
  404b0c:	ba 81 00 00 00       	mov    $0x81,%edx
  404b11:	e8 2a c5 ff ff       	call   401040 <memset@plt>
  404b16:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  404b1d:	00 
  404b1e:	8a 84 24 07 01 00 00 	mov    0x107(%rsp),%al
  404b25:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  404b2c:	00 
  404b2d:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  404b34:	00 
  404b35:	48 c7 84 24 98 01 00 	movq   $0x81,0x198(%rsp)
  404b3c:	00 81 00 00 00 
  404b41:	48 c7 84 24 90 01 00 	movq   $0x0,0x190(%rsp)
  404b48:	00 00 00 00 00 
  404b4d:	48 8d 8c 24 90 01 00 	lea    0x190(%rsp),%rcx
  404b54:	00 
  404b55:	0f b6 f0             	movzbl %al,%esi
  404b58:	e8 d3 fc ff ff       	call   404830 <strconv::is_integer_negative>
  404b5d:	88 c1                	mov    %al,%cl
  404b5f:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  404b66:	00 
  404b67:	48 8b 94 24 90 01 00 	mov    0x190(%rsp),%rdx
  404b6e:	00 
  404b6f:	48 89 94 24 88 01 00 	mov    %rdx,0x188(%rsp)
  404b76:	00 
  404b77:	88 8c 24 87 01 00 00 	mov    %cl,0x187(%rsp)
  404b7e:	48 89 84 24 78 01 00 	mov    %rax,0x178(%rsp)
  404b85:	00 
  404b86:	48 8b 84 24 88 01 00 	mov    0x188(%rsp),%rax
  404b8d:	00 
  404b8e:	48 3b 84 24 78 01 00 	cmp    0x178(%rsp),%rax
  404b95:	00 
  404b96:	0f 93 c0             	setae  %al
  404b99:	24 01                	and    $0x1,%al
  404b9b:	3c 00                	cmp    $0x0,%al
  404b9d:	0f 84 57 01 00 00    	je     404cfa <strconv::write_bits+0x32a>
  404ba3:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  404baa:	00 
  404bab:	48 83 e8 01          	sub    $0x1,%rax
  404baf:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  404bb6:	00 
  404bb7:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  404bbe:	00 
  404bbf:	48 8d 84 24 a6 01 00 	lea    0x1a6(%rsp),%rax
  404bc6:	00 
  404bc7:	4c 01 c0             	add    %r8,%rax
  404bca:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  404bd1:	00 
  404bd2:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404bd7:	be 28 00 00 00       	mov    $0x28,%esi
  404bdc:	ba 4b 00 00 00       	mov    $0x4b,%edx
  404be1:	b9 0b 00 00 00       	mov    $0xb,%ecx
  404be6:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  404bec:	e8 8f cd ff ff       	call   401980 <runtime::bounds_check_error>
  404bf1:	48 8b 84 24 28 02 00 	mov    0x228(%rsp),%rax
  404bf8:	00 
  404bf9:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  404c00:	00 
  404c01:	48 8b 84 24 30 02 00 	mov    0x230(%rsp),%rax
  404c08:	00 
  404c09:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  404c10:	00 
  404c11:	48 8b 84 24 88 01 00 	mov    0x188(%rsp),%rax
  404c18:	00 
  404c19:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  404c20:	00 
  404c21:	48 8b 84 24 78 01 00 	mov    0x178(%rsp),%rax
  404c28:	00 
  404c29:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  404c30:	00 
  404c31:	48 83 f8 00          	cmp    $0x0,%rax
  404c35:	74 1f                	je     404c56 <strconv::write_bits+0x286>
  404c37:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  404c3e:	00 
  404c3f:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  404c46:	00 
  404c47:	31 d2                	xor    %edx,%edx
  404c49:	48 f7 f1             	div    %rcx
  404c4c:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  404c53:	00 
  404c54:	eb 02                	jmp    404c58 <strconv::write_bits+0x288>
  404c56:	0f 0b                	ud2
  404c58:	4c 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%r8
  404c5f:	00 
  404c60:	4c 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%r9
  404c67:	00 
  404c68:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404c6d:	be 28 00 00 00       	mov    $0x28,%esi
  404c72:	ba 4b 00 00 00       	mov    $0x4b,%edx
  404c77:	b9 17 00 00 00       	mov    $0x17,%ecx
  404c7c:	e8 ff cc ff ff       	call   401980 <runtime::bounds_check_error>
  404c81:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  404c88:	00 
  404c89:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  404c90:	00 
  404c91:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  404c98:	00 
  404c99:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  404c9c:	88 08                	mov    %cl,(%rax)
  404c9e:	48 8b 84 24 78 01 00 	mov    0x178(%rsp),%rax
  404ca5:	00 
  404ca6:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  404cad:	00 
  404cae:	48 8b 8c 24 88 01 00 	mov    0x188(%rsp),%rcx
  404cb5:	00 
  404cb6:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  404cbd:	00 
  404cbe:	48 83 f8 00          	cmp    $0x0,%rax
  404cc2:	74 1f                	je     404ce3 <strconv::write_bits+0x313>
  404cc4:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  404ccb:	00 
  404ccc:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  404cd3:	00 
  404cd4:	31 d2                	xor    %edx,%edx
  404cd6:	48 f7 f1             	div    %rcx
  404cd9:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  404ce0:	00 
  404ce1:	eb 02                	jmp    404ce5 <strconv::write_bits+0x315>
  404ce3:	0f 0b                	ud2
  404ce5:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  404cec:	00 
  404ced:	48 89 84 24 88 01 00 	mov    %rax,0x188(%rsp)
  404cf4:	00 
  404cf5:	e9 8c fe ff ff       	jmp    404b86 <strconv::write_bits+0x1b6>
  404cfa:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  404d01:	00 
  404d02:	48 83 e8 01          	sub    $0x1,%rax
  404d06:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  404d0d:	00 
  404d0e:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  404d15:	00 
  404d16:	48 8d 84 24 a6 01 00 	lea    0x1a6(%rsp),%rax
  404d1d:	00 
  404d1e:	4c 01 c0             	add    %r8,%rax
  404d21:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  404d26:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404d2b:	be 28 00 00 00       	mov    $0x28,%esi
  404d30:	ba 4e 00 00 00       	mov    $0x4e,%edx
  404d35:	b9 0a 00 00 00       	mov    $0xa,%ecx
  404d3a:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  404d40:	e8 3b cc ff ff       	call   401980 <runtime::bounds_check_error>
  404d45:	48 8b 84 24 28 02 00 	mov    0x228(%rsp),%rax
  404d4c:	00 
  404d4d:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  404d54:	00 
  404d55:	48 8b 84 24 30 02 00 	mov    0x230(%rsp),%rax
  404d5c:	00 
  404d5d:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  404d64:	00 
  404d65:	48 8b 84 24 88 01 00 	mov    0x188(%rsp),%rax
  404d6c:	00 
  404d6d:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  404d74:	00 
  404d75:	48 8b 84 24 78 01 00 	mov    0x178(%rsp),%rax
  404d7c:	00 
  404d7d:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  404d84:	00 
  404d85:	48 83 f8 00          	cmp    $0x0,%rax
  404d89:	74 1c                	je     404da7 <strconv::write_bits+0x3d7>
  404d8b:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  404d92:	00 
  404d93:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  404d9a:	00 
  404d9b:	31 d2                	xor    %edx,%edx
  404d9d:	48 f7 f1             	div    %rcx
  404da0:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  404da5:	eb 02                	jmp    404da9 <strconv::write_bits+0x3d9>
  404da7:	0f 0b                	ud2
  404da9:	4c 8b 44 24 70       	mov    0x70(%rsp),%r8
  404dae:	4c 8b 8c 24 88 00 00 	mov    0x88(%rsp),%r9
  404db5:	00 
  404db6:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404dbb:	be 28 00 00 00       	mov    $0x28,%esi
  404dc0:	ba 4e 00 00 00       	mov    $0x4e,%edx
  404dc5:	b9 16 00 00 00       	mov    $0x16,%ecx
  404dca:	e8 b1 cb ff ff       	call   401980 <runtime::bounds_check_error>
  404dcf:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  404dd4:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  404ddb:	00 
  404ddc:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  404de1:	8a 84 24 27 01 00 00 	mov    0x127(%rsp),%al
  404de8:	8a 14 32             	mov    (%rdx,%rsi,1),%dl
  404deb:	88 11                	mov    %dl,(%rcx)
  404ded:	24 01                	and    $0x1,%al
  404def:	3c 00                	cmp    $0x0,%al
  404df1:	0f 95 c0             	setne  %al
  404df4:	24 01                	and    $0x1,%al
  404df6:	3c 00                	cmp    $0x0,%al
  404df8:	0f 84 d5 01 00 00    	je     404fd3 <strconv::write_bits+0x603>
  404dfe:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  404e05:	00 
  404e06:	c6 84 24 77 01 00 00 	movb   $0x1,0x177(%rsp)
  404e0d:	01 
  404e0e:	48 83 c0 fe          	add    $0xfffffffffffffffe,%rax
  404e12:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  404e17:	48 83 e8 0e          	sub    $0xe,%rax
  404e1b:	0f 87 51 01 00 00    	ja     404f72 <strconv::write_bits+0x5a2>
  404e21:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  404e26:	48 8b 04 c5 30 f6 40 	mov    0x40f630(,%rax,8),%rax
  404e2d:	00 
  404e2e:	ff e0                	jmp    *%rax
  404e30:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  404e37:	00 
  404e38:	48 83 e8 01          	sub    $0x1,%rax
  404e3c:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  404e43:	00 
  404e44:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  404e4b:	00 
  404e4c:	4c 89 44 24 60       	mov    %r8,0x60(%rsp)
  404e51:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404e56:	be 28 00 00 00       	mov    $0x28,%esi
  404e5b:	ba 53 00 00 00       	mov    $0x53,%edx
  404e60:	b9 14 00 00 00       	mov    $0x14,%ecx
  404e65:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  404e6b:	e8 10 cb ff ff       	call   401980 <runtime::bounds_check_error>
  404e70:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  404e75:	c6 84 04 a6 01 00 00 	movb   $0x62,0x1a6(%rsp,%rax,1)
  404e7c:	62 
  404e7d:	e9 f8 00 00 00       	jmp    404f7a <strconv::write_bits+0x5aa>
  404e82:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  404e89:	00 
  404e8a:	48 83 e8 01          	sub    $0x1,%rax
  404e8e:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  404e95:	00 
  404e96:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  404e9d:	00 
  404e9e:	4c 89 44 24 58       	mov    %r8,0x58(%rsp)
  404ea3:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404ea8:	be 28 00 00 00       	mov    $0x28,%esi
  404ead:	ba 54 00 00 00       	mov    $0x54,%edx
  404eb2:	b9 14 00 00 00       	mov    $0x14,%ecx
  404eb7:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  404ebd:	e8 be ca ff ff       	call   401980 <runtime::bounds_check_error>
  404ec2:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  404ec7:	c6 84 04 a6 01 00 00 	movb   $0x6f,0x1a6(%rsp,%rax,1)
  404ece:	6f 
  404ecf:	e9 a6 00 00 00       	jmp    404f7a <strconv::write_bits+0x5aa>
  404ed4:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  404edb:	00 
  404edc:	48 83 e8 01          	sub    $0x1,%rax
  404ee0:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  404ee7:	00 
  404ee8:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  404eef:	00 
  404ef0:	4c 89 44 24 50       	mov    %r8,0x50(%rsp)
  404ef5:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404efa:	be 28 00 00 00       	mov    $0x28,%esi
  404eff:	ba 56 00 00 00       	mov    $0x56,%edx
  404f04:	b9 14 00 00 00       	mov    $0x14,%ecx
  404f09:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  404f0f:	e8 6c ca ff ff       	call   401980 <runtime::bounds_check_error>
  404f14:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  404f19:	c6 84 04 a6 01 00 00 	movb   $0x7a,0x1a6(%rsp,%rax,1)
  404f20:	7a 
  404f21:	eb 57                	jmp    404f7a <strconv::write_bits+0x5aa>
  404f23:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  404f2a:	00 
  404f2b:	48 83 e8 01          	sub    $0x1,%rax
  404f2f:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  404f36:	00 
  404f37:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  404f3e:	00 
  404f3f:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  404f44:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404f49:	be 28 00 00 00       	mov    $0x28,%esi
  404f4e:	ba 57 00 00 00       	mov    $0x57,%edx
  404f53:	b9 14 00 00 00       	mov    $0x14,%ecx
  404f58:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  404f5e:	e8 1d ca ff ff       	call   401980 <runtime::bounds_check_error>
  404f63:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  404f68:	c6 84 04 a6 01 00 00 	movb   $0x78,0x1a6(%rsp,%rax,1)
  404f6f:	78 
  404f70:	eb 08                	jmp    404f7a <strconv::write_bits+0x5aa>
  404f72:	c6 84 24 77 01 00 00 	movb   $0x0,0x177(%rsp)
  404f79:	00 
  404f7a:	80 bc 24 77 01 00 00 	cmpb   $0x0,0x177(%rsp)
  404f81:	00 
  404f82:	74 4d                	je     404fd1 <strconv::write_bits+0x601>
  404f84:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  404f8b:	00 
  404f8c:	48 83 e8 01          	sub    $0x1,%rax
  404f90:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  404f97:	00 
  404f98:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  404f9f:	00 
  404fa0:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  404fa5:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  404faa:	be 28 00 00 00       	mov    $0x28,%esi
  404faf:	ba 5b 00 00 00       	mov    $0x5b,%edx
  404fb4:	b9 0c 00 00 00       	mov    $0xc,%ecx
  404fb9:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  404fbf:	e8 bc c9 ff ff       	call   401980 <runtime::bounds_check_error>
  404fc4:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  404fc9:	c6 84 04 a6 01 00 00 	movb   $0x30,0x1a6(%rsp,%rax,1)
  404fd0:	30 
  404fd1:	eb 00                	jmp    404fd3 <strconv::write_bits+0x603>
  404fd3:	b0 01                	mov    $0x1,%al
  404fd5:	3a 84 24 87 01 00 00 	cmp    0x187(%rsp),%al
  404fdc:	74 19                	je     404ff7 <strconv::write_bits+0x627>
  404fde:	8a 84 24 27 01 00 00 	mov    0x127(%rsp),%al
  404fe5:	24 02                	and    $0x2,%al
  404fe7:	3c 00                	cmp    $0x0,%al
  404fe9:	0f 95 c1             	setne  %cl
  404fec:	80 e1 01             	and    $0x1,%cl
  404fef:	b0 01                	mov    $0x1,%al
  404ff1:	38 c8                	cmp    %cl,%al
  404ff3:	74 53                	je     405048 <strconv::write_bits+0x678>
  404ff5:	eb 4f                	jmp    405046 <strconv::write_bits+0x676>
  404ff7:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  404ffe:	00 
  404fff:	48 83 e8 01          	sub    $0x1,%rax
  405003:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  40500a:	00 
  40500b:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  405012:	00 
  405013:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  405018:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  40501d:	be 28 00 00 00       	mov    $0x28,%esi
  405022:	ba 61 00 00 00       	mov    $0x61,%edx
  405027:	b9 0b 00 00 00       	mov    $0xb,%ecx
  40502c:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  405032:	e8 49 c9 ff ff       	call   401980 <runtime::bounds_check_error>
  405037:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40503c:	c6 84 04 a6 01 00 00 	movb   $0x2d,0x1a6(%rsp,%rax,1)
  405043:	2d 
  405044:	eb 4f                	jmp    405095 <strconv::write_bits+0x6c5>
  405046:	eb 4d                	jmp    405095 <strconv::write_bits+0x6c5>
  405048:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  40504f:	00 
  405050:	48 83 e8 01          	sub    $0x1,%rax
  405054:	48 89 84 24 98 01 00 	mov    %rax,0x198(%rsp)
  40505b:	00 
  40505c:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  405063:	00 
  405064:	4c 89 44 24 30       	mov    %r8,0x30(%rsp)
  405069:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  40506e:	be 28 00 00 00       	mov    $0x28,%esi
  405073:	ba 63 00 00 00       	mov    $0x63,%edx
  405078:	b9 0b 00 00 00       	mov    $0xb,%ecx
  40507d:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  405083:	e8 f8 c8 ff ff       	call   401980 <runtime::bounds_check_error>
  405088:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40508d:	c6 84 04 a6 01 00 00 	movb   $0x2b,0x1a6(%rsp,%rax,1)
  405094:	2b 
  405095:	4c 8b 84 24 98 01 00 	mov    0x198(%rsp),%r8
  40509c:	00 
  40509d:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  4050a2:	48 89 e0             	mov    %rsp,%rax
  4050a5:	48 c7 00 81 00 00 00 	movq   $0x81,(%rax)
  4050ac:	bf d2 f6 40 00       	mov    $0x40f6d2,%edi
  4050b1:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4050b6:	be 28 00 00 00       	mov    $0x28,%esi
  4050bb:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4050c0:	ba 66 00 00 00       	mov    $0x66,%edx
  4050c5:	b9 0a 00 00 00       	mov    $0xa,%ecx
  4050ca:	41 b9 81 00 00 00    	mov    $0x81,%r9d
  4050d0:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  4050d5:	e8 86 cc ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  4050da:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4050df:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4050e4:	48 8b bc 24 10 01 00 	mov    0x110(%rsp),%rdi
  4050eb:	00 
  4050ec:	48 8b b4 24 08 01 00 	mov    0x108(%rsp),%rsi
  4050f3:	00 
  4050f4:	48 8d 8c 14 a6 01 00 	lea    0x1a6(%rsp,%rdx,1),%rcx
  4050fb:	00 
  4050fc:	48 29 d0             	sub    %rdx,%rax
  4050ff:	48 89 8c 24 60 01 00 	mov    %rcx,0x160(%rsp)
  405106:	00 
  405107:	48 89 84 24 68 01 00 	mov    %rax,0x168(%rsp)
  40510e:	00 
  40510f:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  405116:	00 
  405117:	48 8b 8c 24 68 01 00 	mov    0x168(%rsp),%rcx
  40511e:	00 
  40511f:	48 89 8c 24 58 01 00 	mov    %rcx,0x158(%rsp)
  405126:	00 
  405127:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  40512e:	00 
  40512f:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  405136:	00 
  405137:	48 8b 8c 24 58 01 00 	mov    0x158(%rsp),%rcx
  40513e:	00 
  40513f:	e8 dc c0 ff ff       	call   401220 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  405144:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  405149:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40514e:	4c 8b 8c 24 58 01 00 	mov    0x158(%rsp),%r9
  405155:	00 
  405156:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40515b:	48 8b 8c 24 60 02 00 	mov    0x260(%rsp),%rcx
  405162:	00 
  405163:	48 89 e0             	mov    %rsp,%rax
  405166:	48 89 08             	mov    %rcx,(%rax)
  405169:	31 c0                	xor    %eax,%eax
  40516b:	41 89 c0             	mov    %eax,%r8d
  40516e:	ba 68 00 00 00       	mov    $0x68,%edx
  405173:	b9 13 00 00 00       	mov    $0x13,%ecx
  405178:	e8 e3 cb ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  40517d:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  405182:	48 8b 8c 24 58 02 00 	mov    0x258(%rsp),%rcx
  405189:	00 
  40518a:	48 89 8c 24 40 01 00 	mov    %rcx,0x140(%rsp)
  405191:	00 
  405192:	48 89 84 24 48 01 00 	mov    %rax,0x148(%rsp)
  405199:	00 
  40519a:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4051a1:	00 
  4051a2:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  4051a9:	00 
  4051aa:	48 89 8c 24 38 01 00 	mov    %rcx,0x138(%rsp)
  4051b1:	00 
  4051b2:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  4051b9:	00 
  4051ba:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4051c1:	00 
  4051c2:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  4051c9:	00 
  4051ca:	48 81 c4 68 02 00 00 	add    $0x268,%rsp
  4051d1:	c3                   	ret
  4051d2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4051d9:	1f 84 00 00 00 00 00 

00000000004051e0 <strconv::write_int>:
  4051e0:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  4051e7:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  4051ec:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4051f1:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4051f6:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  4051fb:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  405200:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  405205:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40520a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40520f:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  405214:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  405219:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  40521e:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  405225:	00 
  405226:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  40522b:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  405230:	49 c7 c1 58 40 41 00 	mov    $0x414058,%r9
  405237:	49 8b 01             	mov    (%r9),%rax
  40523a:	4d 8b 49 08          	mov    0x8(%r9),%r9
  40523e:	4c 89 4c 24 60       	mov    %r9,0x60(%rsp)
  405243:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405248:	0f 10 44 24 58       	movups 0x58(%rsp),%xmm0
  40524d:	48 89 e0             	mov    %rsp,%rax
  405250:	0f 11 00             	movups %xmm0,(%rax)
  405253:	4c 89 40 18          	mov    %r8,0x18(%rax)
  405257:	c7 40 10 00 00 00 00 	movl   $0x0,0x10(%rax)
  40525e:	41 b8 01 00 00 00    	mov    $0x1,%r8d
  405264:	41 b9 40 00 00 00    	mov    $0x40,%r9d
  40526a:	e8 61 f7 ff ff       	call   4049d0 <strconv::write_bits>
  40526f:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  405274:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  405279:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40527e:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  405283:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40528a:	c3                   	ret
  40528b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000405290 <runtime::heap_allocator>:
  405290:	48 c7 c0 b0 52 40 00 	mov    $0x4052b0,%rax
  405297:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40529c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  4052a3:	00 00 
  4052a5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  4052aa:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  4052af:	c3                   	ret

00000000004052b0 <runtime::heap_allocator_proc>:
  4052b0:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  4052b7:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  4052bc:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  4052c1:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4052c6:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4052cb:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4052d0:	40 88 f0             	mov    %sil,%al
  4052d3:	88 44 24 47          	mov    %al,0x47(%rsp)
  4052d7:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  4052de:	00 
  4052df:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4052e4:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  4052eb:	00 
  4052ec:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4052f1:	8a 44 24 47          	mov    0x47(%rsp),%al
  4052f5:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4052fa:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4052ff:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  405304:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  405309:	4c 8b 44 24 38       	mov    0x38(%rsp),%r8
  40530e:	4c 89 84 24 b0 00 00 	mov    %r8,0xb0(%rsp)
  405315:	00 
  405316:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  40531d:	48 89 bc 24 a0 00 00 	mov    %rdi,0xa0(%rsp)
  405324:	00 
  405325:	48 89 b4 24 98 00 00 	mov    %rsi,0x98(%rsp)
  40532c:	00 
  40532d:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  405334:	00 
  405335:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40533c:	00 
  40533d:	0f b6 c8             	movzbl %al,%ecx
  405340:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  405345:	2c 07                	sub    $0x7,%al
  405347:	0f 87 4a 01 00 00    	ja     405497 <runtime::heap_allocator_proc+0x1e7>
  40534d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405352:	48 8b 04 c5 98 f7 40 	mov    0x40f798(,%rax,8),%rax
  405359:	00 
  40535a:	ff e0                	jmp    *%rax
  40535c:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  405361:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  405366:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40536b:	8a 44 24 47          	mov    0x47(%rsp),%al
  40536f:	84 c0                	test   %al,%al
  405371:	0f 94 c0             	sete   %al
  405374:	0f 57 c0             	xorps  %xmm0,%xmm0
  405377:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  40537c:	48 89 e1             	mov    %rsp,%rcx
  40537f:	48 89 11             	mov    %rdx,(%rcx)
  405382:	44 0f b6 c0          	movzbl %al,%r8d
  405386:	31 c0                	xor    %eax,%eax
  405388:	89 c1                	mov    %eax,%ecx
  40538a:	4c 8d 4c 24 70       	lea    0x70(%rsp),%r9
  40538f:	48 89 ca             	mov    %rcx,%rdx
  405392:	e8 a9 01 00 00       	call   405540 <runtime::heap_allocator_proc.aligned_alloc-0>
  405397:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40539c:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  4053a1:	48 8b 74 24 78       	mov    0x78(%rsp),%rsi
  4053a6:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4053aa:	48 89 11             	mov    %rdx,(%rcx)
  4053ad:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4053b4:	c3                   	ret
  4053b5:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  4053ba:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4053bf:	e8 cc 03 00 00       	call   405790 <runtime::heap_allocator_proc.aligned_free-1>
  4053c4:	e9 ce 00 00 00       	jmp    405497 <runtime::heap_allocator_proc+0x1e7>
  4053c9:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  4053ce:	31 f6                	xor    %esi,%esi
  4053d0:	ba 10 00 00 00       	mov    $0x10,%edx
  4053d5:	e8 66 bc ff ff       	call   401040 <memset@plt>
  4053da:	b0 04                	mov    $0x4,%al
  4053dc:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4053e3:	c3                   	ret
  4053e4:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4053e9:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  4053ee:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  4053f3:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4053f8:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  4053fd:	8a 44 24 47          	mov    0x47(%rsp),%al
  405401:	2c 03                	sub    $0x3,%al
  405403:	0f 94 c0             	sete   %al
  405406:	0f 57 c0             	xorps  %xmm0,%xmm0
  405409:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  40540e:	49 89 e0             	mov    %rsp,%r8
  405411:	4d 89 08             	mov    %r9,(%r8)
  405414:	44 0f b6 c0          	movzbl %al,%r8d
  405418:	4c 8d 4c 24 60       	lea    0x60(%rsp),%r9
  40541d:	e8 ae 03 00 00       	call   4057d0 <runtime::heap_allocator_proc.aligned_resize-2>
  405422:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  405427:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40542c:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  405431:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405435:	48 89 11             	mov    %rdx,(%rcx)
  405438:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40543f:	c3                   	ret
  405440:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405445:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40544a:	48 83 7c 24 58 00    	cmpq   $0x0,0x58(%rsp)
  405450:	0f 95 c0             	setne  %al
  405453:	24 01                	and    $0x1,%al
  405455:	3c 00                	cmp    $0x0,%al
  405457:	74 08                	je     405461 <runtime::heap_allocator_proc+0x1b1>
  405459:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40545e:	c6 00 db             	movb   $0xdb,(%rax)
  405461:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  405466:	31 f6                	xor    %esi,%esi
  405468:	ba 10 00 00 00       	mov    $0x10,%edx
  40546d:	e8 ce bb ff ff       	call   401040 <memset@plt>
  405472:	31 c0                	xor    %eax,%eax
  405474:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40547b:	c3                   	ret
  40547c:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  405481:	31 f6                	xor    %esi,%esi
  405483:	ba 10 00 00 00       	mov    $0x10,%edx
  405488:	e8 b3 bb ff ff       	call   401040 <memset@plt>
  40548d:	b0 04                	mov    $0x4,%al
  40548f:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  405496:	c3                   	ret
  405497:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  40549c:	31 f6                	xor    %esi,%esi
  40549e:	ba 10 00 00 00       	mov    $0x10,%edx
  4054a3:	e8 98 bb ff ff       	call   401040 <memset@plt>
  4054a8:	31 c0                	xor    %eax,%eax
  4054aa:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  4054b1:	c3                   	ret
  4054b2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4054b9:	1f 84 00 00 00 00 00 

00000000004054c0 <runtime::heap_alloc>:
  4054c0:	48 83 ec 18          	sub    $0x18,%rsp
  4054c4:	48 89 3c 24          	mov    %rdi,(%rsp)
  4054c8:	40 88 f0             	mov    %sil,%al
  4054cb:	88 44 24 0e          	mov    %al,0xe(%rsp)
  4054cf:	8a 44 24 0e          	mov    0xe(%rsp),%al
  4054d3:	48 8b 3c 24          	mov    (%rsp),%rdi
  4054d7:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4054dc:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4054e0:	0f b6 f0             	movzbl %al,%esi
  4054e3:	e8 d8 dc ff ff       	call   4031c0 <runtime::[heap_allocator_unix.odin]::_heap_alloc>
  4054e8:	48 83 c4 18          	add    $0x18,%rsp
  4054ec:	c3                   	ret
  4054ed:	0f 1f 00             	nopl   (%rax)

00000000004054f0 <runtime::heap_resize>:
  4054f0:	48 83 ec 28          	sub    $0x28,%rsp
  4054f4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  4054f9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  4054fe:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  405503:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  405508:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40550d:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  405512:	e8 09 dd ff ff       	call   403220 <runtime::[heap_allocator_unix.odin]::_heap_resize>
  405517:	48 83 c4 28          	add    $0x28,%rsp
  40551b:	c3                   	ret
  40551c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000405520 <runtime::heap_free>:
  405520:	48 83 ec 18          	sub    $0x18,%rsp
  405524:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  405529:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40552e:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  405533:	e8 18 dd ff ff       	call   403250 <runtime::[heap_allocator_unix.odin]::_heap_free>
  405538:	48 83 c4 18          	add    $0x18,%rsp
  40553c:	c3                   	ret
  40553d:	0f 1f 00             	nopl   (%rax)

0000000000405540 <runtime::heap_allocator_proc.aligned_alloc-0>:
  405540:	48 81 ec b8 00 00 00 	sub    $0xb8,%rsp
  405547:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40554c:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  405551:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  405556:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40555b:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  405560:	44 88 c0             	mov    %r8b,%al
  405563:	88 44 24 3f          	mov    %al,0x3f(%rsp)
  405567:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40556e:	00 
  40556f:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  405574:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405579:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40557e:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  405583:	8a 4c 24 3f          	mov    0x3f(%rsp),%cl
  405587:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40558c:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  405593:	00 
  405594:	48 89 b4 24 a8 00 00 	mov    %rsi,0xa8(%rsp)
  40559b:	00 
  40559c:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  4055a3:	00 
  4055a4:	48 89 bc 24 98 00 00 	mov    %rdi,0x98(%rsp)
  4055ab:	00 
  4055ac:	88 8c 24 97 00 00 00 	mov    %cl,0x97(%rsp)
  4055b3:	b9 08 00 00 00       	mov    $0x8,%ecx
  4055b8:	48 83 fe 08          	cmp    $0x8,%rsi
  4055bc:	48 0f 4f ce          	cmovg  %rsi,%rcx
  4055c0:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  4055c7:	00 
  4055c8:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  4055cf:	00 
  4055d0:	48 83 e9 01          	sub    $0x1,%rcx
  4055d4:	48 83 c1 08          	add    $0x8,%rcx
  4055d8:	48 01 d1             	add    %rdx,%rcx
  4055db:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  4055e2:	00 
  4055e3:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  4055ea:	00 00 
  4055ec:	48 83 f8 00          	cmp    $0x0,%rax
  4055f0:	0f 95 c1             	setne  %cl
  4055f3:	80 e1 01             	and    $0x1,%cl
  4055f6:	31 c0                	xor    %eax,%eax
  4055f8:	80 f9 00             	cmp    $0x0,%cl
  4055fb:	88 44 24 0f          	mov    %al,0xf(%rsp)
  4055ff:	74 17                	je     405618 <runtime::heap_allocator_proc.aligned_alloc-0+0xd8>
  405601:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  405606:	48 83 f8 08          	cmp    $0x8,%rax
  40560a:	0f 9f c0             	setg   %al
  40560d:	24 01                	and    $0x1,%al
  40560f:	3c 00                	cmp    $0x0,%al
  405611:	0f 95 c0             	setne  %al
  405614:	88 44 24 0f          	mov    %al,0xf(%rsp)
  405618:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40561d:	8a 4c 24 0f          	mov    0xf(%rsp),%cl
  405621:	80 e1 01             	and    $0x1,%cl
  405624:	88 4c 24 77          	mov    %cl,0x77(%rsp)
  405628:	48 83 f8 00          	cmp    $0x0,%rax
  40562c:	0f 95 c0             	setne  %al
  40562f:	24 01                	and    $0x1,%al
  405631:	3c 00                	cmp    $0x0,%al
  405633:	74 2e                	je     405663 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  405635:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40563a:	75 27                	jne    405663 <runtime::heap_allocator_proc.aligned_alloc-0+0x123>
  40563c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  405641:	48 8b 40 f8          	mov    -0x8(%rax),%rax
  405645:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40564a:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40564f:	48 8b b4 24 80 00 00 	mov    0x80(%rsp),%rsi
  405656:	00 
  405657:	e8 94 fe ff ff       	call   4054f0 <runtime::heap_resize>
  40565c:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  405661:	eb 19                	jmp    40567c <runtime::heap_allocator_proc.aligned_alloc-0+0x13c>
  405663:	8a 44 24 3f          	mov    0x3f(%rsp),%al
  405667:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  40566e:	00 
  40566f:	0f b6 f0             	movzbl %al,%esi
  405672:	e8 49 fe ff ff       	call   4054c0 <runtime::heap_alloc>
  405677:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40567c:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  405681:	48 83 c0 08          	add    $0x8,%rax
  405685:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40568a:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40568f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405694:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  405699:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40569e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  4056a3:	48 03 84 24 88 00 00 	add    0x88(%rsp),%rax
  4056aa:	00 
  4056ab:	48 83 e8 01          	sub    $0x1,%rax
  4056af:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  4056b6:	00 
  4056b7:	48 83 e9 01          	sub    $0x1,%rcx
  4056bb:	48 83 f1 ff          	xor    $0xffffffffffffffff,%rcx
  4056bf:	48 21 c8             	and    %rcx,%rax
  4056c2:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4056c7:	48 83 7c 24 78 00    	cmpq   $0x0,0x78(%rsp)
  4056cd:	0f 94 c0             	sete   %al
  4056d0:	24 01                	and    $0x1,%al
  4056d2:	3c 00                	cmp    $0x0,%al
  4056d4:	74 39                	je     40570f <runtime::heap_allocator_proc.aligned_alloc-0+0x1cf>
  4056d6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4056db:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4056e0:	e8 ab 00 00 00       	call   405790 <runtime::heap_allocator_proc.aligned_free-1>
  4056e5:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  4056ea:	48 8b 7c 24 78       	mov    0x78(%rsp),%rdi
  4056ef:	e8 9c 00 00 00       	call   405790 <runtime::heap_allocator_proc.aligned_free-1>
  4056f4:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4056f9:	31 f6                	xor    %esi,%esi
  4056fb:	ba 10 00 00 00       	mov    $0x10,%edx
  405700:	e8 3b b9 ff ff       	call   401040 <memset@plt>
  405705:	b0 01                	mov    $0x1,%al
  405707:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  40570e:	c3                   	ret
  40570f:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405714:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405719:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40571e:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  405723:	48 89 48 f8          	mov    %rcx,-0x8(%rax)
  405727:	80 7c 24 77 00       	cmpb   $0x0,0x77(%rsp)
  40572c:	74 2f                	je     40575d <runtime::heap_allocator_proc.aligned_alloc-0+0x21d>
  40572e:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  405733:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  405738:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40573d:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  405742:	48 39 d0             	cmp    %rdx,%rax
  405745:	48 0f 4c d0          	cmovl  %rax,%rdx
  405749:	e8 52 6e 00 00       	call   40c5a0 <runtime::mem_copy_non_overlapping>
  40574e:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  405753:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405758:	e8 33 00 00 00       	call   405790 <runtime::heap_allocator_proc.aligned_free-1>
  40575d:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  405762:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  405767:	e8 94 6d 00 00       	call   40c500 <runtime::[internal.odin]::byte_slice>
  40576c:	48 89 c1             	mov    %rax,%rcx
  40576f:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405774:	48 89 50 08          	mov    %rdx,0x8(%rax)
  405778:	48 89 08             	mov    %rcx,(%rax)
  40577b:	31 c0                	xor    %eax,%eax
  40577d:	48 81 c4 b8 00 00 00 	add    $0xb8,%rsp
  405784:	c3                   	ret
  405785:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40578c:	00 00 00 00 

0000000000405790 <runtime::heap_allocator_proc.aligned_free-1>:
  405790:	48 83 ec 18          	sub    $0x18,%rsp
  405794:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  405799:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40579e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  4057a3:	48 83 f8 00          	cmp    $0x0,%rax
  4057a7:	0f 95 c0             	setne  %al
  4057aa:	24 01                	and    $0x1,%al
  4057ac:	3c 00                	cmp    $0x0,%al
  4057ae:	74 0e                	je     4057be <runtime::heap_allocator_proc.aligned_free-1+0x2e>
  4057b0:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4057b5:	48 8b 78 f8          	mov    -0x8(%rax),%rdi
  4057b9:	e8 62 fd ff ff       	call   405520 <runtime::heap_free>
  4057be:	48 83 c4 18          	add    $0x18,%rsp
  4057c2:	c3                   	ret
  4057c3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4057ca:	84 00 00 00 00 00 

00000000004057d0 <runtime::heap_allocator_proc.aligned_resize-2>:
  4057d0:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  4057d7:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  4057dc:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  4057e1:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  4057e6:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  4057eb:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  4057f0:	44 88 c0             	mov    %r8b,%al
  4057f3:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  4057f7:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  4057fe:	00 
  4057ff:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  405804:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  405809:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40580d:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  405812:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  405817:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40581c:	48 89 bc 24 e0 00 00 	mov    %rdi,0xe0(%rsp)
  405823:	00 
  405824:	48 89 b4 24 d8 00 00 	mov    %rsi,0xd8(%rsp)
  40582b:	00 
  40582c:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  405833:	00 
  405834:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  40583b:	00 
  40583c:	88 84 24 c7 00 00 00 	mov    %al,0xc7(%rsp)
  405843:	48 8d bc 24 b0 00 00 	lea    0xb0(%rsp),%rdi
  40584a:	00 
  40584b:	31 f6                	xor    %esi,%esi
  40584d:	ba 10 00 00 00       	mov    $0x10,%edx
  405852:	e8 e9 b7 ff ff       	call   401040 <memset@plt>
  405857:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40585c:	c6 84 24 af 00 00 00 	movb   $0x0,0xaf(%rsp)
  405863:	00 
  405864:	48 83 f8 00          	cmp    $0x0,%rax
  405868:	0f 94 c0             	sete   %al
  40586b:	24 01                	and    $0x1,%al
  40586d:	3c 00                	cmp    $0x0,%al
  40586f:	0f 84 80 00 00 00    	je     4058f5 <runtime::heap_allocator_proc.aligned_resize-2+0x125>
  405875:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40587a:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40587f:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  405884:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  405888:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  40588d:	0f 57 c0             	xorps  %xmm0,%xmm0
  405890:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  405897:	00 
  405898:	48 89 e2             	mov    %rsp,%rdx
  40589b:	4c 89 02             	mov    %r8,(%rdx)
  40589e:	44 0f b6 c0          	movzbl %al,%r8d
  4058a2:	31 c0                	xor    %eax,%eax
  4058a4:	89 c2                	mov    %eax,%edx
  4058a6:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  4058ad:	00 
  4058ae:	e8 8d fc ff ff       	call   405540 <runtime::heap_allocator_proc.aligned_alloc-0>
  4058b3:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  4058b8:	40 88 c7             	mov    %al,%dil
  4058bb:	40 88 f8             	mov    %dil,%al
  4058be:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  4058c5:	00 
  4058c6:	48 8b b4 24 98 00 00 	mov    0x98(%rsp),%rsi
  4058cd:	00 
  4058ce:	48 89 b4 24 b8 00 00 	mov    %rsi,0xb8(%rsp)
  4058d5:	00 
  4058d6:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  4058dd:	00 
  4058de:	40 88 bc 24 af 00 00 	mov    %dil,0xaf(%rsp)
  4058e5:	00 
  4058e6:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4058ea:	48 89 11             	mov    %rdx,(%rcx)
  4058ed:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4058f4:	c3                   	ret
  4058f5:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4058fa:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4058ff:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  405904:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  405909:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40590d:	4c 8b 4c 24 60       	mov    0x60(%rsp),%r9
  405912:	0f 57 c0             	xorps  %xmm0,%xmm0
  405915:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40591c:	00 
  40591d:	49 89 e0             	mov    %rsp,%r8
  405920:	4d 89 08             	mov    %r9,(%r8)
  405923:	44 0f b6 c0          	movzbl %al,%r8d
  405927:	4c 8d 8c 24 80 00 00 	lea    0x80(%rsp),%r9
  40592e:	00 
  40592f:	e8 0c fc ff ff       	call   405540 <runtime::heap_allocator_proc.aligned_alloc-0>
  405934:	88 44 24 1f          	mov    %al,0x1f(%rsp)
  405938:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  40593f:	00 
  405940:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  405945:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40594c:	00 
  40594d:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  405952:	3c 00                	cmp    $0x0,%al
  405954:	74 4d                	je     4059a3 <runtime::heap_allocator_proc.aligned_resize-2+0x1d3>
  405956:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40595b:	8a 44 24 1f          	mov    0x1f(%rsp),%al
  40595f:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  405966:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  40596d:	00 
  40596e:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  405975:	00 
  405976:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  40597d:	48 89 b4 24 b8 00 00 	mov    %rsi,0xb8(%rsp)
  405984:	00 
  405985:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  40598c:	00 
  40598d:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  405994:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405998:	48 89 11             	mov    %rdx,(%rcx)
  40599b:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  4059a2:	c3                   	ret
  4059a3:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  4059a7:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  4059ac:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4059b1:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  4059b8:	00 
  4059b9:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  4059c0:	00 
  4059c1:	3c 00                	cmp    $0x0,%al
  4059c3:	0f 84 85 00 00 00    	je     405a4e <runtime::heap_allocator_proc.aligned_resize-2+0x27e>
  4059c9:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  4059ce:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  4059d3:	48 39 c8             	cmp    %rcx,%rax
  4059d6:	0f 9f c0             	setg   %al
  4059d9:	24 01                	and    $0x1,%al
  4059db:	3c 00                	cmp    $0x0,%al
  4059dd:	74 6f                	je     405a4e <runtime::heap_allocator_proc.aligned_resize-2+0x27e>
  4059df:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  4059e4:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  4059eb:	00 
  4059ec:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  4059f1:	48 89 e0             	mov    %rsp,%rax
  4059f4:	4c 89 08             	mov    %r9,(%rax)
  4059f7:	bf d8 f7 40 00       	mov    $0x40f7d8,%edi
  4059fc:	be 2e 00 00 00       	mov    $0x2e,%esi
  405a01:	ba 4d 00 00 00       	mov    $0x4d,%edx
  405a06:	b9 26 00 00 00       	mov    $0x26,%ecx
  405a0b:	e8 50 c3 ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  405a10:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  405a15:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  405a1a:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405a1f:	48 89 c2             	mov    %rax,%rdx
  405a22:	48 03 94 24 b0 00 00 	add    0xb0(%rsp),%rdx
  405a29:	00 
  405a2a:	48 29 c1             	sub    %rax,%rcx
  405a2d:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  405a32:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  405a37:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  405a3c:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  405a41:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  405a46:	48 29 c6             	sub    %rax,%rsi
  405a49:	e8 f2 6f 00 00       	call   40ca40 <runtime::conditional_mem_zero>
  405a4e:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  405a53:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  405a5a:	00 
  405a5b:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  405a62:	00 
  405a63:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  405a6a:	48 89 b4 24 b8 00 00 	mov    %rsi,0xb8(%rsp)
  405a71:	00 
  405a72:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  405a79:	00 
  405a7a:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  405a81:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  405a85:	48 89 11             	mov    %rdx,(%rcx)
  405a88:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  405a8f:	c3                   	ret

0000000000405a90 <runtime::udivmod128>:
  405a90:	48 81 ec b8 01 00 00 	sub    $0x1b8,%rsp
  405a97:	4c 89 84 24 a8 00 00 	mov    %r8,0xa8(%rsp)
  405a9e:	00 
  405a9f:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  405aa6:	00 
  405aa7:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  405aae:	00 
  405aaf:	48 89 b4 24 c0 00 00 	mov    %rsi,0xc0(%rsp)
  405ab6:	00 
  405ab7:	48 89 bc 24 c8 00 00 	mov    %rdi,0xc8(%rsp)
  405abe:	00 
  405abf:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  405ac6:	00 
  405ac7:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  405ace:	00 
  405acf:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  405ad6:	00 
  405ad7:	48 8b b4 24 c0 00 00 	mov    0xc0(%rsp),%rsi
  405ade:	00 
  405adf:	48 8b bc 24 a8 00 00 	mov    0xa8(%rsp),%rdi
  405ae6:	00 
  405ae7:	48 89 94 24 a0 01 00 	mov    %rdx,0x1a0(%rsp)
  405aee:	00 
  405aef:	48 89 b4 24 a8 01 00 	mov    %rsi,0x1a8(%rsp)
  405af6:	00 
  405af7:	48 89 8c 24 98 01 00 	mov    %rcx,0x198(%rsp)
  405afe:	00 
  405aff:	48 89 84 24 90 01 00 	mov    %rax,0x190(%rsp)
  405b06:	00 
  405b07:	48 89 bc 24 88 01 00 	mov    %rdi,0x188(%rsp)
  405b0e:	00 
  405b0f:	48 89 b4 24 78 01 00 	mov    %rsi,0x178(%rsp)
  405b16:	00 
  405b17:	48 89 94 24 70 01 00 	mov    %rdx,0x170(%rsp)
  405b1e:	00 
  405b1f:	48 8b 94 24 70 01 00 	mov    0x170(%rsp),%rdx
  405b26:	00 
  405b27:	48 8b b4 24 78 01 00 	mov    0x178(%rsp),%rsi
  405b2e:	00 
  405b2f:	48 89 b4 24 68 01 00 	mov    %rsi,0x168(%rsp)
  405b36:	00 
  405b37:	48 89 94 24 60 01 00 	mov    %rdx,0x160(%rsp)
  405b3e:	00 
  405b3f:	48 89 8c 24 58 01 00 	mov    %rcx,0x158(%rsp)
  405b46:	00 
  405b47:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  405b4e:	00 
  405b4f:	48 8b 84 24 50 01 00 	mov    0x150(%rsp),%rax
  405b56:	00 
  405b57:	48 8b 8c 24 58 01 00 	mov    0x158(%rsp),%rcx
  405b5e:	00 
  405b5f:	48 89 8c 24 48 01 00 	mov    %rcx,0x148(%rsp)
  405b66:	00 
  405b67:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  405b6e:	00 
  405b6f:	48 8d bc 24 30 01 00 	lea    0x130(%rsp),%rdi
  405b76:	00 
  405b77:	31 f6                	xor    %esi,%esi
  405b79:	ba 10 00 00 00       	mov    $0x10,%edx
  405b7e:	e8 bd b4 ff ff       	call   401040 <memset@plt>
  405b83:	48 8d bc 24 20 01 00 	lea    0x120(%rsp),%rdi
  405b8a:	00 
  405b8b:	31 f6                	xor    %esi,%esi
  405b8d:	ba 10 00 00 00       	mov    $0x10,%edx
  405b92:	e8 a9 b4 ff ff       	call   401040 <memset@plt>
  405b97:	c7 84 24 1c 01 00 00 	movl   $0x0,0x11c(%rsp)
  405b9e:	00 00 00 00 
  405ba2:	48 83 bc 24 68 01 00 	cmpq   $0x0,0x168(%rsp)
  405ba9:	00 00 
  405bab:	0f 94 c0             	sete   %al
  405bae:	24 01                	and    $0x1,%al
  405bb0:	3c 00                	cmp    $0x0,%al
  405bb2:	0f 84 31 01 00 00    	je     405ce9 <runtime::udivmod128+0x259>
  405bb8:	48 83 bc 24 48 01 00 	cmpq   $0x0,0x148(%rsp)
  405bbf:	00 00 
  405bc1:	0f 94 c0             	sete   %al
  405bc4:	24 01                	and    $0x1,%al
  405bc6:	3c 00                	cmp    $0x0,%al
  405bc8:	0f 84 dc 00 00 00    	je     405caa <runtime::udivmod128+0x21a>
  405bce:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405bd5:	00 
  405bd6:	48 83 f8 00          	cmp    $0x0,%rax
  405bda:	0f 95 c0             	setne  %al
  405bdd:	24 01                	and    $0x1,%al
  405bdf:	3c 00                	cmp    $0x0,%al
  405be1:	74 72                	je     405c55 <runtime::udivmod128+0x1c5>
  405be3:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  405bea:	00 
  405beb:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  405bf2:	00 
  405bf3:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  405bfa:	00 
  405bfb:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  405c02:	00 
  405c03:	48 83 f8 00          	cmp    $0x0,%rax
  405c07:	74 1f                	je     405c28 <runtime::udivmod128+0x198>
  405c09:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  405c10:	00 
  405c11:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  405c18:	00 
  405c19:	31 d2                	xor    %edx,%edx
  405c1b:	48 f7 f1             	div    %rcx
  405c1e:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  405c25:	00 
  405c26:	eb 02                	jmp    405c2a <runtime::udivmod128+0x19a>
  405c28:	0f 0b                	ud2
  405c2a:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405c31:	00 
  405c32:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  405c39:	00 
  405c3a:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  405c41:	00 
  405c42:	48 8b 8c 24 10 01 00 	mov    0x110(%rsp),%rcx
  405c49:	00 
  405c4a:	48 89 08             	mov    %rcx,(%rax)
  405c4d:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405c54:	00 
  405c55:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  405c5c:	00 
  405c5d:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  405c64:	00 
  405c65:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  405c6c:	00 
  405c6d:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  405c74:	00 
  405c75:	48 83 f8 00          	cmp    $0x0,%rax
  405c79:	74 1c                	je     405c97 <runtime::udivmod128+0x207>
  405c7b:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  405c82:	00 
  405c83:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  405c8a:	00 
  405c8b:	31 d2                	xor    %edx,%edx
  405c8d:	48 f7 f1             	div    %rcx
  405c90:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  405c95:	eb 02                	jmp    405c99 <runtime::udivmod128+0x209>
  405c97:	0f 0b                	ud2
  405c99:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  405c9e:	31 c9                	xor    %ecx,%ecx
  405ca0:	89 ca                	mov    %ecx,%edx
  405ca2:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  405ca9:	c3                   	ret
  405caa:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405cb1:	00 
  405cb2:	48 83 f8 00          	cmp    $0x0,%rax
  405cb6:	0f 95 c0             	setne  %al
  405cb9:	24 01                	and    $0x1,%al
  405cbb:	3c 00                	cmp    $0x0,%al
  405cbd:	74 1b                	je     405cda <runtime::udivmod128+0x24a>
  405cbf:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405cc6:	00 
  405cc7:	48 8b 8c 24 60 01 00 	mov    0x160(%rsp),%rcx
  405cce:	00 
  405ccf:	48 89 08             	mov    %rcx,(%rax)
  405cd2:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405cd9:	00 
  405cda:	31 c0                	xor    %eax,%eax
  405cdc:	89 c2                	mov    %eax,%edx
  405cde:	48 89 d0             	mov    %rdx,%rax
  405ce1:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  405ce8:	c3                   	ret
  405ce9:	48 83 bc 24 40 01 00 	cmpq   $0x0,0x140(%rsp)
  405cf0:	00 00 
  405cf2:	0f 94 c0             	sete   %al
  405cf5:	24 01                	and    $0x1,%al
  405cf7:	3c 00                	cmp    $0x0,%al
  405cf9:	0f 84 df 03 00 00    	je     4060de <runtime::udivmod128+0x64e>
  405cff:	48 83 bc 24 48 01 00 	cmpq   $0x0,0x148(%rsp)
  405d06:	00 00 
  405d08:	0f 94 c0             	sete   %al
  405d0b:	24 01                	and    $0x1,%al
  405d0d:	3c 00                	cmp    $0x0,%al
  405d0f:	0f 84 ae 00 00 00    	je     405dc3 <runtime::udivmod128+0x333>
  405d15:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405d1c:	00 
  405d1d:	48 83 f8 00          	cmp    $0x0,%rax
  405d21:	0f 95 c0             	setne  %al
  405d24:	24 01                	and    $0x1,%al
  405d26:	3c 00                	cmp    $0x0,%al
  405d28:	74 50                	je     405d7a <runtime::udivmod128+0x2ea>
  405d2a:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  405d31:	00 
  405d32:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  405d37:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  405d3e:	00 
  405d3f:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  405d44:	48 83 f8 00          	cmp    $0x0,%rax
  405d48:	74 16                	je     405d60 <runtime::udivmod128+0x2d0>
  405d4a:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  405d4f:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  405d54:	31 d2                	xor    %edx,%edx
  405d56:	48 f7 f1             	div    %rcx
  405d59:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  405d5e:	eb 02                	jmp    405d62 <runtime::udivmod128+0x2d2>
  405d60:	0f 0b                	ud2
  405d62:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405d69:	00 
  405d6a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  405d6f:	48 89 08             	mov    %rcx,(%rax)
  405d72:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  405d79:	00 
  405d7a:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  405d81:	00 
  405d82:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  405d87:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  405d8e:	00 
  405d8f:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  405d94:	48 83 f8 00          	cmp    $0x0,%rax
  405d98:	74 16                	je     405db0 <runtime::udivmod128+0x320>
  405d9a:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  405d9f:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  405da4:	31 d2                	xor    %edx,%edx
  405da6:	48 f7 f1             	div    %rcx
  405da9:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  405dae:	eb 02                	jmp    405db2 <runtime::udivmod128+0x322>
  405db0:	0f 0b                	ud2
  405db2:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  405db7:	31 c9                	xor    %ecx,%ecx
  405db9:	89 ca                	mov    %ecx,%edx
  405dbb:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  405dc2:	c3                   	ret
  405dc3:	48 83 bc 24 60 01 00 	cmpq   $0x0,0x160(%rsp)
  405dca:	00 00 
  405dcc:	0f 94 c0             	sete   %al
  405dcf:	24 01                	and    $0x1,%al
  405dd1:	3c 00                	cmp    $0x0,%al
  405dd3:	0f 84 e3 00 00 00    	je     405ebc <runtime::udivmod128+0x42c>
  405dd9:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405de0:	00 
  405de1:	48 83 f8 00          	cmp    $0x0,%rax
  405de5:	0f 95 c0             	setne  %al
  405de8:	24 01                	and    $0x1,%al
  405dea:	3c 00                	cmp    $0x0,%al
  405dec:	0f 84 81 00 00 00    	je     405e73 <runtime::udivmod128+0x3e3>
  405df2:	48 8d 84 24 20 01 00 	lea    0x120(%rsp),%rax
  405df9:	00 
  405dfa:	48 83 c0 08          	add    $0x8,%rax
  405dfe:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  405e03:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  405e0a:	00 
  405e0b:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  405e10:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  405e17:	00 
  405e18:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  405e1d:	48 83 f8 00          	cmp    $0x0,%rax
  405e21:	74 16                	je     405e39 <runtime::udivmod128+0x3a9>
  405e23:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  405e28:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  405e2d:	31 d2                	xor    %edx,%edx
  405e2f:	48 f7 f1             	div    %rcx
  405e32:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  405e37:	eb 02                	jmp    405e3b <runtime::udivmod128+0x3ab>
  405e39:	0f 0b                	ud2
  405e3b:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405e42:	00 
  405e43:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  405e48:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  405e4d:	48 89 11             	mov    %rdx,(%rcx)
  405e50:	48 c7 84 24 20 01 00 	movq   $0x0,0x120(%rsp)
  405e57:	00 00 00 00 00 
  405e5c:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  405e63:	00 
  405e64:	48 8b 94 24 28 01 00 	mov    0x128(%rsp),%rdx
  405e6b:	00 
  405e6c:	48 89 50 08          	mov    %rdx,0x8(%rax)
  405e70:	48 89 08             	mov    %rcx,(%rax)
  405e73:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  405e7a:	00 
  405e7b:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  405e80:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  405e87:	00 
  405e88:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  405e8d:	48 83 f8 00          	cmp    $0x0,%rax
  405e91:	74 16                	je     405ea9 <runtime::udivmod128+0x419>
  405e93:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  405e98:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  405e9d:	31 d2                	xor    %edx,%edx
  405e9f:	48 f7 f1             	div    %rcx
  405ea2:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  405ea7:	eb 02                	jmp    405eab <runtime::udivmod128+0x41b>
  405ea9:	0f 0b                	ud2
  405eab:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  405eb0:	31 c9                	xor    %ecx,%ecx
  405eb2:	89 ca                	mov    %ecx,%edx
  405eb4:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  405ebb:	c3                   	ret
  405ebc:	48 8b 84 24 48 01 00 	mov    0x148(%rsp),%rax
  405ec3:	00 
  405ec4:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  405ecb:	00 
  405ecc:	48 83 e9 01          	sub    $0x1,%rcx
  405ed0:	48 21 c8             	and    %rcx,%rax
  405ed3:	48 83 f8 00          	cmp    $0x0,%rax
  405ed7:	0f 94 c0             	sete   %al
  405eda:	24 01                	and    $0x1,%al
  405edc:	3c 00                	cmp    $0x0,%al
  405ede:	0f 84 9b 00 00 00    	je     405f7f <runtime::udivmod128+0x4ef>
  405ee4:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405eeb:	00 
  405eec:	48 83 f8 00          	cmp    $0x0,%rax
  405ef0:	0f 95 c0             	setne  %al
  405ef3:	24 01                	and    $0x1,%al
  405ef5:	3c 00                	cmp    $0x0,%al
  405ef7:	74 4d                	je     405f46 <runtime::udivmod128+0x4b6>
  405ef9:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405f00:	00 
  405f01:	48 8b 8c 24 60 01 00 	mov    0x160(%rsp),%rcx
  405f08:	00 
  405f09:	48 89 8c 24 20 01 00 	mov    %rcx,0x120(%rsp)
  405f10:	00 
  405f11:	48 8b 8c 24 68 01 00 	mov    0x168(%rsp),%rcx
  405f18:	00 
  405f19:	48 8b 94 24 48 01 00 	mov    0x148(%rsp),%rdx
  405f20:	00 
  405f21:	48 ff ca             	dec    %rdx
  405f24:	48 21 d1             	and    %rdx,%rcx
  405f27:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  405f2e:	00 
  405f2f:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  405f36:	00 
  405f37:	48 8b 94 24 28 01 00 	mov    0x128(%rsp),%rdx
  405f3e:	00 
  405f3f:	48 89 50 08          	mov    %rdx,0x8(%rax)
  405f43:	48 89 08             	mov    %rcx,(%rax)
  405f46:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  405f4d:	00 
  405f4e:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  405f55:	00 
  405f56:	ba 40 00 00 00       	mov    $0x40,%edx
  405f5b:	f3 48 0f bc d1       	tzcnt  %rcx,%rdx
  405f60:	88 d1                	mov    %dl,%cl
  405f62:	48 d3 e8             	shr    %cl,%rax
  405f65:	48 89 c1             	mov    %rax,%rcx
  405f68:	31 c0                	xor    %eax,%eax
  405f6a:	48 83 ea 40          	sub    $0x40,%rdx
  405f6e:	89 c2                	mov    %eax,%edx
  405f70:	48 89 d0             	mov    %rdx,%rax
  405f73:	48 0f 42 c1          	cmovb  %rcx,%rax
  405f77:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  405f7e:	c3                   	ret
  405f7f:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  405f86:	00 
  405f87:	b8 7f 00 00 00       	mov    $0x7f,%eax
  405f8c:	48 0f bd c1          	bsr    %rcx,%rax
  405f90:	48 83 f0 3f          	xor    $0x3f,%rax
  405f94:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  405f9b:	00 
  405f9c:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  405fa1:	48 0f bd ca          	bsr    %rdx,%rcx
  405fa5:	48 83 f1 3f          	xor    $0x3f,%rcx
  405fa9:	29 c8                	sub    %ecx,%eax
  405fab:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  405fb2:	83 bc 24 1c 01 00 00 	cmpl   $0x3e,0x11c(%rsp)
  405fb9:	3e 
  405fba:	0f 97 c0             	seta   %al
  405fbd:	24 01                	and    $0x1,%al
  405fbf:	3c 00                	cmp    $0x0,%al
  405fc1:	74 43                	je     406006 <runtime::udivmod128+0x576>
  405fc3:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405fca:	00 
  405fcb:	48 83 f8 00          	cmp    $0x0,%rax
  405fcf:	0f 95 c0             	setne  %al
  405fd2:	24 01                	and    $0x1,%al
  405fd4:	3c 00                	cmp    $0x0,%al
  405fd6:	74 1f                	je     405ff7 <runtime::udivmod128+0x567>
  405fd8:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  405fdf:	00 
  405fe0:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  405fe7:	00 
  405fe8:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  405fef:	00 
  405ff0:	48 89 10             	mov    %rdx,(%rax)
  405ff3:	48 89 48 08          	mov    %rcx,0x8(%rax)
  405ff7:	31 c0                	xor    %eax,%eax
  405ff9:	89 c2                	mov    %eax,%edx
  405ffb:	48 89 d0             	mov    %rdx,%rax
  405ffe:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  406005:	c3                   	ret
  406006:	8b 84 24 1c 01 00 00 	mov    0x11c(%rsp),%eax
  40600d:	83 c0 01             	add    $0x1,%eax
  406010:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  406017:	48 c7 84 24 30 01 00 	movq   $0x0,0x130(%rsp)
  40601e:	00 00 00 00 00 
  406023:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  40602a:	00 
  40602b:	b9 40 00 00 00       	mov    $0x40,%ecx
  406030:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  406037:	89 c9                	mov    %ecx,%ecx
  406039:	89 ca                	mov    %ecx,%edx
  40603b:	48 89 d1             	mov    %rdx,%rcx
  40603e:	48 d3 e0             	shl    %cl,%rax
  406041:	48 89 c1             	mov    %rax,%rcx
  406044:	31 c0                	xor    %eax,%eax
  406046:	48 83 fa 40          	cmp    $0x40,%rdx
  40604a:	48 0f 42 c1          	cmovb  %rcx,%rax
  40604e:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  406055:	00 
  406056:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40605d:	00 
  40605e:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  406065:	89 ca                	mov    %ecx,%edx
  406067:	48 89 d1             	mov    %rdx,%rcx
  40606a:	48 d3 e8             	shr    %cl,%rax
  40606d:	48 89 c1             	mov    %rax,%rcx
  406070:	31 c0                	xor    %eax,%eax
  406072:	48 83 fa 40          	cmp    $0x40,%rdx
  406076:	48 0f 42 c1          	cmovb  %rcx,%rax
  40607a:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  406081:	00 
  406082:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  406089:	00 
  40608a:	b9 40 00 00 00       	mov    $0x40,%ecx
  40608f:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  406096:	89 c9                	mov    %ecx,%ecx
  406098:	89 ca                	mov    %ecx,%edx
  40609a:	48 89 d1             	mov    %rdx,%rcx
  40609d:	48 d3 e0             	shl    %cl,%rax
  4060a0:	48 89 c1             	mov    %rax,%rcx
  4060a3:	31 c0                	xor    %eax,%eax
  4060a5:	48 83 fa 40          	cmp    $0x40,%rdx
  4060a9:	48 0f 42 c1          	cmovb  %rcx,%rax
  4060ad:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  4060b4:	00 
  4060b5:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  4060bc:	89 ce                	mov    %ecx,%esi
  4060be:	48 89 f1             	mov    %rsi,%rcx
  4060c1:	48 d3 ea             	shr    %cl,%rdx
  4060c4:	31 c9                	xor    %ecx,%ecx
  4060c6:	48 83 fe 40          	cmp    $0x40,%rsi
  4060ca:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4060ce:	48 09 c8             	or     %rcx,%rax
  4060d1:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4060d8:	00 
  4060d9:	e9 2c 05 00 00       	jmp    40660a <runtime::udivmod128+0xb7a>
  4060de:	48 83 bc 24 48 01 00 	cmpq   $0x0,0x148(%rsp)
  4060e5:	00 00 
  4060e7:	0f 94 c0             	sete   %al
  4060ea:	24 01                	and    $0x1,%al
  4060ec:	3c 00                	cmp    $0x0,%al
  4060ee:	0f 84 76 03 00 00    	je     40646a <runtime::udivmod128+0x9da>
  4060f4:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  4060fb:	00 
  4060fc:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  406103:	00 
  406104:	48 83 e9 01          	sub    $0x1,%rcx
  406108:	48 21 c8             	and    %rcx,%rax
  40610b:	48 83 f8 00          	cmp    $0x0,%rax
  40610f:	0f 94 c0             	sete   %al
  406112:	24 01                	and    $0x1,%al
  406114:	3c 00                	cmp    $0x0,%al
  406116:	0f 84 14 01 00 00    	je     406230 <runtime::udivmod128+0x7a0>
  40611c:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  406123:	00 
  406124:	48 83 f8 00          	cmp    $0x0,%rax
  406128:	0f 95 c0             	setne  %al
  40612b:	24 01                	and    $0x1,%al
  40612d:	3c 00                	cmp    $0x0,%al
  40612f:	74 29                	je     40615a <runtime::udivmod128+0x6ca>
  406131:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  406138:	00 
  406139:	48 8b 8c 24 60 01 00 	mov    0x160(%rsp),%rcx
  406140:	00 
  406141:	48 8b 94 24 40 01 00 	mov    0x140(%rsp),%rdx
  406148:	00 
  406149:	48 ff ca             	dec    %rdx
  40614c:	48 21 d1             	and    %rdx,%rcx
  40614f:	48 89 08             	mov    %rcx,(%rax)
  406152:	48 c7 40 08 00 00 00 	movq   $0x0,0x8(%rax)
  406159:	00 
  40615a:	48 83 bc 24 40 01 00 	cmpq   $0x1,0x140(%rsp)
  406161:	00 01 
  406163:	0f 94 c0             	sete   %al
  406166:	24 01                	and    $0x1,%al
  406168:	3c 00                	cmp    $0x0,%al
  40616a:	74 18                	je     406184 <runtime::udivmod128+0x6f4>
  40616c:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  406173:	00 
  406174:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  40617b:	00 
  40617c:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  406183:	c3                   	ret
  406184:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  40618b:	00 
  40618c:	b8 40 00 00 00       	mov    $0x40,%eax
  406191:	f3 48 0f bc c1       	tzcnt  %rcx,%rax
  406196:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  40619d:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  4061a4:	00 
  4061a5:	8b 84 24 1c 01 00 00 	mov    0x11c(%rsp),%eax
  4061ac:	89 44 24 04          	mov    %eax,0x4(%rsp)
  4061b0:	88 c1                	mov    %al,%cl
  4061b2:	48 d3 ea             	shr    %cl,%rdx
  4061b5:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  4061b9:	31 c0                	xor    %eax,%eax
  4061bb:	83 e9 40             	sub    $0x40,%ecx
  4061be:	48 89 c1             	mov    %rax,%rcx
  4061c1:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4061c5:	48 89 8c 24 38 01 00 	mov    %rcx,0x138(%rsp)
  4061cc:	00 
  4061cd:	48 8b bc 24 60 01 00 	mov    0x160(%rsp),%rdi
  4061d4:	00 
  4061d5:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  4061dc:	00 
  4061dd:	8b b4 24 1c 01 00 00 	mov    0x11c(%rsp),%esi
  4061e4:	40 88 f1             	mov    %sil,%cl
  4061e7:	48 d3 ef             	shr    %cl,%rdi
  4061ea:	83 ee 40             	sub    $0x40,%esi
  4061ed:	48 89 c1             	mov    %rax,%rcx
  4061f0:	48 0f 42 cf          	cmovb  %rdi,%rcx
  4061f4:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4061f9:	f7 de                	neg    %esi
  4061fb:	40 88 f1             	mov    %sil,%cl
  4061fe:	48 d3 e2             	shl    %cl,%rdx
  406201:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  406206:	83 ee 40             	sub    $0x40,%esi
  406209:	48 0f 42 c2          	cmovb  %rdx,%rax
  40620d:	48 09 c8             	or     %rcx,%rax
  406210:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  406217:	00 
  406218:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40621f:	00 
  406220:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  406227:	00 
  406228:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  40622f:	c3                   	ret
  406230:	48 8b 8c 24 40 01 00 	mov    0x140(%rsp),%rcx
  406237:	00 
  406238:	b8 7f 00 00 00       	mov    $0x7f,%eax
  40623d:	48 0f bd c1          	bsr    %rcx,%rax
  406241:	48 83 f0 3f          	xor    $0x3f,%rax
  406245:	83 c0 41             	add    $0x41,%eax
  406248:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  40624f:	00 
  406250:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  406255:	48 0f bd ca          	bsr    %rdx,%rcx
  406259:	48 83 f1 3f          	xor    $0x3f,%rcx
  40625d:	29 c8                	sub    %ecx,%eax
  40625f:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  406266:	83 bc 24 1c 01 00 00 	cmpl   $0x40,0x11c(%rsp)
  40626d:	40 
  40626e:	0f 94 c1             	sete   %cl
  406271:	80 e1 01             	and    $0x1,%cl
  406274:	b0 01                	mov    $0x1,%al
  406276:	38 c8                	cmp    %cl,%al
  406278:	74 16                	je     406290 <runtime::udivmod128+0x800>
  40627a:	83 bc 24 1c 01 00 00 	cmpl   $0x40,0x11c(%rsp)
  406281:	40 
  406282:	0f 92 c1             	setb   %cl
  406285:	80 e1 01             	and    $0x1,%cl
  406288:	b0 01                	mov    $0x1,%al
  40628a:	38 c8                	cmp    %cl,%al
  40628c:	74 44                	je     4062d2 <runtime::udivmod128+0x842>
  40628e:	eb 3d                	jmp    4062cd <runtime::udivmod128+0x83d>
  406290:	48 c7 84 24 30 01 00 	movq   $0x0,0x130(%rsp)
  406297:	00 00 00 00 00 
  40629c:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  4062a3:	00 
  4062a4:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  4062ab:	00 
  4062ac:	48 c7 84 24 28 01 00 	movq   $0x0,0x128(%rsp)
  4062b3:	00 00 00 00 00 
  4062b8:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  4062bf:	00 
  4062c0:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4062c7:	00 
  4062c8:	e9 98 01 00 00       	jmp    406465 <runtime::udivmod128+0x9d5>
  4062cd:	e9 c7 00 00 00       	jmp    406399 <runtime::udivmod128+0x909>
  4062d2:	48 c7 84 24 30 01 00 	movq   $0x0,0x130(%rsp)
  4062d9:	00 00 00 00 00 
  4062de:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  4062e5:	00 
  4062e6:	b9 40 00 00 00       	mov    $0x40,%ecx
  4062eb:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  4062f2:	89 c9                	mov    %ecx,%ecx
  4062f4:	89 ca                	mov    %ecx,%edx
  4062f6:	48 89 d1             	mov    %rdx,%rcx
  4062f9:	48 d3 e0             	shl    %cl,%rax
  4062fc:	48 89 c1             	mov    %rax,%rcx
  4062ff:	31 c0                	xor    %eax,%eax
  406301:	48 83 fa 40          	cmp    $0x40,%rdx
  406305:	48 0f 42 c1          	cmovb  %rcx,%rax
  406309:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  406310:	00 
  406311:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  406318:	00 
  406319:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  406320:	89 ca                	mov    %ecx,%edx
  406322:	48 89 d1             	mov    %rdx,%rcx
  406325:	48 d3 e8             	shr    %cl,%rax
  406328:	48 89 c1             	mov    %rax,%rcx
  40632b:	31 c0                	xor    %eax,%eax
  40632d:	48 83 fa 40          	cmp    $0x40,%rdx
  406331:	48 0f 42 c1          	cmovb  %rcx,%rax
  406335:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  40633c:	00 
  40633d:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  406344:	00 
  406345:	b9 40 00 00 00       	mov    $0x40,%ecx
  40634a:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  406351:	89 c9                	mov    %ecx,%ecx
  406353:	89 ca                	mov    %ecx,%edx
  406355:	48 89 d1             	mov    %rdx,%rcx
  406358:	48 d3 e0             	shl    %cl,%rax
  40635b:	48 89 c1             	mov    %rax,%rcx
  40635e:	31 c0                	xor    %eax,%eax
  406360:	48 83 fa 40          	cmp    $0x40,%rdx
  406364:	48 0f 42 c1          	cmovb  %rcx,%rax
  406368:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  40636f:	00 
  406370:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  406377:	89 ce                	mov    %ecx,%esi
  406379:	48 89 f1             	mov    %rsi,%rcx
  40637c:	48 d3 ea             	shr    %cl,%rdx
  40637f:	31 c9                	xor    %ecx,%ecx
  406381:	48 83 fe 40          	cmp    $0x40,%rsi
  406385:	48 0f 42 ca          	cmovb  %rdx,%rcx
  406389:	48 09 c8             	or     %rcx,%rax
  40638c:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  406393:	00 
  406394:	e9 cc 00 00 00       	jmp    406465 <runtime::udivmod128+0x9d5>
  406399:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  4063a0:	00 
  4063a1:	b9 80 00 00 00       	mov    $0x80,%ecx
  4063a6:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  4063ad:	89 c9                	mov    %ecx,%ecx
  4063af:	89 ca                	mov    %ecx,%edx
  4063b1:	48 89 d1             	mov    %rdx,%rcx
  4063b4:	48 d3 e0             	shl    %cl,%rax
  4063b7:	48 89 c1             	mov    %rax,%rcx
  4063ba:	31 c0                	xor    %eax,%eax
  4063bc:	48 83 fa 40          	cmp    $0x40,%rdx
  4063c0:	48 0f 42 c1          	cmovb  %rcx,%rax
  4063c4:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  4063cb:	00 
  4063cc:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  4063d3:	00 
  4063d4:	b9 80 00 00 00       	mov    $0x80,%ecx
  4063d9:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  4063e0:	89 c9                	mov    %ecx,%ecx
  4063e2:	89 ca                	mov    %ecx,%edx
  4063e4:	48 89 d1             	mov    %rdx,%rcx
  4063e7:	48 d3 e0             	shl    %cl,%rax
  4063ea:	48 89 c1             	mov    %rax,%rcx
  4063ed:	31 c0                	xor    %eax,%eax
  4063ef:	48 83 fa 40          	cmp    $0x40,%rdx
  4063f3:	48 0f 42 c1          	cmovb  %rcx,%rax
  4063f7:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  4063fe:	00 
  4063ff:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  406406:	83 e9 40             	sub    $0x40,%ecx
  406409:	89 c9                	mov    %ecx,%ecx
  40640b:	89 ce                	mov    %ecx,%esi
  40640d:	48 89 f1             	mov    %rsi,%rcx
  406410:	48 d3 ea             	shr    %cl,%rdx
  406413:	31 c9                	xor    %ecx,%ecx
  406415:	48 83 fe 40          	cmp    $0x40,%rsi
  406419:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40641d:	48 09 c8             	or     %rcx,%rax
  406420:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  406427:	00 
  406428:	48 c7 84 24 28 01 00 	movq   $0x0,0x128(%rsp)
  40642f:	00 00 00 00 00 
  406434:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  40643b:	00 
  40643c:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  406443:	83 e9 40             	sub    $0x40,%ecx
  406446:	89 c9                	mov    %ecx,%ecx
  406448:	89 ca                	mov    %ecx,%edx
  40644a:	48 89 d1             	mov    %rdx,%rcx
  40644d:	48 d3 e8             	shr    %cl,%rax
  406450:	48 89 c1             	mov    %rax,%rcx
  406453:	31 c0                	xor    %eax,%eax
  406455:	48 83 fa 40          	cmp    $0x40,%rdx
  406459:	48 0f 42 c1          	cmovb  %rcx,%rax
  40645d:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  406464:	00 
  406465:	e9 9e 01 00 00       	jmp    406608 <runtime::udivmod128+0xb78>
  40646a:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  406471:	00 
  406472:	b8 7f 00 00 00       	mov    $0x7f,%eax
  406477:	48 0f bd c1          	bsr    %rcx,%rax
  40647b:	48 83 f0 3f          	xor    $0x3f,%rax
  40647f:	48 8b 94 24 68 01 00 	mov    0x168(%rsp),%rdx
  406486:	00 
  406487:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40648c:	48 0f bd ca          	bsr    %rdx,%rcx
  406490:	48 83 f1 3f          	xor    $0x3f,%rcx
  406494:	29 c8                	sub    %ecx,%eax
  406496:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  40649d:	83 bc 24 1c 01 00 00 	cmpl   $0x3f,0x11c(%rsp)
  4064a4:	3f 
  4064a5:	0f 97 c0             	seta   %al
  4064a8:	24 01                	and    $0x1,%al
  4064aa:	3c 00                	cmp    $0x0,%al
  4064ac:	74 43                	je     4064f1 <runtime::udivmod128+0xa61>
  4064ae:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4064b5:	00 
  4064b6:	48 83 f8 00          	cmp    $0x0,%rax
  4064ba:	0f 95 c0             	setne  %al
  4064bd:	24 01                	and    $0x1,%al
  4064bf:	3c 00                	cmp    $0x0,%al
  4064c1:	74 1f                	je     4064e2 <runtime::udivmod128+0xa52>
  4064c3:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4064ca:	00 
  4064cb:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4064d2:	00 
  4064d3:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  4064da:	00 
  4064db:	48 89 10             	mov    %rdx,(%rax)
  4064de:	48 89 48 08          	mov    %rcx,0x8(%rax)
  4064e2:	31 c0                	xor    %eax,%eax
  4064e4:	89 c2                	mov    %eax,%edx
  4064e6:	48 89 d0             	mov    %rdx,%rax
  4064e9:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  4064f0:	c3                   	ret
  4064f1:	8b 84 24 1c 01 00 00 	mov    0x11c(%rsp),%eax
  4064f8:	83 c0 01             	add    $0x1,%eax
  4064fb:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  406502:	48 c7 84 24 30 01 00 	movq   $0x0,0x130(%rsp)
  406509:	00 00 00 00 00 
  40650e:	83 bc 24 1c 01 00 00 	cmpl   $0x40,0x11c(%rsp)
  406515:	40 
  406516:	0f 94 c0             	sete   %al
  406519:	24 01                	and    $0x1,%al
  40651b:	3c 00                	cmp    $0x0,%al
  40651d:	74 31                	je     406550 <runtime::udivmod128+0xac0>
  40651f:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  406526:	00 
  406527:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  40652e:	00 
  40652f:	48 c7 84 24 28 01 00 	movq   $0x0,0x128(%rsp)
  406536:	00 00 00 00 00 
  40653b:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  406542:	00 
  406543:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40654a:	00 
  40654b:	e9 b6 00 00 00       	jmp    406606 <runtime::udivmod128+0xb76>
  406550:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  406557:	00 
  406558:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  40655f:	89 ca                	mov    %ecx,%edx
  406561:	48 89 d1             	mov    %rdx,%rcx
  406564:	48 d3 e8             	shr    %cl,%rax
  406567:	48 89 c1             	mov    %rax,%rcx
  40656a:	31 c0                	xor    %eax,%eax
  40656c:	48 83 fa 40          	cmp    $0x40,%rdx
  406570:	48 0f 42 c1          	cmovb  %rcx,%rax
  406574:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  40657b:	00 
  40657c:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  406583:	00 
  406584:	b9 40 00 00 00       	mov    $0x40,%ecx
  406589:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  406590:	89 c9                	mov    %ecx,%ecx
  406592:	89 ca                	mov    %ecx,%edx
  406594:	48 89 d1             	mov    %rdx,%rcx
  406597:	48 d3 e0             	shl    %cl,%rax
  40659a:	48 89 c1             	mov    %rax,%rcx
  40659d:	31 c0                	xor    %eax,%eax
  40659f:	48 83 fa 40          	cmp    $0x40,%rdx
  4065a3:	48 0f 42 c1          	cmovb  %rcx,%rax
  4065a7:	48 8b 94 24 60 01 00 	mov    0x160(%rsp),%rdx
  4065ae:	00 
  4065af:	8b 8c 24 1c 01 00 00 	mov    0x11c(%rsp),%ecx
  4065b6:	89 ce                	mov    %ecx,%esi
  4065b8:	48 89 f1             	mov    %rsi,%rcx
  4065bb:	48 d3 ea             	shr    %cl,%rdx
  4065be:	31 c9                	xor    %ecx,%ecx
  4065c0:	48 83 fe 40          	cmp    $0x40,%rsi
  4065c4:	48 0f 42 ca          	cmovb  %rdx,%rcx
  4065c8:	48 09 c8             	or     %rcx,%rax
  4065cb:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4065d2:	00 
  4065d3:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  4065da:	00 
  4065db:	b9 40 00 00 00       	mov    $0x40,%ecx
  4065e0:	2b 8c 24 1c 01 00 00 	sub    0x11c(%rsp),%ecx
  4065e7:	89 c9                	mov    %ecx,%ecx
  4065e9:	89 ca                	mov    %ecx,%edx
  4065eb:	48 89 d1             	mov    %rdx,%rcx
  4065ee:	48 d3 e0             	shl    %cl,%rax
  4065f1:	48 89 c1             	mov    %rax,%rcx
  4065f4:	31 c0                	xor    %eax,%eax
  4065f6:	48 83 fa 40          	cmp    $0x40,%rdx
  4065fa:	48 0f 42 c1          	cmovb  %rcx,%rax
  4065fe:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  406605:	00 
  406606:	eb 00                	jmp    406608 <runtime::udivmod128+0xb78>
  406608:	eb 00                	jmp    40660a <runtime::udivmod128+0xb7a>
  40660a:	c7 84 24 0c 01 00 00 	movl   $0x0,0x10c(%rsp)
  406611:	00 00 00 00 
  406615:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  40661c:	00 00 00 00 00 
  406621:	48 c7 84 24 f0 00 00 	movq   $0x0,0xf0(%rsp)
  406628:	00 00 00 00 00 
  40662d:	83 bc 24 1c 01 00 00 	cmpl   $0x0,0x11c(%rsp)
  406634:	00 
  406635:	0f 97 c0             	seta   %al
  406638:	24 01                	and    $0x1,%al
  40663a:	3c 00                	cmp    $0x0,%al
  40663c:	0f 84 57 01 00 00    	je     406799 <runtime::udivmod128+0xd09>
  406642:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  406649:	00 
  40664a:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  406651:	00 
  406652:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  406659:	00 
  40665a:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  406661:	00 
  406662:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  406667:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  40666e:	00 
  40666f:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  406676:	00 
  406677:	48 8b 8c 24 38 01 00 	mov    0x138(%rsp),%rcx
  40667e:	00 
  40667f:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  406684:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40668b:	00 
  40668c:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  406693:	00 
  406694:	48 8b 84 24 38 01 00 	mov    0x138(%rsp),%rax
  40669b:	00 
  40669c:	48 0f a4 c8 01       	shld   $0x1,%rcx,%rax
  4066a1:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  4066a8:	00 
  4066a9:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  4066b0:	00 
  4066b1:	48 01 c0             	add    %rax,%rax
  4066b4:	8b 8c 24 0c 01 00 00 	mov    0x10c(%rsp),%ecx
  4066bb:	48 09 c8             	or     %rcx,%rax
  4066be:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  4066c5:	00 
  4066c6:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  4066cd:	00 
  4066ce:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  4066d5:	00 
  4066d6:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  4066dd:	00 
  4066de:	48 89 84 24 f0 00 00 	mov    %rax,0xf0(%rsp)
  4066e5:	00 
  4066e6:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  4066ed:	00 
  4066ee:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  4066f5:	00 
  4066f6:	48 f7 d0             	not    %rax
  4066f9:	48 f7 d1             	not    %rcx
  4066fc:	48 01 f1             	add    %rsi,%rcx
  4066ff:	48 11 d0             	adc    %rdx,%rax
  406702:	48 c1 f8 3f          	sar    $0x3f,%rax
  406706:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  40670d:	00 
  40670e:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  406715:	00 
  406716:	8b 84 24 e0 00 00 00 	mov    0xe0(%rsp),%eax
  40671d:	83 e0 01             	and    $0x1,%eax
  406720:	89 84 24 0c 01 00 00 	mov    %eax,0x10c(%rsp)
  406727:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40672e:	00 
  40672f:	48 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%rcx
  406736:	00 
  406737:	48 21 ca             	and    %rcx,%rdx
  40673a:	48 21 c6             	and    %rax,%rsi
  40673d:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  406744:	00 
  406745:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40674c:	00 
  40674d:	48 29 f1             	sub    %rsi,%rcx
  406750:	48 19 d0             	sbb    %rdx,%rax
  406753:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  40675a:	00 
  40675b:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  406762:	00 
  406763:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40676a:	00 
  40676b:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  406772:	00 
  406773:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  40677a:	00 
  40677b:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  406782:	00 
  406783:	8b 84 24 1c 01 00 00 	mov    0x11c(%rsp),%eax
  40678a:	83 e8 01             	sub    $0x1,%eax
  40678d:	89 84 24 1c 01 00 00 	mov    %eax,0x11c(%rsp)
  406794:	e9 94 fe ff ff       	jmp    40662d <runtime::udivmod128+0xb9d>
  406799:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4067a0:	00 
  4067a1:	48 8b 8c 24 30 01 00 	mov    0x130(%rsp),%rcx
  4067a8:	00 
  4067a9:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  4067b0:	00 
  4067b1:	48 0f a4 ca 01       	shld   $0x1,%rcx,%rdx
  4067b6:	48 01 c9             	add    %rcx,%rcx
  4067b9:	8b b4 24 0c 01 00 00 	mov    0x10c(%rsp),%esi
  4067c0:	48 09 f1             	or     %rsi,%rcx
  4067c3:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  4067ca:	00 
  4067cb:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  4067d2:	00 
  4067d3:	48 83 f8 00          	cmp    $0x0,%rax
  4067d7:	0f 95 c0             	setne  %al
  4067da:	24 01                	and    $0x1,%al
  4067dc:	3c 00                	cmp    $0x0,%al
  4067de:	74 1f                	je     4067ff <runtime::udivmod128+0xd6f>
  4067e0:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  4067e7:	00 
  4067e8:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  4067ef:	00 
  4067f0:	48 8b 94 24 f8 00 00 	mov    0xf8(%rsp),%rdx
  4067f7:	00 
  4067f8:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4067fc:	48 89 08             	mov    %rcx,(%rax)
  4067ff:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  406806:	00 
  406807:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  40680e:	00 
  40680f:	48 81 c4 b8 01 00 00 	add    $0x1b8,%rsp
  406816:	c3                   	ret
  406817:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40681e:	00 00 

0000000000406820 <runtime::memset>:
  406820:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  406825:	89 74 24 c4          	mov    %esi,-0x3c(%rsp)
  406829:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  40682e:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  406833:	48 8b 4c 24 c8       	mov    -0x38(%rsp),%rcx
  406838:	8b 54 24 c4          	mov    -0x3c(%rsp),%edx
  40683c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406841:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  406845:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40684a:	48 83 f8 00          	cmp    $0x0,%rax
  40684e:	0f 95 c0             	setne  %al
  406851:	24 01                	and    $0x1,%al
  406853:	3c 00                	cmp    $0x0,%al
  406855:	74 63                	je     4068ba <runtime::memset+0x9a>
  406857:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40685c:	48 83 f8 00          	cmp    $0x0,%rax
  406860:	0f 95 c0             	setne  %al
  406863:	24 01                	and    $0x1,%al
  406865:	3c 00                	cmp    $0x0,%al
  406867:	74 51                	je     4068ba <runtime::memset+0x9a>
  406869:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40686e:	8b 4c 24 c4          	mov    -0x3c(%rsp),%ecx
  406872:	88 4c 24 e7          	mov    %cl,-0x19(%rsp)
  406876:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40687b:	48 c7 44 24 d0 00 00 	movq   $0x0,-0x30(%rsp)
  406882:	00 00 
  406884:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  406889:	48 39 44 24 d0       	cmp    %rax,-0x30(%rsp)
  40688e:	0f 9c c0             	setl   %al
  406891:	24 01                	and    $0x1,%al
  406893:	3c 00                	cmp    $0x0,%al
  406895:	74 21                	je     4068b8 <runtime::memset+0x98>
  406897:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40689c:	48 8b 4c 24 d0       	mov    -0x30(%rsp),%rcx
  4068a1:	8a 54 24 e7          	mov    -0x19(%rsp),%dl
  4068a5:	88 14 08             	mov    %dl,(%rax,%rcx,1)
  4068a8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4068ad:	48 83 c0 01          	add    $0x1,%rax
  4068b1:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  4068b6:	eb cc                	jmp    406884 <runtime::memset+0x64>
  4068b8:	eb 00                	jmp    4068ba <runtime::memset+0x9a>
  4068ba:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  4068bf:	c3                   	ret

00000000004068c0 <runtime::print_string>:
  4068c0:	48 83 ec 48          	sub    $0x48,%rsp
  4068c4:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  4068c9:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4068ce:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4068d3:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4068d8:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  4068dd:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  4068e2:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  4068e9:	00 00 
  4068eb:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4068f0:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4068f5:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4068fa:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  4068ff:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  406906:	00 00 
  406908:	48 8d 54 24 18       	lea    0x18(%rsp),%rdx
  40690d:	e8 de af ff ff       	call   4018f0 <runtime::stderr_write>
  406912:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  406917:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40691c:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  406921:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406926:	48 83 c4 48          	add    $0x48,%rsp
  40692a:	c3                   	ret
  40692b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000406930 <runtime::print_byte>:
  406930:	48 83 ec 58          	sub    $0x58,%rsp
  406934:	40 88 f8             	mov    %dil,%al
  406937:	88 44 24 07          	mov    %al,0x7(%rsp)
  40693b:	8a 54 24 07          	mov    0x7(%rsp),%dl
  40693f:	88 54 24 57          	mov    %dl,0x57(%rsp)
  406943:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  40694a:	00 00 
  40694c:	0f 57 c0             	xorps  %xmm0,%xmm0
  40694f:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  406954:	c6 44 24 20 00       	movb   $0x0,0x20(%rsp)
  406959:	48 8d 44 24 20       	lea    0x20(%rsp),%rax
  40695e:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406963:	48 c7 44 24 18 01 00 	movq   $0x1,0x18(%rsp)
  40696a:	00 00 
  40696c:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406971:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  406976:	88 11                	mov    %dl,(%rcx)
  406978:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40697d:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406982:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  406987:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40698c:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  406993:	00 00 
  406995:	48 8d 54 24 08       	lea    0x8(%rsp),%rdx
  40699a:	e8 51 af ff ff       	call   4018f0 <runtime::stderr_write>
  40699f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4069a4:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4069a9:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4069ae:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  4069b3:	48 83 c4 58          	add    $0x58,%rsp
  4069b7:	c3                   	ret
  4069b8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4069bf:	00 

00000000004069c0 <runtime::print_u64>:
  4069c0:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  4069c7:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4069cc:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  4069d1:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  4069d8:	00 
  4069d9:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  4069e0:	00 
  4069e1:	48 8d 7c 24 47       	lea    0x47(%rsp),%rdi
  4069e6:	31 f6                	xor    %esi,%esi
  4069e8:	ba 81 00 00 00       	mov    $0x81,%edx
  4069ed:	e8 4e a6 ff ff       	call   401040 <memset@plt>
  4069f2:	48 c7 44 24 38 81 00 	movq   $0x81,0x38(%rsp)
  4069f9:	00 00 
  4069fb:	48 83 bc 24 c8 00 00 	cmpq   $0xa,0xc8(%rsp)
  406a02:	00 0a 
  406a04:	0f 93 c0             	setae  %al
  406a07:	24 01                	and    $0x1,%al
  406a09:	3c 00                	cmp    $0x0,%al
  406a0b:	74 5c                	je     406a69 <runtime::print_u64+0xa9>
  406a0d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406a12:	48 83 e8 01          	sub    $0x1,%rax
  406a16:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406a1b:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406a20:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406a25:	48 c7 c0 48 40 41 00 	mov    $0x414048,%rax
  406a2c:	48 8b 08             	mov    (%rax),%rcx
  406a2f:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406a36:	00 
  406a37:	be 0a 00 00 00       	mov    $0xa,%esi
  406a3c:	31 d2                	xor    %edx,%edx
  406a3e:	48 f7 f6             	div    %rsi
  406a41:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406a46:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  406a49:	88 4c 04 47          	mov    %cl,0x47(%rsp,%rax,1)
  406a4d:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406a54:	00 
  406a55:	b9 0a 00 00 00       	mov    $0xa,%ecx
  406a5a:	31 d2                	xor    %edx,%edx
  406a5c:	48 f7 f1             	div    %rcx
  406a5f:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  406a66:	00 
  406a67:	eb 92                	jmp    4069fb <runtime::print_u64+0x3b>
  406a69:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406a6e:	48 ff c8             	dec    %rax
  406a71:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406a76:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406a7b:	48 89 04 24          	mov    %rax,(%rsp)
  406a7f:	48 c7 c0 48 40 41 00 	mov    $0x414048,%rax
  406a86:	48 8b 08             	mov    (%rax),%rcx
  406a89:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406a90:	00 
  406a91:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  406a96:	48 ba cd cc cc cc cc 	movabs $0xcccccccccccccccd,%rdx
  406a9d:	cc cc cc 
  406aa0:	48 f7 e2             	mul    %rdx
  406aa3:	48 8b 04 24          	mov    (%rsp),%rax
  406aa7:	48 89 d6             	mov    %rdx,%rsi
  406aaa:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  406aaf:	48 c1 ee 02          	shr    $0x2,%rsi
  406ab3:	48 83 e6 fe          	and    $0xfffffffffffffffe,%rsi
  406ab7:	48 8d 34 b6          	lea    (%rsi,%rsi,4),%rsi
  406abb:	48 29 f2             	sub    %rsi,%rdx
  406abe:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  406ac1:	88 4c 04 47          	mov    %cl,0x47(%rsp,%rax,1)
  406ac5:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406aca:	48 8d 4c 14 47       	lea    0x47(%rsp,%rdx,1),%rcx
  406acf:	b8 81 00 00 00       	mov    $0x81,%eax
  406ad4:	48 29 d0             	sub    %rdx,%rax
  406ad7:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  406adc:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406ae1:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406ae6:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406aeb:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  406af2:	00 00 
  406af4:	48 8d 54 24 20       	lea    0x20(%rsp),%rdx
  406af9:	e8 f2 ad ff ff       	call   4018f0 <runtime::stderr_write>
  406afe:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  406b05:	c3                   	ret
  406b06:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  406b0d:	00 00 00 

0000000000406b10 <runtime::print_i64>:
  406b10:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  406b17:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  406b1c:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  406b21:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  406b28:	00 
  406b29:	31 c9                	xor    %ecx,%ecx
  406b2b:	89 ca                	mov    %ecx,%edx
  406b2d:	48 29 c2             	sub    %rax,%rdx
  406b30:	48 83 f8 00          	cmp    $0x0,%rax
  406b34:	48 89 c1             	mov    %rax,%rcx
  406b37:	48 0f 4c ca          	cmovl  %rdx,%rcx
  406b3b:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  406b42:	00 
  406b43:	48 83 f8 00          	cmp    $0x0,%rax
  406b47:	0f 9c c0             	setl   %al
  406b4a:	24 01                	and    $0x1,%al
  406b4c:	88 84 24 c7 00 00 00 	mov    %al,0xc7(%rsp)
  406b53:	48 8d 7c 24 46       	lea    0x46(%rsp),%rdi
  406b58:	31 f6                	xor    %esi,%esi
  406b5a:	ba 81 00 00 00       	mov    $0x81,%edx
  406b5f:	e8 dc a4 ff ff       	call   401040 <memset@plt>
  406b64:	48 c7 44 24 38 81 00 	movq   $0x81,0x38(%rsp)
  406b6b:	00 00 
  406b6d:	48 83 bc 24 c8 00 00 	cmpq   $0xa,0xc8(%rsp)
  406b74:	00 0a 
  406b76:	0f 93 c0             	setae  %al
  406b79:	24 01                	and    $0x1,%al
  406b7b:	3c 00                	cmp    $0x0,%al
  406b7d:	74 5c                	je     406bdb <runtime::print_i64+0xcb>
  406b7f:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406b84:	48 83 e8 01          	sub    $0x1,%rax
  406b88:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406b8d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406b92:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406b97:	48 c7 c0 48 40 41 00 	mov    $0x414048,%rax
  406b9e:	48 8b 08             	mov    (%rax),%rcx
  406ba1:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406ba8:	00 
  406ba9:	be 0a 00 00 00       	mov    $0xa,%esi
  406bae:	31 d2                	xor    %edx,%edx
  406bb0:	48 f7 f6             	div    %rsi
  406bb3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  406bb8:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  406bbb:	88 4c 04 46          	mov    %cl,0x46(%rsp,%rax,1)
  406bbf:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406bc6:	00 
  406bc7:	b9 0a 00 00 00       	mov    $0xa,%ecx
  406bcc:	31 d2                	xor    %edx,%edx
  406bce:	48 f7 f1             	div    %rcx
  406bd1:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  406bd8:	00 
  406bd9:	eb 92                	jmp    406b6d <runtime::print_i64+0x5d>
  406bdb:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406be0:	48 83 e8 01          	sub    $0x1,%rax
  406be4:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406be9:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406bee:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  406bf3:	48 c7 c0 48 40 41 00 	mov    $0x414048,%rax
  406bfa:	48 8b 08             	mov    (%rax),%rcx
  406bfd:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  406c04:	00 
  406c05:	be 0a 00 00 00       	mov    $0xa,%esi
  406c0a:	31 d2                	xor    %edx,%edx
  406c0c:	48 f7 f6             	div    %rsi
  406c0f:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  406c14:	8a 0c 11             	mov    (%rcx,%rdx,1),%cl
  406c17:	88 4c 04 46          	mov    %cl,0x46(%rsp,%rax,1)
  406c1b:	80 bc 24 c7 00 00 00 	cmpb   $0x0,0xc7(%rsp)
  406c22:	00 
  406c23:	74 18                	je     406c3d <runtime::print_i64+0x12d>
  406c25:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406c2a:	48 83 e8 01          	sub    $0x1,%rax
  406c2e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  406c33:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  406c38:	c6 44 04 46 2d       	movb   $0x2d,0x46(%rsp,%rax,1)
  406c3d:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  406c42:	48 8d 4c 14 46       	lea    0x46(%rsp,%rdx,1),%rcx
  406c47:	b8 81 00 00 00       	mov    $0x81,%eax
  406c4c:	48 29 d0             	sub    %rdx,%rax
  406c4f:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  406c54:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  406c59:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406c5e:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  406c63:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  406c6a:	00 00 
  406c6c:	48 8d 54 24 20       	lea    0x20(%rsp),%rdx
  406c71:	e8 7a ac ff ff       	call   4018f0 <runtime::stderr_write>
  406c76:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  406c7d:	c3                   	ret
  406c7e:	66 90                	xchg   %ax,%ax

0000000000406c80 <runtime::print_caller_location>:
  406c80:	50                   	push   %rax
  406c81:	48 89 3c 24          	mov    %rdi,(%rsp)
  406c85:	eb 00                	jmp    406c87 <runtime::print_caller_location+0x7>
  406c87:	48 8b 04 24          	mov    (%rsp),%rax
  406c8b:	48 8b 38             	mov    (%rax),%rdi
  406c8e:	48 8b 70 08          	mov    0x8(%rax),%rsi
  406c92:	e8 29 fc ff ff       	call   4068c0 <runtime::print_string>
  406c97:	bf 28 00 00 00       	mov    $0x28,%edi
  406c9c:	e8 8f fc ff ff       	call   406930 <runtime::print_byte>
  406ca1:	48 8b 04 24          	mov    (%rsp),%rax
  406ca5:	48 63 78 10          	movslq 0x10(%rax),%rdi
  406ca9:	e8 12 fd ff ff       	call   4069c0 <runtime::print_u64>
  406cae:	48 8b 04 24          	mov    (%rsp),%rax
  406cb2:	83 78 14 00          	cmpl   $0x0,0x14(%rax)
  406cb6:	0f 95 c0             	setne  %al
  406cb9:	24 01                	and    $0x1,%al
  406cbb:	3c 00                	cmp    $0x0,%al
  406cbd:	74 17                	je     406cd6 <runtime::print_caller_location+0x56>
  406cbf:	bf 3a 00 00 00       	mov    $0x3a,%edi
  406cc4:	e8 67 fc ff ff       	call   406930 <runtime::print_byte>
  406cc9:	48 8b 04 24          	mov    (%rsp),%rax
  406ccd:	48 63 78 14          	movslq 0x14(%rax),%rdi
  406cd1:	e8 ea fc ff ff       	call   4069c0 <runtime::print_u64>
  406cd6:	bf 29 00 00 00       	mov    $0x29,%edi
  406cdb:	e8 50 fc ff ff       	call   406930 <runtime::print_byte>
  406ce0:	58                   	pop    %rax
  406ce1:	c3                   	ret
  406ce2:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  406ce9:	00 00 00 
  406cec:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000406cf0 <linux::read>:
  406cf0:	48 83 ec 48          	sub    $0x48,%rsp
  406cf4:	48 89 0c 24          	mov    %rcx,(%rsp)
  406cf8:	89 7c 24 0c          	mov    %edi,0xc(%rsp)
  406cfc:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  406d01:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  406d06:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406d0b:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406d10:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  406d14:	89 74 24 44          	mov    %esi,0x44(%rsp)
  406d18:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  406d1d:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406d22:	31 c0                	xor    %eax,%eax
  406d24:	89 c7                	mov    %eax,%edi
  406d26:	e8 85 01 00 00       	call   406eb0 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int)->(:int)>
  406d2b:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  406d30:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406d35:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  406d3c:	00 00 
  406d3e:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
  406d43:	e8 78 02 00 00       	call   406fc0 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  406d48:	48 8b 0c 24          	mov    (%rsp),%rcx
  406d4c:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  406d51:	48 89 11             	mov    %rdx,(%rcx)
  406d54:	48 83 c4 48          	add    $0x48,%rsp
  406d58:	c3                   	ret
  406d59:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000406d60 <linux::write>:
  406d60:	48 83 ec 48          	sub    $0x48,%rsp
  406d64:	48 89 0c 24          	mov    %rcx,(%rsp)
  406d68:	89 7c 24 0c          	mov    %edi,0xc(%rsp)
  406d6c:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  406d71:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  406d76:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  406d7b:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  406d80:	8b 74 24 0c          	mov    0xc(%rsp),%esi
  406d84:	89 74 24 44          	mov    %esi,0x44(%rsp)
  406d88:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  406d8d:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  406d92:	bf 01 00 00 00       	mov    $0x1,%edi
  406d97:	e8 14 01 00 00       	call   406eb0 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int)->(:int)>
  406d9c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  406da1:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  406da6:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  406dad:	00 00 
  406daf:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
  406db4:	e8 07 02 00 00       	call   406fc0 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  406db9:	48 8b 0c 24          	mov    (%rsp),%rcx
  406dbd:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  406dc2:	48 89 11             	mov    %rdx,(%rcx)
  406dc5:	48 83 c4 48          	add    $0x48,%rsp
  406dc9:	c3                   	ret
  406dca:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000406dd0 <linux::close>:
  406dd0:	48 83 ec 18          	sub    $0x18,%rsp
  406dd4:	89 7c 24 04          	mov    %edi,0x4(%rsp)
  406dd8:	8b 74 24 04          	mov    0x4(%rsp),%esi
  406ddc:	89 74 24 14          	mov    %esi,0x14(%rsp)
  406de0:	bf 03 00 00 00       	mov    $0x3,%edi
  406de5:	e8 16 00 00 00       	call   406e00 <linux::syscall1:proc"contextless"(nr:uintptr,p1:linux::Fd)->(:int)>
  406dea:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  406def:	31 c0                	xor    %eax,%eax
  406df1:	48 2b 44 24 08       	sub    0x8(%rsp),%rax
  406df6:	48 83 c4 18          	add    $0x18,%rsp
  406dfa:	c3                   	ret
  406dfb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000406e00 <linux::syscall1:proc"contextless"(nr:uintptr,p1:linux::Fd)->(:int)>:
  406e00:	48 89 7c 24 e8       	mov    %rdi,-0x18(%rsp)
  406e05:	89 74 24 f0          	mov    %esi,-0x10(%rsp)
  406e09:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  406e0e:	8b 4c 24 f0          	mov    -0x10(%rsp),%ecx
  406e12:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406e17:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  406e1b:	48 63 f9             	movslq %ecx,%rdi
  406e1e:	0f 05                	syscall
  406e20:	c3                   	ret
  406e21:	66 66 66 66 66 66 2e 	data16 data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  406e28:	0f 1f 84 00 00 00 00 
  406e2f:	00 

0000000000406e30 <linux::syscall2:proc"contextless"(nr:uintptr,p1:i32,p2:^linux::Stat)->(:int)>:
  406e30:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  406e35:	89 74 24 dc          	mov    %esi,-0x24(%rsp)
  406e39:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  406e3e:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  406e43:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  406e48:	8b 4c 24 dc          	mov    -0x24(%rsp),%ecx
  406e4c:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406e51:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  406e55:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  406e5a:	48 63 f9             	movslq %ecx,%rdi
  406e5d:	0f 05                	syscall
  406e5f:	c3                   	ret

0000000000406e60 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:i64,p3:linux::Seek_Whence)->(:int)>:
  406e60:	48 89 54 24 c8       	mov    %rdx,-0x38(%rsp)
  406e65:	89 74 24 d4          	mov    %esi,-0x2c(%rsp)
  406e69:	48 89 7c 24 d8       	mov    %rdi,-0x28(%rsp)
  406e6e:	66 89 c8             	mov    %cx,%ax
  406e71:	66 89 44 24 e4       	mov    %ax,-0x1c(%rsp)
  406e76:	48 8b 74 24 c8       	mov    -0x38(%rsp),%rsi
  406e7b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  406e80:	66 8b 4c 24 e4       	mov    -0x1c(%rsp),%cx
  406e85:	8b 54 24 d4          	mov    -0x2c(%rsp),%edx
  406e89:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406e8e:	89 54 24 f4          	mov    %edx,-0xc(%rsp)
  406e92:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  406e97:	66 89 4c 24 e6       	mov    %cx,-0x1a(%rsp)
  406e9c:	48 63 fa             	movslq %edx,%rdi
  406e9f:	48 0f bf d1          	movswq %cx,%rdx
  406ea3:	0f 05                	syscall
  406ea5:	c3                   	ret
  406ea6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  406ead:	00 00 00 

0000000000406eb0 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int)->(:int)>:
  406eb0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  406eb5:	89 74 24 cc          	mov    %esi,-0x34(%rsp)
  406eb9:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  406ebe:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406ec3:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  406ec8:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  406ecd:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  406ed2:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  406ed6:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406edb:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  406edf:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  406ee4:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  406ee9:	48 63 f9             	movslq %ecx,%rdi
  406eec:	0f 05                	syscall
  406eee:	c3                   	ret
  406eef:	90                   	nop

0000000000406ef0 <linux::syscall3:proc"contextless"(nr:uintptr,p1:rawptr,p2:[^]u8,p3:int)->(:int)>:
  406ef0:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  406ef5:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  406efa:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  406eff:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  406f04:	48 8b 54 24 d8       	mov    -0x28(%rsp),%rdx
  406f09:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  406f0e:	48 8b 7c 24 c8       	mov    -0x38(%rsp),%rdi
  406f13:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  406f18:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406f1d:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  406f22:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  406f27:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  406f2c:	0f 05                	syscall
  406f2e:	c3                   	ret
  406f2f:	90                   	nop

0000000000406f30 <linux::syscall4:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int,p4:uint)->(:int)>:
  406f30:	48 89 7c 24 b0       	mov    %rdi,-0x50(%rsp)
  406f35:	89 74 24 bc          	mov    %esi,-0x44(%rsp)
  406f39:	48 89 54 24 c0       	mov    %rdx,-0x40(%rsp)
  406f3e:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  406f43:	4c 89 44 24 d0       	mov    %r8,-0x30(%rsp)
  406f48:	4c 8b 54 24 d0       	mov    -0x30(%rsp),%r10
  406f4d:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  406f52:	48 8b 74 24 c0       	mov    -0x40(%rsp),%rsi
  406f57:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  406f5c:	8b 4c 24 bc          	mov    -0x44(%rsp),%ecx
  406f60:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406f65:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  406f69:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  406f6e:	48 89 54 24 e0       	mov    %rdx,-0x20(%rsp)
  406f73:	4c 89 54 24 d8       	mov    %r10,-0x28(%rsp)
  406f78:	48 63 f9             	movslq %ecx,%rdi
  406f7b:	0f 05                	syscall
  406f7d:	c3                   	ret
  406f7e:	66 90                	xchg   %ax,%ax

0000000000406f80 <linux::fstat>:
  406f80:	48 83 ec 28          	sub    $0x28,%rsp
  406f84:	89 7c 24 04          	mov    %edi,0x4(%rsp)
  406f88:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  406f8d:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  406f92:	8b 74 24 04          	mov    0x4(%rsp),%esi
  406f96:	89 74 24 24          	mov    %esi,0x24(%rsp)
  406f9a:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  406f9f:	bf 05 00 00 00       	mov    $0x5,%edi
  406fa4:	e8 87 fe ff ff       	call   406e30 <linux::syscall2:proc"contextless"(nr:uintptr,p1:i32,p2:^linux::Stat)->(:int)>
  406fa9:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  406fae:	31 c0                	xor    %eax,%eax
  406fb0:	48 2b 44 24 10       	sub    0x10(%rsp),%rax
  406fb5:	48 83 c4 28          	add    $0x28,%rsp
  406fb9:	c3                   	ret
  406fba:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000406fc0 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>:
  406fc0:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  406fc5:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  406fca:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  406fcf:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  406fd4:	48 83 f8 00          	cmp    $0x0,%rax
  406fd8:	0f 9c c0             	setl   %al
  406fdb:	24 01                	and    $0x1,%al
  406fdd:	3c 00                	cmp    $0x0,%al
  406fdf:	74 21                	je     407002 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)+0x42>
  406fe1:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  406fe6:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  406feb:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  406ff2:	00 00 
  406ff4:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  406ff9:	31 c0                	xor    %eax,%eax
  406ffb:	48 29 f0             	sub    %rsi,%rax
  406ffe:	48 89 11             	mov    %rdx,(%rcx)
  407001:	c3                   	ret
  407002:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  407007:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40700c:	48 89 08             	mov    %rcx,(%rax)
  40700f:	31 c0                	xor    %eax,%eax
  407011:	c3                   	ret
  407012:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  407019:	1f 84 00 00 00 00 00 

0000000000407020 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$i64)->(:i64,:linux::Errno)>:
  407020:	48 89 7c 24 e0       	mov    %rdi,-0x20(%rsp)
  407025:	48 89 74 24 e8       	mov    %rsi,-0x18(%rsp)
  40702a:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40702f:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  407034:	48 83 f8 00          	cmp    $0x0,%rax
  407038:	0f 9c c0             	setl   %al
  40703b:	24 01                	and    $0x1,%al
  40703d:	3c 00                	cmp    $0x0,%al
  40703f:	74 21                	je     407062 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$i64)->(:i64,:linux::Errno)+0x42>
  407041:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  407046:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40704b:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  407052:	00 00 
  407054:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  407059:	31 c0                	xor    %eax,%eax
  40705b:	48 29 f0             	sub    %rsi,%rax
  40705e:	48 89 11             	mov    %rdx,(%rcx)
  407061:	c3                   	ret
  407062:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  407067:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40706c:	48 89 08             	mov    %rcx,(%rax)
  40706f:	31 c0                	xor    %eax,%eax
  407071:	c3                   	ret
  407072:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  407079:	1f 84 00 00 00 00 00 

0000000000407080 <linux::[helpers.odin]::compat64_arg_pair>:
  407080:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  407085:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40708a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40708f:	c3                   	ret

0000000000407090 <linux::lseek>:
  407090:	48 83 ec 48          	sub    $0x48,%rsp
  407094:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  407099:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40709e:	89 7c 24 18          	mov    %edi,0x18(%rsp)
  4070a2:	66 89 d0             	mov    %dx,%ax
  4070a5:	66 89 44 24 1e       	mov    %ax,0x1e(%rsp)
  4070aa:	66 8b 44 24 1e       	mov    0x1e(%rsp),%ax
  4070af:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  4070b4:	8b 74 24 18          	mov    0x18(%rsp),%esi
  4070b8:	89 74 24 44          	mov    %esi,0x44(%rsp)
  4070bc:	48 89 54 24 38       	mov    %rdx,0x38(%rsp)
  4070c1:	66 89 44 24 36       	mov    %ax,0x36(%rsp)
  4070c6:	bf 08 00 00 00       	mov    $0x8,%edi
  4070cb:	0f b7 c8             	movzwl %ax,%ecx
  4070ce:	e8 8d fd ff ff       	call   406e60 <linux::syscall3:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:i64,p3:linux::Seek_Whence)->(:int)>
  4070d3:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4070d8:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4070dd:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  4070e4:	00 00 
  4070e6:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
  4070eb:	e8 30 ff ff ff       	call   407020 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$i64)->(:i64,:linux::Errno)>
  4070f0:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  4070f5:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4070fa:	48 89 11             	mov    %rdx,(%rcx)
  4070fd:	48 83 c4 48          	add    $0x48,%rsp
  407101:	c3                   	ret
  407102:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  407109:	1f 84 00 00 00 00 00 

0000000000407110 <linux::pread>:
  407110:	48 83 ec 58          	sub    $0x58,%rsp
  407114:	4c 89 04 24          	mov    %r8,(%rsp)
  407118:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40711d:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  407121:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  407126:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  40712b:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  407130:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407135:	8b 54 24 14          	mov    0x14(%rsp),%edx
  407139:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40713e:	89 54 24 54          	mov    %edx,0x54(%rsp)
  407142:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  407147:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40714c:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  407151:	e8 2a ff ff ff       	call   407080 <linux::[helpers.odin]::compat64_arg_pair>
  407156:	8b 74 24 14          	mov    0x14(%rsp),%esi
  40715a:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40715f:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  407164:	49 89 c0             	mov    %rax,%r8
  407167:	bf 11 00 00 00       	mov    $0x11,%edi
  40716c:	e8 bf fd ff ff       	call   406f30 <linux::syscall4:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int,p4:uint)->(:int)>
  407171:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  407176:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40717b:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  407182:	00 00 
  407184:	48 8d 74 24 28       	lea    0x28(%rsp),%rsi
  407189:	e8 32 fe ff ff       	call   406fc0 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  40718e:	48 8b 0c 24          	mov    (%rsp),%rcx
  407192:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  407197:	48 89 11             	mov    %rdx,(%rcx)
  40719a:	48 83 c4 58          	add    $0x58,%rsp
  40719e:	c3                   	ret
  40719f:	90                   	nop

00000000004071a0 <linux::pwrite>:
  4071a0:	48 83 ec 58          	sub    $0x58,%rsp
  4071a4:	4c 89 04 24          	mov    %r8,(%rsp)
  4071a8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  4071ad:	89 7c 24 14          	mov    %edi,0x14(%rsp)
  4071b1:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  4071b6:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4071bb:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4071c0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  4071c5:	8b 54 24 14          	mov    0x14(%rsp),%edx
  4071c9:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  4071ce:	89 54 24 54          	mov    %edx,0x54(%rsp)
  4071d2:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4071d7:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4071dc:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  4071e1:	e8 9a fe ff ff       	call   407080 <linux::[helpers.odin]::compat64_arg_pair>
  4071e6:	8b 74 24 14          	mov    0x14(%rsp),%esi
  4071ea:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4071ef:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4071f4:	49 89 c0             	mov    %rax,%r8
  4071f7:	bf 12 00 00 00       	mov    $0x12,%edi
  4071fc:	e8 2f fd ff ff       	call   406f30 <linux::syscall4:proc"contextless"(nr:uintptr,p1:linux::Fd,p2:[^]u8,p3:int,p4:uint)->(:int)>
  407201:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  407206:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
  40720b:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
  407212:	00 00 
  407214:	48 8d 74 24 28       	lea    0x28(%rsp),%rsi
  407219:	e8 a2 fd ff ff       	call   406fc0 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  40721e:	48 8b 0c 24          	mov    (%rsp),%rcx
  407222:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  407227:	48 89 11             	mov    %rdx,(%rcx)
  40722a:	48 83 c4 58          	add    $0x58,%rsp
  40722e:	c3                   	ret
  40722f:	90                   	nop

0000000000407230 <linux::fsync>:
  407230:	48 83 ec 18          	sub    $0x18,%rsp
  407234:	89 7c 24 04          	mov    %edi,0x4(%rsp)
  407238:	8b 74 24 04          	mov    0x4(%rsp),%esi
  40723c:	89 74 24 14          	mov    %esi,0x14(%rsp)
  407240:	bf 4a 00 00 00       	mov    $0x4a,%edi
  407245:	e8 b6 fb ff ff       	call   406e00 <linux::syscall1:proc"contextless"(nr:uintptr,p1:linux::Fd)->(:int)>
  40724a:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  40724f:	31 c0                	xor    %eax,%eax
  407251:	48 2b 44 24 08       	sub    0x8(%rsp),%rax
  407256:	48 83 c4 18          	add    $0x18,%rsp
  40725a:	c3                   	ret
  40725b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000407260 <linux::readlink>:
  407260:	48 83 ec 48          	sub    $0x48,%rsp
  407264:	48 89 0c 24          	mov    %rcx,(%rsp)
  407268:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40726d:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  407272:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  407277:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40727c:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  407281:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  407286:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40728b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  407290:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  407295:	bf 59 00 00 00       	mov    $0x59,%edi
  40729a:	e8 51 fc ff ff       	call   406ef0 <linux::syscall3:proc"contextless"(nr:uintptr,p1:rawptr,p2:[^]u8,p3:int)->(:int)>
  40729f:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4072a4:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  4072a9:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
  4072b0:	00 00 
  4072b2:	48 8d 74 24 20       	lea    0x20(%rsp),%rsi
  4072b7:	e8 04 fd ff ff       	call   406fc0 <linux::errno_unwrap2:proc"contextless"(ret:int,T:$int)->(:int,:linux::Errno)>
  4072bc:	48 8b 0c 24          	mov    (%rsp),%rcx
  4072c0:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  4072c5:	48 89 11             	mov    %rdx,(%rcx)
  4072c8:	48 83 c4 48          	add    $0x48,%rsp
  4072cc:	c3                   	ret
  4072cd:	0f 1f 00             	nopl   (%rax)

00000000004072d0 <runtime::[default_temp_allocator_arena.odin]::safe_add>:
  4072d0:	48 89 7c 24 c8       	mov    %rdi,-0x38(%rsp)
  4072d5:	48 89 74 24 d0       	mov    %rsi,-0x30(%rsp)
  4072da:	48 89 54 24 d8       	mov    %rdx,-0x28(%rsp)
  4072df:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4072e4:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4072e9:	48 8b 54 24 c8       	mov    -0x38(%rsp),%rdx
  4072ee:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  4072f3:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  4072f8:	48 01 c2             	add    %rax,%rdx
  4072fb:	0f 92 c0             	setb   %al
  4072fe:	48 89 54 24 e8       	mov    %rdx,-0x18(%rsp)
  407303:	24 01                	and    $0x1,%al
  407305:	88 44 24 e7          	mov    %al,-0x19(%rsp)
  407309:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40730e:	80 7c 24 e7 00       	cmpb   $0x0,-0x19(%rsp)
  407313:	0f 94 c0             	sete   %al
  407316:	24 01                	and    $0x1,%al
  407318:	48 89 11             	mov    %rdx,(%rcx)
  40731b:	c3                   	ret
  40731c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000407320 <runtime::memory_block_alloc>:
  407320:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  407327:	4c 89 4c 24 38       	mov    %r9,0x38(%rsp)
  40732c:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  407331:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  407336:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40733b:	48 89 74 24 58       	mov    %rsi,0x58(%rsp)
  407340:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  407345:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  40734c:	00 
  40734d:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  407352:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  407357:	4c 8b 4c 24 68       	mov    0x68(%rsp),%r9
  40735c:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  407361:	48 8b 7c 24 50       	mov    0x50(%rsp),%rdi
  407366:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40736b:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  407370:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  407377:	00 
  407378:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  40737f:	00 
  407380:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  407387:	00 
  407388:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40738d:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  407394:	00 
  407395:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40739a:	48 89 bc 24 e8 00 00 	mov    %rdi,0xe8(%rsp)
  4073a1:	00 
  4073a2:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  4073a9:	00 
  4073aa:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  4073b1:	00 00 00 00 00 
  4073b6:	c6 84 24 d7 00 00 00 	movb   $0x0,0xd7(%rsp)
  4073bd:	00 
  4073be:	48 89 d6             	mov    %rdx,%rsi
  4073c1:	48 83 ee 31          	sub    $0x31,%rsi
  4073c5:	be 30 00 00 00       	mov    $0x30,%esi
  4073ca:	48 0f 43 f2          	cmovae %rdx,%rsi
  4073ce:	48 01 f7             	add    %rsi,%rdi
  4073d1:	48 89 bc 24 c8 00 00 	mov    %rdi,0xc8(%rsp)
  4073d8:	00 
  4073d9:	48 89 b4 24 c0 00 00 	mov    %rsi,0xc0(%rsp)
  4073e0:	00 
  4073e1:	48 89 d6             	mov    %rdx,%rsi
  4073e4:	48 83 ee 10          	sub    $0x10,%rsi
  4073e8:	be 10 00 00 00       	mov    $0x10,%esi
  4073ed:	48 0f 4c d6          	cmovl  %rsi,%rdx
  4073f1:	48 89 94 24 b8 00 00 	mov    %rdx,0xb8(%rsp)
  4073f8:	00 
  4073f9:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  407400:	00 
  407401:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  407408:	00 
  407409:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  407410:	00 
  407411:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  407418:	00 
  407419:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  407420:	00 
  407421:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  407428:	00 
  407429:	0f 57 c0             	xorps  %xmm0,%xmm0
  40742c:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  407433:	00 
  407434:	48 89 e0             	mov    %rsp,%rax
  407437:	4c 89 08             	mov    %r9,(%rax)
  40743a:	4c 8d 8c 24 90 00 00 	lea    0x90(%rsp),%r9
  407441:	00 
  407442:	e8 09 53 00 00       	call   40c750 <runtime::mem_alloc>
  407447:	88 44 24 27          	mov    %al,0x27(%rsp)
  40744b:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  407452:	00 
  407453:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  407458:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40745f:	00 
  407460:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  407465:	3c 00                	cmp    $0x0,%al
  407467:	74 39                	je     4074a2 <runtime::memory_block_alloc+0x182>
  407469:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40746e:	8a 44 24 27          	mov    0x27(%rsp),%al
  407472:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  407479:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  407480:	00 
  407481:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  407488:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  40748f:	00 
  407490:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  407497:	48 89 11             	mov    %rdx,(%rcx)
  40749a:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  4074a1:	c3                   	ret
  4074a2:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  4074a7:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  4074ac:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  4074b1:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  4074b6:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  4074bb:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  4074c2:	00 
  4074c3:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  4074ca:	00 
  4074cb:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4074d2:	00 
  4074d3:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  4074da:	00 
  4074db:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4074e2:	00 
  4074e3:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  4074ea:	00 
  4074eb:	48 01 f0             	add    %rsi,%rax
  4074ee:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  4074f3:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  4074f8:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  4074fd:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  407504:	00 
  407505:	48 89 50 10          	mov    %rdx,0x10(%rax)
  407509:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40750d:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  407514:	00 
  407515:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  40751c:	00 
  40751d:	48 03 8c 24 c0 00 00 	add    0xc0(%rsp),%rcx
  407524:	00 
  407525:	48 89 48 18          	mov    %rcx,0x18(%rax)
  407529:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  407530:	00 
  407531:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  407536:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  40753d:	00 
  40753e:	48 8b 52 18          	mov    0x18(%rdx),%rdx
  407542:	48 29 d1             	sub    %rdx,%rcx
  407545:	48 89 48 28          	mov    %rcx,0x28(%rax)
  407549:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  407550:	00 
  407551:	48 83 78 20 00       	cmpq   $0x0,0x20(%rax)
  407556:	0f 94 c0             	sete   %al
  407559:	24 01                	and    $0x1,%al
  40755b:	0f b6 f8             	movzbl %al,%edi
  40755e:	be 50 f8 40 00       	mov    $0x40f850,%esi
  407563:	b9 b0 f8 40 00       	mov    $0x40f8b0,%ecx
  407568:	ba 0f 00 00 00       	mov    $0xf,%edx
  40756d:	e8 3e a1 ff ff       	call   4016b0 <runtime::assert>
  407572:	4c 8b 44 24 68       	mov    0x68(%rsp),%r8
  407577:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  40757e:	00 
  40757f:	48 83 38 00          	cmpq   $0x0,(%rax)
  407583:	0f 94 c0             	sete   %al
  407586:	24 01                	and    $0x1,%al
  407588:	0f b6 f8             	movzbl %al,%edi
  40758b:	be d8 f8 40 00       	mov    $0x40f8d8,%esi
  407590:	b9 f0 f8 40 00       	mov    $0x40f8f0,%ecx
  407595:	ba 11 00 00 00       	mov    $0x11,%edx
  40759a:	e8 11 a1 ff ff       	call   4016b0 <runtime::assert>
  40759f:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  4075a4:	48 8b 94 24 d8 00 00 	mov    0xd8(%rsp),%rdx
  4075ab:	00 
  4075ac:	8a 84 24 d7 00 00 00 	mov    0xd7(%rsp),%al
  4075b3:	48 89 94 24 d8 00 00 	mov    %rdx,0xd8(%rsp)
  4075ba:	00 
  4075bb:	88 84 24 d7 00 00 00 	mov    %al,0xd7(%rsp)
  4075c2:	48 89 11             	mov    %rdx,(%rcx)
  4075c5:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  4075cc:	c3                   	ret
  4075cd:	0f 1f 00             	nopl   (%rax)

00000000004075d0 <runtime::memory_block_dealloc>:
  4075d0:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  4075d7:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  4075dc:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  4075e1:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4075e6:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4075ed:	00 
  4075ee:	48 83 f8 00          	cmp    $0x0,%rax
  4075f2:	0f 95 c0             	setne  %al
  4075f5:	24 01                	and    $0x1,%al
  4075f7:	3c 00                	cmp    $0x0,%al
  4075f9:	0f 84 5f 01 00 00    	je     40775e <runtime::memory_block_dealloc+0x18e>
  4075ff:	48 8b 8c 24 20 01 00 	mov    0x120(%rsp),%rcx
  407606:	00 
  407607:	48 8b 41 08          	mov    0x8(%rcx),%rax
  40760b:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  40760f:	48 89 8c 24 18 01 00 	mov    %rcx,0x118(%rsp)
  407616:	00 
  407617:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40761e:	00 
  40761f:	0f 57 c0             	xorps  %xmm0,%xmm0
  407622:	0f 29 04 24          	movaps %xmm0,(%rsp)
  407626:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40762d:	00 
  40762e:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  407635:	00 
  407636:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  40763d:	00 
  40763e:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  407645:	00 
  407646:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  40764d:	00 
  40764e:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  407655:	00 
  407656:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40765d:	00 
  40765e:	48 8d bc 24 a0 00 00 	lea    0xa0(%rsp),%rdi
  407665:	00 
  407666:	e8 15 d0 ff ff       	call   404680 <runtime::[core.odin]::__init_context>
  40766b:	0f 28 04 24          	movaps (%rsp),%xmm0
  40766f:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  407676:	00 
  407677:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40767e:	00 
  40767f:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  407684:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  407689:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40768e:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  407693:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  407698:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
  40769d:	e8 8e cf ff ff       	call   404630 <runtime::default_context>
  4076a2:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  4076a7:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  4076ac:	0f 28 44 24 30       	movaps 0x30(%rsp),%xmm0
  4076b1:	0f 28 4c 24 40       	movaps 0x40(%rsp),%xmm1
  4076b6:	0f 28 54 24 50       	movaps 0x50(%rsp),%xmm2
  4076bb:	0f 28 5c 24 60       	movaps 0x60(%rsp),%xmm3
  4076c0:	0f 28 64 24 70       	movaps 0x70(%rsp),%xmm4
  4076c5:	0f 28 ac 24 80 00 00 	movaps 0x80(%rsp),%xmm5
  4076cc:	00 
  4076cd:	0f 28 b4 24 90 00 00 	movaps 0x90(%rsp),%xmm6
  4076d4:	00 
  4076d5:	0f 29 b4 24 00 01 00 	movaps %xmm6,0x100(%rsp)
  4076dc:	00 
  4076dd:	0f 29 ac 24 f0 00 00 	movaps %xmm5,0xf0(%rsp)
  4076e4:	00 
  4076e5:	0f 29 a4 24 e0 00 00 	movaps %xmm4,0xe0(%rsp)
  4076ec:	00 
  4076ed:	0f 29 9c 24 d0 00 00 	movaps %xmm3,0xd0(%rsp)
  4076f4:	00 
  4076f5:	0f 29 94 24 c0 00 00 	movaps %xmm2,0xc0(%rsp)
  4076fc:	00 
  4076fd:	0f 29 8c 24 b0 00 00 	movaps %xmm1,0xb0(%rsp)
  407704:	00 
  407705:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  40770c:	00 
  40770d:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  407714:	00 
  407715:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  40771c:	00 
  40771d:	48 89 94 24 a8 00 00 	mov    %rdx,0xa8(%rsp)
  407724:	00 
  407725:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40772c:	00 
  40772d:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  407734:	00 
  407735:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  40773c:	00 
  40773d:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  407742:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407747:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40774c:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  407751:	4c 8d 84 24 a0 00 00 	lea    0xa0(%rsp),%r8
  407758:	00 
  407759:	e8 12 51 00 00       	call   40c870 <runtime::mem_free>
  40775e:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  407765:	c3                   	ret
  407766:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40776d:	00 00 00 

0000000000407770 <runtime::alloc_from_memory_block>:
  407770:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  407777:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40777c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407781:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  407786:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40778b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  407790:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  407795:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40779a:	48 89 94 24 a0 00 00 	mov    %rdx,0xa0(%rsp)
  4077a1:	00 
  4077a2:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  4077a9:	00 
  4077aa:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  4077b1:	00 
  4077b2:	48 8d bc 24 80 00 00 	lea    0x80(%rsp),%rdi
  4077b9:	00 
  4077ba:	31 f6                	xor    %esi,%esi
  4077bc:	ba 10 00 00 00       	mov    $0x10,%edx
  4077c1:	e8 7a 98 ff ff       	call   401040 <memset@plt>
  4077c6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  4077cb:	c6 44 24 7f 00       	movb   $0x0,0x7f(%rsp)
  4077d0:	48 83 f8 00          	cmp    $0x0,%rax
  4077d4:	0f 94 c0             	sete   %al
  4077d7:	24 01                	and    $0x1,%al
  4077d9:	3c 00                	cmp    $0x0,%al
  4077db:	74 34                	je     407811 <runtime::alloc_from_memory_block+0xa1>
  4077dd:	48 8d bc 24 80 00 00 	lea    0x80(%rsp),%rdi
  4077e4:	00 
  4077e5:	31 f6                	xor    %esi,%esi
  4077e7:	ba 10 00 00 00       	mov    $0x10,%edx
  4077ec:	e8 4f 98 ff ff       	call   401040 <memset@plt>
  4077f1:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  4077f6:	c6 44 24 7f 01       	movb   $0x1,0x7f(%rsp)
  4077fb:	31 f6                	xor    %esi,%esi
  4077fd:	ba 10 00 00 00       	mov    $0x10,%edx
  407802:	e8 39 98 ff ff       	call   401040 <memset@plt>
  407807:	b0 01                	mov    $0x1,%al
  407809:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  407810:	c3                   	ret
  407811:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  407816:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40781b:	e8 00 11 00 00       	call   408920 <runtime::alloc_from_memory_block.calc_alignment_offset-0>
  407820:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  407825:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40782a:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  40782f:	48 c7 44 24 68 00 00 	movq   $0x0,0x68(%rsp)
  407836:	00 00 
  407838:	48 8d 54 24 68       	lea    0x68(%rsp),%rdx
  40783d:	e8 8e fa ff ff       	call   4072d0 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  407842:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  407847:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  40784c:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  407850:	80 7c 24 5f 00       	cmpb   $0x0,0x5f(%rsp)
  407855:	75 41                	jne    407898 <runtime::alloc_from_memory_block+0x128>
  407857:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40785c:	c6 44 24 7f 01       	movb   $0x1,0x7f(%rsp)
  407861:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  407868:	00 
  407869:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  407870:	00 
  407871:	8a 44 24 7f          	mov    0x7f(%rsp),%al
  407875:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  40787c:	00 
  40787d:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  407884:	00 
  407885:	88 44 24 7f          	mov    %al,0x7f(%rsp)
  407889:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40788d:	48 89 11             	mov    %rdx,(%rcx)
  407890:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  407897:	c3                   	ret
  407898:	eb 00                	jmp    40789a <runtime::alloc_from_memory_block+0x12a>
  40789a:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  4078a1:	00 
  4078a2:	48 8b 78 20          	mov    0x20(%rax),%rdi
  4078a6:	48 8b 74 24 60       	mov    0x60(%rsp),%rsi
  4078ab:	48 c7 44 24 50 00 00 	movq   $0x0,0x50(%rsp)
  4078b2:	00 00 
  4078b4:	48 8d 54 24 50       	lea    0x50(%rsp),%rdx
  4078b9:	e8 12 fa ff ff       	call   4072d0 <runtime::[default_temp_allocator_arena.odin]::safe_add>
  4078be:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  4078c3:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  4078c8:	88 44 24 47          	mov    %al,0x47(%rsp)
  4078cc:	80 7c 24 47 00       	cmpb   $0x0,0x47(%rsp)
  4078d1:	74 1a                	je     4078ed <runtime::alloc_from_memory_block+0x17d>
  4078d3:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  4078d8:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4078df:	00 
  4078e0:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  4078e4:	0f 97 c0             	seta   %al
  4078e7:	24 01                	and    $0x1,%al
  4078e9:	3c 00                	cmp    $0x0,%al
  4078eb:	74 41                	je     40792e <runtime::alloc_from_memory_block+0x1be>
  4078ed:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  4078f2:	c6 44 24 7f 01       	movb   $0x1,0x7f(%rsp)
  4078f7:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  4078fe:	00 
  4078ff:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  407906:	00 
  407907:	8a 44 24 7f          	mov    0x7f(%rsp),%al
  40790b:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  407912:	00 
  407913:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40791a:	00 
  40791b:	88 44 24 7f          	mov    %al,0x7f(%rsp)
  40791f:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  407923:	48 89 11             	mov    %rdx,(%rcx)
  407926:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  40792d:	c3                   	ret
  40792e:	4c 8b 4c 24 10       	mov    0x10(%rsp),%r9
  407933:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  40793a:	00 
  40793b:	48 8b 41 18          	mov    0x18(%rcx),%rax
  40793f:	48 8b 49 20          	mov    0x20(%rcx),%rcx
  407943:	48 8b 54 24 70       	mov    0x70(%rsp),%rdx
  407948:	48 01 d1             	add    %rdx,%rcx
  40794b:	48 01 c8             	add    %rcx,%rax
  40794e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  407953:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  407958:	48 89 04 24          	mov    %rax,(%rsp)
  40795c:	bf 60 f8 40 00       	mov    $0x40f860,%edi
  407961:	31 c0                	xor    %eax,%eax
  407963:	41 89 c0             	mov    %eax,%r8d
  407966:	be 3c 00 00 00       	mov    $0x3c,%esi
  40796b:	ba 5c 00 00 00       	mov    $0x5c,%edx
  407970:	b9 31 00 00 00       	mov    $0x31,%ecx
  407975:	e8 a6 a2 ff ff       	call   401c20 <runtime::multi_pointer_slice_expr_error>
  40797a:	48 8b 14 24          	mov    (%rsp),%rdx
  40797e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  407983:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  407988:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40798d:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  407992:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  407997:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40799c:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  4079a3:	00 
  4079a4:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  4079ab:	00 
  4079ac:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  4079b3:	00 
  4079b4:	48 8b 74 24 60       	mov    0x60(%rsp),%rsi
  4079b9:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4079bd:	48 01 f2             	add    %rsi,%rdx
  4079c0:	48 89 50 20          	mov    %rdx,0x20(%rax)
  4079c4:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  4079cb:	00 
  4079cc:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  4079d3:	00 
  4079d4:	8a 44 24 7f          	mov    0x7f(%rsp),%al
  4079d8:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  4079df:	00 
  4079e0:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  4079e7:	00 
  4079e8:	88 44 24 7f          	mov    %al,0x7f(%rsp)
  4079ec:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4079f0:	48 89 11             	mov    %rdx,(%rcx)
  4079f3:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  4079fa:	c3                   	ret
  4079fb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000407a00 <runtime::arena_alloc>:
  407a00:	48 81 ec 08 01 00 00 	sub    $0x108,%rsp
  407a07:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  407a0c:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  407a11:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  407a16:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  407a1b:	4c 89 44 24 50       	mov    %r8,0x50(%rsp)
  407a20:	4c 89 4c 24 58       	mov    %r9,0x58(%rsp)
  407a25:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  407a2a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  407a2f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  407a34:	48 89 94 24 00 01 00 	mov    %rdx,0x100(%rsp)
  407a3b:	00 
  407a3c:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  407a43:	00 
  407a44:	48 89 84 24 f0 00 00 	mov    %rax,0xf0(%rsp)
  407a4b:	00 
  407a4c:	48 8d bc 24 e0 00 00 	lea    0xe0(%rsp),%rdi
  407a53:	00 
  407a54:	31 f6                	xor    %esi,%esi
  407a56:	ba 10 00 00 00       	mov    $0x10,%edx
  407a5b:	e8 e0 95 ff ff       	call   401040 <memset@plt>
  407a60:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  407a65:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  407a6a:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  407a6f:	c6 84 24 df 00 00 00 	movb   $0x0,0xdf(%rsp)
  407a76:	00 
  407a77:	48 89 c2             	mov    %rax,%rdx
  407a7a:	48 83 ea 01          	sub    $0x1,%rdx
  407a7e:	48 21 d0             	and    %rdx,%rax
  407a81:	48 83 f8 00          	cmp    $0x0,%rax
  407a85:	0f 94 c0             	sete   %al
  407a88:	24 01                	and    $0x1,%al
  407a8a:	0f b6 f8             	movzbl %al,%edi
  407a8d:	be 18 f9 40 00       	mov    $0x40f918,%esi
  407a92:	ba 1a 00 00 00       	mov    $0x1a,%edx
  407a97:	e8 14 9c ff ff       	call   4016b0 <runtime::assert>
  407a9c:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  407aa1:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  407aa8:	00 
  407aa9:	48 83 bc 24 d0 00 00 	cmpq   $0x0,0xd0(%rsp)
  407ab0:	00 00 
  407ab2:	0f 94 c0             	sete   %al
  407ab5:	24 01                	and    $0x1,%al
  407ab7:	3c 00                	cmp    $0x0,%al
  407ab9:	74 42                	je     407afd <runtime::arena_alloc+0xfd>
  407abb:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  407ac0:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  407ac7:	00 
  407ac8:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  407acf:	00 
  407ad0:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  407ad7:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  407ade:	00 
  407adf:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  407ae6:	00 
  407ae7:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  407aee:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  407af2:	48 89 11             	mov    %rdx,(%rcx)
  407af5:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  407afc:	c3                   	ret
  407afd:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407b04:	00 
  407b05:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  407b0a:	0f 94 c0             	sete   %al
  407b0d:	24 01                	and    $0x1,%al
  407b0f:	3c 00                	cmp    $0x0,%al
  407b11:	74 09                	je     407b1c <runtime::arena_alloc+0x11c>
  407b13:	31 c0                	xor    %eax,%eax
  407b15:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  407b1a:	eb 15                	jmp    407b31 <runtime::arena_alloc+0x131>
  407b1c:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407b23:	00 
  407b24:	48 8b 40 10          	mov    0x10(%rax),%rax
  407b28:	48 8b 40 20          	mov    0x20(%rax),%rax
  407b2c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  407b31:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  407b36:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  407b3b:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  407b40:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  407b47:	00 
  407b48:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407b4f:	00 
  407b50:	48 8b 78 10          	mov    0x10(%rax),%rdi
  407b54:	48 8b b4 24 d0 00 00 	mov    0xd0(%rsp),%rsi
  407b5b:	00 
  407b5c:	0f 57 c0             	xorps  %xmm0,%xmm0
  407b5f:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  407b66:	00 
  407b67:	48 8d 8c 24 b0 00 00 	lea    0xb0(%rsp),%rcx
  407b6e:	00 
  407b6f:	e8 fc fb ff ff       	call   407770 <runtime::alloc_from_memory_block>
  407b74:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  407b7b:	00 
  407b7c:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  407b83:	00 
  407b84:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  407b8b:	00 
  407b8c:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  407b93:	00 
  407b94:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  407b9b:	80 bc 24 df 00 00 00 	cmpb   $0x1,0xdf(%rsp)
  407ba2:	01 
  407ba3:	0f 94 c0             	sete   %al
  407ba6:	24 01                	and    $0x1,%al
  407ba8:	3c 00                	cmp    $0x0,%al
  407baa:	0f 84 24 02 00 00    	je     407dd4 <runtime::arena_alloc+0x3d4>
  407bb0:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407bb7:	00 
  407bb8:	48 83 78 28 00       	cmpq   $0x0,0x28(%rax)
  407bbd:	0f 94 c0             	sete   %al
  407bc0:	24 01                	and    $0x1,%al
  407bc2:	3c 00                	cmp    $0x0,%al
  407bc4:	74 10                	je     407bd6 <runtime::arena_alloc+0x1d6>
  407bc6:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407bcd:	00 
  407bce:	48 c7 40 28 00 00 40 	movq   $0x400000,0x28(%rax)
  407bd5:	00 
  407bd6:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  407bdb:	48 8b bc 24 d0 00 00 	mov    0xd0(%rsp),%rdi
  407be2:	00 
  407be3:	e8 c8 0d 00 00       	call   4089b0 <runtime::arena_alloc.align_forward_uint-0>
  407be8:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  407bef:	00 
  407bf0:	48 8b 8c 24 a8 00 00 	mov    0xa8(%rsp),%rcx
  407bf7:	00 
  407bf8:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407bff:	00 
  407c00:	48 8b 40 28          	mov    0x28(%rax),%rax
  407c04:	48 39 c1             	cmp    %rax,%rcx
  407c07:	48 0f 47 c1          	cmova  %rcx,%rax
  407c0b:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  407c12:	00 
  407c13:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407c1a:	00 
  407c1b:	48 83 38 00          	cmpq   $0x0,(%rax)
  407c1f:	0f 94 c0             	sete   %al
  407c22:	24 01                	and    $0x1,%al
  407c24:	3c 00                	cmp    $0x0,%al
  407c26:	74 46                	je     407c6e <runtime::arena_alloc+0x26e>
  407c28:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  407c2d:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407c34:	00 
  407c35:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407c3a:	e8 51 d6 ff ff       	call   405290 <runtime::heap_allocator>
  407c3f:	48 89 c1             	mov    %rax,%rcx
  407c42:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407c47:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  407c4e:	00 
  407c4f:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  407c56:	00 
  407c57:	48 8b 8c 24 90 00 00 	mov    0x90(%rsp),%rcx
  407c5e:	00 
  407c5f:	48 8b 94 24 98 00 00 	mov    0x98(%rsp),%rdx
  407c66:	00 
  407c67:	48 89 50 08          	mov    %rdx,0x8(%rax)
  407c6b:	48 89 08             	mov    %rcx,(%rax)
  407c6e:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  407c73:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  407c78:	4c 8b 4c 24 58       	mov    0x58(%rsp),%r9
  407c7d:	48 8b 94 24 00 01 00 	mov    0x100(%rsp),%rdx
  407c84:	00 
  407c85:	48 8b 02             	mov    (%rdx),%rax
  407c88:	48 8b 72 08          	mov    0x8(%rdx),%rsi
  407c8c:	48 8b 94 24 a0 00 00 	mov    0xa0(%rsp),%rdx
  407c93:	00 
  407c94:	48 89 b4 24 88 00 00 	mov    %rsi,0x88(%rsp)
  407c9b:	00 
  407c9c:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  407ca3:	00 
  407ca4:	48 8b bc 24 80 00 00 	mov    0x80(%rsp),%rdi
  407cab:	00 
  407cac:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  407cb3:	00 
  407cb4:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  407cbb:	00 00 
  407cbd:	48 89 e0             	mov    %rsp,%rax
  407cc0:	4c 89 08             	mov    %r9,(%rax)
  407cc3:	4c 8d 4c 24 78       	lea    0x78(%rsp),%r9
  407cc8:	e8 53 f6 ff ff       	call   407320 <runtime::memory_block_alloc>
  407ccd:	88 44 24 17          	mov    %al,0x17(%rsp)
  407cd1:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  407cd6:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  407cdb:	3c 00                	cmp    $0x0,%al
  407cdd:	74 4d                	je     407d2c <runtime::arena_alloc+0x32c>
  407cdf:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  407ce4:	8a 44 24 17          	mov    0x17(%rsp),%al
  407ce8:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  407cef:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  407cf6:	00 
  407cf7:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  407cfe:	00 
  407cff:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  407d06:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  407d0d:	00 
  407d0e:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  407d15:	00 
  407d16:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  407d1d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  407d21:	48 89 11             	mov    %rdx,(%rcx)
  407d24:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  407d2b:	c3                   	ret
  407d2c:	4c 8b 44 24 58       	mov    0x58(%rsp),%r8
  407d31:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  407d36:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  407d3b:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  407d40:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  407d45:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  407d4c:	00 
  407d4d:	48 8b 49 10          	mov    0x10(%rcx),%rcx
  407d51:	48 89 08             	mov    %rcx,(%rax)
  407d54:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407d5b:	00 
  407d5c:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  407d61:	48 89 48 10          	mov    %rcx,0x10(%rax)
  407d65:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407d6c:	00 
  407d6d:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  407d72:	48 8b 71 28          	mov    0x28(%rcx),%rsi
  407d76:	48 8b 48 20          	mov    0x20(%rax),%rcx
  407d7a:	48 01 f1             	add    %rsi,%rcx
  407d7d:	48 89 48 20          	mov    %rcx,0x20(%rax)
  407d81:	48 c7 84 24 c8 00 00 	movq   $0x0,0xc8(%rsp)
  407d88:	00 00 00 00 00 
  407d8d:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407d94:	00 
  407d95:	48 8b 78 10          	mov    0x10(%rax),%rdi
  407d99:	48 8b b4 24 d0 00 00 	mov    0xd0(%rsp),%rsi
  407da0:	00 
  407da1:	0f 57 c0             	xorps  %xmm0,%xmm0
  407da4:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  407da9:	48 8d 4c 24 60       	lea    0x60(%rsp),%rcx
  407dae:	e8 bd f9 ff ff       	call   407770 <runtime::alloc_from_memory_block>
  407db3:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  407db8:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  407dbd:	48 89 94 24 e8 00 00 	mov    %rdx,0xe8(%rsp)
  407dc4:	00 
  407dc5:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  407dcc:	00 
  407dcd:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  407dd4:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  407dd9:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  407de0:	00 
  407de1:	48 8b 70 10          	mov    0x10(%rax),%rsi
  407de5:	48 8b 50 18          	mov    0x18(%rax),%rdx
  407de9:	48 8b 76 20          	mov    0x20(%rsi),%rsi
  407ded:	48 8b bc 24 c8 00 00 	mov    0xc8(%rsp),%rdi
  407df4:	00 
  407df5:	48 29 fe             	sub    %rdi,%rsi
  407df8:	48 01 f2             	add    %rsi,%rdx
  407dfb:	48 89 50 18          	mov    %rdx,0x18(%rax)
  407dff:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  407e06:	00 
  407e07:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  407e0e:	00 
  407e0f:	8a 84 24 df 00 00 00 	mov    0xdf(%rsp),%al
  407e16:	48 89 b4 24 e8 00 00 	mov    %rsi,0xe8(%rsp)
  407e1d:	00 
  407e1e:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  407e25:	00 
  407e26:	88 84 24 df 00 00 00 	mov    %al,0xdf(%rsp)
  407e2d:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  407e31:	48 89 11             	mov    %rdx,(%rcx)
  407e34:	48 81 c4 08 01 00 00 	add    $0x108,%rsp
  407e3b:	c3                   	ret
  407e3c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000407e40 <runtime::arena_free_last_memory_block>:
  407e40:	48 83 ec 28          	sub    $0x28,%rsp
  407e44:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  407e49:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407e4e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  407e53:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407e58:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407e5d:	48 8b 40 10          	mov    0x10(%rax),%rax
  407e61:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  407e66:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
  407e6c:	0f 95 c0             	setne  %al
  407e6f:	24 01                	and    $0x1,%al
  407e71:	3c 00                	cmp    $0x0,%al
  407e73:	74 39                	je     407eae <runtime::arena_free_last_memory_block+0x6e>
  407e75:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  407e7a:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407e7f:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  407e84:	48 8b 09             	mov    (%rcx),%rcx
  407e87:	48 89 48 10          	mov    %rcx,0x10(%rax)
  407e8b:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407e90:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  407e95:	48 8b 51 28          	mov    0x28(%rcx),%rdx
  407e99:	48 8b 48 20          	mov    0x20(%rax),%rcx
  407e9d:	48 29 d1             	sub    %rdx,%rcx
  407ea0:	48 89 48 20          	mov    %rcx,0x20(%rax)
  407ea4:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  407ea9:	e8 22 f7 ff ff       	call   4075d0 <runtime::memory_block_dealloc>
  407eae:	48 83 c4 28          	add    $0x28,%rsp
  407eb2:	c3                   	ret
  407eb3:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  407eba:	84 00 00 00 00 00 

0000000000407ec0 <runtime::arena_free_all>:
  407ec0:	48 83 ec 28          	sub    $0x28,%rsp
  407ec4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  407ec9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407ece:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  407ed3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  407ed8:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407edd:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407ee2:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  407ee7:	0f 95 c0             	setne  %al
  407eea:	24 01                	and    $0x1,%al
  407eec:	3c 00                	cmp    $0x0,%al
  407eee:	74 2c                	je     407f1c <runtime::arena_free_all+0x5c>
  407ef0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407ef5:	48 8b 40 10          	mov    0x10(%rax),%rax
  407ef9:	48 83 38 00          	cmpq   $0x0,(%rax)
  407efd:	0f 95 c0             	setne  %al
  407f00:	24 01                	and    $0x1,%al
  407f02:	3c 00                	cmp    $0x0,%al
  407f04:	74 16                	je     407f1c <runtime::arena_free_all+0x5c>
  407f06:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  407f0b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  407f10:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  407f15:	e8 26 ff ff ff       	call   407e40 <runtime::arena_free_last_memory_block>
  407f1a:	eb c1                	jmp    407edd <runtime::arena_free_all+0x1d>
  407f1c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407f21:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  407f26:	0f 95 c0             	setne  %al
  407f29:	24 01                	and    $0x1,%al
  407f2b:	3c 00                	cmp    $0x0,%al
  407f2d:	74 32                	je     407f61 <runtime::arena_free_all+0xa1>
  407f2f:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407f34:	48 8b 40 10          	mov    0x10(%rax),%rax
  407f38:	48 8b 78 18          	mov    0x18(%rax),%rdi
  407f3c:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407f41:	48 8b 40 10          	mov    0x10(%rax),%rax
  407f45:	48 8b 50 20          	mov    0x20(%rax),%rdx
  407f49:	31 f6                	xor    %esi,%esi
  407f4b:	e8 f0 90 ff ff       	call   401040 <memset@plt>
  407f50:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407f55:	48 8b 40 10          	mov    0x10(%rax),%rax
  407f59:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  407f60:	00 
  407f61:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407f66:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  407f6d:	00 
  407f6e:	48 83 c4 28          	add    $0x28,%rsp
  407f72:	c3                   	ret
  407f73:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  407f7a:	84 00 00 00 00 00 

0000000000407f80 <runtime::arena_destroy>:
  407f80:	48 83 ec 28          	sub    $0x28,%rsp
  407f84:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  407f89:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  407f8e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  407f93:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  407f98:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407f9d:	48 83 78 10 00       	cmpq   $0x0,0x10(%rax)
  407fa2:	0f 95 c0             	setne  %al
  407fa5:	24 01                	and    $0x1,%al
  407fa7:	3c 00                	cmp    $0x0,%al
  407fa9:	74 49                	je     407ff4 <runtime::arena_destroy+0x74>
  407fab:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  407fb0:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407fb5:	48 8b 40 10          	mov    0x10(%rax),%rax
  407fb9:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  407fbe:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407fc3:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  407fc8:	48 8b 09             	mov    (%rcx),%rcx
  407fcb:	48 89 48 10          	mov    %rcx,0x10(%rax)
  407fcf:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407fd4:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  407fd9:	48 8b 51 28          	mov    0x28(%rcx),%rdx
  407fdd:	48 8b 48 20          	mov    0x20(%rax),%rcx
  407fe1:	48 29 d1             	sub    %rdx,%rcx
  407fe4:	48 89 48 20          	mov    %rcx,0x20(%rax)
  407fe8:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  407fed:	e8 de f5 ff ff       	call   4075d0 <runtime::memory_block_dealloc>
  407ff2:	eb a4                	jmp    407f98 <runtime::arena_destroy+0x18>
  407ff4:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  407ff9:	48 c7 40 18 00 00 00 	movq   $0x0,0x18(%rax)
  408000:	00 
  408001:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  408006:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  40800d:	00 
  40800e:	48 83 c4 28          	add    $0x28,%rsp
  408012:	c3                   	ret
  408013:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40801a:	84 00 00 00 00 00 

0000000000408020 <runtime::arena_allocator_proc>:
  408020:	48 81 ec f8 01 00 00 	sub    $0x1f8,%rsp
  408027:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  40802e:	00 
  40802f:	4c 89 84 24 88 00 00 	mov    %r8,0x88(%rsp)
  408036:	00 
  408037:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  40803e:	00 
  40803f:	48 89 94 24 98 00 00 	mov    %rdx,0x98(%rsp)
  408046:	00 
  408047:	48 89 bc 24 a0 00 00 	mov    %rdi,0xa0(%rsp)
  40804e:	00 
  40804f:	40 88 f0             	mov    %sil,%al
  408052:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  408059:	48 8b 84 24 10 02 00 	mov    0x210(%rsp),%rax
  408060:	00 
  408061:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  408068:	00 
  408069:	48 8b 84 24 08 02 00 	mov    0x208(%rsp),%rax
  408070:	00 
  408071:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  408078:	00 
  408079:	48 8b 84 24 00 02 00 	mov    0x200(%rsp),%rax
  408080:	00 
  408081:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  408088:	00 
  408089:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  408090:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  408097:	00 
  408098:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  40809f:	00 
  4080a0:	48 8b b4 24 98 00 00 	mov    0x98(%rsp),%rsi
  4080a7:	00 
  4080a8:	48 8b bc 24 a0 00 00 	mov    0xa0(%rsp),%rdi
  4080af:	00 
  4080b0:	4c 8b 84 24 88 00 00 	mov    0x88(%rsp),%r8
  4080b7:	00 
  4080b8:	48 89 bc 24 f0 01 00 	mov    %rdi,0x1f0(%rsp)
  4080bf:	00 
  4080c0:	88 84 24 ef 01 00 00 	mov    %al,0x1ef(%rsp)
  4080c7:	48 89 b4 24 e0 01 00 	mov    %rsi,0x1e0(%rsp)
  4080ce:	00 
  4080cf:	48 89 94 24 d8 01 00 	mov    %rdx,0x1d8(%rsp)
  4080d6:	00 
  4080d7:	4c 89 84 24 d0 01 00 	mov    %r8,0x1d0(%rsp)
  4080de:	00 
  4080df:	48 89 8c 24 c8 01 00 	mov    %rcx,0x1c8(%rsp)
  4080e6:	00 
  4080e7:	0f 57 c0             	xorps  %xmm0,%xmm0
  4080ea:	0f 29 84 24 b0 01 00 	movaps %xmm0,0x1b0(%rsp)
  4080f1:	00 
  4080f2:	c6 84 24 af 01 00 00 	movb   $0x0,0x1af(%rsp)
  4080f9:	00 
  4080fa:	48 89 bc 24 a0 01 00 	mov    %rdi,0x1a0(%rsp)
  408101:	00 
  408102:	48 89 b4 24 98 01 00 	mov    %rsi,0x198(%rsp)
  408109:	00 
  40810a:	48 89 94 24 90 01 00 	mov    %rdx,0x190(%rsp)
  408111:	00 
  408112:	48 89 8c 24 88 01 00 	mov    %rcx,0x188(%rsp)
  408119:	00 
  40811a:	0f b6 c8             	movzbl %al,%ecx
  40811d:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  408122:	2c 07                	sub    $0x7,%al
  408124:	0f 87 b0 07 00 00    	ja     4088da <runtime::arena_allocator_proc+0x8ba>
  40812a:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40812f:	48 8b 04 c5 10 f8 40 	mov    0x40f810(,%rax,8),%rax
  408136:	00 
  408137:	ff e0                	jmp    *%rax
  408139:	4c 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%r9
  408140:	00 
  408141:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  408148:	00 
  408149:	48 8b bc 24 a0 01 00 	mov    0x1a0(%rsp),%rdi
  408150:	00 
  408151:	48 8b b4 24 98 01 00 	mov    0x198(%rsp),%rsi
  408158:	00 
  408159:	48 8b 94 24 90 01 00 	mov    0x190(%rsp),%rdx
  408160:	00 
  408161:	0f 57 c0             	xorps  %xmm0,%xmm0
  408164:	0f 29 84 24 70 01 00 	movaps %xmm0,0x170(%rsp)
  40816b:	00 
  40816c:	4c 8d 84 24 70 01 00 	lea    0x170(%rsp),%r8
  408173:	00 
  408174:	e8 87 f8 ff ff       	call   407a00 <runtime::arena_alloc>
  408179:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  408180:	00 
  408181:	40 88 c7             	mov    %al,%dil
  408184:	40 88 f8             	mov    %dil,%al
  408187:	48 8b 94 24 70 01 00 	mov    0x170(%rsp),%rdx
  40818e:	00 
  40818f:	48 8b b4 24 78 01 00 	mov    0x178(%rsp),%rsi
  408196:	00 
  408197:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  40819e:	00 
  40819f:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  4081a6:	00 
  4081a7:	40 88 bc 24 af 01 00 	mov    %dil,0x1af(%rsp)
  4081ae:	00 
  4081af:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4081b3:	48 89 11             	mov    %rdx,(%rcx)
  4081b6:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  4081bd:	c3                   	ret
  4081be:	c6 84 24 af 01 00 00 	movb   $0x4,0x1af(%rsp)
  4081c5:	04 
  4081c6:	e9 0f 07 00 00       	jmp    4088da <runtime::arena_allocator_proc+0x8ba>
  4081cb:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  4081d2:	00 
  4081d3:	48 8b b4 24 c0 00 00 	mov    0xc0(%rsp),%rsi
  4081da:	00 
  4081db:	48 8b bc 24 a0 01 00 	mov    0x1a0(%rsp),%rdi
  4081e2:	00 
  4081e3:	e8 d8 fc ff ff       	call   407ec0 <runtime::arena_free_all>
  4081e8:	e9 ed 06 00 00       	jmp    4088da <runtime::arena_allocator_proc+0x8ba>
  4081ed:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  4081f4:	00 
  4081f5:	48 89 84 24 68 01 00 	mov    %rax,0x168(%rsp)
  4081fc:	00 
  4081fd:	48 83 bc 24 68 01 00 	cmpq   $0x0,0x168(%rsp)
  408204:	00 00 
  408206:	0f 94 c1             	sete   %cl
  408209:	80 e1 01             	and    $0x1,%cl
  40820c:	b0 01                	mov    $0x1,%al
  40820e:	38 c8                	cmp    %cl,%al
  408210:	74 25                	je     408237 <runtime::arena_allocator_proc+0x217>
  408212:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408219:	00 
  40821a:	48 3b 84 24 88 01 00 	cmp    0x188(%rsp),%rax
  408221:	00 
  408222:	0f 94 c1             	sete   %cl
  408225:	80 e1 01             	and    $0x1,%cl
  408228:	b0 01                	mov    $0x1,%al
  40822a:	38 c8                	cmp    %cl,%al
  40822c:	0f 84 a8 00 00 00    	je     4082da <runtime::arena_allocator_proc+0x2ba>
  408232:	e9 85 00 00 00       	jmp    4082bc <runtime::arena_allocator_proc+0x29c>
  408237:	4c 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%r9
  40823e:	00 
  40823f:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  408246:	00 
  408247:	48 8b bc 24 a0 01 00 	mov    0x1a0(%rsp),%rdi
  40824e:	00 
  40824f:	48 8b b4 24 98 01 00 	mov    0x198(%rsp),%rsi
  408256:	00 
  408257:	48 8b 94 24 90 01 00 	mov    0x190(%rsp),%rdx
  40825e:	00 
  40825f:	0f 57 c0             	xorps  %xmm0,%xmm0
  408262:	0f 29 84 24 50 01 00 	movaps %xmm0,0x150(%rsp)
  408269:	00 
  40826a:	4c 8d 84 24 50 01 00 	lea    0x150(%rsp),%r8
  408271:	00 
  408272:	e8 89 f7 ff ff       	call   407a00 <runtime::arena_alloc>
  408277:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  40827e:	00 
  40827f:	40 88 c7             	mov    %al,%dil
  408282:	40 88 f8             	mov    %dil,%al
  408285:	48 8b 94 24 50 01 00 	mov    0x150(%rsp),%rdx
  40828c:	00 
  40828d:	48 8b b4 24 58 01 00 	mov    0x158(%rsp),%rsi
  408294:	00 
  408295:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  40829c:	00 
  40829d:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  4082a4:	00 
  4082a5:	40 88 bc 24 af 01 00 	mov    %dil,0x1af(%rsp)
  4082ac:	00 
  4082ad:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4082b1:	48 89 11             	mov    %rdx,(%rcx)
  4082b4:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  4082bb:	c3                   	ret
  4082bc:	48 83 bc 24 98 01 00 	cmpq   $0x0,0x198(%rsp)
  4082c3:	00 00 
  4082c5:	0f 94 c1             	sete   %cl
  4082c8:	80 e1 01             	and    $0x1,%cl
  4082cb:	b0 01                	mov    $0x1,%al
  4082cd:	38 c8                	cmp    %cl,%al
  4082cf:	0f 84 e5 00 00 00    	je     4083ba <runtime::arena_allocator_proc+0x39a>
  4082d5:	e9 b7 00 00 00       	jmp    408391 <runtime::arena_allocator_proc+0x371>
  4082da:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  4082e1:	00 
  4082e2:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  4082e7:	4c 8b 8c 24 98 01 00 	mov    0x198(%rsp),%r9
  4082ee:	00 
  4082ef:	4c 89 4c 24 70       	mov    %r9,0x70(%rsp)
  4082f4:	bf 60 f8 40 00       	mov    $0x40f860,%edi
  4082f9:	31 c0                	xor    %eax,%eax
  4082fb:	41 89 c0             	mov    %eax,%r8d
  4082fe:	be 3c 00 00 00       	mov    $0x3c,%esi
  408303:	ba db 00 00 00       	mov    $0xdb,%edx
  408308:	b9 13 00 00 00       	mov    $0x13,%ecx
  40830d:	e8 0e 99 ff ff       	call   401c20 <runtime::multi_pointer_slice_expr_error>
  408312:	48 8b 54 24 68       	mov    0x68(%rsp),%rdx
  408317:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40831c:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  408323:	00 
  408324:	48 89 94 24 40 01 00 	mov    %rdx,0x140(%rsp)
  40832b:	00 
  40832c:	48 89 84 24 48 01 00 	mov    %rax,0x148(%rsp)
  408333:	00 
  408334:	48 8b 84 24 40 01 00 	mov    0x140(%rsp),%rax
  40833b:	00 
  40833c:	48 8b 94 24 48 01 00 	mov    0x148(%rsp),%rdx
  408343:	00 
  408344:	48 89 94 24 b8 01 00 	mov    %rdx,0x1b8(%rsp)
  40834b:	00 
  40834c:	48 89 84 24 b0 01 00 	mov    %rax,0x1b0(%rsp)
  408353:	00 
  408354:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  40835b:	00 
  40835c:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  408363:	00 
  408364:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  40836b:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  408372:	00 
  408373:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  40837a:	00 
  40837b:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  408382:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  408386:	48 89 11             	mov    %rdx,(%rcx)
  408389:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  408390:	c3                   	ret
  408391:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  408398:	00 
  408399:	48 8b 8c 24 90 01 00 	mov    0x190(%rsp),%rcx
  4083a0:	00 
  4083a1:	48 83 e9 01          	sub    $0x1,%rcx
  4083a5:	48 21 c8             	and    %rcx,%rax
  4083a8:	48 83 f8 00          	cmp    $0x0,%rax
  4083ac:	0f 94 c1             	sete   %cl
  4083af:	80 e1 01             	and    $0x1,%cl
  4083b2:	b0 01                	mov    $0x1,%al
  4083b4:	38 c8                	cmp    %cl,%al
  4083b6:	74 54                	je     40840c <runtime::arena_allocator_proc+0x3ec>
  4083b8:	eb 4d                	jmp    408407 <runtime::arena_allocator_proc+0x3e7>
  4083ba:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4083c1:	00 
  4083c2:	c6 84 24 af 01 00 00 	movb   $0x4,0x1af(%rsp)
  4083c9:	04 
  4083ca:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4083d1:	00 
  4083d2:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4083d9:	00 
  4083da:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  4083e1:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  4083e8:	00 
  4083e9:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  4083f0:	00 
  4083f1:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  4083f8:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4083fc:	48 89 11             	mov    %rdx,(%rcx)
  4083ff:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  408406:	c3                   	ret
  408407:	e9 a8 02 00 00       	jmp    4086b4 <runtime::arena_allocator_proc+0x694>
  40840c:	48 8b 84 24 98 01 00 	mov    0x198(%rsp),%rax
  408413:	00 
  408414:	48 3b 84 24 88 01 00 	cmp    0x188(%rsp),%rax
  40841b:	00 
  40841c:	0f 92 c0             	setb   %al
  40841f:	24 01                	and    $0x1,%al
  408421:	3c 00                	cmp    $0x0,%al
  408423:	0f 84 b7 00 00 00    	je     4084e0 <runtime::arena_allocator_proc+0x4c0>
  408429:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  408430:	00 
  408431:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  408436:	4c 8b 8c 24 98 01 00 	mov    0x198(%rsp),%r9
  40843d:	00 
  40843e:	4c 89 4c 24 60       	mov    %r9,0x60(%rsp)
  408443:	bf 60 f8 40 00       	mov    $0x40f860,%edi
  408448:	31 c0                	xor    %eax,%eax
  40844a:	41 89 c0             	mov    %eax,%r8d
  40844d:	be 3c 00 00 00       	mov    $0x3c,%esi
  408452:	ba e3 00 00 00       	mov    $0xe3,%edx
  408457:	b9 14 00 00 00       	mov    $0x14,%ecx
  40845c:	e8 bf 97 ff ff       	call   401c20 <runtime::multi_pointer_slice_expr_error>
  408461:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  408466:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40846b:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  408472:	00 
  408473:	48 89 94 24 30 01 00 	mov    %rdx,0x130(%rsp)
  40847a:	00 
  40847b:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  408482:	00 
  408483:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40848a:	00 
  40848b:	48 8b 94 24 38 01 00 	mov    0x138(%rsp),%rdx
  408492:	00 
  408493:	48 89 94 24 b8 01 00 	mov    %rdx,0x1b8(%rsp)
  40849a:	00 
  40849b:	48 89 84 24 b0 01 00 	mov    %rax,0x1b0(%rsp)
  4084a2:	00 
  4084a3:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4084aa:	00 
  4084ab:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4084b2:	00 
  4084b3:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  4084ba:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  4084c1:	00 
  4084c2:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  4084c9:	00 
  4084ca:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  4084d1:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4084d5:	48 89 11             	mov    %rdx,(%rcx)
  4084d8:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  4084df:	c3                   	ret
  4084e0:	eb 00                	jmp    4084e2 <runtime::arena_allocator_proc+0x4c2>
  4084e2:	48 8b 84 24 a0 01 00 	mov    0x1a0(%rsp),%rax
  4084e9:	00 
  4084ea:	48 8b 40 10          	mov    0x10(%rax),%rax
  4084ee:	48 89 84 24 28 01 00 	mov    %rax,0x128(%rsp)
  4084f5:	00 
  4084f6:	48 83 bc 24 28 01 00 	cmpq   $0x0,0x128(%rsp)
  4084fd:	00 00 
  4084ff:	0f 95 c0             	setne  %al
  408502:	24 01                	and    $0x1,%al
  408504:	3c 00                	cmp    $0x0,%al
  408506:	0f 84 a6 01 00 00    	je     4086b2 <runtime::arena_allocator_proc+0x692>
  40850c:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  408513:	00 
  408514:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  40851b:	00 
  40851c:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  408520:	48 29 c8             	sub    %rcx,%rax
  408523:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40852a:	00 
  40852b:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  408532:	00 
  408533:	48 03 84 24 88 01 00 	add    0x188(%rsp),%rax
  40853a:	00 
  40853b:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  408542:	00 
  408543:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40854a:	00 
  40854b:	48 03 84 24 98 01 00 	add    0x198(%rsp),%rax
  408552:	00 
  408553:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40855a:	00 
  40855b:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  408562:	00 
  408563:	48 3b 84 24 18 01 00 	cmp    0x118(%rsp),%rax
  40856a:	00 
  40856b:	0f 92 c0             	setb   %al
  40856e:	24 01                	and    $0x1,%al
  408570:	3c 00                	cmp    $0x0,%al
  408572:	0f 84 38 01 00 00    	je     4086b0 <runtime::arena_allocator_proc+0x690>
  408578:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  40857f:	00 
  408580:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  408587:	00 
  408588:	48 3b 41 20          	cmp    0x20(%rcx),%rax
  40858c:	0f 94 c0             	sete   %al
  40858f:	24 01                	and    $0x1,%al
  408591:	3c 00                	cmp    $0x0,%al
  408593:	0f 84 17 01 00 00    	je     4086b0 <runtime::arena_allocator_proc+0x690>
  408599:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  4085a0:	00 
  4085a1:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  4085a8:	00 
  4085a9:	48 3b 41 28          	cmp    0x28(%rcx),%rax
  4085ad:	0f 96 c0             	setbe  %al
  4085b0:	24 01                	and    $0x1,%al
  4085b2:	3c 00                	cmp    $0x0,%al
  4085b4:	0f 84 f6 00 00 00    	je     4086b0 <runtime::arena_allocator_proc+0x690>
  4085ba:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  4085c1:	00 
  4085c2:	48 8b 8c 24 10 01 00 	mov    0x110(%rsp),%rcx
  4085c9:	00 
  4085ca:	48 89 48 20          	mov    %rcx,0x20(%rax)
  4085ce:	48 8b 84 24 a0 01 00 	mov    0x1a0(%rsp),%rax
  4085d5:	00 
  4085d6:	48 8b 8c 24 10 01 00 	mov    0x110(%rsp),%rcx
  4085dd:	00 
  4085de:	48 89 48 18          	mov    %rcx,0x18(%rax)
  4085e2:	48 8b 84 24 28 01 00 	mov    0x128(%rsp),%rax
  4085e9:	00 
  4085ea:	48 8b 40 18          	mov    0x18(%rax),%rax
  4085ee:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  4085f3:	4c 8b 84 24 20 01 00 	mov    0x120(%rsp),%r8
  4085fa:	00 
  4085fb:	4c 89 44 24 50       	mov    %r8,0x50(%rsp)
  408600:	4c 8b 8c 24 10 01 00 	mov    0x110(%rsp),%r9
  408607:	00 
  408608:	4c 89 4c 24 48       	mov    %r9,0x48(%rsp)
  40860d:	bf 60 f8 40 00       	mov    $0x40f860,%edi
  408612:	be 3c 00 00 00       	mov    $0x3c,%esi
  408617:	ba ef 00 00 00       	mov    $0xef,%edx
  40861c:	b9 17 00 00 00       	mov    $0x17,%ecx
  408621:	e8 fa 95 ff ff       	call   401c20 <runtime::multi_pointer_slice_expr_error>
  408626:	48 8b 54 24 40       	mov    0x40(%rsp),%rdx
  40862b:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  408630:	48 8b 74 24 50       	mov    0x50(%rsp),%rsi
  408635:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  40863c:	00 
  40863d:	48 01 f2             	add    %rsi,%rdx
  408640:	48 29 f0             	sub    %rsi,%rax
  408643:	48 89 94 24 00 01 00 	mov    %rdx,0x100(%rsp)
  40864a:	00 
  40864b:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  408652:	00 
  408653:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40865a:	00 
  40865b:	48 8b 94 24 08 01 00 	mov    0x108(%rsp),%rdx
  408662:	00 
  408663:	48 89 94 24 b8 01 00 	mov    %rdx,0x1b8(%rsp)
  40866a:	00 
  40866b:	48 89 84 24 b0 01 00 	mov    %rax,0x1b0(%rsp)
  408672:	00 
  408673:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  40867a:	00 
  40867b:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  408682:	00 
  408683:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  40868a:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  408691:	00 
  408692:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  408699:	00 
  40869a:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  4086a1:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4086a5:	48 89 11             	mov    %rdx,(%rcx)
  4086a8:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  4086af:	c3                   	ret
  4086b0:	eb 00                	jmp    4086b2 <runtime::arena_allocator_proc+0x692>
  4086b2:	eb 00                	jmp    4086b4 <runtime::arena_allocator_proc+0x694>
  4086b4:	4c 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%r9
  4086bb:	00 
  4086bc:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  4086c3:	00 
  4086c4:	48 8b bc 24 a0 01 00 	mov    0x1a0(%rsp),%rdi
  4086cb:	00 
  4086cc:	48 8b b4 24 98 01 00 	mov    0x198(%rsp),%rsi
  4086d3:	00 
  4086d4:	48 8b 94 24 90 01 00 	mov    0x190(%rsp),%rdx
  4086db:	00 
  4086dc:	0f 57 c0             	xorps  %xmm0,%xmm0
  4086df:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  4086e6:	00 
  4086e7:	4c 8d 84 24 f0 00 00 	lea    0xf0(%rsp),%r8
  4086ee:	00 
  4086ef:	e8 0c f3 ff ff       	call   407a00 <runtime::arena_alloc>
  4086f4:	88 44 24 2f          	mov    %al,0x2f(%rsp)
  4086f8:	48 8b 8c 24 f0 00 00 	mov    0xf0(%rsp),%rcx
  4086ff:	00 
  408700:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  408705:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  40870c:	00 
  40870d:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  408712:	3c 00                	cmp    $0x0,%al
  408714:	74 50                	je     408766 <runtime::arena_allocator_proc+0x746>
  408716:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  40871d:	00 
  40871e:	8a 44 24 2f          	mov    0x2f(%rsp),%al
  408722:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  408729:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  408730:	00 
  408731:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  408738:	00 
  408739:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  408740:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  408747:	00 
  408748:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  40874f:	00 
  408750:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  408757:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40875b:	48 89 11             	mov    %rdx,(%rcx)
  40875e:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  408765:	c3                   	ret
  408766:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40876b:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  408770:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  408777:	00 
  408778:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  40877f:	00 
  408780:	48 83 bc 24 e0 00 00 	cmpq   $0x0,0xe0(%rsp)
  408787:	00 00 
  408789:	0f 94 c0             	sete   %al
  40878c:	24 01                	and    $0x1,%al
  40878e:	3c 00                	cmp    $0x0,%al
  408790:	74 45                	je     4087d7 <runtime::arena_allocator_proc+0x7b7>
  408792:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  408799:	00 
  40879a:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4087a1:	00 
  4087a2:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4087a9:	00 
  4087aa:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  4087b1:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  4087b8:	00 
  4087b9:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  4087c0:	00 
  4087c1:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  4087c8:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4087cc:	48 89 11             	mov    %rdx,(%rcx)
  4087cf:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  4087d6:	c3                   	ret
  4087d7:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  4087de:	00 
  4087df:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  4087e4:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4087eb:	00 
  4087ec:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4087f1:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  4087f8:	00 
  4087f9:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  4087fe:	4c 8b 8c 24 88 01 00 	mov    0x188(%rsp),%r9
  408805:	00 
  408806:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40880b:	bf 60 f8 40 00       	mov    $0x40f860,%edi
  408810:	31 c0                	xor    %eax,%eax
  408812:	41 89 c0             	mov    %eax,%r8d
  408815:	be 3c 00 00 00       	mov    $0x3c,%esi
  40881a:	ba fa 00 00 00       	mov    $0xfa,%edx
  40881f:	b9 1c 00 00 00       	mov    $0x1c,%ecx
  408824:	e8 f7 93 ff ff       	call   401c20 <runtime::multi_pointer_slice_expr_error>
  408829:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40882e:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  408833:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  408838:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40883d:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  408844:	00 
  408845:	48 89 84 24 d8 00 00 	mov    %rax,0xd8(%rsp)
  40884c:	00 
  40884d:	48 8b 94 24 d0 00 00 	mov    0xd0(%rsp),%rdx
  408854:	00 
  408855:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  40885c:	00 
  40885d:	e8 be 89 ff ff       	call   401220 <runtime::copy_slice:proc"contextless"(dst:[]u8,src:[]u8)->(:int)>
  408862:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  408869:	00 
  40886a:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  408871:	00 
  408872:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  408879:	00 
  40887a:	48 89 94 24 b8 01 00 	mov    %rdx,0x1b8(%rsp)
  408881:	00 
  408882:	48 89 8c 24 b0 01 00 	mov    %rcx,0x1b0(%rsp)
  408889:	00 
  40888a:	c6 84 24 af 01 00 00 	movb   $0x0,0x1af(%rsp)
  408891:	00 
  408892:	48 89 50 08          	mov    %rdx,0x8(%rax)
  408896:	48 89 08             	mov    %rcx,(%rax)
  408899:	31 c0                	xor    %eax,%eax
  40889b:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  4088a2:	c3                   	ret
  4088a3:	48 8b 84 24 88 00 00 	mov    0x88(%rsp),%rax
  4088aa:	00 
  4088ab:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  4088b2:	00 
  4088b3:	48 83 bc 24 c8 00 00 	cmpq   $0x0,0xc8(%rsp)
  4088ba:	00 00 
  4088bc:	0f 95 c0             	setne  %al
  4088bf:	24 01                	and    $0x1,%al
  4088c1:	3c 00                	cmp    $0x0,%al
  4088c3:	74 0b                	je     4088d0 <runtime::arena_allocator_proc+0x8b0>
  4088c5:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  4088cc:	00 
  4088cd:	c6 00 5d             	movb   $0x5d,(%rax)
  4088d0:	eb 08                	jmp    4088da <runtime::arena_allocator_proc+0x8ba>
  4088d2:	c6 84 24 af 01 00 00 	movb   $0x4,0x1af(%rsp)
  4088d9:	04 
  4088da:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4088e1:	00 
  4088e2:	48 8b 94 24 b0 01 00 	mov    0x1b0(%rsp),%rdx
  4088e9:	00 
  4088ea:	48 8b b4 24 b8 01 00 	mov    0x1b8(%rsp),%rsi
  4088f1:	00 
  4088f2:	8a 84 24 af 01 00 00 	mov    0x1af(%rsp),%al
  4088f9:	48 89 b4 24 b8 01 00 	mov    %rsi,0x1b8(%rsp)
  408900:	00 
  408901:	48 89 94 24 b0 01 00 	mov    %rdx,0x1b0(%rsp)
  408908:	00 
  408909:	88 84 24 af 01 00 00 	mov    %al,0x1af(%rsp)
  408910:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  408914:	48 89 11             	mov    %rdx,(%rcx)
  408917:	48 81 c4 f8 01 00 00 	add    $0x1f8,%rsp
  40891e:	c3                   	ret
  40891f:	90                   	nop

0000000000408920 <runtime::alloc_from_memory_block.calc_alignment_offset-0>:
  408920:	48 89 7c 24 c0       	mov    %rdi,-0x40(%rsp)
  408925:	48 89 74 24 c8       	mov    %rsi,-0x38(%rsp)
  40892a:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40892f:	48 8b 4c 24 c0       	mov    -0x40(%rsp),%rcx
  408934:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  408939:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40893e:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  408945:	00 00 
  408947:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40894c:	48 8b 49 18          	mov    0x18(%rcx),%rcx
  408950:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  408955:	48 03 4a 20          	add    0x20(%rdx),%rcx
  408959:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40895e:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  408963:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  408968:	48 83 e8 01          	sub    $0x1,%rax
  40896c:	48 89 44 24 d0       	mov    %rax,-0x30(%rsp)
  408971:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  408976:	48 23 44 24 d0       	and    -0x30(%rsp),%rax
  40897b:	48 83 f8 00          	cmp    $0x0,%rax
  40897f:	0f 95 c0             	setne  %al
  408982:	24 01                	and    $0x1,%al
  408984:	3c 00                	cmp    $0x0,%al
  408986:	74 17                	je     40899f <runtime::alloc_from_memory_block.calc_alignment_offset-0+0x7f>
  408988:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40898d:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  408992:	48 23 4c 24 d0       	and    -0x30(%rsp),%rcx
  408997:	48 29 c8             	sub    %rcx,%rax
  40899a:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40899f:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4089a4:	c3                   	ret
  4089a5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  4089ac:	00 00 00 00 

00000000004089b0 <runtime::arena_alloc.align_forward_uint-0>:
  4089b0:	48 89 7c 24 d0       	mov    %rdi,-0x30(%rsp)
  4089b5:	48 89 74 24 d8       	mov    %rsi,-0x28(%rsp)
  4089ba:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  4089bf:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  4089c4:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  4089c9:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  4089ce:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  4089d3:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  4089d8:	48 83 e9 01          	sub    $0x1,%rcx
  4089dc:	48 21 c8             	and    %rcx,%rax
  4089df:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  4089e4:	48 83 7c 24 e0 00    	cmpq   $0x0,-0x20(%rsp)
  4089ea:	0f 95 c0             	setne  %al
  4089ed:	24 01                	and    $0x1,%al
  4089ef:	3c 00                	cmp    $0x0,%al
  4089f1:	74 14                	je     408a07 <runtime::arena_alloc.align_forward_uint-0+0x57>
  4089f3:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  4089f8:	48 2b 44 24 e0       	sub    -0x20(%rsp),%rax
  4089fd:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  408a02:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  408a07:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  408a0c:	c3                   	ret
  408a0d:	0f 1f 00             	nopl   (%rax)

0000000000408a10 <os::[allocators.odin]::init_thread_local_cleaner>:
  408a10:	48 83 ec 18          	sub    $0x18,%rsp
  408a14:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  408a19:	31 f6                	xor    %esi,%esi
  408a1b:	ba 10 00 00 00       	mov    $0x10,%edx
  408a20:	e8 1b 86 ff ff       	call   401040 <memset@plt>
  408a25:	31 c0                	xor    %eax,%eax
  408a27:	a8 01                	test   $0x1,%al
  408a29:	75 02                	jne    408a2d <os::[allocators.odin]::init_thread_local_cleaner+0x1d>
  408a2b:	eb 13                	jmp    408a40 <os::[allocators.odin]::init_thread_local_cleaner+0x30>
  408a2d:	48 8d 7c 24 08       	lea    0x8(%rsp),%rdi
  408a32:	31 f6                	xor    %esi,%esi
  408a34:	ba 10 00 00 00       	mov    $0x10,%edx
  408a39:	e8 02 86 ff ff       	call   401040 <memset@plt>
  408a3e:	eb 15                	jmp    408a55 <os::[allocators.odin]::init_thread_local_cleaner+0x45>
  408a40:	48 c7 c0 a0 8c 40 00 	mov    $0x408ca0,%rax
  408a47:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  408a4c:	48 c7 44 24 10 02 00 	movq   $0x2,0x10(%rsp)
  408a53:	00 00 
  408a55:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  408a5a:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  408a5f:	e8 ec 8d ff ff       	call   401850 <runtime::add_thread_local_cleaner>
  408a64:	48 83 c4 18          	add    $0x18,%rsp
  408a68:	c3                   	ret
  408a69:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000408a70 <os::[file_linux.odin]::_standard_stream_init>:
  408a70:	50                   	push   %rax
  408a71:	eb 00                	jmp    408a73 <os::[file_linux.odin]::_standard_stream_init+0x3>
  408a73:	31 c0                	xor    %eax,%eax
  408a75:	bf 70 40 41 00       	mov    $0x414070,%edi
  408a7a:	ba 68 fa 40 00       	mov    $0x40fa68,%edx
  408a7f:	31 f6                	xor    %esi,%esi
  408a81:	b9 0f 00 00 00       	mov    $0xf,%ecx
  408a86:	e8 05 39 00 00       	call   40c390 <os::[file_linux.odin]::_standard_stream_init.new_std-0>
  408a8b:	48 89 c1             	mov    %rax,%rcx
  408a8e:	48 c7 c0 38 42 41 00 	mov    $0x414238,%rax
  408a95:	48 89 08             	mov    %rcx,(%rax)
  408a98:	48 b8 70 40 41 00 00 	movabs $0x414070,%rax
  408a9f:	00 00 00 
  408aa2:	48 83 c0 68          	add    $0x68,%rax
  408aa6:	ba 78 fa 40 00       	mov    $0x40fa78,%edx
  408aab:	bf d8 40 41 00       	mov    $0x4140d8,%edi
  408ab0:	be 01 00 00 00       	mov    $0x1,%esi
  408ab5:	b9 0f 00 00 00       	mov    $0xf,%ecx
  408aba:	e8 d1 38 00 00       	call   40c390 <os::[file_linux.odin]::_standard_stream_init.new_std-0>
  408abf:	48 89 c1             	mov    %rax,%rcx
  408ac2:	48 c7 c0 40 42 41 00 	mov    $0x414240,%rax
  408ac9:	48 89 08             	mov    %rcx,(%rax)
  408acc:	48 b8 70 40 41 00 00 	movabs $0x414070,%rax
  408ad3:	00 00 00 
  408ad6:	48 05 d0 00 00 00    	add    $0xd0,%rax
  408adc:	ba 88 fa 40 00       	mov    $0x40fa88,%edx
  408ae1:	bf 40 41 41 00       	mov    $0x414140,%edi
  408ae6:	be 02 00 00 00       	mov    $0x2,%esi
  408aeb:	b9 0f 00 00 00       	mov    $0xf,%ecx
  408af0:	e8 9b 38 00 00       	call   40c390 <os::[file_linux.odin]::_standard_stream_init.new_std-0>
  408af5:	48 89 c1             	mov    %rax,%rcx
  408af8:	48 c7 c0 48 42 41 00 	mov    $0x414248,%rax
  408aff:	48 89 08             	mov    %rcx,(%rax)
  408b02:	58                   	pop    %rax
  408b03:	c3                   	ret
  408b04:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  408b0b:	00 00 00 00 00 

0000000000408b10 <os::[process.odin]::delete_args>:
  408b10:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  408b17:	48 c7 c0 50 42 41 00 	mov    $0x414250,%rax
  408b1e:	48 83 38 00          	cmpq   $0x0,(%rax)
  408b22:	0f 95 c0             	setne  %al
  408b25:	24 01                	and    $0x1,%al
  408b27:	3c 00                	cmp    $0x0,%al
  408b29:	0f 84 5c 01 00 00    	je     408c8b <os::[process.odin]::delete_args+0x17b>
  408b2f:	0f 57 c0             	xorps  %xmm0,%xmm0
  408b32:	0f 29 04 24          	movaps %xmm0,(%rsp)
  408b36:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  408b3d:	00 
  408b3e:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  408b45:	00 
  408b46:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  408b4d:	00 
  408b4e:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  408b55:	00 
  408b56:	0f 29 84 24 e0 00 00 	movaps %xmm0,0xe0(%rsp)
  408b5d:	00 
  408b5e:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  408b65:	00 
  408b66:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  408b6d:	00 
  408b6e:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  408b75:	00 
  408b76:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  408b7b:	e8 00 bb ff ff       	call   404680 <runtime::[core.odin]::__init_context>
  408b80:	0f 28 04 24          	movaps (%rsp),%xmm0
  408b84:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  408b8b:	00 
  408b8c:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  408b93:	00 
  408b94:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  408b9b:	00 
  408b9c:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  408ba3:	00 
  408ba4:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  408ba9:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  408bae:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  408bb3:	48 8d 7c 24 50       	lea    0x50(%rsp),%rdi
  408bb8:	e8 73 ba ff ff       	call   404630 <runtime::default_context>
  408bbd:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  408bc2:	0f 28 44 24 50       	movaps 0x50(%rsp),%xmm0
  408bc7:	0f 28 4c 24 60       	movaps 0x60(%rsp),%xmm1
  408bcc:	0f 28 54 24 70       	movaps 0x70(%rsp),%xmm2
  408bd1:	0f 28 9c 24 80 00 00 	movaps 0x80(%rsp),%xmm3
  408bd8:	00 
  408bd9:	0f 28 a4 24 90 00 00 	movaps 0x90(%rsp),%xmm4
  408be0:	00 
  408be1:	0f 28 ac 24 a0 00 00 	movaps 0xa0(%rsp),%xmm5
  408be8:	00 
  408be9:	0f 28 b4 24 b0 00 00 	movaps 0xb0(%rsp),%xmm6
  408bf0:	00 
  408bf1:	0f 29 b4 24 20 01 00 	movaps %xmm6,0x120(%rsp)
  408bf8:	00 
  408bf9:	0f 29 ac 24 10 01 00 	movaps %xmm5,0x110(%rsp)
  408c00:	00 
  408c01:	0f 29 a4 24 00 01 00 	movaps %xmm4,0x100(%rsp)
  408c08:	00 
  408c09:	0f 29 9c 24 f0 00 00 	movaps %xmm3,0xf0(%rsp)
  408c10:	00 
  408c11:	0f 29 94 24 e0 00 00 	movaps %xmm2,0xe0(%rsp)
  408c18:	00 
  408c19:	0f 29 8c 24 d0 00 00 	movaps %xmm1,0xd0(%rsp)
  408c20:	00 
  408c21:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  408c28:	00 
  408c29:	48 c7 c0 50 42 41 00 	mov    $0x414250,%rax
  408c30:	48 8b 08             	mov    (%rax),%rcx
  408c33:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  408c38:	48 8b 40 08          	mov    0x8(%rax),%rax
  408c3c:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  408c41:	e8 2a 02 00 00       	call   408e70 <os::heap_allocator>
  408c46:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  408c4b:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  408c50:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  408c55:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  408c5a:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  408c5f:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  408c64:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  408c69:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  408c6e:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  408c73:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  408c78:	41 b8 40 fa 40 00    	mov    $0x40fa40,%r8d
  408c7e:	4c 8d 8c 24 c0 00 00 	lea    0xc0(%rsp),%r9
  408c85:	00 
  408c86:	e8 35 87 ff ff       	call   4013c0 <runtime::delete_slice:proc(array:[]string,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  408c8b:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  408c92:	c3                   	ret
  408c93:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  408c9a:	84 00 00 00 00 00 

0000000000408ca0 <os::[allocators.odin]::temp_allocator_fini>:
  408ca0:	48 83 ec 18          	sub    $0x18,%rsp
  408ca4:	48 c7 44 24 10 02 00 	movq   $0x2,0x10(%rsp)
  408cab:	00 00 
  408cad:	48 c7 44 24 08 ff ff 	movq   $0xffffffffffffffff,0x8(%rsp)
  408cb4:	ff ff 
  408cb6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  408cbb:	48 83 c0 01          	add    $0x1,%rax
  408cbf:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  408cc4:	48 83 f8 02          	cmp    $0x2,%rax
  408cc8:	7d 30                	jge    408cfa <os::[allocators.odin]::temp_allocator_fini+0x5a>
  408cca:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  408ccf:	48 c7 c1 90 ff ff ff 	mov    $0xffffffffffffff90,%rcx
  408cd6:	64 48 8b 3c 25 00 00 	mov    %fs:0x0,%rdi
  408cdd:	00 00 
  408cdf:	48 01 cf             	add    %rcx,%rdi
  408ce2:	48 6b c0 38          	imul   $0x38,%rax,%rax
  408ce6:	48 01 c7             	add    %rax,%rdi
  408ce9:	48 be e0 f9 40 00 00 	movabs $0x40f9e0,%rsi
  408cf0:	00 00 00 
  408cf3:	e8 88 f2 ff ff       	call   407f80 <runtime::arena_destroy>
  408cf8:	eb bc                	jmp    408cb6 <os::[allocators.odin]::temp_allocator_fini+0x16>
  408cfa:	0f 57 c0             	xorps  %xmm0,%xmm0
  408cfd:	48 c7 c0 90 ff ff ff 	mov    $0xffffffffffffff90,%rax
  408d04:	64 0f 11 40 60       	movups %xmm0,%fs:0x60(%rax)
  408d09:	64 0f 11 40 50       	movups %xmm0,%fs:0x50(%rax)
  408d0e:	64 0f 11 40 40       	movups %xmm0,%fs:0x40(%rax)
  408d13:	64 0f 11 40 30       	movups %xmm0,%fs:0x30(%rax)
  408d18:	64 0f 11 40 20       	movups %xmm0,%fs:0x20(%rax)
  408d1d:	64 0f 11 40 10       	movups %xmm0,%fs:0x10(%rax)
  408d22:	64 0f 11 00          	movups %xmm0,%fs:(%rax)
  408d26:	48 83 c4 18          	add    $0x18,%rsp
  408d2a:	c3                   	ret
  408d2b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000408d30 <os::[stat_linux.odin]::_fstat>:
  408d30:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  408d37:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  408d3c:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  408d41:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  408d46:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  408d4b:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  408d50:	4c 8b 44 24 08       	mov    0x8(%rsp),%r8
  408d55:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  408d5a:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  408d5f:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  408d64:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  408d6b:	00 
  408d6c:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  408d73:	00 
  408d74:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  408d7b:	00 
  408d7c:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  408d83:	00 
  408d84:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  408d8b:	00 
  408d8c:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  408d93:	00 
  408d94:	48 8b 12             	mov    (%rdx),%rdx
  408d97:	48 89 94 24 c8 00 00 	mov    %rdx,0xc8(%rsp)
  408d9e:	00 
  408d9f:	48 8b 94 24 c8 00 00 	mov    0xc8(%rsp),%rdx
  408da6:	00 
  408da7:	8b 7a 28             	mov    0x28(%rdx),%edi
  408daa:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  408db1:	00 
  408db2:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  408db9:	00 
  408dba:	48 8b b4 24 b0 00 00 	mov    0xb0(%rsp),%rsi
  408dc1:	00 
  408dc2:	48 8b 94 24 b8 00 00 	mov    0xb8(%rsp),%rdx
  408dc9:	00 
  408dca:	0f 57 c0             	xorps  %xmm0,%xmm0
  408dcd:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  408dd4:	00 
  408dd5:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  408ddc:	00 
  408ddd:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  408de4:	00 
  408de5:	0f 29 44 24 70       	movaps %xmm0,0x70(%rsp)
  408dea:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  408def:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  408df4:	48 8d 4c 24 50       	lea    0x50(%rsp),%rcx
  408df9:	e8 92 00 00 00       	call   408e90 <os::[stat_linux.odin]::_fstat_internal>
  408dfe:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  408e03:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  408e08:	8b 44 24 40          	mov    0x40(%rsp),%eax
  408e0c:	8b 4c 24 44          	mov    0x44(%rsp),%ecx
  408e10:	0f 28 44 24 50       	movaps 0x50(%rsp),%xmm0
  408e15:	0f 28 4c 24 60       	movaps 0x60(%rsp),%xmm1
  408e1a:	0f 28 54 24 70       	movaps 0x70(%rsp),%xmm2
  408e1f:	0f 28 9c 24 80 00 00 	movaps 0x80(%rsp),%xmm3
  408e26:	00 
  408e27:	0f 28 a4 24 90 00 00 	movaps 0x90(%rsp),%xmm4
  408e2e:	00 
  408e2f:	0f 28 ac 24 a0 00 00 	movaps 0xa0(%rsp),%xmm5
  408e36:	00 
  408e37:	0f 11 6a 50          	movups %xmm5,0x50(%rdx)
  408e3b:	0f 11 62 40          	movups %xmm4,0x40(%rdx)
  408e3f:	0f 11 5a 30          	movups %xmm3,0x30(%rdx)
  408e43:	0f 11 52 20          	movups %xmm2,0x20(%rdx)
  408e47:	0f 11 4a 10          	movups %xmm1,0x10(%rdx)
  408e4b:	0f 11 02             	movups %xmm0,(%rdx)
  408e4e:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  408e52:	89 44 24 30          	mov    %eax,0x30(%rsp)
  408e56:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  408e5b:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  408e62:	c3                   	ret
  408e63:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  408e6a:	84 00 00 00 00 00 

0000000000408e70 <os::heap_allocator>:
  408e70:	48 c7 c0 e0 94 40 00 	mov    $0x4094e0,%rax
  408e77:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  408e7c:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  408e83:	00 00 
  408e85:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  408e8a:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  408e8f:	c3                   	ret

0000000000408e90 <os::[stat_linux.odin]::_fstat_internal>:
  408e90:	48 81 ec 98 02 00 00 	sub    $0x298,%rsp
  408e97:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  408e9c:	48 89 4c 24 50       	mov    %rcx,0x50(%rsp)
  408ea1:	89 7c 24 5c          	mov    %edi,0x5c(%rsp)
  408ea5:	48 89 54 24 60       	mov    %rdx,0x60(%rsp)
  408eaa:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  408eaf:	8b 54 24 5c          	mov    0x5c(%rsp),%edx
  408eb3:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  408eb8:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  408ebd:	89 94 24 94 02 00 00 	mov    %edx,0x294(%rsp)
  408ec4:	48 89 8c 24 88 02 00 	mov    %rcx,0x288(%rsp)
  408ecb:	00 
  408ecc:	48 89 84 24 80 02 00 	mov    %rax,0x280(%rsp)
  408ed3:	00 
  408ed4:	48 8b 84 24 80 02 00 	mov    0x280(%rsp),%rax
  408edb:	00 
  408edc:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  408ee1:	48 8b 84 24 88 02 00 	mov    0x288(%rsp),%rax
  408ee8:	00 
  408ee9:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  408eee:	48 8d bc 24 20 02 00 	lea    0x220(%rsp),%rdi
  408ef5:	00 
  408ef6:	31 f6                	xor    %esi,%esi
  408ef8:	ba 60 00 00 00       	mov    $0x60,%edx
  408efd:	e8 3e 81 ff ff       	call   401040 <memset@plt>
  408f02:	48 c7 84 24 18 02 00 	movq   $0x0,0x218(%rsp)
  408f09:	00 00 00 00 00 
  408f0e:	48 8d bc 24 88 01 00 	lea    0x188(%rsp),%rdi
  408f15:	00 
  408f16:	31 f6                	xor    %esi,%esi
  408f18:	ba 90 00 00 00       	mov    $0x90,%edx
  408f1d:	e8 1e 81 ff ff       	call   401040 <memset@plt>
  408f22:	8b 7c 24 5c          	mov    0x5c(%rsp),%edi
  408f26:	48 8d b4 24 88 01 00 	lea    0x188(%rsp),%rsi
  408f2d:	00 
  408f2e:	e8 4d e0 ff ff       	call   406f80 <linux::fstat>
  408f33:	89 84 24 84 01 00 00 	mov    %eax,0x184(%rsp)
  408f3a:	83 bc 24 84 01 00 00 	cmpl   $0x0,0x184(%rsp)
  408f41:	00 
  408f42:	0f 95 c0             	setne  %al
  408f45:	24 01                	and    $0x1,%al
  408f47:	3c 00                	cmp    $0x0,%al
  408f49:	0f 84 a2 00 00 00    	je     408ff1 <os::[stat_linux.odin]::_fstat_internal+0x161>
  408f4f:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  408f54:	8b bc 24 84 01 00 00 	mov    0x184(%rsp),%edi
  408f5b:	e8 70 22 00 00       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  408f60:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  408f65:	48 89 84 24 70 01 00 	mov    %rax,0x170(%rsp)
  408f6c:	00 
  408f6d:	8b 84 24 70 01 00 00 	mov    0x170(%rsp),%eax
  408f74:	8b 8c 24 74 01 00 00 	mov    0x174(%rsp),%ecx
  408f7b:	0f 57 c0             	xorps  %xmm0,%xmm0
  408f7e:	0f 29 84 24 70 02 00 	movaps %xmm0,0x270(%rsp)
  408f85:	00 
  408f86:	0f 29 84 24 60 02 00 	movaps %xmm0,0x260(%rsp)
  408f8d:	00 
  408f8e:	0f 29 84 24 50 02 00 	movaps %xmm0,0x250(%rsp)
  408f95:	00 
  408f96:	0f 29 84 24 40 02 00 	movaps %xmm0,0x240(%rsp)
  408f9d:	00 
  408f9e:	0f 29 84 24 30 02 00 	movaps %xmm0,0x230(%rsp)
  408fa5:	00 
  408fa6:	0f 29 84 24 20 02 00 	movaps %xmm0,0x220(%rsp)
  408fad:	00 
  408fae:	89 8c 24 1c 02 00 00 	mov    %ecx,0x21c(%rsp)
  408fb5:	89 84 24 18 02 00 00 	mov    %eax,0x218(%rsp)
  408fbc:	0f 11 42 50          	movups %xmm0,0x50(%rdx)
  408fc0:	0f 11 42 40          	movups %xmm0,0x40(%rdx)
  408fc4:	0f 11 42 30          	movups %xmm0,0x30(%rdx)
  408fc8:	0f 11 42 20          	movups %xmm0,0x20(%rdx)
  408fcc:	0f 11 42 10          	movups %xmm0,0x10(%rdx)
  408fd0:	0f 11 02             	movups %xmm0,(%rdx)
  408fd3:	89 8c 24 64 01 00 00 	mov    %ecx,0x164(%rsp)
  408fda:	89 84 24 60 01 00 00 	mov    %eax,0x160(%rsp)
  408fe1:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  408fe8:	00 
  408fe9:	48 81 c4 98 02 00 00 	add    $0x298,%rsp
  408ff0:	c3                   	ret
  408ff1:	48 c7 84 24 58 01 00 	movq   $0x1,0x158(%rsp)
  408ff8:	00 01 00 00 00 
  408ffd:	8b 84 24 a0 01 00 00 	mov    0x1a0(%rsp),%eax
  409004:	25 00 f0 00 00       	and    $0xf000,%eax
  409009:	89 44 24 34          	mov    %eax,0x34(%rsp)
  40900d:	3d 00 60 00 00       	cmp    $0x6000,%eax
  409012:	74 0d                	je     409021 <os::[stat_linux.odin]::_fstat_internal+0x191>
  409014:	8b 44 24 34          	mov    0x34(%rsp),%eax
  409018:	3d 00 20 00 00       	cmp    $0x2000,%eax
  40901d:	74 20                	je     40903f <os::[stat_linux.odin]::_fstat_internal+0x1af>
  40901f:	eb 11                	jmp    409032 <os::[stat_linux.odin]::_fstat_internal+0x1a2>
  409021:	48 c7 84 24 58 01 00 	movq   $0x6,0x158(%rsp)
  409028:	00 06 00 00 00 
  40902d:	e9 95 00 00 00       	jmp    4090c7 <os::[stat_linux.odin]::_fstat_internal+0x237>
  409032:	8b 44 24 34          	mov    0x34(%rsp),%eax
  409036:	3d 00 40 00 00       	cmp    $0x4000,%eax
  40903b:	74 1d                	je     40905a <os::[stat_linux.odin]::_fstat_internal+0x1ca>
  40903d:	eb 0e                	jmp    40904d <os::[stat_linux.odin]::_fstat_internal+0x1bd>
  40903f:	48 c7 84 24 58 01 00 	movq   $0x7,0x158(%rsp)
  409046:	00 07 00 00 00 
  40904b:	eb 7a                	jmp    4090c7 <os::[stat_linux.odin]::_fstat_internal+0x237>
  40904d:	8b 44 24 34          	mov    0x34(%rsp),%eax
  409051:	3d 00 10 00 00       	cmp    $0x1000,%eax
  409056:	74 1d                	je     409075 <os::[stat_linux.odin]::_fstat_internal+0x1e5>
  409058:	eb 0e                	jmp    409068 <os::[stat_linux.odin]::_fstat_internal+0x1d8>
  40905a:	48 c7 84 24 58 01 00 	movq   $0x2,0x158(%rsp)
  409061:	00 02 00 00 00 
  409066:	eb 5f                	jmp    4090c7 <os::[stat_linux.odin]::_fstat_internal+0x237>
  409068:	8b 44 24 34          	mov    0x34(%rsp),%eax
  40906c:	3d 00 a0 00 00       	cmp    $0xa000,%eax
  409071:	74 1d                	je     409090 <os::[stat_linux.odin]::_fstat_internal+0x200>
  409073:	eb 0e                	jmp    409083 <os::[stat_linux.odin]::_fstat_internal+0x1f3>
  409075:	48 c7 84 24 58 01 00 	movq   $0x4,0x158(%rsp)
  40907c:	00 04 00 00 00 
  409081:	eb 44                	jmp    4090c7 <os::[stat_linux.odin]::_fstat_internal+0x237>
  409083:	8b 44 24 34          	mov    0x34(%rsp),%eax
  409087:	3d 00 80 00 00       	cmp    $0x8000,%eax
  40908c:	74 1d                	je     4090ab <os::[stat_linux.odin]::_fstat_internal+0x21b>
  40908e:	eb 0e                	jmp    40909e <os::[stat_linux.odin]::_fstat_internal+0x20e>
  409090:	48 c7 84 24 58 01 00 	movq   $0x3,0x158(%rsp)
  409097:	00 03 00 00 00 
  40909c:	eb 29                	jmp    4090c7 <os::[stat_linux.odin]::_fstat_internal+0x237>
  40909e:	8b 44 24 34          	mov    0x34(%rsp),%eax
  4090a2:	3d 00 c0 00 00       	cmp    $0xc000,%eax
  4090a7:	74 12                	je     4090bb <os::[stat_linux.odin]::_fstat_internal+0x22b>
  4090a9:	eb 0e                	jmp    4090b9 <os::[stat_linux.odin]::_fstat_internal+0x229>
  4090ab:	48 c7 84 24 58 01 00 	movq   $0x1,0x158(%rsp)
  4090b2:	00 01 00 00 00 
  4090b7:	eb 0e                	jmp    4090c7 <os::[stat_linux.odin]::_fstat_internal+0x237>
  4090b9:	eb 0c                	jmp    4090c7 <os::[stat_linux.odin]::_fstat_internal+0x237>
  4090bb:	48 c7 84 24 58 01 00 	movq   $0x5,0x158(%rsp)
  4090c2:	00 05 00 00 00 
  4090c7:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  4090cc:	8b 7c 24 5c          	mov    0x5c(%rsp),%edi
  4090d0:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  4090d5:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  4090da:	8b 94 24 a0 01 00 00 	mov    0x1a0(%rsp),%edx
  4090e1:	81 e2 ff 0f 00 00    	and    $0xfff,%edx
  4090e7:	89 94 24 54 01 00 00 	mov    %edx,0x154(%rsp)
  4090ee:	0f 57 c0             	xorps  %xmm0,%xmm0
  4090f1:	0f 29 04 24          	movaps %xmm0,(%rsp)
  4090f5:	0f 29 84 24 40 01 00 	movaps %xmm0,0x140(%rsp)
  4090fc:	00 
  4090fd:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  409104:	00 
  409105:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  40910c:	00 
  40910d:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  409114:	00 
  409115:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40911c:	00 
  40911d:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  409124:	00 
  409125:	0f 29 84 24 40 01 00 	movaps %xmm0,0x140(%rsp)
  40912c:	00 
  40912d:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  409134:	00 
  409135:	0f 29 84 24 20 01 00 	movaps %xmm0,0x120(%rsp)
  40913c:	00 
  40913d:	0f 29 84 24 10 01 00 	movaps %xmm0,0x110(%rsp)
  409144:	00 
  409145:	0f 29 84 24 00 01 00 	movaps %xmm0,0x100(%rsp)
  40914c:	00 
  40914d:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  409154:	00 
  409155:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  40915c:	00 
  40915d:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  409164:	00 
  409165:	48 8b b4 24 e0 00 00 	mov    0xe0(%rsp),%rsi
  40916c:	00 
  40916d:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  409174:	00 
  409175:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  40917c:	00 
  40917d:	48 8d 8c 24 d0 00 00 	lea    0xd0(%rsp),%rcx
  409184:	00 
  409185:	e8 36 12 00 00       	call   40a3c0 <os::[path_linux.odin]::_get_full_path>
  40918a:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  409191:	00 
  409192:	8b 84 24 c0 00 00 00 	mov    0xc0(%rsp),%eax
  409199:	89 44 24 18          	mov    %eax,0x18(%rsp)
  40919d:	8b 84 24 c4 00 00 00 	mov    0xc4(%rsp),%eax
  4091a4:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  4091a8:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  4091af:	00 
  4091b0:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  4091b5:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4091bc:	00 
  4091bd:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  4091c2:	83 bc 24 c4 00 00 00 	cmpl   $0x0,0xc4(%rsp)
  4091c9:	00 
  4091ca:	0f 84 9c 00 00 00    	je     40926c <os::[stat_linux.odin]::_fstat_internal+0x3dc>
  4091d0:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  4091d5:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  4091d9:	8b 4c 24 18          	mov    0x18(%rsp),%ecx
  4091dd:	89 8c 24 18 02 00 00 	mov    %ecx,0x218(%rsp)
  4091e4:	89 84 24 1c 02 00 00 	mov    %eax,0x21c(%rsp)
  4091eb:	8b 84 24 18 02 00 00 	mov    0x218(%rsp),%eax
  4091f2:	8b 8c 24 1c 02 00 00 	mov    0x21c(%rsp),%ecx
  4091f9:	89 8c 24 1c 02 00 00 	mov    %ecx,0x21c(%rsp)
  409200:	89 84 24 18 02 00 00 	mov    %eax,0x218(%rsp)
  409207:	0f 28 84 24 20 02 00 	movaps 0x220(%rsp),%xmm0
  40920e:	00 
  40920f:	0f 28 8c 24 30 02 00 	movaps 0x230(%rsp),%xmm1
  409216:	00 
  409217:	0f 28 94 24 40 02 00 	movaps 0x240(%rsp),%xmm2
  40921e:	00 
  40921f:	0f 28 9c 24 50 02 00 	movaps 0x250(%rsp),%xmm3
  409226:	00 
  409227:	0f 28 a4 24 60 02 00 	movaps 0x260(%rsp),%xmm4
  40922e:	00 
  40922f:	0f 28 ac 24 70 02 00 	movaps 0x270(%rsp),%xmm5
  409236:	00 
  409237:	0f 11 6a 50          	movups %xmm5,0x50(%rdx)
  40923b:	0f 11 62 40          	movups %xmm4,0x40(%rdx)
  40923f:	0f 11 5a 30          	movups %xmm3,0x30(%rdx)
  409243:	0f 11 52 20          	movups %xmm2,0x20(%rdx)
  409247:	0f 11 4a 10          	movups %xmm1,0x10(%rdx)
  40924b:	0f 11 02             	movups %xmm0,(%rdx)
  40924e:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  409255:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  40925c:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  409263:	00 
  409264:	48 81 c4 98 02 00 00 	add    $0x298,%rsp
  40926b:	c3                   	ret
  40926c:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  409271:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  409276:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40927b:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  409282:	00 
  409283:	48 89 84 24 f8 00 00 	mov    %rax,0xf8(%rsp)
  40928a:	00 
  40928b:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  409292:	00 
  409293:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40929a:	00 
  40929b:	48 c7 84 24 18 01 00 	movq   $0x0,0x118(%rsp)
  4092a2:	00 00 00 00 00 
  4092a7:	48 8b 84 24 b8 01 00 	mov    0x1b8(%rsp),%rax
  4092ae:	00 
  4092af:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4092b6:	00 
  4092b7:	8b 84 24 54 01 00 00 	mov    0x154(%rsp),%eax
  4092be:	89 84 24 28 01 00 00 	mov    %eax,0x128(%rsp)
  4092c5:	48 8b 84 24 58 01 00 	mov    0x158(%rsp),%rax
  4092cc:	00 
  4092cd:	48 89 84 24 30 01 00 	mov    %rax,0x130(%rsp)
  4092d4:	00 
  4092d5:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  4092dc:	00 00 00 00 00 
  4092e1:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  4092e8:	00 00 00 00 00 
  4092ed:	48 8b 84 24 e0 01 00 	mov    0x1e0(%rsp),%rax
  4092f4:	00 
  4092f5:	48 8b 94 24 e8 01 00 	mov    0x1e8(%rsp),%rdx
  4092fc:	00 
  4092fd:	48 69 c0 00 ca 9a 3b 	imul   $0x3b9aca00,%rax,%rax
  409304:	48 01 d0             	add    %rdx,%rax
  409307:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  40930e:	00 
  40930f:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409316:	00 
  409317:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40931e:	00 
  40931f:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  409326:	00 00 00 00 00 
  40932b:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  409332:	00 00 00 00 00 
  409337:	48 8b 84 24 d0 01 00 	mov    0x1d0(%rsp),%rax
  40933e:	00 
  40933f:	48 8b 94 24 d8 01 00 	mov    0x1d8(%rsp),%rdx
  409346:	00 
  409347:	48 69 c0 00 ca 9a 3b 	imul   $0x3b9aca00,%rax,%rax
  40934e:	48 01 d0             	add    %rdx,%rax
  409351:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  409358:	00 
  409359:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  409360:	00 
  409361:	48 89 84 24 48 01 00 	mov    %rax,0x148(%rsp)
  409368:	00 
  409369:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  409370:	00 00 00 00 00 
  409375:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  40937c:	00 00 00 00 00 
  409381:	48 8b 84 24 f0 01 00 	mov    0x1f0(%rsp),%rax
  409388:	00 
  409389:	48 8b 94 24 f8 01 00 	mov    0x1f8(%rsp),%rdx
  409390:	00 
  409391:	48 69 c0 00 ca 9a 3b 	imul   $0x3b9aca00,%rax,%rax
  409398:	48 01 d0             	add    %rdx,%rax
  40939b:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  4093a2:	00 
  4093a3:	48 8b 84 24 98 00 00 	mov    0x98(%rsp),%rax
  4093aa:	00 
  4093ab:	48 89 84 24 38 01 00 	mov    %rax,0x138(%rsp)
  4093b2:	00 
  4093b3:	0f 28 84 24 f0 00 00 	movaps 0xf0(%rsp),%xmm0
  4093ba:	00 
  4093bb:	0f 28 8c 24 00 01 00 	movaps 0x100(%rsp),%xmm1
  4093c2:	00 
  4093c3:	0f 28 94 24 10 01 00 	movaps 0x110(%rsp),%xmm2
  4093ca:	00 
  4093cb:	0f 28 9c 24 20 01 00 	movaps 0x120(%rsp),%xmm3
  4093d2:	00 
  4093d3:	0f 28 a4 24 30 01 00 	movaps 0x130(%rsp),%xmm4
  4093da:	00 
  4093db:	0f 28 ac 24 40 01 00 	movaps 0x140(%rsp),%xmm5
  4093e2:	00 
  4093e3:	0f 29 ac 24 70 02 00 	movaps %xmm5,0x270(%rsp)
  4093ea:	00 
  4093eb:	0f 29 a4 24 60 02 00 	movaps %xmm4,0x260(%rsp)
  4093f2:	00 
  4093f3:	0f 29 9c 24 50 02 00 	movaps %xmm3,0x250(%rsp)
  4093fa:	00 
  4093fb:	0f 29 94 24 40 02 00 	movaps %xmm2,0x240(%rsp)
  409402:	00 
  409403:	0f 29 8c 24 30 02 00 	movaps %xmm1,0x230(%rsp)
  40940a:	00 
  40940b:	0f 29 84 24 20 02 00 	movaps %xmm0,0x220(%rsp)
  409412:	00 
  409413:	48 8b 84 24 70 02 00 	mov    0x270(%rsp),%rax
  40941a:	00 
  40941b:	48 89 84 24 68 02 00 	mov    %rax,0x268(%rsp)
  409422:	00 
  409423:	48 8b bc 24 20 02 00 	mov    0x220(%rsp),%rdi
  40942a:	00 
  40942b:	48 8b b4 24 28 02 00 	mov    0x228(%rsp),%rsi
  409432:	00 
  409433:	0f 57 c0             	xorps  %xmm0,%xmm0
  409436:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40943d:	00 
  40943e:	48 8d 94 24 80 00 00 	lea    0x80(%rsp),%rdx
  409445:	00 
  409446:	e8 35 26 00 00       	call   40ba80 <os::split_path>
  40944b:	48 89 d1             	mov    %rdx,%rcx
  40944e:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  409453:	48 89 8c 24 38 02 00 	mov    %rcx,0x238(%rsp)
  40945a:	00 
  40945b:	48 89 84 24 30 02 00 	mov    %rax,0x230(%rsp)
  409462:	00 
  409463:	8b 84 24 18 02 00 00 	mov    0x218(%rsp),%eax
  40946a:	8b 8c 24 1c 02 00 00 	mov    0x21c(%rsp),%ecx
  409471:	89 8c 24 1c 02 00 00 	mov    %ecx,0x21c(%rsp)
  409478:	89 84 24 18 02 00 00 	mov    %eax,0x218(%rsp)
  40947f:	0f 28 84 24 20 02 00 	movaps 0x220(%rsp),%xmm0
  409486:	00 
  409487:	0f 28 8c 24 30 02 00 	movaps 0x230(%rsp),%xmm1
  40948e:	00 
  40948f:	0f 28 94 24 40 02 00 	movaps 0x240(%rsp),%xmm2
  409496:	00 
  409497:	0f 28 9c 24 50 02 00 	movaps 0x250(%rsp),%xmm3
  40949e:	00 
  40949f:	0f 28 a4 24 60 02 00 	movaps 0x260(%rsp),%xmm4
  4094a6:	00 
  4094a7:	0f 28 ac 24 70 02 00 	movaps 0x270(%rsp),%xmm5
  4094ae:	00 
  4094af:	0f 11 6a 50          	movups %xmm5,0x50(%rdx)
  4094b3:	0f 11 62 40          	movups %xmm4,0x40(%rdx)
  4094b7:	0f 11 5a 30          	movups %xmm3,0x30(%rdx)
  4094bb:	0f 11 52 20          	movups %xmm2,0x20(%rdx)
  4094bf:	0f 11 4a 10          	movups %xmm1,0x10(%rdx)
  4094c3:	0f 11 02             	movups %xmm0,(%rdx)
  4094c6:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  4094ca:	89 44 24 70          	mov    %eax,0x70(%rsp)
  4094ce:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  4094d3:	48 81 c4 98 02 00 00 	add    $0x298,%rsp
  4094da:	c3                   	ret
  4094db:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004094e0 <os::heap_allocator_proc>:
  4094e0:	48 81 ec a8 00 00 00 	sub    $0xa8,%rsp
  4094e7:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  4094ec:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  4094f1:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  4094f6:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  4094fb:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  409500:	40 88 f0             	mov    %sil,%al
  409503:	88 44 24 47          	mov    %al,0x47(%rsp)
  409507:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  40950e:	00 
  40950f:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  409514:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  40951b:	00 
  40951c:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  409521:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  409528:	00 
  409529:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40952e:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  409533:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  409538:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40953d:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  409542:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  409547:	8a 44 24 47          	mov    0x47(%rsp),%al
  40954b:	4c 8b 54 24 58       	mov    0x58(%rsp),%r10
  409550:	4c 8b 5c 24 48       	mov    0x48(%rsp),%r11
  409555:	48 89 bc 24 a0 00 00 	mov    %rdi,0xa0(%rsp)
  40955c:	00 
  40955d:	88 84 24 9f 00 00 00 	mov    %al,0x9f(%rsp)
  409564:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  40956b:	00 
  40956c:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  409573:	00 
  409574:	4c 89 84 24 80 00 00 	mov    %r8,0x80(%rsp)
  40957b:	00 
  40957c:	4c 89 4c 24 78       	mov    %r9,0x78(%rsp)
  409581:	0f 57 c0             	xorps  %xmm0,%xmm0
  409584:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  409589:	48 89 e6             	mov    %rsp,%rsi
  40958c:	4c 89 5e 10          	mov    %r11,0x10(%rsi)
  409590:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  409595:	4c 89 5e 08          	mov    %r11,0x8(%rsi)
  409599:	4c 89 16             	mov    %r10,(%rsi)
  40959c:	0f b6 f0             	movzbl %al,%esi
  40959f:	e8 0c bd ff ff       	call   4052b0 <runtime::heap_allocator_proc>
  4095a4:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  4095a9:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  4095ae:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  4095b3:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  4095b7:	48 89 11             	mov    %rdx,(%rcx)
  4095ba:	48 81 c4 a8 00 00 00 	add    $0xa8,%rsp
  4095c1:	c3                   	ret
  4095c2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  4095c9:	1f 84 00 00 00 00 00 

00000000004095d0 <os::[path_linux.odin]::_is_path_separator>:
  4095d0:	40 88 f8             	mov    %dil,%al
  4095d3:	88 44 24 fe          	mov    %al,-0x2(%rsp)
  4095d7:	8a 44 24 fe          	mov    -0x2(%rsp),%al
  4095db:	88 44 24 ff          	mov    %al,-0x1(%rsp)
  4095df:	3c 2f                	cmp    $0x2f,%al
  4095e1:	0f 94 c0             	sete   %al
  4095e4:	24 01                	and    $0x1,%al
  4095e6:	c3                   	ret
  4095e7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4095ee:	00 00 

00000000004095f0 <os::[path_posixfs.odin]::_split_path>:
  4095f0:	48 81 ec 58 01 00 00 	sub    $0x158,%rsp
  4095f7:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  4095fe:	00 
  4095ff:	48 89 94 24 b0 00 00 	mov    %rdx,0xb0(%rsp)
  409606:	00 
  409607:	48 89 b4 24 b8 00 00 	mov    %rsi,0xb8(%rsp)
  40960e:	00 
  40960f:	48 89 bc 24 c0 00 00 	mov    %rdi,0xc0(%rsp)
  409616:	00 
  409617:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  40961e:	00 
  40961f:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  409626:	00 
  409627:	48 89 8c 24 48 01 00 	mov    %rcx,0x148(%rsp)
  40962e:	00 
  40962f:	48 89 84 24 50 01 00 	mov    %rax,0x150(%rsp)
  409636:	00 
  409637:	48 8d bc 24 30 01 00 	lea    0x130(%rsp),%rdi
  40963e:	00 
  40963f:	31 f6                	xor    %esi,%esi
  409641:	ba 10 00 00 00       	mov    $0x10,%edx
  409646:	e8 f5 79 ff ff       	call   401040 <memset@plt>
  40964b:	48 8d bc 24 20 01 00 	lea    0x120(%rsp),%rdi
  409652:	00 
  409653:	31 f6                	xor    %esi,%esi
  409655:	ba 10 00 00 00       	mov    $0x10,%edx
  40965a:	e8 e1 79 ff ff       	call   401040 <memset@plt>
  40965f:	48 8b 84 24 b8 00 00 	mov    0xb8(%rsp),%rax
  409666:	00 
  409667:	48 83 e8 01          	sub    $0x1,%rax
  40966b:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  409672:	00 
  409673:	48 83 bc 24 18 01 00 	cmpq   $0x0,0x118(%rsp)
  40967a:	00 00 
  40967c:	0f 9d c0             	setge  %al
  40967f:	24 01                	and    $0x1,%al
  409681:	3c 00                	cmp    $0x0,%al
  409683:	74 6c                	je     4096f1 <os::[path_posixfs.odin]::_split_path+0x101>
  409685:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  40968c:	00 
  40968d:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  409694:	00 
  409695:	4c 89 84 24 a0 00 00 	mov    %r8,0xa0(%rsp)
  40969c:	00 
  40969d:	bf 98 fa 40 00       	mov    $0x40fa98,%edi
  4096a2:	be 27 00 00 00       	mov    $0x27,%esi
  4096a7:	ba 30 00 00 00       	mov    $0x30,%edx
  4096ac:	b9 29 00 00 00       	mov    $0x29,%ecx
  4096b1:	e8 ca 82 ff ff       	call   401980 <runtime::bounds_check_error>
  4096b6:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  4096bd:	00 
  4096be:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4096c5:	00 
  4096c6:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  4096cd:	00 
  4096ce:	0f b6 3c 08          	movzbl (%rax,%rcx,1),%edi
  4096d2:	e8 f9 fe ff ff       	call   4095d0 <os::[path_linux.odin]::_is_path_separator>
  4096d7:	3c 00                	cmp    $0x0,%al
  4096d9:	75 16                	jne    4096f1 <os::[path_posixfs.odin]::_split_path+0x101>
  4096db:	48 8b 84 24 18 01 00 	mov    0x118(%rsp),%rax
  4096e2:	00 
  4096e3:	48 83 e8 01          	sub    $0x1,%rax
  4096e7:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  4096ee:	00 
  4096ef:	eb 82                	jmp    409673 <os::[path_posixfs.odin]::_split_path+0x83>
  4096f1:	48 83 bc 24 18 01 00 	cmpq   $0x0,0x118(%rsp)
  4096f8:	00 00 
  4096fa:	0f 94 c0             	sete   %al
  4096fd:	24 01                	and    $0x1,%al
  4096ff:	3c 00                	cmp    $0x0,%al
  409701:	0f 84 6e 01 00 00    	je     409875 <os::[path_posixfs.odin]::_split_path+0x285>
  409707:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  40970e:	00 
  40970f:	49 ff c0             	inc    %r8
  409712:	4c 89 44 24 58       	mov    %r8,0x58(%rsp)
  409717:	4c 8b 8c 24 50 01 00 	mov    0x150(%rsp),%r9
  40971e:	00 
  40971f:	bf 98 fa 40 00       	mov    $0x40fa98,%edi
  409724:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  409729:	be 27 00 00 00       	mov    $0x27,%esi
  40972e:	48 89 74 24 68       	mov    %rsi,0x68(%rsp)
  409733:	ba 34 00 00 00       	mov    $0x34,%edx
  409738:	89 54 24 74          	mov    %edx,0x74(%rsp)
  40973c:	b9 0e 00 00 00       	mov    $0xe,%ecx
  409741:	e8 6a 85 ff ff       	call   401cb0 <runtime::slice_expr_error_hi>
  409746:	48 8b 44 24 58       	mov    0x58(%rsp),%rax
  40974b:	48 8b 7c 24 60       	mov    0x60(%rsp),%rdi
  409750:	48 8b 74 24 68       	mov    0x68(%rsp),%rsi
  409755:	8b 54 24 74          	mov    0x74(%rsp),%edx
  409759:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  409760:	00 
  409761:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  409768:	00 
  409769:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  409770:	00 
  409771:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  409778:	00 
  409779:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  409780:	00 
  409781:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  409788:	00 
  409789:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  409790:	00 
  409791:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  409798:	00 
  409799:	4c 89 44 24 78       	mov    %r8,0x78(%rsp)
  40979e:	49 ff c0             	inc    %r8
  4097a1:	4c 89 84 24 88 00 00 	mov    %r8,0x88(%rsp)
  4097a8:	00 
  4097a9:	4c 8b 8c 24 50 01 00 	mov    0x150(%rsp),%r9
  4097b0:	00 
  4097b1:	4c 89 8c 24 80 00 00 	mov    %r9,0x80(%rsp)
  4097b8:	00 
  4097b9:	48 89 e0             	mov    %rsp,%rax
  4097bc:	4c 89 08             	mov    %r9,(%rax)
  4097bf:	b9 1a 00 00 00       	mov    $0x1a,%ecx
  4097c4:	e8 97 85 ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  4097c9:	4c 8b 4c 24 78       	mov    0x78(%rsp),%r9
  4097ce:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  4097d5:	00 
  4097d6:	4c 8b 84 24 88 00 00 	mov    0x88(%rsp),%r8
  4097dd:	00 
  4097de:	48 8b bc 24 90 00 00 	mov    0x90(%rsp),%rdi
  4097e5:	00 
  4097e6:	48 8b b4 24 98 00 00 	mov    0x98(%rsp),%rsi
  4097ed:	00 
  4097ee:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  4097f5:	00 
  4097f6:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  4097fd:	00 
  4097fe:	4a 8d 4c 09 01       	lea    0x1(%rcx,%r9,1),%rcx
  409803:	4c 29 c0             	sub    %r8,%rax
  409806:	48 89 8c 24 f8 00 00 	mov    %rcx,0xf8(%rsp)
  40980d:	00 
  40980e:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  409815:	00 
  409816:	48 8b 84 24 f8 00 00 	mov    0xf8(%rsp),%rax
  40981d:	00 
  40981e:	48 8b 8c 24 00 01 00 	mov    0x100(%rsp),%rcx
  409825:	00 
  409826:	48 89 bc 24 38 01 00 	mov    %rdi,0x138(%rsp)
  40982d:	00 
  40982e:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  409835:	00 
  409836:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  40983d:	00 
  40983e:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  409845:	00 
  409846:	48 89 7a 08          	mov    %rdi,0x8(%rdx)
  40984a:	48 89 32             	mov    %rsi,(%rdx)
  40984d:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  409854:	00 
  409855:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  40985c:	00 
  40985d:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  409864:	00 
  409865:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  40986c:	00 
  40986d:	48 81 c4 58 01 00 00 	add    $0x158,%rsp
  409874:	c3                   	ret
  409875:	48 83 bc 24 18 01 00 	cmpq   $0x0,0x118(%rsp)
  40987c:	00 00 
  40987e:	0f 9f c0             	setg   %al
  409881:	24 01                	and    $0x1,%al
  409883:	3c 00                	cmp    $0x0,%al
  409885:	0f 84 53 01 00 00    	je     4099de <os::[path_posixfs.odin]::_split_path+0x3ee>
  40988b:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  409892:	00 
  409893:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  409898:	4c 8b 8c 24 50 01 00 	mov    0x150(%rsp),%r9
  40989f:	00 
  4098a0:	bf 98 fa 40 00       	mov    $0x40fa98,%edi
  4098a5:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  4098aa:	be 27 00 00 00       	mov    $0x27,%esi
  4098af:	48 89 74 24 20       	mov    %rsi,0x20(%rsp)
  4098b4:	ba 36 00 00 00       	mov    $0x36,%edx
  4098b9:	89 54 24 2c          	mov    %edx,0x2c(%rsp)
  4098bd:	b9 0e 00 00 00       	mov    $0xe,%ecx
  4098c2:	e8 e9 83 ff ff       	call   401cb0 <runtime::slice_expr_error_hi>
  4098c7:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  4098cc:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  4098d1:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  4098d6:	8b 54 24 2c          	mov    0x2c(%rsp),%edx
  4098da:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  4098e1:	00 
  4098e2:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  4098e9:	00 
  4098ea:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  4098f1:	00 
  4098f2:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  4098f9:	00 
  4098fa:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  4098ff:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  409906:	00 
  409907:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40990c:	4c 8b 84 24 18 01 00 	mov    0x118(%rsp),%r8
  409913:	00 
  409914:	4c 89 44 24 30       	mov    %r8,0x30(%rsp)
  409919:	49 ff c0             	inc    %r8
  40991c:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  409921:	4c 8b 8c 24 50 01 00 	mov    0x150(%rsp),%r9
  409928:	00 
  409929:	4c 89 4c 24 38       	mov    %r9,0x38(%rsp)
  40992e:	48 89 e0             	mov    %rsp,%rax
  409931:	4c 89 08             	mov    %r9,(%rax)
  409934:	b9 18 00 00 00       	mov    $0x18,%ecx
  409939:	e8 22 84 ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  40993e:	4c 8b 4c 24 30       	mov    0x30(%rsp),%r9
  409943:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  409948:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  40994d:	48 8b 7c 24 48       	mov    0x48(%rsp),%rdi
  409952:	48 8b 74 24 50       	mov    0x50(%rsp),%rsi
  409957:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  40995e:	00 
  40995f:	48 8b 8c 24 48 01 00 	mov    0x148(%rsp),%rcx
  409966:	00 
  409967:	4a 8d 4c 09 01       	lea    0x1(%rcx,%r9,1),%rcx
  40996c:	4c 29 c0             	sub    %r8,%rax
  40996f:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  409976:	00 
  409977:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  40997e:	00 
  40997f:	48 8b 84 24 c8 00 00 	mov    0xc8(%rsp),%rax
  409986:	00 
  409987:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  40998e:	00 
  40998f:	48 89 bc 24 38 01 00 	mov    %rdi,0x138(%rsp)
  409996:	00 
  409997:	48 89 b4 24 30 01 00 	mov    %rsi,0x130(%rsp)
  40999e:	00 
  40999f:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  4099a6:	00 
  4099a7:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  4099ae:	00 
  4099af:	48 89 7a 08          	mov    %rdi,0x8(%rdx)
  4099b3:	48 89 32             	mov    %rsi,(%rdx)
  4099b6:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  4099bd:	00 
  4099be:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  4099c5:	00 
  4099c6:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  4099cd:	00 
  4099ce:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  4099d5:	00 
  4099d6:	48 81 c4 58 01 00 00 	add    $0x158,%rsp
  4099dd:	c3                   	ret
  4099de:	eb 00                	jmp    4099e0 <os::[path_posixfs.odin]::_split_path+0x3f0>
  4099e0:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  4099e7:	00 
  4099e8:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  4099ef:	00 
  4099f0:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  4099f7:	00 
  4099f8:	0f 57 c0             	xorps  %xmm0,%xmm0
  4099fb:	0f 29 84 24 30 01 00 	movaps %xmm0,0x130(%rsp)
  409a02:	00 
  409a03:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  409a0a:	00 
  409a0b:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  409a12:	00 
  409a13:	0f 11 02             	movups %xmm0,(%rdx)
  409a16:	48 89 8c 24 f0 00 00 	mov    %rcx,0xf0(%rsp)
  409a1d:	00 
  409a1e:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  409a25:	00 
  409a26:	48 8b 84 24 e8 00 00 	mov    0xe8(%rsp),%rax
  409a2d:	00 
  409a2e:	48 8b 94 24 f0 00 00 	mov    0xf0(%rsp),%rdx
  409a35:	00 
  409a36:	48 81 c4 58 01 00 00 	add    $0x158,%rsp
  409a3d:	c3                   	ret
  409a3e:	66 90                	xchg   %ax,%ax

0000000000409a40 <os::[file_stream.odin]::file_stream_fstat_utility>:
  409a40:	55                   	push   %rbp
  409a41:	41 57                	push   %r15
  409a43:	41 56                	push   %r14
  409a45:	41 55                	push   %r13
  409a47:	41 54                	push   %r12
  409a49:	53                   	push   %rbx
  409a4a:	48 81 ec 88 01 00 00 	sub    $0x188,%rsp
  409a51:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  409a56:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  409a5b:	4c 89 44 24 40       	mov    %r8,0x40(%rsp)
  409a60:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  409a65:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  409a6a:	48 89 74 24 58       	mov    %rsi,0x58(%rsp)
  409a6f:	48 8b 74 24 50       	mov    0x50(%rsp),%rsi
  409a74:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  409a79:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  409a7e:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  409a83:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  409a88:	48 89 bc 24 80 01 00 	mov    %rdi,0x180(%rsp)
  409a8f:	00 
  409a90:	48 89 b4 24 78 01 00 	mov    %rsi,0x178(%rsp)
  409a97:	00 
  409a98:	48 89 94 24 70 01 00 	mov    %rdx,0x170(%rsp)
  409a9f:	00 
  409aa0:	48 89 8c 24 68 01 00 	mov    %rcx,0x168(%rsp)
  409aa7:	00 
  409aa8:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  409aaf:	00 
  409ab0:	48 8b 84 24 60 01 00 	mov    0x160(%rsp),%rax
  409ab7:	00 
  409ab8:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  409abd:	48 8b 84 24 68 01 00 	mov    0x168(%rsp),%rax
  409ac4:	00 
  409ac5:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  409aca:	48 c7 84 24 58 01 00 	movq   $0x0,0x158(%rsp)
  409ad1:	00 00 00 00 00 
  409ad6:	48 8d bc 24 f0 00 00 	lea    0xf0(%rsp),%rdi
  409add:	00 
  409ade:	31 f6                	xor    %esi,%esi
  409ae0:	ba 60 00 00 00       	mov    $0x60,%edx
  409ae5:	e8 56 75 ff ff       	call   401040 <memset@plt>
  409aea:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  409aef:	48 83 f8 60          	cmp    $0x60,%rax
  409af3:	0f 9d c0             	setge  %al
  409af6:	24 01                	and    $0x1,%al
  409af8:	3c 00                	cmp    $0x0,%al
  409afa:	0f 84 c5 01 00 00    	je     409cc5 <os::[file_stream.odin]::file_stream_fstat_utility+0x285>
  409b00:	4c 8b 44 24 30       	mov    0x30(%rsp),%r8
  409b05:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  409b0a:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  409b0f:	48 8b bc 24 80 01 00 	mov    0x180(%rsp),%rdi
  409b16:	00 
  409b17:	48 89 8c 24 e0 00 00 	mov    %rcx,0xe0(%rsp)
  409b1e:	00 
  409b1f:	48 89 84 24 e8 00 00 	mov    %rax,0xe8(%rsp)
  409b26:	00 
  409b27:	48 8b b4 24 e0 00 00 	mov    0xe0(%rsp),%rsi
  409b2e:	00 
  409b2f:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  409b36:	00 
  409b37:	0f 57 c0             	xorps  %xmm0,%xmm0
  409b3a:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  409b41:	00 
  409b42:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  409b49:	00 
  409b4a:	0f 29 84 24 b0 00 00 	movaps %xmm0,0xb0(%rsp)
  409b51:	00 
  409b52:	0f 29 84 24 a0 00 00 	movaps %xmm0,0xa0(%rsp)
  409b59:	00 
  409b5a:	0f 29 84 24 90 00 00 	movaps %xmm0,0x90(%rsp)
  409b61:	00 
  409b62:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  409b69:	00 
  409b6a:	48 8d 8c 24 80 00 00 	lea    0x80(%rsp),%rcx
  409b71:	00 
  409b72:	e8 b9 f1 ff ff       	call   408d30 <os::[stat_linux.odin]::_fstat>
  409b77:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  409b7c:	8b 44 24 70          	mov    0x70(%rsp),%eax
  409b80:	89 44 24 1c          	mov    %eax,0x1c(%rsp)
  409b84:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  409b88:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  409b8f:	00 
  409b90:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  409b97:	00 
  409b98:	4c 8b 84 24 90 00 00 	mov    0x90(%rsp),%r8
  409b9f:	00 
  409ba0:	4c 8b 8c 24 98 00 00 	mov    0x98(%rsp),%r9
  409ba7:	00 
  409ba8:	4c 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%r10
  409baf:	00 
  409bb0:	44 8b 9c 24 b8 00 00 	mov    0xb8(%rsp),%r11d
  409bb7:	00 
  409bb8:	8a 9c 24 bc 00 00 00 	mov    0xbc(%rsp),%bl
  409bbf:	40 8a ac 24 bd 00 00 	mov    0xbd(%rsp),%bpl
  409bc6:	00 
  409bc7:	44 8a b4 24 be 00 00 	mov    0xbe(%rsp),%r14b
  409bce:	00 
  409bcf:	44 8a bc 24 bf 00 00 	mov    0xbf(%rsp),%r15b
  409bd6:	00 
  409bd7:	4c 8b a4 24 c0 00 00 	mov    0xc0(%rsp),%r12
  409bde:	00 
  409bdf:	4c 8b ac 24 c8 00 00 	mov    0xc8(%rsp),%r13
  409be6:	00 
  409be7:	48 8b bc 24 d0 00 00 	mov    0xd0(%rsp),%rdi
  409bee:	00 
  409bef:	48 8b 84 24 d8 00 00 	mov    0xd8(%rsp),%rax
  409bf6:	00 
  409bf7:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  409bfc:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  409c03:	00 
  409c04:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  409c09:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  409c10:	00 
  409c11:	48 89 84 24 18 01 00 	mov    %rax,0x118(%rsp)
  409c18:	00 
  409c19:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  409c1e:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  409c25:	00 
  409c26:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  409c2b:	48 89 84 24 48 01 00 	mov    %rax,0x148(%rsp)
  409c32:	00 
  409c33:	8b 44 24 1c          	mov    0x1c(%rsp),%eax
  409c37:	48 89 bc 24 40 01 00 	mov    %rdi,0x140(%rsp)
  409c3e:	00 
  409c3f:	48 8b 7c 24 58       	mov    0x58(%rsp),%rdi
  409c44:	4c 89 ac 24 38 01 00 	mov    %r13,0x138(%rsp)
  409c4b:	00 
  409c4c:	4c 89 a4 24 30 01 00 	mov    %r12,0x130(%rsp)
  409c53:	00 
  409c54:	44 88 bc 24 2f 01 00 	mov    %r15b,0x12f(%rsp)
  409c5b:	00 
  409c5c:	44 88 b4 24 2e 01 00 	mov    %r14b,0x12e(%rsp)
  409c63:	00 
  409c64:	40 88 ac 24 2d 01 00 	mov    %bpl,0x12d(%rsp)
  409c6b:	00 
  409c6c:	88 9c 24 2c 01 00 00 	mov    %bl,0x12c(%rsp)
  409c73:	44 89 9c 24 28 01 00 	mov    %r11d,0x128(%rsp)
  409c7a:	00 
  409c7b:	4c 89 94 24 20 01 00 	mov    %r10,0x120(%rsp)
  409c82:	00 
  409c83:	4c 89 8c 24 08 01 00 	mov    %r9,0x108(%rsp)
  409c8a:	00 
  409c8b:	4c 89 84 24 00 01 00 	mov    %r8,0x100(%rsp)
  409c92:	00 
  409c93:	48 89 b4 24 f8 00 00 	mov    %rsi,0xf8(%rsp)
  409c9a:	00 
  409c9b:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  409ca2:	00 
  409ca3:	89 8c 24 5c 01 00 00 	mov    %ecx,0x15c(%rsp)
  409caa:	89 84 24 58 01 00 00 	mov    %eax,0x158(%rsp)
  409cb1:	48 8d b4 24 f0 00 00 	lea    0xf0(%rsp),%rsi
  409cb8:	00 
  409cb9:	ba 60 00 00 00       	mov    $0x60,%edx
  409cbe:	e8 dd 28 00 00       	call   40c5a0 <runtime::mem_copy_non_overlapping>
  409cc3:	eb 49                	jmp    409d0e <os::[file_stream.odin]::file_stream_fstat_utility+0x2ce>
  409cc5:	48 c7 44 24 68 00 00 	movq   $0x0,0x68(%rsp)
  409ccc:	00 00 
  409cce:	31 c0                	xor    %eax,%eax
  409cd0:	a8 01                	test   $0x1,%al
  409cd2:	75 02                	jne    409cd6 <os::[file_stream.odin]::file_stream_fstat_utility+0x296>
  409cd4:	eb 12                	jmp    409ce8 <os::[file_stream.odin]::file_stream_fstat_utility+0x2a8>
  409cd6:	c7 44 24 6c 00 00 00 	movl   $0x0,0x6c(%rsp)
  409cdd:	00 
  409cde:	c7 44 24 68 00 00 00 	movl   $0x0,0x68(%rsp)
  409ce5:	00 
  409ce6:	eb 10                	jmp    409cf8 <os::[file_stream.odin]::file_stream_fstat_utility+0x2b8>
  409ce8:	c7 44 24 68 05 00 00 	movl   $0x5,0x68(%rsp)
  409cef:	00 
  409cf0:	c7 44 24 6c 02 00 00 	movl   $0x2,0x6c(%rsp)
  409cf7:	00 
  409cf8:	8b 44 24 68          	mov    0x68(%rsp),%eax
  409cfc:	8b 4c 24 6c          	mov    0x6c(%rsp),%ecx
  409d00:	89 8c 24 5c 01 00 00 	mov    %ecx,0x15c(%rsp)
  409d07:	89 84 24 58 01 00 00 	mov    %eax,0x158(%rsp)
  409d0e:	8b 84 24 58 01 00 00 	mov    0x158(%rsp),%eax
  409d15:	8b 8c 24 5c 01 00 00 	mov    0x15c(%rsp),%ecx
  409d1c:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  409d20:	89 44 24 60          	mov    %eax,0x60(%rsp)
  409d24:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  409d29:	48 81 c4 88 01 00 00 	add    $0x188,%rsp
  409d30:	5b                   	pop    %rbx
  409d31:	41 5c                	pop    %r12
  409d33:	41 5d                	pop    %r13
  409d35:	41 5e                	pop    %r14
  409d37:	41 5f                	pop    %r15
  409d39:	5d                   	pop    %rbp
  409d3a:	c3                   	ret
  409d3b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000409d40 <os::[file_linux.odin]::_destroy>:
  409d40:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  409d47:	48 89 7c 24 10       	mov    %rdi,0x10(%rsp)
  409d4c:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  409d51:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  409d56:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  409d5d:	00 
  409d5e:	48 83 f8 00          	cmp    $0x0,%rax
  409d62:	0f 94 c0             	sete   %al
  409d65:	24 01                	and    $0x1,%al
  409d67:	3c 00                	cmp    $0x0,%al
  409d69:	74 26                	je     409d91 <os::[file_linux.odin]::_destroy+0x51>
  409d6b:	c7 84 24 d4 00 00 00 	movl   $0x0,0xd4(%rsp)
  409d72:	00 00 00 00 
  409d76:	c7 84 24 d0 00 00 00 	movl   $0x0,0xd0(%rsp)
  409d7d:	00 00 00 00 
  409d81:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  409d88:	00 
  409d89:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  409d90:	c3                   	ret
  409d91:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  409d96:	48 8b 8c 24 e0 00 00 	mov    0xe0(%rsp),%rcx
  409d9d:	00 
  409d9e:	48 8b 41 30          	mov    0x30(%rcx),%rax
  409da2:	48 8b 49 38          	mov    0x38(%rcx),%rcx
  409da6:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  409dad:	00 
  409dae:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  409db5:	00 
  409db6:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  409dbd:	00 
  409dbe:	48 8b 78 18          	mov    0x18(%rax),%rdi
  409dc2:	48 8b 70 20          	mov    0x20(%rax),%rsi
  409dc6:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  409dcd:	00 
  409dce:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  409dd5:	00 
  409dd6:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  409ddd:	00 
  409dde:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  409de5:	00 
  409de6:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  409ded:	00 
  409dee:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  409df5:	00 
  409df6:	41 b8 f0 fa 40 00    	mov    $0x40faf0,%r8d
  409dfc:	e8 bf 74 ff ff       	call   4012c0 <runtime::delete_string>
  409e01:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  409e06:	88 84 24 af 00 00 00 	mov    %al,0xaf(%rsp)
  409e0d:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  409e14:	00 
  409e15:	48 8b 78 40          	mov    0x40(%rax),%rdi
  409e19:	48 8b 70 48          	mov    0x48(%rax),%rsi
  409e1d:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  409e24:	00 
  409e25:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  409e2c:	00 
  409e2d:	48 89 8c 24 98 00 00 	mov    %rcx,0x98(%rsp)
  409e34:	00 
  409e35:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  409e3c:	00 
  409e3d:	48 8b 94 24 90 00 00 	mov    0x90(%rsp),%rdx
  409e44:	00 
  409e45:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  409e4c:	00 
  409e4d:	41 b8 20 fb 40 00    	mov    $0x40fb20,%r8d
  409e53:	e8 e8 74 ff ff       	call   401340 <runtime::delete_slice:proc(array:[]u8,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  409e58:	48 8b 7c 24 10       	mov    0x10(%rsp),%rdi
  409e5d:	4c 8b 44 24 18       	mov    0x18(%rsp),%r8
  409e62:	88 84 24 8f 00 00 00 	mov    %al,0x8f(%rsp)
  409e69:	48 8b 84 24 c0 00 00 	mov    0xc0(%rsp),%rax
  409e70:	00 
  409e71:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  409e78:	00 
  409e79:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  409e7e:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  409e83:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  409e88:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  409e8d:	b9 50 fb 40 00       	mov    $0x40fb50,%ecx
  409e92:	e8 d9 29 00 00       	call   40c870 <runtime::mem_free>
  409e97:	88 44 24 6f          	mov    %al,0x6f(%rsp)
  409e9b:	8a 84 24 af 00 00 00 	mov    0xaf(%rsp),%al
  409ea2:	88 44 24 0f          	mov    %al,0xf(%rsp)
  409ea6:	3c 00                	cmp    $0x0,%al
  409ea8:	74 50                	je     409efa <os::[file_linux.odin]::_destroy+0x1ba>
  409eaa:	8a 44 24 0f          	mov    0xf(%rsp),%al
  409eae:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  409eb5:	00 00 
  409eb7:	3c 00                	cmp    $0x0,%al
  409eb9:	75 12                	jne    409ecd <os::[file_linux.odin]::_destroy+0x18d>
  409ebb:	c7 44 24 64 00 00 00 	movl   $0x0,0x64(%rsp)
  409ec2:	00 
  409ec3:	c7 44 24 60 00 00 00 	movl   $0x0,0x60(%rsp)
  409eca:	00 
  409ecb:	eb 10                	jmp    409edd <os::[file_linux.odin]::_destroy+0x19d>
  409ecd:	8a 44 24 0f          	mov    0xf(%rsp),%al
  409ed1:	88 44 24 60          	mov    %al,0x60(%rsp)
  409ed5:	c7 44 24 64 03 00 00 	movl   $0x3,0x64(%rsp)
  409edc:	00 
  409edd:	8b 44 24 60          	mov    0x60(%rsp),%eax
  409ee1:	8b 4c 24 64          	mov    0x64(%rsp),%ecx
  409ee5:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  409ee9:	89 44 24 50          	mov    %eax,0x50(%rsp)
  409eed:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  409ef2:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  409ef9:	c3                   	ret
  409efa:	8a 84 24 8f 00 00 00 	mov    0x8f(%rsp),%al
  409f01:	88 44 24 0e          	mov    %al,0xe(%rsp)
  409f05:	3c 00                	cmp    $0x0,%al
  409f07:	74 50                	je     409f59 <os::[file_linux.odin]::_destroy+0x219>
  409f09:	8a 44 24 0e          	mov    0xe(%rsp),%al
  409f0d:	48 c7 44 24 48 00 00 	movq   $0x0,0x48(%rsp)
  409f14:	00 00 
  409f16:	3c 00                	cmp    $0x0,%al
  409f18:	75 12                	jne    409f2c <os::[file_linux.odin]::_destroy+0x1ec>
  409f1a:	c7 44 24 4c 00 00 00 	movl   $0x0,0x4c(%rsp)
  409f21:	00 
  409f22:	c7 44 24 48 00 00 00 	movl   $0x0,0x48(%rsp)
  409f29:	00 
  409f2a:	eb 10                	jmp    409f3c <os::[file_linux.odin]::_destroy+0x1fc>
  409f2c:	8a 44 24 0e          	mov    0xe(%rsp),%al
  409f30:	88 44 24 48          	mov    %al,0x48(%rsp)
  409f34:	c7 44 24 4c 03 00 00 	movl   $0x3,0x4c(%rsp)
  409f3b:	00 
  409f3c:	8b 44 24 48          	mov    0x48(%rsp),%eax
  409f40:	8b 4c 24 4c          	mov    0x4c(%rsp),%ecx
  409f44:	89 4c 24 44          	mov    %ecx,0x44(%rsp)
  409f48:	89 44 24 40          	mov    %eax,0x40(%rsp)
  409f4c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  409f51:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  409f58:	c3                   	ret
  409f59:	8a 44 24 6f          	mov    0x6f(%rsp),%al
  409f5d:	88 44 24 0d          	mov    %al,0xd(%rsp)
  409f61:	3c 00                	cmp    $0x0,%al
  409f63:	74 50                	je     409fb5 <os::[file_linux.odin]::_destroy+0x275>
  409f65:	8a 44 24 0d          	mov    0xd(%rsp),%al
  409f69:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  409f70:	00 00 
  409f72:	3c 00                	cmp    $0x0,%al
  409f74:	75 12                	jne    409f88 <os::[file_linux.odin]::_destroy+0x248>
  409f76:	c7 44 24 3c 00 00 00 	movl   $0x0,0x3c(%rsp)
  409f7d:	00 
  409f7e:	c7 44 24 38 00 00 00 	movl   $0x0,0x38(%rsp)
  409f85:	00 
  409f86:	eb 10                	jmp    409f98 <os::[file_linux.odin]::_destroy+0x258>
  409f88:	8a 44 24 0d          	mov    0xd(%rsp),%al
  409f8c:	88 44 24 38          	mov    %al,0x38(%rsp)
  409f90:	c7 44 24 3c 03 00 00 	movl   $0x3,0x3c(%rsp)
  409f97:	00 
  409f98:	8b 44 24 38          	mov    0x38(%rsp),%eax
  409f9c:	8b 4c 24 3c          	mov    0x3c(%rsp),%ecx
  409fa0:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  409fa4:	89 44 24 30          	mov    %eax,0x30(%rsp)
  409fa8:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  409fad:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  409fb4:	c3                   	ret
  409fb5:	c7 44 24 24 00 00 00 	movl   $0x0,0x24(%rsp)
  409fbc:	00 
  409fbd:	c7 44 24 20 00 00 00 	movl   $0x0,0x20(%rsp)
  409fc4:	00 
  409fc5:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  409fca:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  409fd1:	c3                   	ret
  409fd2:	66 66 66 66 66 2e 0f 	data16 data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  409fd9:	1f 84 00 00 00 00 00 

0000000000409fe0 <os::[file_linux.odin]::_close>:
  409fe0:	48 83 ec 78          	sub    $0x78,%rsp
  409fe4:	48 89 3c 24          	mov    %rdi,(%rsp)
  409fe8:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  409fed:	48 8b 04 24          	mov    (%rsp),%rax
  409ff1:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  409ff6:	48 83 f8 00          	cmp    $0x0,%rax
  409ffa:	0f 94 c0             	sete   %al
  409ffd:	24 01                	and    $0x1,%al
  409fff:	3c 00                	cmp    $0x0,%al
  40a001:	74 1a                	je     40a01d <os::[file_linux.odin]::_close+0x3d>
  40a003:	c7 44 24 64 00 00 00 	movl   $0x0,0x64(%rsp)
  40a00a:	00 
  40a00b:	c7 44 24 60 00 00 00 	movl   $0x0,0x60(%rsp)
  40a012:	00 
  40a013:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40a018:	48 83 c4 78          	add    $0x78,%rsp
  40a01c:	c3                   	ret
  40a01d:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40a022:	8b 78 28             	mov    0x28(%rax),%edi
  40a025:	e8 a6 cd ff ff       	call   406dd0 <linux::close>
  40a02a:	89 44 24 5c          	mov    %eax,0x5c(%rsp)
  40a02e:	83 7c 24 5c 09       	cmpl   $0x9,0x5c(%rsp)
  40a033:	0f 94 c0             	sete   %al
  40a036:	24 01                	and    $0x1,%al
  40a038:	3c 00                	cmp    $0x0,%al
  40a03a:	74 2d                	je     40a069 <os::[file_linux.odin]::_close+0x89>
  40a03c:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40a041:	8b 7c 24 5c          	mov    0x5c(%rsp),%edi
  40a045:	e8 86 11 00 00       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40a04a:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40a04f:	8b 44 24 50          	mov    0x50(%rsp),%eax
  40a053:	8b 4c 24 54          	mov    0x54(%rsp),%ecx
  40a057:	89 4c 24 44          	mov    %ecx,0x44(%rsp)
  40a05b:	89 44 24 40          	mov    %eax,0x40(%rsp)
  40a05f:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40a064:	48 83 c4 78          	add    $0x78,%rsp
  40a068:	c3                   	ret
  40a069:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40a06e:	48 8b 3c 24          	mov    (%rsp),%rdi
  40a072:	e8 c9 fc ff ff       	call   409d40 <os::[file_linux.odin]::_destroy>
  40a077:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40a07c:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40a081:	8b 7c 24 5c          	mov    0x5c(%rsp),%edi
  40a085:	e8 46 11 00 00       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40a08a:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40a08f:	8b 44 24 20          	mov    0x20(%rsp),%eax
  40a093:	8b 4c 24 24          	mov    0x24(%rsp),%ecx
  40a097:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  40a09b:	89 44 24 10          	mov    %eax,0x10(%rsp)
  40a09f:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40a0a4:	48 83 c4 78          	add    $0x78,%rsp
  40a0a8:	c3                   	ret
  40a0a9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

000000000040a0b0 <os::[file_linux.odin]::_seek>:
  40a0b0:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  40a0b7:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40a0bc:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40a0c1:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  40a0c6:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40a0cb:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40a0d0:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40a0d5:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40a0da:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40a0df:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  40a0e6:	00 
  40a0e7:	48 89 8c 24 c8 00 00 	mov    %rcx,0xc8(%rsp)
  40a0ee:	00 
  40a0ef:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  40a0f6:	00 
  40a0f7:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  40a0fe:	00 00 00 00 00 
  40a103:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  40a10a:	00 00 00 00 00 
  40a10f:	48 83 e8 02          	sub    $0x2,%rax
  40a113:	77 07                	ja     40a11c <os::[file_linux.odin]::_seek+0x6c>
  40a115:	eb 00                	jmp    40a117 <os::[file_linux.odin]::_seek+0x67>
  40a117:	e9 e4 00 00 00       	jmp    40a200 <os::[file_linux.odin]::_seek+0x150>
  40a11c:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  40a123:	00 00 00 00 00 
  40a128:	31 c0                	xor    %eax,%eax
  40a12a:	a8 01                	test   $0x1,%al
  40a12c:	75 02                	jne    40a130 <os::[file_linux.odin]::_seek+0x80>
  40a12e:	eb 18                	jmp    40a148 <os::[file_linux.odin]::_seek+0x98>
  40a130:	c7 84 24 ac 00 00 00 	movl   $0x0,0xac(%rsp)
  40a137:	00 00 00 00 
  40a13b:	c7 84 24 a8 00 00 00 	movl   $0x0,0xa8(%rsp)
  40a142:	00 00 00 00 
  40a146:	eb 16                	jmp    40a15e <os::[file_linux.odin]::_seek+0xae>
  40a148:	c7 84 24 a8 00 00 00 	movl   $0x7,0xa8(%rsp)
  40a14f:	07 00 00 00 
  40a153:	c7 84 24 ac 00 00 00 	movl   $0x2,0xac(%rsp)
  40a15a:	02 00 00 00 
  40a15e:	8b 84 24 a8 00 00 00 	mov    0xa8(%rsp),%eax
  40a165:	8b 8c 24 ac 00 00 00 	mov    0xac(%rsp),%ecx
  40a16c:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  40a173:	00 00 00 00 00 
  40a178:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  40a17f:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  40a186:	48 c7 84 24 a0 00 00 	movq   $0x0,0xa0(%rsp)
  40a18d:	00 00 00 00 00 
  40a192:	31 c0                	xor    %eax,%eax
  40a194:	a8 01                	test   $0x1,%al
  40a196:	75 02                	jne    40a19a <os::[file_linux.odin]::_seek+0xea>
  40a198:	eb 18                	jmp    40a1b2 <os::[file_linux.odin]::_seek+0x102>
  40a19a:	c7 84 24 a4 00 00 00 	movl   $0x0,0xa4(%rsp)
  40a1a1:	00 00 00 00 
  40a1a5:	c7 84 24 a0 00 00 00 	movl   $0x0,0xa0(%rsp)
  40a1ac:	00 00 00 00 
  40a1b0:	eb 16                	jmp    40a1c8 <os::[file_linux.odin]::_seek+0x118>
  40a1b2:	c7 84 24 a0 00 00 00 	movl   $0x7,0xa0(%rsp)
  40a1b9:	07 00 00 00 
  40a1bd:	c7 84 24 a4 00 00 00 	movl   $0x2,0xa4(%rsp)
  40a1c4:	02 00 00 00 
  40a1c8:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40a1cd:	8b 84 24 a0 00 00 00 	mov    0xa0(%rsp),%eax
  40a1d4:	8b 8c 24 a4 00 00 00 	mov    0xa4(%rsp),%ecx
  40a1db:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40a1e2:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  40a1e9:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  40a1f0:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40a1f7:	00 
  40a1f8:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40a1ff:	c3                   	ret
  40a200:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40a205:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40a20a:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  40a211:	00 
  40a212:	8b 79 28             	mov    0x28(%rcx),%edi
  40a215:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40a21c:	00 00 00 00 00 
  40a221:	89 c2                	mov    %eax,%edx
  40a223:	48 8d 8c 24 88 00 00 	lea    0x88(%rsp),%rcx
  40a22a:	00 
  40a22b:	e8 60 ce ff ff       	call   407090 <linux::lseek>
  40a230:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40a237:	00 
  40a238:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  40a23f:	00 
  40a240:	89 44 24 7c          	mov    %eax,0x7c(%rsp)
  40a244:	8b 44 24 7c          	mov    0x7c(%rsp),%eax
  40a248:	89 44 24 04          	mov    %eax,0x4(%rsp)
  40a24c:	85 c0                	test   %eax,%eax
  40a24e:	0f 84 c2 00 00 00    	je     40a316 <os::[file_linux.odin]::_seek+0x266>
  40a254:	eb 00                	jmp    40a256 <os::[file_linux.odin]::_seek+0x1a6>
  40a256:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40a25a:	83 e8 16             	sub    $0x16,%eax
  40a25d:	0f 85 fe 00 00 00    	jne    40a361 <os::[file_linux.odin]::_seek+0x2b1>
  40a263:	eb 00                	jmp    40a265 <os::[file_linux.odin]::_seek+0x1b5>
  40a265:	48 c7 44 24 70 00 00 	movq   $0x0,0x70(%rsp)
  40a26c:	00 00 
  40a26e:	31 c0                	xor    %eax,%eax
  40a270:	a8 01                	test   $0x1,%al
  40a272:	75 02                	jne    40a276 <os::[file_linux.odin]::_seek+0x1c6>
  40a274:	eb 12                	jmp    40a288 <os::[file_linux.odin]::_seek+0x1d8>
  40a276:	c7 44 24 74 00 00 00 	movl   $0x0,0x74(%rsp)
  40a27d:	00 
  40a27e:	c7 44 24 70 00 00 00 	movl   $0x0,0x70(%rsp)
  40a285:	00 
  40a286:	eb 10                	jmp    40a298 <os::[file_linux.odin]::_seek+0x1e8>
  40a288:	c7 44 24 70 08 00 00 	movl   $0x8,0x70(%rsp)
  40a28f:	00 
  40a290:	c7 44 24 74 02 00 00 	movl   $0x2,0x74(%rsp)
  40a297:	00 
  40a298:	8b 44 24 70          	mov    0x70(%rsp),%eax
  40a29c:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  40a2a0:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  40a2a7:	00 00 00 00 00 
  40a2ac:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  40a2b3:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  40a2ba:	48 c7 44 24 68 00 00 	movq   $0x0,0x68(%rsp)
  40a2c1:	00 00 
  40a2c3:	31 c0                	xor    %eax,%eax
  40a2c5:	a8 01                	test   $0x1,%al
  40a2c7:	75 02                	jne    40a2cb <os::[file_linux.odin]::_seek+0x21b>
  40a2c9:	eb 12                	jmp    40a2dd <os::[file_linux.odin]::_seek+0x22d>
  40a2cb:	c7 44 24 6c 00 00 00 	movl   $0x0,0x6c(%rsp)
  40a2d2:	00 
  40a2d3:	c7 44 24 68 00 00 00 	movl   $0x0,0x68(%rsp)
  40a2da:	00 
  40a2db:	eb 10                	jmp    40a2ed <os::[file_linux.odin]::_seek+0x23d>
  40a2dd:	c7 44 24 68 08 00 00 	movl   $0x8,0x68(%rsp)
  40a2e4:	00 
  40a2e5:	c7 44 24 6c 02 00 00 	movl   $0x2,0x6c(%rsp)
  40a2ec:	00 
  40a2ed:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40a2f2:	8b 44 24 68          	mov    0x68(%rsp),%eax
  40a2f6:	8b 4c 24 6c          	mov    0x6c(%rsp),%ecx
  40a2fa:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40a301:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  40a305:	89 44 24 60          	mov    %eax,0x60(%rsp)
  40a309:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40a30e:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40a315:	c3                   	ret
  40a316:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40a31b:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  40a322:	00 
  40a323:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40a32a:	00 
  40a32b:	c7 84 24 b4 00 00 00 	movl   $0x0,0xb4(%rsp)
  40a332:	00 00 00 00 
  40a336:	c7 84 24 b0 00 00 00 	movl   $0x0,0xb0(%rsp)
  40a33d:	00 00 00 00 
  40a341:	48 89 08             	mov    %rcx,(%rax)
  40a344:	c7 44 24 54 00 00 00 	movl   $0x0,0x54(%rsp)
  40a34b:	00 
  40a34c:	c7 44 24 50 00 00 00 	movl   $0x0,0x50(%rsp)
  40a353:	00 
  40a354:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40a359:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40a360:	c3                   	ret
  40a361:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40a366:	8b 7c 24 7c          	mov    0x7c(%rsp),%edi
  40a36a:	e8 61 0e 00 00       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40a36f:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40a374:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40a379:	8b 44 24 40          	mov    0x40(%rsp),%eax
  40a37d:	8b 4c 24 44          	mov    0x44(%rsp),%ecx
  40a381:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  40a388:	00 00 00 00 00 
  40a38d:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  40a394:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  40a39b:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40a3a2:	89 4c 24 34          	mov    %ecx,0x34(%rsp)
  40a3a6:	89 44 24 30          	mov    %eax,0x30(%rsp)
  40a3aa:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40a3af:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40a3b6:	c3                   	ret
  40a3b7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40a3be:	00 00 

000000000040a3c0 <os::[path_linux.odin]::_get_full_path>:
  40a3c0:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  40a3c7:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40a3cc:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40a3d1:	89 7c 24 3c          	mov    %edi,0x3c(%rsp)
  40a3d5:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40a3da:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40a3df:	8b 54 24 3c          	mov    0x3c(%rsp),%edx
  40a3e3:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40a3e8:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40a3ed:	89 94 24 14 01 00 00 	mov    %edx,0x114(%rsp)
  40a3f4:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  40a3fb:	00 
  40a3fc:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  40a403:	00 
  40a404:	48 8b 84 24 00 01 00 	mov    0x100(%rsp),%rax
  40a40b:	00 
  40a40c:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40a411:	48 8b 84 24 08 01 00 	mov    0x108(%rsp),%rax
  40a418:	00 
  40a419:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40a41e:	0f 57 c0             	xorps  %xmm0,%xmm0
  40a421:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  40a428:	00 
  40a429:	48 c7 84 24 e8 00 00 	movq   $0x0,0xe8(%rsp)
  40a430:	00 00 00 00 00 
  40a435:	0f 29 84 24 d0 00 00 	movaps %xmm0,0xd0(%rsp)
  40a43c:	00 
  40a43d:	0f 29 84 24 c0 00 00 	movaps %xmm0,0xc0(%rsp)
  40a444:	00 
  40a445:	48 8d 84 24 c0 00 00 	lea    0xc0(%rsp),%rax
  40a44c:	00 
  40a44d:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  40a454:	00 
  40a455:	48 c7 84 24 b8 00 00 	movq   $0x20,0xb8(%rsp)
  40a45c:	00 20 00 00 00 
  40a461:	48 8b bc 24 b0 00 00 	mov    0xb0(%rsp),%rdi
  40a468:	00 
  40a469:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  40a470:	00 
  40a471:	ba 78 fb 40 00       	mov    $0x40fb78,%edx
  40a476:	b9 0e 00 00 00       	mov    $0xe,%ecx
  40a47b:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40a480:	e8 eb 6d ff ff       	call   401270 <runtime::copy_from_string:proc"contextless"(dst:[]u8,src:string)->(:int)>
  40a485:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  40a48a:	48 89 e0             	mov    %rsp,%rax
  40a48d:	48 c7 00 20 00 00 00 	movq   $0x20,(%rax)
  40a494:	bf 87 fb 40 00       	mov    $0x40fb87,%edi
  40a499:	be 25 00 00 00       	mov    $0x25,%esi
  40a49e:	ba ca 00 00 00       	mov    $0xca,%edx
  40a4a3:	b9 17 00 00 00       	mov    $0x17,%ecx
  40a4a8:	41 b9 20 00 00 00    	mov    $0x20,%r9d
  40a4ae:	e8 ad 78 ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  40a4b3:	8b 44 24 3c          	mov    0x3c(%rsp),%eax
  40a4b7:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40a4bc:	48 8d 8c 24 ce 00 00 	lea    0xce(%rsp),%rcx
  40a4c3:	00 
  40a4c4:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  40a4cb:	00 
  40a4cc:	48 c7 84 24 a8 00 00 	movq   $0x12,0xa8(%rsp)
  40a4d3:	00 12 00 00 00 
  40a4d8:	48 8b bc 24 a0 00 00 	mov    0xa0(%rsp),%rdi
  40a4df:	00 
  40a4e0:	48 8b b4 24 a8 00 00 	mov    0xa8(%rsp),%rsi
  40a4e7:	00 
  40a4e8:	48 63 d0             	movslq %eax,%rdx
  40a4eb:	b9 0a 00 00 00       	mov    $0xa,%ecx
  40a4f0:	e8 eb ac ff ff       	call   4051e0 <strconv::write_int>
  40a4f5:	4c 8b 44 24 28       	mov    0x28(%rsp),%r8
  40a4fa:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40a4ff:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40a504:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  40a50b:	00 
  40a50c:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  40a513:	00 
  40a514:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  40a51b:	00 
  40a51c:	48 8b 94 24 98 00 00 	mov    0x98(%rsp),%rdx
  40a523:	00 
  40a524:	0f 57 c0             	xorps  %xmm0,%xmm0
  40a527:	0f 29 84 24 80 00 00 	movaps %xmm0,0x80(%rsp)
  40a52e:	00 
  40a52f:	48 8d bc 24 c0 00 00 	lea    0xc0(%rsp),%rdi
  40a536:	00 
  40a537:	48 8d 8c 24 80 00 00 	lea    0x80(%rsp),%rcx
  40a53e:	00 
  40a53f:	e8 1c 12 00 00       	call   40b760 <os::[file_linux.odin]::_read_link_cstr>
  40a544:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40a549:	8b 44 24 70          	mov    0x70(%rsp),%eax
  40a54d:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  40a551:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  40a558:	00 
  40a559:	48 8b b4 24 88 00 00 	mov    0x88(%rsp),%rsi
  40a560:	00 
  40a561:	48 89 b4 24 f8 00 00 	mov    %rsi,0xf8(%rsp)
  40a568:	00 
  40a569:	48 89 94 24 f0 00 00 	mov    %rdx,0xf0(%rsp)
  40a570:	00 
  40a571:	89 8c 24 ec 00 00 00 	mov    %ecx,0xec(%rsp)
  40a578:	89 84 24 e8 00 00 00 	mov    %eax,0xe8(%rsp)
  40a57f:	83 bc 24 ec 00 00 00 	cmpl   $0x0,0xec(%rsp)
  40a586:	00 
  40a587:	0f 95 c0             	setne  %al
  40a58a:	24 01                	and    $0x1,%al
  40a58c:	3c 00                	cmp    $0x0,%al
  40a58e:	75 44                	jne    40a5d4 <os::[path_linux.odin]::_get_full_path+0x214>
  40a590:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40a597:	00 
  40a598:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  40a59d:	4c 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%r9
  40a5a4:	00 
  40a5a5:	bf 87 fb 40 00       	mov    $0x40fb87,%edi
  40a5aa:	31 c0                	xor    %eax,%eax
  40a5ac:	41 89 c0             	mov    %eax,%r8d
  40a5af:	be 25 00 00 00       	mov    $0x25,%esi
  40a5b4:	ba cc 00 00 00       	mov    $0xcc,%edx
  40a5b9:	b9 5a 00 00 00       	mov    $0x5a,%ecx
  40a5be:	e8 bd 73 ff ff       	call   401980 <runtime::bounds_check_error>
  40a5c3:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40a5c8:	80 38 2f             	cmpb   $0x2f,(%rax)
  40a5cb:	0f 95 c0             	setne  %al
  40a5ce:	24 01                	and    $0x1,%al
  40a5d0:	3c 00                	cmp    $0x0,%al
  40a5d2:	74 52                	je     40a626 <os::[path_linux.odin]::_get_full_path+0x266>
  40a5d4:	4c 8b 4c 24 28       	mov    0x28(%rsp),%r9
  40a5d9:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40a5de:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40a5e3:	48 8b bc 24 f0 00 00 	mov    0xf0(%rsp),%rdi
  40a5ea:	00 
  40a5eb:	48 8b b4 24 f8 00 00 	mov    0xf8(%rsp),%rsi
  40a5f2:	00 
  40a5f3:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  40a5f8:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40a5fd:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40a602:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40a607:	41 b8 c0 fb 40 00    	mov    $0x40fbc0,%r8d
  40a60d:	e8 ae 6c ff ff       	call   4012c0 <runtime::delete_string>
  40a612:	48 8d bc 24 f0 00 00 	lea    0xf0(%rsp),%rdi
  40a619:	00 
  40a61a:	31 f6                	xor    %esi,%esi
  40a61c:	ba 10 00 00 00       	mov    $0x10,%edx
  40a621:	e8 1a 6a ff ff       	call   401040 <memset@plt>
  40a626:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40a62b:	48 8b b4 24 f0 00 00 	mov    0xf0(%rsp),%rsi
  40a632:	00 
  40a633:	48 8b bc 24 f8 00 00 	mov    0xf8(%rsp),%rdi
  40a63a:	00 
  40a63b:	8b 84 24 e8 00 00 00 	mov    0xe8(%rsp),%eax
  40a642:	8b 8c 24 ec 00 00 00 	mov    0xec(%rsp),%ecx
  40a649:	48 89 bc 24 f8 00 00 	mov    %rdi,0xf8(%rsp)
  40a650:	00 
  40a651:	48 89 b4 24 f0 00 00 	mov    %rsi,0xf0(%rsp)
  40a658:	00 
  40a659:	89 8c 24 ec 00 00 00 	mov    %ecx,0xec(%rsp)
  40a660:	89 84 24 e8 00 00 00 	mov    %eax,0xe8(%rsp)
  40a667:	48 89 7a 08          	mov    %rdi,0x8(%rdx)
  40a66b:	48 89 32             	mov    %rsi,(%rdx)
  40a66e:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  40a672:	89 44 24 50          	mov    %eax,0x50(%rsp)
  40a676:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40a67b:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40a682:	c3                   	ret
  40a683:	66 66 66 66 2e 0f 1f 	data16 data16 data16 cs nopw 0x0(%rax,%rax,1)
  40a68a:	84 00 00 00 00 00 

000000000040a690 <os::[file_linux.odin]::_read>:
  40a690:	48 81 ec c8 00 00 00 	sub    $0xc8,%rsp
  40a697:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40a69c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40a6a1:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40a6a6:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40a6ab:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40a6b0:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40a6b5:	48 8b 4c 24 48       	mov    0x48(%rsp),%rcx
  40a6ba:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40a6bf:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  40a6c6:	00 
  40a6c7:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  40a6ce:	00 
  40a6cf:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  40a6d6:	00 
  40a6d7:	48 83 f8 00          	cmp    $0x0,%rax
  40a6db:	0f 9e c0             	setle  %al
  40a6de:	24 01                	and    $0x1,%al
  40a6e0:	3c 00                	cmp    $0x0,%al
  40a6e2:	74 32                	je     40a716 <os::[file_linux.odin]::_read+0x86>
  40a6e4:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40a6e9:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40a6f0:	c7 84 24 a4 00 00 00 	movl   $0x0,0xa4(%rsp)
  40a6f7:	00 00 00 00 
  40a6fb:	c7 84 24 a0 00 00 00 	movl   $0x0,0xa0(%rsp)
  40a702:	00 00 00 00 
  40a706:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  40a70d:	00 
  40a70e:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40a715:	c3                   	ret
  40a716:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40a71b:	48 8b 8c 24 c0 00 00 	mov    0xc0(%rsp),%rcx
  40a722:	00 
  40a723:	8b 49 28             	mov    0x28(%rcx),%ecx
  40a726:	89 4c 24 24          	mov    %ecx,0x24(%rsp)
  40a72a:	48 89 c1             	mov    %rax,%rcx
  40a72d:	48 81 e9 00 00 00 40 	sub    $0x40000000,%rcx
  40a734:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  40a73a:	4c 0f 4c c0          	cmovl  %rax,%r8
  40a73e:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40a743:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  40a74a:	00 
  40a74b:	bf c0 fa 40 00       	mov    $0x40fac0,%edi
  40a750:	be 25 00 00 00       	mov    $0x25,%esi
  40a755:	ba da 00 00 00       	mov    $0xda,%edx
  40a75a:	b9 20 00 00 00       	mov    $0x20,%ecx
  40a75f:	e8 4c 75 ff ff       	call   401cb0 <runtime::slice_expr_error_hi>
  40a764:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40a769:	8b 7c 24 24          	mov    0x24(%rsp),%edi
  40a76d:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40a774:	00 
  40a775:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  40a77c:	00 
  40a77d:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  40a784:	00 
  40a785:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  40a78c:	00 
  40a78d:	48 8b 94 24 98 00 00 	mov    0x98(%rsp),%rdx
  40a794:	00 
  40a795:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40a79c:	00 00 00 00 00 
  40a7a1:	48 8d 8c 24 88 00 00 	lea    0x88(%rsp),%rcx
  40a7a8:	00 
  40a7a9:	e8 42 c5 ff ff       	call   406cf0 <linux::read>
  40a7ae:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40a7b5:	00 
  40a7b6:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  40a7bd:	00 
  40a7be:	89 44 24 7c          	mov    %eax,0x7c(%rsp)
  40a7c2:	83 7c 24 7c 00       	cmpl   $0x0,0x7c(%rsp)
  40a7c7:	0f 95 c0             	setne  %al
  40a7ca:	24 01                	and    $0x1,%al
  40a7cc:	3c 00                	cmp    $0x0,%al
  40a7ce:	74 3c                	je     40a80c <os::[file_linux.odin]::_read+0x17c>
  40a7d0:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40a7d5:	8b 7c 24 7c          	mov    0x7c(%rsp),%edi
  40a7d9:	e8 f2 09 00 00       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40a7de:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40a7e3:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40a7e8:	8b 44 24 70          	mov    0x70(%rsp),%eax
  40a7ec:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  40a7f0:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40a7f7:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  40a7fb:	89 44 24 60          	mov    %eax,0x60(%rsp)
  40a7ff:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40a804:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40a80b:	c3                   	ret
  40a80c:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40a813:	00 
  40a814:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40a819:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  40a820:	00 00 
  40a822:	0f 94 c0             	sete   %al
  40a825:	24 01                	and    $0x1,%al
  40a827:	3c 00                	cmp    $0x0,%al
  40a829:	74 45                	je     40a870 <os::[file_linux.odin]::_read+0x1e0>
  40a82b:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  40a832:	00 00 
  40a834:	31 c0                	xor    %eax,%eax
  40a836:	a8 01                	test   $0x1,%al
  40a838:	75 02                	jne    40a83c <os::[file_linux.odin]::_read+0x1ac>
  40a83a:	eb 12                	jmp    40a84e <os::[file_linux.odin]::_read+0x1be>
  40a83c:	c7 44 24 5c 00 00 00 	movl   $0x0,0x5c(%rsp)
  40a843:	00 
  40a844:	c7 44 24 58 00 00 00 	movl   $0x0,0x58(%rsp)
  40a84b:	00 
  40a84c:	eb 10                	jmp    40a85e <os::[file_linux.odin]::_read+0x1ce>
  40a84e:	c7 44 24 58 01 00 00 	movl   $0x1,0x58(%rsp)
  40a855:	00 
  40a856:	c7 44 24 5c 02 00 00 	movl   $0x2,0x5c(%rsp)
  40a85d:	00 
  40a85e:	8b 4c 24 58          	mov    0x58(%rsp),%ecx
  40a862:	8b 44 24 5c          	mov    0x5c(%rsp),%eax
  40a866:	89 4c 24 08          	mov    %ecx,0x8(%rsp)
  40a86a:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  40a86e:	eb 0e                	jmp    40a87e <os::[file_linux.odin]::_read+0x1ee>
  40a870:	31 c9                	xor    %ecx,%ecx
  40a872:	89 c8                	mov    %ecx,%eax
  40a874:	89 4c 24 08          	mov    %ecx,0x8(%rsp)
  40a878:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  40a87c:	eb 00                	jmp    40a87e <os::[file_linux.odin]::_read+0x1ee>
  40a87e:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40a883:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40a888:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40a88c:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40a890:	48 89 32             	mov    %rsi,(%rdx)
  40a893:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  40a897:	89 44 24 50          	mov    %eax,0x50(%rsp)
  40a89b:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40a8a0:	48 81 c4 c8 00 00 00 	add    $0xc8,%rsp
  40a8a7:	c3                   	ret
  40a8a8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40a8af:	00 

000000000040a8b0 <os::[file_linux.odin]::_read_at>:
  40a8b0:	48 81 ec d8 00 00 00 	sub    $0xd8,%rsp
  40a8b7:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40a8bc:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40a8c1:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40a8c6:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40a8cb:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40a8d0:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40a8d5:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40a8da:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40a8df:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40a8e4:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40a8e9:	48 89 b4 24 d0 00 00 	mov    %rsi,0xd0(%rsp)
  40a8f0:	00 
  40a8f1:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  40a8f8:	00 
  40a8f9:	48 89 94 24 c0 00 00 	mov    %rdx,0xc0(%rsp)
  40a900:	00 
  40a901:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40a908:	00 
  40a909:	48 83 f8 00          	cmp    $0x0,%rax
  40a90d:	0f 9e c0             	setle  %al
  40a910:	24 01                	and    $0x1,%al
  40a912:	3c 00                	cmp    $0x0,%al
  40a914:	74 32                	je     40a948 <os::[file_linux.odin]::_read_at+0x98>
  40a916:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40a91b:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40a922:	c7 84 24 b4 00 00 00 	movl   $0x0,0xb4(%rsp)
  40a929:	00 00 00 00 
  40a92d:	c7 84 24 b0 00 00 00 	movl   $0x0,0xb0(%rsp)
  40a934:	00 00 00 00 
  40a938:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40a93f:	00 
  40a940:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40a947:	c3                   	ret
  40a948:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40a94d:	48 83 f8 00          	cmp    $0x0,%rax
  40a951:	0f 9c c0             	setl   %al
  40a954:	24 01                	and    $0x1,%al
  40a956:	3c 00                	cmp    $0x0,%al
  40a958:	74 7a                	je     40a9d4 <os::[file_linux.odin]::_read_at+0x124>
  40a95a:	48 c7 84 24 a8 00 00 	movq   $0x0,0xa8(%rsp)
  40a961:	00 00 00 00 00 
  40a966:	31 c0                	xor    %eax,%eax
  40a968:	a8 01                	test   $0x1,%al
  40a96a:	75 02                	jne    40a96e <os::[file_linux.odin]::_read_at+0xbe>
  40a96c:	eb 18                	jmp    40a986 <os::[file_linux.odin]::_read_at+0xd6>
  40a96e:	c7 84 24 ac 00 00 00 	movl   $0x0,0xac(%rsp)
  40a975:	00 00 00 00 
  40a979:	c7 84 24 a8 00 00 00 	movl   $0x0,0xa8(%rsp)
  40a980:	00 00 00 00 
  40a984:	eb 16                	jmp    40a99c <os::[file_linux.odin]::_read_at+0xec>
  40a986:	c7 84 24 a8 00 00 00 	movl   $0x8,0xa8(%rsp)
  40a98d:	08 00 00 00 
  40a991:	c7 84 24 ac 00 00 00 	movl   $0x2,0xac(%rsp)
  40a998:	02 00 00 00 
  40a99c:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40a9a1:	8b 84 24 a8 00 00 00 	mov    0xa8(%rsp),%eax
  40a9a8:	8b 8c 24 ac 00 00 00 	mov    0xac(%rsp),%ecx
  40a9af:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40a9b6:	89 8c 24 a4 00 00 00 	mov    %ecx,0xa4(%rsp)
  40a9bd:	89 84 24 a0 00 00 00 	mov    %eax,0xa0(%rsp)
  40a9c4:	48 8b 84 24 a0 00 00 	mov    0xa0(%rsp),%rax
  40a9cb:	00 
  40a9cc:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40a9d3:	c3                   	ret
  40a9d4:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40a9d9:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  40a9e0:	00 
  40a9e1:	8b 49 28             	mov    0x28(%rcx),%ecx
  40a9e4:	89 4c 24 0c          	mov    %ecx,0xc(%rsp)
  40a9e8:	48 89 c1             	mov    %rax,%rcx
  40a9eb:	48 81 e9 00 00 00 40 	sub    $0x40000000,%rcx
  40a9f2:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  40a9f8:	4c 0f 4c c0          	cmovl  %rax,%r8
  40a9fc:	4c 89 04 24          	mov    %r8,(%rsp)
  40aa00:	4c 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%r9
  40aa07:	00 
  40aa08:	bf c0 fa 40 00       	mov    $0x40fac0,%edi
  40aa0d:	be 25 00 00 00       	mov    $0x25,%esi
  40aa12:	ba e8 00 00 00       	mov    $0xe8,%edx
  40aa17:	b9 21 00 00 00       	mov    $0x21,%ecx
  40aa1c:	e8 8f 72 ff ff       	call   401cb0 <runtime::slice_expr_error_hi>
  40aa21:	48 8b 04 24          	mov    (%rsp),%rax
  40aa25:	8b 7c 24 0c          	mov    0xc(%rsp),%edi
  40aa29:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40aa2e:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  40aa35:	00 
  40aa36:	48 89 94 24 90 00 00 	mov    %rdx,0x90(%rsp)
  40aa3d:	00 
  40aa3e:	48 89 84 24 98 00 00 	mov    %rax,0x98(%rsp)
  40aa45:	00 
  40aa46:	48 8b b4 24 90 00 00 	mov    0x90(%rsp),%rsi
  40aa4d:	00 
  40aa4e:	48 8b 94 24 98 00 00 	mov    0x98(%rsp),%rdx
  40aa55:	00 
  40aa56:	48 c7 84 24 88 00 00 	movq   $0x0,0x88(%rsp)
  40aa5d:	00 00 00 00 00 
  40aa62:	4c 8d 84 24 88 00 00 	lea    0x88(%rsp),%r8
  40aa69:	00 
  40aa6a:	e8 a1 c6 ff ff       	call   407110 <linux::pread>
  40aa6f:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40aa76:	00 
  40aa77:	48 89 8c 24 80 00 00 	mov    %rcx,0x80(%rsp)
  40aa7e:	00 
  40aa7f:	89 44 24 7c          	mov    %eax,0x7c(%rsp)
  40aa83:	83 7c 24 7c 00       	cmpl   $0x0,0x7c(%rsp)
  40aa88:	0f 95 c0             	setne  %al
  40aa8b:	24 01                	and    $0x1,%al
  40aa8d:	3c 00                	cmp    $0x0,%al
  40aa8f:	74 3c                	je     40aacd <os::[file_linux.odin]::_read_at+0x21d>
  40aa91:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40aa96:	8b 7c 24 7c          	mov    0x7c(%rsp),%edi
  40aa9a:	e8 31 07 00 00       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40aa9f:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40aaa4:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40aaa9:	8b 44 24 70          	mov    0x70(%rsp),%eax
  40aaad:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  40aab1:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40aab8:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  40aabc:	89 44 24 60          	mov    %eax,0x60(%rsp)
  40aac0:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40aac5:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40aacc:	c3                   	ret
  40aacd:	48 83 bc 24 80 00 00 	cmpq   $0x0,0x80(%rsp)
  40aad4:	00 00 
  40aad6:	0f 94 c0             	sete   %al
  40aad9:	24 01                	and    $0x1,%al
  40aadb:	3c 00                	cmp    $0x0,%al
  40aadd:	74 5c                	je     40ab3b <os::[file_linux.odin]::_read_at+0x28b>
  40aadf:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  40aae6:	00 00 
  40aae8:	31 c0                	xor    %eax,%eax
  40aaea:	a8 01                	test   $0x1,%al
  40aaec:	75 02                	jne    40aaf0 <os::[file_linux.odin]::_read_at+0x240>
  40aaee:	eb 12                	jmp    40ab02 <os::[file_linux.odin]::_read_at+0x252>
  40aaf0:	c7 44 24 5c 00 00 00 	movl   $0x0,0x5c(%rsp)
  40aaf7:	00 
  40aaf8:	c7 44 24 58 00 00 00 	movl   $0x0,0x58(%rsp)
  40aaff:	00 
  40ab00:	eb 10                	jmp    40ab12 <os::[file_linux.odin]::_read_at+0x262>
  40ab02:	c7 44 24 58 01 00 00 	movl   $0x1,0x58(%rsp)
  40ab09:	00 
  40ab0a:	c7 44 24 5c 02 00 00 	movl   $0x2,0x5c(%rsp)
  40ab11:	00 
  40ab12:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40ab17:	8b 44 24 58          	mov    0x58(%rsp),%eax
  40ab1b:	8b 4c 24 5c          	mov    0x5c(%rsp),%ecx
  40ab1f:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40ab26:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  40ab2a:	89 44 24 50          	mov    %eax,0x50(%rsp)
  40ab2e:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40ab33:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40ab3a:	c3                   	ret
  40ab3b:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40ab40:	48 8b 8c 24 80 00 00 	mov    0x80(%rsp),%rcx
  40ab47:	00 
  40ab48:	48 89 08             	mov    %rcx,(%rax)
  40ab4b:	c7 44 24 44 00 00 00 	movl   $0x0,0x44(%rsp)
  40ab52:	00 
  40ab53:	c7 44 24 40 00 00 00 	movl   $0x0,0x40(%rsp)
  40ab5a:	00 
  40ab5b:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40ab60:	48 81 c4 d8 00 00 00 	add    $0xd8,%rsp
  40ab67:	c3                   	ret
  40ab68:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40ab6f:	00 

000000000040ab70 <os::[file_linux.odin]::_write>:
  40ab70:	48 81 ec e8 00 00 00 	sub    $0xe8,%rsp
  40ab77:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40ab7c:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40ab81:	48 89 7c 24 38       	mov    %rdi,0x38(%rsp)
  40ab86:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40ab8b:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40ab90:	48 8b 44 24 48       	mov    0x48(%rsp),%rax
  40ab95:	48 8b 4c 24 40       	mov    0x40(%rsp),%rcx
  40ab9a:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40ab9f:	48 89 94 24 e0 00 00 	mov    %rdx,0xe0(%rsp)
  40aba6:	00 
  40aba7:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  40abae:	00 
  40abaf:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  40abb6:	00 
  40abb7:	48 c7 84 24 c8 00 00 	movq   $0x0,0xc8(%rsp)
  40abbe:	00 00 00 00 00 
  40abc3:	48 c7 84 24 c0 00 00 	movq   $0x0,0xc0(%rsp)
  40abca:	00 00 00 00 00 
  40abcf:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40abd6:	00 
  40abd7:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  40abde:	00 
  40abdf:	48 83 bc 24 b8 00 00 	cmpq   $0x0,0xb8(%rsp)
  40abe6:	00 00 
  40abe8:	0f 9f c0             	setg   %al
  40abeb:	24 01                	and    $0x1,%al
  40abed:	3c 00                	cmp    $0x0,%al
  40abef:	0f 84 c2 01 00 00    	je     40adb7 <os::[file_linux.odin]::_write+0x247>
  40abf5:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40abfc:	00 
  40abfd:	8b 40 28             	mov    0x28(%rax),%eax
  40ac00:	89 44 24 24          	mov    %eax,0x24(%rsp)
  40ac04:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  40ac0b:	00 
  40ac0c:	4c 89 c8             	mov    %r9,%rax
  40ac0f:	48 2d 00 00 00 40    	sub    $0x40000000,%rax
  40ac15:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  40ac1b:	4d 0f 4c c1          	cmovl  %r9,%r8
  40ac1f:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40ac24:	bf c0 fa 40 00       	mov    $0x40fac0,%edi
  40ac29:	be 25 00 00 00       	mov    $0x25,%esi
  40ac2e:	ba f5 00 00 00       	mov    $0xf5,%edx
  40ac33:	b9 22 00 00 00       	mov    $0x22,%ecx
  40ac38:	e8 73 70 ff ff       	call   401cb0 <runtime::slice_expr_error_hi>
  40ac3d:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40ac42:	8b 7c 24 24          	mov    0x24(%rsp),%edi
  40ac46:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40ac4d:	00 
  40ac4e:	48 89 8c 24 a0 00 00 	mov    %rcx,0xa0(%rsp)
  40ac55:	00 
  40ac56:	48 89 84 24 a8 00 00 	mov    %rax,0xa8(%rsp)
  40ac5d:	00 
  40ac5e:	48 8b b4 24 a0 00 00 	mov    0xa0(%rsp),%rsi
  40ac65:	00 
  40ac66:	48 8b 94 24 a8 00 00 	mov    0xa8(%rsp),%rdx
  40ac6d:	00 
  40ac6e:	48 c7 84 24 98 00 00 	movq   $0x0,0x98(%rsp)
  40ac75:	00 00 00 00 00 
  40ac7a:	48 8d 8c 24 98 00 00 	lea    0x98(%rsp),%rcx
  40ac81:	00 
  40ac82:	e8 d9 c0 ff ff       	call   406d60 <linux::write>
  40ac87:	48 8b 8c 24 98 00 00 	mov    0x98(%rsp),%rcx
  40ac8e:	00 
  40ac8f:	48 89 8c 24 90 00 00 	mov    %rcx,0x90(%rsp)
  40ac96:	00 
  40ac97:	89 84 24 8c 00 00 00 	mov    %eax,0x8c(%rsp)
  40ac9e:	83 bc 24 8c 00 00 00 	cmpl   $0x0,0x8c(%rsp)
  40aca5:	00 
  40aca6:	0f 95 c0             	setne  %al
  40aca9:	24 01                	and    $0x1,%al
  40acab:	3c 00                	cmp    $0x0,%al
  40acad:	74 76                	je     40ad25 <os::[file_linux.odin]::_write+0x1b5>
  40acaf:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
  40acb4:	8b bc 24 8c 00 00 00 	mov    0x8c(%rsp),%edi
  40acbb:	e8 10 05 00 00       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40acc0:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40acc5:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40accc:	00 
  40accd:	8b 84 24 80 00 00 00 	mov    0x80(%rsp),%eax
  40acd4:	8b 8c 24 84 00 00 00 	mov    0x84(%rsp),%ecx
  40acdb:	89 8c 24 c4 00 00 00 	mov    %ecx,0xc4(%rsp)
  40ace2:	89 84 24 c0 00 00 00 	mov    %eax,0xc0(%rsp)
  40ace9:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  40acf0:	00 
  40acf1:	8b 84 24 c0 00 00 00 	mov    0xc0(%rsp),%eax
  40acf8:	8b 8c 24 c4 00 00 00 	mov    0xc4(%rsp),%ecx
  40acff:	89 8c 24 c4 00 00 00 	mov    %ecx,0xc4(%rsp)
  40ad06:	89 84 24 c0 00 00 00 	mov    %eax,0xc0(%rsp)
  40ad0d:	48 89 32             	mov    %rsi,(%rdx)
  40ad10:	89 4c 24 74          	mov    %ecx,0x74(%rsp)
  40ad14:	89 44 24 70          	mov    %eax,0x70(%rsp)
  40ad18:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40ad1d:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40ad24:	c3                   	ret
  40ad25:	4c 8b 84 24 90 00 00 	mov    0x90(%rsp),%r8
  40ad2c:	00 
  40ad2d:	4c 89 44 24 10       	mov    %r8,0x10(%rsp)
  40ad32:	4c 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%r9
  40ad39:	00 
  40ad3a:	4c 89 4c 24 08       	mov    %r9,0x8(%rsp)
  40ad3f:	48 89 e0             	mov    %rsp,%rax
  40ad42:	4c 89 08             	mov    %r9,(%rax)
  40ad45:	bf c0 fa 40 00       	mov    $0x40fac0,%edi
  40ad4a:	be 25 00 00 00       	mov    $0x25,%esi
  40ad4f:	ba fb 00 00 00       	mov    $0xfb,%edx
  40ad54:	b9 08 00 00 00       	mov    $0x8,%ecx
  40ad59:	e8 02 70 ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  40ad5e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40ad63:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40ad68:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40ad6f:	00 
  40ad70:	48 01 d1             	add    %rdx,%rcx
  40ad73:	48 29 d0             	sub    %rdx,%rax
  40ad76:	48 89 4c 24 60       	mov    %rcx,0x60(%rsp)
  40ad7b:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40ad80:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40ad85:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40ad8a:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40ad91:	00 
  40ad92:	48 89 84 24 b0 00 00 	mov    %rax,0xb0(%rsp)
  40ad99:	00 
  40ad9a:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40ada1:	00 
  40ada2:	48 03 84 24 c8 00 00 	add    0xc8(%rsp),%rax
  40ada9:	00 
  40adaa:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  40adb1:	00 
  40adb2:	e9 28 fe ff ff       	jmp    40abdf <os::[file_linux.odin]::_write+0x6f>
  40adb7:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40adbc:	48 8b b4 24 c8 00 00 	mov    0xc8(%rsp),%rsi
  40adc3:	00 
  40adc4:	8b 84 24 c0 00 00 00 	mov    0xc0(%rsp),%eax
  40adcb:	8b 8c 24 c4 00 00 00 	mov    0xc4(%rsp),%ecx
  40add2:	89 8c 24 c4 00 00 00 	mov    %ecx,0xc4(%rsp)
  40add9:	89 84 24 c0 00 00 00 	mov    %eax,0xc0(%rsp)
  40ade0:	48 89 32             	mov    %rsi,(%rdx)
  40ade3:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  40ade7:	89 44 24 50          	mov    %eax,0x50(%rsp)
  40adeb:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40adf0:	48 81 c4 e8 00 00 00 	add    $0xe8,%rsp
  40adf7:	c3                   	ret
  40adf8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40adff:	00 

000000000040ae00 <os::[file_linux.odin]::_write_at>:
  40ae00:	48 81 ec 28 01 00 00 	sub    $0x128,%rsp
  40ae07:	4c 89 4c 24 30       	mov    %r9,0x30(%rsp)
  40ae0c:	4c 89 44 24 38       	mov    %r8,0x38(%rsp)
  40ae11:	48 89 4c 24 40       	mov    %rcx,0x40(%rsp)
  40ae16:	48 89 7c 24 48       	mov    %rdi,0x48(%rsp)
  40ae1b:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40ae20:	48 89 74 24 58       	mov    %rsi,0x58(%rsp)
  40ae25:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40ae2a:	48 8b 4c 24 58       	mov    0x58(%rsp),%rcx
  40ae2f:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40ae34:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40ae39:	48 89 b4 24 20 01 00 	mov    %rsi,0x120(%rsp)
  40ae40:	00 
  40ae41:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  40ae48:	00 
  40ae49:	48 89 8c 24 10 01 00 	mov    %rcx,0x110(%rsp)
  40ae50:	00 
  40ae51:	48 89 84 24 08 01 00 	mov    %rax,0x108(%rsp)
  40ae58:	00 
  40ae59:	48 c7 84 24 00 01 00 	movq   $0x0,0x100(%rsp)
  40ae60:	00 00 00 00 00 
  40ae65:	48 c7 84 24 f8 00 00 	movq   $0x0,0xf8(%rsp)
  40ae6c:	00 00 00 00 00 
  40ae71:	48 83 f8 00          	cmp    $0x0,%rax
  40ae75:	0f 9c c0             	setl   %al
  40ae78:	24 01                	and    $0x1,%al
  40ae7a:	3c 00                	cmp    $0x0,%al
  40ae7c:	0f 84 e4 00 00 00    	je     40af66 <os::[file_linux.odin]::_write_at+0x166>
  40ae82:	48 c7 84 24 f0 00 00 	movq   $0x0,0xf0(%rsp)
  40ae89:	00 00 00 00 00 
  40ae8e:	31 c0                	xor    %eax,%eax
  40ae90:	a8 01                	test   $0x1,%al
  40ae92:	75 02                	jne    40ae96 <os::[file_linux.odin]::_write_at+0x96>
  40ae94:	eb 18                	jmp    40aeae <os::[file_linux.odin]::_write_at+0xae>
  40ae96:	c7 84 24 f4 00 00 00 	movl   $0x0,0xf4(%rsp)
  40ae9d:	00 00 00 00 
  40aea1:	c7 84 24 f0 00 00 00 	movl   $0x0,0xf0(%rsp)
  40aea8:	00 00 00 00 
  40aeac:	eb 16                	jmp    40aec4 <os::[file_linux.odin]::_write_at+0xc4>
  40aeae:	c7 84 24 f0 00 00 00 	movl   $0x8,0xf0(%rsp)
  40aeb5:	08 00 00 00 
  40aeb9:	c7 84 24 f4 00 00 00 	movl   $0x2,0xf4(%rsp)
  40aec0:	02 00 00 00 
  40aec4:	8b 84 24 f0 00 00 00 	mov    0xf0(%rsp),%eax
  40aecb:	8b 8c 24 f4 00 00 00 	mov    0xf4(%rsp),%ecx
  40aed2:	48 c7 84 24 00 01 00 	movq   $0x0,0x100(%rsp)
  40aed9:	00 00 00 00 00 
  40aede:	89 8c 24 fc 00 00 00 	mov    %ecx,0xfc(%rsp)
  40aee5:	89 84 24 f8 00 00 00 	mov    %eax,0xf8(%rsp)
  40aeec:	48 c7 84 24 e8 00 00 	movq   $0x0,0xe8(%rsp)
  40aef3:	00 00 00 00 00 
  40aef8:	31 c0                	xor    %eax,%eax
  40aefa:	a8 01                	test   $0x1,%al
  40aefc:	75 02                	jne    40af00 <os::[file_linux.odin]::_write_at+0x100>
  40aefe:	eb 18                	jmp    40af18 <os::[file_linux.odin]::_write_at+0x118>
  40af00:	c7 84 24 ec 00 00 00 	movl   $0x0,0xec(%rsp)
  40af07:	00 00 00 00 
  40af0b:	c7 84 24 e8 00 00 00 	movl   $0x0,0xe8(%rsp)
  40af12:	00 00 00 00 
  40af16:	eb 16                	jmp    40af2e <os::[file_linux.odin]::_write_at+0x12e>
  40af18:	c7 84 24 e8 00 00 00 	movl   $0x8,0xe8(%rsp)
  40af1f:	08 00 00 00 
  40af23:	c7 84 24 ec 00 00 00 	movl   $0x2,0xec(%rsp)
  40af2a:	02 00 00 00 
  40af2e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40af33:	8b 84 24 e8 00 00 00 	mov    0xe8(%rsp),%eax
  40af3a:	8b 8c 24 ec 00 00 00 	mov    0xec(%rsp),%ecx
  40af41:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40af48:	89 8c 24 e4 00 00 00 	mov    %ecx,0xe4(%rsp)
  40af4f:	89 84 24 e0 00 00 00 	mov    %eax,0xe0(%rsp)
  40af56:	48 8b 84 24 e0 00 00 	mov    0xe0(%rsp),%rax
  40af5d:	00 
  40af5e:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  40af65:	c3                   	ret
  40af66:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40af6b:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40af70:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  40af75:	48 89 94 24 d0 00 00 	mov    %rdx,0xd0(%rsp)
  40af7c:	00 
  40af7d:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  40af84:	00 
  40af85:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  40af8c:	00 
  40af8d:	48 83 bc 24 d8 00 00 	cmpq   $0x0,0xd8(%rsp)
  40af94:	00 00 
  40af96:	0f 9f c0             	setg   %al
  40af99:	24 01                	and    $0x1,%al
  40af9b:	3c 00                	cmp    $0x0,%al
  40af9d:	0f 84 eb 01 00 00    	je     40b18e <os::[file_linux.odin]::_write_at+0x38e>
  40afa3:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40afaa:	00 
  40afab:	8b 40 28             	mov    0x28(%rax),%eax
  40afae:	89 44 24 2c          	mov    %eax,0x2c(%rsp)
  40afb2:	4c 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%r9
  40afb9:	00 
  40afba:	4c 89 c8             	mov    %r9,%rax
  40afbd:	48 2d 00 00 00 40    	sub    $0x40000000,%rax
  40afc3:	41 b8 00 00 00 40    	mov    $0x40000000,%r8d
  40afc9:	4d 0f 4c c1          	cmovl  %r9,%r8
  40afcd:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40afd2:	bf c0 fa 40 00       	mov    $0x40fac0,%edi
  40afd7:	be 25 00 00 00       	mov    $0x25,%esi
  40afdc:	ba 0a 01 00 00       	mov    $0x10a,%edx
  40afe1:	b9 23 00 00 00       	mov    $0x23,%ecx
  40afe6:	e8 c5 6c ff ff       	call   401cb0 <runtime::slice_expr_error_hi>
  40afeb:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40aff0:	8b 7c 24 2c          	mov    0x2c(%rsp),%edi
  40aff4:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  40affb:	00 
  40affc:	48 89 8c 24 b8 00 00 	mov    %rcx,0xb8(%rsp)
  40b003:	00 
  40b004:	48 89 84 24 c0 00 00 	mov    %rax,0xc0(%rsp)
  40b00b:	00 
  40b00c:	48 8b b4 24 b8 00 00 	mov    0xb8(%rsp),%rsi
  40b013:	00 
  40b014:	48 8b 94 24 c0 00 00 	mov    0xc0(%rsp),%rdx
  40b01b:	00 
  40b01c:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  40b023:	00 
  40b024:	48 c7 84 24 b0 00 00 	movq   $0x0,0xb0(%rsp)
  40b02b:	00 00 00 00 00 
  40b030:	4c 8d 84 24 b0 00 00 	lea    0xb0(%rsp),%r8
  40b037:	00 
  40b038:	e8 63 c1 ff ff       	call   4071a0 <linux::pwrite>
  40b03d:	48 8b 8c 24 b0 00 00 	mov    0xb0(%rsp),%rcx
  40b044:	00 
  40b045:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  40b04c:	00 
  40b04d:	89 84 24 a4 00 00 00 	mov    %eax,0xa4(%rsp)
  40b054:	83 bc 24 a4 00 00 00 	cmpl   $0x0,0xa4(%rsp)
  40b05b:	00 
  40b05c:	0f 95 c0             	setne  %al
  40b05f:	24 01                	and    $0x1,%al
  40b061:	3c 00                	cmp    $0x0,%al
  40b063:	74 7f                	je     40b0e4 <os::[file_linux.odin]::_write_at+0x2e4>
  40b065:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40b06a:	8b bc 24 a4 00 00 00 	mov    0xa4(%rsp),%edi
  40b071:	e8 5a 01 00 00       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40b076:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40b07b:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40b082:	00 
  40b083:	8b 84 24 90 00 00 00 	mov    0x90(%rsp),%eax
  40b08a:	8b 8c 24 94 00 00 00 	mov    0x94(%rsp),%ecx
  40b091:	89 8c 24 fc 00 00 00 	mov    %ecx,0xfc(%rsp)
  40b098:	89 84 24 f8 00 00 00 	mov    %eax,0xf8(%rsp)
  40b09f:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  40b0a6:	00 
  40b0a7:	8b 84 24 f8 00 00 00 	mov    0xf8(%rsp),%eax
  40b0ae:	8b 8c 24 fc 00 00 00 	mov    0xfc(%rsp),%ecx
  40b0b5:	89 8c 24 fc 00 00 00 	mov    %ecx,0xfc(%rsp)
  40b0bc:	89 84 24 f8 00 00 00 	mov    %eax,0xf8(%rsp)
  40b0c3:	48 89 32             	mov    %rsi,(%rdx)
  40b0c6:	89 8c 24 84 00 00 00 	mov    %ecx,0x84(%rsp)
  40b0cd:	89 84 24 80 00 00 00 	mov    %eax,0x80(%rsp)
  40b0d4:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40b0db:	00 
  40b0dc:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  40b0e3:	c3                   	ret
  40b0e4:	4c 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%r8
  40b0eb:	00 
  40b0ec:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40b0f1:	4c 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%r9
  40b0f8:	00 
  40b0f9:	4c 89 4c 24 10       	mov    %r9,0x10(%rsp)
  40b0fe:	48 89 e0             	mov    %rsp,%rax
  40b101:	4c 89 08             	mov    %r9,(%rax)
  40b104:	bf c0 fa 40 00       	mov    $0x40fac0,%edi
  40b109:	be 25 00 00 00       	mov    $0x25,%esi
  40b10e:	ba 10 01 00 00       	mov    $0x110,%edx
  40b113:	b9 08 00 00 00       	mov    $0x8,%ecx
  40b118:	e8 43 6c ff ff       	call   401d60 <runtime::slice_expr_error_lo_hi>
  40b11d:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b122:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40b127:	48 8b 8c 24 d0 00 00 	mov    0xd0(%rsp),%rcx
  40b12e:	00 
  40b12f:	48 01 d1             	add    %rdx,%rcx
  40b132:	48 29 d0             	sub    %rdx,%rax
  40b135:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40b13a:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40b13f:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40b144:	48 8b 4c 24 78       	mov    0x78(%rsp),%rcx
  40b149:	48 89 8c 24 d8 00 00 	mov    %rcx,0xd8(%rsp)
  40b150:	00 
  40b151:	48 89 84 24 d0 00 00 	mov    %rax,0xd0(%rsp)
  40b158:	00 
  40b159:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40b160:	00 
  40b161:	48 03 84 24 00 01 00 	add    0x100(%rsp),%rax
  40b168:	00 
  40b169:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  40b170:	00 
  40b171:	48 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%rax
  40b178:	00 
  40b179:	48 03 84 24 c8 00 00 	add    0xc8(%rsp),%rax
  40b180:	00 
  40b181:	48 89 84 24 c8 00 00 	mov    %rax,0xc8(%rsp)
  40b188:	00 
  40b189:	e9 ff fd ff ff       	jmp    40af8d <os::[file_linux.odin]::_write_at+0x18d>
  40b18e:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40b193:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  40b19a:	00 
  40b19b:	8b 84 24 f8 00 00 00 	mov    0xf8(%rsp),%eax
  40b1a2:	8b 8c 24 fc 00 00 00 	mov    0xfc(%rsp),%ecx
  40b1a9:	89 8c 24 fc 00 00 00 	mov    %ecx,0xfc(%rsp)
  40b1b0:	89 84 24 f8 00 00 00 	mov    %eax,0xf8(%rsp)
  40b1b7:	48 89 32             	mov    %rsi,(%rdx)
  40b1ba:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  40b1be:	89 44 24 60          	mov    %eax,0x60(%rsp)
  40b1c2:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40b1c7:	48 81 c4 28 01 00 00 	add    $0x128,%rsp
  40b1ce:	c3                   	ret
  40b1cf:	90                   	nop

000000000040b1d0 <os::[errors_linux.odin]::_get_platform_error>:
  40b1d0:	48 83 ec 38          	sub    $0x38,%rsp
  40b1d4:	89 7c 24 8c          	mov    %edi,-0x74(%rsp)
  40b1d8:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b1dc:	89 44 24 34          	mov    %eax,0x34(%rsp)
  40b1e0:	85 c0                	test   %eax,%eax
  40b1e2:	0f 84 88 00 00 00    	je     40b270 <os::[errors_linux.odin]::_get_platform_error+0xa0>
  40b1e8:	eb 00                	jmp    40b1ea <os::[errors_linux.odin]::_get_platform_error+0x1a>
  40b1ea:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b1ee:	83 e8 01             	sub    $0x1,%eax
  40b1f1:	0f 84 93 00 00 00    	je     40b28a <os::[errors_linux.odin]::_get_platform_error+0xba>
  40b1f7:	eb 00                	jmp    40b1f9 <os::[errors_linux.odin]::_get_platform_error+0x29>
  40b1f9:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b1fd:	83 e8 02             	sub    $0x2,%eax
  40b200:	0f 84 1c 01 00 00    	je     40b322 <os::[errors_linux.odin]::_get_platform_error+0x152>
  40b206:	eb 00                	jmp    40b208 <os::[errors_linux.odin]::_get_platform_error+0x38>
  40b208:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b20c:	83 e8 09             	sub    $0x9,%eax
  40b20f:	0f 84 f4 01 00 00    	je     40b409 <os::[errors_linux.odin]::_get_platform_error+0x239>
  40b215:	eb 00                	jmp    40b217 <os::[errors_linux.odin]::_get_platform_error+0x47>
  40b217:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b21b:	83 e8 0c             	sub    $0xc,%eax
  40b21e:	0f 84 32 02 00 00    	je     40b456 <os::[errors_linux.odin]::_get_platform_error+0x286>
  40b224:	eb 00                	jmp    40b226 <os::[errors_linux.odin]::_get_platform_error+0x56>
  40b226:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b22a:	83 e8 0d             	sub    $0xd,%eax
  40b22d:	74 5b                	je     40b28a <os::[errors_linux.odin]::_get_platform_error+0xba>
  40b22f:	eb 00                	jmp    40b231 <os::[errors_linux.odin]::_get_platform_error+0x61>
  40b231:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b235:	83 e8 11             	sub    $0x11,%eax
  40b238:	0f 84 99 00 00 00    	je     40b2d7 <os::[errors_linux.odin]::_get_platform_error+0x107>
  40b23e:	eb 00                	jmp    40b240 <os::[errors_linux.odin]::_get_platform_error+0x70>
  40b240:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b244:	83 e8 20             	sub    $0x20,%eax
  40b247:	0f 84 6f 01 00 00    	je     40b3bc <os::[errors_linux.odin]::_get_platform_error+0x1ec>
  40b24d:	eb 00                	jmp    40b24f <os::[errors_linux.odin]::_get_platform_error+0x7f>
  40b24f:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b253:	83 e8 26             	sub    $0x26,%eax
  40b256:	0f 84 44 02 00 00    	je     40b4a0 <os::[errors_linux.odin]::_get_platform_error+0x2d0>
  40b25c:	eb 00                	jmp    40b25e <os::[errors_linux.odin]::_get_platform_error+0x8e>
  40b25e:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b262:	83 e8 6e             	sub    $0x6e,%eax
  40b265:	0f 84 04 01 00 00    	je     40b36f <os::[errors_linux.odin]::_get_platform_error+0x19f>
  40b26b:	e9 7d 02 00 00       	jmp    40b4ed <os::[errors_linux.odin]::_get_platform_error+0x31d>
  40b270:	c7 44 24 24 00 00 00 	movl   $0x0,0x24(%rsp)
  40b277:	00 
  40b278:	c7 44 24 20 00 00 00 	movl   $0x0,0x20(%rsp)
  40b27f:	00 
  40b280:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40b285:	48 83 c4 38          	add    $0x38,%rsp
  40b289:	c3                   	ret
  40b28a:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
  40b291:	00 00 
  40b293:	31 c0                	xor    %eax,%eax
  40b295:	a8 01                	test   $0x1,%al
  40b297:	75 02                	jne    40b29b <os::[errors_linux.odin]::_get_platform_error+0xcb>
  40b299:	eb 12                	jmp    40b2ad <os::[errors_linux.odin]::_get_platform_error+0xdd>
  40b29b:	c7 44 24 1c 00 00 00 	movl   $0x0,0x1c(%rsp)
  40b2a2:	00 
  40b2a3:	c7 44 24 18 00 00 00 	movl   $0x0,0x18(%rsp)
  40b2aa:	00 
  40b2ab:	eb 10                	jmp    40b2bd <os::[errors_linux.odin]::_get_platform_error+0xed>
  40b2ad:	c7 44 24 18 10 00 00 	movl   $0x10,0x18(%rsp)
  40b2b4:	00 
  40b2b5:	c7 44 24 1c 02 00 00 	movl   $0x2,0x1c(%rsp)
  40b2bc:	00 
  40b2bd:	8b 44 24 18          	mov    0x18(%rsp),%eax
  40b2c1:	8b 4c 24 1c          	mov    0x1c(%rsp),%ecx
  40b2c5:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  40b2c9:	89 44 24 10          	mov    %eax,0x10(%rsp)
  40b2cd:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b2d2:	48 83 c4 38          	add    $0x38,%rsp
  40b2d6:	c3                   	ret
  40b2d7:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  40b2de:	00 00 
  40b2e0:	31 c0                	xor    %eax,%eax
  40b2e2:	a8 01                	test   $0x1,%al
  40b2e4:	75 02                	jne    40b2e8 <os::[errors_linux.odin]::_get_platform_error+0x118>
  40b2e6:	eb 12                	jmp    40b2fa <os::[errors_linux.odin]::_get_platform_error+0x12a>
  40b2e8:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  40b2ef:	00 
  40b2f0:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40b2f7:	00 
  40b2f8:	eb 10                	jmp    40b30a <os::[errors_linux.odin]::_get_platform_error+0x13a>
  40b2fa:	c7 44 24 08 01 00 00 	movl   $0x1,0x8(%rsp)
  40b301:	00 
  40b302:	c7 44 24 0c 01 00 00 	movl   $0x1,0xc(%rsp)
  40b309:	00 
  40b30a:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40b30e:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40b312:	89 4c 24 04          	mov    %ecx,0x4(%rsp)
  40b316:	89 04 24             	mov    %eax,(%rsp)
  40b319:	48 8b 04 24          	mov    (%rsp),%rax
  40b31d:	48 83 c4 38          	add    $0x38,%rsp
  40b321:	c3                   	ret
  40b322:	48 c7 44 24 f8 00 00 	movq   $0x0,-0x8(%rsp)
  40b329:	00 00 
  40b32b:	31 c0                	xor    %eax,%eax
  40b32d:	a8 01                	test   $0x1,%al
  40b32f:	75 02                	jne    40b333 <os::[errors_linux.odin]::_get_platform_error+0x163>
  40b331:	eb 12                	jmp    40b345 <os::[errors_linux.odin]::_get_platform_error+0x175>
  40b333:	c7 44 24 fc 00 00 00 	movl   $0x0,-0x4(%rsp)
  40b33a:	00 
  40b33b:	c7 44 24 f8 00 00 00 	movl   $0x0,-0x8(%rsp)
  40b342:	00 
  40b343:	eb 10                	jmp    40b355 <os::[errors_linux.odin]::_get_platform_error+0x185>
  40b345:	c7 44 24 f8 02 00 00 	movl   $0x2,-0x8(%rsp)
  40b34c:	00 
  40b34d:	c7 44 24 fc 01 00 00 	movl   $0x1,-0x4(%rsp)
  40b354:	00 
  40b355:	8b 44 24 f8          	mov    -0x8(%rsp),%eax
  40b359:	8b 4c 24 fc          	mov    -0x4(%rsp),%ecx
  40b35d:	89 4c 24 f4          	mov    %ecx,-0xc(%rsp)
  40b361:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40b365:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40b36a:	48 83 c4 38          	add    $0x38,%rsp
  40b36e:	c3                   	ret
  40b36f:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  40b376:	00 00 
  40b378:	31 c0                	xor    %eax,%eax
  40b37a:	a8 01                	test   $0x1,%al
  40b37c:	75 02                	jne    40b380 <os::[errors_linux.odin]::_get_platform_error+0x1b0>
  40b37e:	eb 12                	jmp    40b392 <os::[errors_linux.odin]::_get_platform_error+0x1c2>
  40b380:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  40b387:	00 
  40b388:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  40b38f:	00 
  40b390:	eb 10                	jmp    40b3a2 <os::[errors_linux.odin]::_get_platform_error+0x1d2>
  40b392:	c7 44 24 e8 03 00 00 	movl   $0x3,-0x18(%rsp)
  40b399:	00 
  40b39a:	c7 44 24 ec 01 00 00 	movl   $0x1,-0x14(%rsp)
  40b3a1:	00 
  40b3a2:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  40b3a6:	8b 4c 24 ec          	mov    -0x14(%rsp),%ecx
  40b3aa:	89 4c 24 e4          	mov    %ecx,-0x1c(%rsp)
  40b3ae:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  40b3b2:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40b3b7:	48 83 c4 38          	add    $0x38,%rsp
  40b3bb:	c3                   	ret
  40b3bc:	48 c7 44 24 d8 00 00 	movq   $0x0,-0x28(%rsp)
  40b3c3:	00 00 
  40b3c5:	31 c0                	xor    %eax,%eax
  40b3c7:	a8 01                	test   $0x1,%al
  40b3c9:	75 02                	jne    40b3cd <os::[errors_linux.odin]::_get_platform_error+0x1fd>
  40b3cb:	eb 12                	jmp    40b3df <os::[errors_linux.odin]::_get_platform_error+0x20f>
  40b3cd:	c7 44 24 dc 00 00 00 	movl   $0x0,-0x24(%rsp)
  40b3d4:	00 
  40b3d5:	c7 44 24 d8 00 00 00 	movl   $0x0,-0x28(%rsp)
  40b3dc:	00 
  40b3dd:	eb 10                	jmp    40b3ef <os::[errors_linux.odin]::_get_platform_error+0x21f>
  40b3df:	c7 44 24 d8 04 00 00 	movl   $0x4,-0x28(%rsp)
  40b3e6:	00 
  40b3e7:	c7 44 24 dc 01 00 00 	movl   $0x1,-0x24(%rsp)
  40b3ee:	00 
  40b3ef:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  40b3f3:	8b 4c 24 dc          	mov    -0x24(%rsp),%ecx
  40b3f7:	89 4c 24 d4          	mov    %ecx,-0x2c(%rsp)
  40b3fb:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  40b3ff:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40b404:	48 83 c4 38          	add    $0x38,%rsp
  40b408:	c3                   	ret
  40b409:	48 c7 44 24 c8 00 00 	movq   $0x0,-0x38(%rsp)
  40b410:	00 00 
  40b412:	31 c0                	xor    %eax,%eax
  40b414:	a8 01                	test   $0x1,%al
  40b416:	75 02                	jne    40b41a <os::[errors_linux.odin]::_get_platform_error+0x24a>
  40b418:	eb 12                	jmp    40b42c <os::[errors_linux.odin]::_get_platform_error+0x25c>
  40b41a:	c7 44 24 cc 00 00 00 	movl   $0x0,-0x34(%rsp)
  40b421:	00 
  40b422:	c7 44 24 c8 00 00 00 	movl   $0x0,-0x38(%rsp)
  40b429:	00 
  40b42a:	eb 10                	jmp    40b43c <os::[errors_linux.odin]::_get_platform_error+0x26c>
  40b42c:	c7 44 24 c8 05 00 00 	movl   $0x5,-0x38(%rsp)
  40b433:	00 
  40b434:	c7 44 24 cc 01 00 00 	movl   $0x1,-0x34(%rsp)
  40b43b:	00 
  40b43c:	8b 44 24 c8          	mov    -0x38(%rsp),%eax
  40b440:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  40b444:	89 4c 24 c4          	mov    %ecx,-0x3c(%rsp)
  40b448:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40b44c:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40b451:	48 83 c4 38          	add    $0x38,%rsp
  40b455:	c3                   	ret
  40b456:	48 c7 44 24 b8 00 00 	movq   $0x0,-0x48(%rsp)
  40b45d:	00 00 
  40b45f:	31 c0                	xor    %eax,%eax
  40b461:	a8 01                	test   $0x1,%al
  40b463:	75 02                	jne    40b467 <os::[errors_linux.odin]::_get_platform_error+0x297>
  40b465:	eb 12                	jmp    40b479 <os::[errors_linux.odin]::_get_platform_error+0x2a9>
  40b467:	c7 44 24 bc 00 00 00 	movl   $0x0,-0x44(%rsp)
  40b46e:	00 
  40b46f:	c7 44 24 b8 00 00 00 	movl   $0x0,-0x48(%rsp)
  40b476:	00 
  40b477:	eb 0d                	jmp    40b486 <os::[errors_linux.odin]::_get_platform_error+0x2b6>
  40b479:	c6 44 24 b8 01       	movb   $0x1,-0x48(%rsp)
  40b47e:	c7 44 24 bc 03 00 00 	movl   $0x3,-0x44(%rsp)
  40b485:	00 
  40b486:	8b 44 24 b8          	mov    -0x48(%rsp),%eax
  40b48a:	8b 4c 24 bc          	mov    -0x44(%rsp),%ecx
  40b48e:	89 4c 24 b4          	mov    %ecx,-0x4c(%rsp)
  40b492:	89 44 24 b0          	mov    %eax,-0x50(%rsp)
  40b496:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40b49b:	48 83 c4 38          	add    $0x38,%rsp
  40b49f:	c3                   	ret
  40b4a0:	48 c7 44 24 a8 00 00 	movq   $0x0,-0x58(%rsp)
  40b4a7:	00 00 
  40b4a9:	31 c0                	xor    %eax,%eax
  40b4ab:	a8 01                	test   $0x1,%al
  40b4ad:	75 02                	jne    40b4b1 <os::[errors_linux.odin]::_get_platform_error+0x2e1>
  40b4af:	eb 12                	jmp    40b4c3 <os::[errors_linux.odin]::_get_platform_error+0x2f3>
  40b4b1:	c7 44 24 ac 00 00 00 	movl   $0x0,-0x54(%rsp)
  40b4b8:	00 
  40b4b9:	c7 44 24 a8 00 00 00 	movl   $0x0,-0x58(%rsp)
  40b4c0:	00 
  40b4c1:	eb 10                	jmp    40b4d3 <os::[errors_linux.odin]::_get_platform_error+0x303>
  40b4c3:	c7 44 24 a8 ff ff ff 	movl   $0xffffffff,-0x58(%rsp)
  40b4ca:	ff 
  40b4cb:	c7 44 24 ac 02 00 00 	movl   $0x2,-0x54(%rsp)
  40b4d2:	00 
  40b4d3:	8b 44 24 a8          	mov    -0x58(%rsp),%eax
  40b4d7:	8b 4c 24 ac          	mov    -0x54(%rsp),%ecx
  40b4db:	89 4c 24 a4          	mov    %ecx,-0x5c(%rsp)
  40b4df:	89 44 24 a0          	mov    %eax,-0x60(%rsp)
  40b4e3:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40b4e8:	48 83 c4 38          	add    $0x38,%rsp
  40b4ec:	c3                   	ret
  40b4ed:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b4f1:	48 c7 44 24 98 00 00 	movq   $0x0,-0x68(%rsp)
  40b4f8:	00 00 
  40b4fa:	83 f8 00             	cmp    $0x0,%eax
  40b4fd:	75 12                	jne    40b511 <os::[errors_linux.odin]::_get_platform_error+0x341>
  40b4ff:	c7 44 24 9c 00 00 00 	movl   $0x0,-0x64(%rsp)
  40b506:	00 
  40b507:	c7 44 24 98 00 00 00 	movl   $0x0,-0x68(%rsp)
  40b50e:	00 
  40b50f:	eb 10                	jmp    40b521 <os::[errors_linux.odin]::_get_platform_error+0x351>
  40b511:	8b 44 24 8c          	mov    -0x74(%rsp),%eax
  40b515:	89 44 24 98          	mov    %eax,-0x68(%rsp)
  40b519:	c7 44 24 9c 04 00 00 	movl   $0x4,-0x64(%rsp)
  40b520:	00 
  40b521:	8b 44 24 98          	mov    -0x68(%rsp),%eax
  40b525:	8b 4c 24 9c          	mov    -0x64(%rsp),%ecx
  40b529:	89 4c 24 94          	mov    %ecx,-0x6c(%rsp)
  40b52d:	89 44 24 90          	mov    %eax,-0x70(%rsp)
  40b531:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  40b536:	48 83 c4 38          	add    $0x38,%rsp
  40b53a:	c3                   	ret
  40b53b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040b540 <os::[file_linux.odin]::_file_size>:
  40b540:	48 81 ec 18 01 00 00 	sub    $0x118,%rsp
  40b547:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40b54c:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40b551:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  40b556:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40b55b:	48 89 84 24 10 01 00 	mov    %rax,0x110(%rsp)
  40b562:	00 
  40b563:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  40b56a:	00 00 00 00 00 
  40b56f:	48 c7 84 24 00 01 00 	movq   $0x0,0x100(%rsp)
  40b576:	00 00 00 00 00 
  40b57b:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  40b582:	00 
  40b583:	8b 78 28             	mov    0x28(%rax),%edi
  40b586:	48 8d 74 24 70       	lea    0x70(%rsp),%rsi
  40b58b:	e8 f0 b9 ff ff       	call   406f80 <linux::fstat>
  40b590:	89 44 24 6c          	mov    %eax,0x6c(%rsp)
  40b594:	83 7c 24 6c 00       	cmpl   $0x0,0x6c(%rsp)
  40b599:	0f 95 c0             	setne  %al
  40b59c:	24 01                	and    $0x1,%al
  40b59e:	3c 00                	cmp    $0x0,%al
  40b5a0:	74 56                	je     40b5f8 <os::[file_linux.odin]::_file_size+0xb8>
  40b5a2:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40b5a7:	8b 7c 24 6c          	mov    0x6c(%rsp),%edi
  40b5ab:	e8 20 fc ff ff       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40b5b0:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40b5b5:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40b5ba:	8b 44 24 60          	mov    0x60(%rsp),%eax
  40b5be:	8b 4c 24 64          	mov    0x64(%rsp),%ecx
  40b5c2:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  40b5c9:	00 00 00 00 00 
  40b5ce:	89 8c 24 04 01 00 00 	mov    %ecx,0x104(%rsp)
  40b5d5:	89 84 24 00 01 00 00 	mov    %eax,0x100(%rsp)
  40b5dc:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40b5e3:	89 4c 24 54          	mov    %ecx,0x54(%rsp)
  40b5e7:	89 44 24 50          	mov    %eax,0x50(%rsp)
  40b5eb:	48 8b 44 24 50       	mov    0x50(%rsp),%rax
  40b5f0:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40b5f7:	c3                   	ret
  40b5f8:	8b 84 24 88 00 00 00 	mov    0x88(%rsp),%eax
  40b5ff:	25 00 f0 00 00       	and    $0xf000,%eax
  40b604:	3d 00 80 00 00       	cmp    $0x8000,%eax
  40b609:	0f 94 c0             	sete   %al
  40b60c:	24 01                	and    $0x1,%al
  40b60e:	3c 00                	cmp    $0x0,%al
  40b610:	74 4b                	je     40b65d <os::[file_linux.odin]::_file_size+0x11d>
  40b612:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b617:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  40b61e:	00 
  40b61f:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  40b626:	00 
  40b627:	c7 84 24 04 01 00 00 	movl   $0x0,0x104(%rsp)
  40b62e:	00 00 00 00 
  40b632:	c7 84 24 00 01 00 00 	movl   $0x0,0x100(%rsp)
  40b639:	00 00 00 00 
  40b63d:	48 89 08             	mov    %rcx,(%rax)
  40b640:	c7 44 24 44 00 00 00 	movl   $0x0,0x44(%rsp)
  40b647:	00 
  40b648:	c7 44 24 40 00 00 00 	movl   $0x0,0x40(%rsp)
  40b64f:	00 
  40b650:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40b655:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40b65c:	c3                   	ret
  40b65d:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  40b664:	00 00 
  40b666:	31 c0                	xor    %eax,%eax
  40b668:	a8 01                	test   $0x1,%al
  40b66a:	75 02                	jne    40b66e <os::[file_linux.odin]::_file_size+0x12e>
  40b66c:	eb 12                	jmp    40b680 <os::[file_linux.odin]::_file_size+0x140>
  40b66e:	c7 44 24 3c 00 00 00 	movl   $0x0,0x3c(%rsp)
  40b675:	00 
  40b676:	c7 44 24 38 00 00 00 	movl   $0x0,0x38(%rsp)
  40b67d:	00 
  40b67e:	eb 10                	jmp    40b690 <os::[file_linux.odin]::_file_size+0x150>
  40b680:	c7 44 24 38 0f 00 00 	movl   $0xf,0x38(%rsp)
  40b687:	00 
  40b688:	c7 44 24 3c 02 00 00 	movl   $0x2,0x3c(%rsp)
  40b68f:	00 
  40b690:	8b 44 24 38          	mov    0x38(%rsp),%eax
  40b694:	8b 4c 24 3c          	mov    0x3c(%rsp),%ecx
  40b698:	48 c7 84 24 08 01 00 	movq   $0x0,0x108(%rsp)
  40b69f:	00 00 00 00 00 
  40b6a4:	89 8c 24 04 01 00 00 	mov    %ecx,0x104(%rsp)
  40b6ab:	89 84 24 00 01 00 00 	mov    %eax,0x100(%rsp)
  40b6b2:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
  40b6b9:	00 00 
  40b6bb:	31 c0                	xor    %eax,%eax
  40b6bd:	a8 01                	test   $0x1,%al
  40b6bf:	75 02                	jne    40b6c3 <os::[file_linux.odin]::_file_size+0x183>
  40b6c1:	eb 12                	jmp    40b6d5 <os::[file_linux.odin]::_file_size+0x195>
  40b6c3:	c7 44 24 34 00 00 00 	movl   $0x0,0x34(%rsp)
  40b6ca:	00 
  40b6cb:	c7 44 24 30 00 00 00 	movl   $0x0,0x30(%rsp)
  40b6d2:	00 
  40b6d3:	eb 10                	jmp    40b6e5 <os::[file_linux.odin]::_file_size+0x1a5>
  40b6d5:	c7 44 24 30 0f 00 00 	movl   $0xf,0x30(%rsp)
  40b6dc:	00 
  40b6dd:	c7 44 24 34 02 00 00 	movl   $0x2,0x34(%rsp)
  40b6e4:	00 
  40b6e5:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40b6ea:	8b 44 24 30          	mov    0x30(%rsp),%eax
  40b6ee:	8b 4c 24 34          	mov    0x34(%rsp),%ecx
  40b6f2:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40b6f9:	89 4c 24 24          	mov    %ecx,0x24(%rsp)
  40b6fd:	89 44 24 20          	mov    %eax,0x20(%rsp)
  40b701:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40b706:	48 81 c4 18 01 00 00 	add    $0x118,%rsp
  40b70d:	c3                   	ret
  40b70e:	66 90                	xchg   %ax,%ax

000000000040b710 <os::[file_linux.odin]::_flush>:
  40b710:	48 83 ec 38          	sub    $0x38,%rsp
  40b714:	48 89 3c 24          	mov    %rdi,(%rsp)
  40b718:	48 89 74 24 08       	mov    %rsi,0x8(%rsp)
  40b71d:	48 8b 04 24          	mov    (%rsp),%rax
  40b721:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
  40b726:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40b72b:	8b 78 28             	mov    0x28(%rax),%edi
  40b72e:	e8 fd ba ff ff       	call   407230 <linux::fsync>
  40b733:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40b738:	89 c7                	mov    %eax,%edi
  40b73a:	e8 91 fa ff ff       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40b73f:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40b744:	8b 44 24 20          	mov    0x20(%rsp),%eax
  40b748:	8b 4c 24 24          	mov    0x24(%rsp),%ecx
  40b74c:	89 4c 24 14          	mov    %ecx,0x14(%rsp)
  40b750:	89 44 24 10          	mov    %eax,0x10(%rsp)
  40b754:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b759:	48 83 c4 38          	add    $0x38,%rsp
  40b75d:	c3                   	ret
  40b75e:	66 90                	xchg   %ax,%ax

000000000040b760 <os::[file_linux.odin]::_read_link_cstr>:
  40b760:	48 81 ec 38 01 00 00 	sub    $0x138,%rsp
  40b767:	4c 89 44 24 18       	mov    %r8,0x18(%rsp)
  40b76c:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40b771:	48 89 7c 24 28       	mov    %rdi,0x28(%rsp)
  40b776:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40b77b:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40b780:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40b785:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
  40b78a:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40b78f:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40b794:	48 89 94 24 30 01 00 	mov    %rdx,0x130(%rsp)
  40b79b:	00 
  40b79c:	48 89 8c 24 28 01 00 	mov    %rcx,0x128(%rsp)
  40b7a3:	00 
  40b7a4:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40b7ab:	00 
  40b7ac:	48 8b 84 24 20 01 00 	mov    0x120(%rsp),%rax
  40b7b3:	00 
  40b7b4:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  40b7b9:	48 8b 8c 24 28 01 00 	mov    0x128(%rsp),%rcx
  40b7c0:	00 
  40b7c1:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40b7c6:	48 c7 84 24 18 01 00 	movq   $0x100,0x118(%rsp)
  40b7cd:	00 00 01 00 00 
  40b7d2:	48 8b bc 24 18 01 00 	mov    0x118(%rsp),%rdi
  40b7d9:	00 
  40b7da:	48 89 8c 24 08 01 00 	mov    %rcx,0x108(%rsp)
  40b7e1:	00 
  40b7e2:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  40b7e9:	00 
  40b7ea:	48 8b b4 24 00 01 00 	mov    0x100(%rsp),%rsi
  40b7f1:	00 
  40b7f2:	48 8b 94 24 08 01 00 	mov    0x108(%rsp),%rdx
  40b7f9:	00 
  40b7fa:	0f 57 c0             	xorps  %xmm0,%xmm0
  40b7fd:	0f 29 84 24 f0 00 00 	movaps %xmm0,0xf0(%rsp)
  40b804:	00 
  40b805:	b9 00 fc 40 00       	mov    $0x40fc00,%ecx
  40b80a:	4c 8d 84 24 f0 00 00 	lea    0xf0(%rsp),%r8
  40b811:	00 
  40b812:	e8 c9 5d ff ff       	call   4015e0 <runtime::make_slice:proc(T:$[]u8,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]u8,err:runtime::Allocator_Error)>
  40b817:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40b81e:	00 
  40b81f:	48 8b 8c 24 f8 00 00 	mov    0xf8(%rsp),%rcx
  40b826:	00 
  40b827:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  40b82e:	00 
  40b82f:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  40b836:	00 
  40b837:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40b83c:	48 8b b4 24 e0 00 00 	mov    0xe0(%rsp),%rsi
  40b843:	00 
  40b844:	48 8b 94 24 e8 00 00 	mov    0xe8(%rsp),%rdx
  40b84b:	00 
  40b84c:	48 c7 84 24 d8 00 00 	movq   $0x0,0xd8(%rsp)
  40b853:	00 00 00 00 00 
  40b858:	48 8d 8c 24 d8 00 00 	lea    0xd8(%rsp),%rcx
  40b85f:	00 
  40b860:	e8 fb b9 ff ff       	call   407260 <linux::readlink>
  40b865:	48 8b 8c 24 d8 00 00 	mov    0xd8(%rsp),%rcx
  40b86c:	00 
  40b86d:	48 89 8c 24 d0 00 00 	mov    %rcx,0xd0(%rsp)
  40b874:	00 
  40b875:	89 84 24 cc 00 00 00 	mov    %eax,0xcc(%rsp)
  40b87c:	83 bc 24 cc 00 00 00 	cmpl   $0x0,0xcc(%rsp)
  40b883:	00 
  40b884:	0f 95 c0             	setne  %al
  40b887:	24 01                	and    $0x1,%al
  40b889:	3c 00                	cmp    $0x0,%al
  40b88b:	0f 84 9a 00 00 00    	je     40b92b <os::[file_linux.odin]::_read_link_cstr+0x1cb>
  40b891:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40b896:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40b89b:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40b8a0:	48 8b bc 24 e0 00 00 	mov    0xe0(%rsp),%rdi
  40b8a7:	00 
  40b8a8:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  40b8af:	00 
  40b8b0:	48 89 8c 24 b0 00 00 	mov    %rcx,0xb0(%rsp)
  40b8b7:	00 
  40b8b8:	48 89 84 24 b8 00 00 	mov    %rax,0xb8(%rsp)
  40b8bf:	00 
  40b8c0:	48 8b 94 24 b0 00 00 	mov    0xb0(%rsp),%rdx
  40b8c7:	00 
  40b8c8:	48 8b 8c 24 b8 00 00 	mov    0xb8(%rsp),%rcx
  40b8cf:	00 
  40b8d0:	41 b8 30 fc 40 00    	mov    $0x40fc30,%r8d
  40b8d6:	e8 65 5a ff ff       	call   401340 <runtime::delete_slice:proc(array:[]u8,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  40b8db:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40b8e0:	8b bc 24 cc 00 00 00 	mov    0xcc(%rsp),%edi
  40b8e7:	e8 e4 f8 ff ff       	call   40b1d0 <os::[errors_linux.odin]::_get_platform_error>
  40b8ec:	48 8b 54 24 20       	mov    0x20(%rsp),%rdx
  40b8f1:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40b8f8:	00 
  40b8f9:	8b 84 24 a0 00 00 00 	mov    0xa0(%rsp),%eax
  40b900:	8b 8c 24 a4 00 00 00 	mov    0xa4(%rsp),%ecx
  40b907:	0f 57 c0             	xorps  %xmm0,%xmm0
  40b90a:	0f 11 02             	movups %xmm0,(%rdx)
  40b90d:	89 8c 24 94 00 00 00 	mov    %ecx,0x94(%rsp)
  40b914:	89 84 24 90 00 00 00 	mov    %eax,0x90(%rsp)
  40b91b:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40b922:	00 
  40b923:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40b92a:	c3                   	ret
  40b92b:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  40b932:	00 
  40b933:	48 3b 84 24 18 01 00 	cmp    0x118(%rsp),%rax
  40b93a:	00 
  40b93b:	0f 94 c0             	sete   %al
  40b93e:	24 01                	and    $0x1,%al
  40b940:	3c 00                	cmp    $0x0,%al
  40b942:	0f 84 bb 00 00 00    	je     40ba03 <os::[file_linux.odin]::_read_link_cstr+0x2a3>
  40b948:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40b94d:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40b952:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40b957:	48 8b 94 24 18 01 00 	mov    0x118(%rsp),%rdx
  40b95e:	00 
  40b95f:	48 01 d2             	add    %rdx,%rdx
  40b962:	48 89 94 24 18 01 00 	mov    %rdx,0x118(%rsp)
  40b969:	00 
  40b96a:	48 8b bc 24 e0 00 00 	mov    0xe0(%rsp),%rdi
  40b971:	00 
  40b972:	48 8b b4 24 e8 00 00 	mov    0xe8(%rsp),%rsi
  40b979:	00 
  40b97a:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40b981:	00 
  40b982:	48 89 84 24 80 00 00 	mov    %rax,0x80(%rsp)
  40b989:	00 
  40b98a:	48 8b 94 24 80 00 00 	mov    0x80(%rsp),%rdx
  40b991:	00 
  40b992:	48 8b 8c 24 88 00 00 	mov    0x88(%rsp),%rcx
  40b999:	00 
  40b99a:	41 b8 60 fc 40 00    	mov    $0x40fc60,%r8d
  40b9a0:	e8 9b 59 ff ff       	call   401340 <runtime::delete_slice:proc(array:[]u8,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(:runtime::Allocator_Error)>
  40b9a5:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40b9aa:	4c 8b 4c 24 18       	mov    0x18(%rsp),%r9
  40b9af:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40b9b4:	48 8b bc 24 18 01 00 	mov    0x118(%rsp),%rdi
  40b9bb:	00 
  40b9bc:	48 89 4c 24 78       	mov    %rcx,0x78(%rsp)
  40b9c1:	48 89 44 24 70       	mov    %rax,0x70(%rsp)
  40b9c6:	48 8b 74 24 70       	mov    0x70(%rsp),%rsi
  40b9cb:	48 8b 54 24 78       	mov    0x78(%rsp),%rdx
  40b9d0:	0f 57 c0             	xorps  %xmm0,%xmm0
  40b9d3:	0f 29 44 24 60       	movaps %xmm0,0x60(%rsp)
  40b9d8:	b9 90 fc 40 00       	mov    $0x40fc90,%ecx
  40b9dd:	4c 8d 44 24 60       	lea    0x60(%rsp),%r8
  40b9e2:	e8 f9 5b ff ff       	call   4015e0 <runtime::make_slice:proc(T:$[]u8,len:int,allocator:runtime::Allocator,loc:runtime::Source_Code_Location)->(res:[]u8,err:runtime::Allocator_Error)>
  40b9e7:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40b9ec:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40b9f1:	48 89 8c 24 e8 00 00 	mov    %rcx,0xe8(%rsp)
  40b9f8:	00 
  40b9f9:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  40ba00:	00 
  40ba01:	eb 76                	jmp    40ba79 <os::[file_linux.odin]::_read_link_cstr+0x319>
  40ba03:	4c 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%r8
  40ba0a:	00 
  40ba0b:	4c 89 04 24          	mov    %r8,(%rsp)
  40ba0f:	4c 8b 8c 24 e8 00 00 	mov    0xe8(%rsp),%r9
  40ba16:	00 
  40ba17:	bf c0 fa 40 00       	mov    $0x40fac0,%edi
  40ba1c:	be 25 00 00 00       	mov    $0x25,%esi
  40ba21:	ba 66 01 00 00       	mov    $0x166,%edx
  40ba26:	b9 15 00 00 00       	mov    $0x15,%ecx
  40ba2b:	e8 80 62 ff ff       	call   401cb0 <runtime::slice_expr_error_hi>
  40ba30:	48 8b 0c 24          	mov    (%rsp),%rcx
  40ba34:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40ba39:	48 8b 94 24 e0 00 00 	mov    0xe0(%rsp),%rdx
  40ba40:	00 
  40ba41:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40ba46:	48 89 4c 24 58       	mov    %rcx,0x58(%rsp)
  40ba4b:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40ba50:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  40ba55:	48 89 50 08          	mov    %rdx,0x8(%rax)
  40ba59:	48 89 08             	mov    %rcx,(%rax)
  40ba5c:	c7 44 24 44 00 00 00 	movl   $0x0,0x44(%rsp)
  40ba63:	00 
  40ba64:	c7 44 24 40 00 00 00 	movl   $0x0,0x40(%rsp)
  40ba6b:	00 
  40ba6c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40ba71:	48 81 c4 38 01 00 00 	add    $0x138,%rsp
  40ba78:	c3                   	ret
  40ba79:	eb 00                	jmp    40ba7b <os::[file_linux.odin]::_read_link_cstr+0x31b>
  40ba7b:	e9 b7 fd ff ff       	jmp    40b837 <os::[file_linux.odin]::_read_link_cstr+0xd7>

000000000040ba80 <os::split_path>:
  40ba80:	48 83 ec 78          	sub    $0x78,%rsp
  40ba84:	48 89 0c 24          	mov    %rcx,(%rsp)
  40ba88:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40ba8d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40ba92:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40ba97:	48 8b 0c 24          	mov    (%rsp),%rcx
  40ba9b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40baa0:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40baa5:	48 89 7c 24 68       	mov    %rdi,0x68(%rsp)
  40baaa:	48 89 74 24 70       	mov    %rsi,0x70(%rsp)
  40baaf:	0f 57 c0             	xorps  %xmm0,%xmm0
  40bab2:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40bab7:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40babc:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40bac1:	48 8d 54 24 30       	lea    0x30(%rsp),%rdx
  40bac6:	e8 25 db ff ff       	call   4095f0 <os::[path_posixfs.odin]::_split_path>
  40bacb:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40bad0:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
  40bad5:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
  40bada:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  40badf:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  40bae4:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40bae9:	48 89 44 24 40       	mov    %rax,0x40(%rsp)
  40baee:	48 89 79 08          	mov    %rdi,0x8(%rcx)
  40baf2:	48 89 31             	mov    %rsi,(%rcx)
  40baf5:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40bafa:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40baff:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40bb04:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40bb09:	48 83 c4 78          	add    $0x78,%rsp
  40bb0d:	c3                   	ret
  40bb0e:	66 90                	xchg   %ax,%ax

000000000040bb10 <os::[file_linux.odin]::_file_stream_proc>:
  40bb10:	48 81 ec 38 02 00 00 	sub    $0x238,%rsp
  40bb17:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40bb1c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40bb21:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40bb26:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40bb2b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40bb30:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40bb35:	48 8b 84 24 58 02 00 	mov    0x258(%rsp),%rax
  40bb3c:	00 
  40bb3d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40bb42:	48 8b 84 24 50 02 00 	mov    0x250(%rsp),%rax
  40bb49:	00 
  40bb4a:	48 89 44 24 50       	mov    %rax,0x50(%rsp)
  40bb4f:	48 8d 84 24 40 02 00 	lea    0x240(%rsp),%rax
  40bb56:	00 
  40bb57:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40bb5c:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40bb61:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40bb66:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  40bb6b:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40bb70:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40bb75:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  40bb7a:	4c 8b 4c 24 38       	mov    0x38(%rsp),%r9
  40bb7f:	48 89 8c 24 30 02 00 	mov    %rcx,0x230(%rsp)
  40bb86:	00 
  40bb87:	48 89 84 24 28 02 00 	mov    %rax,0x228(%rsp)
  40bb8e:	00 
  40bb8f:	4c 89 8c 24 20 02 00 	mov    %r9,0x220(%rsp)
  40bb96:	00 
  40bb97:	4c 89 84 24 18 02 00 	mov    %r8,0x218(%rsp)
  40bb9e:	00 
  40bb9f:	48 89 bc 24 10 02 00 	mov    %rdi,0x210(%rsp)
  40bba6:	00 
  40bba7:	48 89 b4 24 08 02 00 	mov    %rsi,0x208(%rsp)
  40bbae:	00 
  40bbaf:	0f 10 02             	movups (%rdx),%xmm0
  40bbb2:	0f 29 84 24 f0 01 00 	movaps %xmm0,0x1f0(%rsp)
  40bbb9:	00 
  40bbba:	48 c7 84 24 e8 01 00 	movq   $0x0,0x1e8(%rsp)
  40bbc1:	00 00 00 00 00 
  40bbc6:	48 c7 84 24 e0 01 00 	movq   $0x0,0x1e0(%rsp)
  40bbcd:	00 00 00 00 00 
  40bbd2:	48 89 8c 24 d8 01 00 	mov    %rcx,0x1d8(%rsp)
  40bbd9:	00 
  40bbda:	48 89 c1             	mov    %rax,%rcx
  40bbdd:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40bbe2:	48 83 e8 0a          	sub    $0xa,%rax
  40bbe6:	0f 87 f0 06 00 00    	ja     40c2dc <os::[file_linux.odin]::_file_stream_proc+0x7cc>
  40bbec:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40bbf1:	48 8b 04 c5 40 f9 40 	mov    0x40f940(,%rax,8),%rax
  40bbf8:	00 
  40bbf9:	ff e0                	jmp    *%rax
  40bbfb:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40bc00:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40bc05:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40bc0a:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40bc11:	00 
  40bc12:	48 c7 84 24 d0 01 00 	movq   $0x0,0x1d0(%rsp)
  40bc19:	00 00 00 00 00 
  40bc1e:	48 8d 8c 24 d0 01 00 	lea    0x1d0(%rsp),%rcx
  40bc25:	00 
  40bc26:	e8 65 ea ff ff       	call   40a690 <os::[file_linux.odin]::_read>
  40bc2b:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40bc30:	48 89 84 24 c0 01 00 	mov    %rax,0x1c0(%rsp)
  40bc37:	00 
  40bc38:	8b 84 24 c0 01 00 00 	mov    0x1c0(%rsp),%eax
  40bc3f:	8b 8c 24 c4 01 00 00 	mov    0x1c4(%rsp),%ecx
  40bc46:	48 8b b4 24 d0 01 00 	mov    0x1d0(%rsp),%rsi
  40bc4d:	00 
  40bc4e:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  40bc55:	00 
  40bc56:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bc5d:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bc64:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40bc6b:	00 
  40bc6c:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40bc73:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40bc7a:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bc81:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bc88:	48 89 32             	mov    %rsi,(%rdx)
  40bc8b:	89 8c 24 b4 01 00 00 	mov    %ecx,0x1b4(%rsp)
  40bc92:	89 84 24 b0 01 00 00 	mov    %eax,0x1b0(%rsp)
  40bc99:	48 8b 84 24 b0 01 00 	mov    0x1b0(%rsp),%rax
  40bca0:	00 
  40bca1:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40bca8:	c3                   	ret
  40bca9:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40bcae:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40bcb3:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40bcb8:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40bcbd:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40bcc4:	00 
  40bcc5:	48 c7 84 24 a8 01 00 	movq   $0x0,0x1a8(%rsp)
  40bccc:	00 00 00 00 00 
  40bcd1:	4c 8d 84 24 a8 01 00 	lea    0x1a8(%rsp),%r8
  40bcd8:	00 
  40bcd9:	e8 d2 eb ff ff       	call   40a8b0 <os::[file_linux.odin]::_read_at>
  40bcde:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40bce3:	48 89 84 24 a0 01 00 	mov    %rax,0x1a0(%rsp)
  40bcea:	00 
  40bceb:	8b 84 24 a0 01 00 00 	mov    0x1a0(%rsp),%eax
  40bcf2:	8b 8c 24 a4 01 00 00 	mov    0x1a4(%rsp),%ecx
  40bcf9:	48 8b b4 24 a8 01 00 	mov    0x1a8(%rsp),%rsi
  40bd00:	00 
  40bd01:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  40bd08:	00 
  40bd09:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bd10:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bd17:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40bd1e:	00 
  40bd1f:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40bd26:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40bd2d:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bd34:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bd3b:	48 89 32             	mov    %rsi,(%rdx)
  40bd3e:	89 8c 24 94 01 00 00 	mov    %ecx,0x194(%rsp)
  40bd45:	89 84 24 90 01 00 00 	mov    %eax,0x190(%rsp)
  40bd4c:	48 8b 84 24 90 01 00 	mov    0x190(%rsp),%rax
  40bd53:	00 
  40bd54:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40bd5b:	c3                   	ret
  40bd5c:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40bd61:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40bd66:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40bd6b:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40bd72:	00 
  40bd73:	48 c7 84 24 88 01 00 	movq   $0x0,0x188(%rsp)
  40bd7a:	00 00 00 00 00 
  40bd7f:	48 8d 8c 24 88 01 00 	lea    0x188(%rsp),%rcx
  40bd86:	00 
  40bd87:	e8 e4 ed ff ff       	call   40ab70 <os::[file_linux.odin]::_write>
  40bd8c:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40bd91:	48 89 84 24 80 01 00 	mov    %rax,0x180(%rsp)
  40bd98:	00 
  40bd99:	8b 84 24 80 01 00 00 	mov    0x180(%rsp),%eax
  40bda0:	8b 8c 24 84 01 00 00 	mov    0x184(%rsp),%ecx
  40bda7:	48 8b b4 24 88 01 00 	mov    0x188(%rsp),%rsi
  40bdae:	00 
  40bdaf:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  40bdb6:	00 
  40bdb7:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bdbe:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bdc5:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40bdcc:	00 
  40bdcd:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40bdd4:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40bddb:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bde2:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bde9:	48 89 32             	mov    %rsi,(%rdx)
  40bdec:	89 8c 24 74 01 00 00 	mov    %ecx,0x174(%rsp)
  40bdf3:	89 84 24 70 01 00 00 	mov    %eax,0x170(%rsp)
  40bdfa:	48 8b 84 24 70 01 00 	mov    0x170(%rsp),%rax
  40be01:	00 
  40be02:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40be09:	c3                   	ret
  40be0a:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40be0f:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40be14:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40be19:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40be1e:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40be25:	00 
  40be26:	48 c7 84 24 68 01 00 	movq   $0x0,0x168(%rsp)
  40be2d:	00 00 00 00 00 
  40be32:	4c 8d 84 24 68 01 00 	lea    0x168(%rsp),%r8
  40be39:	00 
  40be3a:	e8 c1 ef ff ff       	call   40ae00 <os::[file_linux.odin]::_write_at>
  40be3f:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40be44:	48 89 84 24 60 01 00 	mov    %rax,0x160(%rsp)
  40be4b:	00 
  40be4c:	8b 84 24 60 01 00 00 	mov    0x160(%rsp),%eax
  40be53:	8b 8c 24 64 01 00 00 	mov    0x164(%rsp),%ecx
  40be5a:	48 8b b4 24 68 01 00 	mov    0x168(%rsp),%rsi
  40be61:	00 
  40be62:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  40be69:	00 
  40be6a:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40be71:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40be78:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40be7f:	00 
  40be80:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40be87:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40be8e:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40be95:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40be9c:	48 89 32             	mov    %rsi,(%rdx)
  40be9f:	89 8c 24 54 01 00 00 	mov    %ecx,0x154(%rsp)
  40bea6:	89 84 24 50 01 00 00 	mov    %eax,0x150(%rsp)
  40bead:	48 8b 84 24 50 01 00 	mov    0x150(%rsp),%rax
  40beb4:	00 
  40beb5:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40bebc:	c3                   	ret
  40bebd:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40bec2:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40bec7:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40becc:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40bed3:	00 
  40bed4:	48 c7 84 24 48 01 00 	movq   $0x0,0x148(%rsp)
  40bedb:	00 00 00 00 00 
  40bee0:	48 8d 8c 24 48 01 00 	lea    0x148(%rsp),%rcx
  40bee7:	00 
  40bee8:	e8 c3 e1 ff ff       	call   40a0b0 <os::[file_linux.odin]::_seek>
  40beed:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40bef2:	48 89 84 24 40 01 00 	mov    %rax,0x140(%rsp)
  40bef9:	00 
  40befa:	8b 84 24 40 01 00 00 	mov    0x140(%rsp),%eax
  40bf01:	8b 8c 24 44 01 00 00 	mov    0x144(%rsp),%ecx
  40bf08:	48 8b b4 24 48 01 00 	mov    0x148(%rsp),%rsi
  40bf0f:	00 
  40bf10:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  40bf17:	00 
  40bf18:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bf1f:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bf26:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40bf2d:	00 
  40bf2e:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40bf35:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40bf3c:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bf43:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bf4a:	48 89 32             	mov    %rsi,(%rdx)
  40bf4d:	89 8c 24 34 01 00 00 	mov    %ecx,0x134(%rsp)
  40bf54:	89 84 24 30 01 00 00 	mov    %eax,0x130(%rsp)
  40bf5b:	48 8b 84 24 30 01 00 	mov    0x130(%rsp),%rax
  40bf62:	00 
  40bf63:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40bf6a:	c3                   	ret
  40bf6b:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40bf70:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40bf77:	00 
  40bf78:	48 c7 84 24 28 01 00 	movq   $0x0,0x128(%rsp)
  40bf7f:	00 00 00 00 00 
  40bf84:	48 8d b4 24 28 01 00 	lea    0x128(%rsp),%rsi
  40bf8b:	00 
  40bf8c:	e8 af f5 ff ff       	call   40b540 <os::[file_linux.odin]::_file_size>
  40bf91:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40bf96:	48 89 84 24 20 01 00 	mov    %rax,0x120(%rsp)
  40bf9d:	00 
  40bf9e:	8b 84 24 20 01 00 00 	mov    0x120(%rsp),%eax
  40bfa5:	8b 8c 24 24 01 00 00 	mov    0x124(%rsp),%ecx
  40bfac:	48 8b b4 24 28 01 00 	mov    0x128(%rsp),%rsi
  40bfb3:	00 
  40bfb4:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  40bfbb:	00 
  40bfbc:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bfc3:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bfca:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40bfd1:	00 
  40bfd2:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40bfd9:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40bfe0:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40bfe7:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40bfee:	48 89 32             	mov    %rsi,(%rdx)
  40bff1:	89 8c 24 14 01 00 00 	mov    %ecx,0x114(%rsp)
  40bff8:	89 84 24 10 01 00 00 	mov    %eax,0x110(%rsp)
  40bfff:	48 8b 84 24 10 01 00 	mov    0x110(%rsp),%rax
  40c006:	00 
  40c007:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40c00e:	c3                   	ret
  40c00f:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40c014:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40c01b:	00 
  40c01c:	e8 ef f6 ff ff       	call   40b710 <os::[file_linux.odin]::_flush>
  40c021:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c026:	48 89 84 24 00 01 00 	mov    %rax,0x100(%rsp)
  40c02d:	00 
  40c02e:	8b 84 24 00 01 00 00 	mov    0x100(%rsp),%eax
  40c035:	8b 8c 24 04 01 00 00 	mov    0x104(%rsp),%ecx
  40c03c:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40c043:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40c04a:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40c051:	00 
  40c052:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40c059:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40c060:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40c067:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40c06e:	48 89 32             	mov    %rsi,(%rdx)
  40c071:	89 8c 24 f4 00 00 00 	mov    %ecx,0xf4(%rsp)
  40c078:	89 84 24 f0 00 00 00 	mov    %eax,0xf0(%rsp)
  40c07f:	48 8b 84 24 f0 00 00 	mov    0xf0(%rsp),%rax
  40c086:	00 
  40c087:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40c08e:	c3                   	ret
  40c08f:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40c094:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40c09b:	00 
  40c09c:	e8 3f df ff ff       	call   409fe0 <os::[file_linux.odin]::_close>
  40c0a1:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c0a6:	48 89 84 24 e0 00 00 	mov    %rax,0xe0(%rsp)
  40c0ad:	00 
  40c0ae:	8b 84 24 e0 00 00 00 	mov    0xe0(%rsp),%eax
  40c0b5:	8b 8c 24 e4 00 00 00 	mov    0xe4(%rsp),%ecx
  40c0bc:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40c0c3:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40c0ca:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40c0d1:	00 
  40c0d2:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40c0d9:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40c0e0:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40c0e7:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40c0ee:	48 89 32             	mov    %rsi,(%rdx)
  40c0f1:	89 8c 24 d4 00 00 00 	mov    %ecx,0xd4(%rsp)
  40c0f8:	89 84 24 d0 00 00 00 	mov    %eax,0xd0(%rsp)
  40c0ff:	48 8b 84 24 d0 00 00 	mov    0xd0(%rsp),%rax
  40c106:	00 
  40c107:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40c10e:	c3                   	ret
  40c10f:	48 c7 84 24 c8 00 00 	movq   $0x0,0xc8(%rsp)
  40c116:	00 00 00 00 00 
  40c11b:	bf ff 03 00 00       	mov    $0x3ff,%edi
  40c120:	48 8d b4 24 c8 00 00 	lea    0xc8(%rsp),%rsi
  40c127:	00 
  40c128:	e8 93 7f ff ff       	call   4040c0 <io::query_utility>
  40c12d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  40c131:	48 8b 8c 24 c8 00 00 	mov    0xc8(%rsp),%rcx
  40c138:	00 
  40c139:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40c13e:	48 c7 84 24 c0 00 00 	movq   $0x0,0xc0(%rsp)
  40c145:	00 00 00 00 00 
  40c14a:	83 f8 00             	cmp    $0x0,%eax
  40c14d:	75 18                	jne    40c167 <os::[file_linux.odin]::_file_stream_proc+0x657>
  40c14f:	c7 84 24 c4 00 00 00 	movl   $0x0,0xc4(%rsp)
  40c156:	00 00 00 00 
  40c15a:	c7 84 24 c0 00 00 00 	movl   $0x0,0xc0(%rsp)
  40c161:	00 00 00 00 
  40c165:	eb 16                	jmp    40c17d <os::[file_linux.odin]::_file_stream_proc+0x66d>
  40c167:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40c16b:	89 84 24 c0 00 00 00 	mov    %eax,0xc0(%rsp)
  40c172:	c7 84 24 c4 00 00 00 	movl   $0x2,0xc4(%rsp)
  40c179:	02 00 00 00 
  40c17d:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40c181:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40c186:	8b 8c 24 c0 00 00 00 	mov    0xc0(%rsp),%ecx
  40c18d:	8b 94 24 c4 00 00 00 	mov    0xc4(%rsp),%edx
  40c194:	48 89 b4 24 e8 01 00 	mov    %rsi,0x1e8(%rsp)
  40c19b:	00 
  40c19c:	89 94 24 e4 01 00 00 	mov    %edx,0x1e4(%rsp)
  40c1a3:	89 8c 24 e0 01 00 00 	mov    %ecx,0x1e0(%rsp)
  40c1aa:	48 c7 84 24 b8 00 00 	movq   $0x0,0xb8(%rsp)
  40c1b1:	00 00 00 00 00 
  40c1b6:	83 f8 00             	cmp    $0x0,%eax
  40c1b9:	75 18                	jne    40c1d3 <os::[file_linux.odin]::_file_stream_proc+0x6c3>
  40c1bb:	c7 84 24 bc 00 00 00 	movl   $0x0,0xbc(%rsp)
  40c1c2:	00 00 00 00 
  40c1c6:	c7 84 24 b8 00 00 00 	movl   $0x0,0xb8(%rsp)
  40c1cd:	00 00 00 00 
  40c1d1:	eb 16                	jmp    40c1e9 <os::[file_linux.odin]::_file_stream_proc+0x6d9>
  40c1d3:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40c1d7:	89 84 24 b8 00 00 00 	mov    %eax,0xb8(%rsp)
  40c1de:	c7 84 24 bc 00 00 00 	movl   $0x2,0xbc(%rsp)
  40c1e5:	02 00 00 00 
  40c1e9:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c1ee:	48 8b 74 24 08       	mov    0x8(%rsp),%rsi
  40c1f3:	8b 84 24 b8 00 00 00 	mov    0xb8(%rsp),%eax
  40c1fa:	8b 8c 24 bc 00 00 00 	mov    0xbc(%rsp),%ecx
  40c201:	48 89 32             	mov    %rsi,(%rdx)
  40c204:	89 8c 24 b4 00 00 00 	mov    %ecx,0xb4(%rsp)
  40c20b:	89 84 24 b0 00 00 00 	mov    %eax,0xb0(%rsp)
  40c212:	48 8b 84 24 b0 00 00 	mov    0xb0(%rsp),%rax
  40c219:	00 
  40c21a:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40c221:	c3                   	ret
  40c222:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40c227:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40c22c:	48 8b 74 24 40       	mov    0x40(%rsp),%rsi
  40c231:	48 8b bc 24 d8 01 00 	mov    0x1d8(%rsp),%rdi
  40c238:	00 
  40c239:	48 8b 84 24 f0 01 00 	mov    0x1f0(%rsp),%rax
  40c240:	00 
  40c241:	48 8b 8c 24 f8 01 00 	mov    0x1f8(%rsp),%rcx
  40c248:	00 
  40c249:	48 89 8c 24 a8 00 00 	mov    %rcx,0xa8(%rsp)
  40c250:	00 
  40c251:	48 89 84 24 a0 00 00 	mov    %rax,0xa0(%rsp)
  40c258:	00 
  40c259:	48 8b 8c 24 a0 00 00 	mov    0xa0(%rsp),%rcx
  40c260:	00 
  40c261:	4c 8b 84 24 a8 00 00 	mov    0xa8(%rsp),%r8
  40c268:	00 
  40c269:	e8 d2 d7 ff ff       	call   409a40 <os::[file_stream.odin]::file_stream_fstat_utility>
  40c26e:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c273:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40c27a:	00 
  40c27b:	8b 84 24 90 00 00 00 	mov    0x90(%rsp),%eax
  40c282:	8b 8c 24 94 00 00 00 	mov    0x94(%rsp),%ecx
  40c289:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40c290:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40c297:	48 8b b4 24 e8 01 00 	mov    0x1e8(%rsp),%rsi
  40c29e:	00 
  40c29f:	8b 84 24 e0 01 00 00 	mov    0x1e0(%rsp),%eax
  40c2a6:	8b 8c 24 e4 01 00 00 	mov    0x1e4(%rsp),%ecx
  40c2ad:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40c2b4:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40c2bb:	48 89 32             	mov    %rsi,(%rdx)
  40c2be:	89 8c 24 84 00 00 00 	mov    %ecx,0x84(%rsp)
  40c2c5:	89 84 24 80 00 00 00 	mov    %eax,0x80(%rsp)
  40c2cc:	48 8b 84 24 80 00 00 	mov    0x80(%rsp),%rax
  40c2d3:	00 
  40c2d4:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40c2db:	c3                   	ret
  40c2dc:	48 c7 44 24 78 00 00 	movq   $0x0,0x78(%rsp)
  40c2e3:	00 00 
  40c2e5:	31 c0                	xor    %eax,%eax
  40c2e7:	a8 01                	test   $0x1,%al
  40c2e9:	75 02                	jne    40c2ed <os::[file_linux.odin]::_file_stream_proc+0x7dd>
  40c2eb:	eb 12                	jmp    40c2ff <os::[file_linux.odin]::_file_stream_proc+0x7ef>
  40c2ed:	c7 44 24 7c 00 00 00 	movl   $0x0,0x7c(%rsp)
  40c2f4:	00 
  40c2f5:	c7 44 24 78 00 00 00 	movl   $0x0,0x78(%rsp)
  40c2fc:	00 
  40c2fd:	eb 10                	jmp    40c30f <os::[file_linux.odin]::_file_stream_proc+0x7ff>
  40c2ff:	c7 44 24 78 ff ff ff 	movl   $0xffffffff,0x78(%rsp)
  40c306:	ff 
  40c307:	c7 44 24 7c 02 00 00 	movl   $0x2,0x7c(%rsp)
  40c30e:	00 
  40c30f:	8b 44 24 78          	mov    0x78(%rsp),%eax
  40c313:	8b 4c 24 7c          	mov    0x7c(%rsp),%ecx
  40c317:	48 c7 84 24 e8 01 00 	movq   $0x0,0x1e8(%rsp)
  40c31e:	00 00 00 00 00 
  40c323:	89 8c 24 e4 01 00 00 	mov    %ecx,0x1e4(%rsp)
  40c32a:	89 84 24 e0 01 00 00 	mov    %eax,0x1e0(%rsp)
  40c331:	48 c7 44 24 70 00 00 	movq   $0x0,0x70(%rsp)
  40c338:	00 00 
  40c33a:	31 c0                	xor    %eax,%eax
  40c33c:	a8 01                	test   $0x1,%al
  40c33e:	75 02                	jne    40c342 <os::[file_linux.odin]::_file_stream_proc+0x832>
  40c340:	eb 12                	jmp    40c354 <os::[file_linux.odin]::_file_stream_proc+0x844>
  40c342:	c7 44 24 74 00 00 00 	movl   $0x0,0x74(%rsp)
  40c349:	00 
  40c34a:	c7 44 24 70 00 00 00 	movl   $0x0,0x70(%rsp)
  40c351:	00 
  40c352:	eb 10                	jmp    40c364 <os::[file_linux.odin]::_file_stream_proc+0x854>
  40c354:	c7 44 24 70 ff ff ff 	movl   $0xffffffff,0x70(%rsp)
  40c35b:	ff 
  40c35c:	c7 44 24 74 02 00 00 	movl   $0x2,0x74(%rsp)
  40c363:	00 
  40c364:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c369:	8b 44 24 70          	mov    0x70(%rsp),%eax
  40c36d:	8b 4c 24 74          	mov    0x74(%rsp),%ecx
  40c371:	48 c7 02 00 00 00 00 	movq   $0x0,(%rdx)
  40c378:	89 4c 24 64          	mov    %ecx,0x64(%rsp)
  40c37c:	89 44 24 60          	mov    %eax,0x60(%rsp)
  40c380:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40c385:	48 81 c4 38 02 00 00 	add    $0x238,%rsp
  40c38c:	c3                   	ret
  40c38d:	0f 1f 00             	nopl   (%rax)

000000000040c390 <os::[file_linux.odin]::_standard_stream_init.new_std-0>:
  40c390:	48 83 ec 78          	sub    $0x78,%rsp
  40c394:	89 74 24 14          	mov    %esi,0x14(%rsp)
  40c398:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40c39d:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40c3a2:	48 89 54 24 28       	mov    %rdx,0x28(%rsp)
  40c3a7:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40c3ac:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40c3b1:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40c3b6:	8b 4c 24 14          	mov    0x14(%rsp),%ecx
  40c3ba:	48 89 54 24 70       	mov    %rdx,0x70(%rsp)
  40c3bf:	89 4c 24 6c          	mov    %ecx,0x6c(%rsp)
  40c3c3:	48 89 74 24 60       	mov    %rsi,0x60(%rsp)
  40c3c8:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40c3cd:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c3d2:	48 89 10             	mov    %rdx,(%rax)
  40c3d5:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c3da:	89 48 28             	mov    %ecx,0x28(%rax)
  40c3dd:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c3e2:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  40c3e7:	e8 74 7e ff ff       	call   404260 <runtime::nil_allocator>
  40c3ec:	48 8b 74 24 20       	mov    0x20(%rsp),%rsi
  40c3f1:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c3f6:	48 89 c7             	mov    %rax,%rdi
  40c3f9:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40c3fe:	49 89 d0             	mov    %rdx,%r8
  40c401:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40c406:	4c 89 44 24 48       	mov    %r8,0x48(%rsp)
  40c40b:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40c410:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40c415:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40c41a:	4c 89 40 38          	mov    %r8,0x38(%rax)
  40c41e:	48 89 78 30          	mov    %rdi,0x30(%rax)
  40c422:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c427:	48 89 70 20          	mov    %rsi,0x20(%rax)
  40c42b:	48 89 50 18          	mov    %rdx,0x18(%rax)
  40c42f:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c434:	0f 57 c0             	xorps  %xmm0,%xmm0
  40c437:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40c43c:	48 c7 c2 10 bb 40 00 	mov    $0x40bb10,%rdx
  40c443:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40c448:	48 c7 44 24 38 00 00 	movq   $0x0,0x38(%rsp)
  40c44f:	00 00 
  40c451:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40c456:	48 8b 4c 24 30       	mov    0x30(%rsp),%rcx
  40c45b:	48 8b 54 24 38       	mov    0x38(%rsp),%rdx
  40c460:	48 89 50 10          	mov    %rdx,0x10(%rax)
  40c464:	48 89 48 08          	mov    %rcx,0x8(%rax)
  40c468:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c46d:	48 83 c4 78          	add    $0x78,%rsp
  40c471:	c3                   	ret
  40c472:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40c479:	00 00 00 
  40c47c:	0f 1f 40 00          	nopl   0x0(%rax)

000000000040c480 <__$startup_runtime>:
  40c480:	50                   	push   %rax
  40c481:	eb 00                	jmp    40c483 <__$startup_runtime+0x3>
  40c483:	e8 88 c5 ff ff       	call   408a10 <os::[allocators.odin]::init_thread_local_cleaner>
  40c488:	e8 e3 c5 ff ff       	call   408a70 <os::[file_linux.odin]::_standard_stream_init>
  40c48d:	58                   	pop    %rax
  40c48e:	c3                   	ret
  40c48f:	90                   	nop

000000000040c490 <__$equal$$struct{x:f32,y:f32}>:
  40c490:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  40c495:	48 89 74 24 f8       	mov    %rsi,-0x8(%rsp)
  40c49a:	eb 00                	jmp    40c49c <__$equal$$struct{x:f32,y:f32}+0xc>
  40c49c:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40c4a1:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40c4a6:	48 39 c8             	cmp    %rcx,%rax
  40c4a9:	75 03                	jne    40c4ae <__$equal$$struct{x:f32,y:f32}+0x1e>
  40c4ab:	b0 01                	mov    $0x1,%al
  40c4ad:	c3                   	ret
  40c4ae:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40c4b3:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40c4b8:	f3 0f 10 01          	movss  (%rcx),%xmm0
  40c4bc:	0f 2e 00             	ucomiss (%rax),%xmm0
  40c4bf:	75 1c                	jne    40c4dd <__$equal$$struct{x:f32,y:f32}+0x4d>
  40c4c1:	7a 1a                	jp     40c4dd <__$equal$$struct{x:f32,y:f32}+0x4d>
  40c4c3:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40c4c8:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40c4cd:	f3 0f 10 41 04       	movss  0x4(%rcx),%xmm0
  40c4d2:	0f 2e 40 04          	ucomiss 0x4(%rax),%xmm0
  40c4d6:	75 05                	jne    40c4dd <__$equal$$struct{x:f32,y:f32}+0x4d>
  40c4d8:	7a 03                	jp     40c4dd <__$equal$$struct{x:f32,y:f32}+0x4d>
  40c4da:	b0 01                	mov    $0x1,%al
  40c4dc:	c3                   	ret
  40c4dd:	31 c0                	xor    %eax,%eax
  40c4df:	c3                   	ret

000000000040c4e0 <__$cleanup_runtime>:
  40c4e0:	50                   	push   %rax
  40c4e1:	eb 00                	jmp    40c4e3 <__$cleanup_runtime+0x3>
  40c4e3:	e8 28 c6 ff ff       	call   408b10 <os::[process.odin]::delete_args>
  40c4e8:	e8 b3 c7 ff ff       	call   408ca0 <os::[allocators.odin]::temp_allocator_fini>
  40c4ed:	e8 3e 5d ff ff       	call   402230 <runtime::[default_temporary_allocator.odin]::_destroy_temp_allocator_fini>
  40c4f2:	58                   	pop    %rax
  40c4f3:	c3                   	ret
  40c4f4:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40c4fb:	00 00 00 
  40c4fe:	66 90                	xchg   %ax,%ax

000000000040c500 <runtime::[internal.odin]::byte_slice>:
  40c500:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40c505:	48 89 74 24 c0       	mov    %rsi,-0x40(%rsp)
  40c50a:	48 8b 54 24 c0       	mov    -0x40(%rsp),%rdx
  40c50f:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40c514:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40c519:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  40c51e:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40c523:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40c528:	31 c0                	xor    %eax,%eax
  40c52a:	48 85 d2             	test   %rdx,%rdx
  40c52d:	48 0f 49 c2          	cmovns %rdx,%rax
  40c531:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  40c536:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40c53b:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40c540:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40c545:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40c54a:	48 89 44 24 c8       	mov    %rax,-0x38(%rsp)
  40c54f:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40c554:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  40c559:	c3                   	ret
  40c55a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

000000000040c560 <runtime::is_power_of_two_int>:
  40c560:	48 89 7c 24 f0       	mov    %rdi,-0x10(%rsp)
  40c565:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40c56a:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40c56f:	48 83 f8 00          	cmp    $0x0,%rax
  40c573:	0f 9e c0             	setle  %al
  40c576:	24 01                	and    $0x1,%al
  40c578:	3c 00                	cmp    $0x0,%al
  40c57a:	74 03                	je     40c57f <runtime::is_power_of_two_int+0x1f>
  40c57c:	31 c0                	xor    %eax,%eax
  40c57e:	c3                   	ret
  40c57f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40c584:	48 89 c1             	mov    %rax,%rcx
  40c587:	48 83 e9 01          	sub    $0x1,%rcx
  40c58b:	48 21 c8             	and    %rcx,%rax
  40c58e:	48 83 f8 00          	cmp    $0x0,%rax
  40c592:	0f 94 c0             	sete   %al
  40c595:	24 01                	and    $0x1,%al
  40c597:	c3                   	ret
  40c598:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40c59f:	00 

000000000040c5a0 <runtime::mem_copy_non_overlapping>:
  40c5a0:	48 83 ec 38          	sub    $0x38,%rsp
  40c5a4:	48 89 7c 24 08       	mov    %rdi,0x8(%rsp)
  40c5a9:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40c5ae:	48 89 54 24 18       	mov    %rdx,0x18(%rsp)
  40c5b3:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
  40c5b8:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c5bd:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40c5c2:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40c5c7:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40c5cc:	48 89 4c 24 20       	mov    %rcx,0x20(%rsp)
  40c5d1:	48 83 f8 00          	cmp    $0x0,%rax
  40c5d5:	0f 95 c0             	setne  %al
  40c5d8:	24 01                	and    $0x1,%al
  40c5da:	3c 00                	cmp    $0x0,%al
  40c5dc:	74 3c                	je     40c61a <runtime::mem_copy_non_overlapping+0x7a>
  40c5de:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40c5e3:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40c5e8:	48 39 c8             	cmp    %rcx,%rax
  40c5eb:	0f 95 c0             	setne  %al
  40c5ee:	24 01                	and    $0x1,%al
  40c5f0:	3c 00                	cmp    $0x0,%al
  40c5f2:	74 26                	je     40c61a <runtime::mem_copy_non_overlapping+0x7a>
  40c5f4:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40c5f9:	48 83 f8 00          	cmp    $0x0,%rax
  40c5fd:	0f 9f c0             	setg   %al
  40c600:	24 01                	and    $0x1,%al
  40c602:	3c 00                	cmp    $0x0,%al
  40c604:	74 14                	je     40c61a <runtime::mem_copy_non_overlapping+0x7a>
  40c606:	48 8b 54 24 18       	mov    0x18(%rsp),%rdx
  40c60b:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40c610:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  40c615:	e8 46 4a ff ff       	call   401060 <memcpy@plt>
  40c61a:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40c61f:	48 83 c4 38          	add    $0x38,%rsp
  40c623:	c3                   	ret
  40c624:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40c62b:	00 00 00 00 00 

000000000040c630 <runtime::mem_alloc_bytes>:
  40c630:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40c637:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40c63c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40c641:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40c646:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40c64b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40c650:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40c655:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40c65c:	00 
  40c65d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40c662:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40c667:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40c66c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40c671:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40c676:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40c67d:	00 
  40c67e:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  40c683:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  40c688:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40c68d:	e8 ce fe ff ff       	call   40c560 <runtime::is_power_of_two_int>
  40c692:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40c697:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40c69c:	0f b6 f8             	movzbl %al,%edi
  40c69f:	be 6e ff 40 00       	mov    $0x40ff6e,%esi
  40c6a4:	ba 20 00 00 00       	mov    $0x20,%edx
  40c6a9:	e8 02 50 ff ff       	call   4016b0 <runtime::assert>
  40c6ae:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40c6b3:	48 83 f8 00          	cmp    $0x0,%rax
  40c6b7:	0f 94 c0             	sete   %al
  40c6ba:	24 01                	and    $0x1,%al
  40c6bc:	3c 00                	cmp    $0x0,%al
  40c6be:	75 0f                	jne    40c6cf <runtime::mem_alloc_bytes+0x9f>
  40c6c0:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  40c6c6:	0f 94 c0             	sete   %al
  40c6c9:	24 01                	and    $0x1,%al
  40c6cb:	3c 00                	cmp    $0x0,%al
  40c6cd:	74 1b                	je     40c6ea <runtime::mem_alloc_bytes+0xba>
  40c6cf:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40c6d4:	31 f6                	xor    %esi,%esi
  40c6d6:	ba 10 00 00 00       	mov    $0x10,%edx
  40c6db:	e8 60 49 ff ff       	call   401040 <memset@plt>
  40c6e0:	31 c0                	xor    %eax,%eax
  40c6e2:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c6e9:	c3                   	ret
  40c6ea:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40c6ef:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40c6f4:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  40c6f9:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40c6fe:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40c703:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40c708:	0f 57 c0             	xorps  %xmm0,%xmm0
  40c70b:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40c710:	48 89 e6             	mov    %rsp,%rsi
  40c713:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  40c717:	4c 8d 4c 24 50       	lea    0x50(%rsp),%r9
  40c71c:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  40c720:	4c 89 06             	mov    %r8,(%rsi)
  40c723:	31 f6                	xor    %esi,%esi
  40c725:	41 89 f1             	mov    %esi,%r9d
  40c728:	4d 89 c8             	mov    %r9,%r8
  40c72b:	ff d0                	call   *%rax
  40c72d:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c732:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c737:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  40c73c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40c740:	48 89 11             	mov    %rdx,(%rcx)
  40c743:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c74a:	c3                   	ret
  40c74b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040c750 <runtime::mem_alloc>:
  40c750:	48 81 ec 88 00 00 00 	sub    $0x88,%rsp
  40c757:	4c 89 4c 24 18       	mov    %r9,0x18(%rsp)
  40c75c:	4c 89 44 24 20       	mov    %r8,0x20(%rsp)
  40c761:	48 89 74 24 28       	mov    %rsi,0x28(%rsp)
  40c766:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40c76b:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40c770:	48 89 54 24 40       	mov    %rdx,0x40(%rsp)
  40c775:	48 8b 84 24 90 00 00 	mov    0x90(%rsp),%rax
  40c77c:	00 
  40c77d:	48 89 44 24 48       	mov    %rax,0x48(%rsp)
  40c782:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40c787:	48 8b 7c 24 28       	mov    0x28(%rsp),%rdi
  40c78c:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40c791:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40c796:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40c79d:	00 
  40c79e:	48 89 7c 24 78       	mov    %rdi,0x78(%rsp)
  40c7a3:	48 89 4c 24 68       	mov    %rcx,0x68(%rsp)
  40c7a8:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40c7ad:	e8 ae fd ff ff       	call   40c560 <runtime::is_power_of_two_int>
  40c7b2:	48 8b 4c 24 20       	mov    0x20(%rsp),%rcx
  40c7b7:	4c 8b 44 24 48       	mov    0x48(%rsp),%r8
  40c7bc:	0f b6 f8             	movzbl %al,%edi
  40c7bf:	be 6e ff 40 00       	mov    $0x40ff6e,%esi
  40c7c4:	ba 20 00 00 00       	mov    $0x20,%edx
  40c7c9:	e8 e2 4e ff ff       	call   4016b0 <runtime::assert>
  40c7ce:	48 8b 44 24 30       	mov    0x30(%rsp),%rax
  40c7d3:	48 83 f8 00          	cmp    $0x0,%rax
  40c7d7:	0f 94 c0             	sete   %al
  40c7da:	24 01                	and    $0x1,%al
  40c7dc:	3c 00                	cmp    $0x0,%al
  40c7de:	75 0f                	jne    40c7ef <runtime::mem_alloc+0x9f>
  40c7e0:	48 83 7c 24 60 00    	cmpq   $0x0,0x60(%rsp)
  40c7e6:	0f 94 c0             	sete   %al
  40c7e9:	24 01                	and    $0x1,%al
  40c7eb:	3c 00                	cmp    $0x0,%al
  40c7ed:	74 1b                	je     40c80a <runtime::mem_alloc+0xba>
  40c7ef:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40c7f4:	31 f6                	xor    %esi,%esi
  40c7f6:	ba 10 00 00 00       	mov    $0x10,%edx
  40c7fb:	e8 40 48 ff ff       	call   401040 <memset@plt>
  40c800:	31 c0                	xor    %eax,%eax
  40c802:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c809:	c3                   	ret
  40c80a:	48 8b 4c 24 28       	mov    0x28(%rsp),%rcx
  40c80f:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40c814:	4c 8b 44 24 20       	mov    0x20(%rsp),%r8
  40c819:	4c 8b 4c 24 48       	mov    0x48(%rsp),%r9
  40c81e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40c823:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40c828:	0f 57 c0             	xorps  %xmm0,%xmm0
  40c82b:	0f 29 44 24 50       	movaps %xmm0,0x50(%rsp)
  40c830:	48 89 e6             	mov    %rsp,%rsi
  40c833:	4c 89 4e 10          	mov    %r9,0x10(%rsi)
  40c837:	4c 8d 4c 24 50       	lea    0x50(%rsp),%r9
  40c83c:	4c 89 4e 08          	mov    %r9,0x8(%rsi)
  40c840:	4c 89 06             	mov    %r8,(%rsi)
  40c843:	31 f6                	xor    %esi,%esi
  40c845:	41 89 f1             	mov    %esi,%r9d
  40c848:	4d 89 c8             	mov    %r9,%r8
  40c84b:	ff d0                	call   *%rax
  40c84d:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40c852:	48 8b 54 24 50       	mov    0x50(%rsp),%rdx
  40c857:	48 8b 74 24 58       	mov    0x58(%rsp),%rsi
  40c85c:	48 89 71 08          	mov    %rsi,0x8(%rcx)
  40c860:	48 89 11             	mov    %rdx,(%rcx)
  40c863:	48 81 c4 88 00 00 00 	add    $0x88,%rsp
  40c86a:	c3                   	ret
  40c86b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040c870 <runtime::mem_free>:
  40c870:	53                   	push   %rbx
  40c871:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  40c878:	4c 89 44 24 30       	mov    %r8,0x30(%rsp)
  40c87d:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40c882:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40c887:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40c88c:	48 89 74 24 50       	mov    %rsi,0x50(%rsp)
  40c891:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40c896:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40c89b:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40c8a0:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  40c8a7:	00 
  40c8a8:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40c8ad:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40c8b2:	48 83 f8 00          	cmp    $0x0,%rax
  40c8b6:	0f 94 c0             	sete   %al
  40c8b9:	24 01                	and    $0x1,%al
  40c8bb:	3c 00                	cmp    $0x0,%al
  40c8bd:	75 0f                	jne    40c8ce <runtime::mem_free+0x5e>
  40c8bf:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  40c8c5:	0f 94 c0             	sete   %al
  40c8c8:	24 01                	and    $0x1,%al
  40c8ca:	3c 00                	cmp    $0x0,%al
  40c8cc:	74 0b                	je     40c8d9 <runtime::mem_free+0x69>
  40c8ce:	31 c0                	xor    %eax,%eax
  40c8d0:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40c8d7:	5b                   	pop    %rbx
  40c8d8:	c3                   	ret
  40c8d9:	48 8b 5c 24 38       	mov    0x38(%rsp),%rbx
  40c8de:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c8e3:	48 89 44 24 28       	mov    %rax,0x28(%rsp)
  40c8e8:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40c8ed:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c8f2:	48 8d 7c 24 60       	lea    0x60(%rsp),%rdi
  40c8f7:	31 f6                	xor    %esi,%esi
  40c8f9:	ba 10 00 00 00       	mov    $0x10,%edx
  40c8fe:	e8 3d 47 ff ff       	call   401040 <memset@plt>
  40c903:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40c908:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  40c90d:	4c 8b 54 24 30       	mov    0x30(%rsp),%r10
  40c912:	48 8b 44 24 28       	mov    0x28(%rsp),%rax
  40c917:	be 01 00 00 00       	mov    $0x1,%esi
  40c91c:	31 c9                	xor    %ecx,%ecx
  40c91e:	41 89 c9             	mov    %ecx,%r9d
  40c921:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  40c926:	4c 89 ca             	mov    %r9,%rdx
  40c929:	4c 89 c9             	mov    %r9,%rcx
  40c92c:	48 89 1c 24          	mov    %rbx,(%rsp)
  40c930:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  40c935:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  40c93a:	ff d0                	call   *%rax
  40c93c:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  40c940:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40c944:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40c94b:	5b                   	pop    %rbx
  40c94c:	c3                   	ret
  40c94d:	0f 1f 00             	nopl   (%rax)

000000000040c950 <runtime::mem_free_with_size>:
  40c950:	53                   	push   %rbx
  40c951:	48 81 ec 90 00 00 00 	sub    $0x90,%rsp
  40c958:	4c 89 4c 24 28       	mov    %r9,0x28(%rsp)
  40c95d:	4c 89 44 24 30       	mov    %r8,0x30(%rsp)
  40c962:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40c967:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40c96c:	48 89 4c 24 48       	mov    %rcx,0x48(%rsp)
  40c971:	48 89 54 24 50       	mov    %rdx,0x50(%rsp)
  40c976:	48 8b 44 24 40       	mov    0x40(%rsp),%rax
  40c97b:	48 8b 4c 24 50       	mov    0x50(%rsp),%rcx
  40c980:	48 8b 54 24 48       	mov    0x48(%rsp),%rdx
  40c985:	48 8b 74 24 38       	mov    0x38(%rsp),%rsi
  40c98a:	48 89 84 24 88 00 00 	mov    %rax,0x88(%rsp)
  40c991:	00 
  40c992:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40c999:	00 
  40c99a:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40c99f:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40c9a4:	48 83 f8 00          	cmp    $0x0,%rax
  40c9a8:	0f 94 c0             	sete   %al
  40c9ab:	24 01                	and    $0x1,%al
  40c9ad:	3c 00                	cmp    $0x0,%al
  40c9af:	75 0f                	jne    40c9c0 <runtime::mem_free_with_size+0x70>
  40c9b1:	48 83 7c 24 70 00    	cmpq   $0x0,0x70(%rsp)
  40c9b7:	0f 94 c0             	sete   %al
  40c9ba:	24 01                	and    $0x1,%al
  40c9bc:	3c 00                	cmp    $0x0,%al
  40c9be:	74 0b                	je     40c9cb <runtime::mem_free_with_size+0x7b>
  40c9c0:	31 c0                	xor    %eax,%eax
  40c9c2:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40c9c9:	5b                   	pop    %rbx
  40c9ca:	c3                   	ret
  40c9cb:	48 8b 5c 24 30       	mov    0x30(%rsp),%rbx
  40c9d0:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40c9d5:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40c9da:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40c9df:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
  40c9e4:	48 8d 7c 24 60       	lea    0x60(%rsp),%rdi
  40c9e9:	31 f6                	xor    %esi,%esi
  40c9eb:	ba 10 00 00 00       	mov    $0x10,%edx
  40c9f0:	e8 4b 46 ff ff       	call   401040 <memset@plt>
  40c9f5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40c9fa:	4c 8b 44 24 40       	mov    0x40(%rsp),%r8
  40c9ff:	4c 8b 4c 24 38       	mov    0x38(%rsp),%r9
  40ca04:	4c 8b 54 24 28       	mov    0x28(%rsp),%r10
  40ca09:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40ca0e:	be 01 00 00 00       	mov    $0x1,%esi
  40ca13:	31 c9                	xor    %ecx,%ecx
  40ca15:	4c 8d 5c 24 60       	lea    0x60(%rsp),%r11
  40ca1a:	48 89 ca             	mov    %rcx,%rdx
  40ca1d:	48 89 1c 24          	mov    %rbx,(%rsp)
  40ca21:	4c 89 5c 24 08       	mov    %r11,0x8(%rsp)
  40ca26:	4c 89 54 24 10       	mov    %r10,0x10(%rsp)
  40ca2b:	ff d0                	call   *%rax
  40ca2d:	88 44 24 5f          	mov    %al,0x5f(%rsp)
  40ca31:	8a 44 24 5f          	mov    0x5f(%rsp),%al
  40ca35:	48 81 c4 90 00 00 00 	add    $0x90,%rsp
  40ca3c:	5b                   	pop    %rbx
  40ca3d:	c3                   	ret
  40ca3e:	66 90                	xchg   %ax,%ax

000000000040ca40 <runtime::conditional_mem_zero>:
  40ca40:	48 83 ec 30          	sub    $0x30,%rsp
  40ca44:	48 89 7c 24 90       	mov    %rdi,-0x70(%rsp)
  40ca49:	48 89 74 24 98       	mov    %rsi,-0x68(%rsp)
  40ca4e:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40ca53:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40ca58:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40ca5d:	48 89 44 24 20       	mov    %rax,0x20(%rsp)
  40ca62:	48 83 f8 00          	cmp    $0x0,%rax
  40ca66:	0f 9e c0             	setle  %al
  40ca69:	24 01                	and    $0x1,%al
  40ca6b:	3c 00                	cmp    $0x0,%al
  40ca6d:	74 05                	je     40ca74 <runtime::conditional_mem_zero+0x34>
  40ca6f:	48 83 c4 30          	add    $0x30,%rsp
  40ca73:	c3                   	ret
  40ca74:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  40ca79:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40ca7e:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40ca83:	48 8b 4c 24 18       	mov    0x18(%rsp),%rcx
  40ca88:	48 c1 e9 03          	shr    $0x3,%rcx
  40ca8c:	48 89 4c 24 10       	mov    %rcx,0x10(%rsp)
  40ca91:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
  40ca96:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40ca9b:	48 8b 4c 24 10       	mov    0x10(%rsp),%rcx
  40caa0:	48 89 54 24 f8       	mov    %rdx,-0x8(%rsp)
  40caa5:	48 89 0c 24          	mov    %rcx,(%rsp)
  40caa9:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40caae:	48 8b 14 24          	mov    (%rsp),%rdx
  40cab2:	48 89 54 24 f0       	mov    %rdx,-0x10(%rsp)
  40cab7:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40cabc:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40cac1:	48 8b 4c 24 e0       	mov    -0x20(%rsp),%rcx
  40cac6:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40cacb:	48 89 f2             	mov    %rsi,%rdx
  40cace:	48 c1 e2 03          	shl    $0x3,%rdx
  40cad2:	48 8b 44 24 18       	mov    0x18(%rsp),%rax
  40cad7:	48 8d 0c f1          	lea    (%rcx,%rsi,8),%rcx
  40cadb:	48 29 d0             	sub    %rdx,%rax
  40cade:	48 89 4c 24 d0       	mov    %rcx,-0x30(%rsp)
  40cae3:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40cae8:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40caed:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40caf2:	48 89 4c 24 c8       	mov    %rcx,-0x38(%rsp)
  40caf7:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  40cafc:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cb01:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40cb06:	48 c7 44 24 b0 ff ff 	movq   $0xffffffffffffffff,-0x50(%rsp)
  40cb0d:	ff ff 
  40cb0f:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40cb14:	48 83 c0 01          	add    $0x1,%rax
  40cb18:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40cb1d:	48 3b 44 24 b8       	cmp    -0x48(%rsp),%rax
  40cb22:	7d 38                	jge    40cb5c <runtime::conditional_mem_zero+0x11c>
  40cb24:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40cb29:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40cb2e:	48 89 ce             	mov    %rcx,%rsi
  40cb31:	48 c1 e6 03          	shl    $0x3,%rsi
  40cb35:	48 89 c2             	mov    %rax,%rdx
  40cb38:	48 01 f2             	add    %rsi,%rdx
  40cb3b:	48 89 54 24 88       	mov    %rdx,-0x78(%rsp)
  40cb40:	48 83 3c c8 00       	cmpq   $0x0,(%rax,%rcx,8)
  40cb45:	0f 95 c0             	setne  %al
  40cb48:	24 01                	and    $0x1,%al
  40cb4a:	3c 00                	cmp    $0x0,%al
  40cb4c:	74 0c                	je     40cb5a <runtime::conditional_mem_zero+0x11a>
  40cb4e:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40cb53:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  40cb5a:	eb b3                	jmp    40cb0f <runtime::conditional_mem_zero+0xcf>
  40cb5c:	48 8b 44 24 c8       	mov    -0x38(%rsp),%rax
  40cb61:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40cb66:	48 c7 44 24 a0 ff ff 	movq   $0xffffffffffffffff,-0x60(%rsp)
  40cb6d:	ff ff 
  40cb6f:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40cb74:	48 83 c0 01          	add    $0x1,%rax
  40cb78:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40cb7d:	48 3b 44 24 a8       	cmp    -0x58(%rsp),%rax
  40cb82:	7d 2c                	jge    40cbb0 <runtime::conditional_mem_zero+0x170>
  40cb84:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  40cb89:	48 8b 44 24 c0       	mov    -0x40(%rsp),%rax
  40cb8e:	48 89 c2             	mov    %rax,%rdx
  40cb91:	48 01 ca             	add    %rcx,%rdx
  40cb94:	48 89 54 24 80       	mov    %rdx,-0x80(%rsp)
  40cb99:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  40cb9d:	0f 95 c0             	setne  %al
  40cba0:	24 01                	and    $0x1,%al
  40cba2:	3c 00                	cmp    $0x0,%al
  40cba4:	74 08                	je     40cbae <runtime::conditional_mem_zero+0x16e>
  40cba6:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  40cbab:	c6 00 00             	movb   $0x0,(%rax)
  40cbae:	eb bf                	jmp    40cb6f <runtime::conditional_mem_zero+0x12f>
  40cbb0:	48 83 c4 30          	add    $0x30,%rsp
  40cbb4:	c3                   	ret
  40cbb5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
  40cbbc:	00 00 00 00 

000000000040cbc0 <runtime::memory_equal>:
  40cbc0:	48 83 ec 28          	sub    $0x28,%rsp
  40cbc4:	48 89 7c 24 88       	mov    %rdi,-0x78(%rsp)
  40cbc9:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  40cbce:	48 89 54 24 98       	mov    %rdx,-0x68(%rsp)
  40cbd3:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40cbd8:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40cbdd:	48 8b 54 24 88       	mov    -0x78(%rsp),%rdx
  40cbe2:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40cbe7:	48 89 4c 24 18       	mov    %rcx,0x18(%rsp)
  40cbec:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
  40cbf1:	48 83 f8 00          	cmp    $0x0,%rax
  40cbf5:	0f 94 c1             	sete   %cl
  40cbf8:	80 e1 01             	and    $0x1,%cl
  40cbfb:	b0 01                	mov    $0x1,%al
  40cbfd:	38 c8                	cmp    %cl,%al
  40cbff:	74 1b                	je     40cc1c <runtime::memory_equal+0x5c>
  40cc01:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40cc06:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40cc0b:	48 39 c8             	cmp    %rcx,%rax
  40cc0e:	0f 94 c1             	sete   %cl
  40cc11:	80 e1 01             	and    $0x1,%cl
  40cc14:	b0 01                	mov    $0x1,%al
  40cc16:	38 c8                	cmp    %cl,%al
  40cc18:	74 0b                	je     40cc25 <runtime::memory_equal+0x65>
  40cc1a:	eb 07                	jmp    40cc23 <runtime::memory_equal+0x63>
  40cc1c:	b0 01                	mov    $0x1,%al
  40cc1e:	48 83 c4 28          	add    $0x28,%rsp
  40cc22:	c3                   	ret
  40cc23:	eb 07                	jmp    40cc2c <runtime::memory_equal+0x6c>
  40cc25:	b0 01                	mov    $0x1,%al
  40cc27:	48 83 c4 28          	add    $0x28,%rsp
  40cc2b:	c3                   	ret
  40cc2c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40cc31:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40cc36:	48 8b 54 24 88       	mov    -0x78(%rsp),%rdx
  40cc3b:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40cc40:	48 89 0c 24          	mov    %rcx,(%rsp)
  40cc44:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40cc49:	48 c7 44 24 f0 00 00 	movq   $0x0,-0x10(%rsp)
  40cc50:	00 00 
  40cc52:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  40cc59:	00 00 
  40cc5b:	48 83 7c 24 f8 08    	cmpq   $0x8,-0x8(%rsp)
  40cc61:	0f 93 c0             	setae  %al
  40cc64:	24 01                	and    $0x1,%al
  40cc66:	3c 00                	cmp    $0x0,%al
  40cc68:	0f 84 43 01 00 00    	je     40cdb1 <runtime::memory_equal+0x1f1>
  40cc6e:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40cc73:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  40cc78:	48 c1 e8 04          	shr    $0x4,%rax
  40cc7c:	48 c1 e0 04          	shl    $0x4,%rax
  40cc80:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40cc85:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cc8a:	48 3b 44 24 e8       	cmp    -0x18(%rsp),%rax
  40cc8f:	0f 92 c0             	setb   %al
  40cc92:	24 01                	and    $0x1,%al
  40cc94:	3c 00                	cmp    $0x0,%al
  40cc96:	0f 84 9a 00 00 00    	je     40cd36 <runtime::memory_equal+0x176>
  40cc9c:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40cca1:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40cca6:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40ccaa:	0f 29 44 24 d0       	movaps %xmm0,-0x30(%rsp)
  40ccaf:	48 8b 04 24          	mov    (%rsp),%rax
  40ccb3:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40ccb8:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40ccbc:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  40ccc1:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40ccc6:	0f 28 4c 24 c0       	movaps -0x40(%rsp),%xmm1
  40cccb:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  40cccf:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40ccd3:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40ccd7:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  40ccdc:	0f 28 44 24 b0       	movaps -0x50(%rsp),%xmm0
  40cce1:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40cce6:	66 0f eb c1          	por    %xmm1,%xmm0
  40ccea:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40ccef:	66 0f eb c1          	por    %xmm1,%xmm0
  40ccf3:	0f 28 c8             	movaps %xmm0,%xmm1
  40ccf6:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40ccfb:	66 0f eb c1          	por    %xmm1,%xmm0
  40ccff:	0f 28 c8             	movaps %xmm0,%xmm1
  40cd02:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40cd07:	66 0f eb c1          	por    %xmm1,%xmm0
  40cd0b:	66 0f 7e c0          	movd   %xmm0,%eax
  40cd0f:	3c 00                	cmp    $0x0,%al
  40cd11:	0f 95 c0             	setne  %al
  40cd14:	24 01                	and    $0x1,%al
  40cd16:	3c 00                	cmp    $0x0,%al
  40cd18:	74 07                	je     40cd21 <runtime::memory_equal+0x161>
  40cd1a:	31 c0                	xor    %eax,%eax
  40cd1c:	48 83 c4 28          	add    $0x28,%rsp
  40cd20:	c3                   	ret
  40cd21:	eb 00                	jmp    40cd23 <runtime::memory_equal+0x163>
  40cd23:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cd28:	48 83 c0 10          	add    $0x10,%rax
  40cd2c:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40cd31:	e9 4f ff ff ff       	jmp    40cc85 <runtime::memory_equal+0xc5>
  40cd36:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40cd3b:	48 2b 44 24 f0       	sub    -0x10(%rsp),%rax
  40cd40:	48 c1 e8 03          	shr    $0x3,%rax
  40cd44:	48 c1 e0 03          	shl    $0x3,%rax
  40cd48:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40cd4d:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cd52:	48 3b 44 24 e8       	cmp    -0x18(%rsp),%rax
  40cd57:	0f 92 c0             	setb   %al
  40cd5a:	24 01                	and    $0x1,%al
  40cd5c:	3c 00                	cmp    $0x0,%al
  40cd5e:	74 4f                	je     40cdaf <runtime::memory_equal+0x1ef>
  40cd60:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40cd65:	48 03 44 24 f0       	add    -0x10(%rsp),%rax
  40cd6a:	48 8b 00             	mov    (%rax),%rax
  40cd6d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40cd72:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40cd77:	48 8b 0c 24          	mov    (%rsp),%rcx
  40cd7b:	48 03 4c 24 f0       	add    -0x10(%rsp),%rcx
  40cd80:	48 8b 09             	mov    (%rcx),%rcx
  40cd83:	48 89 4c 24 a0       	mov    %rcx,-0x60(%rsp)
  40cd88:	48 3b 44 24 a0       	cmp    -0x60(%rsp),%rax
  40cd8d:	0f 95 c0             	setne  %al
  40cd90:	24 01                	and    $0x1,%al
  40cd92:	3c 00                	cmp    $0x0,%al
  40cd94:	74 07                	je     40cd9d <runtime::memory_equal+0x1dd>
  40cd96:	31 c0                	xor    %eax,%eax
  40cd98:	48 83 c4 28          	add    $0x28,%rsp
  40cd9c:	c3                   	ret
  40cd9d:	eb 00                	jmp    40cd9f <runtime::memory_equal+0x1df>
  40cd9f:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cda4:	48 83 c0 08          	add    $0x8,%rax
  40cda8:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40cdad:	eb 9e                	jmp    40cd4d <runtime::memory_equal+0x18d>
  40cdaf:	eb 00                	jmp    40cdb1 <runtime::memory_equal+0x1f1>
  40cdb1:	eb 00                	jmp    40cdb3 <runtime::memory_equal+0x1f3>
  40cdb3:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cdb8:	48 3b 44 24 f8       	cmp    -0x8(%rsp),%rax
  40cdbd:	0f 92 c0             	setb   %al
  40cdc0:	24 01                	and    $0x1,%al
  40cdc2:	3c 00                	cmp    $0x0,%al
  40cdc4:	74 3b                	je     40ce01 <runtime::memory_equal+0x241>
  40cdc6:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
  40cdcb:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40cdd0:	8a 04 08             	mov    (%rax,%rcx,1),%al
  40cdd3:	48 8b 0c 24          	mov    (%rsp),%rcx
  40cdd7:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40cddc:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  40cddf:	0f 95 c0             	setne  %al
  40cde2:	24 01                	and    $0x1,%al
  40cde4:	3c 00                	cmp    $0x0,%al
  40cde6:	74 07                	je     40cdef <runtime::memory_equal+0x22f>
  40cde8:	31 c0                	xor    %eax,%eax
  40cdea:	48 83 c4 28          	add    $0x28,%rsp
  40cdee:	c3                   	ret
  40cdef:	eb 00                	jmp    40cdf1 <runtime::memory_equal+0x231>
  40cdf1:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40cdf6:	48 83 c0 01          	add    $0x1,%rax
  40cdfa:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40cdff:	eb b2                	jmp    40cdb3 <runtime::memory_equal+0x1f3>
  40ce01:	b0 01                	mov    $0x1,%al
  40ce03:	48 83 c4 28          	add    $0x28,%rsp
  40ce07:	c3                   	ret
  40ce08:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40ce0f:	00 

000000000040ce10 <runtime::memory_compare>:
  40ce10:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
  40ce17:	48 89 7c 24 98       	mov    %rdi,-0x68(%rsp)
  40ce1c:	48 89 74 24 a0       	mov    %rsi,-0x60(%rsp)
  40ce21:	48 89 54 24 a8       	mov    %rdx,-0x58(%rsp)
  40ce26:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40ce2b:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  40ce30:	48 8b 54 24 a8       	mov    -0x58(%rsp),%rdx
  40ce35:	48 89 84 24 90 00 00 	mov    %rax,0x90(%rsp)
  40ce3c:	00 
  40ce3d:	48 89 8c 24 88 00 00 	mov    %rcx,0x88(%rsp)
  40ce44:	00 
  40ce45:	48 89 94 24 80 00 00 	mov    %rdx,0x80(%rsp)
  40ce4c:	00 
  40ce4d:	48 39 c8             	cmp    %rcx,%rax
  40ce50:	0f 94 c1             	sete   %cl
  40ce53:	80 e1 01             	and    $0x1,%cl
  40ce56:	b0 01                	mov    $0x1,%al
  40ce58:	38 c8                	cmp    %cl,%al
  40ce5a:	74 17                	je     40ce73 <runtime::memory_compare+0x63>
  40ce5c:	48 8b 44 24 98       	mov    -0x68(%rsp),%rax
  40ce61:	48 83 f8 00          	cmp    $0x0,%rax
  40ce65:	0f 94 c1             	sete   %cl
  40ce68:	80 e1 01             	and    $0x1,%cl
  40ce6b:	b0 01                	mov    $0x1,%al
  40ce6d:	38 c8                	cmp    %cl,%al
  40ce6f:	74 23                	je     40ce94 <runtime::memory_compare+0x84>
  40ce71:	eb 0a                	jmp    40ce7d <runtime::memory_compare+0x6d>
  40ce73:	31 c0                	xor    %eax,%eax
  40ce75:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40ce7c:	c3                   	ret
  40ce7d:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40ce82:	48 83 f8 00          	cmp    $0x0,%rax
  40ce86:	0f 94 c1             	sete   %cl
  40ce89:	80 e1 01             	and    $0x1,%cl
  40ce8c:	b0 01                	mov    $0x1,%al
  40ce8e:	38 c8                	cmp    %cl,%al
  40ce90:	74 13                	je     40cea5 <runtime::memory_compare+0x95>
  40ce92:	eb 0f                	jmp    40cea3 <runtime::memory_compare+0x93>
  40ce94:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40ce9b:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40cea2:	c3                   	ret
  40cea3:	eb 0d                	jmp    40ceb2 <runtime::memory_compare+0xa2>
  40cea5:	b8 01 00 00 00       	mov    $0x1,%eax
  40ceaa:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40ceb1:	c3                   	ret
  40ceb2:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40ceb7:	48 8b 4c 24 a0       	mov    -0x60(%rsp),%rcx
  40cebc:	48 8b 54 24 98       	mov    -0x68(%rsp),%rdx
  40cec1:	48 89 54 24 78       	mov    %rdx,0x78(%rsp)
  40cec6:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40cecb:	48 89 44 24 68       	mov    %rax,0x68(%rsp)
  40ced0:	48 c7 44 24 60 00 00 	movq   $0x0,0x60(%rsp)
  40ced7:	00 00 
  40ced9:	48 c7 44 24 58 00 00 	movq   $0x0,0x58(%rsp)
  40cee0:	00 00 
  40cee2:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40cee7:	48 2b 44 24 60       	sub    0x60(%rsp),%rax
  40ceec:	48 c1 e8 04          	shr    $0x4,%rax
  40cef0:	48 c1 e0 04          	shl    $0x4,%rax
  40cef4:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40cef9:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40cefe:	48 3b 44 24 58       	cmp    0x58(%rsp),%rax
  40cf03:	0f 92 c0             	setb   %al
  40cf06:	24 01                	and    $0x1,%al
  40cf08:	3c 00                	cmp    $0x0,%al
  40cf0a:	0f 84 44 01 00 00    	je     40d054 <runtime::memory_compare+0x244>
  40cf10:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40cf15:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40cf1a:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40cf1e:	0f 29 44 24 40       	movaps %xmm0,0x40(%rsp)
  40cf23:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40cf28:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40cf2d:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40cf31:	0f 29 44 24 30       	movaps %xmm0,0x30(%rsp)
  40cf36:	0f 28 44 24 40       	movaps 0x40(%rsp),%xmm0
  40cf3b:	0f 28 4c 24 30       	movaps 0x30(%rsp),%xmm1
  40cf40:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  40cf44:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40cf48:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40cf4c:	0f 29 44 24 20       	movaps %xmm0,0x20(%rsp)
  40cf51:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  40cf56:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40cf5b:	66 0f eb c1          	por    %xmm1,%xmm0
  40cf5f:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40cf64:	66 0f eb c1          	por    %xmm1,%xmm0
  40cf68:	0f 28 c8             	movaps %xmm0,%xmm1
  40cf6b:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40cf70:	66 0f eb c1          	por    %xmm1,%xmm0
  40cf74:	0f 28 c8             	movaps %xmm0,%xmm1
  40cf77:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40cf7c:	66 0f eb c1          	por    %xmm1,%xmm0
  40cf80:	66 0f 7e c0          	movd   %xmm0,%eax
  40cf84:	3c 00                	cmp    $0x0,%al
  40cf86:	0f 95 c0             	setne  %al
  40cf89:	24 01                	and    $0x1,%al
  40cf8b:	3c 00                	cmp    $0x0,%al
  40cf8d:	0f 84 ac 00 00 00    	je     40d03f <runtime::memory_compare+0x22f>
  40cf93:	66 0f 76 c0          	pcmpeqd %xmm0,%xmm0
  40cf97:	0f 29 44 24 10       	movaps %xmm0,0x10(%rsp)
  40cf9c:	0f 28 05 ed 22 00 00 	movaps 0x22ed(%rip),%xmm0        # 40f290 <_IO_stdin_used+0x290>
  40cfa3:	0f 29 04 24          	movaps %xmm0,(%rsp)
  40cfa7:	0f 28 44 24 20       	movaps 0x20(%rsp),%xmm0
  40cfac:	0f 28 0c 24          	movaps (%rsp),%xmm1
  40cfb0:	0f 28 54 24 10       	movaps 0x10(%rsp),%xmm2
  40cfb5:	0f 57 db             	xorps  %xmm3,%xmm3
  40cfb8:	66 0f 74 c3          	pcmpeqb %xmm3,%xmm0
  40cfbc:	66 0f 38 10 ca       	pblendvb %xmm0,%xmm2,%xmm1
  40cfc1:	0f 28 c1             	movaps %xmm1,%xmm0
  40cfc4:	0f 29 44 24 f0       	movaps %xmm0,-0x10(%rsp)
  40cfc9:	0f 28 44 24 f0       	movaps -0x10(%rsp),%xmm0
  40cfce:	0f 28 c8             	movaps %xmm0,%xmm1
  40cfd1:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40cfd6:	66 0f da c1          	pminub %xmm1,%xmm0
  40cfda:	66 0f 38 41 c0       	phminposuw %xmm0,%xmm0
  40cfdf:	66 0f 7e c0          	movd   %xmm0,%eax
  40cfe3:	0f b6 c0             	movzbl %al,%eax
  40cfe6:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40cfeb:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40cff0:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40cff5:	48 03 4c 24 e8       	add    -0x18(%rsp),%rcx
  40cffa:	8a 04 08             	mov    (%rax,%rcx,1),%al
  40cffd:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40d002:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40d007:	48 03 54 24 e8       	add    -0x18(%rsp),%rdx
  40d00c:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  40d00f:	0f 92 c0             	setb   %al
  40d012:	24 01                	and    $0x1,%al
  40d014:	3c 00                	cmp    $0x0,%al
  40d016:	74 0e                	je     40d026 <runtime::memory_compare+0x216>
  40d018:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40d01f:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40d024:	eb 0c                	jmp    40d032 <runtime::memory_compare+0x222>
  40d026:	b8 01 00 00 00       	mov    $0x1,%eax
  40d02b:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40d030:	eb 00                	jmp    40d032 <runtime::memory_compare+0x222>
  40d032:	48 8b 44 24 90       	mov    -0x70(%rsp),%rax
  40d037:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40d03e:	c3                   	ret
  40d03f:	eb 00                	jmp    40d041 <runtime::memory_compare+0x231>
  40d041:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40d046:	48 83 c0 10          	add    $0x10,%rax
  40d04a:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40d04f:	e9 a5 fe ff ff       	jmp    40cef9 <runtime::memory_compare+0xe9>
  40d054:	48 8b 44 24 68       	mov    0x68(%rsp),%rax
  40d059:	48 2b 44 24 60       	sub    0x60(%rsp),%rax
  40d05e:	48 c1 e8 03          	shr    $0x3,%rax
  40d062:	48 c1 e0 03          	shl    $0x3,%rax
  40d066:	48 89 44 24 58       	mov    %rax,0x58(%rsp)
  40d06b:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40d070:	48 3b 44 24 58       	cmp    0x58(%rsp),%rax
  40d075:	0f 92 c0             	setb   %al
  40d078:	24 01                	and    $0x1,%al
  40d07a:	3c 00                	cmp    $0x0,%al
  40d07c:	0f 84 59 01 00 00    	je     40d1db <runtime::memory_compare+0x3cb>
  40d082:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40d087:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40d08c:	48 8b 04 08          	mov    (%rax,%rcx,1),%rax
  40d090:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40d095:	48 8b 44 24 70       	mov    0x70(%rsp),%rax
  40d09a:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40d09f:	48 8b 04 08          	mov    (%rax,%rcx,1),%rax
  40d0a3:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40d0a8:	f3 0f 7e 44 24 e0    	movq   -0x20(%rsp),%xmm0
  40d0ae:	f3 0f 7e 4c 24 d8    	movq   -0x28(%rsp),%xmm1
  40d0b4:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  40d0b8:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40d0bc:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40d0c0:	66 0f d6 44 24 d0    	movq   %xmm0,-0x30(%rsp)
  40d0c6:	f3 0f 7e 44 24 d0    	movq   -0x30(%rsp),%xmm0
  40d0cc:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40d0d1:	66 0f eb c1          	por    %xmm1,%xmm0
  40d0d5:	0f 28 c8             	movaps %xmm0,%xmm1
  40d0d8:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40d0dd:	66 0f eb c1          	por    %xmm1,%xmm0
  40d0e1:	0f 28 c8             	movaps %xmm0,%xmm1
  40d0e4:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40d0e9:	66 0f eb c1          	por    %xmm1,%xmm0
  40d0ed:	66 0f 7e c0          	movd   %xmm0,%eax
  40d0f1:	3c 00                	cmp    $0x0,%al
  40d0f3:	0f 95 c0             	setne  %al
  40d0f6:	24 01                	and    $0x1,%al
  40d0f8:	3c 00                	cmp    $0x0,%al
  40d0fa:	0f 84 c6 00 00 00    	je     40d1c6 <runtime::memory_compare+0x3b6>
  40d100:	48 c7 44 24 c8 ff ff 	movq   $0xffffffffffffffff,-0x38(%rsp)
  40d107:	ff ff 
  40d109:	48 b8 00 01 02 03 04 	movabs $0x706050403020100,%rax
  40d110:	05 06 07 
  40d113:	48 89 44 24 c0       	mov    %rax,-0x40(%rsp)
  40d118:	f3 0f 7e 44 24 d0    	movq   -0x30(%rsp),%xmm0
  40d11e:	f3 0f 7e 4c 24 c0    	movq   -0x40(%rsp),%xmm1
  40d124:	f3 0f 7e 54 24 c8    	movq   -0x38(%rsp),%xmm2
  40d12a:	0f 57 db             	xorps  %xmm3,%xmm3
  40d12d:	66 0f 74 c3          	pcmpeqb %xmm3,%xmm0
  40d131:	66 0f 38 10 ca       	pblendvb %xmm0,%xmm2,%xmm1
  40d136:	0f 28 c1             	movaps %xmm1,%xmm0
  40d139:	66 0f d6 44 24 b8    	movq   %xmm0,-0x48(%rsp)
  40d13f:	f3 0f 7e 44 24 b8    	movq   -0x48(%rsp),%xmm0
  40d145:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40d14a:	66 0f da c1          	pminub %xmm1,%xmm0
  40d14e:	0f 28 c8             	movaps %xmm0,%xmm1
  40d151:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40d156:	66 0f da c1          	pminub %xmm1,%xmm0
  40d15a:	0f 28 c8             	movaps %xmm0,%xmm1
  40d15d:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40d162:	66 0f da c1          	pminub %xmm1,%xmm0
  40d166:	66 0f 7e c0          	movd   %xmm0,%eax
  40d16a:	0f b6 c0             	movzbl %al,%eax
  40d16d:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40d172:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40d177:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40d17c:	48 03 4c 24 b0       	add    -0x50(%rsp),%rcx
  40d181:	8a 04 08             	mov    (%rax,%rcx,1),%al
  40d184:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40d189:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40d18e:	48 03 54 24 b0       	add    -0x50(%rsp),%rdx
  40d193:	3a 04 11             	cmp    (%rcx,%rdx,1),%al
  40d196:	0f 92 c0             	setb   %al
  40d199:	24 01                	and    $0x1,%al
  40d19b:	3c 00                	cmp    $0x0,%al
  40d19d:	74 0e                	je     40d1ad <runtime::memory_compare+0x39d>
  40d19f:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40d1a6:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40d1ab:	eb 0c                	jmp    40d1b9 <runtime::memory_compare+0x3a9>
  40d1ad:	b8 01 00 00 00       	mov    $0x1,%eax
  40d1b2:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40d1b7:	eb 00                	jmp    40d1b9 <runtime::memory_compare+0x3a9>
  40d1b9:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40d1be:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40d1c5:	c3                   	ret
  40d1c6:	eb 00                	jmp    40d1c8 <runtime::memory_compare+0x3b8>
  40d1c8:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40d1cd:	48 83 c0 08          	add    $0x8,%rax
  40d1d1:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40d1d6:	e9 90 fe ff ff       	jmp    40d06b <runtime::memory_compare+0x25b>
  40d1db:	eb 00                	jmp    40d1dd <runtime::memory_compare+0x3cd>
  40d1dd:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40d1e2:	48 3b 44 24 68       	cmp    0x68(%rsp),%rax
  40d1e7:	0f 92 c0             	setb   %al
  40d1ea:	24 01                	and    $0x1,%al
  40d1ec:	3c 00                	cmp    $0x0,%al
  40d1ee:	0f 84 8d 00 00 00    	je     40d281 <runtime::memory_compare+0x471>
  40d1f4:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40d1f9:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40d1fe:	8a 04 08             	mov    (%rax,%rcx,1),%al
  40d201:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40d206:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40d20b:	32 04 11             	xor    (%rcx,%rdx,1),%al
  40d20e:	3c 00                	cmp    $0x0,%al
  40d210:	0f 95 c0             	setne  %al
  40d213:	24 01                	and    $0x1,%al
  40d215:	3c 00                	cmp    $0x0,%al
  40d217:	74 53                	je     40d26c <runtime::memory_compare+0x45c>
  40d219:	48 8b 44 24 78       	mov    0x78(%rsp),%rax
  40d21e:	48 8b 4c 24 60       	mov    0x60(%rsp),%rcx
  40d223:	0f b6 04 08          	movzbl (%rax,%rcx,1),%eax
  40d227:	48 8b 4c 24 70       	mov    0x70(%rsp),%rcx
  40d22c:	48 8b 54 24 60       	mov    0x60(%rsp),%rdx
  40d231:	0f b6 0c 11          	movzbl (%rcx,%rdx,1),%ecx
  40d235:	48 29 c8             	sub    %rcx,%rax
  40d238:	48 83 f8 00          	cmp    $0x0,%rax
  40d23c:	0f 9c c0             	setl   %al
  40d23f:	24 01                	and    $0x1,%al
  40d241:	3c 00                	cmp    $0x0,%al
  40d243:	74 0e                	je     40d253 <runtime::memory_compare+0x443>
  40d245:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  40d24c:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40d251:	eb 0c                	jmp    40d25f <runtime::memory_compare+0x44f>
  40d253:	b8 01 00 00 00       	mov    $0x1,%eax
  40d258:	48 89 44 24 80       	mov    %rax,-0x80(%rsp)
  40d25d:	eb 00                	jmp    40d25f <runtime::memory_compare+0x44f>
  40d25f:	48 8b 44 24 80       	mov    -0x80(%rsp),%rax
  40d264:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40d26b:	c3                   	ret
  40d26c:	eb 00                	jmp    40d26e <runtime::memory_compare+0x45e>
  40d26e:	48 8b 44 24 60       	mov    0x60(%rsp),%rax
  40d273:	48 83 c0 01          	add    $0x1,%rax
  40d277:	48 89 44 24 60       	mov    %rax,0x60(%rsp)
  40d27c:	e9 5c ff ff ff       	jmp    40d1dd <runtime::memory_compare+0x3cd>
  40d281:	31 c0                	xor    %eax,%eax
  40d283:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40d28a:	c3                   	ret
  40d28b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

000000000040d290 <runtime::memory_compare_zero>:
  40d290:	50                   	push   %rax
  40d291:	48 89 7c 24 88       	mov    %rdi,-0x78(%rsp)
  40d296:	48 89 74 24 90       	mov    %rsi,-0x70(%rsp)
  40d29b:	48 8b 44 24 88       	mov    -0x78(%rsp),%rax
  40d2a0:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40d2a5:	48 89 04 24          	mov    %rax,(%rsp)
  40d2a9:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d2ae:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40d2b3:	48 c7 44 24 e8 00 00 	movq   $0x0,-0x18(%rsp)
  40d2ba:	00 00 
  40d2bc:	48 c7 44 24 e0 00 00 	movq   $0x0,-0x20(%rsp)
  40d2c3:	00 00 
  40d2c5:	48 89 44 24 d8       	mov    %rax,-0x28(%rsp)
  40d2ca:	48 83 7c 24 f0 08    	cmpq   $0x8,-0x10(%rsp)
  40d2d0:	0f 93 c0             	setae  %al
  40d2d3:	24 01                	and    $0x1,%al
  40d2d5:	3c 00                	cmp    $0x0,%al
  40d2d7:	0f 84 24 01 00 00    	je     40d401 <runtime::memory_compare_zero+0x171>
  40d2dd:	0f 57 c0             	xorps  %xmm0,%xmm0
  40d2e0:	0f 29 44 24 c0       	movaps %xmm0,-0x40(%rsp)
  40d2e5:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40d2ea:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  40d2ef:	48 c1 e8 04          	shr    $0x4,%rax
  40d2f3:	48 c1 e0 04          	shl    $0x4,%rax
  40d2f7:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40d2fc:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d301:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  40d306:	0f 92 c0             	setb   %al
  40d309:	24 01                	and    $0x1,%al
  40d30b:	3c 00                	cmp    $0x0,%al
  40d30d:	0f 84 88 00 00 00    	je     40d39b <runtime::memory_compare_zero+0x10b>
  40d313:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d318:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40d31d:	0f 10 04 08          	movups (%rax,%rcx,1),%xmm0
  40d321:	0f 29 44 24 b0       	movaps %xmm0,-0x50(%rsp)
  40d326:	0f 28 44 24 c0       	movaps -0x40(%rsp),%xmm0
  40d32b:	0f 28 4c 24 b0       	movaps -0x50(%rsp),%xmm1
  40d330:	66 0f 74 c1          	pcmpeqb %xmm1,%xmm0
  40d334:	66 0f 76 c9          	pcmpeqd %xmm1,%xmm1
  40d338:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40d33c:	0f 29 44 24 a0       	movaps %xmm0,-0x60(%rsp)
  40d341:	0f 28 44 24 a0       	movaps -0x60(%rsp),%xmm0
  40d346:	66 0f 70 c8 ee       	pshufd $0xee,%xmm0,%xmm1
  40d34b:	66 0f eb c1          	por    %xmm1,%xmm0
  40d34f:	66 0f 70 c8 55       	pshufd $0x55,%xmm0,%xmm1
  40d354:	66 0f eb c1          	por    %xmm1,%xmm0
  40d358:	0f 28 c8             	movaps %xmm0,%xmm1
  40d35b:	66 0f 72 d1 10       	psrld  $0x10,%xmm1
  40d360:	66 0f eb c1          	por    %xmm1,%xmm0
  40d364:	0f 28 c8             	movaps %xmm0,%xmm1
  40d367:	66 0f 71 d1 08       	psrlw  $0x8,%xmm1
  40d36c:	66 0f eb c1          	por    %xmm1,%xmm0
  40d370:	66 0f 7e c0          	movd   %xmm0,%eax
  40d374:	3c 00                	cmp    $0x0,%al
  40d376:	0f 95 c0             	setne  %al
  40d379:	24 01                	and    $0x1,%al
  40d37b:	3c 00                	cmp    $0x0,%al
  40d37d:	74 07                	je     40d386 <runtime::memory_compare_zero+0xf6>
  40d37f:	b8 01 00 00 00       	mov    $0x1,%eax
  40d384:	59                   	pop    %rcx
  40d385:	c3                   	ret
  40d386:	eb 00                	jmp    40d388 <runtime::memory_compare_zero+0xf8>
  40d388:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d38d:	48 83 c0 10          	add    $0x10,%rax
  40d391:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d396:	e9 61 ff ff ff       	jmp    40d2fc <runtime::memory_compare_zero+0x6c>
  40d39b:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40d3a0:	48 2b 44 24 e8       	sub    -0x18(%rsp),%rax
  40d3a5:	48 c1 e8 03          	shr    $0x3,%rax
  40d3a9:	48 c1 e0 03          	shl    $0x3,%rax
  40d3ad:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40d3b2:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d3b7:	48 3b 44 24 e0       	cmp    -0x20(%rsp),%rax
  40d3bc:	0f 92 c0             	setb   %al
  40d3bf:	24 01                	and    $0x1,%al
  40d3c1:	3c 00                	cmp    $0x0,%al
  40d3c3:	74 3a                	je     40d3ff <runtime::memory_compare_zero+0x16f>
  40d3c5:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d3ca:	48 03 44 24 e8       	add    -0x18(%rsp),%rax
  40d3cf:	48 8b 00             	mov    (%rax),%rax
  40d3d2:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40d3d7:	48 83 7c 24 98 00    	cmpq   $0x0,-0x68(%rsp)
  40d3dd:	0f 95 c0             	setne  %al
  40d3e0:	24 01                	and    $0x1,%al
  40d3e2:	3c 00                	cmp    $0x0,%al
  40d3e4:	74 07                	je     40d3ed <runtime::memory_compare_zero+0x15d>
  40d3e6:	b8 01 00 00 00       	mov    $0x1,%eax
  40d3eb:	59                   	pop    %rcx
  40d3ec:	c3                   	ret
  40d3ed:	eb 00                	jmp    40d3ef <runtime::memory_compare_zero+0x15f>
  40d3ef:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d3f4:	48 83 c0 08          	add    $0x8,%rax
  40d3f8:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d3fd:	eb b3                	jmp    40d3b2 <runtime::memory_compare_zero+0x122>
  40d3ff:	eb 00                	jmp    40d401 <runtime::memory_compare_zero+0x171>
  40d401:	eb 00                	jmp    40d403 <runtime::memory_compare_zero+0x173>
  40d403:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d408:	48 3b 44 24 f0       	cmp    -0x10(%rsp),%rax
  40d40d:	0f 92 c0             	setb   %al
  40d410:	24 01                	and    $0x1,%al
  40d412:	3c 00                	cmp    $0x0,%al
  40d414:	74 30                	je     40d446 <runtime::memory_compare_zero+0x1b6>
  40d416:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d41b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40d420:	80 3c 08 00          	cmpb   $0x0,(%rax,%rcx,1)
  40d424:	0f 95 c0             	setne  %al
  40d427:	24 01                	and    $0x1,%al
  40d429:	3c 00                	cmp    $0x0,%al
  40d42b:	74 07                	je     40d434 <runtime::memory_compare_zero+0x1a4>
  40d42d:	b8 01 00 00 00       	mov    $0x1,%eax
  40d432:	59                   	pop    %rcx
  40d433:	c3                   	ret
  40d434:	eb 00                	jmp    40d436 <runtime::memory_compare_zero+0x1a6>
  40d436:	48 8b 44 24 e8       	mov    -0x18(%rsp),%rax
  40d43b:	48 83 c0 01          	add    $0x1,%rax
  40d43f:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d444:	eb bd                	jmp    40d403 <runtime::memory_compare_zero+0x173>
  40d446:	31 c0                	xor    %eax,%eax
  40d448:	59                   	pop    %rcx
  40d449:	c3                   	ret
  40d44a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

000000000040d450 <__truncsfhf2>:
  40d450:	48 83 ec 18          	sub    $0x18,%rsp
  40d454:	f3 0f 11 44 24 8c    	movss  %xmm0,-0x74(%rsp)
  40d45a:	f3 0f 10 44 24 8c    	movss  -0x74(%rsp),%xmm0
  40d460:	f3 0f 11 44 24 14    	movss  %xmm0,0x14(%rsp)
  40d466:	c7 44 24 10 00 00 00 	movl   $0x0,0x10(%rsp)
  40d46d:	00 
  40d46e:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
  40d475:	00 
  40d476:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
  40d47d:	00 
  40d47e:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
  40d485:	00 
  40d486:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  40d48d:	f3 0f 11 44 24 10    	movss  %xmm0,0x10(%rsp)
  40d493:	8b 44 24 10          	mov    0x10(%rsp),%eax
  40d497:	89 44 24 0c          	mov    %eax,0xc(%rsp)
  40d49b:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40d49f:	c1 f9 10             	sar    $0x10,%ecx
  40d4a2:	b2 01                	mov    $0x1,%dl
  40d4a4:	31 c0                	xor    %eax,%eax
  40d4a6:	f6 c2 01             	test   $0x1,%dl
  40d4a9:	0f 45 c1             	cmovne %ecx,%eax
  40d4ac:	25 00 80 00 00       	and    $0x8000,%eax
  40d4b1:	89 44 24 08          	mov    %eax,0x8(%rsp)
  40d4b5:	8b 4c 24 0c          	mov    0xc(%rsp),%ecx
  40d4b9:	c1 f9 17             	sar    $0x17,%ecx
  40d4bc:	b2 01                	mov    $0x1,%dl
  40d4be:	31 c0                	xor    %eax,%eax
  40d4c0:	f6 c2 01             	test   $0x1,%dl
  40d4c3:	0f 45 c1             	cmovne %ecx,%eax
  40d4c6:	25 ff 00 00 00       	and    $0xff,%eax
  40d4cb:	83 e8 70             	sub    $0x70,%eax
  40d4ce:	89 44 24 04          	mov    %eax,0x4(%rsp)
  40d4d2:	8b 44 24 0c          	mov    0xc(%rsp),%eax
  40d4d6:	25 ff ff 7f 00       	and    $0x7fffff,%eax
  40d4db:	89 04 24             	mov    %eax,(%rsp)
  40d4de:	83 7c 24 04 00       	cmpl   $0x0,0x4(%rsp)
  40d4e3:	0f 9e c0             	setle  %al
  40d4e6:	24 01                	and    $0x1,%al
  40d4e8:	3c 00                	cmp    $0x0,%al
  40d4ea:	0f 84 82 00 00 00    	je     40d572 <__truncsfhf2+0x122>
  40d4f0:	83 7c 24 04 f6       	cmpl   $0xfffffff6,0x4(%rsp)
  40d4f5:	0f 9c c0             	setl   %al
  40d4f8:	24 01                	and    $0x1,%al
  40d4fa:	3c 00                	cmp    $0x0,%al
  40d4fc:	74 16                	je     40d514 <__truncsfhf2+0xc4>
  40d4fe:	66 8b 44 24 08       	mov    0x8(%rsp),%ax
  40d503:	66 89 44 24 f0       	mov    %ax,-0x10(%rsp)
  40d508:	66 0f c4 44 24 f0 00 	pinsrw $0x0,-0x10(%rsp),%xmm0
  40d50f:	48 83 c4 18          	add    $0x18,%rsp
  40d513:	c3                   	ret
  40d514:	8b 04 24             	mov    (%rsp),%eax
  40d517:	0d 00 00 80 00       	or     $0x800000,%eax
  40d51c:	ba 01 00 00 00       	mov    $0x1,%edx
  40d521:	2b 54 24 04          	sub    0x4(%rsp),%edx
  40d525:	89 d1                	mov    %edx,%ecx
  40d527:	d3 f8                	sar    %cl,%eax
  40d529:	89 c1                	mov    %eax,%ecx
  40d52b:	31 c0                	xor    %eax,%eax
  40d52d:	83 fa 20             	cmp    $0x20,%edx
  40d530:	0f 42 c1             	cmovb  %ecx,%eax
  40d533:	89 04 24             	mov    %eax,(%rsp)
  40d536:	8b 04 24             	mov    (%rsp),%eax
  40d539:	25 00 10 00 00       	and    $0x1000,%eax
  40d53e:	83 f8 00             	cmp    $0x0,%eax
  40d541:	0f 95 c0             	setne  %al
  40d544:	24 01                	and    $0x1,%al
  40d546:	3c 00                	cmp    $0x0,%al
  40d548:	74 0b                	je     40d555 <__truncsfhf2+0x105>
  40d54a:	8b 04 24             	mov    (%rsp),%eax
  40d54d:	05 00 20 00 00       	add    $0x2000,%eax
  40d552:	89 04 24             	mov    %eax,(%rsp)
  40d555:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d559:	8b 0c 24             	mov    (%rsp),%ecx
  40d55c:	c1 e9 0d             	shr    $0xd,%ecx
  40d55f:	09 c8                	or     %ecx,%eax
  40d561:	66 89 44 24 e0       	mov    %ax,-0x20(%rsp)
  40d566:	66 0f c4 44 24 e0 00 	pinsrw $0x0,-0x20(%rsp),%xmm0
  40d56d:	48 83 c4 18          	add    $0x18,%rsp
  40d571:	c3                   	ret
  40d572:	81 7c 24 04 8f 00 00 	cmpl   $0x8f,0x4(%rsp)
  40d579:	00 
  40d57a:	0f 94 c0             	sete   %al
  40d57d:	24 01                	and    $0x1,%al
  40d57f:	3c 00                	cmp    $0x0,%al
  40d581:	74 59                	je     40d5dc <__truncsfhf2+0x18c>
  40d583:	83 3c 24 00          	cmpl   $0x0,(%rsp)
  40d587:	0f 94 c0             	sete   %al
  40d58a:	24 01                	and    $0x1,%al
  40d58c:	3c 00                	cmp    $0x0,%al
  40d58e:	74 1a                	je     40d5aa <__truncsfhf2+0x15a>
  40d590:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d594:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40d599:	66 89 44 24 d0       	mov    %ax,-0x30(%rsp)
  40d59e:	66 0f c4 44 24 d0 00 	pinsrw $0x0,-0x30(%rsp),%xmm0
  40d5a5:	48 83 c4 18          	add    $0x18,%rsp
  40d5a9:	c3                   	ret
  40d5aa:	8b 04 24             	mov    (%rsp),%eax
  40d5ad:	c1 f8 0d             	sar    $0xd,%eax
  40d5b0:	89 04 24             	mov    %eax,(%rsp)
  40d5b3:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d5b7:	8b 0c 24             	mov    (%rsp),%ecx
  40d5ba:	09 c8                	or     %ecx,%eax
  40d5bc:	85 c9                	test   %ecx,%ecx
  40d5be:	0f 94 c1             	sete   %cl
  40d5c1:	0f b6 c9             	movzbl %cl,%ecx
  40d5c4:	09 c8                	or     %ecx,%eax
  40d5c6:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40d5cb:	66 89 44 24 c0       	mov    %ax,-0x40(%rsp)
  40d5d0:	66 0f c4 44 24 c0 00 	pinsrw $0x0,-0x40(%rsp),%xmm0
  40d5d7:	48 83 c4 18          	add    $0x18,%rsp
  40d5db:	c3                   	ret
  40d5dc:	8b 04 24             	mov    (%rsp),%eax
  40d5df:	25 00 10 00 00       	and    $0x1000,%eax
  40d5e4:	83 f8 00             	cmp    $0x0,%eax
  40d5e7:	0f 95 c0             	setne  %al
  40d5ea:	24 01                	and    $0x1,%al
  40d5ec:	3c 00                	cmp    $0x0,%al
  40d5ee:	74 33                	je     40d623 <__truncsfhf2+0x1d3>
  40d5f0:	8b 04 24             	mov    (%rsp),%eax
  40d5f3:	05 00 20 00 00       	add    $0x2000,%eax
  40d5f8:	89 04 24             	mov    %eax,(%rsp)
  40d5fb:	8b 04 24             	mov    (%rsp),%eax
  40d5fe:	25 00 00 80 00       	and    $0x800000,%eax
  40d603:	83 f8 00             	cmp    $0x0,%eax
  40d606:	0f 95 c0             	setne  %al
  40d609:	24 01                	and    $0x1,%al
  40d60b:	3c 00                	cmp    $0x0,%al
  40d60d:	74 12                	je     40d621 <__truncsfhf2+0x1d1>
  40d60f:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
  40d616:	8b 44 24 04          	mov    0x4(%rsp),%eax
  40d61a:	83 c0 01             	add    $0x1,%eax
  40d61d:	89 44 24 04          	mov    %eax,0x4(%rsp)
  40d621:	eb 00                	jmp    40d623 <__truncsfhf2+0x1d3>
  40d623:	83 7c 24 04 1e       	cmpl   $0x1e,0x4(%rsp)
  40d628:	0f 9f c0             	setg   %al
  40d62b:	24 01                	and    $0x1,%al
  40d62d:	3c 00                	cmp    $0x0,%al
  40d62f:	74 75                	je     40d6a6 <__truncsfhf2+0x256>
  40d631:	48 b8 00 10 a5 d4 e8 	movabs $0xe8d4a51000,%rax
  40d638:	00 00 00 
  40d63b:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40d640:	48 c7 44 24 b0 00 00 	movq   $0x0,-0x50(%rsp)
  40d647:	00 00 
  40d649:	48 83 7c 24 b0 0a    	cmpq   $0xa,-0x50(%rsp)
  40d64f:	0f 9c c0             	setl   %al
  40d652:	24 01                	and    $0x1,%al
  40d654:	3c 00                	cmp    $0x0,%al
  40d656:	74 34                	je     40d68c <__truncsfhf2+0x23c>
  40d658:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40d65d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40d662:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40d667:	48 0f af 44 24 a8    	imul   -0x58(%rsp),%rax
  40d66d:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40d672:	48 8b 44 24 a8       	mov    -0x58(%rsp),%rax
  40d677:	48 89 44 24 b8       	mov    %rax,-0x48(%rsp)
  40d67c:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40d681:	48 83 c0 01          	add    $0x1,%rax
  40d685:	48 89 44 24 b0       	mov    %rax,-0x50(%rsp)
  40d68a:	eb bd                	jmp    40d649 <__truncsfhf2+0x1f9>
  40d68c:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d690:	0d 00 7c 00 00       	or     $0x7c00,%eax
  40d695:	66 89 44 24 a0       	mov    %ax,-0x60(%rsp)
  40d69a:	66 0f c4 44 24 a0 00 	pinsrw $0x0,-0x60(%rsp),%xmm0
  40d6a1:	48 83 c4 18          	add    $0x18,%rsp
  40d6a5:	c3                   	ret
  40d6a6:	8b 44 24 08          	mov    0x8(%rsp),%eax
  40d6aa:	8b 4c 24 04          	mov    0x4(%rsp),%ecx
  40d6ae:	c1 e1 0a             	shl    $0xa,%ecx
  40d6b1:	09 c8                	or     %ecx,%eax
  40d6b3:	8b 0c 24             	mov    (%rsp),%ecx
  40d6b6:	c1 e9 0d             	shr    $0xd,%ecx
  40d6b9:	09 c8                	or     %ecx,%eax
  40d6bb:	66 89 44 24 90       	mov    %ax,-0x70(%rsp)
  40d6c0:	66 0f c4 44 24 90 00 	pinsrw $0x0,-0x70(%rsp),%xmm0
  40d6c7:	48 83 c4 18          	add    $0x18,%rsp
  40d6cb:	c3                   	ret
  40d6cc:	0f 1f 40 00          	nopl   0x0(%rax)

000000000040d6d0 <__truncdfhf2>:
  40d6d0:	48 83 ec 18          	sub    $0x18,%rsp
  40d6d4:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
  40d6da:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
  40d6e0:	f2 0f 11 44 24 10    	movsd  %xmm0,0x10(%rsp)
  40d6e6:	f2 0f 5a c0          	cvtsd2ss %xmm0,%xmm0
  40d6ea:	e8 61 fd ff ff       	call   40d450 <__truncsfhf2>
  40d6ef:	48 83 c4 18          	add    $0x18,%rsp
  40d6f3:	c3                   	ret
  40d6f4:	66 66 66 2e 0f 1f 84 	data16 data16 cs nopw 0x0(%rax,%rax,1)
  40d6fb:	00 00 00 00 00 

000000000040d700 <__gnu_h2f_ieee>:
  40d700:	f3 0f 11 44 24 e4    	movss  %xmm0,-0x1c(%rsp)
  40d706:	f3 0f 10 44 24 e4    	movss  -0x1c(%rsp),%xmm0
  40d70c:	66 0f 3a 15 44 24 fe 	pextrw $0x0,%xmm0,-0x2(%rsp)
  40d713:	00 
  40d714:	66 0f 3a 15 44 24 f8 	pextrw $0x0,%xmm0,-0x8(%rsp)
  40d71b:	00 
  40d71c:	66 8b 44 24 f8       	mov    -0x8(%rsp),%ax
  40d721:	66 89 44 24 f6       	mov    %ax,-0xa(%rsp)
  40d726:	c7 44 24 f0 00 00 00 	movl   $0x0,-0x10(%rsp)
  40d72d:	00 
  40d72e:	c7 44 24 ec 00 00 00 	movl   $0x0,-0x14(%rsp)
  40d735:	00 
  40d736:	c7 44 24 e8 00 00 00 	movl   $0x0,-0x18(%rsp)
  40d73d:	00 
  40d73e:	c7 44 24 ec 00 00 80 	movl   $0x77800000,-0x14(%rsp)
  40d745:	77 
  40d746:	c7 44 24 e8 00 00 80 	movl   $0x47800000,-0x18(%rsp)
  40d74d:	47 
  40d74e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  40d753:	66 25 ff 7f          	and    $0x7fff,%ax
  40d757:	0f b7 c8             	movzwl %ax,%ecx
  40d75a:	c1 e1 0d             	shl    $0xd,%ecx
  40d75d:	b2 01                	mov    $0x1,%dl
  40d75f:	31 c0                	xor    %eax,%eax
  40d761:	f6 c2 01             	test   $0x1,%dl
  40d764:	0f 45 c1             	cmovne %ecx,%eax
  40d767:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40d76b:	f3 0f 10 44 24 ec    	movss  -0x14(%rsp),%xmm0
  40d771:	f3 0f 59 44 24 f0    	mulss  -0x10(%rsp),%xmm0
  40d777:	f3 0f 11 44 24 f0    	movss  %xmm0,-0x10(%rsp)
  40d77d:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  40d783:	0f 2e 44 24 e8       	ucomiss -0x18(%rsp),%xmm0
  40d788:	0f 93 c0             	setae  %al
  40d78b:	24 01                	and    $0x1,%al
  40d78d:	3c 00                	cmp    $0x0,%al
  40d78f:	74 0d                	je     40d79e <__gnu_h2f_ieee+0x9e>
  40d791:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  40d795:	0d 00 00 80 7f       	or     $0x7f800000,%eax
  40d79a:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40d79e:	66 8b 44 24 f6       	mov    -0xa(%rsp),%ax
  40d7a3:	66 25 00 80          	and    $0x8000,%ax
  40d7a7:	0f b7 c8             	movzwl %ax,%ecx
  40d7aa:	c1 e1 10             	shl    $0x10,%ecx
  40d7ad:	b2 01                	mov    $0x1,%dl
  40d7af:	31 c0                	xor    %eax,%eax
  40d7b1:	f6 c2 01             	test   $0x1,%dl
  40d7b4:	0f 45 c1             	cmovne %ecx,%eax
  40d7b7:	0b 44 24 f0          	or     -0x10(%rsp),%eax
  40d7bb:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  40d7bf:	f3 0f 10 44 24 f0    	movss  -0x10(%rsp),%xmm0
  40d7c5:	c3                   	ret
  40d7c6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40d7cd:	00 00 00 

000000000040d7d0 <__gnu_f2h_ieee>:
  40d7d0:	50                   	push   %rax
  40d7d1:	f3 0f 11 04 24       	movss  %xmm0,(%rsp)
  40d7d6:	f3 0f 10 04 24       	movss  (%rsp),%xmm0
  40d7db:	f3 0f 11 44 24 04    	movss  %xmm0,0x4(%rsp)
  40d7e1:	e8 6a fc ff ff       	call   40d450 <__truncsfhf2>
  40d7e6:	58                   	pop    %rax
  40d7e7:	c3                   	ret
  40d7e8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40d7ef:	00 

000000000040d7f0 <__extendhfsf2>:
  40d7f0:	50                   	push   %rax
  40d7f1:	f3 0f 11 44 24 02    	movss  %xmm0,0x2(%rsp)
  40d7f7:	f3 0f 10 44 24 02    	movss  0x2(%rsp),%xmm0
  40d7fd:	0f 28 c8             	movaps %xmm0,%xmm1
  40d800:	66 0f 3a 15 4c 24 06 	pextrw $0x0,%xmm1,0x6(%rsp)
  40d807:	00 
  40d808:	e8 f3 fe ff ff       	call   40d700 <__gnu_h2f_ieee>
  40d80d:	58                   	pop    %rax
  40d80e:	c3                   	ret
  40d80f:	90                   	nop

000000000040d810 <__floattidf>:
  40d810:	53                   	push   %rbx
  40d811:	48 83 ec 10          	sub    $0x10,%rsp
  40d815:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40d81a:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40d81f:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40d824:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40d829:	48 89 04 24          	mov    %rax,(%rsp)
  40d82d:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40d832:	48 09 c8             	or     %rcx,%rax
  40d835:	0f 94 c0             	sete   %al
  40d838:	24 01                	and    $0x1,%al
  40d83a:	3c 00                	cmp    $0x0,%al
  40d83c:	74 09                	je     40d847 <__floattidf+0x37>
  40d83e:	0f 57 c0             	xorps  %xmm0,%xmm0
  40d841:	48 83 c4 10          	add    $0x10,%rsp
  40d845:	5b                   	pop    %rbx
  40d846:	c3                   	ret
  40d847:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40d84c:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40d851:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40d856:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40d85b:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40d860:	48 c1 f8 3f          	sar    $0x3f,%rax
  40d864:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40d869:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40d86e:	48 8b 4c 24 f0       	mov    -0x10(%rsp),%rcx
  40d873:	48 8b 44 24 f8       	mov    -0x8(%rsp),%rax
  40d878:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40d87d:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40d882:	48 31 d0             	xor    %rdx,%rax
  40d885:	48 31 f1             	xor    %rsi,%rcx
  40d888:	48 29 f1             	sub    %rsi,%rcx
  40d88b:	48 19 d0             	sbb    %rdx,%rax
  40d88e:	48 89 4c 24 f0       	mov    %rcx,-0x10(%rsp)
  40d893:	48 89 44 24 f8       	mov    %rax,-0x8(%rsp)
  40d898:	48 8b 74 24 f0       	mov    -0x10(%rsp),%rsi
  40d89d:	48 8b 54 24 f8       	mov    -0x8(%rsp),%rdx
  40d8a2:	48 0f bd c2          	bsr    %rdx,%rax
  40d8a6:	48 83 f0 3f          	xor    $0x3f,%rax
  40d8aa:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40d8af:	48 0f bd ce          	bsr    %rsi,%rcx
  40d8b3:	48 83 f1 3f          	xor    $0x3f,%rcx
  40d8b7:	48 83 c1 40          	add    $0x40,%rcx
  40d8bb:	48 85 d2             	test   %rdx,%rdx
  40d8be:	48 0f 45 c8          	cmovne %rax,%rcx
  40d8c2:	31 c0                	xor    %eax,%eax
  40d8c4:	ba 80 00 00 00       	mov    $0x80,%edx
  40d8c9:	48 29 ca             	sub    %rcx,%rdx
  40d8cc:	48 89 c1             	mov    %rax,%rcx
  40d8cf:	48 19 c9             	sbb    %rcx,%rcx
  40d8d2:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40d8d7:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  40d8dc:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  40d8e0:	ff c9                	dec    %ecx
  40d8e2:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  40d8e6:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  40d8eb:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40d8f0:	ba 35 00 00 00       	mov    $0x35,%edx
  40d8f5:	48 29 f2             	sub    %rsi,%rdx
  40d8f8:	48 19 c8             	sbb    %rcx,%rax
  40d8fb:	0f 9c c0             	setl   %al
  40d8fe:	24 01                	and    $0x1,%al
  40d900:	3c 00                	cmp    $0x0,%al
  40d902:	0f 84 c0 01 00 00    	je     40dac8 <__floattidf+0x2b8>
  40d908:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40d90d:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40d912:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d917:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40d91c:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40d921:	0f 28 0d 78 19 00 00 	movaps 0x1978(%rip),%xmm1        # 40f2a0 <_IO_stdin_used+0x2a0>
  40d928:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40d92c:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  40d931:	74 17                	je     40d94a <__floattidf+0x13a>
  40d933:	eb 00                	jmp    40d935 <__floattidf+0x125>
  40d935:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  40d93a:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40d93f:	48 83 f0 37          	xor    $0x37,%rax
  40d943:	48 09 c8             	or     %rcx,%rax
  40d946:	74 26                	je     40d96e <__floattidf+0x15e>
  40d948:	eb 29                	jmp    40d973 <__floattidf+0x163>
  40d94a:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40d94f:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40d954:	48 89 d0             	mov    %rdx,%rax
  40d957:	48 01 c0             	add    %rax,%rax
  40d95a:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40d95f:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40d964:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40d969:	e9 d6 00 00 00       	jmp    40da44 <__floattidf+0x234>
  40d96e:	e9 d1 00 00 00       	jmp    40da44 <__floattidf+0x234>
  40d973:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40d978:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  40d97d:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  40d982:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40d987:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40d98c:	49 89 fb             	mov    %rdi,%r11
  40d98f:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  40d993:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  40d997:	44 88 db             	mov    %r11b,%bl
  40d99a:	88 d9                	mov    %bl,%cl
  40d99c:	49 89 f2             	mov    %rsi,%r10
  40d99f:	49 d3 ea             	shr    %cl,%r10
  40d9a2:	88 d9                	mov    %bl,%cl
  40d9a4:	49 89 d1             	mov    %rdx,%r9
  40d9a7:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40d9ab:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40d9b0:	45 31 c0             	xor    %r8d,%r8d
  40d9b3:	f6 c3 40             	test   $0x40,%bl
  40d9b6:	4d 0f 45 ca          	cmovne %r10,%r9
  40d9ba:	4d 0f 45 d0          	cmovne %r8,%r10
  40d9be:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  40d9c5:	48 83 d8 00          	sbb    $0x0,%rax
  40d9c9:	4c 89 c0             	mov    %r8,%rax
  40d9cc:	49 0f 42 c2          	cmovb  %r10,%rax
  40d9d0:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40d9d5:	4c 89 c0             	mov    %r8,%rax
  40d9d8:	49 0f 42 c1          	cmovb  %r9,%rax
  40d9dc:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  40d9e2:	49 29 fb             	sub    %rdi,%r11
  40d9e5:	4c 89 c7             	mov    %r8,%rdi
  40d9e8:	48 19 cf             	sbb    %rcx,%rdi
  40d9eb:	45 88 d9             	mov    %r11b,%r9b
  40d9ee:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  40d9f5:	44 88 c9             	mov    %r9b,%cl
  40d9f8:	4c 89 d3             	mov    %r10,%rbx
  40d9fb:	48 d3 eb             	shr    %cl,%rbx
  40d9fe:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40da03:	41 f6 c1 40          	test   $0x40,%r9b
  40da07:	49 89 d9             	mov    %rbx,%r9
  40da0a:	4d 0f 45 c8          	cmovne %r8,%r9
  40da0e:	4c 0f 45 d3          	cmovne %rbx,%r10
  40da12:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  40da19:	48 83 df 00          	sbb    $0x0,%rdi
  40da1d:	4c 89 c7             	mov    %r8,%rdi
  40da20:	49 0f 42 fa          	cmovb  %r10,%rdi
  40da24:	4d 0f 42 c1          	cmovb  %r9,%r8
  40da28:	4c 21 c6             	and    %r8,%rsi
  40da2b:	48 21 fa             	and    %rdi,%rdx
  40da2e:	48 09 f2             	or     %rsi,%rdx
  40da31:	0f 95 c2             	setne  %dl
  40da34:	0f b6 d2             	movzbl %dl,%edx
  40da37:	48 09 d0             	or     %rdx,%rax
  40da3a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40da3f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40da44:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40da49:	89 c1                	mov    %eax,%ecx
  40da4b:	83 e1 04             	and    $0x4,%ecx
  40da4e:	c1 e9 02             	shr    $0x2,%ecx
  40da51:	48 09 c8             	or     %rcx,%rax
  40da54:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40da59:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40da5e:	48 83 c0 01          	add    $0x1,%rax
  40da62:	48 83 54 24 f8 00    	adcq   $0x0,-0x8(%rsp)
  40da68:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40da6d:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40da72:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40da77:	48 89 c8             	mov    %rcx,%rax
  40da7a:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40da7f:	48 c1 f9 02          	sar    $0x2,%rcx
  40da83:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40da88:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40da8d:	8a 44 24 f6          	mov    -0xa(%rsp),%al
  40da91:	24 20                	and    $0x20,%al
  40da93:	c0 e8 05             	shr    $0x5,%al
  40da96:	24 01                	and    $0x1,%al
  40da98:	3c 00                	cmp    $0x0,%al
  40da9a:	74 2a                	je     40dac6 <__floattidf+0x2b6>
  40da9c:	48 8b 54 24 f0       	mov    -0x10(%rsp),%rdx
  40daa1:	48 8b 4c 24 f8       	mov    -0x8(%rsp),%rcx
  40daa6:	48 89 c8             	mov    %rcx,%rax
  40daa9:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40daae:	48 d1 f9             	sar    $1,%rcx
  40dab1:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40dab6:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40dabb:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40dabf:	83 c0 01             	add    $0x1,%eax
  40dac2:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  40dac6:	eb 5c                	jmp    40db24 <__floattidf+0x314>
  40dac8:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  40dacc:	b9 35 00 00 00       	mov    $0x35,%ecx
  40dad1:	29 c1                	sub    %eax,%ecx
  40dad3:	83 e1 7f             	and    $0x7f,%ecx
  40dad6:	89 4c 24 8c          	mov    %ecx,-0x74(%rsp)
  40dada:	48 8b 44 24 f0       	mov    -0x10(%rsp),%rax
  40dadf:	48 8b 74 24 f8       	mov    -0x8(%rsp),%rsi
  40dae4:	40 88 cf             	mov    %cl,%dil
  40dae7:	40 88 f9             	mov    %dil,%cl
  40daea:	48 89 c2             	mov    %rax,%rdx
  40daed:	48 d3 e2             	shl    %cl,%rdx
  40daf0:	40 88 f9             	mov    %dil,%cl
  40daf3:	48 0f a5 c6          	shld   %cl,%rax,%rsi
  40daf7:	8b 4c 24 8c          	mov    -0x74(%rsp),%ecx
  40dafb:	31 c0                	xor    %eax,%eax
  40dafd:	40 f6 c7 40          	test   $0x40,%dil
  40db01:	48 0f 45 f2          	cmovne %rdx,%rsi
  40db05:	48 0f 45 d0          	cmovne %rax,%rdx
  40db09:	81 e9 80 00 00 00    	sub    $0x80,%ecx
  40db0f:	48 89 c1             	mov    %rax,%rcx
  40db12:	48 0f 42 ce          	cmovb  %rsi,%rcx
  40db16:	48 0f 42 c2          	cmovb  %rdx,%rax
  40db1a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40db1f:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40db24:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  40db2b:	00 00 
  40db2d:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  40db31:	25 00 00 00 80       	and    $0x80000000,%eax
  40db36:	8b 4c 24 cc          	mov    -0x34(%rsp),%ecx
  40db3a:	c1 e1 14             	shl    $0x14,%ecx
  40db3d:	81 c1 00 00 f0 3f    	add    $0x3ff00000,%ecx
  40db43:	09 c8                	or     %ecx,%eax
  40db45:	8b 4c 24 f4          	mov    -0xc(%rsp),%ecx
  40db49:	81 e1 ff ff 0f 00    	and    $0xfffff,%ecx
  40db4f:	09 c8                	or     %ecx,%eax
  40db51:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  40db55:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  40db59:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40db5d:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  40db63:	48 83 c4 10          	add    $0x10,%rsp
  40db67:	5b                   	pop    %rbx
  40db68:	c3                   	ret
  40db69:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

000000000040db70 <__floattidf_unsigned>:
  40db70:	53                   	push   %rbx
  40db71:	48 89 74 24 b0       	mov    %rsi,-0x50(%rsp)
  40db76:	48 89 7c 24 b8       	mov    %rdi,-0x48(%rsp)
  40db7b:	48 8b 4c 24 b0       	mov    -0x50(%rsp),%rcx
  40db80:	48 8b 44 24 b8       	mov    -0x48(%rsp),%rax
  40db85:	48 89 44 24 f0       	mov    %rax,-0x10(%rsp)
  40db8a:	48 89 4c 24 f8       	mov    %rcx,-0x8(%rsp)
  40db8f:	48 09 c8             	or     %rcx,%rax
  40db92:	0f 94 c0             	sete   %al
  40db95:	24 01                	and    $0x1,%al
  40db97:	3c 00                	cmp    $0x0,%al
  40db99:	74 05                	je     40dba0 <__floattidf_unsigned+0x30>
  40db9b:	0f 57 c0             	xorps  %xmm0,%xmm0
  40db9e:	5b                   	pop    %rbx
  40db9f:	c3                   	ret
  40dba0:	48 8b 44 24 b0       	mov    -0x50(%rsp),%rax
  40dba5:	48 8b 4c 24 b8       	mov    -0x48(%rsp),%rcx
  40dbaa:	48 89 4c 24 e0       	mov    %rcx,-0x20(%rsp)
  40dbaf:	48 89 44 24 e8       	mov    %rax,-0x18(%rsp)
  40dbb4:	48 8b 74 24 e0       	mov    -0x20(%rsp),%rsi
  40dbb9:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40dbbe:	48 0f bd c2          	bsr    %rdx,%rax
  40dbc2:	48 83 f0 3f          	xor    $0x3f,%rax
  40dbc6:	b9 7f 00 00 00       	mov    $0x7f,%ecx
  40dbcb:	48 0f bd ce          	bsr    %rsi,%rcx
  40dbcf:	48 83 f1 3f          	xor    $0x3f,%rcx
  40dbd3:	48 83 c1 40          	add    $0x40,%rcx
  40dbd7:	48 85 d2             	test   %rdx,%rdx
  40dbda:	48 0f 45 c8          	cmovne %rax,%rcx
  40dbde:	31 c0                	xor    %eax,%eax
  40dbe0:	ba 80 00 00 00       	mov    $0x80,%edx
  40dbe5:	48 29 ca             	sub    %rcx,%rdx
  40dbe8:	48 89 c1             	mov    %rax,%rcx
  40dbeb:	48 19 c9             	sbb    %rcx,%rcx
  40dbee:	48 89 54 24 d0       	mov    %rdx,-0x30(%rsp)
  40dbf3:	48 89 4c 24 d8       	mov    %rcx,-0x28(%rsp)
  40dbf8:	8b 4c 24 d0          	mov    -0x30(%rsp),%ecx
  40dbfc:	ff c9                	dec    %ecx
  40dbfe:	89 4c 24 cc          	mov    %ecx,-0x34(%rsp)
  40dc02:	48 8b 74 24 d0       	mov    -0x30(%rsp),%rsi
  40dc07:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40dc0c:	ba 35 00 00 00       	mov    $0x35,%edx
  40dc11:	48 29 f2             	sub    %rsi,%rdx
  40dc14:	48 19 c8             	sbb    %rcx,%rax
  40dc17:	0f 92 c0             	setb   %al
  40dc1a:	24 01                	and    $0x1,%al
  40dc1c:	3c 00                	cmp    $0x0,%al
  40dc1e:	0f 84 c0 01 00 00    	je     40dde4 <__floattidf_unsigned+0x274>
  40dc24:	48 8b 44 24 d0       	mov    -0x30(%rsp),%rax
  40dc29:	48 89 44 24 a0       	mov    %rax,-0x60(%rsp)
  40dc2e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40dc33:	48 89 44 24 a8       	mov    %rax,-0x58(%rsp)
  40dc38:	0f 28 44 24 d0       	movaps -0x30(%rsp),%xmm0
  40dc3d:	0f 28 0d 5c 16 00 00 	movaps 0x165c(%rip),%xmm1        # 40f2a0 <_IO_stdin_used+0x2a0>
  40dc44:	66 0f ef c1          	pxor   %xmm1,%xmm0
  40dc48:	66 0f 38 17 c0       	ptest  %xmm0,%xmm0
  40dc4d:	74 17                	je     40dc66 <__floattidf_unsigned+0xf6>
  40dc4f:	eb 00                	jmp    40dc51 <__floattidf_unsigned+0xe1>
  40dc51:	48 8b 4c 24 a8       	mov    -0x58(%rsp),%rcx
  40dc56:	48 8b 44 24 a0       	mov    -0x60(%rsp),%rax
  40dc5b:	48 83 f0 37          	xor    $0x37,%rax
  40dc5f:	48 09 c8             	or     %rcx,%rax
  40dc62:	74 26                	je     40dc8a <__floattidf_unsigned+0x11a>
  40dc64:	eb 29                	jmp    40dc8f <__floattidf_unsigned+0x11f>
  40dc66:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40dc6b:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40dc70:	48 89 d0             	mov    %rdx,%rax
  40dc73:	48 01 c0             	add    %rax,%rax
  40dc76:	48 0f a4 d1 01       	shld   $0x1,%rdx,%rcx
  40dc7b:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40dc80:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dc85:	e9 d6 00 00 00       	jmp    40dd60 <__floattidf_unsigned+0x1f0>
  40dc8a:	e9 d1 00 00 00       	jmp    40dd60 <__floattidf_unsigned+0x1f0>
  40dc8f:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40dc94:	48 8b 74 24 e8       	mov    -0x18(%rsp),%rsi
  40dc99:	48 8b 7c 24 d0       	mov    -0x30(%rsp),%rdi
  40dc9e:	48 8b 44 24 d8       	mov    -0x28(%rsp),%rax
  40dca3:	48 89 44 24 90       	mov    %rax,-0x70(%rsp)
  40dca8:	49 89 fb             	mov    %rdi,%r11
  40dcab:	49 83 c3 c9          	add    $0xffffffffffffffc9,%r11
  40dcaf:	48 83 d0 ff          	adc    $0xffffffffffffffff,%rax
  40dcb3:	44 88 db             	mov    %r11b,%bl
  40dcb6:	88 d9                	mov    %bl,%cl
  40dcb8:	49 89 f2             	mov    %rsi,%r10
  40dcbb:	49 d3 ea             	shr    %cl,%r10
  40dcbe:	88 d9                	mov    %bl,%cl
  40dcc0:	49 89 d1             	mov    %rdx,%r9
  40dcc3:	49 0f ad f1          	shrd   %cl,%rsi,%r9
  40dcc7:	48 8b 4c 24 90       	mov    -0x70(%rsp),%rcx
  40dccc:	45 31 c0             	xor    %r8d,%r8d
  40dccf:	f6 c3 40             	test   $0x40,%bl
  40dcd2:	4d 0f 45 ca          	cmovne %r10,%r9
  40dcd6:	4d 0f 45 d0          	cmovne %r8,%r10
  40dcda:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  40dce1:	48 83 d8 00          	sbb    $0x0,%rax
  40dce5:	4c 89 c0             	mov    %r8,%rax
  40dce8:	49 0f 42 c2          	cmovb  %r10,%rax
  40dcec:	48 89 44 24 98       	mov    %rax,-0x68(%rsp)
  40dcf1:	4c 89 c0             	mov    %r8,%rax
  40dcf4:	49 0f 42 c1          	cmovb  %r9,%rax
  40dcf8:	41 bb b7 00 00 00    	mov    $0xb7,%r11d
  40dcfe:	49 29 fb             	sub    %rdi,%r11
  40dd01:	4c 89 c7             	mov    %r8,%rdi
  40dd04:	48 19 cf             	sbb    %rcx,%rdi
  40dd07:	45 88 d9             	mov    %r11b,%r9b
  40dd0a:	49 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%r10
  40dd11:	44 88 c9             	mov    %r9b,%cl
  40dd14:	4c 89 d3             	mov    %r10,%rbx
  40dd17:	48 d3 eb             	shr    %cl,%rbx
  40dd1a:	48 8b 4c 24 98       	mov    -0x68(%rsp),%rcx
  40dd1f:	41 f6 c1 40          	test   $0x40,%r9b
  40dd23:	49 89 d9             	mov    %rbx,%r9
  40dd26:	4d 0f 45 c8          	cmovne %r8,%r9
  40dd2a:	4c 0f 45 d3          	cmovne %rbx,%r10
  40dd2e:	49 81 eb 80 00 00 00 	sub    $0x80,%r11
  40dd35:	48 83 df 00          	sbb    $0x0,%rdi
  40dd39:	4c 89 c7             	mov    %r8,%rdi
  40dd3c:	49 0f 42 fa          	cmovb  %r10,%rdi
  40dd40:	4d 0f 42 c1          	cmovb  %r9,%r8
  40dd44:	4c 21 c6             	and    %r8,%rsi
  40dd47:	48 21 fa             	and    %rdi,%rdx
  40dd4a:	48 09 f2             	or     %rsi,%rdx
  40dd4d:	0f 95 c2             	setne  %dl
  40dd50:	0f b6 d2             	movzbl %dl,%edx
  40dd53:	48 09 d0             	or     %rdx,%rax
  40dd56:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40dd5b:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dd60:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40dd65:	89 c1                	mov    %eax,%ecx
  40dd67:	83 e1 04             	and    $0x4,%ecx
  40dd6a:	c1 e9 02             	shr    $0x2,%ecx
  40dd6d:	48 09 c8             	or     %rcx,%rax
  40dd70:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dd75:	48 8b 44 24 e0       	mov    -0x20(%rsp),%rax
  40dd7a:	48 83 c0 01          	add    $0x1,%rax
  40dd7e:	48 83 54 24 e8 00    	adcq   $0x0,-0x18(%rsp)
  40dd84:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dd89:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40dd8e:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40dd93:	48 89 c8             	mov    %rcx,%rax
  40dd96:	48 0f a4 d0 3e       	shld   $0x3e,%rdx,%rax
  40dd9b:	48 c1 e9 02          	shr    $0x2,%rcx
  40dd9f:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40dda4:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40dda9:	8a 44 24 e6          	mov    -0x1a(%rsp),%al
  40ddad:	24 20                	and    $0x20,%al
  40ddaf:	c0 e8 05             	shr    $0x5,%al
  40ddb2:	24 01                	and    $0x1,%al
  40ddb4:	3c 00                	cmp    $0x0,%al
  40ddb6:	74 2a                	je     40dde2 <__floattidf_unsigned+0x272>
  40ddb8:	48 8b 54 24 e0       	mov    -0x20(%rsp),%rdx
  40ddbd:	48 8b 4c 24 e8       	mov    -0x18(%rsp),%rcx
  40ddc2:	48 89 c8             	mov    %rcx,%rax
  40ddc5:	48 0f a4 d0 3f       	shld   $0x3f,%rdx,%rax
  40ddca:	48 d1 e9             	shr    $1,%rcx
  40ddcd:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40ddd2:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40ddd7:	8b 44 24 cc          	mov    -0x34(%rsp),%eax
  40dddb:	83 c0 01             	add    $0x1,%eax
  40ddde:	89 44 24 cc          	mov    %eax,-0x34(%rsp)
  40dde2:	eb 6a                	jmp    40de4e <__floattidf_unsigned+0x2de>
  40dde4:	48 8b 54 24 d0       	mov    -0x30(%rsp),%rdx
  40dde9:	48 8b 4c 24 d8       	mov    -0x28(%rsp),%rcx
  40ddee:	31 c0                	xor    %eax,%eax
  40ddf0:	bf 35 00 00 00       	mov    $0x35,%edi
  40ddf5:	48 29 d7             	sub    %rdx,%rdi
  40ddf8:	48 89 44 24 88       	mov    %rax,-0x78(%rsp)
  40ddfd:	48 19 c8             	sbb    %rcx,%rax
  40de00:	4c 8b 4c 24 e0       	mov    -0x20(%rsp),%r9
  40de05:	48 8b 54 24 e8       	mov    -0x18(%rsp),%rdx
  40de0a:	41 88 f8             	mov    %dil,%r8b
  40de0d:	44 88 c1             	mov    %r8b,%cl
  40de10:	4c 89 ce             	mov    %r9,%rsi
  40de13:	48 d3 e6             	shl    %cl,%rsi
  40de16:	44 88 c1             	mov    %r8b,%cl
  40de19:	4c 0f a5 ca          	shld   %cl,%r9,%rdx
  40de1d:	48 8b 4c 24 88       	mov    -0x78(%rsp),%rcx
  40de22:	41 f6 c0 40          	test   $0x40,%r8b
  40de26:	48 0f 45 d6          	cmovne %rsi,%rdx
  40de2a:	48 0f 45 f1          	cmovne %rcx,%rsi
  40de2e:	48 81 ef 80 00 00 00 	sub    $0x80,%rdi
  40de35:	48 83 d8 00          	sbb    $0x0,%rax
  40de39:	48 89 c8             	mov    %rcx,%rax
  40de3c:	48 0f 42 c6          	cmovb  %rsi,%rax
  40de40:	48 0f 42 ca          	cmovb  %rdx,%rcx
  40de44:	48 89 4c 24 e8       	mov    %rcx,-0x18(%rsp)
  40de49:	48 89 44 24 e0       	mov    %rax,-0x20(%rsp)
  40de4e:	48 c7 44 24 c0 00 00 	movq   $0x0,-0x40(%rsp)
  40de55:	00 00 
  40de57:	8b 54 24 cc          	mov    -0x34(%rsp),%edx
  40de5b:	c1 e2 14             	shl    $0x14,%edx
  40de5e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  40de62:	25 ff ff 0f 00       	and    $0xfffff,%eax
  40de67:	89 c1                	mov    %eax,%ecx
  40de69:	89 d0                	mov    %edx,%eax
  40de6b:	8d 84 08 00 00 f0 3f 	lea    0x3ff00000(%rax,%rcx,1),%eax
  40de72:	89 44 24 c4          	mov    %eax,-0x3c(%rsp)
  40de76:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  40de7a:	89 44 24 c0          	mov    %eax,-0x40(%rsp)
  40de7e:	f2 0f 10 44 24 c0    	movsd  -0x40(%rsp),%xmm0
  40de84:	5b                   	pop    %rbx
  40de85:	c3                   	ret
  40de86:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
  40de8d:	00 00 00 

000000000040de90 <__umodti3>:
  40de90:	48 83 ec 58          	sub    $0x58,%rsp
  40de94:	48 89 0c 24          	mov    %rcx,(%rsp)
  40de98:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40de9d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40dea2:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40dea7:	48 8b 0c 24          	mov    (%rsp),%rcx
  40deab:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40deb0:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40deb5:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40deba:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40debf:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40dec4:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40dec9:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40dece:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  40ded3:	e8 b8 7b ff ff       	call   405a90 <runtime::udivmod128>
  40ded8:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40dedd:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40dee2:	48 83 c4 58          	add    $0x58,%rsp
  40dee6:	c3                   	ret
  40dee7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40deee:	00 00 

000000000040def0 <__udivmodti4>:
  40def0:	48 83 ec 58          	sub    $0x58,%rsp
  40def4:	4c 89 04 24          	mov    %r8,(%rsp)
  40def8:	48 89 4c 24 08       	mov    %rcx,0x8(%rsp)
  40defd:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
  40df02:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
  40df07:	48 89 7c 24 20       	mov    %rdi,0x20(%rsp)
  40df0c:	4c 8b 04 24          	mov    (%rsp),%r8
  40df10:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40df15:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40df1a:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40df1f:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
  40df24:	48 89 7c 24 40       	mov    %rdi,0x40(%rsp)
  40df29:	48 89 74 24 48       	mov    %rsi,0x48(%rsp)
  40df2e:	48 89 4c 24 38       	mov    %rcx,0x38(%rsp)
  40df33:	48 89 54 24 30       	mov    %rdx,0x30(%rsp)
  40df38:	4c 89 44 24 28       	mov    %r8,0x28(%rsp)
  40df3d:	e8 4e 7b ff ff       	call   405a90 <runtime::udivmod128>
  40df42:	48 83 c4 58          	add    $0x58,%rsp
  40df46:	c3                   	ret
  40df47:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40df4e:	00 00 

000000000040df50 <__udivti3>:
  40df50:	48 83 ec 48          	sub    $0x48,%rsp
  40df54:	48 89 0c 24          	mov    %rcx,(%rsp)
  40df58:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40df5d:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40df62:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40df67:	48 8b 0c 24          	mov    (%rsp),%rcx
  40df6b:	48 8b 54 24 08       	mov    0x8(%rsp),%rdx
  40df70:	48 8b 74 24 10       	mov    0x10(%rsp),%rsi
  40df75:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
  40df7a:	48 89 7c 24 30       	mov    %rdi,0x30(%rsp)
  40df7f:	48 89 74 24 38       	mov    %rsi,0x38(%rsp)
  40df84:	48 89 4c 24 28       	mov    %rcx,0x28(%rsp)
  40df89:	48 89 54 24 20       	mov    %rdx,0x20(%rsp)
  40df8e:	31 c0                	xor    %eax,%eax
  40df90:	41 89 c0             	mov    %eax,%r8d
  40df93:	e8 58 ff ff ff       	call   40def0 <__udivmodti4>
  40df98:	48 83 c4 48          	add    $0x48,%rsp
  40df9c:	c3                   	ret
  40df9d:	0f 1f 00             	nopl   (%rax)

000000000040dfa0 <__modti3>:
  40dfa0:	48 81 ec 98 00 00 00 	sub    $0x98,%rsp
  40dfa7:	48 89 0c 24          	mov    %rcx,(%rsp)
  40dfab:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
  40dfb0:	48 89 74 24 10       	mov    %rsi,0x10(%rsp)
  40dfb5:	48 89 7c 24 18       	mov    %rdi,0x18(%rsp)
  40dfba:	48 8b 4c 24 08       	mov    0x8(%rsp),%rcx
  40dfbf:	48 8b 04 24          	mov    (%rsp),%rax
  40dfc3:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
  40dfc8:	48 8b 54 24 10       	mov    0x10(%rsp),%rdx
  40dfcd:	48 89 b4 24 80 00 00 	mov    %rsi,0x80(%rsp)
  40dfd4:	00 
  40dfd5:	48 89 94 24 88 00 00 	mov    %rdx,0x88(%rsp)
  40dfdc:	00 
  40dfdd:	48 89 44 24 78       	mov    %rax,0x78(%rsp)
  40dfe2:	48 89 4c 24 70       	mov    %rcx,0x70(%rsp)
  40dfe7:	48 89 d7             	mov    %rdx,%rdi
  40dfea:	48 c1 ff 3f          	sar    $0x3f,%rdi
  40dfee:	48 89 7c 24 68       	mov    %rdi,0x68(%rsp)
  40dff3:	48 89 7c 24 60       	mov    %rdi,0x60(%rsp)
  40dff8:	48 89 c7             	mov    %rax,%rdi
  40dffb:	48 c1 ff 3f          	sar    $0x3f,%rdi
  40dfff:	48 89 7c 24 58       	mov    %rdi,0x58(%rsp)
  40e004:	48 89 7c 24 50       	mov    %rdi,0x50(%rsp)
  40e009:	4c 8b 44 24 60       	mov    0x60(%rsp),%r8
  40e00e:	48 8b 7c 24 68       	mov    0x68(%rsp),%rdi
  40e013:	48 31 fa             	xor    %rdi,%rdx
  40e016:	4c 31 c6             	xor    %r8,%rsi
  40e019:	4c 29 c6             	sub    %r8,%rsi
  40e01c:	48 19 fa             	sbb    %rdi,%rdx
  40e01f:	48 89 74 24 40       	mov    %rsi,0x40(%rsp)
  40e024:	48 89 54 24 48       	mov    %rdx,0x48(%rsp)
  40e029:	48 8b 74 24 50       	mov    0x50(%rsp),%rsi
  40e02e:	48 8b 54 24 58       	mov    0x58(%rsp),%rdx
  40e033:	48 31 d0             	xor    %rdx,%rax
  40e036:	48 31 f1             	xor    %rsi,%rcx
  40e039:	48 29 f1             	sub    %rsi,%rcx
  40e03c:	48 19 d0             	sbb    %rdx,%rax
  40e03f:	48 89 4c 24 30       	mov    %rcx,0x30(%rsp)
  40e044:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
  40e049:	48 8b 7c 24 40       	mov    0x40(%rsp),%rdi
  40e04e:	48 8b 74 24 48       	mov    0x48(%rsp),%rsi
  40e053:	48 8b 54 24 30       	mov    0x30(%rsp),%rdx
  40e058:	48 8b 4c 24 38       	mov    0x38(%rsp),%rcx
  40e05d:	4c 8d 44 24 20       	lea    0x20(%rsp),%r8
  40e062:	e8 29 7a ff ff       	call   405a90 <runtime::udivmod128>
  40e067:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
  40e06c:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
  40e071:	48 8b 74 24 60       	mov    0x60(%rsp),%rsi
  40e076:	48 8b 4c 24 68       	mov    0x68(%rsp),%rcx
  40e07b:	48 31 ca             	xor    %rcx,%rdx
  40e07e:	48 31 f0             	xor    %rsi,%rax
  40e081:	48 29 f0             	sub    %rsi,%rax
  40e084:	48 19 ca             	sbb    %rcx,%rdx
  40e087:	48 81 c4 98 00 00 00 	add    $0x98,%rsp
  40e08e:	c3                   	ret

Disassembly of section .fini:

000000000040e090 <_fini>:
  40e090:	f3 0f 1e fa          	endbr64
  40e094:	48 83 ec 08          	sub    $0x8,%rsp
  40e098:	48 83 c4 08          	add    $0x8,%rsp
  40e09c:	c3                   	ret
