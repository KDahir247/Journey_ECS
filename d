Field "disasm" requires perf to be built with libcapstone support.

 Usage: perf script [<options>]
    or: perf script [<options>] record <script> [<record-options>] <command>
    or: perf script [<options>] report <script> [script-args]
    or: perf script [<options>] <script> [<record-options>] <command>
    or: perf script [<options>] <top-script> [script-args]

    -F, --fields <str>    comma separated output fields prepend with 'type:'. +field to add and -field to remove.Valid types: hw,sw,trace,raw,synth. Fields: comm,tid,pid,time,cpu,event,trace,ip,sym,dso,dsoff,addr,symoff,srcline,period,iregs,uregs,brstack,brstacksym,flags,data_src,weight,bpf-output,brstackinsn,brstackinsnlen,brstackdisasm,brstackoff,callindent,insn,disasm,insnlen,synth,phys_addr,metric,misc,srccode,ipc,tod,data_page_size,code_page_size,ins_lat,machine_pid,vcpu,cgroup,retire_lat,brcntr
