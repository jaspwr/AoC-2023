global main

extern print
extern load_file
extern print_divider

section .text

main:
  call load_file
  call solve

  mov rax, 60
  mov rdi, 0
  syscall

solve:
  push rbp
  mov rbp, rsp
  sub rsp, 8000

  ; string pointer
  mov qword [rbp - 8], rax

  ; seeds list length
  mov qword [rbp - 16], 0
  ; seeds list capacity is 30 * 8


__seedLoop:
  ; check for newline otherwise continue loop
  mov rax, qword [rbp - 8]
  cmp byte [rax], 10
  je __seedBreak

  ; space skip
  mov rax, qword [rbp - 8]
  call skipNonDigit
  mov qword [rbp - 8], rax

  ; get next number
  mov rax, qword [rbp - 8]
  call parseNum

  ; append to list
  mov rdx, qword [rbp - 16]
  add rdx, 1
  mov qword [rbp - 16], rdx

  sub rdx, 1
  imul rdx, 8
  lea rcx, qword [rbp - 24]
  sub rcx, rdx

  mov qword [rcx], rax

  ; add num length to pointer
  mov rcx, qword [rbp - 8]
  add rcx, rbx
  mov qword [rbp - 8], rcx

  jmp __seedLoop
__seedBreak:

  ; seed counter
  mov qword [rbp - 272], 0

  ; string ptr at strat of transformations store
  mov rax, qword [rbp - 8]
  mov qword [rbp - 280], rax

__startTransformations:

  call print_divider

  ; current seed pointer
  lea rax, qword [rbp - 24]
  mov rbx, qword [rbp - 272]
  imul rbx, 8
  sub rax, rbx
  mov qword [rbp - 264], rax

__transformationLoop:

  ; desination base
  mov qword [rbp - 240], 0
  ; source base
  mov qword [rbp - 248], 0
  ; range
  mov qword [rbp - 256], 0

  mov qword [rbp - 300], 0

  ; parse origin base
  mov rax, qword [rbp - 8]
  call skipNonDigit

  cmp byte [rax], 0
  je __breakTransformationLoop

  mov qword [rbp - 8], rax
  mov qword [rbp - 300], rax

  call parseNum
  mov rcx, qword [rbp - 300]
  add rcx, rbx
  mov qword [rbp - 8], rcx
  mov qword [rbp - 240], rax

  ; parse desination base
  mov rax, qword [rbp - 8]
  call skipNonDigit
  mov qword [rbp - 8], rax
  mov qword [rbp - 300], rax

  call parseNum
  mov rcx, qword [rbp - 300]
  add rcx, rbx
  mov qword [rbp - 8], rcx
  mov qword [rbp - 248], rax

  ; parse range
  mov rax, qword [rbp - 8]
  call skipNonDigit
  mov qword [rbp - 8], rax
  mov qword [rbp - 300], rax

  call parseNum
  mov rcx, qword [rbp - 300]
  add rcx, rbx
  mov qword [rbp - 8], rcx
  mov qword [rbp - 256], rax

  ; mov rdi, qword [rbp - 240]
  ; call print
  ; mov rdi, qword [rbp - 248]
  ; call print
  ; mov rdi, qword [rbp - 256]
  ; call print

  ; apply transformations to current seed
  mov rdx, qword [rbp - 264]
  mov rax, qword [rdx]
  mov rdi, rax
  call print

  ; load current seed
  mov rdx, qword [rbp - 264]
  mov rax, qword [rdx]

  ; check if seed is in source range
  cmp rax, qword [rbp - 248]
  jl __d

  mov rbx, qword [rbp - 256]
  add rbx, qword [rbp - 248]
  sub rbx, 1
  cmp rax, rbx;
  jg __d

  ; if didn't jump to __d, then we apply the transformation

  ; get difference
  mov rbx, qword [rbp - 240]
  sub rbx, qword [rbp - 248]

  add rax, rbx

  ; write new seed
  mov qword [rdx], rax

  ; a transformation was applied and therefore we need to move
  ; down to the next section

  mov rax, qword [rbp - 8]
  call findEmptyLineOrEOF
  mov qword [rbp - 8], rax


__d:

  jmp __transformationLoop
__breakTransformationLoop:

  ; move string ptr back to start of transformations
  mov rax, qword [rbp - 280]
  mov qword [rbp - 8], rax

  ; compare counter with seeds length
  mov rax, qword [rbp - 272]
  add rax, 1
  mov qword [rbp - 272], rax
  cmp rax, qword [rbp - 16]
  jne __startTransformations


  call print_divider

  ; smallest
  mov qword [rbp - 288], 9999999999999

__printLoop:
  mov rax, qword [rbp - 16]
  sub rax, 1
  mov qword [rbp - 16], rax

  cmp rax, -1
  je __printBreak

  imul rax, 8
  lea rbx, qword [rbp - 24]
  sub rbx, rax

  ; see if smaller and replace
  mov rax, qword [rbx]
  cmp rax, qword [rbp - 288]
  jg __g
  mov qword [rbp - 288], rax
__g:

  mov rdi, qword [rbx]
  call print

  jmp __printLoop
__printBreak:


  call print_divider

  mov rdi, qword [rbp - 288]
  call print

  add rsp, 8000
  pop rbp
  ret


skipNonDigit:
  ; takes string pointer rax and increments it until not pointing to whitespace
__nonDigSkipLoop:
  cmp byte [rax], 0
  je __nonDigSkipBreak
  cmp byte [rax], 48
  jl __a
  cmp byte [rax], 57
  jg __a
  jmp __nonDigSkipBreak
__a:

  add rax, 1

  jmp __nonDigSkipLoop
__nonDigSkipBreak:
  ret

findEmptyLineOrEOF:
  ; takes string pointer rax and increments until empty line or EOF
  __emptyLineLoop:
  cmp byte [rax], 0
  je __empytLineBreak
  cmp byte [rax], ':'
  je __empytLineBreak

  add rax, 1

  jmp __emptyLineLoop
  __empytLineBreak:
  ret

parseNum:
  ; rax must be set to a pointer of a number stirng
  ; when returned, rax will contain the number as an int and rbx will contain the length

  push rbp
  mov rbp, rsp
  sub rsp, 24

  ; string pointer
  mov qword [rbp - 8], rax

  ; num buffer
  mov qword [rbp - 16], 0

  ; length
  mov qword [rbp - 24], 0

__parseLoop:

  ; see if string pointer pointing to a digit.
  mov rax, qword [rbp - 8]
  cmp byte [rax], 48
  jl __parseBreak
  cmp byte [rax], 57
  jg __parseBreak

  ; load num buffer and * 10
  mov rax, qword [rbp - 16]
  imul rax, 10

  ; add next number in string
  mov rcx, qword [rbp - 8]
  movzx rbx, byte [rcx]
  sub rbx, 48
  add rax, rbx

  ; store num again
  mov qword [rbp - 16], rax

  ; increment length
  add qword [rbp - 24], 1

  ; increment string pointer
  mov rax, qword [rbp - 8]
  add rax, 1
  mov qword [rbp - 8], rax

  jmp __parseLoop
__parseBreak:

  ; load return values
  mov rax, qword [rbp - 16]
  mov rbx, qword [rbp - 24]

  add rsp, 24
  pop rbp
  ret

section .rodata
  teststr: db "1 5 15", 10, 0
