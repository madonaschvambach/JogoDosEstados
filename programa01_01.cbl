      $set sourceformat"free"

      *>--- divisão de identificação do programa
       identification division.

      *>--- nome do programa
       program-id. "programa01_01".

      *>--- nome do autor
       author. "Madona Schvambach".
       installation. "PC".

      *>--- data que o programa foi escrito
       date-written.   25/07/2020.
       date-compiled.  29/07/2020.

      *>--- divisao para configuração do programa
       environment division.
       configuration section.

      *>--- declarado que será utilizado vírgulo ao invés de ponto
           special-names. decimal-point is comma.

      *>--- declaracao de recursos eternos
       input-output Section.
       file-control.


      *>   nome lógico e e arquivo de memoria
           select arqEstados assign to "estados.txt"
      *>   tipo de arquivo (sequencial)
           organization    is line sequential
      *>   modo de acesso ao arquivo (sequencial)
           access mode     is sequential
      *>   evita perda de dados em ambientes multi-usuarios(varios usuarios entrando com dados ao mesmo tempo)
           lock mode is automatic
      *>   variavel "ws-fs-arqAlunos" retona o status do arquivo (0, 35....)
           file status     is  ws-fs-arqEstados.



       i-o-control.


      *>--- declaracao de variaveis
       data division.

      *>--- variáveis de arquivos
       file section.


       fd  arqEstados.
       01  fs-estados-capitais occurs 27.
           05  fs-estado                           pic x(25).
           05  fs-capital                          pic x(25).
           05  fs-repeticao                        pic 9(01) value 0.



      *>--- variavéis de trabalho
       working-storage section.


       77  ws-fs-arqEstados                        pic 9(02).


       01  ws-estados-capitais occurs 27.
           05  ws-estado                           pic x(25).
           05  ws-capital                          pic x(25).
           05  ws-repeticao                        pic 9(01).


       01  jogadores occurs 04.
           05 nome                                 pic x(10).
           05 pontuacao                            pic 9(02) value 0.
           05 escolha                              pic x(20) value "a".


       01  ws-msn-erro.
           05 ws-msn-erro-ofsset                   pic 9(04).
           05 filler                               pic x(01) value "-".
           05 ws-msn-erro-cod                      pic 9(02).
           05 filler                               pic x(01) value space.
           05 ws-msn-erro-text                     pic x(42).


       77  ws-escolha-fechar-prog                  pic x(01).
           88  ws-fechar-programa                  value "S".
           88  ws-nao-fechar-programa              value "N".


       77  ws-aux-erase                            pic 9(02) value 0.
       77  ws-ind-est                           pic 9(02) value 0.
       77  ws-quant-jogadores                      pic 9(01) value 4.
       77  ws-funcionamento-jogo                   pic 9(01) value 3.
       77  ws-quant-estados                        pic 9(02) value 27.
       77  ws-aux-random                           pic 9(02) value 27.
       77  ws-i                                    pic 9(02).
       77  ws-num-random                           pic 9(02) value 0.
       77  ws-controle                             pic a(10).
       77  ws-aux-pontuacao                        pic 9(02).
       77  ws-aux-nome                             pic x(10).
       77  ws-auxiliar                             pic 9(02).


      *>--- variaveis para comunicaçao entre programa
       linkage section.



      *>--- declaração de tela
       screen section.



      *>--- declaração do corpo do programa
       procedure division.


           perform inicializacao.
           perform processamento.
           perform finalizacao.


      *>-----------------------------------------------------------------
      *>                   inicializacao do programa
      *>-----------------------------------------------------------------
       inicializacao section.


           open input arqEstados

           if ws-fs-arqEstados <> 0 then
               move 1                                to ws-msn-erro-ofsset
               move ws-fs-arqEstados                 to ws-msn-erro-cod
               move "Erro ao abrir arq. arqEstados " to ws-msn-erro-text
               perform finaliza-anormal

           else
               *>mover o que está no arquivo para as variaveis de trabalho
               perform varying ws-ind-est from 1 by 1 until ws-fs-arqEstados = 10
                                                       or ws-ind-est > 27


                   read arqEstados into ws-estados-capitais(ws-ind-est)
                   if ws-fs-arqEstados <> 0 and ws-fs-arqEstados <> 10 then
                       move 2                                  to ws-msn-erro-ofsset
                       move ws-fs-arqEstados                   to ws-msn-erro-cod
                       move "Erro ao ler arq. arqEstados "     to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if

               end-perform

           end-if


           .
       inicializacao-exit.
           exit.


      *>-----------------------------------------------------------------
      *>                   processamento do programa
      *>-----------------------------------------------------------------
       processamento section.

           perform until ws-fechar-programa

               perform cad-nomes-jogadores

               *>rodadas do jogo
               perform ws-funcionamento-jogo times

                   perform gerar-random
                   perform entrada_capital

               end-perform
               display erase

               perform conferir-pontuacao

               display " "
               display "   Deseja fechar o programa?  'S'/'N'"
               accept  ws-escolha-fechar-prog
               display erase


           end-perform


           .
       processamento-exit.
           exit.


      *>-----------------------------------------------------------------
      *>                 blocos de comando do programa
      *>-----------------------------------------------------------------



      *>-----------------------------------------------------------------
      *> Cadastro dos jogadores
      *>-----------------------------------------------------------------
       cad-nomes-jogadores section.


           move 1 to ws-i

           *>-- ler o nome dos jogadores
           perform ws-quant-jogadores times
               display "Nome do " ws-i " jogador:"
               accept nome(ws-i)
               display " "
               add 1 to ws-i
           end-perform


           .
       cad-nomes-jogadores-exit.
           exit.


      *>-----------------------------------------------------------------
      *> Gerar Random
      *>-----------------------------------------------------------------
       gerar-random section.


           compute ws-num-random = function random (1) * ws-quant-estados + 1

           perform conf-repeticao-random


           .
       gerar-random-exit.
           exit.


      *>-----------------------------------------------------------------
      *> Conferir Repeticao Num Sorteado
      *>-----------------------------------------------------------------
       conf-repeticao-random section.


           if ws-repeticao(ws-num-random) = 0 then *>quando for igual a 0, significa que aquele numero do random ainda não foi sortedo, entao ele recebe 1 para não ser sorteado dnv e repetir o estado

               move 1 to ws-repeticao(ws-num-random)

           else

               perform until ws-repeticao(ws-num-random) <> 1 *>ficara sorteando um novo numero ate que ele seja diferente de 1, logo, ainda não foi sorteado
                   subtract 1 from ws-aux-random
                   compute ws-num-random = function random (1) * ws-aux-random + 1
               end-perform

               move 1 to ws-repeticao(ws-num-random) *>o novo num sorteado, recebe 1 tbm, para n se repetir

           end-if


           .
       conf-repeticao-random-exit.
           exit.


      *>-----------------------------------------------------------------
      *> Entrada Das Capitais
      *>-----------------------------------------------------------------
       entrada_capital section.


           display erase
           display "Qual a capital de " ws-estado(ws-num-random) "?"
           display " "
           display "   -- NAO ESQUECA: inicial com letra Maiuscula --"
           display " "

           *>entrada da capital pelo usuario
           move 1 to ws-i
           perform ws-quant-jogadores times
               display "Vez do(a): " nome (ws-i)
               accept escolha(ws-i)
               display " "

               *>conferir se a entrada é igual a capital
               if escolha(ws-i) = ws-capital(ws-num-random) then
                   add 1 to pontuacao(ws-i)
               end-if

               add 1 to ws-i

           end-perform

           display "A capital de " ws-estado(ws-num-random) "eh " ws-capital(ws-num-random)
           accept ws-aux-erase
           display erase


           .
       entrada_capital-exit.
           exit.


      *>-----------------------------------------------------------------
      *> Conferir Pontuação
      *>-----------------------------------------------------------------
       conferir-pontuacao section.


       *>  colocar na ordem crescente
           move  0 to ws-i
           move "trocou" to ws-controle
           perform until ws-controle <> "trocou"

               move 1 to ws-i
               move "Ntrocou" to ws-controle

               perform until ws-i = ws-quant-jogadores

                   if pontuacao(ws-i) > pontuacao(ws-i + 1) then

                       move nome(ws-i + 1)         to ws-aux-nome
                       move nome(ws-i)             to nome(ws-i + 1)
                       move ws-aux-nome            to nome(ws-i)

                       move pontuacao(ws-i + 1)    to ws-aux-pontuacao
                       move pontuacao(ws-i)        to pontuacao(ws-i + 1)
                       move ws-aux-pontuacao       to pontuacao(ws-i)

                       move "trocou" to ws-controle

                   end-if

                   add 1 to ws-i

               end-perform

           end-perform

           move ws-quant-jogadores to ws-i
           move ws-quant-jogadores to ws-auxiliar

           *>saida do nome do jogador que está na ultima posicao na ordem crescente (0,0,2,4), ou seja, tem a maior pontuacao'4'(vencedor)
           perform until ws-auxiliar <> ws-i

               display " "
               display " "
               display "------------------ CAMPEOES -------------------"
               display " "
               if pontuacao(ws-i) <> 0 then

                   display "O(a) " nome(ws-auxiliar) " eh o campeao(a) com " pontuacao(ws-auxiliar) " pontos."

               end-if

               subtract 1 from ws-i

           end-perform

          *>confere se tem empate de pontos
           perform until ws-i = 0

               if pontuacao(ws-auxiliar) = pontuacao(ws-i) and pontuacao(ws-i) <> 0 then

                   display "O(a) " nome(ws-i) " eh o campeao(a) com "
                   pontuacao(ws-i) " pontos."
                   subtract 1 from ws-i

               else
                   subtract 1 from ws-i

               end-if

           end-perform


           display " "
           display " "
           display "----------- PONTUACAO DOS JOGADORES -----------"

           *>--- mostrar na ordem decrescente
           *>subtract 1 from auxiliar
           move ws-quant-jogadores to ws-i
           display " "
           perform ws-quant-jogadores times
               display "Nome: " nome(ws-i) "  " "Pontuacao: " pontuacao(ws-i)
               subtract 1 from ws-i
           end-perform

           *>zerar o valor da pontuação dos usuarios
           perform zerar-tudo


           .
       conferir-pontuacao-exit.
           exit.


      *>-----------------------------------------------------------------
      *> Zerar Pontuação Dos Usuarios
      *>-----------------------------------------------------------------
       zerar-tudo section.


           move 1 to ws-i
           perform ws-quant-jogadores times
               move 0 to pontuacao(ws-i)
               add 1 to ws-i
           end-perform


           .
       zerar-tudo-exit.
           exit.


      *>-----------------------------------------------------------------
      *> Cadastro dos jogadores
      *>-----------------------------------------------------------------
       finaliza-anormal section.

           display erase
           display ws-msn-erro.
           Stop run


           .
       finaliza-anormal-exit.
           exit.


      *>-----------------------------------------------------------------
      *>                   finalizacao do programa
      *>-----------------------------------------------------------------
       finalizacao section.

           close arqEstados
           if ws-fs-arqEstados <> 0 then
               move 4                                      to ws-msn-erro-ofsset
               move ws-fs-arqEstados                       to ws-msn-erro-cod
               move "Erro ao fechar arq. arqEstados "      to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           display erase
           display "            --FIM--"
           stop run


           .
       finalizacao-exit.
           exit.

