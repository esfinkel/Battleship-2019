printf "\n\nwithout test\n\n"
scc ai_normal.ml ai_random.ml ai_smart.ml board.ml command.ml helpers.ml main.ml
printf "\n\n\nwith test\n\n"
scc ai_normal.ml ai_random.ml ai_smart.ml board.ml command.ml helpers.ml main.ml test.ml
