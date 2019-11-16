function ctrl_c() {
	killall afplay || echo ""
}
trap ctrl_c INT
(afplay audio/s1_70pc_compress1.mp3 || echo "") & ./main.byte
