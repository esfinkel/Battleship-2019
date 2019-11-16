function ctrl_c() {
	killall afplay || echo ""
}
trap ctrl_c INT
(afplay audio/s1_70pc_compress2_30mins.mp3 || echo "") & ./main.byte
