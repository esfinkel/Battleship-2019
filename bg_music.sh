function ctrl_c() {
	killall afplay || echo ""
}
trap ctrl_c INT
(afplay audio/s1_70pc.mp3 || echo "") & ./main.byte
