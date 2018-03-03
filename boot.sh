pulseaudio -k
scsynth -u 57110 &
sleep 5
jack_connect SuperCollider:out_1 system:playback_1
jack_connect SuperCollider:out_2 system:playback_2
