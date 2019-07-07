vars
    p1 p2 p3 p4 p5 q1 q2 q3 q4 q5 q6 q7 q8 xt xf yt yf

rules
    p1 >= 1 ->
	       p1' = p1-1,
	       p2' = p2+1;

    p2 >= 1,
    xf >= 1 ->
	       p2' = p2-1,
	       p3' = p3+1,
               xf' = xf-1,
               xt' = xt+1;

    p3 >= 1,
    yt >= 1 ->
	       p3' = p3+0;

    p3 >= 1,
    yf >= 1 ->
	       p3' = p3-1,
	       p4' = p4+1;

    p4 >= 1 ->
	       p4' = p4-1,
	       p5' = p5+1;

    p5 >= 1,
    xt >= 1 ->
	       p5' = p5-1,
	       p1' = p1+1,
               xt' = xt-1,
               xf' = xf+1;

    q1 >= 1 ->
	       q1' = q1-1,
	       q2' = q2+1;

    q2 >= 1,
    yf >= 1 ->
	       q2' = q2-1,
	       q3' = q3+1,
               yf' = yf-1,
               yt' = yt+1;

    q3 >= 1,
    xt >= 1 ->
	       q3' = q3-1,
               q4' = q4+1;

    q3 >= 1,
    xf >= 1 ->
	       q3' = q3-1,
               q7' = q7+1;

    q4 >= 1,
    yt >= 1 ->
	       q4' = q4-1,
               q5' = q5+1,
               yt' = yt-1,
               yf' = yf+1;

    q5 >= 1,
    xt >= 1 ->
	       q5' = q5+0;

    q5 >= 1,
    xf >= 1 ->
	       q5' = q5-1,
               q6' = q6+1;

    q6 >= 1 ->
	       q6' = q6-1,
               q2' = q2+1;

    q7 >= 1 ->
	       q7' = q7-1,
               q8' = q8+1;

    q8 >= 1,
    yt >= 1 ->
	       q8' = q8-1,
               q1' = q1+1,
               yt' = yt-1,
               yf' = yf+1;

init
    p1=1, p2=0, p3=0, p4=0, p5=0, q1=1, q2=0, q3=0, q4=0, q5=0, q6=0, q7=0, q8=0, xt=0, xf=1, yt=0, yf=1

target
    p4 >= 1, q7 >= 1
