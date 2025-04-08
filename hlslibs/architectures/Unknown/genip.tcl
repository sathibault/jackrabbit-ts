namespace eval unknown {
    proc findDepth {width_a width_b} {
	set width $width_a
	if {$width_b > $width} {
	    set width $width_b
	}
	set words [expr $width / 16]
	return [expr $words * $words]
    }
    
    proc multTiming {opName field paramList} {
	array set param $paramList

	switch $field {
	    rate {
		if {$param(pipeline)} {
		    return 1
		} else {
		    return 0
		}
	    }
	    cycles {
		if {$param(pipeline)} {
		    return [findDepth $param(width_a) $param(width_b)]
		} else {
		    return 0
		}
	    }
	}
	error "Unknown timing field $field"
    }
    
    proc genMult {fd lang compName paramList} {
	array set param $paramList

	set outWidth [expr $param(width_a) + $param(width_b)]
	hdl::definePort "ce" 0
	hdl::definePort "a" $param(width_a)
	hdl::definePort "b" $param(width_b)
	hdl::definePort "sum" $outWidth

	if {$param(pipeline)} {
	    hdl::defineAttribute "MULT_STYLE" $compName "entity" "pipe_block"
	} elseif {$param(dsp)} {
	    hdl::defineAttribute "MULT_STYLE" $compName "entity" "block"
	} elseif {$param(lut)} {
	    hdl::defineAttribute "MULT_STYLE" $compName "entity" "lut"
	}

	if {$param(pipeline)} {
	    set depth [findDepth $param(width_a) $param(width_b)]
	    for {set i 0} {$i <= $depth} {incr i} {
		hdl::defineReg "s$i" $outWidth "" "ce"
	    }
	    if {$param(signed)} {
		hdl::assign "s0" [hdl::muls [hdl::val "a"] [hdl::val "b"]]
	    } else {
		hdl::assign "s0" [hdl::mulu [hdl::val "a"] [hdl::val "b"]]
	    }
	    for {set i 1} {$i <= $depth} {incr i} {
		hdl::assign "s$i" [hdl::val "s[expr $i - 1]"]
	    }
	    hdl::assign "sum" [hdl::val "s$depth"]
	} else {
	    if {$param(signed)} {
		hdl::assign "sum" [hdl::muls [hdl::val "a"] [hdl::val "b"]]
	    } else {
		hdl::assign "sum" [hdl::mulu [hdl::val "a"] [hdl::val "b"]]
	    }
	}

	hdl::gen $fd $compName $lang
    }
}
