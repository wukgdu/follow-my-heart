class varRange {
    has $.lbound is rw;
    has $.ubound is rw;
    has Bool $.lclose is rw = True;
    has Bool $.uclose is rw = True;
    has Bool $.isint is rw = True;
    has Bool $.init_p is rw = False;
    has $.except is rw;
    method Str() {
        if $!init_p {
            if $!isint {
                my $l = $!lbound;
                $l = $l.round if (not ninf_p($l));
                my $u = $!ubound;
                $u = $u.round if (not inf_p($u));
                return "[$l, $u]";
            } else {
                return "[$!lbound, $!ubound]";
            }
        } else {
            "[,]"
        }
    }
}

sub copy_range($newrange, $oldrange) {
    $newrange.lbound = $oldrange.lbound;
    $newrange.ubound = $oldrange.ubound;
    $newrange.lclose = $oldrange.lclose;
    $newrange.uclose = $oldrange.uclose;
    #$newrange.isint = $oldrange.isint;
    $newrange.except = $oldrange.except;
    $newrange.init_p = $oldrange.init_p;
}

class Assign {
    has $.target is rw;
    has $.op is rw; # 为空，说明只是j=1, j=k
    has $.leftvar is rw;
    has $.rightvar is rw;
    has $.lefttype is rw;
    has $.righttype is rw;
    method Str() {
        my $str = "$!target = $!leftvar";
        if ($!op) {
            $str ~= " $!op $!rightvar";
        }
        return $str;
    }
}

class Branch {
    has $.op is rw;
    has $.left is rw;
    has $.right is rw;
    # "< k 100"
    method Str() {
        my $str  = "$!op";
        if ($!op ne "true") {
            $str ~= " $!left $!right"
        }
        return $str;
    }
}

class CallExp {
    has $.funcname is rw;
    has @.paras is rw;
    method Str() {
        "$!funcname @!paras[]";
    }
}

class BBlock {
    has $.numid is rw;
    has Assign @.assigns is rw;
    has $.branch is rw;
    has $.tbblock is rw; # branch为真，跳到的blockid
    has $.fbblock is rw;
    has %.var2range is rw;
    has $.isend is rw = False;
    has $.locked is rw = False;
    method gist() {
        my $str = "block $!numid\n";
        $str ~= @!assigns.map(*.Str).join("\n") if @!assigns;
        if ($!branch) {
            $str ~= "\n$!branch goto $!tbblock ";
            if ($!branch.op ne 'true') {
                $str ~= "goto $!fbblock";
            }
        }
        return $str;
    }
}

class Function {
    has Str $.name is rw;
    has %.vars is rw;
    has @.declarevars is rw;
    has BBlock @.bblocks is rw;
    has $.ret_value is rw;
    has %.id2block is rw;
    has @.speciallAssign is rw;
    has @.runStack is rw;
    has $.keeptime is rw = 0;
    method Str() {
        my $str = "function: $!name\n" ~ @.bblocks.map(*.gist).join("\n");
        $str ~= "\nreturn $!ret_value\n" if $!ret_value;
        return $str;
    }
}

class SSA {
    has Function %.name2func is rw;
    method gist() {
        %.name2func.values.map(*.Str).join("\n");
    }
}

my Function $curFunc;
my $has_return_stmt = False;
my BBlock $curBlock;
my SSA $curSSA .= new;

grammar Flow {
    token TOP {
        <.ws>
        <Function>+
        <.ws>
    }
    token Func_header {
        ';; Function ' <Func_name> <-[\n]>* <.ws>
    }
    token Func_name {
        \w+
    }
    token Func_declare {
        <Func_name> ' (' [<type> ' ' <Variable> [', ' <type> ' ' <Variable>]*]? ')' <.ws>
    }

    token ltype {
        || 'int'
        || 'float'
    }
    token rtype {
        || 'int'
        || 'float'
    }
    token type {
        || 'int'
        || 'float'
    }

    token Function {
        <Func_header>
        <Func_declare>
        '{' <.ws>
        <Declare>*
        <Baseblock>+
        '}' <.ws>
    }
    token Declare {
        <type> <.ws> <Variable> <.ws> ';' <.ws>
    }
    token Variable {
        <[_a..zA..Z]><[\d\.\w]>*['('<-[)]>*')']*
    }
    token Number {
        '-'?\d+['.'\d+]?['e' ['+' || '-'] \d+]?
    }

    token Block_start {
        '<bb ' <Number> '>:'
    }
    token Block_header {
        '<bb ' <Number> '>'
    }

    token Baseblock {
        <Block_start> <.ws>
        <Stmt>+
    }
    token Stmt {
        || <Goto_stmt>
        || <If_stmt>
        || <Call_stmt>
        || <Return_stmt>
        || <Assign_stmt>
        || <PHI_stmt>
    }

    token Call_stmt {
        <Func_name>  ' (' [<VorN_candi> [', ' <VorN_candi>]*]? ')' ';' <.ws>
    }
    token Call_exp {
        <Func_name>  ' (' [<VorN_candi> [', ' <VorN_candi>]*]? ')'
    }
    token Goto_exp {
        'goto ' <Block_header> [<.ws> '(' <Label0> ')']?
    }
    token Goto_stmt {
        <Goto_exp> ';' <.ws>
    }
    token compareop {
        || '=='
        || '!='
        || '<='
        || '<'
        || '>='
        || '>'
    }

    token VorN_candi {
        || <Variable>
        || <Number>
    }

    token If_stmt {
        'if' <.ws> '(' <VorN_candi> ' ' <compareop> ' ' <VorN_candi> ')' <.ws>
        <Goto_exp> ';' <.ws>
        'else' <.ws>
        <Goto_exp> ';' <.ws>
    }

    token Assign_stmt {
        <Variable> <.ws> '=' <.ws> <Exp> <.ws> ';' <.ws>
    }
    token ops {
        || '+'
        || '-'
        || '*'
        || '/'
    }

    token Exp_candi {
        || <Number>
        || <Call_exp>
        || <Variable>
    }

    token Exp {
        ['(' <ltype> ')']? <.ws> <Exp_candi> <.ws> [<ops> <.ws> ['(' <rtype> ')']? <.ws> <Exp_candi> <.ws>]?
    }

    token PHI_stmt {
        '# ' <Variable> ' = PHI <' <Variable> ', ' <Variable> '>' <.ws>
    }
    token Return_stmt {
        [<Label>':' <.ws>]?
        'return' <.ws> <Variable>? ';' <.ws>
    }
    token Label {
        '<L' <Number> '>'
    }

    token Label0 {
        '<L' <Number> '>'
    }
    token ws {
        \s*
    }
}

class getFlow {
    method TOP ($/) {
        make $curSSA;
    }

    method Func_header ($/) {
        my Function $func .= new;
        $func.name = $/<Func_name>.made;
        $curFunc = $func;
        $curSSA.name2func{$curFunc.name} = $curFunc;
    }
    method Func_name ($/) {
        make $/.Str;
    }
    method Func_declare ($/) {
        for zip($/<type>>>.made, $/<Variable>>>.made) -> ($t, $v) {
            $curFunc.vars{$v} = $t;
            $curFunc.declarevars.push($v);
        }
    }

    method ltype ($/) {
        make $/.Str;
    }
    method rtype ($/) {
        make $/.Str;
    }
    method type ($/) {
        make $/.Str;
    }

    method Function ($/) {
        $has_return_stmt = False;
    }
    method Declare ($/) {
        my $var = $/<Variable>.made;
        my $type = $/<type>.made;
        $curFunc.vars{$var} = $type;
    }
    method Variable ($/) {
        if ($/.Str.starts-with('_')) {
            make $/.Str.split('(')[0];
        } else {
            make var_real($/.Str);
        }
    }
    method Number ($/) {
        make $/.Real;
    }

    method Block_start ($/) {
        my BBlock $block .= new;
        $block.numid = $/<Number>.made;
        $curFunc.bblocks.push($block);
        $curBlock = $block;
        $curFunc.id2block{$curBlock.numid} = $curBlock;
    }
    method Block_header ($/) {
    }

    method Baseblock ($/) {
        # children are processed
        if ((not $curBlock.branch) and (not $has_return_stmt)) {
            $curBlock.branch = Branch.new(:op('true'));
            $curBlock.tbblock = $curBlock.numid + 1;
        }
    }
    method Stmt ($/) {
    }

    method Call_stmt ($/) {
        # not useful?
    }
    method Goto_exp ($/) {
    }
    method Goto_stmt ($/) {
        $curBlock.tbblock = $/<Goto_exp><Block_header><Number>.made;
        $curBlock.branch = Branch.new(:op("true"));
    }
    method VorN_candi ($/) {
        if $/<Variable> {
            make $/<Variable>.made;
        } else {
            make $/<Number>.made;
        }
    }

    method If_stmt ($/) {
        $curBlock.tbblock = $/<Goto_exp>[0]<Block_header><Number>.made;
        $curBlock.fbblock = $/<Goto_exp>[1]<Block_header><Number>.made;
        my @candimade = $/<VorN_candi>>>.made;
        my Branch $branch .= new(:op($/<compareop>.made), :left(@candimade[0]), :right(@candimade[1]));
        $curBlock.branch = $branch;
    }
    method Assign_stmt ($/) {
        my Assign $assign .= new;
        $assign.target = $/<Variable>.made;
        if $/<Exp><ops> {
            $assign.op = $/<Exp><ops>.made;
            my @expcandimade = $/<Exp><Exp_candi>>>.made;
            $assign.leftvar = @expcandimade[0];
            $assign.rightvar = @expcandimade[1];
        } else {
            my @expcandimade = $/<Exp><Exp_candi>>>.made;
            $assign.leftvar = @expcandimade[0];
            if ($assign.leftvar ~~ Num) {
                $curFunc.vars{$assign.target} = 'float';
            }
        }
        $assign.lefttype = $/<ltype>.made if $/<ltype>;
        $assign.righttype = $/<rtype>.made if $/<rtype>;
        $curBlock.assigns.push($assign);
    }
    method ops ($/) {
        make $/.Str;
    }

    method PHI_stmt ($/) {
        my @varmade = $/<Variable>>>.made;
        my Assign $assign .= new(:target(@varmade[0]), :op('PHI'), :leftvar(@varmade[1]), :rightvar(@varmade[2]));
        if @varmade[0].starts-with('_') {
            $curFunc.speciallAssign.push($assign);
        }
    }
    method Return_stmt ($/) {
        $curFunc.ret_value = $/<Variable>.made;
        $curBlock.isend = $curFunc.ret_value;
        $has_return_stmt = True;
    }
    method Exp_candi ($/) {
        if $/<Number> {
            make $/<Number>.made;
        } elsif $/<Variable> {
            make $/<Variable>.made;
        } else {
            make $/<Call_exp>.made;
        }
    }
    method Exp ($/) {
    }
    method Call_exp ($/) {
        my CallExp $callexp .= new;
        $callexp.funcname = $/<Func_name>.made;
        for $/<VorN_candi>>>.made -> $v {
            $callexp.paras.push($v);
        }
        make $callexp;
    }
    method compareop ($/) {
        make $/.Str;
    }
    method Label ($/) {
        my BBlock $block .= new;
        $block.numid = $curBlock.numid + 1;
        $curBlock.branch = Branch.new(:op('true'));
        $curBlock.tbblock = $block.numid;
        $curFunc.bblocks.push($block);
        $curBlock = $block;
        $curFunc.id2block{$curBlock.numid} = $curBlock;
    }
}

sub var_real(Str $var) {
    $var.split('_')[0]
}

sub ninf_p($v) {
    return True if ($v eq '-inf');
    return False;
}

sub inf_p($v) {
    return True if ($v eq 'inf');
    return False;
}

sub less_p($v1, $v2) {
    return True if (ninf_p($v1)) and not ninf_p($v2);
    return True if (inf_p($v2)) and not inf_p($v1);
    return False if (ninf_p($v2));
    return False if (inf_p($v1));
    return $v1 < $v2;
}

sub more_p($v1, $v2) {
    return True if (inf_p($v1)) and not inf_p($v2);
    return True if (ninf_p($v2)) and not ninf_p($v1);
    return False if (inf_p($v2));
    return False if (ninf_p($v1));
    return $v1 > $v2;
}

sub max_of($v1, $v2) {
    if (inf_p($v1) or inf_p($v2)) {
        return "inf";
    }
    if (ninf_p($v1)) {
        return $v2;
    }
    if (ninf_p($v2)) {
        return $v1;
    }
    return $v2 if ($v1 < $v2);
    return $v1;
}

sub min_of($v1, $v2) {
    if (ninf_p($v1) or ninf_p($v2)) {
        return "-inf";
    }
    if (inf_p($v1)) {
        return $v2;
    }
    if (inf_p($v2)) {
        return $v1;
    }
    return $v1 if ($v1 < $v2);
    return $v2;
}

# 一些操作
sub union_range($range1, $range2) {
    my varRange $newrange .= new;
    return $newrange if ((not $range1.init_p) and (not $range2.init_p));
    if (not $range1.init_p) {
        copy_range($newrange, $range2);
        return $newrange;
    }
    if (not $range2.init_p) {
        copy_range($newrange, $range1);
        return $newrange;
    }
    $newrange.init_p = True;
    $newrange.lbound = min_of($range1.lbound, $range2.lbound);
    $newrange.ubound = max_of($range1.ubound, $range2.ubound);
    return $newrange;
}

sub inter_range($range1, $range2) {
    my varRange $newrange .= new;
    return $newrange if ((not $range1.init_p) or (not $range2.init_p));
    if (less_p($range1.ubound, $range2.lbound) or less_p($range2.ubound, $range1.lbound)) {
        return $newrange;
    }
    $newrange.init_p = True;
    $newrange.lbound = max_of($range1.lbound, $range2.lbound);
    $newrange.ubound = min_of($range1.ubound, $range2.ubound);
    return $newrange;
}

sub PHI_range($range1, $range2) {
    return union_range($range1, $range2);
}

sub not_equal($range1, $range2) {
    return True if (ninf_p($range1.lbound) and (not ninf_p($range2.lbound)));
    return True if (ninf_p($range2.lbound) and (not ninf_p($range1.lbound)));
    return True if (inf_p($range1.ubound) and (not inf_p($range2.ubound)));
    return True if (inf_p($range2.ubound) and (not inf_p($range1.ubound)));
    if (inf_p($range1.ubound) and (not ninf_p($range1.lbound))) {
        return $range1.lbound != $range2.lbound;
    }
    if (ninf_p($range1.lbound) and (not inf_p($range1.ubound))) {
        return $range1.ubound != $range2.ubound;
    }
    return ($range1.lbound != $range2.lbound or ($range1.ubound != $range2.ubound));
}

sub add_range($range, $delta) {
    my varRange $newrange .= new;
    if ($range ~~ varRange and $delta ~~ Real) {
        return $newrange if not $range.init_p;
        copy_range($newrange, $range);
        $newrange.init_p = True;
        if (not ninf_p($newrange.lbound)) {
            $newrange.lbound += $delta;
        }
        if (not inf_p($range.ubound)) {
            $newrange.ubound += $delta;
        }
    } elsif ($range ~~ varRange and $delta ~~ varRange) {
        return $newrange if (not $range.init_p) or (not $delta.init_p);
        copy_range($newrange, $range);
        $newrange.init_p = True;
        if (ninf_p($range.lbound) or ninf_p($delta.lbound)) {
            $newrange.lbound = '-inf';
        } else {
            $newrange.lbound += $delta.lbound;
        }
        if (inf_p($range.ubound) or inf_p($delta.ubound)) {
            $newrange.ubound = 'inf';
        } else {
            $newrange.ubound += $delta.ubound;
        }
    } elsif ($range ~~ Real and $delta ~~ varRange) {
        return $newrange if not $delta.init_p;
        copy_range($newrange, $delta);
        $newrange.init_p = True;
        if (not ninf_p($range.lbound)) {
            $newrange.lbound += $range;
        }
        if (not inf_p($range.ubound)) {
            $newrange.ubound += $range;
        }
    } else {
        error(105);
    }
    return $newrange;
}

sub sub_range($range, $delta) {
    my varRange $newrange .= new;
    if ($range ~~ varRange and $delta ~~ Real) {
        return $newrange if not $range.init_p;
        copy_range($newrange, $range);
        $newrange.init_p = True;
        if (not ninf_p($range.lbound)) {
            $newrange.lbound -= $delta;
        }
        if (not inf_p($range.ubound)) {
            $newrange.ubound -= $delta;
        }
    } elsif ($range ~~ varRange and $delta ~~ varRange) {
        return $newrange if (not $range.init_p) or (not $delta.init_p);
        copy_range($newrange, $range);
        $newrange.init_p = True;
        if (ninf_p($range.lbound) or inf_p($delta.ubound)) {
            $newrange.lbound = '-inf';
        } else {
            $newrange.lbound -= $delta.ubound;
        }
        if (inf_p($range.ubound) or ninf_p($delta.lbound)) {
            $newrange.ubound = 'inf';
        } else {
            $newrange.ubound -= $delta.lbound;
        }
    } elsif ($range ~~ Real and $delta ~~ varRange) {
        return $newrange if (not $delta.init_p);
        copy_range($newrange, $delta);
        $newrange.init_p = True;
        if (not ninf_p($delta.lbound)) {
            $newrange.ubound = $range - $delta.lbound;
        }
        if (not inf_p($delta.ubound)) {
            $newrange.lbound = $range - $delta.ubound;
        }
    } else {
        error(104);
    }
    return $newrange;
}

sub mul_range($range, $delta) {
    my varRange $newrange .= new;
    if ($range ~~ varRange and $delta ~~ Real) {
        return $newrange if not $range.init_p;
        copy_range($newrange, $range);
        $newrange.init_p = True;
        if ($delta == 0) {
            $newrange.lbound = 0;
            $newrange.ubound = 0;
        } elsif ($delta > 0) {
            if (not ninf_p($range.lbound)) {
                $newrange.lbound *= $delta;
            }
            if (not inf_p($range.ubound)) {
                $newrange.ubound *= $delta;
            }
        } else {
            if (not ninf_p($range.lbound)) {
                $newrange.ubound = $range.lbound * $delta;
            } else {
                $newrange.ubound = 'inf';
            }
            if (not inf_p($range.ubound)) {
                $newrange.lbound = $range.ubound * $delta;
            } else {
                $newrange.lbound = '-inf';
            }
        }
    } elsif ($range ~~ Real and $delta ~~ varRange) {
        return $newrange if (not $delta.init_p);
        copy_range($newrange, $delta);
        $newrange.init_p = True;
        if ($range == 0) {
            $newrange.lbound = 0;
            $newrange.ubound = 0;
        } elsif ($range > 0) {
            if (not ninf_p($delta.lbound)) {
                $newrange.lbound *= $range;
            }
            if (not inf_p($delta.ubound)) {
                $newrange.ubound *= $range;
            }
        } else {
            if (not ninf_p($delta.lbound)) {
                $newrange.ubound = $delta.lbound * $range;
            } else {
                $newrange.ubound = 'inf';
            }
            if (not inf_p($range.ubound)) {
                $newrange.lbound = $delta.ubound * $range;
            } else {
                $newrange.lbound = '-inf';
            }
        }
    } elsif ($range ~~ varRange and $delta ~~ varRange) {
        return $newrange if (not $range.init_p) or (not $delta.init_p);
        copy_range($newrange, $range);
        $newrange.init_p = True;
        if ($range.gist eq $delta.gist) { # same variable
            my $temp1 = mul_s($delta.lbound, $delta.lbound);
            my $temp2 = mul_s($delta.ubound, $delta.ubound);
            $newrange.lbound = min_of($temp1, $temp2);
            $newrange.ubound = max_of($temp1, $temp2);
            if ((ninf_p($delta.lbound) or ($delta.lbound <= 0)) and (inf_p($delta.ubound) or ($delta.ubound >= 0))) {
                $newrange.lbound = 0;
            }
        } else {
            my $temp1 = mul_s($newrange.lbound, $delta.lbound);
            my $temp2 = mul_s($newrange.lbound, $delta.ubound);
            my $temp3 = mul_s($newrange.ubound, $delta.lbound);
            my $temp4 = mul_s($newrange.ubound, $delta.ubound);
            my $t1 = min_of($temp1, $temp2);
            my $t2 = min_of($temp3, $t1);
            $newrange.lbound = min_of($temp4, $t2);
            $t1 = max_of($temp1, $temp2);
            $t2 = max_of($temp3, $t1);
            $newrange.ubound = max_of($temp4, $t2);
        }
    } else {
        error(105);
    }
    return $newrange;
}

sub mul_s($v1, $v2) {
    if (inf_p($v1) and ninf_p($v2)) {
        return '-inf';
        #error(108);
    } elsif (ninf_p($v1) and inf_p($v2)) {
        return '-inf';
        #error(108);
    } elsif (inf_p($v1) and inf_p($v2)) {
        return 'inf';
    } elsif (ninf_p($v1) and ninf_p($v2)) {
        return 'inf';
    } elsif (inf_p($v1) and $v2 == 0) {
        return 0;
    } elsif (inf_p($v1) and $v2 > 0) {
        return 'inf';
    } elsif (inf_p($v1) and $v2 < 0) {
        return '-inf';
    } elsif (ninf_p($v1) and $v2 == 0) {
        return 0;
    } elsif (ninf_p($v1) and $v2 > 0) {
        return '-inf';
    } elsif (ninf_p($v1) and $v2 < 0) {
        return 'inf';
    } elsif (inf_p($v2) and $v1 == 0) {
        return 0;
    } elsif (inf_p($v2) and $v1 > 0) {
        return 'inf';
    } elsif (inf_p($v2) and $v1 < 0) {
        return '-inf';
    } elsif (ninf_p($v2) and $v1 == 0) {
        return 0;
    } elsif (ninf_p($v2) and $v1 > 0) {
        return '-inf';
    } elsif (ninf_p($v2) and $v1 < 0) {
        return 'inf';
    } else {
        return $v1 * $v2;
    }
}

sub div_range($range, $delta) {
    my varRange $newrange .= new;
    if ($range ~~ varRange and $delta ~~ Real) {
        return $newrange if not $range.init_p;
        copy_range($newrange, $range);
        $newrange.init_p = True;
        if ($delta == 0) {
            $newrange.init_p = False;
            #error(116);
        } else {
            return mul_range($range, 1 / $delta);
        }
    } elsif ($range ~~ Real and $delta ~~ varRange) {
        return $newrange if (not $delta.init_p);
        copy_range($newrange, $delta);
        $newrange.init_p = True;
        if ($range == 0) {
            $newrange.lbound = 0;
            $newrange.ubound = 0;
        } else {
            if (ninf_p($delta.lbound) and inf_p($delta.ubound)) {
                if ($delta.isint) {
                    $newrange.lbound = - ($range).abs;
                    $newrange.ubound = ($range).abs;
                } else {
                    $newrange.lbound = '-inf';
                    $newrange.ubound = 'inf';
                }
            } elsif (inf_p($delta.lbound) and ninf_p($delta.ubound)) {
                error(106);
            } elsif (inf_p($delta.lbound) and inf_p($delta.ubound)) {
                $newrange.lbound = 0;
                $newrange.ubound = 0;
            } elsif (ninf_p($delta.lbound) and ninf_p($delta.ubound)) {
                $newrange.lbound = 0;
                $newrange.ubound = 0;
            } elsif (ninf_p($delta.lbound) and $delta.ubound == 0) {
                if ($delta.isint) {
                    if ($range > 0) {
                        $newrange.lbound = - $range;
                        $newrange.ubound = 0;
                    } else {
                        $newrange.lbound = 0;
                        $newrange.ubound = $range;
                    }
                } else {
                    if ($range > 0) {
                        $newrange.lbound = '-inf';
                        $newrange.ubound = 0;
                    } else {
                        $newrange.lbound = 0;
                        $newrange.ubound = 'inf';
                    }
                }
            } elsif (ninf_p($delta.lbound) and $delta.ubound > 0) {
                if ($delta.isint) {
                    $newrange.lbound = - ($range).abs;
                    $newrange.ubound = ($range).abs;
                } else {
                    $newrange.lbound = '-inf';
                    $newrange.ubound = 'inf';
                }
            } elsif (ninf_p($delta.lbound) and $delta.ubound < 0) {
                if ($range > 0) {
                    $newrange.lbound = $range / $delta.ubound;
                    $newrange.ubound = 0;
                } else {
                    $newrange.lbound = 0;
                    $newrange.ubound = $range / $delta.ubound;
                }
            } elsif (inf_p($delta.ubound) and $delta.lbound == 0) {
                if ($delta.isint) {
                    if ($range > 0) {
                        $newrange.lbound = 0;
                        $newrange.ubound = $range;
                    } else {
                        $newrange.lbound = - $range;
                        $newrange.ubound = 0;
                    }
                } else {
                    if ($range > 0) {
                        $newrange.lbound = 0;
                        $newrange.ubound = 'inf';
                    } else {
                        $newrange.lbound = '-inf';
                        $newrange.ubound = 0;
                    }
                }
            } elsif (inf_p($delta.ubound) and $delta.lbound < 0) {
                if ($delta.isint) {
                    $newrange.lbound = - ($range).abs;
                    $newrange.ubound = ($range).abs;
                } else {
                    $newrange.lbound = '-inf';
                    $newrange.ubound = 'inf';
                }
            } elsif (inf_p($delta.ubound) and $delta.lbound > 0) {
                if ($range > 0) {
                    $newrange.lbound = 0;
                    $newrange.ubound = $range / $delta.lbound;
                } else {
                    $newrange.lbound = $range / $delta.lbound;
                    $newrange.ubound = 0;
                }
            } elsif ($delta.lbound == 0 and $delta.ubound == 0) {
                $newrange.init_p = False;
            } elsif ($delta.lbound == 0 and $delta.ubound > 0) {
                if ($range > 0) {
                    if ($delta.isint) {
                        $newrange.lbound = $range / $delta.ubound;
                        $newrange.ubound = $range;
                    } else {
                        $newrange.lbound = $range / $delta.ubound;
                        $newrange.ubound = 'inf';
                    }
                } else {
                    if ($delta.isint) {
                        $newrange.lbound = $range;
                        $newrange.ubound = $range / $delta.ubound;
                    } else {
                        $newrange.lbound = '-inf';
                        $newrange.ubound = $range / $delta.ubound;
                    }
                }
            } elsif ($delta.lbound < 0 and $delta.ubound == 0) {
                if ($range > 0) {
                    if ($delta.isint) {
                        $newrange.lbound = - $range;
                        $newrange.ubound = $range / $delta.lbound;
                    } else {
                        $newrange.lbound = '-inf';
                        $newrange.ubound = $range / $delta.lbound;
                    }
                } else {
                    if ($delta.isint) {
                        $newrange.lbound = $range / $delta.lbound;
                        $newrange.ubound = - $range;
                    } else {
                        $newrange.lbound = $range / $delta.lbound;
                        $newrange.ubound = 'inf';
                    }
                }
            } elsif ($delta.lbound < 0 and $delta.ubound > 0) {
                if ($delta.isint) {
                    $newrange.lbound = - ($range).abs;
                    $newrange.ubound = ($range).abs;
                } else {
                    $newrange.lbound = '-inf';
                    $newrange.ubound = 'inf';
                }
            } elsif (($delta.lbound > 0 and $delta.ubound > 0) or ($delta.lbound < 0 and $delta.ubound < 0)) {
                # both > 0 or < 0
                if ($range > 0) {
                    $newrange.lbound = $range / $delta.ubound;
                    $newrange.ubound = $range / $delta.lbound;
                } else {
                    $newrange.lbound = $range / $delta.lbound;
                    $newrange.ubound = $range / $delta.ubound;
                }
            } else {
                error(117);
            }
        }
    } elsif ($range ~~ varRange and $delta ~~ varRange) {
        return $newrange if (not $range.init_p) or (not $delta.init_p);
        copy_range($newrange, $range);
        $newrange.init_p = True;
        if ($range.gist eq $delta.gist) {
            $newrange.lbound = 1;
            $newrange.ubound = 1;
        } else {
            #$newrange.lbound /= $delta.lbound;
            #$newrange.ubound /= $delta.ubound;
            my $delta2 = div_range(1, $delta);
            return mul_range($range, $delta2);
        }
    } else {
        error(116);
    }
    return $newrange;
}

sub add_defineRanges($func, %defineRanges) {
    my $minblock = $func.id2block.keys>>.Int.min;
    $func.runStack.push($minblock);
    my $block = $func.id2block{$minblock};
    for $func.id2block.values -> $block {
        for $func.vars.keys -> $var {
            $block.var2range{$var} = varRange.new;
            if ($func.vars{$var} eq 'float') {
                $block.var2range{$var}.isint = False;
            }
        }
    }
    if (%defineRanges and %defineRanges.elems > 0) {
        for %defineRanges.kv -> $k, $v {
            $block.var2range{$k} = $v;
        }
    }
}

sub compute_equal($ssa, $func, $block, $assign) {
    my $rangeT = $block.var2range{$assign.target};
    if $assign.leftvar ~~ Real {
        $rangeT.lbound = $assign.leftvar;
        $rangeT.ubound = $assign.leftvar;
        if (not $rangeT.init_p) {
            $rangeT.init_p = True;
        }
    } elsif ($assign.leftvar ~~ varRange) {
        $rangeT.lbound = $assign.leftvar.lbound;
        $rangeT.ubound = $assign.leftvar.ubound;
        if (not $rangeT.init_p) {
            $rangeT.init_p = True;
        }
    } elsif ($assign.leftvar ~~ Str) { # j = k;
        my $in_range = $block.var2range{$assign.leftvar};
        if ($in_range.init_p) {
            if (not $rangeT.init_p) {
                $rangeT.init_p = True;
                copy_range($rangeT, $in_range);
            } elsif (not_equal($rangeT, $in_range)) {
                if ($func.ret_value eq $assign.target) {
                    $func.keeptime += 1;
                    if (less_p($in_range.lbound, $rangeT.lbound)) {
                        $rangeT.lbound = $in_range.lbound;
                        $func.keeptime = 0;
                    }
                    if (more_p($in_range.ubound, $rangeT.ubound)) {
                        $rangeT.ubound = $in_range.ubound;
                        $func.keeptime = 0;
                    }
                    if $func.keeptime > 5 {
                        $func.runStack = [];
                    }
                } else {
                    copy_range($rangeT, $in_range);
                }
            }
        }
    } elsif ($assign.leftvar ~~ CallExp) {
        my %defineRanges = %( );
        my $func_outer = $ssa.name2func{$assign.leftvar.funcname};
        for ($assign.leftvar.paras.kv) -> $k, $var {
            my $range = varRange.new;
            copy_range($range, $block.var2range{$var});
            %defineRanges{$func_outer.declarevars[$k]} = $range;
        }
        my $in_range = range_analysis($ssa, $assign.leftvar.funcname, %defineRanges);
        if ($in_range.init_p) {
            if (not $rangeT.init_p) {
                $rangeT.init_p = True;
                copy_range($rangeT, $in_range);
            } elsif (not_equal($rangeT, $in_range)) {
                if ($func.ret_value eq $assign.target) {
                    if (less_p($in_range.lbound, $rangeT.lbound)) {
                        $rangeT.lbound = $in_range.lbound;
                    }
                    if (more_p($in_range.ubound, $rangeT.ubound)) {
                        $rangeT.ubound = $in_range.ubound;
                    }
                } else {
                    copy_range($rangeT, $in_range);
                }
            }
        }
    }
}

sub compute_two_var($func, $block, $assign) {
    my $rangeT = $block.var2range{$assign.target};
    my $in_range;
    my $left, my $right;
    if ($assign.leftvar ~~ Str and $assign.rightvar ~~ Str) {
        $left = $block.var2range{$assign.leftvar};
        $right = $block.var2range{$assign.rightvar};
    } elsif ($assign.leftvar ~~ Str and $assign.rightvar ~~ Real) {
        $left = $block.var2range{$assign.leftvar};
        $right = $assign.rightvar;
    } elsif ($assign.leftvar ~~ Real and $assign.rightvar ~~ Str) {
        $left = $assign.leftvar;
        $right = $block.var2range{$assign.rightvar};
    } else {
        error(100);
    }
    if ($assign.op eq 'PHI') {
        $in_range = PHI_range($left, $right);
    } elsif ($assign.op eq '+') {
        $in_range = add_range($left, $right);
    } elsif ($assign.op eq '-') {
        $in_range = sub_range($left, $right);
    } elsif ($assign.op eq '*') {
        $in_range = mul_range($left, $right);
    } elsif ($assign.op eq '/') {
        $in_range = div_range($left, $right);
    }
    if ($in_range.init_p) {
        if (not $rangeT.init_p) {
            copy_range($rangeT, $in_range);
            $rangeT.init_p = True;
        } elsif (not_equal($rangeT, $in_range)) {
            if ($func.ret_value eq $assign.target) {
                if (less_p($in_range.lbound, $rangeT.lbound)) {
                    $rangeT.lbound = $in_range.lbound;
                }
                if (more_p($in_range.ubound, $rangeT.ubound)) {
                    $rangeT.ubound = $in_range.ubound;
                }
            } else {
                copy_range($rangeT, $in_range);
            }
        }
    } else {
        error(101);
    }
}

sub pass_var_to_block($block, $blocknext) {
    for ($block.var2range.keys) -> $var {
        if ($block.var2range{$var}.init_p) {
            copy_range($blocknext.var2range{$var}, $block.var2range{$var});
            $blocknext.var2range{$var}.init_p = True;
        }
    }
    $blocknext.locked = True;
}

sub get_branch_range($block, $branch) {
    my $range1;
    my $range2;
    my $range3;
    my $range4;
    if ($branch.op eq '>') {
        $branch.op = '<=';
        ($block.tbblock, $block.fbblock) = ($block.fbblock, $block.tbblock);
    } elsif ($branch.op eq '>=') {
        $branch.op = '<';
        ($block.tbblock, $block.fbblock) = ($block.fbblock, $block.tbblock);
    } elsif ($branch.op eq '==') {
        $branch.op = '!=';
        ($block.tbblock, $block.fbblock) = ($block.fbblock, $block.tbblock);
    }
    if ($branch.op eq '<=') {
        if ($branch.left ~~ Str and $branch.right ~~ Str) {
            my $rangeL = $block.var2range{$branch.left};
            my $rangeR = $block.var2range{$branch.right};
            my $range1t = varRange.new(:lbound('-inf'), :init_p(True));
            if (not inf_p($rangeR.ubound)) {
                $range1t.ubound = $rangeR.ubound;
            } else {
                $range1t.ubound = 'inf';
            }
            $range1 = inter_range($rangeL, $range1t);
            my $range2t = varRange.new(:ubound('inf'), :init_p(True));
            if (not ninf_p($rangeL.lbound)) {
                $range2t.lbound = $rangeL.lbound;
            } else {
                $range2t.lbound = '-inf';
            }
            $range2 = inter_range($rangeR, $range2t);
            my $range3t = varRange.new(:ubound('inf'), :init_p(True));
            if (not ninf_p($rangeR.lbound)) {
                $range3t.lbound = $rangeR.lbound + 1;
            } else {
                $range3t.lbound = '-inf';
            }
            $range3 = inter_range($rangeL, $range3t);
            my $range4t = varRange.new(:lbound('-inf'), :init_p(True));
            if (not inf_p($rangeL.ubound)) {
                $range4t.ubound = $rangeL.ubound - 1;
            } else {
                $range4t.ubound = 'inf';
            }
            $range4 = inter_range($rangeR, $range4t);
        } elsif ($branch.left ~~ Str and $branch.right ~~ Real) {
            my $rangeL = $block.var2range{$branch.left};
            my $rangeR = varRange.new(:lbound($branch.right), :ubound($branch.right), :init_p(True));
            my $range1t = varRange.new(:lbound('-inf'), :init_p(True));
            if (not inf_p($rangeR.ubound)) {
                $range1t.ubound = $rangeR.ubound;
            } else {
                $range1t.ubound = 'inf';
            }
            $range1 = inter_range($rangeL, $range1t);
            $range2 = $rangeR;
            my $range3t = varRange.new(:ubound('inf'), :init_p(True));
            if (not ninf_p($rangeR.lbound)) {
                $range3t.lbound = $rangeR.lbound + 1;
            } else {
                $range3t.lbound = '-inf';
            }
            $range3 = inter_range($rangeL, $range3t);
            $range4 = $rangeR;
        } elsif ($branch.left ~~ Real and $branch.right ~~ Str) {
            my $rangeL = varRange.new(:lbound($branch.right), :ubound($branch.right), :init_p(True));
            my $rangeR = $block.var2range{$branch.left};
            $range1 = $rangeL;
            my $range2t = varRange.new(:ubound('inf'), :init_p(True));
            if (not ninf_p($rangeL.lbound)) {
                $range2t.lbound = $rangeL.lbound;
            } else {
                $range2t.lbound = '-inf';
            }
            $range2 = inter_range($rangeL, $range2t);
            $range3 = $rangeL;
            my $range4t = varRange.new(:lbound('-inf'), :init_p(True));
            if (not inf_p($rangeL.ubound)) {
                $range4t.ubound = $rangeL.ubound - 1;
            } else {
                $range4t.ubound = 'inf';
            }
            $range4 = inter_range($rangeL, $range4t);
        }
    } elsif ($branch.op eq '<') {
        if ($branch.left ~~ Str and $branch.right ~~ Str) {
            my $rangeL = $block.var2range{$branch.left};
            my $rangeR = $block.var2range{$branch.right};
            my $range1t = varRange.new(:lbound('-inf'), :init_p(True));
            if (not inf_p($rangeR.ubound)) {
                $range1t.ubound = $rangeR.ubound - 1;
            } else {
                $range1t.ubound = 'inf';
                $rangeL.lbound = max_of($rangeL.lbound, $rangeR.lbound);
                $rangeL.ubound = 'inf';
            }
            $range1 = inter_range($rangeL, $range1t);
            my $range2t = varRange.new(:ubound('inf'), :init_p(True));
            if (not ninf_p($rangeL.lbound)) {
                $range2t.lbound = $rangeL.lbound + 1;
            } else {
                $range2t.lbound = '-inf';
            }
            $range2 = inter_range($rangeR, $range2t);
            my $range3t = varRange.new(:ubound('inf'), :init_p(True));
            if (not ninf_p($rangeR.lbound)) {
                $range3t.lbound = $rangeR.lbound;
            } else {
                $range3t.lbound = '-inf';
            }
            $range3 = inter_range($rangeL, $range3t);
            my $range4t = varRange.new(:lbound('-inf'), :init_p(True));
            if (not inf_p($rangeL.ubound)) {
                $range4t.ubound = $rangeL.ubound;
            } else {
                $range4t.ubound = 'inf';
            }
            $range4 = inter_range($rangeR, $range4t);
        } elsif ($branch.left ~~ Str and $branch.right ~~ Real) {
            my $rangeL = $block.var2range{$branch.left};
            my $rangeR = varRange.new(:lbound($branch.right), :ubound($branch.right), :init_p(True));
            my $range1t = varRange.new(:lbound('-inf'), :init_p(True));
            if (not inf_p($rangeR.ubound)) {
                $range1t.ubound = $rangeR.ubound - 1;
            } else {
                $range1t.ubound = 'inf';
            }
            $range1 = inter_range($rangeL, $range1t);
            $range2 = $rangeR;
            my $range3t = varRange.new(:ubound('inf'), :init_p(True));
            if (not ninf_p($rangeR.lbound)) {
                $range3t.lbound = $rangeR.lbound;
            } else {
                $range3t.lbound = '-inf';
            }
            $range3 = inter_range($rangeL, $range3t);
            $range4 = $rangeR;
        } elsif ($branch.left ~~ Real and $branch.right ~~ Str) {
            my $rangeL = varRange.new(:lbound($branch.right), :ubound($branch.right), :init_p(True));
            my $rangeR = $block.var2range{$branch.left};
            $range1 = $rangeL;
            my $range2t = varRange.new(:lbound('-inf'), :init_p(True));
            if (not inf_p($rangeL.ubound)) {
                $range2t.ubound = $rangeL.ubound - 1;
            } else {
                $range2t.ubound = 'inf';
            }
            $range2 = inter_range($rangeR, $range2t);
            $range3 = $rangeL;
            my $range4t = varRange.new(:ubound('inf'), :init_p(True));
            if (not ninf_p($rangeL.lbound)) {
                $range4t.lbound = $rangeL.lbound;
            } else {
                $range4t.lbound = '-inf';
            }
            $range4 = inter_range($rangeR, $range4t);
        }
    } elsif ($branch.op eq '!=') {
        if ($branch.left ~~ Str and $branch.right ~~ Str) {
            my $rangeL = $block.var2range{$branch.left};
            my $rangeR = $block.var2range{$branch.right};
            $range1 = varRange.new;
            copy_range($range1, $rangeL);
            $range2 = varRange.new;
            copy_range($range2, $rangeR);
            $range3 = inter_range($rangeL, $rangeR);
            $range4 = inter_range($rangeL, $rangeR);
            if (($rangeL.lbound eq $rangeR.lbound) and ($rangeL.ubound eq $rangeR.ubound) and ($rangeL.lbound ~~ Real) and ($rangeL.ubound ~~ Real)) {
                $range1.init_p = False;
                $range2.init_p = False;
            }
        } elsif ($branch.left ~~ Str and $branch.right ~~ Real) {
            my $rangeL = $block.var2range{$branch.left};
            my $rangeR = varRange.new(:lbound($branch.right), :ubound($branch.right), :init_p(True));
            $range1 = varRange.new;
            copy_range($range1, $block.var2range{$branch.left});
            if (not_equal($rangeL, $rangeR)) {
                $range1.init_p = True;
            } else {
                $range1.init_p = False;
            }
            $range2 = $rangeR;
            $range3 = inter_range($rangeL, $rangeR);
            $range4 = $rangeR;
        }
    }
    return $range1, $range2, $range3, $range4;
}

sub not_para($func, $var) {
    for (0..^$func.declarevars.elems) {
        if ($var eq $func.declarevars[$_]) {
            return False;
        }
    }
    return True;
}

sub run_block($ssa, $func, $block) {
    say "block {$block.numid}";
    $block.locked = False;
    for $block.assigns -> $assign {
        if $assign.op {
            compute_two_var($func, $block, $assign);
        } else { # =
            compute_equal($ssa, $func, $block, $assign);
        }
    }
    if ($block.isend) {
        for $func.speciallAssign -> $assign {
            if $assign.op {
                compute_two_var($func, $block, $assign);
            } else { # =
                compute_equal($ssa, $func, $block, $assign);
            }
        }
    }
    if ($block.branch) {
        if ($block.branch.op ne 'true') {
            my ($range1, $range2, $range3, $range4) = get_branch_range($block, $block.branch);
            if ($range1.init_p and $range2.init_p) {
                my $blocknext1 = $func.id2block{$block.tbblock};
                test_block($ssa, $func, $blocknext1);
                pass_var_to_block($block, $blocknext1);
                $blocknext1.var2range{$block.branch.left} = $range1 if $block.branch.left ~~ Str;
                $blocknext1.var2range{$block.branch.right} = $range2 if $block.branch.right ~~ Str;
                $func.runStack.push($blocknext1.numid);
            }
            if ($range3.init_p and $range4.init_p) {
                my $blocknext2 = $func.id2block{$block.fbblock};
                test_block($ssa, $func, $blocknext2);
                pass_var_to_block($block, $blocknext2);
                $blocknext2.var2range{$block.branch.left} = $range3 if $block.branch.left ~~ Str;
                $blocknext2.var2range{$block.branch.right} = $range4 if $block.branch.right ~~ Str;
                $func.runStack.push($blocknext2.numid);
            }
        } else {
            my $blocknext = $func.id2block{$block.tbblock};
            test_block($ssa, $func, $blocknext);
            pass_var_to_block($$block, $blocknext);
            $func.runStack.push($blocknext.numid);
        }
    }
}

sub test_block($ssa, $func, $block) {
    while ($block.locked) {
        for 0..^$func.runStack.elems {
            if ($func.runStack[$_] and $func.runStack[$_] eq $block.numid) {
                $func.runStack[$_]:delete;
                last;
            }
        }
        run_block($ssa, $func, $block);
    }
}

sub range_analysis(SSA $ssa, Str $funcname, %defineRanges?) {
    my Function $func = $ssa.name2func{$funcname};
    $func.runStack = [];
    add_defineRanges($func, %defineRanges);
    while ($func.runStack.elems > 0) {
        if ($func.runStack.elems > 10) {
            exit(0);
        }
        my $blockId = $func.runStack.shift;
        while (not $blockId) {
            $blockId = $func.runStack.shift;
        }
        my $block = $func.id2block{$blockId};
        run_block($ssa, $func, $block);
    }
    my $maxblockId = $func.id2block.keys>>.Int.max;
    my $lastBlock = $func.id2block{$maxblockId};
    return $lastBlock.var2range{$func.ret_value};
}

sub input_range(%defineRanges, $var, $lb, $ub, $isint) {
    %defineRanges{$var} = varRange.new(:lbound($lb), :ubound($ub), :init_p(True), :isint($isint));
}

sub MAIN(Str $file = "t/t1.ssa") {
    my $ssaStr = $file.IO.slurp;
    my $result = Flow.parse($ssaStr, actions => getFlow.new);
    if (not $result) {
        say "parse error";
        return;
    }
    my SSA $ssa = $result.made;
    my %defineRanges;
    if ($file.ends-with('t2.ssa')) {
        input_range(%defineRanges, 'k', 200, 300, True);
    } elsif ($file.ends-with('t3.ssa')) {
        input_range(%defineRanges, 'k', 0, 10, True);
        input_range(%defineRanges, 'N', 20, 50, True);
    } elsif ($file.ends-with('t4.ssa')) {
        input_range(%defineRanges, 'argc', '-inf', 'inf', True);
    } elsif ($file.ends-with('t6.ssa')) {
        input_range(%defineRanges, 'argc', '-inf', 'inf', True);
    } elsif ($file.ends-with('t7.ssa')) {
        input_range(%defineRanges, 'i', -10, 10, True);
    } elsif ($file.ends-with('t8.ssa')) {
        input_range(%defineRanges, 'a', 1, 100, True);
        input_range(%defineRanges, 'b', -2, 2, True);
    } elsif ($file.ends-with('t10.ssa')) {
        input_range(%defineRanges, 'a', 30, 50, True);
        input_range(%defineRanges, 'b', 90, 100, True);
    }
    my $ret = range_analysis($ssa, "foo", %defineRanges);
    say "result: $ret";
}

sub error($num) {
    say "error $num";
    exit($num);
}
