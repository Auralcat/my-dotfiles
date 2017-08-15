# -*- tab-width:  4 -*- ;; Emacs
# vi: set tabstop=8     :: Vi/ViM
#
# .bash_profile
# 	Author:      Devin Teske <dteske@freebsd.org>
# 	Last Change: 2017 May 8
############################################################ GLOBALS

#
# Global exit status variables
#
export SUCCESS=0
export FAILURE=1

#
# Are we running interactively?
#
interactive=
case "$-" in
*i*) interactive=1
esac

#
# Is filename globbing enabled?
#
glob=1
case "$SHELLOPTS" in noglob|*:noglob:*|*:noglob) glob= ;; esac

#
# OS Specifics
# NB: Requires uname(1) -- from base system
#
: ${UNAME_s:=$(uname -s)}
: ${UNAME_r:=$(uname -r)}
: ${UNAME_p:=$(uname -p)}
: ${MESSAGES:=/var/log/messages}
case "$UNAME_s" in
Linux) Linux=$( lsb_release -si 2> /dev/null ) ;;
esac

#
# Fix lolcat to work properly in local XTerm
#
[ "$TERM" != "${TERM#xterm}" -a ! "$SSH_CLIENT" ] && stty tab0

#
# bash(1) builtin's we should pass to $LOLCAT [lolcat]
#
LOL_BUILTINS='cd dirs echo help popd printf pushd pwd shopt times type'
_LOL_BUILTINS=
[ "$glob" ] && set -f
for _prog in $LOL_BUILTINS; do
	_LOL_BUILTINS="$_LOL_BUILTINS|$_prog"
done
unset _prog
_LOL_BUILTINS="${_LOL_BUILTINS#|}"
[ "$glob" ] && set +f

#
# External programs we should NOT pass to $LOLCAT [lolcat]
#
NOLOL_PROGS='
	adduser
	*bash bashbug bc bitchx bsdconfig bsdinstall
	cscope csh cvsbug
	dd dialog dpv
	edit edquota ee emacs* env etcupdate ex
	freebsd-update fsdb *ftp fvwm-bug
	*gdb*
	*hx
	iftop irssi
	*login lolcat
	Mail mail* *man*
	nano ngctl nvi
	perlbug perlthanks pico pppctl
	rcp
	sade *scp screen sh shgid sr srvi *ssh* su sudo sysinstall systat
	talk tcsh *telnet tmux tzsetup
	vi *view vigr *vim* vipw visudo
	watch
	zsh
' # NOLOL_PROGS
_NOLOL_PROGS=
[ "$glob" ] && set -f
for _prog in $NOLOL_PROGS; do
	_NOLOL_PROGS="$_NOLOL_PROGS|$_prog"
done
unset _prog
_NOLOL_PROGS="${_NOLOL_PROGS#|}"
[ "$glob" ] && set +f

#
# Commands we should pass to $LOLCAT [lolcat]
#
LOL_CMDS='
	# One case pattern per-line; no inline comments
	# NB: Whitespace needs to be escaped or quoted
	# NB: Patterns implicitly allow trailing args

	# FreeBSD commands
	"wpa_cli [a-z]*"

	# FreeBSD pkg(8) commands
	"pkg annotate -y"
	"pkg annotate "*" -y"
	"pkg annotate --yes"
	"pkg annotate "*" --yes"
	"pkg autoremove -y"
	"pkg autoremove "*" -y"
	"pkg autoremove --yes"
	"pkg autoremove "*" --yes"
	"pkg check -y"
	"pkg check "*" -y"
	"pkg check --yes"
	"pkg check "*" --yes"
	"pkg clean -y"
	"pkg clean "*" -y"
	"pkg clean --yes"
	"pkg clean "*" --yes"
	"pkg delete -y"
	"pkg delete "*" -y"
	"pkg delete --yes"
	"pkg delete "*" --yes"
	"pkg fetch -y"
	"pkg fetch "*" -y"
	"pkg fetch --yes"
	"pkg fetch "*" --yes"
	"pkg install -y"
	"pkg install "*" -y"
	"pkg install --yes"
	"pkg install "*" --yes"
	"pkg lock -y"
	"pkg lock "*" -y"
	"pkg lock --yes"
	"pkg lock "*" --yes"
	"pkg remove -y"
	"pkg remove "*" -y"
	"pkg remove --yes"
	"pkg remove "*" --yes"
	"pkg unlock -y"
	"pkg unlock "*" -y"
	"pkg unlock --yes"
	"pkg unlock "*" --yes"
	"pkg upgrade -y"
	"pkg upgrade "*" -y"
	"pkg upgrade --yes"
	"pkg upgrade "*" --yes"

	# VCS commands
	"cvs ci -m"
	"cvs ci "*" -m"
	"cvs commit -m"
	"cvs commit "*" -m"
	"git commit -m"
	"git commit "*" -m"
	"git commit --message"
	"git commit "*" --message"
	"git commit --no-edit"
	"git commit "*" --no-edit"
	"git revert --no-edit"
	"git revert "*" --no-edit"
	"p4 branch -"[dio]
	"p4 branch "*" -"[dio]
	"p4 change -"[dio]
	"p4 change "*" -"[dio]
	"p4 changelist -"[dio]
	"p4 changelist "*" -"[dio]
	"p4 client -"[dio]
	"p4 client "*" -"[dio]
	"p4 depot -"[dio]
	"p4 depot "*" -"[dio]
	"p4 job -"[dio]
	"p4 job "*" -"[dio]
	"p4 label -"[dio]
	"p4 label "*" -"[dio]
	"p4 protect -"[io]
	"p4 protect "*" -"[io]
	"p4 resolve -a"[mty]
	"p4 resolve "*" -a"[mty]
	"p4 stream -"[dio]
	"p4 stream "*" -"[dio]
	"p4 submit -"[di]
	"p4 submit "*" -"[di]
	"p4 user -"[dio]
	"p4 user "*" -"[dio]
	"p4 workspace -"[dio]
	"p4 workspace "*" -"[dio]
	"svn ci -m"
	"svn ci "*" -m"
	"svn ci --message"
	"svn ci "*" --message"
	"svn ci --non-interactive"
	"svn ci "*" --non-interactive"
	"svn commit -m"
	"svn commit "*" -m"
	"svn commit --message"
	"svn commit "*" --message"
	"svn commit --non-interactive"
	"svn ci "*" --non-interactive"

' # LOL_CMDS
_LOL_CMDS=$( echo "$LOL_CMDS" |
	while read -r _cmd; do
		[ "${_cmd:-x}" = "${_cmd#\#}" ] || continue
		echo -n "|$_cmd|$_cmd[\$IFS]*"
	done
)
_LOL_CMDS="${_LOL_CMDS#|}"

#
# Commands we should NOT pass to $LOLCAT [lolcat]
#
NOLOL_CMDS='
	# One case pattern per-line; no inline comments
	# NB: Whitespace needs to be escaped or quoted
	# NB: Patterns implicitly allow trailing args

	# Commands containing pipe
	*"|"*

	# General commands that launch $EDITOR or behave interactively
	"crontab -e"
	"crontab "*" -e"
	"sdiff -o"
	"sdiff "*" -o"

	# FreeBSD specific commands that launch $EDITOR or behave interactively
	"bsdlabel -e"
	"camcontrol modepage "*" -e"
	"disklabel -e"
	"gbde init "*" -i"
	"gvinum create"
	"make config"
	"make import"
	"wpa_cli"

	# FreeBSD pkg(8) commands that either prompt or show progress
	"pkg annotate"
	"pkg autoremove"
	"pkg check"
	"pkg clean"
	"pkg delete"
	"pkg fetch"
	"pkg install"
	"pkg lock"
	"pkg remove"
	"pkg shell"
	"pkg unlock"
	"pkg upgrade"

	# VCS commands that launch $EDITOR or behave interactively
	"cvs ci"
	"cvs commit"
	"git commit"
	"git help"
	"git rebase -i"
	"git rebase "*" -i"
	"git rebase --interactive"
	"git rebase "*" --interactive"
	"git revert"
	"p4 attribute -i"
	"p4 attribute "*" -i"
	"p4 branch"
	"p4 change"
	"p4 changelist"
	"p4 client"
	"p4 depot"
	"p4 job"
	"p4 label"
	"p4 login"
	"p4 passwd"
	"p4 protect"
	"p4 resolve"
	"p4 stream"
	"p4 submit"
	"p4 user"
	"p4 workspace"
	"svn ci"
	"svn commit"

	# Other commands that behave interactively
	"make chroot_shell"
	"make "*" chroot_shell"
	"make distclean"
	"make "*" distclean"
	"make remove_chroot"
	"make "*" remove_chroot"

        # My extra cmds
        "nyancat"
        "cmatrix"
        '
        # NOLOL_CMDS
_NOLOL_CMDS=$( echo "$NOLOL_CMDS" |
	while read -r _cmd; do
		[ "${_cmd:-x}" = "${_cmd#\#}" ] || continue
		echo -n "|$_cmd|$_cmd[\$IFS]*"
	done
)
_NOLOL_CMDS="${_NOLOL_CMDS#|}"

#
# Where are apache's log files?
#
case "$UNAME_s" in
FreeBSD)
	case "$UNAME_r" in
	4.8-*)
		: ${APACHE_ERROR_LOG:=/usr/local/www/logs/error_log}
		: ${APACHE_ACCESS_LOG:=/usr/local/www/logs/access_log}
		: ${APACHE_ERROR_LOG_SA:=$APACHE_ERROR_LOG}
		: ${APACHE_ACCESS_LOG_SA:=$APACHE_ACCESS_LOG}
		: ${APACHE_SSL_ERROR_LOG:=/usr/local/www/logs/ssl_error_log}
		: ${APACHE_SSL_ACCESS_LOG:=/usr/local/www/logs/ssl_access_log}
		;;
	8.3-*)
		: ${APACHE_ERROR_LOG:=/var/log/httpd-error.log}
		: ${APACHE_ACCESS_LOG:=/var/log/httpd-access.log}
		: ${APACHE_ERROR_LOG_SA:=$APACHE_ERROR_LOG}
		: ${APACHE_ACCESS_LOG_SA:=$APACHE_ACCESS_LOG}
		: ${APACHE_SSL_ERROR_LOG:=/var/log/httpd-ssl_error.log}
		: ${APACHE_SSL_ACCESS_LOG:=/var/log/httpd-ssl_request.log}
		;;
	*)
		: ${APACHE_ERROR_LOG:=/usr/local/apache2/logs/error_log}
		: ${APACHE_ACCESS_LOG:=/usr/local/apache2/logs/access_log}
		: ${APACHE_ERROR_LOG_SA:=$APACHE_ERROR_LOG}
		: ${APACHE_ACCESS_LOG_SA:=$APACHE_ACCESS_LOG}
		: ${APACHE_SSL_ERROR_LOG:=/usr/local/apache2/logs/ssl_error_log}
		: ${APACHE_SSL_ACCESS_LOG:=/usr/local/apache2/logs/ssl_access_log}
		;;
	esac
	;;
Linux)
	case "$Linux" in
	Ubuntu)
		etc_httpd=/etc/apache2
		error_log=error.log access_log=access.log
		internal_error_log=internal_error_log
		internal_access_log=internal_access_log
		ssl_error_log=ssl_error_log
		ssl_access_log=ssl_access_log
		;;
	CentOS)
		etc_httpd=/etc/httpd
		error_log=error_log access_log=access_log
		internal_error_log=internal_error_log
		internal_access_log=internal_access_log
		ssl_error_log=ssl_error_log
		ssl_access_log=ssl_access_log
		;;
	*)
		etc_httpd=/etc/httpd
		[ -d "$etc_httpd" ] || etc_httpd=/etc/apache2
		error_log=error_log access_log=access_log
		internal_error_log=internal_error_log
		internal_access_log=internal_access_log
		ssl_error_log=ssl_error_log
		ssl_access_log=ssl_access_log
	esac
	: ${APACHE_ERROR_LOG:=$etc_httpd/logs/$error_log}
	: ${APACHE_ACCESS_LOG:=$etc_httpd/logs/$access_log}
	: ${APACHE_ERROR_LOG_SA:=$etc_httpd/logs/$internal_error_log}
	: ${APACHE_ACCESS_LOG_SA:=$etc_httpd/logs/$internal_access_log}
	: ${APACHE_SSL_ERROR_LOG:=$etc_httpd/logs/$ssl_error_log}
	: ${APACHE_SSL_ACCESS_LOG:=$etc_httpd/logs/$ssl_access_log}
	;;
CYGWIN*)
	: ${APACHE_ERROR_LOG:=/var/log/apache2/error_log}
	: ${APACHE_ACCESS_LOG:=/var/log/apache2/access_log}
	: ${APACHE_ERROR_LOG_SA:=$APACHE_ERROR_LOG}
	: ${APACHE_ACCESS_LOG_SA:=$APACHE_ACCESS_LOG}
	: ${APACHE_SSL_ERROR_LOG:=/var/log/apache2/ssl_error_log}
	: ${APACHE_SSL_ACCESS_LOG:=/var/log/apache2/ssl_access_log}
	;;
esac

#
# Set DISPLAY for Xming
#
case "$UNAME_s" in
CYGWIN*)
	export DISPLAY=localhost:0
	;;
esac

#
# For dialog(1) and Xdialog(1) menus -- mainly cvspicker in FUNCTIONS below
#
DIALOG_MENU_TAGS="123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#
# Default directory to store dialog(1) and Xdialog(1) temporary files
#
: ${DIALOG_TMPDIR:="/tmp"}

#
# cvspicker export commands (one per-line)
#
CVSPICKER_EXPORTS="
	CVS_RSH=ssh CVSROOT=cvs.vicor.com:/repos/projects
	CVS_RSH=ssh CVSROOT=mp1.yvod.com:/CVSbase/iPhoneApps
	CVS_RSH=ssh CVSROOT=:ext:devinteske@druidbsd.cvs.sourceforge.net:/cvsroot/druidbsd
	CVS_RSH=ssh CVSROOT=dcvs.freebsd.org:/home/dcvs
	CVS_RSH=ssh CVSROOT=pcvs.freebsd.org:/home/pcvs
	CVS_RSH=ssh CVSROOT=projcvs.freebsd.org:/home/projcvs
	CVS_RSH=ssh CVSROOT=ncvs.freebsd.org:/home/ncvs
" # END-QUOTE

#
# perforce settings
#
export P4CONFIG=P4CONFIG

#
# route(8) helper settings (see routes() et al., in FUNCTIONS)
#
# NB: ROUTES_${NAME} syntax is "DOMAIN ROUTE_ARGS" where:
# 	NAME		1st arg to routes() (converted to upper-case)
# 	DOMAIN		Domain prefix to match in hosts(5)
# 	ROUTE_ARGS	Arguments to pass to route(8) for matched entries
#
case "$UNAME_s" in
FreeBSD)
	pptp_iface=$( service pptp iface 2> /dev/null )
	ROUTES_PANZURA=".pixel8networks.com -iface ${pptp_iface:-tun0}"
	unset pptp_iface
	;;
*) ROUTES_PANZURA=".pixel8networks.com -iface ppp0"
esac
ROUTES_VICOR=".vicor.com 10.242.182.1"

############################################################ FUNCTIONS

unalias quietly 2> /dev/null
quietly() { "$@" > /dev/null 2>&1; }

quietly unalias have
have() { type "$@" > /dev/null 2>&1; }

quietly unalias eval2
eval2() {
	if [ "$LOLCAT" ]; then
		echo "$*" | $LOLCAT
	else
		echo "$*"
	fi
	eval "$@"
}

# setvar $var_to_set [$value]
#
# Implement setvar for shells unlike FreeBSD sh(1).
#
if ! have setvar; then
setvar()
{
	[ $# -gt 0 ] || return ${SUCCESS:-0}
	local __setvar_var_to_set="$1" __setvar_right="$2" __setvar_left=
	case $# in
	1) unset "$__setvar_var_to_set"
	   return $? ;;
	2) : fall through ;;
	*) echo "$FUNCNAME: too many arguments" >&2
	   return ${FAILURE:-1}
	esac
	case "$__setvar_var_to_set" in *[!0-9A-Za-z_]*)
		echo "$FUNCNAME: $__setvar_var_to_set: bad variable name" >&2
		return 2
	esac
	while case "$__setvar_r" in *\'*) : ;; *) false ; esac
	do
		__setvar_left="$__setvar_left${__setvar_right%%\'*}'\\''"
		__setvar_right="${__setvar_right#*\'}"
	done
	__setvar_left="$__setvar_left${__setvar_right#*\'}"
	eval "$__setvar_var_to_set='$__setvar_left'"
}
fi

# lolexec [$command [$args ...]]
#
# Create a wrapper function for $command, sending output to `lolcat' (or
# `cowsay') transparently. To make this execute for all commands, automatically
# sending all output to lolcat, make this function the DEBUG trap.
#
# For example:
# 	trap lolexec DEBUG
#
# When trapped as the DEBUG routine in bash, any/all command lines that...
# 	+ Do NOT contain the pipe character
# 	+ Do NOT start with an exclamation point by-itself (e.g., "! top")
# ... will have their output sent to lolcat.
#
# NB: Requires awk(1) -- from base system
#
LOLEXEC='[ "$( trap -p DEBUG )" ] || trap "trap - DEBUG; lolexec" DEBUG'
alias -- +lol='trap - DEBUG; +lolcat'
alias -- -lol='-lolcat'
quietly unalias -lolcat
-lolcat()
{
	if [ $# -lt 1 -o ! "$*" ]; then
		unset PROMPT_COMMAND
		return $SUCCESS
	fi

	local cmd
	for cmd in "$@"; do
		_NOLOL_CMDS="$_NOLOL_CMDS|$cmd|$cmd[\$IFS]*"
	done
}
case "$UNAME_s" in
Darwin) -lol ls ;;
esac
quietly unalias +lolcat
+lolcat()
{
	PROMPT_COMMAND="$LOLEXEC"
	LOLCAT="${*:-$LOLCAT}"
}
quietly unalias lolexec
lolexec()
{
	builtin export OLDPWD # Required for `cd -' and `pushd -'
	builtin local _lolcat="$LOLCAT" _lc=
	builtin local _prog _register=1
	builtin local cmd fprog prefix=_${FUNCNAME}_ prog=
	case "$_LOLFUNC" in
	""|$FUNCNAME) builtin : fall through ;;
	*)
		builtin unset -f $_LOLFUNC 2> /dev/null
		builtin type -t $prefix$_LOLFUNC > /dev/null 2>&1 &&
			builtin eval "$( builtin type $prefix$_LOLFUNC |
				builtin command awk -v prefix=$prefix '
					NR == 2 { sub("^" prefix, ""); print }
					NR > 2
			' )"
	esac
	_LOLFUNC=
	case "$_lolcat" in
	"") builtin [ $# -eq 0 ] || "$@"
	    builtin return ;;
	*lolcat) _lc=1 ;;
	esac
	case "$#" in
	0) cmd=$( builtin history 1 ) || builtin return
	   cmd="${cmd#*[0-9]  }" ;;
	*) cmd="$*"
	esac
	builtin [ "$cmd" ] || builtin return
	builtin eval "case \"\$cmd\" in
	$_LOL_CMDS) builtin : fall through ;;
	$_NOLOL_CMDS)
		builtin [ \$# -eq 0 ] || \"\$@\"
		builtin return
	esac"
	for prog in $cmd; do
		builtin [ "$prog" = "!" ] || builtin break
		builtin [ $# -eq 0 ] || "$@"
		builtin return
	done
	case "$prog" in
	""|*[^[:alnum:]_]*)
		builtin [ $# -eq 0 ] || "$@"
		builtin return
	esac
	_prog=$prog
	while builtin [ "$( builtin type -t $prog 2> /dev/null )" = alias ]; do
		fprog=$( builtin type $prog )
		fprog="${fprog#*\`}"
		fprog="${fprog%\'}"
		for prog in $fprog; do
			builtin [ "$prog" = "!" ] || builtin break
			builtin [ $# -eq 0 ] || "$@"
			builtin return
		done
		builtin [ "$prog" = "$_prog" ] && builtin break # infinite loop
		_prog=$prog
	done
	if builtin [ "$prog" = "$FUNCNAME" -o ! "$prog" ]; then
		builtin [ $# -eq 0 ] || "$@"
		builtin return
	fi
	case "$( builtin type -t $prog 2> /dev/null )" in
	alias|file) # blacklist
		builtin eval "case \"\$prog\" in
		$_NOLOL_PROGS)
			builtin [ \$# -eq 0 ] || \"\$@\"
			builtin return
		esac"
		fprog="builtin command $prog"
		_LOLFUNC=$prog
		builtin [ "$_lc" ] && case "$prog" in
		*less|*more) # [bz,lz,xz,z]less [z]more
			builtin eval "function $prog()
			{
				$fprog \"\$@\" | $LOLCAT -f |
					builtin command less -R
			}"
			_register= ;;
		*ping*) # l2ping ping ping6
			_lolcat="$_lolcat -a" ;;
		tail) # check for `-f' or `-F'
			case "$cmd" in
			*[$IFS]-[fF]|*[$IFS]-[fF][$IFS]*) _lolcat="$_lolcat -a"
			esac ;;
		esac
		;;
	builtin|keyword) # whitelist
		builtin eval "case \"\$prog\" in
		$_LOL_BUILTINS) builtin : fall through ;;
		*)
			builtin [ \$# -eq 0 ] || \"\$@\"
			builtin return
		esac"
		fprog="builtin $prog"
		_LOLFUNC=$prog ;;
	function) # overlay
		builtin [ "$_lc" ] && case "$prog" in
		tlog|tm|taal|tael) # wrappers to `tail -F'
			_lolcat="$_lolcat -a" ;;
		esac
		builtin eval "$prefix$( builtin type $prog |
			builtin command awk 'NR>1' )"
		fprog="$prefix$prog"
		_LOLFUNC=$prog ;;
	*) # unknown
		builtin [ $# -eq 0 ] || "$@"
		builtin return
	esac
	builtin [ "$LOLDEBUG" ] && builtin echo \
		"$FUNCNAME: prog=[$prog] fprog=[$fprog] cmd=[$cmd]" >&2
	builtin [ "$_register" ] && builtin eval "function $prog()
	{
		exec 3>&1
		local cwd=\"\${D0:-\${DIRSTACK[0]:-\$( builtin pwd )}}\"
		builtin local meta=\"\$( exec 4>&1; (
			$fprog \"\$@\"
			builtin echo \"\$?:\$( builtin pwd )\" >&4
		) | $_lolcat >&3 )\"
		builtin local ret=\"\${meta%%:*}\" chdir=\"\${meta#*:}\"
		case \"$prog\" in
		dirstack_wrap) builtin \"\$@\" ;;
		pushd|popd) builtin $prog \"\$@\" ;;
		*) builtin [ \"\$cwd\" != \"\$chdir\" ] &&
			builtin cd \"\$chdir\"
		esac > /dev/null 2>&1
		builtin type dirstack2d > /dev/null 2>&1 &&
			builtin [ \"\$cwd\" != \"\$chdir\" ] && dirstack2d
		builtin return \$ret
	}"
	builtin [ $# -eq 0 ] || "$@"
}

if have cowsay; then
# cowsay
#
# Default arguments for cowsay
#
cowsay()
{
	local wrap=-n
	[ $# -gt 0 ] && wrap="-W $(( ${COLUMNS:-80} - 3 ))"
	command cowsay $wrap "$@"
}

# unicornsay
#
# Cows are boring
#
# NB: unicorn.cow obtained from https://github.com/schacon/cowsay/pull/13
# Special Thanks to: Sven Wittevrongel (@CupOfTea696)
#
unicornsay()
{
	local wrap=-n
	[ $# -gt 0 ] && wrap="-W $(( ${COLUMNS:-80} - 3 ))"
	command cowsay -f ~/bin/unicorn.cow $wrap "$@"
}
fi # have cowsay

if have cowthink; then
# cowthink
#
# Default arguments for cowthink
#
cowthink()
{
	local wrap=-n
	[ $# -gt 0 ] && wrap="-W $(( ${COLUMNS:-80} - 3 ))"
	command cowthink $wrap "$@"
}

# unicornthink
#
# Cows are boring
#
# NB: unicorn.cow obtained from https://github.com/schacon/cowsay/pull/13
# Special Thanks to: Sven Wittevrongel (@CupOfTea696)
#
unicornthink()
{
	local wrap=-n
	[ $# -gt 0 ] && wrap="-W $(( ${COLUMNS:-80} - 3 ))"
	command cowthink -f ~/bin/unicorn.cow $wrap "$@"
}
fi # have cowthink

# path_munge $entry ...
#
# Add $entry to $PATH if not already added.
#
quietly unalias path_munge
path_munge()
{
	while [ $# -gt 0 ]; do
		[ ! "$1" ] && shift && continue
		( PATH="$PATH:"
		  while [ "$PATH" ]; do
		  	[ "${PATH%%:*}" = "$1" ] && exit ${SUCCESS:-0}
		  	PATH="${PATH#*:}"
		  done
		  exit ${FAILURE:-1}
		) || export PATH="$PATH${PATH:+:}$1"
		shift
	done
}

# fprintf $fd $fmt [ $opts ... ]
#
# Like printf, except allows you to print to a specific file-descriptor. Useful
# for printing to stderr (fd=2) or some other known file-descriptor.
#
quietly unalias fprintf
fprintf()
{
	local fd=$1
	[ $# -gt 1 ] || return ${FAILURE:-1}
	shift 1
	printf "$@" >&$fd
}

quietly unalias eprintf
eprintf() { fprintf 2 "$@"; }

# pidof [-s] $process_name ...
#
# Print the pid(s) of each process matching the given name(s). If the `-s'
# option is passed, only the first matching pid is returned.
#
# NB: Requires ps(1) awk(1) -- from base system
#
quietly unalias pidof
pidof()
{
	local pids=
	local single=0

	local OPTIND flag
	while getopts s flag "$@"; do
		case "$flag" in
		s) single=1;;
		esac
	done
	shift $(( $OPTIND - 1 ))

	local process_name process_pids=
	while [ $# -gt 0 ]; do
		process_name="$1"
		process_pids=$(
			ps axo pid=,ucomm= |
			awk -v pattern="$process_name" \
			    -v single=$single \
			'
				BEGIN { space = "" }
				( $2 ~ pattern ) \
				{
					printf "%c%s", space, $1
					if ( single ) exit
					space = " "
				}
			'
		)
		pids="$pids${pids:+ }$process_pids"
		shift
	done

	[ "$pids" ] && echo "$pids"
}

# settitle $title ...
#
# Set the title of your ANSI-compatible shell window.
#
quietly unalias settitle
settitle()
{
	printf "\e]2;$*\a\e]1;$*\a\e]0;$*\a";
}

# ssh-agent [ssh-agent options]
#
# Override ``ssh-agent'' to call a function (you can always call the real
# binary by executing /usr/bin/ssh-agent) that launches a background ssh-agent
# that times-out in 30 minutes.
#
# Will evaluate the output of /usr/bin/ssh-agent (the real ssh-agent) called
# with either a known-secure set of arguments (if none are provided) or the
# unmodified arguments to this functin.
#
# Purpose is to prevent memorizing something like ``eval "$( ssh-agent ... )"''
# but instead simply ``ssh-agent [...]''.
#
# This allows you to, for example:
#
# 	ssh-agent
# 	: do some ssh-add
# 	: do some commits
# 	ssh-agent -k
# 	: or instead of ``ssh-agent -k'' just wait 30m for it to die
#
# NB: Requires /usr/bin/ssh-agent -- from base system
#
quietly unalias ssh-agent
ssh-agent()
{
	[ $# -gt 0 ] || set -- -t 1800
	eval "$( /usr/bin/ssh-agent "$@" )"
}

# ssh-agent-dup [-aqn]
#
# Connect to an open/active ssh-agent session available to the currently
# authenticated user. If more than one ssh-agent is available and the `-n' flag
# is not given, provide a menu list of open/active sessions available. Allows
# the user to quickly duplicate access to an ssh-agent launched in another
# interactive session on the same machine or for switching between agents.
#
# This allows you to, for example:
#
# 	(in shell session A)
# 	ssh-agent
# 	(in shell session B)
# 	ssh-agent-dup
# 	(now both sessions A and B can use the same agent)
#
# No menu is presented if only a single agent session is available (the open
# session is duplicated for the active shell session). If more than one agent
# is available, a menu is presented. The menu choice becomes the active agent.
#
# If `-a' is present, list all readable agent sockets, not just those owned by
# the currently logged-in user.
#
# If `-q' is present, do not list agent nor keys.
#
# If `-n' is present, run non-interactively (good for scripts; pedantic).
#
# NB: Requires dialog_menutag() dialog_menutag2help() eval2() have()
#     -- from this file
# NB: Requires awk(1) cat(1) grep(1) id(1) ls(1) ps(1) ssh-add(1) stat(1)
#     -- from base system
#
quietly unalias ssh-agent-dup
ssh-agent-dup()
{
	local list_all= quiet= interactive=1 noninteractive=
	local sockets=
	local owner socket socket_owner pid current_user

	local OPTIND=1 OPTARG flag
	while getopts anq flag; do
		case "$flag" in
		a) list_all=1 ;;
		n) noninteractive=1 interactive= ;;
		q) quiet=1 ;;
		\?|*)
			[ "$noninteractive" ] ||
				echo "$FUNCNAME [-aq]" | ${LOLCAT:-cat} >&2
			return ${FAILURE:-1}
		esac
	done
	shift $(( $OPTIND - 1 ))

	case "$UNAME_s" in
	*BSD) owner="-f%Su" ;;
	*) owner="-c%U"
	esac

	current_user=$( id -nu )
	for socket in /tmp/ssh-*/agent.[0-9]*; do
		# Must exist as a socket
		[ -S "$socket" ] || continue

		# Must end in numbers-only (after trailing dot)
		pid="${socket##*.}"
		[ "$pid" -a "$pid" = "${pid#*[!0-9]}" ] || continue
		pid=$(( $pid + 1 )) # socket num is one below agent PID

		# Must be a running pid and an ssh or ssh-agent
		kill -0 $pid 2> /dev/null || continue
		if ! [ "$( ps -p $pid -o ucomm= 2> /dev/null )" = ssh-agent ]
		then
			# This could be a forwarded agent
			pid=$(( $pid - 1 ))
			[ "$( ps -p $pid -o ucomm= 2> /dev/null )" = sshd ] ||
				continue
		fi

		# Must be owned by the current user unless `-a' is used
		# NB: When `-a' is used, the socket still has to be readable
		if [ ! "$list_all" ]; then
			socket_owner=$( stat $owner "$socket" 2> /dev/null ) ||
				continue
			[ "$socket_owner" = "$current_user" ] || continue
		fi

		sockets="$sockets $socket"
	done

	sockets="${sockets# }"
	if [ ! "$sockets" ]; then
		if [ ! "$noninteractive" ]; then
			local msg="$FUNCNAME: No agent sockets available"
			echo "$msg" | ${LOLCAT:-cat} >&2
		fi
		return ${FAILURE:-1}
	fi
	if [ "${sockets}" = "${sockets%% *}" ]; then
		# Only one socket found
		pid=$(( ${sockets##*.} + 1 ))
		if [ "$( ps -p $pid -o ucomm= 2> /dev/null )" = ssh-agent ]
		then
			eval${interactive:+2} export \
				SSH_AUTH_SOCK="$sockets" \
				SSH_AGENT_PID="$pid"
		else
			# This could be a forwarded agent
			pid=$(( $pid - 1 ))
			if [ "$( ps -p $pid -o ucomm= 2> /dev/null )" = sshd ]
			then
				eval${interactive:+2} export \
					SSH_AUTH_SOCK="$sockets" \
					SSH_AGENT_PID="$pid"
			else
				eval${interactive:+2} export \
					SSH_AUTH_SOCK="$sockets"
			fi
		fi
		[ "$SSH_AGENT_PID" -a ! "$quiet" ] && # show process
			[ "$interactive" ] &&
			ps -p "$SSH_AGENT_PID" | ${LOLCAT:-cat}
		# dump fingerprints from newly configured agent
		if ! [ "$quiet" -o "$noninteractive" ]; then
			echo "# NB: Use \`ssh-agent -k' to kill this agent"
			ssh-add -l
		fi | ${LOLCAT:-cat}
		return ${SUCCESS:-0}
	fi

	# There's more than one agent available
	[ "$noninteractive" ] && return ${FAILURE:-1}

	#
	# If we don't have dialog(1), just print the possible values
	#
	if ! have dialog; then
		local prefix="%3s"
		local fmt="$prefix %5s %-20s %s\n"
		local num=0 choice
		local identities nloaded

		sockets=$( command ls -tr $sockets ) # ascending order by age
		printf "$fmt" "" PID USER+NKEYS COMMAND
		for socket in $sockets; do
			num=$(( $num + 1 ))
			pid=$(( ${socket##*.} + 1 ))
			ucomm=$( ps -p $pid -o ucomm= 2> /dev/null )
			[ "$ucomm" = ssh-agent ] || pid=$(( $pid - 1 ))
			nkeys=0
			identities=$( SSH_AUTH_SOCK="$socket" ssh-add -l ) &&
				nkeys=$( echo "$identities" | grep -c . )
			printf "$fmt" $num: "$pid" \
				"$( ps -p $pid -o user= )"+"$nkeys" \
				"$( ps -p $pid -o command= )" | ${LOLCAT:-cat}
		done
		echo
		echo -n "Select a number [$num]: " | ${LOLCAT:-cat}
		read choice
		: ${choice:=$num}
		case "$choice" in
		""|*[!0-9]*)
			echo "$FUNCNAME: Invalid choice [$choice]" |
				${LOLCAT:-cat} >&2
			return ${FAILURE:-1} ;;
		esac
		if [ $choice -gt $num -o $choice -lt 1 ]; then
			echo "$FUNCNAME: Choice out of range [$choice]" |
				${LOLCAT:-cat} >&2
			return ${FAILURE:-1}
		fi
		set -- $sockets
		eval socket=\"\${$choice}\"

		pid=$(( ${socket##*.} + 1 ))
		if [ "$( ps -p $pid -o ucomm= 2> /dev/null )" = ssh-agent ]
		then
			eval2 export \
				SSH_AUTH_SOCK="$socket" \
				SSH_AGENT_PID="$pid"
		else
			# This could be a forwarded agent
			pid=$(( $pid - 1 ))
			if [ "$( ps -p $pid -o ucomm= 2> /dev/null )" = sshd ]
			then
				eval2 export \
					SSH_AUTH_SOCK="$socket" \
					SSH_AGENT_PID="$pid"
			else
				eval2 export SSH_AUTH_SOCK="$socket"
			fi
		fi
	else
		local menu_list=

		sockets=$( command ls -1t $sockets ) # descending order by age
		menu_list=$(
			echo "$sockets" | awk -v tags="$DIALOG_MENU_TAGS" '
			{
				if (++tagn > length(tags)) exit
				if (!match($0, /[[:digit:]]+$/)) next
				pid = substr($0, RSTART, RLENGTH) + 1
				cmd = sprintf("ps -p %u -o user=", pid)
				cmd | getline user
				close(cmd)
				cmd = sprintf("ps -p %u -o command=", pid)
				cmd | getline command
				close(cmd)
				nloaded = 0
				cmd = "SSH_AUTH_SOCK=" $0 " ssh-add -l"
				while (cmd | getline identity) {
					nloaded += identity ~ /^[[:digit:]]/
				}
				close(cmd)
				printf "'\'%s\'\ \'%s\'\ \'%s\''\n",
					substr(tags, tagn, 1),
					sprintf("pid %u %s+%u %s",
						pid, user, nloaded, command),
					sprintf("export %s %s",
						"SSH_AUTH_SOCK=" $0,
						"SSH_AGENT_PID=" pid)
			}'
		)

		local prompt="Pick an ssh-agent to duplicate (user+nkeys):"
		eval dialog \
			--clear --title "'$FUNCNAME'" --item-help \
			--menu "'$prompt'" 17 55 9 $menu_list \
			2> "$DIALOG_TMPDIR/dialog.menu.$$"
		local retval=$?

		# Return if "Cancel" was chosen (-1) or ESC was pressed (255)
		[ $retval -eq ${SUCCESS:-0} ] || return $retval

		local tag="$( dialog_menutag )"
		eval2 $( eval dialog_menutag2help "'$tag'" $menu_list )
	fi

	# Attempt to show the running agent
	[ "$SSH_AGENT_PID" -a ! "$quiet" ] &&
		ps -p "$SSH_AGENT_PID" | ${LOLCAT:-cat}

	# Attempt to dump fingerprints from newly configured agent
	if [ ! "$quiet" ]; then
		echo "# NB: Use \`$FUNCNAME' to select a different agent"
		echo "# NB: Use \`ssh-agent -k' to kill this agent"
		ssh-add -l
	fi | ${LOLCAT:-cat}
}

# openkey [-hv]
#
# Mounts my F.o thumb
#
# NB: Requires eprintf() have() -- from this file
# NB: Requires awk(1) df(1) id(1) mount(8) -- from base system
#
quietly unalias openkey
openkey()
{
	[ "$UNAME_s" = "FreeBSD" ] ||
		{ echo "$FUNCNAME: FreeBSD only!" >&2; return 1; }
	local OPTIND=1 OPTARG flag verbose= sudo=
	while getopts hv flag; do
		case "$flag" in
		v) verbose=1 ;;
		*) local optfmt="\t%-4s %s\n"
		   eprintf "Usage: $FUNCNAME [-hv]\n"
		   eprintf "OPTIONS:\n"
		   eprintf "$optfmt" "-h" \
		           "Print this text to stderr and return."
		   eprintf "$optfmt" "-v" \
		           "Print verbose debugging information."
		   return ${FAILURE:-1}
		esac
	done
	shift $(( $OPTIND - 1 ))
	if [ "$( id -u )" != "0" ]; then
		if have sr; then
			sudo=sr
		elif have sudo; then
			sudo=sudo
		fi || {
			eprintf "$FUNCNAME: not enough privileges\n"
			return ${FAILURE:-1}
		}
	fi
	df -l /mnt | awk '
		$NF == "/mnt" { exit found++ } END { exit !found }
	' || ${verbose:+eval2} $sudo mount /mnt || return
	local nfail=3
	while [ $nfail -gt 0 ]; do
		/mnt/mount.sh -d${verbose:+v} && break
		nfail=$(( $nfail - 1 ))
	done
	[ "$verbose" ] && df -hT /mnt/* | ( awk '
		NR == 1 { print > "/dev/stderr"; next } 1
	' | sort -u ) 2>&1
	return ${SUCCESS:-0}
}

# closekey [-ehv]
#
# Unmounts my F.o thumb
#
# NB: Requires eprintf() have() -- from this file
# NB: Requires awk(1) camcontrol(8) df(1) id(1) umount(8) -- from base system
#
quietly unalias closekey
closekey()
{
	local OPTIND=1 OPTARG flag eject= verbose= sudo=
	while getopts ehv flag; do
		case "$flag" in
		e) eject=1 ;;
		v) verbose=1 ;;
		*) local optfmt="\t%-4s %s\n"
		   eprintf "Usage: $FUNCNAME [-ehv]\n"
		   eprintf "OPTIONS:\n"
		   eprintf "$optfmt" "-e" \
		           "Eject USB media (using \`camcontrol eject')."
		   eprintf "$optfmt" "-h" \
		           "Print this text to stderr and return."
		   eprintf "$optfmt" "-v" \
		           "Print verbose debugging information."
		   return ${FAILURE:-1}
		esac
	done
	shift $(( $OPTIND - 1 ))
	if [ "$( id -u )" != "0" ]; then
		if have sr; then
			sudo=sr
		elif have sudo; then
			sudo=sudo
		fi || {
			eprintf "$FUNCNAME: not enough privileges\n"
			return ${FAILURE:-1}
		}
	fi
	[ ! -f "/mnt/umount.sh" ] ||
		${verbose:+eval2} /mnt/umount.sh ${verbose:+-v} || return
	[ ! "$eject" ] || daN=$( df -l /mnt | awk '
		$NF == "/mnt" && match($0, "^/dev/[[:alpha:]]+[[:digit:]]+") {
			print substr($0, 6, RLENGTH - 5)
			exit found++
		} END { exit ! found }
	' ) || daN=$(
		[ "$sudo" -a "$verbose" ] && echo $sudo camcontrol devlist >&2
		$sudo camcontrol devlist | awk '
		BEGIN {
			camfmt = "^<%s>[[:space:]]+[^(]*"

			disk[nfind = 0] = "da[[:digit:]]+"
			find[nfind++] = "USB Flash Disk 1100"

			#disk[nfind] = "device_pattern"
			#find[nfind++] = "model_pattern"
		}
		found = 0
		{
			for (n = 0; n < nfind; n++)
			{
				if (!match($0, sprintf(camfmt, find[n])))
					continue
				devicestr = substr($0, RSTART + RLENGTH + 1)
				gsub(/\).*/, "", devicestr)
				ndevs = split(devicestr, devices, /,/)
				for (d = 1; d <= ndevs; d++) {
					if (devices[d] !~ "^" disk[n] "$")
						continue
					found = 1
					break
				}
				if (found) break
			}
		}
		found && $0 = devices[d] { print; exit }
		END { exit !found }
	' ) || return
	! df -l /mnt | awk '$NF=="/mnt"{exit found++}END{exit !found}' ||
		${verbose:+eval2} $sudo umount /mnt || return
	[ "$eject" -a "$daN" ] &&
		${verbose:+eval2} $sudo camcontrol eject "$daN"
	return ${SUCCESS:-0}
}

# loadkeys [OPTIONS] [key ...]
#
# Load my SSH private keys from my F.o thumb. The `key' argument is to the
# SSH private keyfile's suffix; in example, "sf" for "id_rsa.sf" or "f.o" for
# "id_rsa.f.o" or "gh" for "id_rsa.gh".
#
# For example, to load the Sourceforge.net key, F.o key, and Github key:
# 	loadkeys sf f.o gh
#
# OPTIONS:
# 	-c           Close USB media after loading keys.
# 	-e           Close and eject USB media after loading keys.
# 	-h           Print this text to stderr and return.
# 	-k           Kill running ssh-agent(1) and launch new one.
# 	-n           Start a new ssh-agent, ignoring current one.
# 	-t timeout   Timeout. Only used if starting ssh-agent(1).
# 	-v           Print verbose debugging information.
#
# NB: Requires closekey() colorize() eprintf() openkey() quietly() ssh-agent()
#     ssh-agent-dup() -- from this file
# NB: Requires awk(1) kill(1) ps(1) ssh-add(1) -- from base system
#
quietly unalias loadkeys
loadkeys()
{
	local OPTIND=1 OPTARG flag close= eject= kill= new= timeout= verbose=
	while getopts cehknt:v flag; do
		case "$flag" in
		c) close=1 ;;
		e) close=1 eject=1 ;;
		k) kill=1 ;;
		n) new=1 ;;
		v) verbose=1 ;;
		t) timeout="$OPTARG" ;;
		*) local optfmt="\t%-12s %s\n"
		   eprintf "Usage: $FUNCNAME [OPTIONS] [key ...]\n"
		   eprintf "OPTIONS:\n"
		   eprintf "$optfmt" "-c" \
		           "Close USB media after loading keys."
		   eprintf "$optfmt" "-e" \
		           "Close and eject USB media after loading keys."
		   eprintf "$optfmt" "-h" \
		           "Print this text to stderr and return."
		   eprintf "$optfmt" "-k" \
		           "Kill running ssh-agent(1) and launch new one."
		   eprintf "$optfmt" "-n" \
		           "Start a new ssh-agent, ignoring current one."
		   eprintf "$optfmt" "-t timeout" \
		           "Timeout. Only used if starting ssh-agent(1)."
		   eprintf "$optfmt" "-v" \
		           "Print verbose debugging information."
		   return ${FAILURE:-1}
		esac
	done
	shift $(( $OPTIND - 1 ))
	[ "$kill" ] && quietly ssh-agent -k
	if [ "$new" ]; then
		ssh-agent ${timeout:+-t"$timeout"} ||
			return ${FAILURE:-1}
	elif quietly kill -0 "$SSH_AGENT_PID"; then
		: already running
	elif [ "$SSH_AUTH_SOCK" ] && quietly ssh-add -l; then
		eval2 export SSH_AGENT_PID=$( lsof -t -- $SSH_AUTH_SOCK )
	else
		if ! ssh-agent-dup -q; then
			ssh-agent ${timeout:+-t"$timeout"} ||
				return ${FAILURE:-1}
		fi
	fi
	ps -p "$SSH_AGENT_PID" || return ${FAILURE:-1}
	local suffix file show= load_required=
	[ $# -eq 0 ] && load_required=1
	for suffix in "$@"; do
		file="/mnt/keys/id_rsa.$suffix"
		ssh-add -l | awk -v file="$file" '
			gsub(/(^[0-9]+ [[:xdigit:]:]+ | \(.*\).*$)/, "") &&
				$0 == file { exit found++ }
			END { exit !found }
		' && show="$show${show:+|}$suffix" && continue # already loaded
		load_required=1
		break
	done
	ssh-add -l | colorize -c 36 "/mnt/keys/id_rsa\\.($show)([[:space:]]|$)"
	[ "$load_required" ] || return ${SUCCESS:-0}
	openkey ${verbose:+-v} || return ${FAILURE:-1}
	[ "$verbose" ] && ssh-add -l
	local loaded_new=
	if [ $# -gt 0 ]; then
		for suffix in "$@"; do
			file="/mnt/keys/id_rsa.$suffix"
			[ -f "$file" ] || continue
			ssh-add -l | awk -v file="$file" '
				gsub(/(^[0-9]+ [[:xdigit:]:]+ | \(.*\).*$)/,
					"") && $0 == file { exit found++ }
				END { exit !found }
			' && continue
			ssh-add "$file" || continue
			loaded_new=1
			show="$show${show:+|}$suffix"
		done
	else
		for file in /mnt/keys/id_rsa.*; do
			[ -e "$file" ] || continue
			[ "$file" != "${file%.[Pp][Uu][Bb]}" ] && continue
			ssh-add -l | awk -v file="$file" '
				gsub(/(^[0-9]+ [[:xdigit:]:]+ | \(.*\).*$)/,
					"") && $0 == file { exit found++ }
				END { exit !found }
			' && continue
			ssh-add "$file" || continue
			loaded_new=1
			show="$show${show:+|}${file#/mnt/keys/id_rsa.}"
		done
	fi
	[ "$close" ] && closekey ${verbose:+-v} ${eject:+-e}
	[ "$loaded_new" ] && ssh-add -l |
		colorize -c 36 "/mnt/keys/id_rsa\\.($show)([[:space:]]|$)"
}

# unloadkeys [OPTIONS] [key ...]
#
# Unload my SSH private keys from my F.o thumb. The `key' argument is to the
# SSH private keyfile's suffix; in example, "sf" for "id_rsa.sf" or "f.o" for
# "id_rsa.f.o" or "gh" for "id_rsa.gh".
#
# For example, to unload the Sourceforge.net key, F.o key, and Github key:
# 	unloadkeys sf f.o gh
#
# OPTIONS:
# 	-a           Unload all keys.
# 	-c           Close USB media after unloading keys.
# 	-e           Close and eject USB media after unloading keys.
# 	-h           Print this text to stderr and return.
# 	-v           Print verbose debugging information.
#
# NB: Requires closekey() colorize() eprintf() openkey() quietly()
#     -- from this file
# NB: Requires awk(1) ps(1) ssh-add(1) -- from base system
#
quietly unalias unloadkeys
unloadkeys()
{
	local OPTIND=1 OPTARG flag all= close= eject= verbose=
	while getopts acehv flag; do
		case "$flag" in
		a) all=1 ;;
		c) close=1 ;;
		e) close=1 eject=1 ;;
		v) verbose=1 ;;
		*) local optfmt="\t%-12s %s\n"
		   eprintf "Usage: $FUNCNAME [OPTIONS] [key ...]\n"
		   eprintf "OPTIONS:\n"
		   eprintf "$optfmt" "-a" "Unload all keys."
		   eprintf "$optfmt" "-c" \
		           "Close USB media after loading keys."
		   eprintf "$optfmt" "-e" \
		           "Close and eject USB media after loading keys."
		   eprintf "$optfmt" "-h" \
		           "Print this text to stderr and return."
		   eprintf "$optfmt" "-v" \
		           "Print verbose debugging information."
		   return ${FAILURE:-1}
		esac
	done
	shift $(( $OPTIND - 1 ))
	local suffix file show= unload_required=
	if [ "$all" ]; then
		unload_required=1
		shift $#
	fi
	for suffix in "$@"; do
		file="/mnt/keys/id_rsa.$suffix"
		ssh-add -l | awk -v file="$file" '
			gsub(/(^[0-9]+ [[:xdigit:]:]+ | \(.*\).*$)/, "") &&
				$0 == file { exit found++ }
			END { exit !found }
		' || continue # not loaded
		show="$show${show:+|}$suffix"
		unload_required=1
		break
	done
	ssh-add -l | colorize -c 31 "/mnt/keys/id_rsa\\.($show)([[:space:]]|$)"
	[ "$unload_required" ] || return ${SUCCESS:-0}
	openkey ${verbose:+-v} || return ${FAILURE:-1}
	[ "$verbose" ] && ssh-add -l
	if [ "$all" ]; then
		ssh-add -D
	else
		for suffix in "$@"; do
			file="/mnt/keys/id_rsa.$suffix"
			[ -f "$file" ] || continue
			ssh-add -l | awk -v file="$file" '
				gsub(/(^[0-9]+ [[:xdigit:]:]+ | \(.*\).*$)/,
					"") && $0 == file { exit found++ }
				END { exit !found }
			' || continue
			ssh-add -d "$file"
		done
	fi
	[ "$close" ] && closekey ${verbose:+-v} ${eject:+-e}
	[ "$all" ] || ssh-add -l |
		colorize -c 36 "/mnt/keys/id_rsa\\.($show)([[:space:]]|$)"
}

# zless [OPTIONS] $file ...
#
# Allow quick paging of gzip-compressed files. Any options passed in-addition
# to the filename pertain to zcat(1).
#
# NOTE: Some systems provide zless as an actual utility. For those systems,
# we'll fallback to the actual utility opposed to this function.
#
if ! have zless; then
	zless()
	{
		zcat "$@" | less
	}
fi

# zlog $logfile [$pattern]
#
# A general purpose log searching function. Given $log base pathname, this
# function will search for all occurrences of $pattern in $logfile and
# $logfile.N.gz [FreeBSD] or $logfile.N [Linux] (preserving reverse-chrono-
# logical order, searching oldest logfiles first).
#
# If $pattern is NULL or omitted, all lines are returned.
#
# NOTE: $pattern is an awk regular expression.
#
quietly unalias zlog
zlog()
{
	local logfile="$1" pattern="$2"
	local logfiles="$logfile" gzlogfiles="" n=0
	local sudo= needsudo=

	#
	# Check for two conditions where we would need sudo
	# 1. If the log file is not readable
	# 2. If the parent directory is not readable/traversable
	#
	[ -r "$logfile" ] || needsudo=1
	case "$logfile" in
	*/*) [ -r "${logfile%/*}" ] || needsudo=1;;
	esac

	if [ "$needsudo" ]; then
		if [ "$UNAME_s" = "Linux" -a "$Linux" = "Ubuntu" ]; then
			sudo=sudo
		else
			if have sr; then
				sudo=sr
			elif have sudo; then
				sudo=sudo
			fi
		fi
	fi

	case "$UNAME_s" in
	FreeBSD)
		#
		# FreeBSD rotates logs in the following fashion:
		#
		# FILENAME        DESCRIPTION
		# logfile         current log file
		# logfile.0.gz    most-recently rotated log file
		# logfile.1.gz    second-most recently rotated log file
		#
		local n=0
		if [ "$needsudo" ]; then
			while $sudo [ -f "$logfile.$n.gz" ]; do
				gzlogfiles="$logfile.$n.gz $gzlogfiles"
				n=$(( $n + 1 ))
			done
			if [ ! "$gzlogfiles" ]; then
				n=0
				while $sudo [ -f "$logfile.$n.bz2" ]; do
					gzlogfiles="$logfile.$n.bz2 $gzlogfiles"
					n=$(( $n + 1 ))
				done
			fi
		else
			while [ -f "$logfile.$n.gz" ]; do
				[ -r "$logfile.$n.gz" ] || needsudo=1
				gzlogfiles="$logfile.$n.gz $gzlogfiles"
				n=$(( $n + 1 ))
			done
			if [ ! "$gzlogfiles" ]; then
				n=0
				while $sudo [ -f "$logfile.$n.bz2" ]; do
					gzlogfiles="$logfile.$n.bz2 $gzlogfiles"
					n=$(( $n + 1 ))
				done
			fi
		fi
		;;
	Linux)
		#
		# Linux rotates logs in the following fashion:
		#
		# FILENAME     DESCRIPTION
		# logfile      current log file
		# logfile.1    most-recently rotated log file
		# logfile.2    second-most recently rotated log file
		#
		local n=1
		if [ "$needsudo" ]; then
			while $sudo [ -f "$logfile.$n" ]; do
				logfiles="$logfile.$n $logfiles"
				n=$(( $n + 1 ))
			done
		else
			while [ -f "$logfile.$n" ]; do
				[ -r "$logfile.$n" ] || needsudo=1
				logfiles="$logfile.$n $logfiles"
				n=$(( $n + 1 ))
			done
		fi
		;;
	esac

	if [ "$gzlogfiles" ]; then
		$sudo zcat $gzlogfiles \
			| sh -c "cat; cat $logfiles" \
			| awk "/$pattern/ {print}"
	else
		$sudo cat $logfiles \
			| awk "/$pattern/ {print}"
	fi
}

# zlog_summary $logfile [-d1,2,3] [$pattern]
#
# A wrapper around the zlog general purpose log searching function designed to
# provide a brief summary containing the following information:
# 	- The first line matching $pattern
# 	- The timestamp of the first occurrence
# 	- The timestamp of the last occurrence
# 	- How many occurrences were found
# 	  NOTE: Also counts "last message repeated N times" entries
#
# NOTE: If $pattern is NULL or omitted, the first/oldest log-entry is printed.
#       In addition, the timestamps in the summary will instead be the first
#       (oldest) and last (newest) entry, respectively. This serves as a sort
#       of brief look at the date-range spanning the individual log files.
#
# NOTE: $pattern is an awk regular expression.
#
# By default, it is assumed that the date/time fields to be displayed are the
# first, second, and third fields of each log entry, separated by whitespace.
# For example, /var/log/messages has the following first three fields:
#
# 	Jun  5 04:02:10
#
# However, not all logfiles display date/time entries in the above manner.
# Therefore, the following options can be passed to customize which line-fields
# (and how many) are displayed for the date/time:
#
# For example, the Apache error_log displays the date/time in five pieces:
#
# 	[Sun May 31 04:02:14 2009]
#
# So to display the proper values for date-range outputs when parsing the
# Apache error_log, you should pass the following parameters:
#
# 	-d1,2,3,4,5
#
# You can optionally pass `-dNF' to print the whole line.
#
# NB: Requires zlog() -- from this file
# NB: Requires awk(1) -- from base system
#
quietly unalias zlog_summary
zlog_summary()
{
	local logfile="$1"
	local datetime="1,2,3"

	shift 1 # logfile

	local OPTIND flag
	while getopts d: flag; do
		case "$flag" in
		d) datetime="$OPTARG";;
		esac
	done
	shift $(( $OPTIND - 1 ))

	local pattern="$1"
	zlog "$logfile" |
		awk -v pattern="$pattern" -v datetime="$datetime" '
	BEGIN {
		hr = sprintf("%-60s", "")
		gsub(" ", "-", hr)
		count = init = crepeat = 0
		last = ""
		ndt = split(datetime, dt, /,/)
	}
	{
		if ($0 !~ pattern && !crepeat) next
		else if (crepeat)
		{
			if ($0 !~ /repeated/) {
				crepeat = 0
				next
			}
			count += $8
			if (length(datetime) <= 0) next
			last = ""
			for (n = 1; n <= ndt; n++) last = sprintf("%s%s%s",
				last, (length(last) > 0 ? " " : ""), $dt[n])
			next
		}
		count++
		if (!init)
		{
			print hr
			print $0
			print hr
			printf(" 1st:")
			if (length(datetime) > 0)
				for (n = 1; n <= ndt; n++)
					printf(" %s", $dt[n])
			printf("\n")
			init = 1
		}
		crepeat = 1
		if (length(datetime) > 0) {
			last = ""
			for (n = 1; n <= ndt; n++) last = sprintf("%s%s%s",
				last, (length(last) > 0 ? " " : ""), $dt[n])
		}
	}
	END {
		print "Last: " last
		print "Total Occurrences: " count
		print hr
	}
	' # END-QUOTE
}

# zlog_usage [$FUNCNAME]
#
# Print usage statement for zlog_wrapper family of functions.
#
# NB: Requires eprintf() -- from this file
#
quietly unalias zlog_usage
zlog_usage()
{
	local func="${1:-$FUNCNAME}"
	local optfmt='\t%-12s %s\n'

	eprintf "Usage: %s [OPTIONS] [PATTERN]\n" "$func"
	eprintf "Arguments:\n"
	eprintf "$optfmt" "PATTERN" \
	        "Awk(1) regular expression pattern to search logfile for."

	eprintf "\n"

	eprintf "Options:\n"
	eprintf "$optfmt" "-h" \
	        "Print this usage statement and exit."
	eprintf "$optfmt" "-v" \
	        "Verbose. Show individual matching lines."
	eprintf "$optfmt" "-d1,2,3" \
	        "Comma-separated list of field numbers to display as the"
	eprintf "$optfmt" "" \
	        "date/time when providing summary output (ignored if \`-v'"
	eprintf "$optfmt" "" \
	        "is passed). Default is 1,2,3. A value of \`NF' prints the"
	eprintf "$optfmt" "" \
	        "entire line."

	eprintf "\n"

	return ${FAILURE:-1}
}

# zlog_wrapper $FUNCNAME $logfile [OPTIONS] [$pattern]
#
# A wrapper for the zlog/zlog_summary functions.
#
# NB: Requires zlog() zlog_summary() zlog_usage() -- from this file
#
quietly unalias zlog_wrapper
zlog_wrapper()
{
	local func="${1:-$FUNCNAME}"
	local logfile="$2"
	local datetime=
	local verbose=

	shift 2 # func/logfile

	local OPTIND flag
	while getopts d:hv flag; do
		case "$flag" in
		d) datetime="$OPTARG";;
		v) verbose=1;;
		h|\?)
			zlog_usage "$func"
			return ${FAILURE:-1}
			;;
		esac
	done
	shift $(( $OPTIND - 1 ))

	if [ "$verbose" ]; then
		zlog "$logfile" "$@"
	else
		zlog_summary "$logfile" ${datetime:+-d"$datetime"} "$@"
	fi
}

# tlog $log [OPTIONS]
#
# A general purpose wrapper to tail(1) that checks if the destination logfile
# is readable by you. If the file is readable by your effective UID, this
# command is equivalent to execute "tail -F [options] $log", otherwise one of
# sudo(8) or sr(8) is used to provide root-privileges to read the logfile.
#
# NB: Requires eval2() have() -- from this file
# NB: Requires tail(1) -- from base system
#
quietly unalias tlog
tlog()
{
	local logfile="$1"
	local sudo= needsudo=
	shift 1 # logfile

	#
	# Check for two conditions where we would need sudo
	# 1. If the log file is not readable
	# 2. If the parent directory is not readable/traversable
	#
	[ -r "$logfile" ] || needsudo=1
	case "$logfile" in
	*/*) [ -r "${logfile%/*}" ] || needsudo=1;;
	esac

	if [ "$needsudo" ]; then
		if have sr; then
			sudo=sr
		elif have sudo; then
			sudo=sudo
		fi
	fi

	eval2 ${needsudo:+$sudo} tail -F "$@" "$logfile"
}

# dialog_menutag
#
# Obtain the menutag chosen by the user from the most recently displayed
# dialog(1) menu and clean up any temporary files.
#
# NB: Requires quietly() -- from this file
# NB: Requires $DIALOG_TMPDIR -- from this file
# NB: Requires rm(1) -- from base system
#
quietly unalias dialog_menutag
dialog_menutag()
{
	local tmpfile="$DIALOG_TMPDIR/dialog.menu.$$"

	[ -f "$tmpfile" ] || return ${FAILURE:-1}

	cat "$tmpfile" 2> /dev/null
	quietly rm -f "$tmpfile"

	return ${SUCCESS:-0}
}

# dialog_menutag2item $tag_chosen $tag1 $item1 $tag2 $item2 ...
#
# To use the `--menu' option of dialog(1) you must pass an ordered list of
# tag/item pairs on the command-line. When the user selects a menu option the
# tag for that item is printed to stderr.
#
# This function allows you to dereference the tag chosen by the user back into
# the item associated with said tag.
#
# Pass the tag chosen by the user as the first argument, followed by the
# ordered list of tag/item pairs (HINT: use the same tag/item list as was
# passed to dialog(1) for consistency).
#
# If the tag cannot be found, NULL is returned.
#
quietly unalias dialog_menutag2item
dialog_menutag2item()
{
	local tag="$1" tagn item
	shift 1

	while [ $# -gt 0 ]; do
		tagn="$1"
		item="$2"
		shift 2

		if [ "$tag" = "$tagn" ]; then
			echo "$item"
			return ${SUCCESS:-0}
		fi
	done
	return ${FAILURE:-1}
}

# dialog_menutag2help $tag_chosen $tag1 $item1 $help1 \
#                                   $tag2 $item2 $help2
#
# To use the `--menu' option of dialog(1) with the `--item-help' option, you
# must pass an ordered list of tag/item/help triplets on the command-line. When
# the user selects a menu option the tag for that item is printed to stderr.
#
# This function allows you to dereference the tag chosen by the user back into
# the help associated with said tag (item is discarded/ignored).
#
# Pass the tag chosen by the user as the first argument, followed by the
# ordered list of tag/item/help triplets (HINT: use the same tag/item/help list
# as was passed to dialog(1) for consistency).
#
# If the tag cannot be found, NULL is returned.
#
dialog_menutag2help()
{
	local tag="$1" tagn help
	shift 1 # tag

	while [ $# -gt 0 ]; do
		tagn="$1"
		help="$3"
		shift 3 # tagn/item/help

		if [ "$tag" = "$tagn" ]; then
			echo "$help"
			return ${SUCCESS:-0}
		fi
	done
	return ${FAILURE:-1}
}

# cvspicker
#
# Display a menu of CVS servers to which you have access to. Selecting an entry
# configures your environment for access to said CVS server.
#
# NB: Requires dialog_menutag() dialog_menutag2help() -- from this file
# NB: Requires $CVSPICKER_EXPORTS $DIALOG_MENU_TAGS -- from this file
# NB: Requires awk(1) dialog(1) -- from base system
#
quietly unalias cvspicker
cvspicker()
{
	if ! have dialog; then
		echo "$CVSPICKER_EXPORTS"
		return ${FAILURE:-1}
	fi
	local menu_list
	menu_list=$(
		echo "$CVSPICKER_EXPORTS" |
		awk -v tags="$DIALOG_MENU_TAGS" '
		!/^[[:space:]]*(#|$)/ {
			if (++tagn > length(tags)) exit
			sub(/^[[:space:]]*/, "")
			sub(/[[:space:]]*$/, "")
			printf "'\''%s'\'' '\''%s'\''\n",
			       substr(tags, tagn, 1), $0
		}'
	)

	eval dialog \
		--clear --title "'CVSPicker'" \
		--menu "'Pick a CVS_RSH/CVSROOT pair:'" 17 55 9 \
		$menu_list \
		2> "$DIALOG_TMPDIR/dialog.menu.$$"
	local retval=$?

	# Return if "Cancel" was chosen (-1) or ESC was pressed (255)
	[ $retval -eq ${SUCCESS:-0} ] || return $retval

	local tag="$( dialog_menutag )"
	eval2 export $( eval dialog_menutag2item "'$tag'" $menu_list )
}

# hgrep ...
#
# Use in-place of grep(1) whenever you want to preserve the first line of
# output (often the header when viewing output from ps(1), lsof(8), df(1),
# netstat(8), arp(8), lsmod(8), route(8), free(1), and many many others).
#
# NB: Requires grep(1) head(1) -- from base system
#
quietly unalias hgrep
hgrep()
{
	local timeout=0

	[ "$BASH_VERSION" ] && timeout=1

	if [ $# -gt 1 ]; then
		local arg
		for arg in "$@"; do
			[ -f "$arg" ] && head -1 "$arg" && break
		done
	else
	(
		IFS=;
		read -r -t $timeout LINE
		printf "\e[1m%s\e[0m\n" "$LINE"
	)
	fi

	grep "$@"
}

# myshopts [OPTIONS]
#
# Configures/Displays your desired shopt(1) options. The following environment
# variables should be configured to enable/disable desired options:
#
# Options:
# 	-h   Print this usage statement and exit.
# 	-v   Verbose. Print the state of desired options regardless of
# 	     whether their states have changed or not. If passed twice,
# 	     (e.g., `-vv' or `-v -v') the values of each variable will
# 	     be printed while processing each for options.
#
# Environment:
# 	BASH234_SHOPT_ENABLE
# 	BASH234_SHOPT_DISABLE
# 		Each line represents a single option, with the first word on
# 		each line representing the option and the remainder of the
# 		line being comments you make up. A line can also be nothing
# 		but a comment if it begins with the pound-sign (#), after any
# 		length of whitespace.
#
# 		For each option, the option will either be disabled if it is
# 		added to the "*_DISABLE" variable or enabled if added to the
# 		"*_ENABLE" variable.
#
# 		If a variable is changed from its current state and you are
# 		logged in interactively, a line showing the current state of
# 		each option will be printed.
#
# 	BASH4_SHOPT_ENABLE
# 	BASH4_SHOPT_ENABLE
# 		These variables are like the above but are for options that
# 		are specific to version 4.x of bash(1). These options will
# 		not be enabled/disabled unless BASH_VERSION is 4.*, prevent-
# 		ing errors about unknown options in older versions of bash.
#
# NB: Requires eprintf() myshopts_usage() -- from this file
# NB: Requires awk(1) -- from base system
#
quietly unalias myshopts_usage
myshopts_usage()
{
	local optfmt='\t%-4s %s\n'
	local envfmt='\t%s\t%s\n'

	eprintf "Usage: %s [OPTIONS]\n" "$FUNCNAME"
	eprintf "Options:\n"
	eprintf "$optfmt" "-h" \
	        "Print this usage statement and exit."
	eprintf "$optfmt" "-v" \
	        "Verbose. Print the state of desired options regardless of"
	eprintf "$optfmt" "" \
	        "whether their states have changed or not. If passed twice,"
	eprintf "$optfmt" "" \
	        "(e.g., \`-vv' or \`-v -v') the values of each variable will"
	eprintf "$optfmt" "" \
	        "be printed while processing each for options."

	eprintf "\n"

	eprintf "Environment:\n"
	eprintf "$envfmt" "BASH234_SHOPT_ENABLE" ""
	eprintf "$envfmt" "BASH234_SHOPT_DISABLE" ""
	eprintf "$envfmt" "" \
	        "Each line represents a single option, with the first word on"
	eprintf "$envfmt" "" \
	        "each line representing the option and the remainder of the"
	eprintf "$envfmt" "" \
	        "line being comments you make up. A line can also be nothing"
	eprintf "$envfmt" "" \
	        "but a comment if it begins with the pound-sign (#), after any"
	eprintf "$envfmt" "" \
	        "length of whitespace."
	eprintf "\n"
	eprintf "$envfmt" "" \
	        "For each option, the option will either be disabled if it is"
	eprintf "$envfmt" "" \
	        'added to the "*_DISABLE" variable or enabled if added to the'
	eprintf "$envfmt" "" \
	        '"*_ENABLE" variable.'
	eprintf "\n"
	eprintf "$envfmt" "" \
	        "If a variable is changed from its current state and you are"
	eprintf "$envfmt" "" \
	        "logged in interactively, a line showing the current state of"
	eprintf "$envfmt" "" \
	        "each option will be printed."
	eprintf "\n"
	eprintf "$envfmt" "BASH4_SHOPT_ENABLE" ""
	eprintf "$envfmt" "BASH4_SHOPT_DISABLE" ""
	eprintf "$envfmt" "" \
	        "These variables are like the above but are for options that"
	eprintf "$envfmt" "" \
	        "are specific to version 4.x of bash(1). These options will"
	eprintf "$envfmt" "" \
	        "not be enabled/disabled unless BASH_VERSION is 4.*, prevent-"
	eprintf "$envfmt" "" \
	        "ing errors about unknown options in older versions of bash."

	return ${FAILURE:-1}
}
quietly unalias myshopts
myshopts()
{
	local verbose=
	local OPTIND flag
	while getopts hv flag; do
		case "$flag" in
		v) verbose=$(( ${verbose:-0} + 1 ));;
		h|\?) myshopts_usage
		      return ${FAILURE:-1} ;;
		esac
	done
	shift $(( $OPTIND - 1 ))

	#
	# Process user-defined environment variables into our options-lists.
	#
	local shopt_enable_list="" shopt_disable_list="" options_list var
	case "$BASH_VERSION" in
	2.*|3.*|4.*)
		#
		# Bash version 2, 3, or 4 shopt(1) variables to be ENABLED
		#
		options_list=$( echo "$BASH234_SHOPT_ENABLE" |
		                	awk '!/^[[:space:]]*($|#)/{print $1}' )
		if [ "$options_list" ]; then
			[ ${verbose:-0} -ge 2 ] &&
				eprintf "BASH234_SHOPT_ENABLE='%s'\n" \
				        "$BASH234_SHOPT_ENABLE"
			for var in $options_list; do
				shopt_enable_list="$shopt_enable_list $var"
			done
		fi

		#
		# Bash version 2, 3, or 4 shopt(1) variables to be DISABLED
		#
		options_list=$( echo "$BASH234_SHOPT_DISABLE" |
		                	awk '!/^[[:space:]]*(#|$)/{print $1}' )
		if [ "$options_list" ]; then
			[ ${verbose:-0} -ge 2 ] &&
				eprintf "BASH234_SHOPT_DISABLE='%s'\n" \
				        "$BASH234_SHOPT_DISABLE"
			for var in $options_list; do
				shopt_disable_list="$shopt_disable_list $var"
			done
		fi
	esac
	case "$BASH_VERSION" in
	4.*)
		#
		# Bash version 4 shopt(1) variables to be ENABLED
		#
		options_list=$( echo "$BASH4_SHOPT_ENABLE" |
		                	awk '!/^[[:space:]]*($|#)/{print $1}' )
		if [ "$options_list" ]; then
			[ ${verbose:-0} -ge 2 ] &&
				eprintf "BASH4_SHOPT_ENABLE='%s'\n" \
				        "$BASH4_SHOPT_ENABLE"
			for var in $options_list; do
				shopt_enable_list="$shopt_enable_list $var"
			done
		fi

		#
		# Bash version 4 shopt(1) variables to be DISABLED
		#
		options_list=$( echo "$BASH4_SHOPT_DISABLE" |
		                	awk '!/^[[:space:]]*($|#)/{print $1}' )
		if [ "$options_list" ]; then
			[ ${verbose:-0} -ge 2 ] &&
				eprintf "BASH4_SHOPT_DISABLE='%s'\n" \
				        "$BASH4_SHOPT_DISABLE"
			for var in $options_list; do
				shopt_disable_list="$shopt_disable_list $var"
			done
		fi
	esac

	#
	# Toggle the values of variables controlling optional shell behavior.
	# NOTE: These will be implicitly ignored for non-interactive sessions
	# NOTE: Capture the current value for later before changing the value
	#
	for var in $shopt_enable_list; do
		shopt -q $var
		eval local ${var}_before=$?
		shopt -s $var
	done
	for var in $shopt_disable_list; do
		shopt -q $var
		eval local ${var}_before=$?
		shopt -u $var
	done

	#
	# If we're running interactively (or verbosity is requested), show
	# which shell options have been enabled/disabled.
	#
	if [ "$interactive" -o "$verbose" ]; then
		for var in $shopt_enable_list $shopt_disable_list; do
			if [ "$verbose" ]; then
				if [ "$LOLCAT" ]; then
					shopt $var | $LOLCAT
				else
					shopt $var
				fi
			else
				# Report on values that changed state
				shopt -q $var
				if ! eval [ \$${var}_before -eq $? ]; then
					if [ "$LOLCAT" ]; then
						shopt $var | $LOLCAT
					else
						shopt $var
					fi
				fi
			fi
		done
	fi
}

#
# CVS functions
#
# NB: Requires cvs(1) file(1) find(1) xargs(1) -- from base system
#
quietly unalias cvs_admin_binaries
cvs_admin_binaries()
{
	local recursive= undo=
	local gnu_xargs=

	case "$UNAME_s" in
	Linux|CYGWIN*) gnu_xargs=1;;
	esac

	local OPTIND flag
	while getopts ru flag; do
		case "$flag" in
		r) recursive=1;;
		u) undo=1;;
		esac
	done
	shift $(( $OPTIND - 1 ))

	( if [ "$recursive" ]; then
	  	find . -type f ! -path '*/CVS/*'
	  else
	  	find . -mindepth 1 -maxdepth 1 -type f
	  fi
	) | (
		while read file; do
			filetype=$( file -b "$file" )
			case "$filetype" in
			*ICC*|*ELF*|*data*|*font*|*icon*|*image*|*archive*)
				echo "$file";;
			esac
		done
	) | (
		if [ "$undo" ]; then
			xargs ${gnu_xargs:+-r} cvs admin -kkv
		else
			xargs ${gnu_xargs:+-r} cvs admin -ko
		fi
	)
}
alias cvs_admin_binaries_undo='cvs_admin_binaries -u'
quietly unalias cvs_add_recursive
cvs_add_recursive()
{
	local gnu_xargs=

	case "$UNAME_s" in
	Linux|CYGWIN*) gnu_xargs=1;;
	esac

	( [ $# -gt 0 ] || set -- .
	  while [ $# -gt 0 ]; do
	  	find "$1" -type d
	  	shift 1
	  done
	) | (
		while read dir; do
			# Skip invalid directories
			case "$dir" in
			.) continue;;
			*) [ "${dir##*/}" != "CVS" ] || continue;;
			esac

			# Add the directory
			cvs add "$dir"

			# Add regular files from this directory
			find "$dir" -type f -mindepth 1 -maxdepth 1 |
				xargs ${gnu_xargs:+-r} cvs add
		done
	)
}
quietly unalias cvs_rm_recursive
cvs_rm_recursive()
{
	local gnu_xargs=

	case "$UNAME_s" in
	Linux|CYGWIN*) gnu_xargs=1;;
	esac

	( [ $# -gt 0 ] || set -- .
	  while [ $# -gt 0 ]; do
	  	find "$1" -type d
	  	shift 1
	  done
	) | (
		while read dir; do
			# Skip invalid directories
			case "$dir" in
			.) continue;;
			*) [ "${dir##*/}" != "CVS" ] || continue;;
			esac

			# Remove regular files from this directory
			find "$dir" -type f -mindepth 1 -maxdepth 1 |
				xargs ${gnu_xargs:+-r} cvs rm -f
		done
	)
}

# dirstack2d
#
# Create a series of $D# environment variables based on ${DIRSTACK[#]} array
# items -- for quick[er] access to directories in the stack.
#
# NB: Requires awk(1) -- from base system
#
quietly unalias dirstack2d
dirstack2d()
{
	# Unset current $D# variables
	unset $( set | command awk -F= '/^D[[:digit:]]+=/{print $1}' )

	#
	# Recreate $D# variables from $DIRSTACK array
	#
	local n=0
	while [ $n -lt ${#DIRSTACK[@]} ]; do
		eval D$n=\"\${DIRSTACK[$n]}\"

		# Expand leading ~ to $HOME
		case "${DIRSTACK[$n]}" in
		\~|\~/*)
			eval D$n=\"\${D$n\#?}\"
			eval D$n=\"\$HOME\$D$n\"
		esac

		n=$(( $n + 1 ))
	done
}

# dirstack_wrap builtin [options]
#
# Simple wrapper to execute dirstack2d after forking to some builtin.
#
# NB: Requires dirstack2d() -- from this file
#
quietly unalias dirstack_wrap
dirstack_wrap()
{
	local bi="$1"
	shift
	builtin $bi "$@"
	dirstack2d
}

#
# Dirstack wrapper aliases
# NB: Requires dirstack_wrap() -- from this file
#
alias pushd='dirstack_wrap pushd'
alias  popd='dirstack_wrap popd'
alias  dirs='dirstack_wrap dirs'
alias    cd='dirstack_wrap cd'

# colorize [-c ANSI] pattern
#
# Colorize text matching pattern with ANSI sequence (default is `31;1' for red-
# bold). Non-matching lines are printed as-is.
#
# NB: Requires awk(1) -- from base system
#
colorize()
{
	local OPTIND=1 OPTARG flag
	local ansi_color=

	while getopts c: flag; do
		case "$flag" in
		c) ansi_color="$OPTARG" ;;
		esac
	done
	shift $(( $OPTIND - 1 ))

	awk -v color="${ansi_color:-31;1}" -v pattern="$1" '
		(match($0, pattern) && $0 = sprintf("%s%c[%sm%s%c[0m%s",
			substr($0, 0, RSTART - 1), 27, color,
			substr($0, RSTART, RLENGTH), 27,
			substr($0, RSTART + RLENGTH))) || 1
	' # END-QUOTE
}

# pident $directory
#
# Print ident strings from any/all files found in $directory.
#
quietly unalias pident
pident()
{
	while [ $# -gt 0 ]; do
		find "$1" -type f ! -path './.svn/*' | xargs ident -q |
			awk '/^[^ ]/&&(p=$0)&&getline&&!/^$/&&$0=p"\n"$0;/^ /'
		shift
	done
}

#
# Perforce helpers
#
# NB: Requires devel/p4 -- from ports collection
# NB: Requires awk(1) find(1) sed(1) -- from base system
#
quietly unalias p4up
p4up()
{
	find "${1:-.}" -not -type d |
		sed -e 's/%/%25/g;s/@/%40/g;s/#/%23/g;s/\*/%2A/g' |
		p4 -x - have 2>&1 |
		awk '/not on client/&&sub(/ - .*/,"")' |
                p4 -x - opened 2>&1 |
                awk '/not opened on this client/&&sub(/ - .*/,"")' |
		sed -e 's/%40/@/g;s/%23/#/g;s/%2A/*/g;s/%25/%/g' |
		sed -e 's/[[:space:]"'\''\\]/\\&/g'
}
quietly unalias p4changes2
p4changes2()
{
	local verbose=
	[ "$1" = "-v" ] && verbose=1 && shift 1
	[ "$1" ] || return 1
	local cmd="p4 changes \"//...@>$1,@<=${2:-now}\""
	[ "$verbose" ] && cmd="$cmd | awk '{system(\"p4 describe -s \" \$2)}'"
	echo ">>> $cmd" && eval "$cmd"
}
quietly unalias p4getrevs
p4getrevs()
{
	local func=p4getrevs OPTIND=1 OPTARG flag
	local allrevs=1 revs= get_info= quiet=
	while getopts iqR: flag; do
		case "$flag" in
		i) get_info=1 ;;
		q) quiet=1 ;;
		R) revs="$OPTARG" allrevs= ;;
		esac
	done
	shift $(( $OPTIND - 1 ))
	if [ $# -lt 1 ] || ! [ "$revs" -o "$allrevs" ]; then
		echo "Usage: $func [-i] [-q] [-R rev[,...]] file ..." >&2
		return ${FAILURE:-1}
	fi
	local file p4path change
	for file in "$@"; do
		p4path=$( p4 where "$file" | awk '$0 = $1' ) || continue
		[ "$allrevs" ] && revs=$( p4 filelog "$file" | awk '
			$1 == "..." && $2 ~ /^#[[:digit:]]+$/ {
				revs = substr($2,2) "," revs
			}
			END { sub(/,$/,"",revs); print revs }
		' )
		echo -n "$revs" | awk 'gsub(/,/, " ")||1' | xargs -rn1 sh -c '
			info_getter="$1" file="$2" p4path="$3"
			get_info="$4" quiet="$5" revnum="$6"
			output_file="${file##*/}.$revnum"
			info_file="${output_file}-info"
			file_url="http://perforce/@@$p4path?rev1=$revnum"
			have() { type "$@" > /dev/null 2>&1; }
			get_revision() {
				local cmd=:
				if have fetch; then
					cmd="fetch ${quiet:+-q} -o"
				elif have wget; then
					cmd="wget ${quiet:+-q} -O"
				elif have curl; then
					cmd="curl ${quiet:+-s} -o"
				fi
				eval $cmd \"\$output_file\" \"\$file_url\"
			}
			get_info() {
				local change
				change=$( p4 filelog "$file#$revnum" |
					awk "$info_getter" ) || return
				p4 describe -s "$change" | awk "
					sub(/^\t+/, \"\"),sub(/^\t+/,\"\")||1
				" > "$info_file"
			}
			[ -e "$output_file" ] || get_revision
			[ "$get_info" -a ! -e "$info_file" ] && get_info
		' /bin/sh '
			$1$3 == "...change" && $4 ~ /^[[:digit:]]+$/ {
				print $4; exit
			}
		' "$file" "$p4path" "$get_info" "$quiet"
	done
}

#
# route(8) helpers
#
routes_awk='
BEGIN {
	columns["host"] = 0
	domain_len = length(domain)
}
!/^[[:space:]]*(#|$)/ &&
	match($0, /^[^[:space:]]+[[:space:]]+[^[:space:]]/) &&
	n = split(substr($0, RSTART + RLENGTH - 1), hosts) &&
1 {
	addr = $1
	for (i = 1; i <= n; i++)
	{
		host = hosts[i]
		host_len = length(host)
		if (host_len <= domain_len) next
		host_domain = substr(host, host_len - domain_len + 1)
		if (host_domain != domain) next

		short_host_len = host_len - domain_len
		if (short_host_len > columns["host"])
			columns["host"] = short_host_len
		cmd = sprintf("%s route %s %s %s 2>&1",
			sudo, route_action, addr, route_args)
		output = ""
		lines  = 0
		while (cmd | getline input) {
			lines++
			output = sprintf("%s%s%s",
				output, output ? "\n" : "", input)
		}
		close(cmd)
		short_hosts[addr] = substr(host, 1, short_host_len)
		full_hosts[addr] = host
		cmd_output[addr] = output
		cmd_lines[addr] = lines
		next
	}
}
END {
	for (addr in cmd_output) {
		short_host = short_hosts[addr]
		host = full_hosts[addr]
		output = cmd_output[addr]
		lines = cmd_lines[addr]
		if (lines <= 1) {
			sub(/^[[:space:]]*/, "", output)
			printf "%-*s => %s\n", columns["host"],
				short_host, output
		} else {
			printf "%s:\n", host
			gsub(/\n/, "&\t", output)
			printf "\t%s\n", output
		}
	}
}
'
routes()
{
	local usage="$FUNCNAME %s {add|up|down|delete|reload|show|get}\n"
	local hosts_file="${ETC_HOSTS:-${HOSTS:-/etc/hosts}}"
	local name="$1" action="$2" _name domain route_args
	[ "$name" -a "$action" ] || {
		printf "Usage: $usage" "${name:-name}" >&2
		return $FAILURE
	}
	_name=$( echo "${name%%[!a-zA-Z0-9_]*}" | tr '[:lower:]' '[:upper:]' )
	eval routes=\"\$ROUTES_$_name\"
	[ "$routes" ] || {
		printf "%s:%u: variable ROUTES_%s unset or NULL!\n" \
			"$FUNCNAME" "$LINENO" "$_name" >&2
		return $FAILURE
	}
	read domain route_args <<-EOF
	$routes
	EOF
	case "$action" in
	[Uu][Pp]|[Aa][Dd][Dd])
		awk -v sudo="sr" \
			-v route_action="add" -v route_args="$route_args" \
			-v domain="$domain" "$routes_awk" "$hosts_file" ;;
	[Dd][Oo][Ww][Nn]|[Dd][Ee][Ll][Ee][Tt][Ee])
		awk -v sudo="sr" -v route_action="delete" -v route_args="" \
			-v domain="$domain" "$routes_awk" "$hosts_file" ;;
	[Rr][Ee][Ll][Oo][Aa][Dd])
		$FUNCNAME $_name down && $FUNCNAME $_name up ;;
	[Ss][Hh][Oo][Ww]|[Gg][Ee][Tt])
		awk -v route_action="-nv get" -v route_args="| tail -1" \
			-v domain="$domain" "$routes_awk" "$hosts_file" ;;
	*)
		printf "Usage: $usage" "${name:-name}" >&2
		return $FAILURE
	esac
}

############################################################ SHELL BEHAVIOR

#
# Set prompt style
#
case "$HOSTNAME" in
*.*.*.*) export HOSTID="${HOSTNAME%.*.*}" ;;
*.*.*) export HOSTID="${HOSTNAME%%.*}" ;;
*) export HOSTID="$HOSTNAME"
esac
export PS1='\[\e[32;1m\]$USER@$HOSTID \[\e[34m\]\W \$\[\e[0m\] '
[ "$interactive" ] && PROMPT_COMMAND="$LOLEXEC"

#
# lolcat integration
#
have lolcat && LOLCAT=lolcat

#
# If we became root via sudo(8), preserve $HOME
#
[ "$SUDO_USER" != "root" ] && export HOME="$( eval echo ~$SUDO_USER )"

#
# Set the window title for terminals such as "xterm", "rxvt", and others
#
[ "$interactive" ] \
	&& [ "$TERM" != "linux" ] \
	&& [ "$TERM" != "cons25" ] \
	&& settitle "$USER@${HOSTNAME%.*.*}" "${PWD/#$HOME/~}"

#
# Fixup $PATH
#
export PATH="$HOME/bin:$HOME/Desktop/bin${PATH:+:}$PATH"
path_munge /sbin /usr/sbin /usr/local/sbin /usr/games

# Fixup $PATH on Mac OS X Lion
[ -d "/Developer/usr/bin" ] && path_munge /Developer/usr/bin

# Latest Apple Developer Tools move cvs once-again, this time
# XCode 4.3.1 places cvs inside itself -- March 17th, 2012
[ -f "/Applications/Xcode.app/Contents/Developer/usr/bin/cvs" ] &&
	path_munge /Applications/Xcode.app/Contents/Developer/usr/bin

#
# Make new files group-writable by default
#
umask 002

#
# shopt(1) options to enable/disable.
#
BASH234_SHOPT_ENABLE='
	cdspell        inline correction of minor errors in cd
	checkwinsize   live tracking of $COLUMNS and $LINES
	histreedit     editing of a failed history expansion
	histverify     inline history expansion
'
BASH234_SHOPT_DISABLE='
	# none to report at this time
'
BASH4_SHOPT_ENABLE='
	dirspell     directory spell correction on tab-completion
'
BASH4_SHOPT_DISABLE='
	# none to report at this time
'
myshopts # see `myshopts -h'

############################################################ ALIASES

# timecat
#
# Used for timing adhoc duties or timing cat operations.
#
alias tc="time cat"
alias timecat="time cat"

# stuff | dim
#
# Make stdout appear "dimmed" in the terminal, allowing errors to stand out.
#
alias dim="sed -e s/^/$'\e'[2m/\;s/\$/$'\e'[0m/"

#
# zlog_wrapper aliases (see zlog_wrapper in FUNCTIONS above)
#
# !!!the below usage statement is generated from zlog_usage above!!!
# Usage: ALIAS [OPTIONS] [PATTERN]
# Arguments:
# 	PATTERN    Awk(1) regular expression pattern to search logfile for.
#
# Options:
# 	-h         Print this usage statement and exit.
# 	-v         Verbose. Show individual matching lines.
# 	-d1,2,3    Comma-separated list of field numbers to display as the
# 	           date/time when providing summary output (ignored if `-v'
# 	           is passed). Default is 1,2,3. A value of `NF' prints the
# 	           entire line.
#
alias messages='zlog_wrapper messages "$MESSAGES"'
alias aelog='zlog_wrapper aelog "$APACHE_ERROR_LOG" -d1,2,3,4,5'
alias aalog='zlog_wrapper aalog "$APACHE_ACCESS_LOG" -d4,5,1'
alias aelog_sa='zlog_wrapper aelog_sa "$APACHE_ERROR_LOG_SA" -d1,2,3,4,5'
alias aalog_sa='zlog_wrapper aalog_sa "$APACHE_ACCESS_LOG_SA" -d4,5,1'
alias aselog='zlog_wrapper aselog "$APACHE_SSL_ERROR_LOG" -d1,2,3,4,5'
alias asalog='zlog_wrapper asalog "$APACHE_SSL_ACCESS_LOG" -d4,5,1'

#
# tlog aliases (see tlog in FUNCTIONS above)
#
# Usage: ALIAS [OPTIONS]
# Options are sent to tail(1).
#
alias tm='tlog "$MESSAGES"'
alias tael='tlog "$APACHE_ERROR_LOG"'
alias taal='tlog "$APACHE_ACCESS_LOG"'
alias tael_sa='tlog "$APACHE_ERROR_LOG_SA"'
alias taal_sa='tlog "$APACHE_ACCESS_LOG_SA"'
alias tasel='tlog "$APACHE_SSL_ERROR_LOG"'
alias tasal='tlog "$APACHE_SSL_ACCESS_LOG"'

#
# Random keepalive [ka] alias (based on system-availability of various
# fun utilities).
#
declare -a ka=()
have worms   && ka[${#ka[@]}]="worms -d 25"
have rain    && ka[${#ka[@]}]="rain -d 50"
have grdc    && ka[${#ka[@]}]="grdc"
have fortune && ka[${#ka[@]}]="
	( stty -kerninfo
	  trap 'kill -9 \$sleeppid' SIGINFO
	  trap exit SIGINT
	  while :;do
	  	case \"\$LOLCAT\" in
	  	*lolcat) fortune |
	  		sed -e 's/^[[:space:]]\{1,\}/ /' | \$LOLCAT -a ;;
	  	*) fortune | \${LOLCAT:-cat}
	  	esac
	  	sleep 80 & sleeppid=\$!
	  	wait \$sleeppid
	  	echo --
	  done
	) 2> /dev/null"
[ ${#ka[@]} -gt 0 ] || ka[0]="while :;do echo -n .;sleep 80;done"
alias ka='eval "${ka[$(($RANDOM/(32769/${#ka[@]})))]}"'

#
# dbcat alias for reading Berkeley DB database files (based on system-
# availability of standard utilities).
#
dbutil=false
dbflags=
if have "db_dump"; then
	# RHEL/CentOS/Cygwin
	dbutil="db_dump"
	db185util="db_dump185"
	dbflags="-p"
elif have "db_dump-4.6"; then
	# FreeBSD-8.x
	dbutil="db_dump-4.6"
	db185util="db_dump185-4.6"
	dbflags="-p"
elif have "db_dump-4.2"; then
	# FreeBSD-4.11
	dbutil="db_dump-4.2"
	db185util="db_dump185-4.2"
	dbflags="-p"
elif have "db3_dump"; then
	# FreeBSD-4.8
	dbutil="db3_dump"
	db185util="db3_dump185"
	dbflags="-p"
elif have "db"; then
	# Legacy
	dbutil="db"
fi
alias dbcat="/bin/sh -c '"'
	while [ $# -gt 0 ]; do
		dbfile="$1"
		shift
		type=$( file "$dbfile" ) || continue
		echo "$type" | awk \
			-v dbfile="$dbfile" \
			-v dbutil="'"$dbutil"'" \
			-v db185util="'"$db185util"'" \
			-v dbflags="'"$dbflags${dbflags:+ }"'" \
		'\'\\\'\''
			BEGIN { isdb = 0 }
			/: Berkeley DB / \
			{
				isdb = 1
				if ( dbutil == "db" ) {
					sub(/.*\(/, "")
					sub(/,.*/, "")
					dbutil = dbutil " " tolower($0)
				}
				else if ( $0 ~ /1.8[45]/ )
					dbutil = db185util
				system(dbutil " " dbflags dbfile)
			}
			END { exit ! isdb }
		'\'\\\'\''
	done'\'" -- /bin/sh"

#
# Alias for becoming root while maintaining shell customizations.
#
if have sr; then
	alias srsu="sr env $SHELL --rcfile $HOME/.bash_profile"
elif have sudo; then
	alias srsu="sudo env $SHELL --rcfile $HOME/.bash_profile"
elif have su; then
	alias srsu="su -m root --rcfile $HOME/.bash_profile"
fi

#
# Editor settings (in order of preference)
#
if have vim; then
	export EDITOR=vim
	alias vi='vim'
elif have vi; then
	export EDITOR=vi
elif have ee; then
	export EDITOR=ee
elif have nano; then
	export EDITOR=nano
elif have pico; then
	export EDITOR=pico
elif have emacs; then
	export EDITOR=emacs
fi
alias edit="$EDITOR"

#
# Alias for editing a file as root maintaining vi customizations
#
if have sr; then
	alias srvi='sr env HOME="$HOME" $EDITOR'
elif have sudo; then
	alias srvi='sudo env HOME="$HOME" $EDITOR'
fi

#
# Alias for machines without sr
#
have sr || alias sr="sudo"

#
# Forthisms
#
# I was spending a lot of time in Forth while writing loader_menu for FreeBSD,
# and couldn't stop myself from using these to no-end of pain. So I decided to
# add the below to ease my pain.
#
alias .s="ls"

#
# View man(1) pages with vim(1)
#
have vim && alias man="man -P \"col -b | vim -c 'set ft=man nomod nolist' -\""

#
# View man(1) pages with lolcat
#
have lolcat && alias lolman="man -P 'col -b | lolcat -f | less -R'"

#
# diff-specific vim(1) alias
#
quietly unalias dview
alias dview="vim +/'^\\(Index\\|diff\\|--\\|@@\\|====\\).*' +'set hls' +'set syntax=diff' -R"

#
# Abbreviations
#
alias b=basename
alias d=date
alias di='dirs -v'
alias h=history
alias j=jobs
alias la='ls -al'
alias ll='ls -l'
alias lt='ls -Altr'
alias pd=pushd
alias po=popd
alias z=suspend
alias bx=BitchX

#
# OS-specific Aliases/functions (until they can be generalized)
#
case "$UNAME_s" in
Linux|CYGWIN*)
	#
	# Force sort(1) to NOT ignore leading dots
	# NB: Is Linux brain-dead? or just high on crack?
	#
	export LC_COLLATE=C

	#
	# Make ls(1) colorful (customized by $HOME/.dir_colors)
	#
	alias ls='ls --color=tty -ACF'

	#
	# X-related aliases
	#
	alias Xsession="/etc/X11/xdm/Xsession"
	alias xsession="Xsession"
	;;
FreeBSD|Darwin)
	#
	# boottime: Prints the date/time (format based on current locale)
	#           that the system booted (resolution is 1-second).
	# bootsec:  Prints the number of seconds since the epoch (see `%s'
	#           format in date(1)) that was recorded during boot,
	#           allowing you to use the `-r seconds' syntax of date(1)
	#           to represent the boottime in any desired format.
	#
	kbt="sysctl kern.boottime"
	alias boottime="$kbt | sed -e 's/.*} //'"
	alias bootsec="$kbt | sed -e 's/.* sec = \([0-9]\{1,\}\).*/\1/'"
	unset kbt

	#
	# bootfile: Prints the path to the running kernel.
	#
	alias bootfile="sysctl -n kern.bootfile"

	#
	# Make ls(1) colorful (customized by $LSCOLORS -- see ls(1))
	#
	alias ls="ls -AGCF"
	export LSCOLORS="Exfxcxdxbxegedabagacad"

	#
	# X-related aliases
	#
	alias Xsession="/usr/X11R6/lib/X11/xdm/Xsession"
	alias xsession="Xsession"

	#
	# ps(1) related
	#
	pst()
	{
		ps axo lstart,pid,tt,stat,time,command "$@" | awk '
		BEGIN {
			(cmd = "date +%c") | getline cdate
			clen = length(cdate)
			close(cmd)
		}
		!/^-/ {
			if ($1 != (sdate = "STARTED"))
			{
				cdate = substr($0, 0, clen)
				cmd = "date -j -f %c \"" cdate "\" +%s"
				cmd | getline sdate
				close(cmd)
			}
			printf "%-10s %s\n", sdate, substr($0, clen + 1)
		}'
	}
	;;
esac

#
# ssh(1) helpers
#
alias sshk="ssh -t scribe9 . .bash_profile '&&' ssh -At tink ssh -At localhost ssh -A"

############################################################ MISCELLANEOUS

#
# cvs(1) settings
#
export CVS_RSH=ssh CVSROOT=cvs.vicor.com:/repos/projects

#
# BitchX(1) settings
#
export IRCNICK=dteske
#export IRCSERVER=frenode.net
export IRCSERVER=irc.prison.net
export IRCNAME=dteske

#
# jail_build(8) settings
#
export JAIL_BUILD_DESTDIR=/raid1/jails

#
# Override the default password prompt for sudo(8). This helps differentiate
# the sudo(8) password prompt from others such as su(1), ssh(1), and login(1).
#
export SUDO_PROMPT='[sudo] Password:'

#
# Manual pages with color
#
export LESS_TERMCAP_mb=$'\e[31m'      # turn on blinking
export LESS_TERMCAP_md=$'\e[31m'      # turn on bold (extra bright)
export LESS_TERMCAP_me=$'\e[m'        # turn off all attributes
export LESS_TERMCAP_so=$'\e[1;33;44m' # begin standout mode
export LESS_TERMCAP_se=$'\e[m'        # exit standout mode
export LESS_TERMCAP_us=$'\e[1;34m'    # begin underline mode
export LESS_TERMCAP_ue=$'\e[m'        # exit underline mode

#
# Quietly attach to running ssh-agent(1) unless agent already given
#
if [ ! "$SSH_AUTH_SOCK" ]; then
	if [ "$interactive" ]; then
		ssh-agent-dup
	else
		quietly ssh-agent-dup -n || :
	fi
fi

################################################################################
# END
################################################################################
