#!/usr/bin/perl
use strict;
use warnings;
use v5.10;
use Carp;

# Environment variables:
# FORCE_USIM - don't consider input to be GSM
# FORCE_GSM  - don't consider input to be USIM

while(<>) {
	# header
	print;
	last if(/^\s*$/);
}

my $GSMSTART = ['A0', 'A4', '00'];
my $USIMSTART = ['00', 'A4', '00'];
my @SIM_SERVICES = (
	"CHV1 disable function",
	"Abbreviated Dialling Numbers",
	"Fixed Dialling Numbers",
	"Short Message Storage",
	"Advice of Charge",
	"Capability Configuration Parameters",
	"PLMN selector",
	"(RFU)",
	"MSISDN",
	"Extension1",
	"Extension2",
	"SMS Parameters",
	"Last Number Dialled",
	"Cell Broadcast Message Identifier",
	"Group Identifier Level 1",
	"Group Identifier Level 2",
	"Service Provider Name",
	"Service Dialling Numbers",
	"Extension3",
	"(RFU)",
	"VCGS Group Identifier List",
	"VBS Group Identifier List",
	"enhanced Multi-Level Precedence and Pre-emption Service",
	"Automatic Answer for eMLPP",
	"Data download via SMS-CB",
	"Data download via SMS-PP",
	"Menu selection",
	"Call control",
	"Proactive SIM",
	"Cell Broadcast Message Identifier Ranges",
	"(RFU)",
	"(RFU)",
);
# 3GPP TS 102.223 v10.05.00, pg 23
my @ME_SERVICES = (
	"Profile download", # start of byte 1
	"SMS-PP data download",
	"Cell Broadcast data download",
	"Menu selection",
	"SMS-PP data download",
	"Timer expiration",
	"USSD string data object support in Call Control by USIM",
	"Call Control by NAA",
	"Command result", # start of byte 2
	"Call Control by NAA",
	"Call Control by NAA",
	"MO short message control support",
	"Call Control by NAA",
	"UCS2 Entry supported",
	"UCS2 Display supported",
	"Display Text",
	"Proactive SIM: DISPLAY TEXT", # start of byte 3
	"Proactive SIM: GET INKEY",
	"Proactive SIM: GET INPUT",
	"Proactive SIM: MORE TIME",
	"Proactive SIM: PLAY TONE",
	"Proactive SIM: POLL INTERVAL",
	"Proactive SIM: POLLING OFF",
	"Proactive SIM: REFRESH",
	"Proactive SIM: SELECT ITEM", # start of byte 4
	"Proactive SIM: SEND SHORT MESSAGE",
	"Proactive SIM: SEND SS",
	"Proactive SIM: SEND USSD",
	"Proactive SIM: SET UP CALL",
	"Proactive SIM: SET UP MENU",
	"Proactive SIM: PROVIDE LOCAL INFORMATION (MCC, MNC, LAC, Cell ID & IMEI)",
	"Proactive SIM: PROVIDE LOCAL INFORMATION (NMR)",
	"Proactive SIM: SET UP EVENT LIST", # start of byte 5
	"Event: MT call",
	"Event: Call connected",
	"Event: Call disconnected",
	"Event: Location status",
	"Event: User activity",
	"Event: Idle screen available",
	"Event: Card reader status",
	"Event: Language selection", # start of byte 6
	"Event: Browser Termination",
	"Event: Data available",
	"Event: Channel status",
	"Event: Access Technology Change",
	"Event: Display parameters changed",
	"Event: Local Connection",
	"Event: Network Search Mode Change",
	"Proactive UICC: POWER ON CARD", # start of byte 7
	"Proactive UICC: POWER OFF CARD",
	"Proactive UICC: PERFORM CARD APDU",
	"Proactive UICC: GET READER STATUS (Card reader status)",
	"Proactive UICC: GET READER STATUS (Card reader identifier)",
	"(RFU)",
	"(RFU)",
	"(RFU)",
	"Proactive UICC: TIMER MANAGEMENT (start, stop)", # start of byte 8
	"Proactive UICC: TIMER MANAGEMENT (get current value)",
	"Proactive UICC: PROVIDE LOCAL INFORMATION (date, time and time zone)",
	"GET INKEY",
	"SET UP IDLE MODE TEXT",
	"RUN AT COMMAND",
	"SETUP CALL",
	"Call Control by NAA",
	"DISPLAY TEXT", # start of byte 9
	"SEND DTMP command",
	"Proactive UICC: PROVIDE LOCAL INFORMATION (NMR)",
	"Proactive UICC: PROVIDE LOCAL INFORMATION (language)",
	"Proactive UICC: PROVIDE LOCAL INFORMATION (Timing Advance)",
	"Proactive UICC: LANGUAGE NOTIFICATION",
	"Proactive UICC: LAUNCH BROWSER",
	"Proactive UICC: PROVIDE LOCAL INFORMATION (Access Technology)",
	"Soft keys support for SELECT ITEM", # start of byte 10
	"Soft keys support for SET UP MENU",
	"(RFU)",
	"(RFU)",
	"(RFU)",
	"(RFU)",
	"(RFU)",
	"(RFU)",
	"(Soft keys information)", # start of byte 11
	"(Soft keys information)",
	"(Soft keys information)",
	"(Soft keys information)",
	"(Soft keys information)",
	"(Soft keys information)",
	"(Soft keys information)",
	"(Soft keys information)",
	"Proactive UICC: OPEN CHANNEL", # start of byte 12
	"Proactive UICC: CLOSE CHANNEL",
	"Proactive UICC: RECEIVE DATA",
	"Proactive UICC: SEND DATA",
	"Proactive UICC: GET CHANNEL STATUS",
	"Proactive UICC: SERVICE SEARCH",
	"Proactive UICC: GET SERVICE INFORMATION",
	"Proactive UICC: DECLARE SERVICE",
	"CSD", # start of byte 13
	"GPRS",
	"Bluetooth",
	"IrDA",
	"RS232",
	"(Number of channels supported)",
	"(Number of channels supported)",
	"(Number of channels supported)",
	"(Screen height)", # start of byte 14
	"(Screen height)",
	"(Screen height)",
	"(Screen height)",
	"(Screen height)",
	"No display capability",
	"No keypad capability",
	"Screen Sizing Parameters",
	"(Screen width)", # start of byte 15
	"(Screen width)",
	"(Screen width)",
	"(Screen width)",
	"(Screen width)",
	"(Screen width)",
	"(Screen width)",
	"Variable size fonts",
	"Display can be resized", # start of byte 16
	"Text Wrapping supported",
	"Text Scrolling supported",
	"Text Attributes supported",
	"(RFU)",
	"(Width reduction)",
	"(Width reduction)",
	"(Width reduction)",
	"TCP, UICC in client mode, remote connection", # start of byte 17
	"UDP, UICC in client mode, remote connection",
	"TCP, UICC in server mode, remote connection",
	"TCP, UICC in client mode, local connection",
	"UDP, UICC in client mode, local connection",
	"Direct communication channel",
	"E-UTRAN",
	"HSDPA",
	"Proactive UICC: DISPLAY TEXT (Variable Time out)", # start of byte 18
	"Proactive UICC: GET INKEY (help is supported)",
	"USB (Bearer Independent protocol supported bearers)",
	"Proactive UICC: GET INKEY (Variable Timeout)",
	"Proactive UICC: PROVIDE LOCAL INFORMATION (ESN)",
	"Call control on GPRS",
	"Proactive UICC: PROVIDE LOCAL INFORMATION (IMEISV)",
	"Proactive UICC: PROVIDE LOCAL INFORMATION (Search Mode change)",
	"(Reserved by TIA/EIA-136)", # start of byte 19
	"(Reserved by TIA/EIA-136)",
	"(Reserved by TIA/EIA-136)",
	"(Reserved by TIA/EIA-136)",
	"(RFU)",
	"(RFU)",
	"(RFU)",
	"(RFU)",
	"(Reserved by TIA/EIA/IS-820)", # start of byte 20
	"(Reserved by TIA/EIA/IS-820)",
	"(Reserved by TIA/EIA/IS-820)",
	"(Reserved by TIA/EIA/IS-820)",
	"(Reserved by TIA/EIA/IS-820)",
	"(Reserved by TIA/EIA/IS-820)",
	"(Reserved by TIA/EIA/IS-820)",
	"(Reserved by TIA/EIA/IS-820)",
	"**End of list** more options still remain in the standard"
);
# 3GPP TS 101.221 pg 80
my @SELECT_SIM_TAGS = (
	"File size",
	"Total file size",
	"File descriptor",
	"File identifier",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"Short file identifier",
	"(invalid)",
	"Life cycle status integer",
	"Security attributes type 1",
	"Security attributes type 2",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"Proprietary information",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"(invalid)",
	"Security attributes",
);
# ETSI TS 101.220 v11.0.0 (pg 14)
my @PROACTIVE_SIM_TAGS = (
	"(invalid)", # 0
	"Command details tag",
	"Device identity tag",
	"Result tag",
	"Duration tag",
	"Alpha identifier tag",
	"Address tag",
	"Capability configuration parameters tag",
	"Called party subaddress tag",
	"SS string tag",
	"Reserved for USSD string tag", # 10
	"SMS TPDU tag",
	"Cell Broadcast page tag",
	"Text string tag",
	"Tone tag / eCAT client profile",
	"Item tag / eCAT client identity",
	"Item identifier tag / Encapsulated envelope type",
	"Response length tag",
	"File List tag",
	"Location Information tag",
	"IMEI tag", # 20
	"Help request tag",
	"Network Measurement Results tag",
	"Default Text tag",
	"Items Next Action Indicator tag",
	"Event list tag",
	"Cause tag (reserved)",
	"Location status tag",
	"Transaction identifier tag",
	"BCCH channel list tag (reserved)",
	"Icon identifier tag", # 30
	"Item Icon identifier list tag",
	"Card reader status tag",
	"Card ATR tag",
	"C-APDU tag",
	"R-APDU tag",
	"Timer identifier tag",
	"Timer value tag",
	"Date-Time and Time zone tag",
	"Call control requested action tag",
	"AT Command tag", # 40
	"AT Response tag",
	"BC Repeat Indicator tag (reserved)",
	"Immediate response tag",
	"DTMF string tag",
	"Language tag",
	"Timing Advance tag (reserved)",
	"AID tag",
	"Browser Identity tag",
	"URL / URI tag",
	"Bearer tag", # 50
	"Provisioning Reference File tag",
	"Browser Termination Clause tag",
	"Bearer description tag",
	"Channel data tag",
	"Channel data length tag",
	"Channel status tag",
	"Buffer size tag",
	"Card reader identification tag",
	"File Update Information tag",
	"UICC/terminal interface transport level tag", # 60
	"Not used",
	"Other address (data destination address) tag",
	"Access Technology tag",
	"Display parameters tag",
	"Service Record tag",
	"Device Filter tag",
	"Service Search tag",
	"Attribute information tag",
	"Service Availability tag",
	"ESN tag (reserved)", # 70
	"Network Access Name tag",
	"CDMA-SMS-TPDU tag (reserved)",
	"Remote Entity Address tag",
	"I-WLAN identifier tag (reserved)",
	"I-WLAN Access Status tag (reserved)",
	"Reserved for future use",
	"Reserved for future use",
	"Reserved for future use",
	"Reserved for future use",
	"Text attribute tag", # 80
	"Item text attribute tag",
	"PDP context activation tag (reserved)",
	"Contactless state request tag",
	"Contactless functionality state tag",
	"CSG cell selection status (reserved)",
	"CSG ID (reserved)",
	"HNB name (reserved)",
	"Reserved for future use",
	"Reserved for future use",
	"Reserved for future use", # 90
	"Reserved for future use",
	"Reserved for future use",
	"Reserved for future use",
	"Reserved for future use",
	"Reserved for future use",
	"Reserved for future use",
	"Reserved for future use",
	"IMEISV tag",
	"Battery state tag",
	"Browsing status tag", # 100
	"Network Search Mode tag",
	"Frame Layout tag",
	"Frames Information tag",
	"Frame identifier tag",
	"UTRAN/E-UTRAN Measurement Qualifier tag (reserved)",
	"Multimedia Message Reference tag",
	"Multimedia Message Identifier tag",
	"Multimedia Message Transfer Status tag",
	"MEID tag",
	"Multimedia Message Content Identifier tag",
	"Multimedia Message Notification tag",
	"Last Envelope tag",
	"Registry application data tag",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag reserved for 3GPP",
	"Tag 7F / FF; end of tag list; invalid tag"
);
my %DEVICE_IDENTIFIERS = (
	'01' => "Keypad",
	'02' => "Display",
	'03' => "Earpiece",
	'81' => "SIM",
	'82' => "ME",
	'83' => "Network",
);
# GSM TS 11.14 says the commands are listed in section 13.4
# however, section 13.4 does not exist (section 13 is very short and has no subsections)
# alternative source, table 1, chapter 1, page 6:
# http://www.multitech.com/en_US/documents/collateral/manuals/s000391c.pdf
my %PROACTIVE_COMMAND_DESCRIPTION = (
# Any commented out value comes from the linked document, but isn't named in GSM TS 11.14
	'01' => "REFRESH",
	'03' => "POLL INTERVAL", # <-- unsure, but very likely (parameters match)
#	'05' => "SET UP EVENT LIST",
	# Possibilities, considering:
	#   - Only device identity, then unimplemented or no parameters
	#   - All other values are already defined
	#   - Device identity is from SIM to ME
	# are:
	#   MORE TIME, POLLING OFF, PROVIDE LOCAL INFORMATION
	'10' => "SET UP CALL",
	'11' => "SEND SS",
#	'12' => "SEND USSD",
	'13' => "SEND SHORT MESSAGE",
#	'14' => "SEND DTMF",
#	'15' => "LAUNCH BROWSER",
	'20' => "PLAY TONE",
	'21' => "DISPLAY TEXT",
	'22' => "GET INKEY",
	'23' => "GET INPUT",
	'24' => "SELECT ITEM",
	'25' => "SET UP MENU",
#	'28' => "SET UP IDLE MODE TEXT",
# Not listed:
# MORE TIME
# POLLING OFF
# PROVIDE LOCAL INFORMATION
);
my %RESULT_TAG_DESCRIPTION = (
	'00' => "Command performed succesfully",
	'01' => "Command performed with partial comprehension",
	'02' => "Command performed, with missing information",
	'03' => "REFRESH performed with additional EFs read",
	'10' => "Proactive SIM session terminated by user",
	'11' => "Backward move in the proactive SIM session requested by the user",
	'12' => "No response from user",
	'20' => "ME currently unable to process command",
	'21' => "Network currently unable to process command",
	'22' => "User did not accept call set-up request",
	'23' => "User cleared down call before connection or network release",
	'30' => "Command beyond ME's capabilities",
	'31' => "Command type not understood by ME",
	'32' => "Command data not understood by ME",
	'33' => "Command number not known by ME",
	'34' => "SS Return Error",
	'35' => "SMS RP-ERROR",
	'36' => "Error, required values are missing",
# Missing: Screen is busy, Currently busy on call, No service, Busy on SS transaction
);

my $startsec;
my @unparsed_bytes;
my $gsmstarted = 0;
my @warnings;
my $relsec = 0;
my $cmdstartsec;
my $is_usim = 0;
my $maybe_not_usim = 0;
my $apdus = 0;
while(<>) {
	s/[\r\n]+//g;
	next if($_ =~ /^$/);
	my ($hour, $min, $sec, $msec) = /^(\d\d):(\d\d):(\d\d)\.(\d\d\d):$/;
	next if !defined $msec;
	$sec = $hour * 3600 + $min * 60 + $sec + ($msec / 1000);
	if(!defined($startsec)) {
		$startsec = $sec;
		$relsec = 0;
	} else {
		$relsec = $sec - $startsec;
	}

	$_ = <>;
	if(isOdd(length $_)) {
		die "Bytes don't have even length: $_\n";
	}
	push @unparsed_bytes, $_ =~ /(..)/g;

	if(!$gsmstarted) {
		# find GSMSTART in unparsed_bytes
		my $pos = $ENV{FORCE_USIM} ? -1 : findBytes($GSMSTART, \@unparsed_bytes);
		my $upos = $ENV{FORCE_GSM} ? -1 : findBytes($USIMSTART, \@unparsed_bytes);
		if($pos >= 0 || $upos >= 0) {
			# Read from USIM start if it's found earlier
			$is_usim = $upos >= 0 && ($pos < 0 || $upos < $pos);
			# Though, fallback to GSM later if SIM card did not understand USIM
			$maybe_not_usim = $is_usim;
			$pos = $upos if($is_usim);

			$gsmstarted = 1;
			print "$relsec: found " . ($is_usim ? "USIM" : "GSM") . " communication start\n";
			my @start_bytes = splice @unparsed_bytes, 0, $pos if $pos > 0;
			my %unique_bytes;
			foreach(@start_bytes) {
				$unique_bytes{$_} ||= 0;
				$unique_bytes{$_} ++;
			}
			print "$relsec: Bytes before communication start: " . join(', ', keys %unique_bytes) . "\n";
		} else {
			next;
		}
	}

	# Try to parse a command and response APDU
	# ETSI TS 102.221 (USIM) calls orig_len "p3", the first five bytes the "command header"
	# in old SIM, the card always responds with INS again, then either of both sends some data
	# in USIM, the UICC will only send a procedure byte if the length is not 0x00.
	# * Equal to INS byte => terminal/uicc should continue reading/writing data as planned
	# * Equal to complement of INS => terminal/uicc should send next byte
	# * 0x60 ("NULL") => wait a bit for the next procedure byte
	# * SW1 (only 0x61 or 0x6c) => wait for SW2, then send GET RESPONSE or repeat the command
	my ($cla, $ins, $p1, $p2, $orig_len, $proc_byte) = @unparsed_bytes;
	$cmdstartsec = $relsec if(defined $cla && !defined($cmdstartsec));
	next if(!defined($proc_byte));
	my $len = hex($orig_len);

	my $sending = insIsSending($cla, $ins);

	# Only require length to be > 0 if this is USIM
	if(!$is_usim || $len > 0) {
		if($ins ne $proc_byte) {
			if($proc_byte eq "6C") {
				my (undef, undef, undef, undef, undef, undef, $sw2) = splice @unparsed_bytes, 0, 7;
				warning("SIM tells phone its length is incorrect; try again with 0x$sw2 (".hex($sw2).").");
				next;
			} elsif($proc_byte eq "6A") {
				# This is a normal situation if the phone tries to speak USIM to the card and
				# the card only understands GSM
				warning("SIM tells phone P1 or P2 is incorrect");
				$len = 0;
				# fall-through
			} else {
				warning("Procedure byte is not equal to INS; INS=$ins, RESP_INS=$proc_byte");
			}
		}
	}

	if(!defined($sending)) {
		my $pos = findBytes(["A0"], \@unparsed_bytes, 1);
		my $upos = findBytes(["00"], \@unparsed_bytes, 1);
		if($pos < 0 && $upos < 0) {
			warning("Command not understood or SIM card responded incorrectly... waiting for new command.");
			next;
		}

		my $this_is_usim = 0;
		if($is_usim && $upos >= 0 && ($pos < 0 || $upos < $pos)) {
			$this_is_usim = 1;
			$pos = $upos;
		}

		my @skipped_bytes = splice @unparsed_bytes, 0, $pos if $pos > 0;
		printf("%.3f: Bytes before communication restoration:\n", $relsec);
		print printapdu(\@skipped_bytes) . "\n";
		warning("Command not understood or SIM card responded incorrectly, skipped $pos bytes to next " . ($this_is_usim ? "USIM" : "GSM") . " command!");
		print "\n";
		next;
	}

	if(!$sending && $len != 0) {
		warning("Command APDU describes length, but this INS byte is known not to send data");
	} elsif($sending eq "SIM" && $len == 0) {
		# GSM 11.11 Version 5.3.0 page 35
		# If the SIM should send data, but length is 0x00, then
		# there will be a data transfer of 256 bytes.
		$len = 256 if(!$is_usim);
		# In USIM, according to TS 102.221, this happens by sending procedure bytes (???)
		# Not sure if that's true, it's not clear from the specs.
		# See 7.3.1.1.5.1, page 47
	}

	my @payload = @unparsed_bytes[6 .. 5 + $len] if($len != 0);
	next if(@payload != $len);

	my ($sw1, $sw2);
	# TODO: In usim mode, we should read procedure bytes. They come before the
	# status words if the length is nonzero.
	if($is_usim && $len == 0) {
		($sw1, $sw2) = @unparsed_bytes[5..6];
	} else {
		($sw1, $sw2) = @unparsed_bytes[(6 + $len) .. (7 + $len)];
	}
	next if(!defined($sw2));

	my $commandapdu = [$cla, $ins, $p1, $p2, $orig_len];
	my $responseapdu = [$sw1, $sw2];

	if($is_usim && $len == 0) {
		splice @unparsed_bytes, 0, 7;
	} else {
		splice @unparsed_bytes, 0, 8 + $len;
	}

	my $explanations = explain($commandapdu, $responseapdu, \@payload, $sending);

	print "Apdu " . ++$apdus . ": \n";
	output("COMMAND", $cmdstartsec, printapdu($commandapdu), $explanations->[0]);
	if($sending eq "ME") {
		output("COMMAND DATA", undef, printapdu(\@payload), $explanations->[2]);
	}
	output("RESPONSE", $relsec, printapdu($responseapdu), $explanations->[1]);
	if($sending eq "SIM") {
		output("RESPONSE DATA", undef, printapdu(\@payload), $explanations->[2]);
	}

	print "\n";

	undef $cmdstartsec;
}

printf "%.3f: Done. List of warnings during operation:\n", $relsec;
use Data::Dumper;
print Dumper(@warnings);


sub isOdd {
	return ($_[0] % 2) == 1;
}

sub warning {
	my $warning = sprintf("%.3f: %s", $relsec, $_[0]);
	push @warnings, $warning;
	warn "$warning\n";
}

sub printapdu {
	return join ' ', @{$_[0]};
}

sub findBytes {
	my ($bytes, $string, $offset) = @_;
	$offset ||= 0;
	OUTER: for(my $i = $offset; $i <= @$string - @$bytes; ++$i) {
		INNER: for(my $j = 0; $j < @$bytes; ++$j) {
			if($string->[$i + $j] ne $bytes->[$j]) {
				next OUTER;
			}
		}

		return $i;
	}
	return -1;
}

sub insIsSending {
	my ($cla, $ins) = @_;
	# Source: GSM 11.11 version 5.3.0 page 38
	my @gsm_sending_bytes = qw/A4 D6 DC A2 32 20 24 26 28 2C 88 10 C2 14/;
	my @gsm_receiving_bytes = qw/A4 F2 B0 B2 A2 32 88 C0 C2 12/;
	my @gsm_nodata_bytes = qw/04 44 FA/;

	# Source: ETSI TS 102.221 version 10.0.0 page 71
	my $rcla = hex $cla;
	my $valid_gsm_cla = !bit($rcla, 7) && !bit($rcla, 5);

	if($valid_gsm_cla) {
		if($ins ~~ @gsm_sending_bytes) {
			return "ME";
		} elsif($ins ~~ @gsm_receiving_bytes) {
			return "SIM";
		} elsif($ins ~~ @gsm_nodata_bytes) {
			return "";
		} else {
			warning "Unrecognised instruction byte $ins (CLA=$cla).";
			return;
		}
	} else {
		warning "Unrecognised class byte $cla.";
		return;
	}
}

sub output {
	my ($type, $sec, $string, $explanation) = @_;
	if(defined $sec) {
		$sec = sprintf("%.3f", $sec) . ":";
	} else {
		$sec = "";
	}
	printf "%8s %13s: %s\n", $sec, $type, $string;
	print ((' ' x 24), $explanation, "\n") if $explanation;
}

my @selected_file;
my $stk_length;
my $response_type;
sub explain {
	my ($command, $response, $data, $sending) = @_;
	my ($cla, $ins, $p1, $p2, $len) = @$command;
	my ($sw1, $sw2) = ($response->[-2], $response->[-1]);

	# In USIM, any CLA is allowed, as long as bits 5 and 7 are 0
	# ETSI TS 102.221 version 10.0.0 page 71
	my $cla046 = !$is_usim || $cla =~ /^(0|4|6).$/;
	my $cla8ce = !$is_usim || $cla =~ /^(8|c|e).$/i;
	my $rcla = hex $cla;
	die if(bit($rcla, 7) || bit($rcla, 5));

	my ($cmd_explain, $resp_explain, $data_explain);

	my $p1_understood = "00";
	my $p2_understood = "00";

	# Immediately forget response_type if the command is not GET RESPONSE
	unless($ins eq "C0") {
		undef $response_type;
	}

	if($ins eq "A4") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "SELECT file";
		@selected_file = @$data;
		$response_type = "SELECT";
		warning "SELECT with data length is not 2, and this is not USIM" if @$data != 2 && !$is_usim;
		my $is_aid = 0;
		if($p1 eq "04") {
			$p1_understood = $p1;
			$is_aid = 1;
		}
		if($maybe_not_usim && $is_usim && $sw1 eq "6A" && $sw2 eq "86") {
			$maybe_not_usim = 0;
			$is_usim = 0;
			$data_explain = "Phone understands USIM, but SIM does not; switching to GSM mode";
			warning $data_explain;
		} else {
			$data_explain = getFileDescription($data, $is_aid);
		}
	} elsif($ins eq "F2") {
		warning "Wrong class byte for this instruction" if !$cla8ce;
		$cmd_explain = "STATUS";
		if(!@selected_file) {
			warning "STATUS command out of sequence?";
		} else {
			if($p2 eq "0C") {
				$p2_understood = $p2;
				$data_explain = "STATUS without description of file";
			} elsif($p2 eq "01") {
				$p2_understood = $p2;
				$data_explain = "TLV object of currently selected application";
			} else {
				if(!@$data) {
					$data_explain = "There should be data, but there isn't...";
				} else {
					my @lines = explain_status_file(@selected_file, $data);
					$data_explain = "== Description of file ".join(' ', @selected_file)." ==\n";
					foreach(@lines) {
						$data_explain .= (' ' x 24) . $_ . "\n";
					}
					$data_explain =~ s/\n$//;
				}
			}
		}
	} elsif($ins eq "B0") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "READ BINARY";
		my @lines = explain_file_contents($data, @selected_file);
		$data_explain = "== Contents of file ".join(' ', @selected_file)." ==\n";
		foreach(@lines) {
			$data_explain .= (' ' x 24) . $_ . "\n";
		}
		$data_explain =~ s/\n$//;
	} elsif($ins eq "D6") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "UPDATE BINARY";
	} elsif($ins eq "B2") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "READ RECORD";
	} elsif($ins eq "DC") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "UPDATE RECORD";
	} elsif($ins eq "A2") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "SEEK / SEARCH RECORD";
	} elsif($ins eq "32") {
		warning "Wrong class byte for this instruction" if !$cla8ce;
		$cmd_explain = "INCREASE";
	} elsif($ins eq "CB") {
		warning "Wrong class byte for this instruction" if !$cla8ce;
		$cmd_explain = "RETRIEVE DATA"
	} elsif($ins eq "DB") {
		warning "Wrong class byte for this instruction" if !$cla8ce;
		$cmd_explain = "SET DATA";
	} elsif($ins eq "20") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "VERIFY CHV";
		$p2_understood = $p2;
		my $pin = bytes_to_string($data, 0xff);
		$data_explain = "PIN code: \"$pin\"";
	} elsif($ins eq "24") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "CHANGE CHV";
	} elsif($ins eq "26") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "DISABLE CHV";
	} elsif($ins eq "28") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "ENABLE CHV";
	} elsif($ins eq "2C") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "UNBLOCK CHV";
	} elsif($ins eq "04") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "INVALIDATE / DEACTIVATE FILE";
	} elsif($ins eq "44") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "REHABILITATE / ACTIVATE FILE";
	} elsif($ins eq "88" || $ins eq "89") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "RUN GSM ALGORITHM / AUTHENTICATE";
	} elsif($ins eq "84") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "GET CHALLENGE";
	} elsif($ins eq "FA") {
		warning "The SLEEP instruction is obsolete";
		$cmd_explain = "SLEEP";
	} elsif($ins eq "AA") {
		warning "Wrong class byte for this instruction" if !$cla8ce;
		$cmd_explain = "TERMINAL CAPABILITY";
	} elsif($ins eq "C0") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "GET RESPONSE, " . (hex $len) . " bytes";
		if(!defined($response_type)) {
			$data_explain = "(did not expect this GET RESPONSE)\n";
			warning "Did not expect this GET RESPONSE.\n";
		} elsif($response_type eq "SELECT") {
			my $tag    = $data->[0];
			my $length = hex $data->[1];
			my $offset = 2;
			my $context = {};
			$data_explain = "== Response to SELECT ==\n";
			until($offset >= $length) {
				my ($lastlength, @lines) = explain_data_object($data, $offset, $context, "SELECT");
				$offset += $lastlength;
				foreach(@lines) {
					$data_explain .= (' ' x 24) . $_ . "\n";
				}
			}
			$data_explain =~ s/\n$//;
		}
	} elsif($ins eq "10") {
		warning "Wrong class byte for this instruction" if $is_usim and $cla ne "80";
		$cmd_explain = "TERMINAL PROFILE";
		my $offset = 1;
		my @services_enabled;
		foreach(@$data) {
			my $byte = hex $_;
			for(1..8) {
				my $value = bit($byte, $_);
				$services_enabled[$offset] = $value;
				$offset ++;
			}
		}
		$data_explain = "== Mobile Equipment SIM Toolkit Capabilities ==\n";
		for(my $i = 1; $i < @services_enabled; ++$i) {
			my $service = $ME_SERVICES[$i-1] || "(unknown capability)";
			my $enabled = $services_enabled[$i] ? "enabled" : "not enabled";
			$data_explain .= (' ' x 24) . "Capability $i: $service ($enabled)\n";
		}
	} elsif($ins eq "C2") {
		warning "Wrong class byte for this instruction" if $is_usim and $cla ne "80";
		$cmd_explain = "ENVELOPE (SMS Point-to-point or cell broadcast data download)";
		my $tag = $data->[0];
		my $descr = "???";
		# TS 101.220 v11.00.00 p11 to 16
		if($tag eq "CF") {
			$descr = "Reserved for proprietary use";
			# TODO: what would this do?
		} elsif($tag eq "D0") {
			warning "Proactive Command byte in ENVELOPE!";
			$descr = "Proactive Command (forbidden in ENVELOPE!)";
		} elsif($tag eq "D1") {
			$descr = "SMS-PP download";
		} elsif($tag eq "D2") {
			$descr = "Cell Broadcast download";
		} elsif($tag eq "D3") {
			$descr = "Menu Selection";
		} elsif($tag eq "D4") {
			$descr = "Call Control";
		} elsif($tag eq "D5") {
			$descr = "MO Short Message Control";
		} elsif($tag eq "D6") {
			$descr = "Event Download";
		} elsif($tag eq "D7") {
			$descr = "Timer Expiration";
		} elsif($tag eq "D8") {
			$descr = "Reserved for intra-UICC communication";
			# TODO: what would this do?
		} elsif($tag eq "D9") {
			$descr = "USSD Download";
		} elsif($tag eq "DA") {
			$descr = "MMS Transfer Status";
		} elsif($tag eq "DB") {
			$descr = "MMS notification download";
		} elsif($tag eq "DC") {
			$descr = "Terminal application tag";
		} elsif($tag eq "DD") {
			$descr = "Geographic Location Reporting tag";
		}
		my $length = hex $data->[1];
		if($length != (hex $len) - 2) {
			warning "Length of the ENVELOPE instruction seems incorrect ($length, but ".($len - 2) . " bytes left in response)";
		}
		my $offset = 2;
		my $context = {};
		$data_explain = "== Command from ME / network to SIM ==\n";
		$data_explain .= (' ' x 24) . "Type: $tag ($descr)\n";
		until($offset >= $length) {
			my ($lastlength, @lines) = explain_data_object($data, $offset, $context);
			$offset += $lastlength;
			foreach(@lines) {
				$data_explain .= (' ' x 24) . $_ . "\n";
			}
		}

		$data_explain =~ s/\n$//;
		undef $stk_length;
	} elsif($ins eq "12") {
		warning "Wrong class byte for this instruction" if $is_usim and $cla ne "80";
		$cmd_explain = "FETCH";

		# First byte should be Proactive SIM command tag
		# Second (and according to specs, "maybe" third) should be length
		# After that are multiple data blocks that can be explained by explain_data_object
		if($data->[0] ne "D0") {
			warning "First byte of FETCH command was not 'Proactive SIM command tag', this is required by BER-TLV in SIM to ME direction";
		}

		my $length = hex $data->[1];
		if($length != (hex $len) - 2) {
			warning "Length of the FETCH instruction seems incorrect ($length, but ".($len - 2) . " bytes left in response)";
		}
		my $offset = 2;
		my $context = {};

		$data_explain = "== Proactive SIM command from SimToolkit to phone ==\n";

		until($offset >= $length) {
			my ($lastlength, @lines) = explain_data_object($data, $offset, $context);
			$offset += $lastlength;
			foreach(@lines) {
				$data_explain .= (' ' x 24) . $_ . "\n";
			}
		}

		$data_explain =~ s/\n$//;
		undef $stk_length;
	} elsif($ins eq "14") {
		warning "Wrong class byte for this instruction" if $is_usim and $cla ne "80";
		$cmd_explain = "TERMINAL RESPONSE";

		# GSM TS 11.14 section 6.8
		# This response contains SIMPLE-TLV objects, three of which are mandatory,
		# some of which are optional

		my $read = 0;
		my $context = {};

		$data_explain = "== Response from phone to proactive SIM command from SimToolkit ==\n";

		# TODO: check if command details, device identities and result are given
		# all others are optional, maybe check if there is too much data too?
		until($read == @$data) {
			my ($lastlength, @lines) = explain_data_object($data, $read, $context);
			$read += $lastlength;
			foreach(@lines) {
				$data_explain .= (' ' x 24) . $_ . "\n";
			}
		}

		$data_explain =~ s/\n$//;
	} elsif($ins eq "70") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "MANAGE CHANNEL";
	} elsif($ins eq "73") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "MANAGE SECURE CHANNEL";
	} elsif($ins eq "75") {
		warning "Wrong class byte for this instruction" if !$cla046;
		$cmd_explain = "TRANSACT DATA";
	} else {
		$cmd_explain = "???";
		warning "Unknown GSM INS byte: $ins";
	}

	if($p1_understood ne $p1) {
		$cmd_explain .= " [P0=$p1?]";
	}
	if($p2_understood ne $p2) {
		$cmd_explain .= " [P1=$p2?]";
	}

	my $sw2h = hex $sw2;
	if($sw1 eq "90" && $sw2 eq "00") {
		$resp_explain = "SUCCESS";
	} elsif($sw1 eq "91") {
		if(defined $stk_length && $stk_length != $sw2h) {
			warning("SimToolkit length bytes changed from $stk_length to $sw2h without FETCH command");
		}
		$stk_length = $sw2h;
		$resp_explain = "SUCCESS ($sw2h bytes of extra information in memory, possibly Simtoolkit)";
	} elsif($sw1 eq "9F") {
		$resp_explain = "SUCCESS, response length $sw2h";
	} elsif($sw1 eq "93") {
		$resp_explain = "SIM TOOLKIT is busy";
	} elsif($sw1 eq "92") {
		$resp_explain = "SUCCESS, and $sw2h bytes of extra data transfer information";
		$resp_explain .= " (in case of old SIM: ";
		if($sw2h < 16) {
			$resp_explain .= "SUCCES, but after $sw2h tries)";
		} elsif($sw2h == 0x40) {
			$resp_explain .= "MEMORY PROBLEM)";
		} else {
			$resp_explain .= "???)";
		}
	} elsif($sw1 eq "94") {
		$resp_explain = "Referencing error";
	} elsif($sw1 eq "98") {
		if($sw2 eq "02") {
			$resp_explain = "No CHV initialised";
		} elsif($sw2 eq "04") {
			$resp_explain = "Access denied";
		} elsif($sw2 eq "08") {
			$resp_explain = "In contradiction with CHV status";
		} elsif($sw2 eq "10") {
			$resp_explain = "In contradiction with invalidation status";
		} elsif($sw2 eq "40") {
			$resp_explain = "Unsuccessful CHV verification, CHV blocked";
			warning "CHV was blocked at least once in this session";
		} elsif($sw2 eq "50") {
			$resp_explain = "Cannot increase, max value reached";
		} elsif($sw2 eq "62") {
			$resp_explain = "Authentication error, application specific";
		} elsif($sw2 eq "63") {
			$resp_explain = "Security session or association expired";
		} else {
			$resp_explain = "General application or authentication error";
			warning "General application or authentication error";
		}
	} elsif($is_usim && $sw1 eq "61") {
		$resp_explain = "SUCCESS, $sw2h extra response bytes, use GET RESPONSE";
	} elsif($is_usim && $sw1 eq "6C") {
		$resp_explain = "Immediately resend that command with length=$sw2h";
	} elsif($is_usim && $sw1 eq "62") {
		if($sw2 eq "00") {
			$resp_explain = "Warning, no information given, non-volatile memory unchanged";
		} elsif($sw2 eq "81") {
			$resp_explain = "Part of returned data may be corrupted";
		} elsif($sw2 eq "82") {
			$resp_explain = "End of file/record reached before reading Le bytes";
		} elsif($sw2 eq "83") {
			$resp_explain = "Selected file invalidated";
		} elsif($sw2 eq "85") {
			$resp_explain = "Selected file in termination state";
		} elsif($sw2 eq "F1") {
			$resp_explain = "More data available";
		} elsif($sw2 eq "F2") {
			$resp_explain = "More data available and proactive command pending";
		} elsif($sw2 eq "F3") {
			$resp_explain = "Response data available";
		} else {
			$resp_explain = "UNKNOWN WARNING";
		}
	} elsif($is_usim && $sw1 eq "63") {
		if($sw2 eq "F1") {
			$resp_explain = "More data expected";
		} elsif($sw2 eq "F2") {
			$resp_explain = "More data expected and proactive command pending";
		} elsif($sw2 =~ /^C(.)$/) {
			my $num = hex $1;
			$resp_explain = "Command succesful but after $num internal retries";
			$resp_explain .= " / Verification failed, $num tries remaining";
		} else {
			$resp_explain = "UNKNOWN WARNING";
		}
	} elsif($is_usim && $sw1 eq "64") {
		$resp_explain = "No information given, state of non-volatile memory unchanged";
	} elsif($is_usim && $sw1 eq "65") {
		if($sw2 eq "81") {
			$resp_explain = "Memory problem";
		} else {
			$resp_explain = "No information given, state of non-volatile memory changed";
		}
	} elsif($is_usim && $sw1 eq "67") {
		$resp_explain = "Wrong length / command dependent error";
	} elsif($is_usim && $sw1 eq "68") {
		if($sw2 eq "00") {
			$resp_explain = "Function in CLA not supported";
		} elsif($sw2 eq "81") {
			$resp_explain = "Logical channel not supported";
		} elsif($sw2 eq "82") {
			$resp_explain = "Secure messaging not supported";
		}
	} elsif($is_usim && $sw1 eq "69") {
		$resp_explain = "Command not allowed (for reason $sw2, see ETSI TS 102.221)";
	} elsif($is_usim && $sw1 eq "6B") {
		$resp_explain = "Wrong parameters P1-P2";
	} elsif($is_usim && $sw1 eq "6A") {
		$resp_explain = "Wrong parameters (for reason $sw2, see ETSI TS 102.221)";
	} elsif($is_usim && $sw1 eq "6D") {
		$resp_explain = "Instruction code not supported or invalid";
	} elsif($is_usim && $sw1 eq "6E") {
		$resp_explain = "Class not supported";
	} elsif($is_usim && $sw1 eq "6F") {
		$resp_explain = "Technical problem / command dependent error";
	} elsif(!$is_usim && substr($sw1, 0, 1) eq "6") {
		$resp_explain = "Application independent error (incorrect parameter, ...)";
	} else {
		$resp_explain = "???";
		warning "Unknown GSM SW1 SW2 combination";
	}

	return [$cmd_explain, $resp_explain, $data_explain];
}

sub explain_status_file {
	my ($d1, $d2, $data_orig) = @_;

	# Make a copy of the data
	my $data = ();
	push @$data, $_ for(@$data_orig);

	# 2F: Elementary file under master file
	# 3F: Master file
	# 6F: Elementary file under dedicated file
	# 7F: Dedicated file
	my $mfordf = $d1 eq "3F" || $d1 eq "7F";
	my $ef     = $d1 eq "2F" || $d1 eq "6F";
	if(!$mfordf and !$ef) {
		warning "I don't understand this directory file type: $d1\n";
		return ("Didn't understand it");
	}
	my @lines;

	if(@$data < 13) {
		die "Status file response length is incorrect\n";
	}
	my @rfu1 = splice @$data, 0, 2;
	my @size = splice @$data, 0, 2;
	my @id   = splice @$data, 0, 2;
	my $ftyp = splice @$data, 0, 1;
	my $flag = splice @$data, 0, 1 if $ef;
	my @accs = splice @$data, 0, 3 if $ef;
	my $fsta = splice @$data, 0, 1 if $ef;
	my @rfu2 = splice @$data, 0, 5 if $mfordf;
	my $len  = splice @$data, 0, 1;

	my ($char, $nDFc, $nEFc, $code, $rfu3, $chv1, $uch1) = ('') x 7;
	my ($chv2, $uch2, $rfu4, @admn) = ('') x 3;
	my ($efstruct, $recordlength) = ('') x 2;

	if(@$data < (hex $len)) {
		warning ("Status file response GSM specific data is incorrect (0x$len bytes expected, got " . scalar(@$data) . " bytes)");
	} elsif($mfordf) {
		$char = splice @$data, 0, 1; # characteristics
		$nDFc = splice @$data, 0, 1; # number of DF childs
		$nEFc = splice @$data, 0, 1; # number of EF childs
		$code = splice @$data, 0, 1;
		  # number of CHV, UNBLOCK CHV and administrative codes
		$rfu3 = splice @$data, 0, 1;
		$chv1 = splice @$data, 0, 1; # CHV1 status
		$uch1 = splice @$data, 0, 1; # UNBLOCK CHV1 status
		$chv2 = splice @$data, 0, 1; # CHV2 status
		$uch2 = splice @$data, 0, 1; # UNBLOCK CHV2 status
		$rfu4 = splice @$data, 0, 1;
		@admn = splice @$data;
	} else {
		$efstruct = splice @$data, 0, 1; # structure of EF
		$recordlength = splice @$data, 0, 1; # length of record
	}

	my $size = ((hex $size[0]) << 8) + hex $size[1];
	push @lines, "Total directory metadata size: $size" if $mfordf;
	push @lines, "Total file size: $size" if $ef;
	push @lines, "File ID: " . join (' ', @id);
	push @lines, "Type of file: $ftyp"; # TODO: see subclause 9.3
	if($mfordf) {
		push @lines, "File characteristics: $char"; # TODO: see detail 1
		push @lines, "DF childs: $nDFc";
		push @lines, "EF childs: $nEFc";
		push @lines, "Number of CHVs, UNBLOCK CHVs and administrative codes: $code";
		push @lines, "CHV1 status: $chv1"; # TODO: see detail 2
		push @lines, "UNBLOCK CHV1 status: $uch1";
		push @lines, "CHV2 status: $chv2";
		push @lines, "UNBLOCK CHV2 status: $uch2";
		push @lines, "Administrative management bytes: " . join(' ', @admn);
	} else {
		$flag = ($flag & 0b0000_0010) >> 1;
		push @lines, "INCREASE: " . ($flag ? "allowed" : "not allowed") if 0 && $ftyp == $ftyp; # TODO
		push @lines, "Access conditions: " . join(' ', @accs);
		push @lines, "File status: $fsta";
		push @lines, "Structure of EF: $efstruct" if $efstruct;
		push @lines, "Length of record: $recordlength" if $recordlength;
	}

	return @lines;
}

sub explain_file_contents {
	my ($data_orig, $d1, $d2) = @_;
	my @lines;
	my $data = [@$data_orig];

	if($d1 eq "2F") { # EF under MF
		if($d2 eq "E2") { # ICCID
			die "ICCID data length incorrect" if @$data != 10;
			push @lines, "Unique SIM ID number: " . join(' ', @$data);
			# TODO: this data is oddly encoded (page 51), decode it
		}
	}
	elsif($d1 eq "6F") { # EF under DF
		if($d2 eq "38") { # SIM service table
			die "SST data length incorrect" if @$data < 2;
			my @services_allocated;
			my @services_activated;
			my $service_ctr = 1;
			foreach(@$data) {
				my $byte = hex $_;
				for(1..4) {
					$services_allocated[$service_ctr] = bit($byte, $_ * 2 - 1);
					$services_activated[$service_ctr] = bit($byte, $_ * 2);
					$service_ctr++;
				}
			}
			for(my $i = 1; $i < @services_allocated; ++$i) {
				my $allocated = $services_allocated[$i] ? "allocated" : "not allocated";
				my $activated = $services_activated[$i] ? "activated" : "not activated";
				my $service   = $SIM_SERVICES[$i-1] || "unknown service";
				push @lines, "Service $i: $service ($allocated, $activated)";
			}
		}
		elsif($d2 eq "AE") { # EF_PHASE / Phase identification
			die "SIM PHASE specification has incorrect byte length" if(@$data != 1);
			my $phase = hex $data->[0];
			push @lines, "SIM supports phase $phase communication";
			if($phase == 0) {
				warning "SIM has old-style communication";
				push @lines, "SIM has old-style communication";
			} elsif($phase == 1) {
				warning "SIM has incorrect phase bit";
				push @lines, "SIM has incorrect phase bit specification (0x01 is not allowed)";
			} elsif($phase == 3) {
				push @lines, "SIM supports phase 2+, TERMINAL PROFILE is required if ME supports SIMToolkit";
			} elsif($phase > 3) {
				warning "SIM has reserved phase bit";
				push @lines, "phase bits higher than 3 are reserved by ETSI TC SMG.";
			}
		}
		elsif($d2 eq "AD") { # EF_AD / Administrative data
			my $mode = shift @$data;
			if($mode eq "00") {
				$mode = "Normal";
			} elsif($mode eq "01") {
				$mode = "Normal + specific facilities";
			} elsif($mode eq "02") {
				$mode = "Maintenance (off-line)";
			} elsif($mode eq "04") {
				$mode = "Cell test operation";
			} elsif($mode eq "80") {
				$mode = "Type approval operations";
			} elsif($mode eq "81") {
				$mode = "Type approval operations + specific facilities";
			} else {
				$mode = "Unknown ($mode)";
			}
			push @lines, "Operation mode: $mode";
			push @lines, "Additional data: " . join(' ', @$data);
		}
	}

	return @lines;
}

sub bit {
	my ($byte, $bit) = @_;
	die if $bit < 1 || $bit > 8;
	if($byte !~ /^\d{1,}$/) {
		croak "Byte must contain at least one digit";
	}
	my $mask = 1 << ($bit - 1);
	my $val = ($byte & $mask) >> ($bit - 1);
	return $val;
}

# Specification: GSM TS 11.14 v5.9.0 section 6.6, section 11
# SIMPLE-TLV data objects
sub explain_data_object {
	my ($data, $offset, $context, $type) = @_;
	$context ||= {};
	my @lines;

	my $tag_byte = $data->[$offset];
	if($tag_byte =~ /^D/) {
		warning "SIMPLE-TLV tag byte looks like DX, looks a lot like BER-TLV constants, wrong offset?";
	}

	my $comprehension_required = bit(hex $tag_byte, 8) ? "comprehension required" : "comprehension not required";
	$tag_byte = (hex $tag_byte) & 0b0111_1111;
	my $length = hex $data->[$offset + 1];
	my @data = (@$data)[($offset + 2) .. ($offset + $length + 1)];

	if(defined $type and $type eq "SELECT") {
		my $tag_descr = $SELECT_SIM_TAGS[$tag_byte] || "(undefined)";
		push @lines, "$tag_descr ($comprehension_required)";

		push @lines, "  Data: " . join(' ', @data);
		return ($length + 2, @lines);
	}

	my $tag_descr = $PROACTIVE_SIM_TAGS[$tag_byte] || "(undefined)";
	push @lines, "$tag_descr ($comprehension_required)";

	if($tag_byte == 1) { # Command details
		warning "Tag length seems incorrect, should be 3" if($length != 3);

		my $number = hex $data[0];
		if($number < 1 || $number > 0xfe) {
			warning "Command number has invalid value, should be at least 1 and at most 0xfe";
		}
		$context->{'CommandByte'} = $data[1];
		my $command = $context->{'COMMAND'} = $PROACTIVE_COMMAND_DESCRIPTION{$data[1]};
		$command ||= "unknown";

		my $qualifier_explanation = "unknown";
		if($command eq "REFRESH") {
			if($data[2] eq "00") {
				$qualifier_explanation = "SIM Initialization and Full File Change Notification";
			} elsif($data[2] eq "01") {
				$qualifier_explanation = "File Change Notification",
			} elsif($data[2] eq "02") {
				$qualifier_explanation = "SIM Initialization and File Change Notification",
			} elsif($data[2] eq "03") {
				$qualifier_explanation = "SIM Initialization";
			} elsif($data[2] eq "04") {
				$qualifier_explanation = "SIM Reset";
			} else {
				$qualifier_explanation = "unknown REFRESH qualifier";
			}
		} elsif($command eq "SET UP CALL") {
			if($data[2] eq "00") {
				$qualifier_explanation = "Set up call, but only if not currently busy on another call";
			} elsif($data[2] eq "01") {
				$qualifier_explanation = "Set up call, but only if not currently busy on another call, with redial";
			} elsif($data[2] eq "02") {
				$qualifier_explanation = "Set up call, putting all other calls (if any) on hold";
			} elsif($data[2] eq "03") {
				$qualifier_explanation = "Set up call, putting all other calls (if any) on hold, with redial";
			} elsif($data[2] eq "04") {
				$qualifier_explanation = "Set up call, disconnecting all other calls (if any)";
			} elsif($data[2] eq "05") {
				$qualifier_explanation = "Set up call, disconnecting all other calls (if any), with redial";
			} else {
				$qualifier_explanation = "unknown SET UP CALL qualifier";
			}
		} elsif($command eq "SEND SHORT MESSAGE") {
			if(bit(hex $data[2], 1)) {
				$qualifier_explanation = "SMS packing by the ME required";
			} else {
				$qualifier_explanation = "Packing not required";
			}
		} elsif($command eq "DISPLAY TEXT") {
			$qualifier_explanation = bit(hex $data[2], 1) ? "High priority; " : "Normal priority; ";
			$qualifier_explanation .= bit(hex $data[2], 8) ? "Wait for user to clear message" : "Clear message after a delay";
		} elsif($command eq "GET INKEY") {
			$qualifier_explanation = bit(hex $data[2], 1) ? "Characters from SMS default alphabet" : "Digits (0-9, *, # and +) only";
		} elsif($command eq "GET INPUT") {
			$qualifier_explanation = bit(hex $data[2], 1) ? "Characters from SMS default alphabet; " : "Digits (0-9, *, # and +) only; ";
			$qualifier_explanation .= bit(hex $data[2], 3) ? "User input shall not be revealed in any way; " : "ME may echo user input on display; ";
			$qualifier_explanation .= bit(hex $data[2], 4) ? "User input to be in SMS packed format" : "User input to be in unpacked format";
		} elsif($command eq "PROVIDE LOCAL INFORMATION") {
			if($data[2] eq "00") {
				$qualifier_explanation = "Location Information (MCC, MNC, LAC and Cell Identity)";
			} elsif($data[2] eq "01") {
				$qualifier_explanation = "IMEI of the ME";
			} else {
				$qualifier_explanation = "unknown PROVIDE LOCAL INFORMATION qualifier";
			}
		}

		push @lines, "  Command sequence number: $number";
		push @lines, "  Type of command: " . $data[1] . " ($command)";
		push @lines, "  Command Qualifier: " . $data[2] . " ($qualifier_explanation)";
		# TODO: 21 81: "display text (normal priority, clear message after delay)"
	} elsif($tag_byte == 2) { # Device identity
		warning "Tag length seems incorrect, should be 2" if($length != 2);

		my $source = $DEVICE_IDENTIFIERS{$data[0]} || "(unknown device)";
		my $destination = $DEVICE_IDENTIFIERS{$data[1]} || "(unknown device)";
		push @lines, "  Source: $source";
		push @lines, "  Destination: $destination";
	} elsif($tag_byte == 3) { # Result tag
		warning "Tag length seems incorrect, should be at least 1" if($length <= 0);
		my $description = $RESULT_TAG_DESCRIPTION{$data[0]} || "unknown result";
		my $status = "Unknown status";
		$status = "Succesfully performed" if($data[0] =~ /^0/);
		$status = "Cancelled by user" if($data[0] =~ /^1/);
		$status = "Temporary failure" if($data[0] =~ /^2/);
		$status = "Permanent failure" if($data[0] =~ /^3/);
		push @lines, "  Result: " . shift(@data) . " ($description)";
		push @lines, "  Status: $status";
		push @lines, "  Additional information: " . join(' ', @data);
		# TODO: additional information is described in GSM TS 11.12
		#       for SEND SS, ME problem, network problem, SS problem and SMS problem
	} elsif($tag_byte == 4) { # Duration
		warning "Tag length seems incorrect, should be 2" if($length != 2);
		my $in_second;
		if($data[0] eq "00") {
			push @lines, "  Time unit: minutes";
			$in_second = 60;
		} elsif($data[0] eq "01") {
			push @lines, "  Time unit: seconds";
			$in_second = 1;
		} elsif($data[0] eq "02") {
			push @lines, "  Time unit: Tenths of seconds";
			$in_second = 1/10;
		}
		my $units = hex $data[1];
		my $seconds = $units * $in_second;
		push @lines, "  Number of units: $units";
		push @lines, "  Ergo, number of seconds: $seconds";
	} elsif($tag_byte == 5) { # Alpha identifier
		if(@data == 0) {
			push @lines, "  Empty alpha identifier";
		} else {
			push @lines, "  Alpha identifier: " . join(' ', @data);
			my $string = bytes_to_string(\@data);
			push @lines, "  As string: \"$string\"";
		}
	} elsif($tag_byte == 13) { # Text string
		my $encoding = shift @data;
		if($encoding eq '04' || $encoding eq 'F4') {
			push @lines, "  Encoding: $encoding / 8-bit default SMS";
			my $string = bytes_to_string(\@data);
			push @lines, "  String: \"$string\"";
		} else {
			push @lines, "  Encoding: $encoding / ???";
			push @lines, "  Data: " . join(' ', @data);
		}
	} elsif($tag_byte == 15) { # Item tag
		my $identifier = shift @data;
		my $string = bytes_to_string(\@data);
		push @lines, "  Item '$identifier': \"$string\"";
	} elsif($tag_byte == 16) { # Item identifier tag
		warning "Tag length seems incorrect, should be 1" if($length != 1);
		push @lines, "  Item identifier: " . $data[0];
	} elsif($tag_byte == 17) { # Response length tag
		warning "Tag length seems incorrect, should be 2" if($length != 2);
		push @lines, "  Length between " . hex($data[0]) . " and " . hex($data[1]) . " characters";
	} else {
		push @lines, "  Unknown tag";
		push @lines, "  Data: " . join(' ', @data);
	}

	return ($length + 2, @lines);
}

sub bytes_to_string {
	my ($bytes, $end_chr) = @_;

	my $string = "";
	my $done = 0;
	for(@$bytes) {
		if(defined $end_chr && hex $_ == $end_chr) {
			$done = 1;
		} elsif($done) {
			warning "String in bytes_to_string had end character, and other bytes after that!";
		} elsif(hex $_ < 32 || hex $_ > 126) {
			warning "Unprintable character in bytes_to_string string!";
			$string .= '.';
		} else {
			$string .= chr hex $_;
		}
	}
	return $string;
}

sub getFileDescription {
	my ($data, $is_aid) = @_;
	$is_aid ||= 0;

	if($is_aid) {
		warning "Data length is lower than 7 while we're handling an AID SELECT" if(scalar(@$data) < 7);
		my @rid = @$data[0..4];
		return "AID SELECT todo";
	}

	if(@$data > 2) {
		# Path
		warning "Path length seems incorrect, not a divisor of 2" if(@$data % 2 != 0);
		my $i = 0;
		my $d = "Path consisting of ".(@$data / 2)." parts: ";
		until($i >= @$data) {
			$d .= getFileDescription([$data->[$i], $data->[$i + 1]]);
			$i += 2;
			if($i != @$data) {
				$d .= " /// ";
			}
		}
		return $d;
	}

	my $d0 = "???";
	my $d1 = "???";
	if($data->[0] eq "3F") {
		$d0 = "master file (directory)";
		if($data->[1] eq "00") {
			$d1 = "normal master file (directory)";
		}
	} elsif($data->[0] eq "7F") {
		$d0 = "dedicated file (directory)";
		if($data->[1] eq "10") {
			$d1 = "DF_TELECOM";
		} elsif($data->[1] eq "20") {
			$d1 = "DF_GSM";
		} elsif($data->[1] eq "21") {
			$d1 = "DF_GSM alternative name";
		} elsif(substr($data->[1], 0, 1) eq "2") {
			$d1 = "for operational use";
		} elsif(substr($data->[1], 0, 1) eq "4") {
			$d1 = "Administrative Use DF";
			warning "Phone tried to retrieve DF for administrative use";
		} elsif($data->[1] eq "FF") {
			$d1 = "Current Application ADF";
		} else {
			$d1 = "unknown directory file";
			warning "Phone tried to retrieve DF outside of specification";
		}
	} elsif($data->[0] eq "2F") {
		$d0 = "elementary file under master file";
		if($data->[1] eq "E2") {
			$d1 = "ICCID file (ICC identification)";
		}
	} elsif($data->[0] eq "6F") {
		$d0 = "elementary file under dedicated file";
		if($data->[1] eq "05") {
			$d1 = "LP (Language preference)";
		} elsif($data->[1] eq "07") {
			$d1 = "IMSI";
		} elsif($data->[1] eq "20") {
			$d1 = "Kc (Ciphering key)";
		} elsif($data->[1] eq "30") {
			$d1 = "PLMNsel (Network selectors)";
		} elsif($data->[1] eq "31") {
			$d1 = "HPLMN (Search period)";
		} elsif($data->[1] eq "37") {
			$d1 = "ACMmax (Max. accumulated call meter)";
		} elsif($data->[1] eq "38") {
			$d1 = "SST (SIM service table)";
		} elsif($data->[1] eq "39") {
			$d1 = "ACM (Accumulated call meter)";
		} elsif($data->[1] eq "3A") {
			$d1 = "ADN (Abbreviated Dialing Numbers)";
		} elsif($data->[1] eq "3B") {
			$d1 = "FDN (Fixed Dialing Numbers / Supplementary Service Control strings)";
		} elsif($data->[1] eq "3C") {
			$d1 = "SMS (Short messages)";
		} elsif($data->[1] eq "3D") {
			$d1 = "CCP (Capability configuration parameters)";
		} elsif($data->[1] eq "3E") {
			$d1 = "GID1 (Group Identifier level 1)";
		} elsif($data->[1] eq "3F") {
			$d1 = "GID2 (Group Identifier level 2)";
		} elsif($data->[1] eq "40") {
			$d1 = "MSISDN";
		} elsif($data->[1] eq "41") {
			$d1 = "PUCT (Price per unit and currency table)";
		} elsif($data->[1] eq "42") {
			$d1 = "SMSP (Short Message Service Parameters)";
		} elsif($data->[1] eq "43") {
			$d1 = "SMSS (Short Message Service Status)";
		} elsif($data->[1] eq "44") {
			$d1 = "LND (Last Number Dialed)";
		} elsif($data->[1] eq "45") {
			$d1 = "CBMI (Cell Broadcast Message Identifier selection)";
		} elsif($data->[1] eq "46") {
			$d1 = "SPN (Service Provider name)";
		} elsif($data->[1] eq "48") {
			$d1 = "CBMID (Cell Broadcast Message Identifier for Data Download)";
		} elsif($data->[1] eq "49") {
			$d1 = "SDN (Service Dialing Numbers)";
		} elsif($data->[1] eq "4A") {
			$d1 = "EXT1 (Extension1)";
		} elsif($data->[1] eq "4B") {
			$d1 = "EXT2 (Extension2)";
		} elsif($data->[1] eq "4C") {
			$d1 = "EXT3 (Extension3)";
		} elsif($data->[1] eq "74") {
			$d1 = "BCCH (Broadcast control channels)";
		} elsif($data->[1] eq "78") {
			$d1 = "ACC (Access control class)";
		} elsif($data->[1] eq "7B") {
			$d1 = "FPLMN (Forbidden PLMNs)";
		} elsif($data->[1] eq "7E") {
			$d1 = "LOCI (Location Information)";
		} elsif($data->[1] eq "AD") {
			$d1 = "AD (Administrative Data)";
		} elsif($data->[1] eq "AE") {
			$d1 = "PHASE (Phase identification)";
		} elsif($data->[1] eq "B1") {
			$d1 = "VGCS (Voice Group Call Service)";
		} elsif($data->[1] eq "B2") {
			$d1 = "VGCSS (Voice Group Call Service Status)";
		} elsif($data->[1] eq "B3") {
			$d1 = "VBS (Voice Broadcast Service)";
		} elsif($data->[1] eq "B4") {
			$d1 = "VBSS (Voice Broadcast Service Status)";
		} elsif($data->[1] eq "B5") {
			$d1 = "eMLPP (enhanced Multi Level Preemption and Priority)";
		} elsif($data->[1] eq "B6") {
			$d1 = "AAeM (Automatic Answer for eMLPP Service)";
		} elsif($data->[1] eq "B7") {
			$d1 = "ECC (Emergency Cell Codes)";
		} elsif($data->[1] eq "50") {
			$d1 = "CBMIR (Cell Broadcast Message Identifier Range Selection)";
		}
	} else {
		warning "Unknown file type ".$data->[0]." in GSM SELECT";
	}
	return "$d0; $d1";
}
