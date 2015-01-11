:- module(refcodes,[ refcode/3 ]).
/** <module> Humdrum reference code database

This module is not indended for direct use by client code.
Use humdrum:hum_prop_desc/2.
*/

%% refcode(+Code:atom, -Type:atom, -Desc:atom) is semidet.
%% refcode(-Code:atom, -Type:atom, -Desc:atom) is nondet.
%
%  Catalogue of known comment codes and their meanings.
	refcode('COM',name,'composer\'s name').
	refcode('COM1',name,'first composer\'s name').
	refcode('COM2',name,'second composer\'s name').
	refcode('COM3',name,'third composer\'s name').
	refcode('CDT',dates,'composer\'s dates').
	refcode('CDT1',dates,'first composer\'s dates').
	refcode('CDT2',dates,'second composer\'s dates').
	refcode('CDT3',dates,'third composer\'s dates').
	refcode('CNT',country,'composer\'s nationality').
   refcode('COC',name,'composer\'s corporate name').
	refcode('COL',name,'composer\'s alias or stage-name').
	refcode('COA',name,'attributed composer').
	refcode('COS',name,'suspected composer').

	refcode('LYR',text,'lyricist').
	refcode('LIB',text,'librettist').
	refcode('LAR',text,'arranger').
	refcode('LOR',text,'orchestrator').
   refcode('LDT',date,'lyric date').

	refcode('TXO',lang,'original language of vocal/choral text').
	refcode('TXL',lang,'language of encoded vocal/choral text').
	refcode('TRN',name,'translator of vocal/choral text').

	refcode('MPN',name,'performer\'s name').
	refcode('MPS',name,'suspected performer').
	refcode('MRD',date,'date of performance').
	refcode('MLC',place,'place of performance').
	refcode('MCN',name,'conductor').
	refcode('MPD',date,'date of first performance').

	refcode('RTL',text,'title of album').
	refcode('RMM',name,'manufacturer or sponsoring company').
	refcode('RC#',text,'recording company\'s catalogue number').
	refcode('RRD',date,'date of release').
	refcode('RDT',date,'date of recording').
	refcode('RLC',place,'place of recording').
	refcode('RNP',name,'producer\'s name').
	refcode('RT#',number,'track number').

	refcode('OTL',text,'title (in original language)').
	refcode('OTP',text,'popular title').
	refcode('OCL',name,'collector').
	refcode('OCO',text,'commission').
	refcode('ODE',text,'dedication').
	refcode('OKY',text,'key').
	refcode('OTA',text,'alternative title').
	refcode('OPT',text,'title of parent work').
	refcode('OPR',text,'title of parent work').
	refcode('XEN',text,'title (English translation)').
	refcode('XFR',text,'title (French translation)').
	refcode('XDE',text,'title (German translation)').
	refcode('XNI',text,'title (Japanese translation)').
	refcode('OAC',number,'act number').
	refcode('OSC',number,'scene number').
	refcode('OMV',number,'movement number').
	refcode('OMD',text,'movement designation').
	refcode('OPS',text,'opus number').
	refcode('ONM',number,'number').
	refcode('OVM',number,'volume').
	refcode('ONB',text,'note').
	refcode('ODT',date,'date of composition').
	refcode('OPC',place,'place of composition').
	refcode('OCY',country,'country of composition').

	refcode('GTL',text,'group title').
	refcode('GAW',text,'associated work').

	refcode('PUB',text,'publication status').
	refcode('PPR',text,'first publisher').
	refcode('PPG',text,'page').
	refcode('PDT',date,'date first published').
	refcode('PPP',place,'place first published').
	refcode('PC#',text,'publisher\'s catalogue number').

	refcode('SCT',text,'scholarly catalogue name & number').
	refcode('SCA',text,'scholarly cataloge name (unabbreviated)').
	refcode('SMS',text,'manuscript source name').
	refcode('SML',place,'manuscript location').
	refcode('SMA',text,'acknowledgement of manuscript access').


	refcode('EED',name,'electronic editor').
	refcode('EEV',text,'electronic edition version').
	refcode('EFL',text,'file number, e.g. 1 of 4 (1/4)').
	refcode('EMD',text,'document modification description').
	refcode('EST',text,'encoding status').

	refcode('GCO',text,'collection designation').
	refcode('GNM',number,'group number').

	refcode('YEP',name,'publisher of electronic edition').
	refcode('YED',date,'last edited').
	refcode('YEC',text,'date & owner of electronic copyright').
	refcode('YER',date,'date electronic edition released').
	refcode('YEM',text,'copyright message').
	refcode('YEN',country,'country of copyright').
	refcode('ENC',name,'encoder of electronic document').
   refcode('URL',url,'URL').
	refcode('VTS',number,'checksum validation number').
	refcode('YOR',text,'original document').
	refcode('YOO',name,'original document owner').
	refcode('YOY',date,'original copyright year').
	refcode('YOE',name,'original editor').
	refcode('END',date,'initial encoding date').

	refcode('ACO',text,'analytic collection designation').
	refcode('AMT',metre,'metric classification').
	refcode('AIN',list(instr),'instrumentation').
	refcode('AFR',text,'form designation').
	refcode('AGN',text,'genre designation').
	refcode('AMD',text,'mode designation').
	refcode('AST',text,'style, period or type').
	refcode('ARE',place,'geographic region of origin').
	refcode('ARL',place,'geographic location of origin').
   refcode('ASW',text,'associated work').

	refcode('HAO',text,'aural history').
	refcode('HTX',text,'free-form translation of vocal text').

	refcode('RLN',text,'ASCII language setting').
	refcode('RDF',text,'user-defined signifiers').
	refcode('RDT',date,'date encoded').
	refcode('RNB',text,'representation note').
	refcode('RWG',text,'representation warning').

ref_type(metre,V) :- member(V,[simple,compound, duple,triple,quadruple,irregular]).
ref_type(mode,V) :- member(V,[
   dorian,hypodorian,phrygian,hypophrygian,lydian,mixolydian,
   hypomixolydian,ionian,hypoionian,aeolian,hypoaeolian]).

