#!/bin/sh
#
# shell script to build all JVCLX packages for K3 
#
# André Snepvangers, 2004-09-13
#
eval `grep 'DelphiRoot=' ~/.borland/delphi69rc`
DELPHI=$DelphiRoot
export DELPHI
echo DELPHI = $DELPHI
source "$DELPHI/bin/kylixpath" >/dev/null
cd packages/K3
echo [Building JvQ3rdK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQ3rdK3R.dpk
echo [Building JvQCoreK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCoreK3R.dpk
echo [Building JvQCoreK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCoreK3D.dpk
echo [Building JvQSystemK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQSystemK3R.dpk
echo [Building JvQSystemK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQSystemK3D.dpk
echo [Building JvQCtrlsK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCtrlsK3R.dpk
echo [Building JvQCustomK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCtrlsK3D.dpk
echo [Building JvQCmpK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCmpK3R.dpk
echo [Building JvQCmpK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCmpK3D.dpk
echo [Building JvQCustomK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCustomK3R.dpk
echo [Building JvQCtrlsK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCustomK3D.dpk
echo [Building JvQDlgsK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQDlgsK3R.dpk
echo [Building JvQDlgsK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQDlgsK3D.dpk
echo [Building JvQStdCtrlsK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQStdCtrlsK3R.dpk
echo [Building JvQStdCtrlsK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQStdCtrlsK3D.dpk
echo [Building JvQAppFrmK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQAppFrmK3R.dpk
echo [Building JvQAppFrmK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQAppFrmK3D.dpk
echo [Building JvQCryptK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCryptK3R.dpk
echo [Building JvQCryptK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQCryptK3D.dpk
echo [Building JvQJansK3R.dpk]]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQJansK3R.dpk
echo [Building JvQJansK3D.dpk]]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQJansK3D.dpk
echo [Building JvQManagedThreadsK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQManagedThreadsK3R.dpk
echo [Building JvQManagedThreadsK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQManagedThreadsK3D.dpk
echo [Building JvQMMK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQMMK3R.dpk
echo [Building JvQMMK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQMMK3D.dpk
echo [Building JvQHMIK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQHMIK3R.dpk
echo [Building JvQHMIK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQHMIK3D.dpk
echo [Building JvQNetK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQNetK3R.dpk
echo [Building JvQNetK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQNetK3D.dpk
echo [Building JvQXPCtrlsK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQXPCtrlsK3R.dpk
echo [Building JvQXPCtrlsK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQXPCtrlsK3D.dpk
echo [Building JvQWizardK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQWizardK3R.dpk
echo [Building JvQWizardK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQWizardK3D.dpk
echo [Building JvQPageCompsK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQPageCompsK3R.dpk
echo [Building JvQPageCompsK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQPageCompsK3D.dpk
echo [Building JvQEDIK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQEDIK3R.dpk
echo [Building JvQEDIK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQEDIK3D.dpk
echo [Building JvQUIBK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQUIBK3R.dpk
echo [Building JvQUIBK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQUIBK3D.dpk
echo [Building JvQValidatorsK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQValidatorsK3R.dpk
echo [Building JvQValidatorsK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQValidatorsK3D.dpk
echo [Building JvQInspectorK3R.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQInspectorK3R.dpk
echo [Building JvQInspectorK3D.dpk]
dcc -Q -I../../common -U../../qcommon,../../qdesign,../..qrun JvQInspectorK3D.dpk
mv JvQ*.dcp $DELPHI/lib
mv bplJvQ* $DELPHI/bin
echo [Finished building JVCLX packages]



