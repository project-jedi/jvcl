#----------------------------------------------#
#                                              #
# JVCL Resources                               #
# generates all .dcr and .res files            #
#                                              #
#----------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)
!endif
#----------------------------------------
RSRC = ..\Resources
#-----------------------------------------------
MAKE = $(ROOT)\make.exe -$(MAKEFLAGS) -f$**
BRC = $(ROOT)\brc32.exe -r
#-----------------------------------------------
default: ResGenerate DcrGenerate

ResGenerate: \
JvBaseEdits.res \
JvCalc.res \
JvCheckmarks.res \
JvColorCombo.res \
JvComponentPanel.res \
JvConsts.res \
JvCSVBase.res \
JvCtrls.res \
JvDBCtrl.res \
JvDice.res \
JvEnterTab.res \
JvErrorIndicator.res \
JvGammaPanel.res \
JvgButton.res \
JvgCaption.res \
JvgCheckBox.res \
JvgHint.res \
JvgLabelEditorForm.res \
JvgTreeView.res \
JvLED.res \
JvLinkLabel.res \
JvOutlookBar.res \
JvPageSetupTitledEng.res \
JvPageSetupTitledRus.res \
JvPickDate.res \
JvPluginWiz.res \
JvRegistryTreeView.res \
JvScrollMax.res \
JvSegmentedLEDDisplay.res \
JvSimImages.res \
JvSlider.res \
JvSpin.res \
JvSwitch.res \
JvTimeLine.res \
JvTipOfDay.res \
JvTMTimeLine.res \
JvToolEdit.res \
JvWinampLabel.res \
JvxCheckListBox.res \
JvxSlider.res

DcrGenerate: \
JvAppFrmReg.dcr \
JvBDEReg.dcr \
JvCmpReg.dcr \
JvCustomReg.dcr \
JvCoreReg.dcr \
JvCryptReg.dcr \
JvCtrlsReg.dcr \
JvDBReg.dcr \
JvDlgsReg.dcr \
JvInterpreterReg.dcr \
JvHMIReg.dcr \
JvMMReg.dcr \
JvNetReg.dcr \
JvStdCtrlsReg.dcr \
JvSystemReg.dcr \
JvPluginReg.dcr \
JvJansReg.dcr \
JvGlobusReg.dcr \
JvUIBReg.dcr \
JvPageListTreeViewReg.dcr \
JvPreviewReg.dcr \
JvValidatorsReg.dcr \
JvWizardReg.dcr \
JvTimeFrameWorkReg.dcr \
JvManagedThreadsReg.dcr

#--- RES ---------------------------------------
JvBaseEdits.res: JvBaseEdits.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvCalc.res: JvCalc.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvCheckmarks.res: JvCheckmarks.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvColorCombo.res: JvColorCombo.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvComponentPanel.res: JvComponentPanel.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvConsts.res: JvConsts.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvCSVBase.res: JvCSVBase.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvCtrls.res: JvCtrls.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvDBCtrl.res: JvDBCtrl.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvDice.res: JvDice.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvEnterTab.res: JvEnterTab.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvErrorIndicator.res: JvErrorIndicator.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvGammaPanel.res: JvGammaPanel.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvgButton.res: JvgButton.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvgCaption.res: JvgCaption.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvgCheckBox.res: JvgCheckBox.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvgHint.res: JvgHint.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvgLabelEditorForm.res: JvgLabelEditorForm.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvgTreeView.res: JvgTreeView.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvLED.res: JvLED.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvLinkLabel.res: JvLinkLabel.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvOutlookBar.res: JvOutlookBar.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvPageSetupTitledEng.res: JvPageSetupTitledEng.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvPageSetupTitledRus.res: JvPageSetupTitledRus.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvPickDate.res: JvPickDate.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvPluginWiz.res: JvPluginWiz.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvRegistryTreeView.res: JvRegistryTreeView.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvScrollMax.res: JvScrollMax.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvSegmentedLEDDisplay.res: JvSegmentedLEDDisplay.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvSimImages.res: JvSimImages.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvSlider.res: JvSlider.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvSpin.res: JvSpin.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvSwitch.res: JvSwitch.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvTimeLine.res: JvTimeLine.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvTipOfDay.res: JvTipOfDay.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvTMTimeLine.res: JvTMTimeLine.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvToolEdit.res: JvToolEdit.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvWinampLabel.res: JvWinampLabel.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvxCheckListBox.res: JvxCheckListBox.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

JvxSlider.res: JvxSlider.rc
	$(BRC) -fo$(RSRC)\$&.res $&.rc

#--- DCR ---------------------------------------
JvAppFrmReg.dcr: JvAppFrmReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvBDEReg.dcr: JvBDEReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvCmpReg.dcr: JvCmpReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvCustomReg.dcr: JvCustomReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvCoreReg.dcr: JvCoreReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvCryptReg.dcr: JvCryptReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvCtrlsReg.dcr: JvCtrlsReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvDBReg.dcr: JvDBReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvDlgsReg.dcr: JvDlgsReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvInterpreterReg.dcr: JvInterpreterReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvHMIReg.dcr: JvHMIReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvMMReg.dcr: JvMMReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvNetReg.dcr: JvNetReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvStdCtrlsReg.dcr: JvStdCtrlsReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvSystemReg.dcr: JvSystemReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvPluginReg.dcr: JvPluginReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvJansReg.dcr: JvJansReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvGlobusReg.dcr: JvGlobusReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvUIBReg.dcr: JvUIBReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvPageListTreeViewReg.dcr: JvPageListTreeViewReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvPreviewReg.dcr: JvPreviewReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvValidatorsReg.dcr: JvValidatorsReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvWizardReg.dcr: JvWizardReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvTimeFrameWorkReg.dcr: JvTimeFrameWorkReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc

JvManagedThreadsReg.dcr: JvManagedThreadsReg.rc
	$(BRC) -fo$(RSRC)\$&.dcr $&.rc
