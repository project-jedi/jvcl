#----------------------------------------------#
#                                              #
# JVCL Resources                               #
# generates all .dcr and .res files            #
#                                              #
#----------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#----------------------------------------
RSRC = ..\Resources
#-----------------------------------------------
MAKE = "$(ROOT)\bin\make.exe" -$(MAKEFLAGS) -f$**
BRC = "$(ROOT)\bin\brc32.exe" -r

.rc.res:
  @echo [Compiling: $&.res]
  attrib -r $(RSRC)\$&.res
  @$(BRC) -fo$(RSRC)\$&.res $&.rc

.rc.dcr:
  @echo [Compiling: $&.dcr]
  attrib -r $(RSRC)\$&.dcr
  @$(BRC) -fo$(RSRC)\$&.dcr $&.rc

.path.res = $(RSRC)
.path.dcr = $(RSRC)
.path.rc = ..\images

#-----------------------------------------------
default: Display ResGenerate DcrGenerate

ResGenerate: \
JvBaseEdits.res \
JvCalc.res \
JvCheckmarks.res \
JvColorCombo.res \
JvComponentPanel.res \
JvConsts.res \
JvCSVBase.res \
JvCtrls.res \
JvDBControls.res \
JvDBGrid.res \
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
JvOutlookBar.res \
JvPageSetupTitled.res \
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
JvXPBar.res \
JvXPCore.res \
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
JvDockingReg.dcr \
JvDotNetCtrlsReg.dcr \
JvEDIDBBufferingReg.dcr \
JvGlobusReg.dcr \
JvHMIReg.dcr \
JvInterpreterReg.dcr \
JvJansReg.dcr \
JvManagedThreadsReg.dcr \
JvMMReg.dcr \
JvNetReg.dcr \
JvPageListTreeViewReg.dcr \
JvPluginReg.dcr \
JvPreviewReg.dcr \
JvStdCtrlsReg.dcr \
JvSystemReg.dcr \
JvTimeFrameWorkReg.dcr \
JvUIBReg.dcr \
JvValidatorsReg.dcr \
JvWizardReg.dcr \
JvXPCtrlsReg.dcr \
JvInspectorReg.dcr

Display:
	@echo [Generating: Resources]

#--- RES ---------------------------------------
JvBaseEdits.res: JvBaseEdits.rc
JvCalc.res: JvCalc.rc
JvCheckmarks.res: JvCheckmarks.rc
JvColorCombo.res: JvColorCombo.rc
JvComponentPanel.res: JvComponentPanel.rc
JvConsts.res: JvConsts.rc
JvCSVBase.res: JvCSVBase.rc
JvCtrls.res: JvCtrls.rc
JvDBControls.res: JvDBControls.rc
JvDBGrid.res: JvDBGrid.rc
JvDice.res: JvDice.rc
JvEnterTab.res: JvEnterTab.rc
JvErrorIndicator.res: JvErrorIndicator.rc
JvGammaPanel.res: JvGammaPanel.rc
JvgButton.res: JvgButton.rc
JvgCaption.res: JvgCaption.rc
JvgCheckBox.res: JvgCheckBox.rc
JvgHint.res: JvgHint.rc
JvgLabelEditorForm.res: JvgLabelEditorForm.rc
JvgTreeView.res: JvgTreeView.rc
JvLED.res: JvLED.rc
JvOutlookBar.res: JvOutlookBar.rc
JvPageSetupTitled.res: JvPageSetupTitled.rc
JvPickDate.res: JvPickDate.rc
JvPluginWiz.res: JvPluginWiz.rc
JvRegistryTreeView.res: JvRegistryTreeView.rc
JvScrollMax.res: JvScrollMax.rc
JvSegmentedLEDDisplay.res: JvSegmentedLEDDisplay.rc
JvSimImages.res: JvSimImages.rc
JvSlider.res: JvSlider.rc
JvSpin.res: JvSpin.rc
JvSwitch.res: JvSwitch.rc
JvTimeLine.res: JvTimeLine.rc
JvTipOfDay.res: JvTipOfDay.rc
JvTMTimeLine.res: JvTMTimeLine.rc
JvToolEdit.res: JvToolEdit.rc
JvWinampLabel.res: JvWinampLabel.rc
JvxCheckListBox.res: JvxCheckListBox.rc
JvxSlider.res: JvxSlider.rc
JvXPBar.res: JvXPBar.rc
JvXPCore.res: JvXPCore.rc

#--- DCR ---------------------------------------
JvAppFrmReg.dcr: JvAppFrmReg.rc
JvBDEReg.dcr: JvBDEReg.rc
JvCmpReg.dcr: JvCmpReg.rc
JvCustomReg.dcr: JvCustomReg.rc
JvCoreReg.dcr: JvCoreReg.rc
JvCryptReg.dcr: JvCryptReg.rc
JvCtrlsReg.dcr: JvCtrlsReg.rc
JvDBReg.dcr: JvDBReg.rc
JvDlgsReg.dcr: JvDlgsReg.rc
JvEDIDBBufferingReg.dcr: JvEDIDBBufferingReg.rc
JvInterpreterReg.dcr: JvInterpreterReg.rc
JvHMIReg.dcr: JvHMIReg.rc
JvMMReg.dcr: JvMMReg.rc
JvNetReg.dcr: JvNetReg.rc
JvStdCtrlsReg.dcr: JvStdCtrlsReg.rc
JvSystemReg.dcr: JvSystemReg.rc
JvPluginReg.dcr: JvPluginReg.rc
JvJansReg.dcr: JvJansReg.rc
JvGlobusReg.dcr: JvGlobusReg.rc
JvUIBReg.dcr: JvUIBReg.rc
JvPageListTreeViewReg.dcr: JvPageListTreeViewReg.rc
JvPreviewReg.dcr: JvPreviewReg.rc
JvValidatorsReg.dcr: JvValidatorsReg.rc
JvWizardReg.dcr: JvWizardReg.rc
JvTimeFrameWorkReg.dcr: JvTimeFrameWorkReg.rc
JvManagedThreadsReg.dcr: JvManagedThreadsReg.rc
JvXPCtrlsReg.dcr: JvXPCtrlsReg.rc
JvDotNetCtrlsReg.dcr: JvDotNetCtrlsReg.rc
JvDockingReg.dcr: JvDockingReg.rc
JvInspectorReg.dcr: JvInspectorReg.rc
