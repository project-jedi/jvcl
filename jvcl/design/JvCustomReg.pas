{$I JVCL.INC}

unit JvCustomReg;

interface

procedure Register;

implementation
uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  ExptIntf, ToolsApi,
  JclSchedule, 

  JvTrayIcon, JvGammaPanel, JvInspector, JvLinkLabel,
  JvBandForms, JvBandObjectDLLWizard, JvBandObjectDLLWizardForm, 
  JvLookOut, JvOutlookBar, JvRadioControl, JvScheduledEvents, JvThumbImage,
  JvThumbnails, JvThumbviews, JvTimeLine, JvTMTimeLine, JvBalloonHint,
  JvPluginManager, JvPLuginWizard,
  JvValidateEdit, JvEditor, JvHLEditor, JvHLEditorPropertyForm, JvHLParser,
  JvTimeLineEditor, JvHLEditEditor, JvScheduleEditors, JvOutlookBarEditors;


{.$R ..\resources\JvCustomReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Custom',[
    TJvTrayIcon, TJvGammaPanel, TJvLinkLabel,
    TJvInspector, TJvInspectorBorlandPainter, TJvInspectorDotNETPainter,
    TJvLookout, TJvLookOutPage, TJvLookOutButton,
    TJvExpress, TJvExpressButton,
    TJvOutlookBar, TJvRadioControl, TJvScheduledEvents,
    TJvThumbImage, TJvTimeLine, TJvTMTimeLine, TJvBalloonHint,
    TJvPluginManager, TJvValidateEdit,
    TJvEditor, TJvHLEditor, TJvHLEdPropDlg
    ]);
  RegisterCustomModule(TJvBandForm, TCustomModule);
  RegisterPackageWizard(TJvBandObjectDLLWizard.Create);

  RegisterComponentEditor(TJvHLEdPropDlg, TJvHLEdPropDlgEditor);
  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarComponentEditor);
  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);

  RegisterPropertyEditor(TypeInfo(Integer),
    TJvCustomOutlookBar, 'ActivePageIndex', TJvOutlookBarActivePageEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages),
    TJvCustomOutlookBar, '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarButtons),
    TJvOutlookBarPage, '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer),
    TJvOutlookBarButton, 'ImageIndex', TJvOutlookBarButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TJvColors),
    TJvHLEditor, 'Colors', TJvHLEditorColorProperty);
  RegisterPropertyEditor(TypeInfo(IJclSchedule), TJvEventCollectionItem, 'Schedule', TJvSchedulePropertyEditor);

  RegisterComponentEditor(TJvCustomScheduledEvents, TJvSchedEventComponentEditor);
  RegisterLibraryExpert(TJvPluginWizard.Create)
end;

end.
