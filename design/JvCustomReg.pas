{$I JVCL.INC}

unit JvCustomReg;

interface

procedure Register;

implementation
uses
  Classes, ImgList,
  {$IFDEF COMPILER6_UP}
  FiltEdit, DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  ExptIntf, ToolsApi,
  JclSchedule, 

  JvTrayIcon, JvGammaPanel, JvInspector, JvLinkLabel,
  JvBandForms, JvBandObjectDLLWizard, JvBandObjectDLLWizardForm, 
  JvLookOut, JvOutlookBar, JvRadioControl, JvScheduledEvents, JvThumbImage,
  JvThumbnails, JvThumbviews, JvTimeLine, JvTMTimeLine, JvBalloonHint,
  JvPlugin, JvPluginManager, JvPluginWizard,
  JvValidateEdit, JvEditor, JvHLEditor, JvHLEditorPropertyForm, JvHLParser,
  JvTimeLineEditor, JvHLEditEditor, JvScheduleEditors, JvOutlookBarEditors, JvLookoutEditor;


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
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvLookoutButton,
    'ImageIndex', TJvLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvExpressButton,
    'ImageIndex', TJvLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvThumbView,
    'Filter', TFilterProperty);

  RegisterComponentEditor(TJvHLEdPropDlg, TJvHLEdPropDlgEditor);
  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarComponentEditor);
  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);
  RegisterComponentEditor(TJvLookOut, TJvLookOutEditor);
  RegisterComponentEditor(TJvLookOutPage, TJvLookOutPageEditor);
  RegisterComponentEditor(TJvExpress, TJvExpressEditor);
  RegisterComponentEditor(TJvCustomScheduledEvents, TJvSchedEventComponentEditor);

  RegisterCustomModule(TJvBandForm, TCustomModule);
  RegisterPackageWizard(TJvBandObjectDLLWizard.Create);
  RegisterLibraryExpert(TJvPluginWizard.Create)
end;

end.
