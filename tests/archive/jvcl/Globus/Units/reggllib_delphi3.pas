{
  
  							          
 	      Freeware Delphi VCL Extensions Library	           
 		          ' GLOBUS LIB '		           
  		                                                  
        Copyright (c) 1998-2001 Chudin A.V, FidoNet: 1246.16	  
  	   chudin@biblio-globus.ru; avchudin@yandex.ru            
  
  this unit contains registration procedures for Delphi 3
  Delphi 3 is obsolete. Use Delphi 5-7.
}
unit RegGlLib_Delphi3;
interface

// {$DEFINE INC_ALPHA_UNITS} // compile untested components

procedure Register;

implementation
{$R *.DCR}
uses Classes, DsgnIntf,
     glBevel,glButton,glCBox,glCapt,glDigits,glFlyTxt,glGrBox,glImage,glJump,
     glLabel,glLBox,glALBox,glPage,glprgrs,glTab,glSBox,glTView,glShadow,
     glHint,gl3DCol,glProcs,glSGrid,glEdit,glRuler,glCTable,glDBGrid,glVDBGrd,glStringContainer,
     glMRes, mresed, glSysInf, glPropCn, geCList, glReport, geReport, geRPForm, glGBtn, 
     glRLabel, glBitBtn, glHShape, glSmallFontsDefence;

procedure Register;
begin
  RegisterComponents( 'Gl Controls', [ TglBevel, TglButton, TglGraphicButton, TglCheckBox,
		       TglDigits, TglShadow, TglFlyingText, TglGroupBox, TglBitmapImage,
		       TglLabel, TglBitBtn, TglMaskEdit, TglStaticText, TglListBox,
		       TglCheckListBox, TglAskListBox,
		       TglPageControl, TglTabControl, TglScrollBox, TglHoleShape,
		       TglTreeView, TglCheckTreeView, TglProgress, TglRuler, TglStringGrid ] );

  RegisterComponents( 'Gl Components', [ Tgl3DColors, TglCaption,
		      TglHint, TglJumpingComponent, TglProcess, TglStringContainer,
		      TglMultiResources , TglPropertyCenter, TglSysInfo,
		      TglReport, TglReportEditor, TglReportParamsEditor,
                      TglSmallFontsDefence] );

  {$IFDEF INC_ALPHA_UNITS}
  RegisterComponents( 'Gl DBAware', [ TglDBGrid, TglVertDBSGrid, TglPrintCrossTable ] );
  {$ENDIF};

  {RegisterComponents('Gl ExportImport', [TglExportExcel, TglExportDBETable]);}

  RegisterComponents( 'Gl QReport', [ TglQRLabel, TglQRDBText ] );

//  RegisterPropertyEditor(TypeInfo(string), TglEdit, 'EditMask', TMaskProperty);
//  RegisterPropertyEditor(TypeInfo(string), TglProcess, 'FileName', TFilenameProperty);
  RegisterPropertyEditor( TypeInfo(TResStringList), TglMultiResources, 'Resources', TglResourcesProperty );

  RegisterComponentEditor(TglPropertyCenter, TglComponentListEditor);
  RegisterPropertyEditor( TypeInfo(TStringList), TglPropertyCenter, 'ComponentList', TglComponentListProperty );

  RegisterComponentEditor(TglReport, TglRep_Editor);
  RegisterPropertyEditor( TypeInfo(TStringList), TglReport, 'Report', TglRep_Property );
  RegisterComponentEditor(TglReportEditor, TglRep_Editor);
  RegisterComponentEditor(TglReportParamsEditor, TglRepParams_Editor);

end;

end.