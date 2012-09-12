unit MainFm;

interface

{New JVCL3 demo showing how to do Docking from Code, and also
 showing use of new center dock area.

This is a demo of the new docking features added to JvDocking by Warren Postma.

You can now dock on the Center of the form, something a few people have
requested, and I particularly need.  Other docking libraries can handle this
without any problems, so why can't JvDocking?  The problem with JvDocking before
was that there was an implicit assumption by the original designer that only the
edges should be dockable, and that each docking area should have an associated
splitter.  Thus, the implementation relied on a 1:1 relationship of the
TControl.Align enum values alLeft,alRight, alTop, alBottom, and one dock panel
was created for each of these. I simply added one more, and then found and
fixed the fallout.

So one more area has been added, with the ugly property name of "CustomDockPanel". For this panel to exist (be non-nil), you must add an event handler to the
newly added TJvDockServer.OnCustomPanel event.  This main form demo shows
how to do that, and also a lot of other docking-from-code features


**** Notes on Internal Architecture of JvDocking: ****

(1) The form that hosts the docking ability must contain a TJvDockServer
    component.

(2) Each client form that wants to dock to the docking host must have a
    TJvDockClient.

(3) Extreme Polymorphism:
    Both the client and the dockserver require a link to a style component,
    which derives from the TJvDockStyle base class. For example, the
    JvDockVIDStyle which implements a series of classes. These classes are
    passed into the basic TJvDockServer object, and when the dock server
    needs to create an instance of a class, rather than having a hard coded
    class type it creates, it creates whatever class was specified by the
    TJvDockXXXStyle you are currently using.

(4)  The JvDockServer originally had TJvDockPanels at the Top, Bottom, Left
     and Right side of the main host form, but not a center panel, this was
     added by Warren.

(5) Client forms are docked using their ancestor, TWinControl, a base
    VCL class, because the VCL docking interface uses this as a
    parameter type for flexibility. Well enough.  However in practice,
    users will be dropping a TJvDockClient only onto TForms. The code
    at various places expects the TJvDockClient to be a non-visual component
    inside a form, and so we can only *un-dock* a  TForm.
    Interestingly you can manually dock a TForm that does not contain a
    TJvDockClient, but once docked, you can't drag the form OUT of the
    docking area, since this functionality is part of
    the TJvDockClient. This manual docking function (TControl.ManualDock)
    would not invoke any JvDocking code at all, which is why this happens.
    This is one of the reasons that telling end-users to to call
    TControl.ManualDock to dock a form at runtime in code is problematic.

(6) You can dock any number of items beside each other, resizing each using splitters,
and no outer container form is needed if the docking occurs inside the main
form, that is, inside the form that contains the TJvDockServer, and if only
side-by-side docking is used.  There are two cases where such a container form
is needed: Either form tabbed docking, or for conjoined docking.  In the case
of tabbed+conjoined, you actually have multiple levels of nested container
forms before you actually get to the controls. Sound complicated? It is.

(7) However, another form of docking is provided, which is known in JvDocking
as "Conjoining". It appears that Conjoin in the context of JvDocking means to
join together two windows while they are floating, so that they have, in
effect their own virtual DockServer and they aren't connected to the form
that contains the JvDockServer in any way.  These docked windows are
contained within a TJvDockConjoinHostForm and 1 or more  TJvDockConjoinPanel
components.

(8) Either one of the docking areas on the main form, or a Conjoined
(floating container form) set of docked controls can be docked either in
a side-by-side or tabbed fashion.  If they are tabbed, things are more complex
internally.

(9) One strange side effect of the Tabbed way of docking is that the close button
which appears in the pseudo-titlebar area (called the Grabber internally) closes
ALL the docked controls (all pages in this tabbed notebook, that is, all the forms)
rather than just the topmost (currently visible) one.


(10) Even inside the a main form a second inner container form is created
if you have any tabs. A form of type TJvDockTabHostForm is created,
which contains a page control (FPageControl:TJvDockTabPageControl)
for when you drag one page on top of another page. This creates a
tabbed notebook of docked forms. There is no clean way of doing this
from code, only a sequence of user-invoked mouse clicks and drag
operations can currently do this in the API, and this is a
failure of the API, in my opinion. I intend to fix this, and other places
where I think there exists currently no clean way, or no way at all,
to do certain things programmatically(with code) instead of with mouse
clicks+drags.


****  Notes on Internals of JvDocking that Warren feels need fixing up: *****

  #1: GLOBALS, potential Access Violations, and debug Assertions to guard them.

    In JvDockGlobals.pas there are global dock manager and dock client pointers.
    These must be set to non-nil  values during drag/drop operations but they
    usually used without nil checks first.

    It seems that internally there are not  enough Assert() checks anywhere
    in this code, espeically before global pointers are dereferenced,
    and I feel  uncomfortable with this.

    Adding some Assert() stuff could prevent  Access Violations if there
    are any problems where handling of  JvDocking  globals are concerned.
    Better to have a description of the problem  than a non-descript
    Access Violation to debug, and in code this complex, it's better to complain
    sooner rather than later of any errors.

    Docking libraries are hideously complex internally, and bugs are easy to
    introduce. Take the unstable VCL 'ActionMainMenu' as an example
    of the sort of  crap I am trying to avoid putting into my applications!

    In general, a lot of debug assertions ( Assert(Assigned(FXyz)) ) should be
    added since this is a REALLY complex and not-very-well commented component,
    I have a hard time believing there are no memory/pointer access problems
    in this component set.


    #2: Add a dock style to a form, connect it to the TjvDockManager, then
      delete it from the form, access violations ensue. This is a design-time
      access violation that I might have created by my changes. I am looking
      into this further.

}
uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Contnrs,
  Spin,

  DocFm,

  JvComponent,
  JvDockControlForm,
  JvDockVIDVCStyle,
  JvDockVIDStyle,
  JvDockDelphiStyle,
  JvDockVSNetStyle,
  JvAppStorage,
  JvAppIniStorage,
  JvExExtCtrls,
  JvSplitter,
  JvDockTree,
  JvComponentBase;


const
  MinWidth=500;
type
  TLastClickEnum = (lcNone,lcConjoin,lcSibling,lcTabbed);

  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    dockServer: TJvDockServer;
    ButtonSibDock: TButton;
    JvDockVIDStyle1: TJvDockVIDStyle;
    DockIniStorage: TJvAppIniFileStorage;
    ButtonCreateTabDock: TButton;
    ButtonCreateConjoin: TButton;
    Panel3: TPanel;
    MemoTrace: TMemo;
    JvSplitter1: TJvSplitter;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    tbDockRightSide: TCheckBox;
    cbWorkaround: TCheckBox;
    procedure dockServerCustomPanel(Sender: TJvDockServer;
      var aParent: TWinControl; var Align: TAlign);
    procedure ButtonSibDockClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dockServerFinishSetDockPanelSize(DockPanel: TJvDockPanel);
    procedure dockServerGetClientAlignSize(Align: TAlign;
      var Value: Integer);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonCreateTabDockClick(Sender: TObject);
    procedure ButtonCreateConjoinClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure tbDockRightSideClick(Sender: TObject);
  private
    { Private declarations }
    FColors : Array of TColor;
    FIndex :Integer;
    FDocumentFormIndex:Integer; // Give each form a different caption.
    FDocs:TObjectList;
    FLastClick:TLastClickEnum;
    procedure Trace(msg:String);

    function MakeNewDocFm:TDocForm;

    procedure clearOldDocs;


  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}
uses JvDockAdvTree;


procedure TMainForm.dockServerCustomPanel(Sender: TJvDockServer;
  var aParent: TWinControl; var Align: TAlign);
begin
  aParent := Panel2; // Set up custom docking area!

end;

procedure TMainForm.Trace(msg:String);
begin
  MemoTrace.Lines.Add(msg);
  OutputDebugSTring(PChar(msg));
end;

function TMainForm.MakeNewDocFm:TDocForm;
begin
  result := TDocForm.Create(nil);
  result.Visible := false;

  result.DockClient.DockStyle := DockServer.DockStyle;
  result.sg.Color := FColors[FIndex];
  result.OnTrace := Trace;


  Inc(FDocumentFormIndex);
  result.Caption := 'Document Form #'+IntToStr(FDocumentFormIndex);
  result.Name := 'DocumentForm'+IntToStr(FDocumentFormIndex);

  FDocs.Add(result);
  FIndex := FDocs.Count;
  if (FIndex>=Length(FColors)) then FIndex := 0;

  result.Top := result.Top + (FIndex *  10);
  result.Left := result.Left + (FIndex *  10);

end;


procedure TMainForm.ButtonSibDockClick(Sender: TObject);
var
 newDocFm:TDocForm;
 besideForm:TWinControl;
// WinControls:TList;
 n : Integer;
 ctrl:TWinControl;
 adef:TAlign;
begin
  if FLastClick<>lcSibling then
    clearOldDocs;

  FLastClick := lcSibling;
  Assert(Assigned(DockServer.DockStyle));

  newDocFm := MakeNewDocFm;


 { Simplest version just puts siblings side by side horizontally: }
 //  newDocFm.ManualDock( DockServer.CustomDockPanel, nil, alNone )

 (* A nicer way to lay things out, a "drill down" viewing method
	suitable for a wide top level document, and a bunch of smaller
	panels below it:   *)

  if tbDockRightSide.Checked then begin
	ctrl := DockServer.RightDockPanel;
	adef := alClient;
	newDocFm.Width := 50;
	newDocFm.sg.FixedCols := 0;
	newDocFm.sg.FixedRows := 0;
	newDocFm.sg.ColCount := 6;
	newDocFm.sg.RowCount := 20;
	newDocFm.sg.ScrollBars := ssNone;
  end else begin
	ctrl := DockServer.CustomDockPanel;
	adef := alNone;
  end;

  n := ctrl.DockClientCount;
  Trace('before New Document Window docked, Docked Clients='+IntToStr(n));

 { here is how we decide how to position the documents when they are docked }
 if n = 0 then begin { first entry }
	if  tbDockRightSide.Checked then
	  Trace('docking first form which should take up entire custom dock area ')
	else
	  Trace('docking first form which should be on the right-side dock area');
	  
  newDocFm.ManualDock( ctrl, nil, adef )
 end else if n = 1 then begin {second entry }
   Trace('docking second form which should be docked below the first form');
   newDocFm.ManualDock( ctrl, ctrl.DockClients[0],  alBottom )
 end else if n >= 2 then begin { third and later entries! }

   Trace('docking additional form which should be docked beside the last form');
   besideForm := TWinControl(ctrl.DockClients[n-1]);
   newDocFm.ManualDock( ctrl, besideForm, alRight );
 end;

  newDocFm.Show;


  TJvDockPanel(ctrl).DockManager.ResetBounds(true);

end;

procedure TMainForm.clearOldDocs;
var
  n:Integer;
  R:TRect;
  dockClient:TJvDockClient;
  ctrl:TControl;
begin
  R.Top := -800;
  R.Left := -800;
  for n := 0 to dockServer.CustomDockPanel.DockClientCount-1 do
  begin
      ctrl := dockServer.CustomDockPanel.DockClients[n];
      if ctrl is TForm then
      begin
         dockClient := FindDockClient(ctrl);
         dockClient.FormUnDock(nil,nil);
      end;

      //dockClient.FormUnDock(nil,nil);
  end;

  // This Frees previously created forms.
  FDocs.Clear;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    SetLengtH( FColors, 5 );
    FColors[0] := $00F9CAD9;
    FColors[1] := $00C8F7FB;
    FColors[2] := $00CBDDF8;
    FColors[3] := $00DBFBE8;
    FColors[4] := $00FDEFD9;

    DockIniStorage.FileName := ExtractFilePath(Application.ExeName)+'CustomTabbedDockingLayout.ini';

    FDocs :=  TObjectList.Create(true);


end;

procedure TMainForm.dockServerFinishSetDockPanelSize(
  DockPanel: TJvDockPanel);
begin
  Trace('OnFinishSetDockPanelSize panel='+DockPanel.Name)
end;

procedure TMainForm.dockServerGetClientAlignSize(Align: TAlign;
  var Value: Integer);
var
 s:String;
begin
    case Align of
           alNone:
              s := 'None';
           alTop:
              s := 'Top';
           alBottom:
              s := 'Bottom';
           alLeft:
              s := 'Left';
           alRight:
              s := 'Right';
           alClient:
              s := 'Client';
           alCustom:
              s := 'Custom';
    end;
	Trace('OnGetClientAlignSize '+s);
	if Value< MinWidth then
		Value := MinWidth;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
 t,n:Integer;
 ctrl :TWinControl;
 s:String;
 strs:TStringLIst;
 tree:TJvDockAdvTree;
 WinControls:TList;
begin
{$ifdef JVDOCK_DEBUG}
 // if you build the JvDock packages with the $define JVDOCK_DEBUG,
 // you can do XML dumps of the entire ADVTree, which is helpful when
 // you're debugging.
  Trace('--- DebugDump starts ---');
    strs := TStringList.Create;
    try

        tree := TJvDockAdvPanel( DockServer.CustomDockPanel ).ADVTree;
        tree.Debug('CustomDockPanel.ADVTree',Strs);


      for t := 0 to strs.Count-1 do begin
          Trace(strs[t]);
      end;

    finally
        strs.Free;
    end;

      Trace('--- DebugDump ends ---');
{$endif}
      
  {

    Note that every VCL Control already has a basic set of docking information
    properties:
        TControl.DockClientCount - number of things docked directly onto this control.
        TControl.DockClient[x] - get the actual docked objects.

    DockServer.<XYZ>DockPanel.GetDockedControls gets you access not only to
    directly docked Dock Client forms, but also gets you anything nested
    inside a set of tabbed-pages, etc. This gives you a simple FLAT
    way to find out what is docked. If you need to know the hierachy stuff,
    you need to go in and write lots of code, use GetDockedControls as your
    template, and good luck.
    
  }

  Trace('--- Introspection starts ---');
  WinControls := TList.Create;
  try
  DockServer.CustomDockPanel.GetDockedControls(WinControls);
  n := WinControls.Count; // WinControls contains pointers to TWinControls.

  Trace('Docked Client Count='+IntToStr(n));

  for t := 0 to n-1 do begin
      ctrl := TWinControl(WInControls.Items[t]);
      if Assigned(ctrl) then
          s := ctrl.Name
      else
          s := '<nil>';
      Trace('WinControls['+intToStr(t)+']='+s );

  end;
  Trace('--- finished. ---');

  finally
      WinControls.Free;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
 SaveDockTreeToAppStorage( DockIniStorage, 'DockingLayout' );
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
 LoadDockTreeFromAppStorage(DockIniStorage, 'DockingLayout' );
end;



procedure TMainForm.ButtonCreateTabDockClick(Sender: TObject);
var
 newDocFm1,newDocFm2:TDocForm;
 tabHost: TJvDockTabHostForm;
 ctrl:TWinControl;
begin
  // clear previous contents
  clearOldDocs;
  FLastClick := lcTabbed;


  newDocFm1 := MakeNewDocFm;
  newDocFm2 := MakeNewDocFm;


   { If checkbox is checked, reporduce JEDI Issue 5023 }
  if tbDockRightSide.Checked then
    ctrl := DockServer.RightDockPanel
  else
    ctrl := DockServer.CustomDockPanel;

	tabHost := ManualTabDock( ctrl, newDocFm1,  newDocFm2);
  //tabHost := _ManualTabDock( ctrl, newDocFm1,  newDocFm2, cbWorkaround.Checked );


  // How to add a 3rd and a fourth page:
  if Assigned(tabHost) then begin
	newDocFm2 := MakeNewDocFm;
	ManualTabDockAddPage( tabHost, newDocFm2 );
  end;

  newDocFm1.Show;
  newDocFm2.Show;
end;




procedure TMainForm.ButtonCreateConjoinClick(Sender: TObject);
var
 newDocFm1,newDocFm2:TDocForm;
// conjoinHost:TJvDockConjoinHostForm;
begin
  if FLastClick <> lcConjoin then
    clearOldDocs;

  FLastClick := lcConjoin;
  newDocFm1 := MakeNewDocFm;
  newDocFm2 := MakeNewDocFm;
  {conjoinHost := }ManualConjoinDock( DockServer.CustomDockPanel, newDocFm1,  newDocFm2 );

  newDocFm1.Show;
  newDocFm2.Show;
  // How to add a 3rd and a fourth page:
//  newDocFm2 := MakeNewDocFm;
//  ManualConjoinDockAdd( conjoinHost, newDocFm2 );   //TODO! IMPLEMENT THIS HELPER FUNCTION!


end;

procedure TMainForm.SpinEdit1Change(Sender: TObject);
begin
  JvDockVIDStyle1.ConjoinServerOption.GrabbersSize := SpinEdit1.Value;
end;

procedure TMainForm.tbDockRightSideClick(Sender: TObject);
begin
  dockServer.CustomDock := not tbDockRightSide.Checked;
  dockServer.RightDock := tbDockRightSide.Checked;

  if (tbDockRightSide.Checked) then begin
	panel2.Align := alLeft;
	panel2.Width := 40;
  end else begin
	panel2.Align := alCLient;
	if DockServer.DockPanel[dpRight].DockClientCount>0 then begin
		if (DockServer.DockPanel[dpRight].Width < MinWidth )then
				DockServer.DockPanel[dpRight].Width := MinWidth;
	end;
  end;
end;

end.
