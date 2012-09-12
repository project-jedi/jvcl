unit DocFm;
                  
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, JvComponent, JvDockControlForm,
  JvComponentBase;

type
  TMsgEvent = procedure(msg:String) of object;

  TDocForm = class(TForm)
    sg: TStringGrid;
    DockClient: TJvDockClient;
    Bevel1: TBevel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    CheckBox2: TCheckBox;
    procedure DockClientFormHide(Sender: TObject);
    procedure DockClientFormShow(Sender: TObject);
    procedure DockClientCheckIsDockable(DockClient: TJvDockClient;
      DockForm: TForm; DockServer: TJvDockServer; DockPanel: TJvDockPanel;
      var CanDock: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure FormUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure Button1StartDock(Sender: TObject;
      var DragObject: TDragDockObject);
    procedure FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure FormShow(Sender: TObject);
    procedure FormGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
    FOnTrace:TMsgEvent;

    procedure Trace(msg:String);
    procedure DockClientShow(Sender:Tobject);
    procedure DockClientAllowDrag(Sender:TObject; var AllowDrag:Boolean );

  public
    { Public declarations }
    property OnTrace:TMsgEvent read FOnTrace write FOnTrace;

  end;

var
  DocForm: TDocForm;

implementation

{$R *.dfm}

procedure TDocForm.Trace(msg:String);
begin
    if Assigned(FOnTrace) then begin
        FOnTrace( msg );
    end;

end;
procedure TDocForm.DockClientFormHide(Sender: TObject);
begin
      Trace( Self.Caption + ' HIDE ' );
end;

procedure TDocForm.DockClientFormShow(Sender: TObject);
begin
     Trace( Self.Caption + ' SHOW ' );

end;



procedure TDocForm.DockClientShow(Sender: Tobject);
begin
   Button1.Caption := 'dockclientshow';

end;

procedure TDocForm.Button1Click(Sender: TObject);
begin
  DoFloatForm(Self);
end;

procedure TDocForm.Button1StartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
   Button1.Caption := 'dock';
end;

procedure TDocForm.CheckBox2Click(Sender: TObject);
begin
 DockClient.CanFloat := CheckBox2.Checked;
end;

procedure TDocForm.DockClientAllowDrag(Sender: TObject; var AllowDrag: Boolean);
begin
Trace('event:Allow drag');
   AllowDrag := Checkbox2.Checked;
end;

procedure TDocForm.DockClientCheckIsDockable(DockClient: TJvDockClient;
  DockForm: TForm; DockServer: TJvDockServer; DockPanel: TJvDockPanel;
  var CanDock: Boolean);
var
  s,n:String;
begin
   if Assigned(FOnTrace) then begin
        s := BoolToStr( CanDock,true );
        if Assigned(DockPanel) then begin
            n := DockPanel.Name;
        end else begin
            n := '<nil>';
        end;
        Trace( Self.Caption + ' DockClient.CheckIsDockable  DockPanel='+n+', CanDock='+s );
    end;
end;

procedure TDocForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckBox1.Checked;
end;

procedure TDocForm.FormCreate(Sender: TObject);
begin
//   DockClient.OnAllowDrag := DockClientAllowDrag;
end;

procedure TDocForm.FormEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  Trace ('Docked');
 Button1.Caption := 'enddock';
end;

procedure TDocForm.FormGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
 Button1.Caption := 'site';
end;

procedure TDocForm.FormShow(Sender: TObject);
begin
// Button1.Caption := 'show';
// DockClient.OnFormShow := Self.DockClientShow;
end;

procedure TDocForm.FormStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  { JVCL NEVER FIRES THIS EVENT }
   Button1.Caption := 'dock';

end;

procedure TDocForm.FormUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin

   Button1.Caption := 'undock';
   Allow := CheckBox2.Checked;
   if Allow then
     Trace('event:Undocked')
   else
     Trace('event:Undock blocked');


end;

end.
