unit DocFm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, JvComponent, JvDockControlForm;

type
  TMsgEvent = procedure(msg:String) of object;

  TDocForm = class(TForm)
    sg: TStringGrid;
    DockClient: TJvDockClient;
    Bevel1: TBevel;
    CheckBox1: TCheckBox;
    procedure DockClientFormHide(Sender: TObject);
    procedure DockClientFormShow(Sender: TObject);
    procedure DockClientCheckIsDockable(DockClient: TJvDockClient;
      DockForm: TForm; DockServer: TJvDockServer; DockPanel: TJvDockPanel;
      var CanDock: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FOnTrace:TMsgEvent;

    procedure Trace(msg:String);
    
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

procedure TDocForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

end.
