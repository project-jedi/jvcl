unit Design;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  JvDesignSurface;

type
  TObjectArray = array of TObject;
  TDesignForm = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    JvDesignSurface: TJvDesignSurface;
    procedure Clear;
    procedure LoadFromFile(const inFilename: string);
    procedure SaveToFile(const inFilename: string);
  end;

var
  DesignForm: TDesignForm;

implementation

uses
  Utils;

{$R *.dfm}

procedure TDesignForm.FormCreate(Sender: TObject);
begin
  DesignSurface := TDesignSurface.Create(Self);
  DesignSurface.Name := 'Surface';
end;

procedure TDesignForm.Clear;
begin
  // DesignSurface property value is lost on clear.
  // Restore it with the value returned from LoadFromFile.
  DesignSurface := DesignSurface.Clear;
end;

procedure TDesignForm.LoadFromFile(const inFilename: string);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromFile.
  DesignSurface := DesignSurface.LoadFromFile(inFilename);
end;

procedure TDesignForm.SaveToFile(const inFilename: string);
begin
  DesignSurface.SaveToFile(inFilename);
end;

initialization
  RegisterClass(TDesignSurface);
end.
