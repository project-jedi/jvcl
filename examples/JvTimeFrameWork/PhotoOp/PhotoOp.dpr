program PhotoOp;

{%File 'dbUTF'}

uses
  Forms,
  PhotoOpUnit in 'PhotoOpUnit.pas' {PhotoOpMain},
  VisibleResourcesUnit in 'VisibleResourcesUnit.pas' {VisibleResources},
  ShareUnit in 'ShareUnit.pas' {Share},
  ApptEditUnit in 'ApptEditUnit.pas' {ApptEdit},
  PrintProgressUnit in 'PrintProgressUnit.pas' {PrintProgress};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TPhotoOpMain, PhotoOpMain);
  Application.CreateForm(TVisibleResources, VisibleResources);
  Application.CreateForm(TShare, Share);
  Application.CreateForm(TApptEdit, ApptEdit);
  Application.CreateForm(TPrintProgress, PrintProgress);
  Application.Run;
end.
