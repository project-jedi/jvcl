program MSDN2002Pro;

uses
  Forms,
  MSDN2002MainUnit in 'MSDN2002MainUnit.pas' {MSDN2002},
  ContentsFormUnit in 'ContentsFormUnit.pas' {ContentsForm},
  IndexFormUnit in 'IndexFormUnit.pas' {IndexForm},
  SearchFormUnit in 'SearchFormUnit.pas' {SearchForm},
  FavoritesFormUnit in 'FavoritesFormUnit.pas' {FavoritesForm},
  IndexResultFormUnit in 'IndexResultFormUnit.pas' {IndexResultForm},
  SearchResultFormUnit in 'SearchResultFormUnit.pas' {SearchResultForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MSDN Library - April 2002';
  Application.CreateForm(TMSDN2002, MSDN2002);
  Application.Run;
end.
