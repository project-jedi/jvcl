unit glCGI;

interface
{$APPTYPE CONSOLE}
uses
  Windows, SysUtils, Classes;

type
  TglCGI = class
    private
      FAutoHeaderAndFooter: boolean;
      procedure Split(s: string);
      procedure Decode(sl: TStringList);
    public
//      Names: TStringList;
//      Values: TStringList;
      val: array[0..500] of char;
      Params: TStringList;
      constructor Create(AutoHeaderAndFooter: boolean);
      destructor Destroy; override;
      procedure InsHeader;
      procedure InsFooter;
      function ParamNameIndex( str: string ): integer; //...returns -1 if Name doesn't exist
      function ParamValueIndex( str: string ): integer;//...returns -1 if Name doesn't exist
  end;

implementation

constructor TglCGI.Create(AutoHeaderAndFooter: boolean);
var sMsg: string;
begin
  FillChar( val, sizeof(val), 0 );

  Reset(Input);
  //_____________________________GET METHOD
  GetEnvironmentVariable( 'QUERY_STRING', @val, sizeof(val) );
  //_____________________________POST METHOD
//  if val='' then
//  begin Read(Input,val); sMsg := 'Post'; end else sMsg := 'Get';

//  MessageBox(0,val, PChar(sMsg+' method detected.'),MB_OK); //exit;

  FAutoHeaderAndFooter := AutoHeaderAndFooter;
  if FAutoHeaderAndFooter then InsHeader;

  val := ' ';
//val := 'order&p=5&u=55&c=98';
//  val := 'EDITION=Piter&?AUTHOR=&TITLE=%DF%E7%FB%EA+%EF%F0%EE%E3%F0%E0%EC%EC%E8%F0%EE%E2%E0%ED%E8%FF+%D4%EE%F0%F2&PB_ADD=%C4%EE%E1%E0%E2%E8%F2%FC';
//  Names := TStringList.Create;
//  Values := TStringList.Create;
  Params := TStringList.Create;
  if length(val) > 0 then
  begin
    Split( val );
//    Decode( Values );
    Decode( Params );
  end;
end;

destructor TglCGI.Destroy;
begin
  if FAutoHeaderAndFooter then InsFooter;
//  Names.Free;
//  Values.Free;
  Params.Free;
  inherited;
end;

procedure TglCGI.InsHeader;
begin
   Writeln( 'HTTP/1.0 200 OK');
   Writeln( 'Date: Thursday, 12-Jan-96 16:04:30 GMT');
   Writeln( 'Server: WebSite 1.0');
   Writeln( 'Content-type: text/html');
   Writeln( 'Last-modified: Thursday, 25-Jan-96 16:04:30 GMT');
   Writeln( 'Content-length: 5000');
   Writeln( '');
end;

procedure TglCGI.InsFooter;
begin
  Writeln( 'Debug: '+val );//...debug
  Writeln( '<META IDENTITY CONTENT="'+ IntToStr(random(100000))+'">');
  Writeln( '</BODY>');
  Writeln( '</HTML>');
end;

procedure TglCGI.Split( s: string );
var i, lastI: integer;
  procedure Extract( LastI, i: integer );
  var j: integer;
  begin
{    j := LastI;
    while (s[j]<>'=')and(s[j]<>#0) do inc(j);
    Names.Add( copy( s, LastI, j-LastI ) );
    Values.Add( copy( s, j+1, i-j-1 ) );}
    Params.Add( copy( s, LastI, i-LastI+1 ) );
  end;
begin
  i := 1; lastI := 1;
  while i < length(s) do
  begin
    if (s[i] = '&')or(s[i] = '?') then begin Extract( LastI, i-1 ); lastI := i+1; end;
    inc(i);
  end;
  Extract( LastI, i+1 );
end;

procedure TglCGI.Decode( sl: TStringList );
var
  i,j: integer;
  s: string;
begin
  for i := 0 to sl.Count - 1 do
  begin
    s := sl[i];
    for j := 1 to length(s) do
      if s[j] = '+' then s[j] := ' ' else
      if s[j] = '%' then
      try
        s := copy( s, 0, j-1 )+char(StrToInt('$'+copy( s, j+1, 2 )))+copy( s, j+3, length(s)-j-2 );
      except end;
    if s[1] = '?' then
     s := copy( s, 2, length(s)-1 );
    sl[i] := s;

  end;
end;

function TglCGI.ParamNameIndex( str: string ): integer;
var i: integer;
begin
  for i:=0 to Params.Count-1 do
    if CompareText( Params.Names[i], str ) = 0 then
    begin Result := i; exit; end;
  Result := -1;
end;

function TglCGI.ParamValueIndex( str: string ): integer;
var i: integer;
begin
{  for i:=0 to Params.Count-1 do
    if CompareText( Params.Values[i], str ) = 0 then exit;
    begin Result := i; exit; end;}
  Result := -1;
end;
//================================================

end.
