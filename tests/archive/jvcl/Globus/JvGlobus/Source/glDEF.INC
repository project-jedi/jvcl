//... Globus Library include file

// >>>>>>  Comment next string for English messages !!!
// {$DEFINE RUSSIAN}


// Delphi 3.0 and higher
// C++Builder 4.0 and higher

{$IFDEF VER100} { Borland Delphi 3.0 }
  {$DEFINE GLVER_D3}
{$ENDIF}

{$IFDEF VER120} { Borland Delphi 4.0 }
  {$DEFINE GLVER_D3}
  {$DEFINE GLVER_D4}
{$ENDIF}

{$IFDEF VER130} { Borland Delphi 5.0 }
  {$DEFINE GLVER_D3}
  {$DEFINE GLVER_D4}
  {$DEFINE GLVER_D5}
{$ENDIF}

{$IFDEF VER135} { C++Builder 5.0 }

{$ENDIF}

{$IFDEF VER145} { C++Builder 6.0 }

{$ENDIF}

{$IFDEF VER140} { Borland Delphi 6.0 }
  {$DEFINE GLVER_D3}
  {$DEFINE GLVER_D4}
  {$DEFINE GLVER_D5}
  {$DEFINE GLVER_D6}
{$ENDIF}

{$IFDEF VER150} { Borland Delphi 7.0 }
  {$DEFINE GLVER_D3}
  {$DEFINE GLVER_D4}
  {$DEFINE GLVER_D5}
  {$DEFINE GLVER_D6}
  {$DEFINE GLVER_D7}
{$ENDIF}

{$IFNDEF VER80}{$IFNDEF VER90}{$IFNDEF VER93}
  {$DEFINE GLVER_D3} // ...DELPHI 3.0 or higher
  {$DEFINE GL_RUS}   //...RUSSIAN_CHARSET
{$ENDIF}{$ENDIF}{$ENDIF}



