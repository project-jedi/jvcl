// *****************
// BEGIN USER DEFINE
// *****************

//----------
// FIREBIRD
//----------
{.$DEFINE FB102}
{.$DEFINE FB103}
{.$DEFINE FB15}
{.$DEFINE FBEMBED} //Firebird Embeded FB15 only

//----------
// INTERBASE
//----------
{.$DEFINE IB65}
{.$DEFINE IB7}

//----------
// YAFFIL
//----------
{.$DEFINE YF867}


// use dynamic linking by default
{$DEFINE INTERBASE_DYNAMIC_LINK}

// Enable Multithreading,
// Disabling Multithrading will made UIB a bit faster.
{$DEFINE UIBTHREADSAFE}

//----------
// BCB6
// Uncomment this lines if you want to use original Interbase headers
//----------
{.$DEFINE USE_IBERROR_H}
{.$DEFINE USE_IBASE_H}

//----------
// Languages
//----------
{$DEFINE UIBLANG_EN} // English
{.$DEFINE UIBLANG_FR} // French

//----------
// OTHER
//----------
// Uncomment if you want to use class instead of components.
// (will be helpfull with free pascal compiler)
{.$DEFINE UIBNOCOMPONENT}

// ***************
// END USER DEFINE
// ***************

{$IFDEF LINUX}
  {$DEFINE SET_TCP_NO_DELAY}
{$ENDIF}

{$ALIGN ON}
{$MINENUMSIZE 4}

// FIREBIRD

{$IFDEF FB15}
  {$DEFINE FIREBIRD}
  {$DEFINE FB15_UP}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE IB7ORFB15}
  {$DEFINE FB15ORYF867}
  {$DEFINE FB102ORYF867}
{$ENDIF}

{$IFDEF FB103}
  {$DEFINE FIREBIRD}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE FB102ORYF867}
{$ENDIF}

{$IFDEF FB102}
  {$DEFINE FIREBIRD}
  {$DEFINE FB102_UP}

  {$DEFINE FB102ORYF867}
{$ENDIF}

// INTERBASE

{$IFDEF IB7}
  {$DEFINE INTERBASE}
  {$DEFINE IB7_UP}
  {$DEFINE IB65_UP}

  {$DEFINE IB7ORFB15}
  {$DEFINE IB65ORYF867}
{$ENDIF}

{$IFDEF IB65}
  {$DEFINE INTERBASE}
  {$DEFINE IB65_UP}

  {$DEFINE IB65ORYF867}
{$ENDIF}

// YAFFIL

{$IFDEF YF867}
  {$DEFINE YAFFIL}
  {$DEFINE YF867_UP}

  {$DEFINE FB15ORYF867}
  {$DEFINE FB102ORYF867}
  {$DEFINE IB65ORYF867}
{$ENDIF}

////////////////////////

{$IFDEF INTERBASE}
  {$DEFINE INTERBASEORYAFFIL}
  {$DEFINE INTERBASEORFIREBIRD}
{$ENDIF}

{$IFDEF FIREBIRD}
  {$DEFINE INTERBASEORFIREBIRD}
  {$DEFINE FIREBIRDORYAFFIL}
{$ENDIF}

{$IFDEF YAFFIL}
  {$DEFINE INTERBASEORYAFFIL}
  {$DEFINE FIREBIRDORYAFFIL}
{$ENDIF}
