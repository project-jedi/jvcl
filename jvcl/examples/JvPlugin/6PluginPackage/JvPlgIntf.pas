unit JvPlgIntf;

// holding interfaces for plugins and main application
// remember to always produce unique GUIDs (Ctrl+Shift+G) - never reuse them after changes

interface

uses Classes;

// A common interface for all plugins
type
  IMyPluginInterface = interface
    ['{A7F68489-2B52-4E59-B5E8-2044C7F67C09}']

    procedure ShowPlug(Sender: TObject);
    // ...
  end;


  // if plugins shall be able to access the mainapplication,
  //   an interface for the main application should be defined as well
type
  IMyMainAppInterface = interface

    ['{00D251C6-8D61-43F7-88F8-35F7F7EC364D}']
    procedure DoSomethingSpecial(Name: string; OnClick: TNotifyEvent);

    // this way properties can be used:
  //  function GetECGPos : double;
  //  Procedure SetECGPos(const NewPos : double);
  //  property ECGPos : double read GetECGPos write SetECGPos;
  end;


implementation

end.

