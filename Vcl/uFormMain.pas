// Copyright (C) 2023 Pontus Wiberg <wiberg.public@outlook.com>
// Licensed under GPLv2 or later version. Contact for any licensing concerns

unit uFormMain;

interface

uses
  uTypes,

  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    pnlMain: TPanel;
    Memo1: TMemo;
    pnlTop: TFlowPanel;
    btnStart: TButton;
    btnStop: TButton;
    btnUpdate: TButton;
    btnDelete: TButton;
    Timer1: TTimer;
    pnlThreads: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnStopFramesClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FFac: IFac;

    procedure LogP(const AStr: string; const AFunc: string; const ALogLevel: TLogLevel = llInfo);
    procedure Stop;
  public

  end;

var
  FormMain: TFormMain;

implementation

uses
  uStart,
  uFrmThread;

{$R *.dfm}

procedure TFormMain.btnDeleteClick(Sender: TObject);
var
  LCont: TControl;
begin
  for var I := pnlThreads.ControlCount - 1 downto 1 do
  begin
    LCont := pnlThreads.Controls[I];

    if LCont is TFrameThread and TFrameThread(LCont).DeleteEnabled then
      TFrameThread(LCont).Destroy;
  end;
end;

procedure TFormMain.btnStartClick(Sender: TObject);
begin
  LogP('Start', 'btnStartClick');

  // btnStart.Enabled := False;
  // btnStop.Enabled := True;

  uStart.Start;
end;

procedure TFormMain.btnStopClick(Sender: TObject);
begin
  Stop;
end;

procedure TFormMain.btnStopFramesClick(Sender: TObject);
var
  LCont: TControl;
begin
  for var I := 0 to pnlThreads.ControlCount - 1 do
  begin
    LCont := pnlThreads.Controls[I];

    if LCont is TFrameThread then
      TFrameThread(LCont).btnStopClick(nil);
  end;
end;

procedure TFormMain.btnUpdateClick(Sender: TObject);
var
  LCont: TControl;
begin
  uStart.Tick;

  for var I := 0 to pnlThreads.ControlCount - 1 do
  begin
    LCont := pnlThreads.Controls[I];

    if LCont is TFrameThread then
      TFrameThread(LCont).Updatee;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Self.Visible := False;
  Stop;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  LOnAdd: TProcArg1<ITaskAbstract>;
begin
  // btnStart.Enabled := True;
  // btnStop.Enabled := False;

  FGet.Init(Memo1.Lines.Append);
  FFac := FGet.Fac;
  FFac.Log.Log(TThread.Current.ThreadID.ToString);

  LOnAdd := procedure(const ATask: ITaskAbstract)
    var
      LFrameThread: TFrameThread;
    begin
      LFrameThread := TFrameThread.Create(nil, ATask);
      LFrameThread.Parent := pnlThreads;
      LFrameThread.Align := alTop;
      LFrameThread.Top := pnlThreads.Height;

      ATask.LogSet(LFrameThread.Log);
    end;

  LOnAdd(FFac.TaskClusterMain);
  FFac.TaskClusterMain.DoOnAdd(LOnAdd);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  btnStartClick(nil);
end;

procedure TFormMain.LogP(const AStr, AFunc: string; const ALogLevel: TLogLevel);
begin
  FFac.Log.Log(AStr, AFunc, 'TFormMain', ALogLevel, Now);
end;

procedure TFormMain.Stop;
var
  LB: Boolean;
begin
  // btnStop.Enabled := False;
  uStart.Stop;

  FFac.TaskClusterMain.Terminate;
  LB := FFac.TaskClusterMain.WaitFor(2000);

  LogP('Waitfor ' + BoolToStr(LB, True), 'Stop');
  // Sleep(500);

  btnStart.Enabled := True;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  btnUpdateClick(nil);
end;

end.
