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
    btnRefresh: TButton;
    btnDelete: TButton;
    tiRefresh: TTimer;
    Splitter1: TSplitter;
    pnlTasks: TFlowPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnStopFramesClick(Sender: TObject);
    procedure tiRefreshTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FFac: IFac;

    procedure LogP(const AStr: string; const AFunc: string; const ALogLevel: TLogLevel = llInfo);
    procedure RefreshDemos;
    procedure Start;
    procedure Stop;
  public

  end;

var
  FormMain: TFormMain;

implementation

uses
  uDemos,
  uFrmThread;

{$R *.dfm}

procedure TFormMain.btnDeleteClick(Sender: TObject);
var
  LCont: TControl;
begin
  for var I := pnlTasks.ControlCount - 1 downto 1 do
  begin
    LCont := pnlTasks.Controls[I];

    if LCont is TFrameThread and TFrameThread(LCont).FThread.IsDone then
      TFrameThread(LCont).Destroy;
  end;
end;

procedure TFormMain.btnStartClick(Sender: TObject);
begin
  Start;
end;

procedure TFormMain.btnStopClick(Sender: TObject);
begin
  Stop;
end;

procedure TFormMain.btnStopFramesClick(Sender: TObject);
var
  LCont: TControl;
begin
  for var I := 0 to pnlTasks.ControlCount - 1 do
  begin
    LCont := pnlTasks.Controls[I];

    if LCont is TFrameThread then
      TFrameThread(LCont).Terminate;
  end;
end;

procedure TFormMain.btnRefreshClick(Sender: TObject);
begin
  RefreshDemos;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Stop;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  LOnAdd: TProcArg1<ITaskAbstract>;
begin
  FGet.Init(Memo1.Lines.Append);
  FFac := FGet.Fac;

  LOnAdd := procedure(const ATask: ITaskAbstract)
    var
      LFrameThread: TFrameThread;
    begin
      LFrameThread := TFrameThread.Create(nil, ATask);
      LFrameThread.Parent := pnlTasks;
      ATask.LogSet(LFrameThread.Log);
    end;

  LOnAdd(FFac.TaskClusterMain);
  FFac.TaskClusterMain.DoOnAdd(LOnAdd);

  tiRefresh.Interval := 500;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Start;
end;

procedure TFormMain.tiRefreshTimer(Sender: TObject);
begin
  RefreshDemos;
end;

procedure TFormMain.LogP(const AStr, AFunc: string; const ALogLevel: TLogLevel);
begin
  FFac.Log.Log(AStr, AFunc, Self.ClassName, ALogLevel, 0);
end;

procedure TFormMain.RefreshDemos;
var
  LCont: TControl;
begin
  uDemos.Refresh;

  for var I := 0 to pnlTasks.ControlCount - 1 do
  begin
    LCont := pnlTasks.Controls[I];

    if LCont is TFrameThread then
      TFrameThread(LCont).RefreshUI;
  end;
end;

procedure TFormMain.Start;
begin
  LogP('Start', 'Start');
  uDemos.Start;
end;

procedure TFormMain.Stop;
var
  LB: Boolean;
begin
  uDemos.Stop;
  FFac.TaskClusterMain.Terminate;

  LB := FFac.TaskClusterMain.WaitFor(2000);
  LogP('Waitfor ' + BoolToStr(LB, True), 'Stop');
end;

end.
