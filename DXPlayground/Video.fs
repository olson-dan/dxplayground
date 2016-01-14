#light "off"

module Video

open SlimDX
open SlimDX.DXGI
open SlimDX.Direct3D11
open SlimDX.D3DCompiler
open SlimDX.Windows

type Window(label, width, height) = class
	let w = width
	let h = height
	let form = new RenderForm(label)
	let swapChainDesc = new SwapChainDescription(
			BufferCount = 2,
			Usage = Usage.RenderTargetOutput,
			OutputHandle = form.Handle,
			IsWindowed = true,
			ModeDescription = new ModeDescription(w,h, new Rational(60,1), Format.R8G8B8A8_UNorm),
			SampleDescription = new SampleDescription(1,0),
			Flags = SwapChainFlags.AllowModeSwitch,
			SwapEffect = SwapEffect.Discard)
	let _, device, swapChain = Device.CreateWithSwapChain (DriverType.Hardware, DeviceCreationFlags.Debug, swapChainDesc)
	let renderResource = Resource.FromSwapChain<Texture2D>(swapChain, 0)
	let renderTarget = new RenderTargetView(device, renderResource)
	let context = device.ImmediateContext
	do
		form.SetBounds (0,0,w,h);
		context.Rasterizer.SetViewports(Viewport(0.0f,0.0f, float32 w, float32 h, 0.0f, 1.0f));
		context.Rasterizer.State <- RasterizerState.FromDescription(
			device, RasterizerStateDescription(CullMode = CullMode.None, FillMode = FillMode.Solid));
		context.OutputMerger.SetTargets(renderTarget)
	member this.Form = form
	member this.Device = device
	member this.Context = context
	member this.RenderTarget = renderTarget
	member this.Loop f = MessagePump.Run (form, MainLoop(f))
	member this.Present = swapChain.Present (0, PresentFlags.None) |> ignore
end

let CompileCS device (code : string) =
	let bytecode = ShaderBytecode.Compile(code, "cs_main", "cs_5_0", ShaderFlags.None, EffectFlags.None) in
	new ComputeShader(device, bytecode)
 
let CompileVS device (code : string) =
	let bytecode = ShaderBytecode.Compile(code, "vs_main", "vs_5_0", ShaderFlags.None, EffectFlags.None) in
	new VertexShader(device, bytecode)
 
let CompilePS device (code : string) =
	let bytecode = ShaderBytecode.Compile(code, "ps_main", "ps_5_0", ShaderFlags.None, EffectFlags.None) in
	new PixelShader(device, bytecode)
 


