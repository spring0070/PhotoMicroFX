import SwiftUI
import PhotosUI
import CoreImage
import CoreImage.CIFilterBuiltins
import Photos
import UIKit
import Vision
import ImageIO // CGImagePropertyOrientation

// UIImage.Orientation -> Vision 方向
extension CGImagePropertyOrientation {
    init(_ uiOrientation: UIImage.Orientation) {
        switch uiOrientation {
        case .up: self = .up
        case .down: self = .down
        case .left: self = .left
        case .right: self = .right
        case .upMirrored: self = .upMirrored
        case .downMirrored: self = .downMirrored
        case .leftMirrored: self = .leftMirrored
        case .rightMirrored: self = .rightMirrored
        @unknown default: self = .up
        }
    }
}

struct ContentView: View {
    // MARK: - Image State
    @State private var pickerItem: PhotosPickerItem?
    @State private var originalImage: UIImage?
    @State private var processedImage: UIImage?

    private let ciContext = CIContext(options: nil)

    // MARK: - Effect Mode
    enum EffectMode: String, CaseIterable, Identifiable {
        case none = "無"
        case tiltShift = "微小模型"
        case vibrant = "彩度+對比+暗角"
        case beauty  = "人臉磨皮"
        var id: String { rawValue }
    }
    @State private var mode: EffectMode = .none

    // MARK: - Tilt-Shift Params
    @State private var tsCenterY: CGFloat = 0.55
    @State private var tsBand: CGFloat = 0.30
    @State private var tsFeather: CGFloat = 0.22
    @State private var tsBlurRadius: CGFloat = 12.0

    // MARK: - Vibrant Params
    @State private var vbSaturation: CGFloat = 1.35
    @State private var vbContrast: CGFloat   = 1.15
    @State private var vbVignetteIntensity: CGFloat = 0.9
    @State private var vbVignetteRadiusRatio: CGFloat = 0.55
    @State private var vbVignetteFalloff: CGFloat = 0.9

    // MARK: - Beauty Params
    @State private var beautyStrength: CGFloat = 0.3   // 0...1
    @State private var beautyFeather:  CGFloat = 6.0    // px
    @State private var eyeEnhanceEnabled: Bool = true
    @State private var eyeEnhanceIntensity: CGFloat = 0.6
    @State private var eyeFeather: CGFloat = 8.0        // px

    // Vision 遮罩快取
    @State private var faceMask: CIImage?
    @State private var eyeMask: CIImage?
    @State private var featureMask: CIImage? // 五官（眼眉鼻唇）

    // MARK: - 縮放/拖曳 + 置頂間距
    @State private var zoom: CGFloat = 1
    @State private var lastZoom: CGFloat = 1
    @State private var offset: CGSize = .zero
    @State private var lastOffset: CGSize = .zero
    @State private var topInset: CGFloat = 8

    // 底部面板
    @State private var showPanel: Bool = true
    private var controlsHeightOneThird: CGFloat {
        let h = UIScreen.main.bounds.height * 0.33
        return min(max(h, 180), 360)
    }

    // 遮罩除錯（只在 .beauty 顯示）
    @State private var showMaskOverlay: Bool = false
    @GestureState private var isMaskPreviewPressing: Bool = false

    // 存檔提示
    @State private var showSaveAlert = false
    @State private var saveAlertTitle = "完成"
    @State private var saveAlertMessage = "已存到相簿"

    var body: some View {
        ZStack {
            Color.black.ignoresSafeArea()

            // 圖片顯示（靠上、左右貼齊）
            if let img = processedImage ?? originalImage {
                GeometryReader { _ in
                    // 手勢
                    let magnify = MagnificationGesture()
                        .onChanged { value in zoom = min(max(lastZoom * value, 1), 6) }
                        .onEnded   { value in lastZoom = min(max(lastZoom * value, 1), 6) }

                    let drag = DragGesture(minimumDistance: 0)
                        .onChanged { v in
                            guard zoom > 1 else { return }
                            offset = CGSize(width: lastOffset.width + v.translation.width,
                                            height: lastOffset.height + v.translation.height)
                        }
                        .onEnded { _ in lastOffset = offset }

                    let doubleTap = TapGesture(count: 2)
                        .onEnded {
                            if zoom > 1 { withAnimation(.spring) { resetViewTransform() } }
                            else        { withAnimation(.spring) { zoom = 2; lastZoom = 2 } }
                        }

                    // 真正長按才暫顯遮罩
                    let longPress = LongPressGesture(minimumDuration: 0.5, maximumDistance: 12)
                        .updating($isMaskPreviewPressing) { value, state, _ in
                            state = value
                        }

                    // 底圖（照片）
                    Image(uiImage: img)
                        .resizable()
                        .scaledToFit()
                        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
                        .safeAreaPadding(.top, topInset)
                        .scaleEffect(zoom)
                        .offset(offset)
                        .animation(.spring(duration: 0.2), value: zoom)
                        .animation(.spring(duration: 0.2), value: offset)
                        .highPriorityGesture(doubleTap)
                        .gesture(magnify.simultaneously(with: drag))
                        .simultaneousGesture(longPress)
                        .contentShape(Rectangle())

                    // Face mask（紅）— 僅在 .beauty 顯示
                    if mode == .beauty,
                       (showMaskOverlay || isMaskPreviewPressing),
                       let base = originalImage,
                       let faceCI = preparedSkinMaskCI(for: base),
                       let faceOverlay = overlayUIImage(from: faceCI, color: (1, 0, 0), alpha: 0.30) {
                        Image(uiImage: faceOverlay)
                            .resizable()
                            .scaledToFit()
                            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
                            .safeAreaPadding(.top, topInset)
                            .scaleEffect(zoom)
                            .offset(offset)
                            .allowsHitTesting(false)
                    }

                    // Eye mask（綠）— 僅在 .beauty 顯示
                    if mode == .beauty,
                       eyeEnhanceEnabled,
                       (showMaskOverlay || isMaskPreviewPressing),
                       let base = originalImage,
                       let eyeCI = preparedEyeMaskCI(for: base),
                       let eyeOverlay = overlayUIImage(from: eyeCI, color: (0, 1, 0), alpha: 0.35) {
                        Image(uiImage: eyeOverlay)
                            .resizable()
                            .scaledToFit()
                            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
                            .safeAreaPadding(.top, topInset)
                            .scaleEffect(zoom)
                            .offset(offset)
                            .allowsHitTesting(false)
                    }
                }
                .ignoresSafeArea(edges: .bottom)
            } else {
                Text("選擇一張照片開始")
                    .foregroundStyle(.secondary)
            }

            // 底部控制面板
            VStack {
                Spacer()
                bottomPanel
            }
            .ignoresSafeArea(edges: .bottom)
        }
        .alert(saveAlertTitle, isPresented: $showSaveAlert) {
            Button("好", role: .cancel) {}
        } message: {
            Text(saveAlertMessage)
        }
        .onChange(of: pickerItem) { _, newItem in
            Task {
                if let data = try? await newItem?.loadTransferable(type: Data.self),
                   let ui = UIImage(data: data) {

                    showMaskOverlay = false // 換圖關閉遮罩預覽

                    originalImage = ui
                    processedImage = nil
                    faceMask    = makeFaceSkinMask(for: ui)
                    eyeMask     = makeEyeMask(for: ui)
                    featureMask = makeFeaturesMask(for: ui)
                    resetViewTransform()
                    applyCurrentEffect()
                }
            }
        }
    }

    // MARK: - 底部面板
    private var bottomPanel: some View {
        VStack(spacing: 8) {
            HStack(spacing: 8) {
                PhotosPicker(selection: $pickerItem, matching: .images) {
                    Label("選取照片", systemImage: "photo.on.rectangle")
                        .labelStyle(.iconOnly)
                        .font(.title3)
                }

                Picker("", selection: $mode) {
                    ForEach(EffectMode.allCases) { m in Text(m.rawValue).tag(m) }
                }
                .pickerStyle(.segmented)
                .onChange(of: mode) { _, newMode in
                    if newMode != .beauty { showMaskOverlay = false }
                    applyCurrentEffect()
                }
                .disabled(originalImage == nil)

                Spacer(minLength: 4)

                // 重置視圖
                Button { withAnimation(.spring) { resetViewTransform() } } label: {
                    Image(systemName: "arrow.counterclockwise")
                }
                .disabled(originalImage == nil)

                // 90° 旋轉（順時針）
                Button { rotate90CW() } label: {
                    Image(systemName: "rotate.right")
                }
                .disabled(originalImage == nil)

                // 收合/展開面板
                Button { withAnimation(.easeInOut) { showPanel.toggle() } } label: {
                    Image(systemName: showPanel ? "chevron.down" : "chevron.up")
                }

                // 儲存
                Button {
                    if let img = processedImage ?? originalImage { saveToPhotos(img) }
                } label: { Image(systemName: "square.and.arrow.down") }
                .disabled((processedImage ?? originalImage) == nil)
            }

            if showPanel {
                ScrollView { parameterControls }
                    .frame(height: controlsHeightOneThird) // 設定面板 ≈ 螢幕 1/3
                    .transition(.move(edge: .bottom).combined(with: .opacity))
            }
        }
        .padding(10)
        .background(.ultraThinMaterial)
        .clipShape(RoundedRectangle(cornerRadius: 14))
        .padding(.horizontal, 10)
        .padding(.bottom, 8)
    }

    // MARK: - 控制面板內容
    @ViewBuilder
    private var parameterControls: some View {
        switch mode {
        case .tiltShift:
            VStack(spacing: 10) {
                LabeledSlider(title: "焦點中心（Y）", value: $tsCenterY, range: 0...1, format: .number.precision(.fractionLength(2)))
                LabeledSlider(title: "清晰帶寬度", value: $tsBand, range: 0.05...0.60, format: .number.precision(.fractionLength(2)))
                LabeledSlider(title: "羽化距離", value: $tsFeather, range: 0.05...0.60, format: .number.precision(.fractionLength(2)))
                LabeledSlider(title: "模糊半徑", value: $tsBlurRadius, range: 0...30, format: .number.precision(.fractionLength(1)))
            }
            .onChange(of: tsCenterY) { applyCurrentEffect() }
            .onChange(of: tsBand) { applyCurrentEffect() }
            .onChange(of: tsFeather) { applyCurrentEffect() }
            .onChange(of: tsBlurRadius) { applyCurrentEffect() }

        case .vibrant:
            VStack(spacing: 10) {
                LabeledSlider(title: "彩度", value: $vbSaturation, range: 0...2, format: .number.precision(.fractionLength(2)))
                LabeledSlider(title: "對比", value: $vbContrast, range: 0.5...2, format: .number.precision(.fractionLength(2)))
                LabeledSlider(title: "暗角強度", value: $vbVignetteIntensity, range: 0...2, format: .number.precision(.fractionLength(2)))
                LabeledSlider(title: "暗角半徑（比例）", value: $vbVignetteRadiusRatio, range: 0.2...1.0, format: .number.precision(.fractionLength(2)))
                LabeledSlider(title: "暗角衰減", value: $vbVignetteFalloff, range: 0...2, format: .number.precision(.fractionLength(2)))
            }
            .onChange(of: vbSaturation) { applyCurrentEffect() }
            .onChange(of: vbContrast) { applyCurrentEffect() }
            .onChange(of: vbVignetteIntensity) { applyCurrentEffect() }
            .onChange(of: vbVignetteRadiusRatio) { applyCurrentEffect() }
            .onChange(of: vbVignetteFalloff) { applyCurrentEffect() }

        case .beauty:
            VStack(spacing: 10) {
                LabeledSlider(title: "磨皮強度", value: $beautyStrength, range: 0...1, format: .number.precision(.fractionLength(2)))
                LabeledSlider(title: "臉部羽化（px）", value: $beautyFeather, range: 0...30, format: .number.precision(.fractionLength(0)))
                Toggle("眼睛細節加強", isOn: $eyeEnhanceEnabled)
                LabeledSlider(title: "眼睛加強強度", value: $eyeEnhanceIntensity, range: 0...1, format: .number.precision(.fractionLength(2)))
                    .disabled(!eyeEnhanceEnabled)
                LabeledSlider(title: "眼周羽化（px）", value: $eyeFeather, range: 0...24, format: .number.precision(.fractionLength(0)))
                    .disabled(!eyeEnhanceEnabled)
                Toggle("顯示遮罩（除錯）", isOn: $showMaskOverlay)
            }
            .onChange(of: beautyStrength) { applyCurrentEffect() }
            .onChange(of: beautyFeather)  { applyCurrentEffect() }
            .onChange(of: eyeEnhanceEnabled) { applyCurrentEffect() }
            .onChange(of: eyeEnhanceIntensity) { applyCurrentEffect() }
            .onChange(of: eyeFeather) { applyCurrentEffect() }

        case .none:
            EmptyView()
        }
    }

    // MARK: - 視圖重置
    private func resetViewTransform() {
        zoom = 1; lastZoom = 1; offset = .zero; lastOffset = .zero
    }
}

// MARK: - 小組件：帶數值的滑桿
struct LabeledSlider: View {
    let title: String
    @Binding var value: CGFloat
    let range: ClosedRange<CGFloat>
    var format: FloatingPointFormatStyle<Double> = .number

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack {
                Text(title)
                Spacer()
                Text(Double(value).formatted(format))
                    .monospacedDigit()
                    .foregroundStyle(.secondary)
            }
            Slider(value: Binding(get: { value }, set: { value = $0 }), in: range)
        }
    }
}

// MARK: - 濾鏡 / 效果
extension ContentView {

    private func applyCurrentEffect() {
        guard let base = originalImage else { processedImage = nil; return }
        switch mode {
        case .none:     processedImage = nil
        case .tiltShift:processedImage = applyTiltShift(to: base)
        case .vibrant:  processedImage = applyVibrantVignette(to: base)
        case .beauty:   processedImage = applyBeautyAndEyes(to: base)
        }
    }

    // 1) Tilt-Shift
    private func applyTiltShift(to ui: UIImage) -> UIImage? {
        guard let ciInput = CIImage(image: ui) else { return nil }
        let extent = ciInput.extent
        let width = extent.width, height = extent.height

        let focusCenterY = height * tsCenterY
        let focusBand = height * tsBand
        let feather = height * tsFeather

        let gaussian = CIFilter.gaussianBlur()
        gaussian.radius = Float(tsBlurRadius)
        gaussian.inputImage = ciInput.clampedToExtent()
        guard let blurredFull = gaussian.outputImage?.cropped(to: extent) else { return nil }

        let topStart = CGPoint(x: width * 0.5, y: focusCenterY + focusBand / 2)
        let topEnd   = CGPoint(x: width * 0.5, y: focusCenterY + focusBand / 2 + feather)
        let topGrad = linearGradient(from: topStart, to: topEnd, color0: .black, color1: .white)

        let bottomStart = CGPoint(x: width * 0.5, y: focusCenterY - focusBand / 2)
        let bottomEnd   = CGPoint(x: width * 0.5, y: focusCenterY - focusBand / 2 - feather)
        let bottomGrad = linearGradient(from: bottomStart, to: bottomEnd, color0: .black, color1: .white)

        let combinedMask = maximumComposite(topGrad, over: bottomGrad).cropped(to: extent)

        let blend = CIFilter.blendWithMask()
        blend.inputImage = blurredFull
        blend.backgroundImage = ciInput
        blend.maskImage = combinedMask

        guard let output = blend.outputImage else { return nil }
        return render(output: output, cropTo: extent)
    }

    // 2) Vibrant + Vignette
    private func applyVibrantVignette(to ui: UIImage) -> UIImage? {
        guard let ciInput = CIImage(image: ui) else { return nil }
        let extent = ciInput.extent
        let shortest = min(extent.width, extent.height)

        let color = CIFilter.colorControls()
        color.inputImage = ciInput
        color.saturation = Float(vbSaturation)
        color.contrast   = Float(vbContrast)
        color.brightness = 0
        guard var current = color.outputImage else { return nil }

        let vignette = CIFilter.vignetteEffect()
        vignette.inputImage = current
        vignette.center = CGPoint(x: extent.midX, y: extent.midY)
        vignette.intensity = Float(vbVignetteIntensity)
        vignette.radius = Float(shortest * vbVignetteRadiusRatio)
        vignette.falloff = Float(vbVignetteFalloff)
        if let vOut = vignette.outputImage { current = vOut }

        return render(output: current, cropTo: extent)
    }

    // 3) Beauty + Eye Enhance（皮膚＝臉部遮罩－五官遮罩）
    private func applyBeautyAndEyes(to ui: UIImage) -> UIImage? {
        guard let ciInput = CIImage(image: ui) else { return nil }
        let extent = ciInput.extent
        let shortest = min(extent.width, extent.height)

        // 遮罩（快取或現做）
        var face = faceMask ?? makeFaceSkinMask(for: ui)
        var eyes = eyeMask  ?? makeEyeMask(for: ui)
        let feats = featureMask ?? makeFeaturesMask(for: ui) // 五官

        // 臉部遮罩：擴張 + 羽化，再扣掉五官
        if let m = face {
            var mask = m.cropped(to: extent)
            let grow = CIFilter.morphologyMaximum()
            grow.inputImage = mask
            grow.radius = Float(max(1, 2 + beautyStrength * 3)) // 2~5
            mask = grow.outputImage?.cropped(to: extent) ?? mask

            let blur = CIFilter.gaussianBlur()
            blur.inputImage = mask
            blur.radius = Float(max(1, beautyFeather))
            mask = blur.outputImage?.cropped(to: extent) ?? mask

            if let f = feats {
                let protectFeather = max(eyeFeather, beautyFeather) * 0.9 + 6
                let protBlur = CIFilter.gaussianBlur()
                protBlur.inputImage = f
                protBlur.radius = Float(max(3, protectFeather))
                let prot = protBlur.outputImage?.cropped(to: extent) ?? f

                let inv = prot.applyingFilter("CIColorInvert")
                let mult = CIFilter.multiplyCompositing()
                mult.inputImage = mask
                mult.backgroundImage = inv
                face = mult.outputImage?.cropped(to: extent) ?? mask
            } else {
                face = mask
            }
        }

        // 眼睛遮罩：羽化
        if eyeEnhanceEnabled, let m = eyes {
            let b = CIFilter.gaussianBlur()
            b.inputImage = m
            b.radius = Float(max(1, eyeFeather))
            eyes = b.outputImage?.cropped(to: extent)
        }

        // 尺寸感知柔化半徑
        let baseRadius = max(2.0, Double(shortest) * 0.008 * Double(beautyStrength))

        // 中值 + 高斯
        let median = CIFilter.median()
        median.inputImage = ciInput
        var smoothed = median.outputImage ?? ciInput

        let g = CIFilter.gaussianBlur()
        g.inputImage = smoothed
        g.radius = Float(baseRadius)
        smoothed = g.outputImage?.cropped(to: extent) ?? smoothed

        // 只覆蓋臉部（皮膚區域）
        var current = ciInput
        if let mask = face {
            let blend = CIFilter.blendWithMask()
            blend.inputImage = smoothed
            blend.backgroundImage = ciInput
            blend.maskImage = mask
            current = blend.outputImage ?? ciInput
        }

        // 眼睛細節加強
        if eyeEnhanceEnabled, let eyeMask = eyes {
            let sharp = CIFilter.unsharpMask()
            sharp.inputImage = ciInput
            sharp.radius = Float(0.8 + 2.2 * eyeEnhanceIntensity)
            sharp.intensity = Float(0.2 + 0.8 * eyeEnhanceIntensity)
            var sharpened = sharp.outputImage ?? ciInput

            let cc = CIFilter.colorControls()
            cc.inputImage = sharpened
            cc.brightness = 0.02 * Float(eyeEnhanceIntensity)
            cc.contrast   = 1.03 + 0.02 * Float(eyeEnhanceIntensity)
            sharpened = cc.outputImage ?? sharpened

            let blend = CIFilter.blendWithMask()
            blend.inputImage = sharpened
            blend.backgroundImage = current
            blend.maskImage = eyeMask
            current = blend.outputImage ?? current
        }

        return render(output: current, cropTo: extent)
    }
}

// MARK: - 旋轉（順時針 90°）
extension ContentView {
    private func rotate90CW() {
        guard let base = originalImage,
              let rotated = rotateImage90CW(base) else { return }

        showMaskOverlay = false // 旋轉時關閉遮罩預覽

        originalImage = rotated
        processedImage = nil

        // 方向改變需重建遮罩
        faceMask    = makeFaceSkinMask(for: rotated)
        eyeMask     = makeEyeMask(for: rotated)
        featureMask = makeFeaturesMask(for: rotated)

        resetViewTransform()
        applyCurrentEffect()
    }

    private func rotateImage90CW(_ image: UIImage) -> UIImage? {
        // 新尺寸：寬高對調
        let newSize = CGSize(width: image.size.height, height: image.size.width)
        let format = UIGraphicsImageRendererFormat.default()
        format.scale = image.scale
        format.opaque = false

        let renderer = UIGraphicsImageRenderer(size: newSize, format: format)
        let img = renderer.image { ctx in
            let cg = ctx.cgContext
            cg.translateBy(x: newSize.width / 2, y: newSize.height / 2)
            cg.rotate(by: .pi / 2) // ✅ 順時針 90°
            image.draw(in: CGRect(x: -image.size.width / 2,
                                  y: -image.size.height / 2,
                                  width: image.size.width,
                                  height: image.size.height))
        }
        return img
    }
}

// MARK: - Vision 遮罩（臉部、眼、五官）
extension ContentView {

    private func makeFaceSkinMask(for ui: UIImage) -> CIImage? {
        guard let cg = ui.cgImage else { return nil }
        let imageSize = CGSize(width: cg.width, height: cg.height)
        let orientation = CGImagePropertyOrientation(ui.imageOrientation)

        let landmarksReq = VNDetectFaceLandmarksRequest()
        let handler = VNImageRequestHandler(cgImage: cg, orientation: orientation, options: [:])
        try? handler.perform([landmarksReq])

        if let faces = landmarksReq.results, !faces.isEmpty {
            let cs = CGColorSpaceCreateDeviceGray()
            guard let ctx = CGContext(data: nil, width: Int(imageSize.width), height: Int(imageSize.height),
                                      bitsPerComponent: 8, bytesPerRow: 0, space: cs,
                                      bitmapInfo: CGImageAlphaInfo.none.rawValue) else { return nil }

            ctx.setFillColor(UIColor.black.cgColor)
            ctx.fill(CGRect(origin: .zero, size: imageSize))
            ctx.setFillColor(UIColor.white.cgColor)

            for ob in faces {
                var faceRect = denormRect(ob.boundingBox, imageSize: imageSize)
                faceRect = faceRect.insetBy(dx: -faceRect.width * 0.08, dy: -faceRect.height * 0.12)
                ctx.fill(faceRect)

                ctx.setBlendMode(.clear)
                if let leftEye = ob.landmarks?.leftEye    { fill(points: leftEye.normalizedPoints,  in: ob, context: ctx, imageSize: imageSize) }
                if let rightEye = ob.landmarks?.rightEye  { fill(points: rightEye.normalizedPoints, in: ob, context: ctx, imageSize: imageSize) }
                if let leftBrow = ob.landmarks?.leftEyebrow   { fill(points: leftBrow.normalizedPoints,  in: ob, context: ctx, imageSize: imageSize) }
                if let rightBrow = ob.landmarks?.rightEyebrow { fill(points: rightBrow.normalizedPoints, in: ob, context: ctx, imageSize: imageSize) }
                if let lips = ob.landmarks?.outerLips     { fill(points: lips.normalizedPoints,     in: ob, context: ctx, imageSize: imageSize) }
                ctx.setBlendMode(.normal)
            }

            guard let maskCG = ctx.makeImage() else { return nil }
            return flipY(CIImage(cgImage: maskCG))
        }

        // fallback：臉框
        return makeFaceRectMask(for: cg, orientation: orientation, imageSize: imageSize)
    }

    private func makeFaceRectMask(for cg: CGImage,
                                  orientation: CGImagePropertyOrientation,
                                  imageSize: CGSize) -> CIImage? {
        let rectReq = VNDetectFaceRectanglesRequest()
        let handler = VNImageRequestHandler(cgImage: cg, orientation: orientation, options: [:])
        try? handler.perform([rectReq])
        guard let faces = rectReq.results, !faces.isEmpty else { return nil }

        let cs = CGColorSpaceCreateDeviceGray()
        guard let ctx = CGContext(data: nil, width: Int(imageSize.width), height: Int(imageSize.height),
                                  bitsPerComponent: 8, bytesPerRow: 0, space: cs,
                                  bitmapInfo: CGImageAlphaInfo.none.rawValue) else { return nil }

        ctx.setFillColor(UIColor.black.cgColor)
        ctx.fill(CGRect(origin: .zero, size: imageSize))
        ctx.setFillColor(UIColor.white.cgColor)

        for ob in faces {
            var r = denormRect(ob.boundingBox, imageSize: imageSize)
            r = r.insetBy(dx: -r.width * 0.10, dy: -r.height * 0.14)
            ctx.fill(r)
        }

        guard let maskCG = ctx.makeImage() else { return nil }
        return flipY(CIImage(cgImage: maskCG))
    }

    private func makeEyeMask(for ui: UIImage) -> CIImage? {
        guard let cg = ui.cgImage else { return nil }
        let imageSize = CGSize(width: cg.width, height: cg.height)
        let orientation = CGImagePropertyOrientation(ui.imageOrientation)

        let req = VNDetectFaceLandmarksRequest()
        let handler = VNImageRequestHandler(cgImage: cg, orientation: orientation, options: [:])
        try? handler.perform([req])
        guard let faces = req.results, !faces.isEmpty else { return nil }

        let cs = CGColorSpaceCreateDeviceGray()
        guard let ctx = CGContext(data: nil, width: Int(imageSize.width), height: Int(imageSize.height),
                                  bitsPerComponent: 8, bytesPerRow: 0, space: cs,
                                  bitmapInfo: CGImageAlphaInfo.none.rawValue) else { return nil }

        ctx.setFillColor(UIColor.black.cgColor)
        ctx.fill(CGRect(origin: .zero, size: imageSize))
        ctx.setFillColor(UIColor.white.cgColor)

        for ob in faces {
            if let leftEye = ob.landmarks?.leftEye {
                fill(points: leftEye.normalizedPoints, in: ob, context: ctx, imageSize: imageSize)
            }
            if let rightEye = ob.landmarks?.rightEye {
                fill(points: rightEye.normalizedPoints, in: ob, context: ctx, imageSize: imageSize)
            }
        }

        guard let maskCG = ctx.makeImage() else { return nil }
        return flipY(CIImage(cgImage: maskCG))
    }

    /// 五官遮罩（白=五官：眼睛、眉毛、鼻、唇）
    private func makeFeaturesMask(for ui: UIImage) -> CIImage? {
        guard let cg = ui.cgImage else { return nil }
        let imageSize = CGSize(width: cg.width, height: cg.height)
        let orientation = CGImagePropertyOrientation(ui.imageOrientation)

        let req = VNDetectFaceLandmarksRequest()
        let handler = VNImageRequestHandler(cgImage: cg, orientation: orientation, options: [:])
        try? handler.perform([req])
        guard let faces = req.results, !faces.isEmpty else { return nil }

        let cs = CGColorSpaceCreateDeviceGray()
        guard let ctx = CGContext(data: nil, width: Int(imageSize.width), height: Int(imageSize.height),
                                  bitsPerComponent: 8, bytesPerRow: 0, space: cs,
                                  bitmapInfo: CGImageAlphaInfo.none.rawValue) else { return nil }

        ctx.setFillColor(UIColor.black.cgColor)
        ctx.fill(CGRect(origin: .zero, size: imageSize))
        ctx.setFillColor(UIColor.white.cgColor)

        for ob in faces {
            if let le = ob.landmarks?.leftEye      { fill(points: le.normalizedPoints,    in: ob, context: ctx, imageSize: imageSize) }
            if let re = ob.landmarks?.rightEye     { fill(points: re.normalizedPoints,    in: ob, context: ctx, imageSize: imageSize) }
            if let lb = ob.landmarks?.leftEyebrow  { fill(points: lb.normalizedPoints,    in: ob, context: ctx, imageSize: imageSize) }
            if let rb = ob.landmarks?.rightEyebrow { fill(points: rb.normalizedPoints,    in: ob, context: ctx, imageSize: imageSize) }
            if let ol = ob.landmarks?.outerLips    { fill(points: ol.normalizedPoints,    in: ob, context: ctx, imageSize: imageSize) }
            if let il = ob.landmarks?.innerLips    { fill(points: il.normalizedPoints,    in: ob, context: ctx, imageSize: imageSize) }
            if let nose = ob.landmarks?.nose       { fill(points: nose.normalizedPoints,  in: ob, context: ctx, imageSize: imageSize) }
            if let cr  = ob.landmarks?.noseCrest   { fill(points: cr.normalizedPoints,    in: ob, context: ctx, imageSize: imageSize) }
        }

        guard let maskCG = ctx.makeImage() else { return nil }
        return flipY(CIImage(cgImage: maskCG))
    }

    // landmarks 轉座標與填滿
    private func fill(points: [CGPoint], in ob: VNFaceObservation, context ctx: CGContext, imageSize: CGSize) {
        guard !points.isEmpty else { return }
        let path = CGMutablePath()
        path.move(to: denormPoint(points[0], in: ob, imageSize: imageSize))
        for p in points.dropFirst() { path.addLine(to: denormPoint(p, in: ob, imageSize: imageSize)) }
        path.closeSubpath()
        ctx.addPath(path)
        ctx.fillPath()
    }

    private func denormPoint(_ p: CGPoint, in ob: VNFaceObservation, imageSize: CGSize) -> CGPoint {
        let x = (ob.boundingBox.origin.x + p.x * ob.boundingBox.size.width) * imageSize.width
        let yNorm = (ob.boundingBox.origin.y + p.y * ob.boundingBox.size.height)
        let y = (1.0 - yNorm) * imageSize.height
        return CGPoint(x: x, y: y)
    }

    private func denormRect(_ r: CGRect, imageSize: CGSize) -> CGRect {
        let x = r.origin.x * imageSize.width
        let y = (1.0 - r.origin.y - r.size.height) * imageSize.height
        return CGRect(x: x, y: y, width: r.size.width * imageSize.width, height: r.size.height * imageSize.height)
    }
}

// MARK: - CI Helpers
extension ContentView {
    private func linearGradient(from p0: CGPoint, to p1: CGPoint, color0: CIColor, color1: CIColor) -> CIImage {
        let g = CIFilter.linearGradient()
        g.point0 = p0
        g.point1 = p1
        g.color0 = color0
        g.color1 = color1
        return g.outputImage!
    }

    private func maximumComposite(_ top: CIImage, over bottom: CIImage) -> CIImage {
        let f = CIFilter.maximumCompositing()
        f.inputImage = top
        f.backgroundImage = bottom
        return f.outputImage!
    }

    private func render(output: CIImage, cropTo rect: CGRect) -> UIImage? {
        guard let cg = ciContext.createCGImage(output, from: rect) else { return nil }
        return UIImage(cgImage: cg, scale: UIScreen.main.scale, orientation: .up)
    }

    // 將 CIImage 以高度為基準上下翻轉，讓 CGImage 畫的遮罩和 Core Image 座標系對齊
    private func flipY(_ img: CIImage) -> CIImage {
        let t = CGAffineTransform(translationX: 0, y: img.extent.height).scaledBy(x: 1, y: -1)
        return img.transformed(by: t)
    }

    // 除錯疊圖用：皮膚遮罩（臉部遮罩擴張+羽化後，再扣掉五官）
    private func preparedSkinMaskCI(for ui: UIImage) -> CIImage? {
        guard var mask = faceMask ?? makeFaceSkinMask(for: ui) else { return nil }
        guard let extent = CIImage(image: ui)?.extent else { return mask }

        // 擴張 + 羽化
        let grow = CIFilter.morphologyMaximum()
        grow.inputImage = mask
        grow.radius = Float(max(1, 2 + beautyStrength * 3))
        mask = grow.outputImage?.cropped(to: extent) ?? mask

        let blur = CIFilter.gaussianBlur()
        blur.inputImage = mask
        blur.radius = Float(max(1, beautyFeather))
        mask = blur.outputImage?.cropped(to: extent) ?? mask

        // 扣掉五官
        if let f = featureMask ?? makeFeaturesMask(for: ui) {
            let protectFeather = max(eyeFeather, beautyFeather) * 0.9 + 6
            let protBlur = CIFilter.gaussianBlur()
            protBlur.inputImage = f
            protBlur.radius = Float(max(3, protectFeather))
            let prot = protBlur.outputImage?.cropped(to: extent) ?? f

            let inv = prot.applyingFilter("CIColorInvert")
            let mult = CIFilter.multiplyCompositing()
            mult.inputImage = mask
            mult.backgroundImage = inv
            mask = mult.outputImage?.cropped(to: extent) ?? mask
        }
        return mask
    }

    private func preparedEyeMaskCI(for ui: UIImage) -> CIImage? {
        guard let mask = eyeMask ?? makeEyeMask(for: ui) else { return nil }
        guard let extent = CIImage(image: ui)?.extent else { return mask }
        let blur = CIFilter.gaussianBlur()
        blur.inputImage = mask
        blur.radius = Float(max(1, eyeFeather))
        return blur.outputImage?.cropped(to: extent) ?? mask
    }

    private func overlayUIImage(from mask: CIImage, color: (Float, Float, Float), alpha: Float) -> UIImage? {
        let cm = CIFilter.colorMatrix()
        cm.inputImage = mask
        cm.rVector = CIVector(x: CGFloat(color.0), y: 0, z: 0, w: 0)
        cm.gVector = CIVector(x: 0, y: CGFloat(color.1), z: 0, w: 0)
        cm.bVector = CIVector(x: 0, y: 0, z: CGFloat(color.2), w: 0)
        cm.aVector = CIVector(x: 0, y: 0, z: 0, w: CGFloat(alpha))
        cm.biasVector = CIVector(x: 0, y: 0, z: 0, w: 0)
        guard let out = cm.outputImage else { return nil }
        return render(output: out, cropTo: mask.extent)
    }
}

// MARK: - Save（含提示）
extension ContentView {
    private func saveToPhotos(_ image: UIImage) {
        PHPhotoLibrary.requestAuthorization(for: .addOnly) { status in
            guard status == .authorized || status == .limited else {
                DispatchQueue.main.async {
                    saveAlertTitle = "無權限"
                    saveAlertMessage = "請到設定允許寫入照片。"
                    showSaveAlert = true
                }
                return
            }
            PHPhotoLibrary.shared().performChanges({
                PHAssetCreationRequest.creationRequestForAsset(from: image)
            }) { success, error in
                DispatchQueue.main.async {
                    if let error = error {
                        saveAlertTitle = "存檔失敗"
                        saveAlertMessage = error.localizedDescription
                    } else {
                        saveAlertTitle = "完成"
                        saveAlertMessage = "已存到相簿。"
                    }
                    showSaveAlert = true
                }
            }
        }
    }
}

