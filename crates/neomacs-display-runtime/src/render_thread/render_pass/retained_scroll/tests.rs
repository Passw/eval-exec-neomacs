use super::*;
use neomacs_display_protocol::input_progress::InputStream;
use neomacs_display_protocol::*;

fn fixture() -> FrameGlyphBuffer {
    let mut state = FrameDisplayState::new(8, 4, 10.0, 10.0);
    state.presentation_id = PresentationId::new(71);
    state.background = Color::RED;
    let window = DisplayWindowId::new(1);
    let viewport = Rect::new(10.0, 10.0, 60.0, 20.0);
    let bounds = Rect::new(10.0, 10.0, 60.0, 60.0);
    let mut matrix = GlyphMatrix::new(6, 6);
    let mut positions = Vec::new();
    for index in 0..6 {
        let face_id = FaceId::new(index as u32 + 1);
        let mut face = Face::new(face_id);
        face.background = if index % 2 == 0 {
            Color::GREEN
        } else {
            Color::BLUE
        };
        state.faces.insert(face_id, face);
        let mut row = GlyphRow::new(GlyphRowRole::Text);
        row.pixel_y = index as f32 * 10.0;
        row.height_px = 10.0;
        row.ascent_px = 8.0;
        row.start_charpos = index * 2;
        row.end_charpos = index * 2 + 1;
        let mut glyph =
            Glyph::stretch_with_provenance(6, face_id, GlyphProvenance::buffer(index * 2));
        glyph.pixel_width = 60.0;
        glyph.pixel_height = 10.0;
        glyph.pixel_ascent = 8.0;
        row.glyphs[1].push(glyph);
        positions.push(PresentedTextPosition::new(
            window,
            FrameRect::new(10.0, 10.0 + row.pixel_y, 60.0, 10.0).unwrap(),
            index as i64 * 2 + 1,
            index as i64,
            0,
        ));
        matrix.rows[index] = MatrixRow::new(row);
    }
    let content = WindowMatrixEntry {
        window_id: window,
        matrix,
        pixel_bounds: viewport,
        text_pixel_bounds: viewport,
        text_clip_bounds: Some(bounds),
        selected: true,
    };
    let hit_index = PresentedHitIndex::from_parts(
        state.presentation_id,
        vec![PresentedHitRegion::new(
            Some(window),
            PresentedRegionKind::TextBody,
            FrameRect::new(10.0, 10.0, 60.0, 60.0).unwrap(),
            0,
        )],
        positions,
    )
    .unwrap();
    state.window_matrices.push(content.clone());
    state
        .scroll_coverage
        .push(Arc::new(scroll_coverage::ScrollCoverage {
            epoch: 1,
            anchor_row: 0,
            predict_pixels: true,
            compositor_enabled: true,
            viewport,
            origin: 0.0,
            content,
            faces: state.faces.clone(),
            fonts: Default::default(),
            char_fonts: Default::default(),
            shaped_clusters: Default::default(),
            hit_index,
        }));
    let frame = state.materialize();
    assert_eq!(frame.scroll_surfaces.len(), 1);
    frame
}

fn mapping(frame: &FrameGlyphBuffer, scale: f32) -> PresentMapping {
    let SurfaceState::Drawable(surface) = SurfaceState::from_device_size(
        (frame.width * scale) as u32,
        (frame.height * scale) as u32,
        DeviceScale::new(scale).unwrap(),
    )
    .unwrap() else {
        unreachable!()
    };
    PresentMapping::top_left_clip(
        surface,
        PresentationExtent::new(
            frame.presentation_id,
            GeometrySize::<LogicalPixels>::from_px(frame.width, frame.height).unwrap(),
        ),
    )
}

fn pixels(renderer: &WgpuRenderer, texture: &wgpu::Texture) -> Vec<u8> {
    let width = texture.width();
    let height = texture.height();
    let row = (width * 4).div_ceil(wgpu::COPY_BYTES_PER_ROW_ALIGNMENT)
        * wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
    let buffer = renderer.device().create_buffer(&wgpu::BufferDescriptor {
        label: Some("retained-scroll-pixel-test"),
        size: u64::from(row * height),
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });
    let mut encoder = renderer
        .device()
        .create_command_encoder(&Default::default());
    encoder.copy_texture_to_buffer(
        wgpu::TexelCopyTextureInfo {
            texture,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: wgpu::TextureAspect::All,
        },
        wgpu::TexelCopyBufferInfo {
            buffer: &buffer,
            layout: wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(row),
                rows_per_image: Some(height),
            },
        },
        wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
    );
    renderer.queue().submit(std::iter::once(encoder.finish()));
    buffer.slice(..).map_async(wgpu::MapMode::Read, |_| {});
    renderer
        .device()
        .poll(wgpu::PollType::Wait {
            submission_index: None,
            timeout: Some(std::time::Duration::from_secs(3)),
        })
        .unwrap();
    let data = buffer.slice(..).get_mapped_range().unwrap();
    (0..height)
        .flat_map(|y| data[(y * row) as usize..(y * row + width * 4) as usize].to_vec())
        .collect()
}

#[test]
fn retained_scroll_pixels_match_full_render_and_reuse_coverage_during_reversal() {
    let Ok(mut renderer) = WgpuRenderer::new(None, 80, 40) else {
        assert!(std::env::var_os("NEOMACS_REQUIRE_GPU_TESTS").is_none());
        return;
    };
    for scale in [1.0, 1.5, 2.0] {
        let mut render = GuiFrameRenderState::new(
            1,
            renderer.device(),
            scale as f64,
            false,
            frame_time::observe_platform_now(),
        );
        let original = fixture();
        let stream = InputStream::default();
        let size = SnapshotSize::new((80.0 * scale) as u32, (40.0 * scale) as u32).unwrap();
        let expected = renderer.acquire_snapshot(size).unwrap();
        let actual = renderer.acquire_snapshot(size).unwrap();
        let mut deliveries = Vec::new();
        let mut cached_id = None;
        let mut held = None;
        for delta in [4.0, 8.0, -8.0] {
            let delivery = stream.issue().unwrap();
            assert!(render.compositor.input_scroll.push(
                &original,
                11.0,
                11.0,
                delta,
                delivery.receipt(),
                None
            ));
            deliveries.push(delivery);
            let mut frame = original.clone();
            render.compositor.input_scroll.paint(&mut frame);
            let map = mapping(&frame, scale);
            let atlas = render.compositor.glyph_atlas.as_mut().unwrap();
            atlas.set_current_frame_fonts(frame.font_bindings());
            renderer.render_frame_glyphs(
                expected.view(),
                &frame,
                atlas,
                map,
                false,
                None,
                None,
                None,
                None,
                None,
            );
            super::super::scene::render_frame_root_glyphs(
                &mut renderer,
                &mut render,
                actual.view(),
                &frame,
                map,
                false,
                None,
                None,
                true,
            );
            let cache = render
                .compositor
                .retained_scroll
                .as_ref()
                .expect("retained body must run");
            if let Some(id) = cached_id {
                assert_eq!(cache.texture.id(), id);
            }
            cached_id = Some(cache.texture.id());
            held = Some(cache.texture.clone());
            let expected_pixels = pixels(&renderer, expected.view().texture());
            let actual_pixels = pixels(&renderer, actual.view().texture());
            assert!(
                actual_pixels
                    .iter()
                    .zip(&expected_pixels)
                    .all(|(&a, &b)| a.abs_diff(b) <= 1),
                "retained crop differs from canonical projection at scale={scale}, delta={delta}"
            );
            render.compositor.input_scroll.submit();
        }
        let mut frame = original.clone();
        render.compositor.input_scroll.paint(&mut frame);
        render.compositor.current_scene_generation += 1;
        let rebuilt = prepare(
            &mut renderer,
            &mut render,
            &frame,
            mapping(&frame, scale),
            false,
        )
        .unwrap();
        assert_ne!(
            rebuilt.texture.id(),
            held.as_ref().unwrap().id(),
            "a live old lease cannot be overwritten"
        );
        renderer.effects.line_highlight.enabled = true;
        assert!(
            prepare(
                &mut renderer,
                &mut render,
                &frame,
                mapping(&frame, scale),
                false
            )
            .is_none()
        );
        renderer.effects.line_highlight.enabled = false;
        renderer.effects.cursor_color_cycle.enabled = false;
        renderer.effects.scroll_bar.width = 0;
        assert!(
            prepare(
                &mut renderer,
                &mut render,
                &frame,
                mapping(&frame, scale),
                false
            )
            .is_some(),
            "the software-adapter profile must retain static body pixels too"
        );
        renderer.effects = Default::default();
        assert!(
            prepare(
                &mut renderer,
                &mut render,
                &frame,
                mapping(&frame, scale),
                true
            )
            .is_none(),
            "extra spacing and gradients require the full glyph path"
        );
    }
}
