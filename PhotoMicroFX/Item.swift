//
//  Item.swift
//  PhotoMicroFX
//
//  Created by Joseph Yu on 2025/8/17.
//

import Foundation
import SwiftData

@Model
final class Item {
    var timestamp: Date
    
    init(timestamp: Date) {
        self.timestamp = timestamp
    }
}
